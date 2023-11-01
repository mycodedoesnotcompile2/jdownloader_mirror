/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.scheduler;

import java.lang.Thread.UncaughtExceptionHandler;
import java.lang.reflect.Modifier;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

/**
 * @author daniel
 *
 */
public abstract class DelayedRunnable implements Runnable {
    public static String getCaller() {
        final Throwable stackTrace = new Throwable();
        try {
            for (final StackTraceElement element : stackTrace.getStackTrace()) {
                final String currentClassName = element.getClassName();
                final Class<?> currentClass = Class.forName(currentClassName, true, Thread.currentThread().getContextClassLoader());
                if (Modifier.isAbstract(currentClass.getModifiers())) {
                    /* we dont want the abstract class to be used */
                    continue;
                }
                if (Modifier.isInterface(currentClass.getModifiers())) {
                    /* we dont want the interface class to be used */
                    continue;
                }
                return currentClassName;
            }
        } catch (final Throwable e2) {
        }
        return null;
    }

    /**
     * return a ScheduledExecutorService with deamon Threads, allowCoreThreadTimeOut(true) and maxPoolSize(1)
     */
    public static ScheduledExecutorService getNewScheduledExecutorService() {
        final String caller = DelayedRunnable.getCaller();
        /*
         * changed core to 1 because of http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=7091003
         */
        final ScheduledThreadPoolExecutor ret = new ScheduledThreadPoolExecutor(1, new ThreadFactory() {
            @Override
            public Thread newThread(final Runnable r) {
                final Thread thread = new Thread(r);
                if (caller != null) {
                    thread.setName("Scheduler:" + caller);
                }
                thread.setDaemon(true);
                return thread;
            }
        });

        ret.setMaximumPoolSize(1);
        ret.setKeepAliveTime(10000, TimeUnit.MILLISECONDS);
        ret.allowCoreThreadTimeOut(true);
        return ret;
    }

    private final ScheduledExecutorService service;
    private final long                     delayInMS;
    private final AtomicLong               lastRunRequest  = new AtomicLong(0);
    private final AtomicLong               firstRunRequest = new AtomicLong(0);
    private final AtomicBoolean            delayerSet      = new AtomicBoolean(false);
    private final long                     maxInMS;
    private final AtomicBoolean            delayerEnabled  = new AtomicBoolean(true);

    public DelayedRunnable(final long minDelayInMS) {
        this(DelayedRunnable.getNewScheduledExecutorService(), minDelayInMS);
    }

    public DelayedRunnable(final long minDelayInMS, final long maxDelayInMS) {
        this(DelayedRunnable.getNewScheduledExecutorService(), minDelayInMS, maxDelayInMS);
    }

    public DelayedRunnable(final ScheduledExecutorService service, final long delayInMS) {
        this(service, delayInMS, -1);
    }

    public DelayedRunnable(final ScheduledExecutorService service, final long minDelayInMS, final long maxDelayInMS) {
        this.service = service;
        this.delayInMS = minDelayInMS;
        this.maxInMS = maxDelayInMS;
        if (this.delayInMS <= 0) {
            throw new IllegalArgumentException("minDelay must be >0");
        }
        if (this.maxInMS == 0) {
            throw new IllegalArgumentException("maxDelay must be !=0");
        }
    }

    public ScheduledExecutorService getService() {
        return service;
    }

    abstract public void delayedrun();

    public String getID() {
        return null;
    }

    public boolean isDelayerActive() {
        return this.delayerSet.get();
    }

    public boolean isDelayerEnabled() {
        return this.delayerEnabled.get();
    }

    public void resetAndStart() {
        this.run();
    }

    public long getMinimumDelay() {
        return delayInMS;
    }

    public long getMaximumDelay() {
        return maxInMS;
    }

    public long getEstimatedNextRun() {
        final long firstRunRequest = this.firstRunRequest.get();
        if (firstRunRequest > 0) {
            final long lastRunRequest = this.lastRunRequest.get();
            final long currentTime = System.currentTimeMillis();
            return Math.max(0, delayInMS - (currentTime - lastRunRequest));
        }
        return -1;
    }

    @Override
    public void run() {
        if (this.isDelayerEnabled() == false) {
            try {
                DelayedRunnable.this.delayedrun();
            } catch (Throwable e) {
                onUncaughtException(e);
            }
            return;
        }
        this.lastRunRequest.set(System.currentTimeMillis());
        if (this.delayerSet.getAndSet(true) == true) {
            return;
        }
        this.firstRunRequest.compareAndSet(0, System.currentTimeMillis());
        this.service.schedule(new Runnable() {
            private void delayAgain(final long currentTime, Long nextDelay, final long minDif, final long thisRequestRun) {
                if (DelayedRunnable.this.delayerSet.get() == false) {
                    return;
                }
                if (nextDelay == null) {
                    nextDelay = Math.max(0, DelayedRunnable.this.delayInMS - minDif);
                }
                if (nextDelay < 10) {
                    this.runNow(currentTime, thisRequestRun, minDif);
                    return;
                }
                DelayedRunnable.this.service.schedule(this, nextDelay, TimeUnit.MILLISECONDS);
            }

            public void run() {
                if (DelayedRunnable.this.delayerSet.get() == false) {
                    return;
                }
                final long thisRunRequest = DelayedRunnable.this.lastRunRequest.get();
                final long currentTime = System.currentTimeMillis();
                final long minDif = currentTime - thisRunRequest;
                if (minDif >= DelayedRunnable.this.delayInMS) {
                    /* minDelay reached, run now */
                    this.runNow(currentTime, thisRunRequest, minDif);
                    return;
                }
                final long firstRunRequest = DelayedRunnable.this.firstRunRequest.get();
                Long nextDelay = null;
                if (DelayedRunnable.this.maxInMS > 0) {
                    final long maxDif = currentTime - firstRunRequest;
                    if (maxDif >= DelayedRunnable.this.maxInMS) {
                        /* maxDelay reached, run now */
                        this.runNow(currentTime, thisRunRequest, minDif);
                        return;
                    }
                    final long delay = DelayedRunnable.this.maxInMS - maxDif;
                    nextDelay = Math.min(delay, DelayedRunnable.this.delayInMS);
                }
                this.delayAgain(currentTime, nextDelay, minDif, thisRunRequest);
            }

            private void runNow(final long currentTime, final long thisRunRequest, final long minDif) {
                try {
                    DelayedRunnable.this.delayedrun();
                } catch (Throwable e) {
                    onUncaughtException(e);
                } finally {
                    if (thisRunRequest != DelayedRunnable.this.lastRunRequest.get()) {
                        DelayedRunnable.this.firstRunRequest.set(currentTime);
                        this.delayAgain(currentTime, DelayedRunnable.this.delayInMS, minDif, thisRunRequest);
                    } else {
                        this.stop();
                    }
                }
            }

            private void stop() {
                DelayedRunnable.this.firstRunRequest.set(0);
                DelayedRunnable.this.delayerSet.set(false);
            }
        }, DelayedRunnable.this.delayInMS, TimeUnit.MILLISECONDS);
    }

    /**
     * @param e
     */
    protected void onUncaughtException(Throwable e) {
        UncaughtExceptionHandler handler = Thread.getDefaultUncaughtExceptionHandler();
        if (handler != null) {
            handler.uncaughtException(Thread.currentThread(), e);
        } else {
            e.printStackTrace();
        }

    }

    public void setDelayerEnabled(final boolean b) {
        if (this.delayerEnabled.getAndSet(b) == b) {
            return;
        }
        if (!b) {
            this.stop();
        }
    }

    public boolean stop() {
        return this.delayerSet.compareAndSet(true, false);
    }
}
