/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.utils.Time;

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

    private final ScheduledExecutorService    service;
    private final long                        delayInMS;
    private final AtomicReference<DelayState> state          = new AtomicReference<>(null);
    private final long                        maxInMS;
    private final AtomicBoolean               delayerEnabled = new AtomicBoolean(true);

    public static class DelayState {
        public final AtomicBoolean cycleToken;
        public final long          firstRunRequest;
        public final long          lastRunRequest;

        // Erster Request eines neuen Zyklus: Erstellt ein FRISCHES cycleToken
        public DelayState(long firstRunRequest) {
            this(new AtomicBoolean(true), firstRunRequest, firstRunRequest);
        }

        // Interner Konstruktor zum Mitschleifen des Tokens
        private DelayState(AtomicBoolean cycleToken, long firstRunRequest, long lastRunRequest) {
            this.cycleToken = cycleToken;
            this.firstRunRequest = firstRunRequest;
            this.lastRunRequest = lastRunRequest;
        }

        // Folge-Request im selben Zyklus: cycleToken BLEIBT ERHALTEN!
        public DelayState withNewRequest(long now) {
            return new DelayState(this.cycleToken, this.firstRunRequest, now);
        }
    }

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
        return state.get() != null;
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
        final DelayState currentState = this.state.get();
        // Wenn kein Timer aktiv ist (currentState == null)
        if (currentState == null) {
            return -1;
        }
        final long currentTime = Time.systemIndependentCurrentJVMTimeMillis();
        final long minDif = Math.max(0, currentTime - currentState.lastRunRequest);
        // Verbleibender Min-Delay
        final long delayInMS = getMinimumDelay();
        final long minRemaining = Math.max(0, delayInMS - minDif);
        // Falls maxInMS konfiguriert ist, müssen wir auch den verbleibenden Max-Delay beachten!
        final long maxInMS = getMaximumDelay();
        if (maxInMS > 0) {
            final long maxDif = Math.max(0, currentTime - currentState.firstRunRequest);
            final long maxRemaining = Math.max(0, maxInMS - maxDif);
            // Der nächste Run passiert beim früheren der beiden Termine
            return Math.min(minRemaining, maxRemaining);
        }
        return minRemaining;
    }

    protected void scheduleTimer(final AtomicBoolean cycleToken, long delay) {
        service.schedule(new Runnable() {
            @Override
            public void run() {
                checkAndExecute(cycleToken);
            }
        }, delay, TimeUnit.MILLISECONDS);
    }

    private void checkAndExecute(final AtomicBoolean cycleToken) {
        final DelayState currentState = state.get();
        // TOKEN-PRÜFUNG:
        // Läuft noch derselbe Zyklus? (Gehört das Token im aktuellen State zu diesem Timer?)
        if (currentState == null || currentState.cycleToken != cycleToken || cycleToken.get() == false) {
            return; // Outdated! Entweder abgearbeitet oder ein völlig neuer Zyklus läuft.
        }
        final long now = Time.systemIndependentCurrentJVMTimeMillis();
        final long minDif = Math.max(0, now - currentState.lastRunRequest);
        final long delayInMS = getMinimumDelay();
        final long maxInMS = getMaximumDelay();
        final long maxDif = (maxInMS > 0) ? Math.max(0, now - currentState.firstRunRequest) : 0;
        // 1. Min- oder Max-Delay abgelaufen?
        if (minDif >= delayInMS || (maxInMS > 0 && maxDif >= maxInMS)) {
            executeNow(currentState);
            return;
        }
        // 2. Noch Restzeit -> Re-Schedule für DASSELBE boundToken!
        long nextDelay = delayInMS - minDif;
        if (maxInMS > 0) {
            long maxRemaining = maxInMS - maxDif;
            nextDelay = Math.min(nextDelay, maxRemaining);
        }
        if (nextDelay < 10) {
            executeNow(currentState);
        } else {
            // Wir schedulen erneut mit demselben Token
            scheduleTimer(cycleToken, nextDelay);
        }
    }

    protected void executeNow(final DelayState expectedState) {
        // CAS-Schleife: Wir leeren den State NUR dann, wenn er immer noch zu UNSEREM CycleToken gehört!
        while (true) {
            final DelayState current = state.get();
            // 1. Ist der Zyklus überhaupt noch aktiv und gehört zu unserem Token?
            if (current == null || current.cycleToken != expectedState.cycleToken) {
                return; // Ein anderer Thread war schneller und hat die Arbeit bereits ausgeführt!
            }
            // 2. Atomar auf null setzen – EGAL ob in der Zwischenzeit ein withNewRequest()
            // den lastRunRequest im selben Zyklus aktualisiert hat!
            if (state.compareAndSet(current, null)) {
                break; // Erfolgreich! Wir übernehmen die Ausführung.
            }
        }
        try {
            if (expectedState.cycleToken.compareAndSet(true, false)) {
                DelayedRunnable.this.delayedrun();
            }
        } catch (Throwable e) {
            onUncaughtException(e);
        } finally {
            // Nachlauf-Check für Requests, die WÄHREND doActualWork reinkamen
            final DelayState nextCycleState = state.get();
            if (nextCycleState != null && nextCycleState.cycleToken.get()) {
                final long now = Time.systemIndependentCurrentJVMTimeMillis();
                final long minDif = Math.max(0, now - nextCycleState.lastRunRequest);
                final long delayInMS = getMinimumDelay();
                final long maxInMS = getMaximumDelay();
                final long maxDif = (maxInMS > 0) ? Math.max(0, now - nextCycleState.firstRunRequest) : 0;
                if (minDif >= delayInMS || (maxInMS > 0 && maxDif >= maxInMS)) {
                    // Der NEUE Zyklus ist bereits fällig -> sofort ausführen
                    service.execute(new Runnable() {
                        @Override
                        public void run() {
                            checkAndExecute(nextCycleState.cycleToken);
                        }
                    });
                }
            }
        }
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
        final long now = Time.systemIndependentCurrentJVMTimeMillis();
        while (true) {
            final DelayState oldState = state.get();
            final DelayState newState;
            if (oldState == null) {
                newState = new DelayState(now);
            } else {
                newState = oldState.withNewRequest(now);
            }
            if (state.compareAndSet(oldState, newState)) {
                if (oldState == null) {
                    scheduleTimer(newState.cycleToken, getMinimumDelay());
                }
                return;
            }
        }
    }

    /**
     * @param e
     */
    protected void onUncaughtException(Throwable e) {
        final UncaughtExceptionHandler handler = Thread.getDefaultUncaughtExceptionHandler();
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
        final DelayState delayState = this.state.getAndSet(null);
        return delayState != null && delayState.cycleToken.compareAndSet(true, false);
    }
}