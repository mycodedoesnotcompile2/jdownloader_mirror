/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.scheduler.test;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import org.appwork.exceptions.WTFException;
import org.appwork.scheduler.DelayedRunnable;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.Time;

/**
 * @author Daniel Wilhelm
 * @date 30.07.2026
 *
 */
@TestDependency(value = { "org.appwork.scheduler.DelayedRunnable" })
public class DelayedRunnableTest extends AWTest {

    public static void main(String[] args) {
        run();
    }

    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        if (true) {
            minTest(500, 10, 20);
            minTest(500, 25, 20);
            minTest(500, 50, 20);
            minTest(500, 100, 20);
            minTest(1000, 250, 20);
        }
        if (true) {
            maxTest(500, 10, 20);
            maxTest(500, 25, 20);
            maxTest(500, 50, 20);
            maxTest(500, 100, 20);
            maxTest(1000, 250, 20);
        }
        if (true) {
            minMaxTest(500, 10, 20);
            minMaxTest(500, 25, 20);
            minMaxTest(500, 50, 20);
            minMaxTest(500, 100, 20);
            minMaxTest(1000, 250, 20);
        }
    }

    private void minTest(final int testDuration, final int minDelay, final int jitter) throws Exception {
        final Object signal = new Object();
        final DelayedRunnable delayer = new DelayedRunnable(getNewTestScheduledExecutorService(), minDelay) {

            @Override
            public void delayedrun() {
                synchronized (signal) {
                    signal.notify();
                }
            }
        };

        final long until = Time.systemIndependentCurrentJVMTimeMillis() + testDuration;
        while (Time.systemIndependentCurrentJVMTimeMillis() < until) {
            final long timeStamp = Time.systemIndependentCurrentJVMTimeMillis();
            delayer.run();
            synchronized (signal) {
                signal.wait(minDelay + jitter);
            }
            final long delay = Time.systemIndependentCurrentJVMTimeMillis() - timeStamp;
            final long delayJitter = delay - minDelay;
            if (Math.abs(delayJitter) > jitter) {
                throw new WTFException(minDelay + "\t" + jitter + "\t" + delayJitter + "\t" + delay);
            }
        }
    }

    private void maxTest(final int testDuration, final int maxDelay, final int jitter) throws Exception {
        final Object signal = new Object();
        final DelayedRunnable delayer = new DelayedRunnable(getNewTestScheduledExecutorService(), maxDelay / 2, maxDelay) {

            @Override
            public void delayedrun() {
                synchronized (signal) {
                    signal.notify();
                }
            }
        };

        final long until = Time.systemIndependentCurrentJVMTimeMillis() + testDuration;
        final Thread fireThread = new Thread() {
            /**
             * @see org.appwork.testframework.AWTest#run()
             */
            @Override
            public void run() {
                while (!Thread.currentThread().isInterrupted()) {
                    try {
                        Thread.sleep(1);
                    } catch (InterruptedException e) {
                        return;
                    }
                    delayer.run();
                }
            }
        };
        try {
            fireThread.start();
            while (Time.systemIndependentCurrentJVMTimeMillis() < until) {
                final long timeStamp = Time.systemIndependentCurrentJVMTimeMillis();
                synchronized (signal) {
                    signal.wait(maxDelay + jitter);
                }
                final long delay = Time.systemIndependentCurrentJVMTimeMillis() - timeStamp;
                final long delayJitter = delay - maxDelay;
                if (Math.abs(delayJitter) > jitter) {
                    throw new WTFException(maxDelay + "\t" + jitter + "\t" + delayJitter + "\t" + delay);
                }
            }
        } finally {
            fireThread.interrupt();
        }
    }

    public static ScheduledExecutorService getNewTestScheduledExecutorService() {
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
                thread.setPriority(Thread.MAX_PRIORITY);
                thread.setDaemon(true);
                return thread;
            }
        });

        ret.setMaximumPoolSize(1);
        ret.setKeepAliveTime(10000, TimeUnit.MILLISECONDS);
        ret.allowCoreThreadTimeOut(true);
        return ret;
    }

    private void minMaxTest(final int testDuration, final int minMaxDelay, final int jitter) throws Exception {
        final Object signal = new Object();
        final DelayedRunnable delayer = new DelayedRunnable(getNewTestScheduledExecutorService(), minMaxDelay, minMaxDelay) {

            @Override
            public void delayedrun() {
                synchronized (signal) {
                    signal.notify();
                }
            }
        };

        final long until = Time.systemIndependentCurrentJVMTimeMillis() + testDuration;
        final Thread fireThread = new Thread() {
            /**
             * @see org.appwork.testframework.AWTest#run()
             */
            @Override
            public void run() {
                while (!Thread.currentThread().isInterrupted()) {
                    try {
                        Thread.sleep(1);
                    } catch (InterruptedException e) {
                        return;
                    }
                    delayer.run();
                }
            }
        };
        try {
            fireThread.start();
            while (Time.systemIndependentCurrentJVMTimeMillis() < until) {
                final long timeStamp = Time.systemIndependentCurrentJVMTimeMillis();
                synchronized (signal) {
                    signal.wait(minMaxDelay + jitter);
                }
                final long delay = Time.systemIndependentCurrentJVMTimeMillis() - timeStamp;
                final long delayJitter = delay - minMaxDelay;
                if (Math.abs(delayJitter) > jitter) {
                    throw new WTFException(minMaxDelay + "\t" + jitter + "\t" + delayJitter + "\t" + delay);
                }
            }
        } finally {
            fireThread.interrupt();
        }
    }
}
