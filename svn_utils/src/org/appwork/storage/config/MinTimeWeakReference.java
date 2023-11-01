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
package org.appwork.storage.config;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.scheduler.DelayedRunnable;

/**
 * @author thomas
 *
 */
public class MinTimeWeakReference<T> extends WeakReference<T> {

    private static final ScheduledExecutorService EXECUTER     = DelayedRunnable.getNewScheduledExecutorService();
    private static final ScheduledExecutorService QUEUECLEANUP = DelayedRunnable.getNewScheduledExecutorService();
    private static final ReferenceQueue<Object>   QUEUE        = new ReferenceQueue<Object>();
    static {
        MinTimeWeakReference.QUEUECLEANUP.scheduleWithFixedDelay(new Runnable() {

            @Override
            public void run() {
                try {
                    Reference<?> remove = null;
                    while ((remove = MinTimeWeakReference.QUEUE.poll()) != null) {
                        ((MinTimeWeakReference<?>) remove).onCleanup();
                    }
                } catch (final Throwable e) {
                    e.printStackTrace();
                }
            }
        }, 10, 60, TimeUnit.SECONDS);
    }

    public static void main(final String[] args) throws InterruptedException {
        final AtomicBoolean onCallBack = new AtomicBoolean(false);

        MinTimeWeakReferenceCleanup callback = new MinTimeWeakReferenceCleanup() {

            @Override
            public void onMinTimeWeakReferenceCleanup(MinTimeWeakReference<?> minTimeWeakReference) {
                onCallBack.set(true);

            }
        };

        final MinTimeWeakReference<double[]> ref = new MinTimeWeakReference<double[]>(new double[20000], 2000, "test", callback);
        for (int i = 0; i < 10; i++) {
            System.out.println(i * 1000 + " - " + ref.get().length);
            Thread.sleep(1000);
        }

        for (int i = 0; i < 10; i++) {
            Thread.sleep(1000);
            System.gc();
            System.out.println(i * 1000 + " - " + (ref.superget() != null));
        }
        while (onCallBack.get() == false) {
            Thread.sleep(1000);
        }
        System.out.println("nice");
    }

    private final AtomicReference<DelayedRunnable>             hardReference = new AtomicReference<DelayedRunnable>(null);
    private final AtomicReference<MinTimeWeakReferenceCleanup> cleanupMinTimeWeakReference;
    private final String                                       id;
    private final long                                         minLifeTime;

    public MinTimeWeakReference(final T ret, final long minlifetime, final String id) {
        this(ret, minlifetime, id, null);
    }

    public MinTimeWeakReference(final T ret, final long minLifeTime, final String id, final MinTimeWeakReferenceCleanup cleanupMinTimeWeakReference) {
        super(ret, MinTimeWeakReference.QUEUE);
        this.id = id;
        this.minLifeTime = minLifeTime;
        this.cleanupMinTimeWeakReference = new AtomicReference<MinTimeWeakReferenceCleanup>(cleanupMinTimeWeakReference);
        /* we get the item at least once to start the cleanup process here */
        this.hardReference.set(new DelayedRunnable(MinTimeWeakReference.EXECUTER, minLifeTime) {
            @SuppressWarnings("unused")
            private final T hardReference = ret;

            @Override
            public void delayedrun() {
                MinTimeWeakReference.this.hardReference.compareAndSet(this, null);
            }

            @Override
            public String getID() {
                return "MinTimeWeakReference_" + id;
            }

            @Override
            public boolean stop() {
                this.delayedrun();
                return super.stop();
            }
        });
        this.get();
    }

    @Override
    public void clear() {
        super.clear();
        removeDelayedHardReference();
        enqueue();
    }

    protected void removeDelayedHardReference() {
        final DelayedRunnable old = this.hardReference.getAndSet(null);
        if (old != null) {
            old.stop();
        }
    }

    /**
     * @return
     */
    @Override
    public T get() {
        final T ret = super.get();
        if (ret == null) {
            this.removeDelayedHardReference();
        } else {
            DelayedRunnable minHardReference = this.hardReference.get();
            if (minHardReference != null) {
                minHardReference.resetAndStart();
                this.hardReference.compareAndSet(null, minHardReference);
            } else {
                minHardReference = new DelayedRunnable(MinTimeWeakReference.EXECUTER, this.minLifeTime) {
                    @SuppressWarnings("unused")
                    private final T hardReference = ret;

                    @Override
                    public void delayedrun() {
                        MinTimeWeakReference.this.hardReference.compareAndSet(this, null);
                    }

                    @Override
                    public String getID() {
                        return "MinTimeWeakReference_" + MinTimeWeakReference.this.id;
                    }

                    @Override
                    public boolean stop() {
                        this.delayedrun();
                        return super.stop();
                    }
                };
                if (this.hardReference.compareAndSet(null, minHardReference)) {
                    minHardReference.resetAndStart();
                }
            }
        }
        return ret;
    }

    public String getID() {
        return this.id;
    }

    public boolean isGone() {
        final T ret = super.get();
        if (ret == null) {
            this.removeDelayedHardReference();
            return true;
        } else {
            return false;
        }
    }

    protected void onCleanup() {
        final MinTimeWeakReferenceCleanup cleanupMinTimeWeakReference = this.cleanupMinTimeWeakReference.getAndSet(null);
        if (cleanupMinTimeWeakReference != null) {
            cleanupMinTimeWeakReference.onMinTimeWeakReferenceCleanup(this);
        }
    }

    public T superget() {
        return super.get();
    }

    @Override
    public String toString() {
        return "MinTimeWeakReference_" + MinTimeWeakReference.this.id + "|Gone:" + this.isGone();
    }

}
