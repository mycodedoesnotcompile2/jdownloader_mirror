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
import java.lang.ref.SoftReference;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.scheduler.DelayedRunnable;

/**
 * @author thomas
 *
 */
public class MaxTimeSoftReference<T> extends SoftReference<T> {

    private static final ScheduledExecutorService                EXECUTER     = DelayedRunnable.getNewScheduledExecutorService();
    private static final ScheduledExecutorService                QUEUECLEANUP = DelayedRunnable.getNewScheduledExecutorService();
    private static final ReferenceQueue<Object>                  QUEUE        = new ReferenceQueue<Object>();
    static {
        MaxTimeSoftReference.QUEUECLEANUP.scheduleWithFixedDelay(new Runnable() {

            @Override
            public void run() {
                try {
                    Reference<?> remove = null;
                    while ((remove = MaxTimeSoftReference.QUEUE.poll()) != null) {
                        ((MaxTimeSoftReference<?>) remove).onCleanup();
                    }
                } catch (final Throwable e) {
                    e.printStackTrace();
                }
            }
        }, 10, 60, TimeUnit.SECONDS);
    }

    protected final AtomicReference<DelayedRunnable>             delayedRunnable;
    protected final AtomicReference<MaxTimeSoftReferenceCleanupCallback> cleanupMaxTimeSoftReference;
    protected final String                                       id;

    public MaxTimeSoftReference(final T ret, final long minlifetime, final String id) {
        this(ret, minlifetime, id, null);
    }

    public MaxTimeSoftReference(final T ret, final long maxLifeTime, final String id, final MaxTimeSoftReferenceCleanupCallback cleanupMaxTimeSoftReference) {
        super(ret, MaxTimeSoftReference.QUEUE);
        this.id = id;
        this.cleanupMaxTimeSoftReference = new AtomicReference<MaxTimeSoftReferenceCleanupCallback>(cleanupMaxTimeSoftReference);
        /* we get the item at least once to start the cleanup process here */
        delayedRunnable = new AtomicReference<DelayedRunnable>(new DelayedRunnable(MaxTimeSoftReference.EXECUTER, maxLifeTime) {

            @Override
            public void delayedrun() {
                if (delayedRunnable.compareAndSet(this, null)) {
                    MaxTimeSoftReference.this.clear();
                }
            }

            @Override
            public String getID() {
                return "MaxTimeSoftReference_" + id;
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
        enqueue();
        stopDelayedReferenceClear();
    }

    @Override
    public T get() {
        final T ret = super.get();
        if (ret == null) {
            stopDelayedReferenceClear();
            return null;
        } else {
            resetDelayedReferenceClear();
            return ret;
        }
    }

    protected boolean resetDelayedReferenceClear() {
        final DelayedRunnable delayedRunnable = this.delayedRunnable.get();
        if (delayedRunnable != null) {
            delayedRunnable.resetAndStart();
            return true;
        } else {
            return false;
        }
    }

    protected boolean stopDelayedReferenceClear() {
        final DelayedRunnable delayedRunnable = this.delayedRunnable.get();
        if (delayedRunnable != null) {
            return delayedRunnable.stop();
        } else {
            return false;
        }
    }

    public String getID() {
        return this.id;
    }

    public boolean isGone() {
        final T ret = super.get();
        if (ret == null) {
            stopDelayedReferenceClear();
            return true;
        } else {
            return false;
        }
    }

    protected void onCleanup() {
        final MaxTimeSoftReferenceCleanupCallback cleanupMaxTimeSoftReference = this.cleanupMaxTimeSoftReference.getAndSet(null);
        if (cleanupMaxTimeSoftReference != null) {
            cleanupMaxTimeSoftReference.onMaxTimeSoftReferenceCleanup(this);
        }
    }

    public T superget() {
        return super.get();
    }

    @Override
    public String toString() {
        return "MaxTimeSoftReference_" + MaxTimeSoftReference.this.getID() + "|Gone:" + this.isGone();
    }

}
