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
package org.appwork.utils.event.queue;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.SwingUtilities;

import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;

/**
 * @author daniel
 * @param <D>
 * @param <T>
 *
 */
public abstract class Queue {
    protected final Object queueLock = new Object();

    public static enum QueuePriority {
        HIGH,
        NORM,
        LOW;
    }

    protected boolean                                                   debugFlag           = false;
    protected final java.util.List<QueueAction<?, ? extends Throwable>> queueThreadHistory  = new ArrayList<QueueAction<?, ? extends Throwable>>(20);
    protected final AtomicReference<QueueThread>                        thread              = new AtomicReference<QueueThread>(null);
    protected volatile QueueAction<?, ? extends Throwable>              sourceItem          = null;
    protected final LinkedBlockingDeque<QueueAction<?, ?>>              currentJobs         = new LinkedBlockingDeque<QueueAction<?, ?>>();
    protected final AtomicLong                                          addStats            = new AtomicLong(0);
    protected final AtomicLong                                          addWaitStats        = new AtomicLong(0);
    protected final AtomicLong                                          addRunStats         = new AtomicLong(0);
    protected static AtomicInteger                                      QUEUELOOPPREVENTION = new AtomicInteger(0);
    protected final String                                              id;
    protected volatile long                                             timeout             = 10 * 1000l;
    protected final ArrayDeque<?>[]                                     queues;

    public Queue(final String id) {
        this.id = id;
        Queue.QUEUELOOPPREVENTION.incrementAndGet();
        queues = new ArrayDeque<?>[QueuePriority.values().length];
        for (int i = 0; i < queues.length; i++) {
            queues[i] = new ArrayDeque<QueueAction<?, ? extends Throwable>>();
        }
    }

    /**
     * This method adds an action to the queue. if the caller is a queueaction itself, the action will be executed directly. In this case,
     * this method can throw Exceptions. If the caller is not the QUeuethread, this method is not able to throw exceptions, but the
     * exceptions are passed to the exeptionhandler method of the queueaction
     *
     * @param <T>
     * @param <E>
     * @param item
     * @throws T
     */
    public <E, T extends Throwable> void add(final QueueAction<?, T> action) throws T {
        /* set calling Thread to current item */
        action.reset();
        action.setCallerThread(this, Thread.currentThread());
        action.onEnqueu(this);
        if (isQueueThread(action)) {
            /*
             * call comes from current running item, so lets start item
             */
            final QueueAction<?, ? extends Throwable> source = ((QueueThread) Thread.currentThread()).getSourceQueueAction();
            if (source != null) {
                /* forward source priority */
                action.setQueuePrio(source.getQueuePrio());
            }
            addRunStats.incrementAndGet();
            startItem(action, false);
        } else {
            addStats.incrementAndGet();
            /* call does not come from current running item, so lets queue it */
            internalAdd(action);
        }
    }

    /**
     * Only use this method if you can asure that the caller is NEVER the queue itself. if you are not sure use #add
     *
     * @param <E>
     * @param <T>
     * @param action
     * @throws T
     */
    public <E, T extends Throwable> void addAsynch(final QueueAction<?, T> action) {
        /* set calling Thread to current item */
        if (action.allowAsync() == false && isQueueThread(action)) {
            throw new RuntimeException("called addAsynch from the queue itself");
        } else {
            addStats.incrementAndGet();
            action.reset();
            action.setCallerThread(this, Thread.currentThread());
            action.onEnqueu(this);
            internalAdd(action);
        }
    }

    protected final boolean notifyEDT = DebugMode.TRUE_IN_IDE_ELSE_FALSE && Application.isHeadless();

    @SuppressWarnings("unchecked")
    public <E, T extends Throwable> E addWait(final QueueAction<E, T> item) throws T {
        if (notifyEDT && SwingUtilities.isEventDispatchThread()) {
            new Exception("Inside EDT:" + item).printStackTrace();
        }
        /* set calling Thread to current item */
        item.reset();
        item.setCallerThread(this, Thread.currentThread());
        item.onEnqueu(this);
        if (isQueueThread(item)) {
            /*
             * call comes from current running item, so lets start item excaption handling is passed to top item. startItem throws an
             * exception in error case
             */
            final QueueAction<?, ? extends Throwable> source = ((QueueThread) Thread.currentThread()).getSourceQueueAction();
            if (source != null) {
                /* forward source priority */
                item.setQueuePrio(source.getQueuePrio());
            }
            addRunStats.incrementAndGet();
            startItem(item, false);
        } else {
            addWaitStats.incrementAndGet();
            /* call does not come from current running item, so lets queue it */
            internalAdd(item);
            /* wait till item is finished */
            try {
                while (!item.isFinished()) {
                    synchronized (item) {
                        if (!item.isFinished()) {
                            item.wait(1000);
                        }
                    }
                }
            } catch (final InterruptedException e) {
                if (!item.handleException(e)) {
                    Thread.currentThread().interrupt();
                }
            }
            if (item.getExeption() != null) {
                // throw exception if item canot handle the exception itself
                if (!item.callExceptionHandler()) {
                    if (item.getExeption() instanceof RuntimeException) {
                        throw (RuntimeException) item.getExeption();
                    } else {
                        throw (T) item.getExeption();
                    }
                }
            }
            if (item.gotKilled() && !item.gotStarted()) {
                item.handleException(new InterruptedException("Queue got killed!"));
            }
        }
        return item.getResult();
    }

    public void enqueue(final QueueAction<?, ?> action) {
        /* set calling Thread to current item */
        action.reset();
        action.setCallerThread(this, Thread.currentThread());
        action.onEnqueu(this);
        internalAdd(action);
    }

    protected BlockingDeque<QueueAction<?, ?>> getCurrentJobs() {
        return currentJobs;
    }

    protected final Object getLock() {
        return queueLock;
    }

    public List<QueueAction<?, ?>> getEntries() {
        final List<QueueAction<?, ?>> ret = new ArrayList<QueueAction<?, ?>>();
        synchronized (getLock()) {
            final QueueAction<?, ?> pendingItem = this.pendingItem.get();
            ret.addAll(currentJobs);
            if (pendingItem != null && !ret.contains(pendingItem)) {
                ret.add(pendingItem);
            }
            for (int i = 0; i < queues.length; i++) {
                final ArrayDeque<?> queue = queues[i];
                ret.addAll((ArrayDeque<QueueAction<?, ?>>) queue);
            }
        }
        return ret;
    }

    public String getID() {
        return id;
    }

    protected QueueAction<?, ? extends Throwable> getLastHistoryItem() {
        final List<QueueAction<?, ? extends Throwable>> lQueueThreadHistory = queueThreadHistory;
        synchronized (lQueueThreadHistory) {
            final int size = lQueueThreadHistory.size();
            if (size == 0) {
                return null;
            } else {
                return lQueueThreadHistory.get(size - 1);
            }
        }
    }

    public QueueThread getQueueThread() {
        return thread.get();
    }

    protected QueueAction<?, ? extends Throwable> getSourceQueueAction() {
        return sourceItem;
    }

    public long getTimeout() {
        return timeout;
    }

    /**
     * Overwrite this to hook before a action execution
     */
    protected void handlePreRun() {
    }

    protected void internalAdd(final QueueAction<?, ?> action) {
        if (action != null) {
            final Object lock = queueLock;
            synchronized (lock) {
                try {
                    final QueuePriority prio = action.getQueuePrio();
                    if (prio != null) {
                        ((ArrayDeque<QueueAction<?, ? extends Throwable>>) queues[prio.ordinal()]).offer(action);
                    } else {
                        ((ArrayDeque<QueueAction<?, ? extends Throwable>>) queues[QueuePriority.NORM.ordinal()]).offer(action);
                    }
                } finally {
                    try {
                        final Thread currentThread = thread.get();
                        if (currentThread == null || !currentThread.isAlive()) {
                            final QueueThread newThread = new QueueThread(this);
                            thread.set(newThread);
                            newThread.start();
                        }
                    } finally {
                        lock.notifyAll();
                    }
                }
            }
        }
    }

    /**
     * returns true if this queue shows debug info
     *
     * @return
     */
    public boolean isDebug() {
        return debugFlag;
    }

    public boolean isEmpty() {
        synchronized (getLock()) {
            for (int i = 0; i < queues.length; i++) {
                final ArrayDeque<?> queue = queues[i];
                if (!queue.isEmpty()) {
                    return false;
                }
            }
            return true;
        }
    }

    /**
     * this functions returns true if the current running Thread is our QueueThread OR the SourceQueueItem chain is rooted in current
     * running QueueItem
     */
    public boolean isQueueThread(final QueueAction<?, ? extends Throwable> item) {
        if (Thread.currentThread() == thread.get()) {
            return true;
        }
        QueueAction<?, ? extends Throwable> last = item;
        Thread t = null;
        /*
         * we walk through actionHistory to check if we are still in our QueueThread
         */
        int loopprevention = 0;
        while (last != null && (t = last.getCallerThread()) != null) {
            if (t != null && t instanceof QueueThread) {
                if (t == getQueueThread()) {
                    if (debugFlag) {
                        org.appwork.loggingv3.LogV3.warning("Multiple queues detected-> external synchronization may be required! " + item);
                    }
                    return true;
                }
                last = ((QueueThread) t).getLastHistoryItem();
                if (loopprevention > Queue.QUEUELOOPPREVENTION.get()) {
                    /*
                     * loop prevention: while can only loop max QUEUELOOPPREVENTION times, cause no more different queues exist
                     */
                    if (debugFlag) {
                        org.appwork.loggingv3.LogV3.warning("QueueLoopPrevention!");
                    }
                    break;
                }
                loopprevention++;
            } else {
                break;
            }
        }
        return false;
    }

    /**
     * Does NOT kill the currently running job
     *
     */
    public void killQueue() {
        final List<QueueAction<?, ? extends Throwable>> killList = new ArrayList<QueueAction<?, ? extends Throwable>>();
        synchronized (getLock()) {
            System.out.println("Kill: " + this);
            for (final ArrayDeque<?> queue : queues) {
                killList.addAll((ArrayDeque<QueueAction<?, ? extends Throwable>>) queue);
                queue.clear();
            }
        }
        for (final QueueAction<?, ? extends Throwable> item : killList) {
            item.kill();
        }
    }

    /**
     * @param item
     */
    protected void onItemHandled(final QueueAction<?, ? extends Throwable> item) {
    }

    public boolean remove(final QueueAction<?, ?> action) {
        QueueAction<?, ?> kill = null;
        synchronized (getLock()) {
            final QueuePriority prio = action.getQueuePrio();
            if (prio != null && queues[prio.ordinal()].remove(action)) {
                kill = action;
            }
            if (kill == null) {
                for (int i = 0; i < queues.length; i++) {
                    final ArrayDeque<?> queue = queues[i];
                    if (queue.remove(action)) {
                        kill = action;
                        break;
                    }
                }
            }
        }
        if (kill != null) {
            kill.kill();
            return true;
        }
        return false;
    }

    protected boolean isQueued(final QueueAction<?, ?> action) {
        synchronized (getLock()) {
            for (int i = 0; i < queues.length; i++) {
                final ArrayDeque<?> queue = queues[i];
                if (queue.contains(action)) {
                    return true;
                }
            }
            return false;
        }
    }

    protected QueueAction<?, ? extends Throwable> poll() {
        synchronized (getLock()) {
            for (int i = 0; i < queues.length; i++) {
                final ArrayDeque<?> queue = queues[i];
                final QueueAction<?, ? extends Throwable> ret = (QueueAction<?, ? extends Throwable>) queue.poll();
                if (ret != null) {
                    return ret;
                }
            }
            return null;
        }
    }

    protected QueueAction<?, ? extends Throwable> peek() {
        synchronized (getLock()) {
            for (int i = 0; i < queues.length; i++) {
                final ArrayDeque<?> queue = queues[i];
                final QueueAction<?, ? extends Throwable> ret = (QueueAction<?, ? extends Throwable>) queue.peek();
                if (ret != null) {
                    return ret;
                }
            }
            return null;
        }
    }

    protected final AtomicReference<QueueAction<?, ? extends Throwable>> pendingItem = new AtomicReference<QueueAction<?, ? extends Throwable>>();

    protected void runQueue() {
        final Object lock = getLock();
        try {
            QueueAction<?, ? extends Throwable> item = null;
            while (true) {
                try {
                    handlePreRun();
                    synchronized (lock) {
                        item = poll();
                        if (item == null) {
                            lock.wait(getTimeout());
                            item = poll();
                            if (item == null) {
                                final Thread lThread = Thread.currentThread();
                                if (lThread instanceof QueueThread) {
                                    thread.compareAndSet((QueueThread) lThread, null);
                                }
                                return;
                            }
                        }
                        pendingItem.set(item);
                    }
                    if (!handleItem(item)) {
                        continue;
                    }
                    try {
                        sourceItem = item;
                        startItem(item, true);
                    } catch (final Throwable e) {
                    } finally {
                        sourceItem = null;
                        onItemHandled(item);
                    }
                } catch (final Throwable e) {
                    org.appwork.loggingv3.LogV3.info("Queue rescued!");
                    org.appwork.loggingv3.LogV3.log(e);
                } finally {
                    pendingItem.compareAndSet(item, null);
                }
            }
        } finally {
            synchronized (lock) {
                final Thread lThread = Thread.currentThread();
                if (lThread instanceof QueueThread) {
                    thread.compareAndSet((QueueThread) lThread, null);
                }
            }
        }
    }

    /**
     * @param item
     * @return
     */
    protected boolean handleItem(QueueAction<?, ? extends Throwable> item) {
        return true;
    }

    /**
     * changes this queue's debugFlag
     *
     * @param b
     */
    public void setDebug(final boolean b) {
        debugFlag = b;
    }

    public void setTimeout(long timeout) {
        timeout = Math.max(0, timeout);
        final Object lock = getLock();
        synchronized (lock) {
            lock.notifyAll();
        }
    }

    public int size() {
        synchronized (getLock()) {
            int ret = 0;
            for (int i = 0; i < queues.length; i++) {
                final ArrayDeque<?> queue = queues[i];
                ret += queue.size();
            }
            return ret;
        }
    }

    /* if you override this, DON'T forget to notify item when its done! */
    @SuppressWarnings("unchecked")
    protected <T extends Throwable> void startItem(final QueueAction<?, T> item, final boolean callExceptionhandler) throws T {
        try {
            currentJobs.addLast(item);
            pendingItem.compareAndSet(item, null);
            if (getQueueThread() != item.getCallerThread()) {
                synchronized (queueThreadHistory) {
                    queueThreadHistory.add(item);
                }
            }
            item.start(this);
        } catch (final Throwable e) {
            if (!callExceptionhandler || !item.callExceptionHandler()) {
                if (e instanceof RuntimeException) {
                    throw (RuntimeException) e;
                } else {
                    throw (T) e;
                }
            }
        } finally {
            if (getQueueThread() != item.getCallerThread()) {
                synchronized (queueThreadHistory) {
                    final int size = queueThreadHistory.size();
                    if (size != 0) {
                        queueThreadHistory.remove(size - 1);
                    }
                }
            }
            item.setFinished(true);
            if (currentJobs.removeLast() != item) {
                DebugMode.debugger();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Thread#toString()
     */
    @Override
    public String toString() {
        return this.id + ": add=" + this.addStats.get() + " addWait=" + this.addWaitStats.get() + " addRun=" + this.addRunStats.get();
    }
}
