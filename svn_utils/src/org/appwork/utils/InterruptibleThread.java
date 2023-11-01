package org.appwork.utils;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class InterruptibleThread extends Thread {
    /**
     * @param string
     */
    public InterruptibleThread(String string) {
        super(string);
    }

    public InterruptibleThread() {
        super();
    }

    public InterruptibleThread startThread() {
        start();
        return this;
    }

    public InterruptibleThread joinThread() throws InterruptedException {
        join();
        return this;
    }

    /**
     * @param runnable
     * @param string
     */
    public InterruptibleThread(Runnable runnable, String string) {
        super(runnable, string);
    }

    public List<Interruptible> getInterruptibles() {
        return java.util.Collections.unmodifiableList(interruptibles);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Thread#interrupt()
     */
    @Override
    public void interrupt() {
        super.interrupt();
        for (final Interruptible b : interruptibles) {
            b.interrupt(this);
        }
    }

    private final CopyOnWriteArrayList<Interruptible> interruptibles = new CopyOnWriteArrayList<Interruptible>();

    /**
     * @param interruptible
     */
    public boolean addInterruptible(Interruptible interruptible) {
        return interruptible != null && interruptibles.addIfAbsent(interruptible);
    }

    public boolean removeInterruptible(Interruptible interruptible) {
        return interruptible != null && interruptibles.remove(interruptible);
    }

    public static Boolean add(Interruptible interruptible) {
        final Thread ct = Thread.currentThread();
        if (ct instanceof InterruptibleThread) {
            return ((InterruptibleThread) ct).addInterruptible(interruptible);
        } else {
            return null;
        }
    }

    public static Boolean remove(Interruptible interruptible) {
        final Thread ct = Thread.currentThread();
        if (ct instanceof InterruptibleThread) {
            return ((InterruptibleThread) ct).removeInterruptible(interruptible);
        } else {
            return null;
        }
    }
}
