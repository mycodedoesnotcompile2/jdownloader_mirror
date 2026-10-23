package org.jdownloader.gui.packagehistorycontroller;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.QueueAction;

import jd.controlling.TaskQueue;

public abstract class HistoryManager<T extends HistoryEntry> {
    protected final ArrayList<T> packageHistory;

    public HistoryManager(final List<T> packageNameHistory) {
        try {
            ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
                @Override
                public void onShutdown(ShutdownRequest shutdownRequest) {
                    save(list());
                }
            });
        } catch (Throwable e) {
            /* prevent ClassNotFound exception on shutdown */
        }
        packageHistory = new ArrayList<T>();
        if (packageNameHistory != null) {
            packageHistory.addAll(packageNameHistory);
        }
        cleanup();
    }

    /**
     * Removes invalid entries and duplicates, sorts the list (newest first) and cuts it to {@link #getMaxLength()}. Invalid entries are
     * removed before sorting because sorting would throw a NullPointerException on null entries.
     */
    private void cleanup() {
        for (final Iterator<T> it = packageHistory.iterator(); it.hasNext();) {
            final T next = it.next();
            if (next == null || StringUtils.isEmpty(next.getName()) || !isValid(next.getName())) {
                it.remove();
            }
        }
        Collections.sort(packageHistory);
        final int max = Math.max(0, getMaxLength());
        final ArrayList<T> kept = new ArrayList<T>();
        for (final T next : packageHistory) {
            if (kept.size() >= max) {
                break;
            }
            boolean duplicate = false;
            for (final T existing : kept) {
                if (isSameName(existing.getName(), next.getName())) {
                    /* List is sorted newest first -> the entry we already kept is the more recent one */
                    duplicate = true;
                    break;
                }
            }
            if (!duplicate) {
                kept.add(next);
            }
        }
        packageHistory.clear();
        packageHistory.addAll(kept);
    }

    /**
     * Replaces the whole history, e.g. because the underlying config value has been changed from outside. Timestamps of the given entries
     * are kept. Does not trigger a save.
     */
    protected synchronized void setEntries(final List<T> entries) {
        packageHistory.clear();
        if (entries != null) {
            packageHistory.addAll(entries);
        }
        cleanup();
    }

    /** Max number of history entries. */
    protected abstract int getMaxLength();

    /** Returns true if both names are considered to be the same history entry. */
    protected boolean isSameName(final String name1, final String name2) {
        return name1.equalsIgnoreCase(name2);
    }

    public synchronized List<T> list() {
        return new ArrayList<T>(packageHistory);
    }

    public synchronized void clear() {
        packageHistory.clear();
        requestSave();
    }

    protected synchronized boolean contains(String packageName) {
        return get(packageName) != null;
    }

    protected synchronized T get(String packageName) {
        if (StringUtils.isEmpty(packageName)) {
            return null;
        }
        for (final T existing : packageHistory) {
            if (isSameName(existing.getName(), packageName)) {
                return existing;
            }
        }
        return null;
    }

    private final AtomicLong saveRequest = new AtomicLong(-1);

    protected boolean isValid(String input) {
        return true;
    }

    public synchronized void add(String packageName) {
        if (StringUtils.isEmpty(packageName) || !isValid(packageName)) {
            return;
        }
        final T existing = get(packageName);
        if (existing != null) {
            /*
             * Entry with the same name has been found -> Update name (spelling may differ) and last used timestamp and re-sort list so it
             * will be the first entry next time.
             */
            existing.setName(packageName);
            existing.setTime(System.currentTimeMillis());
            Collections.sort(packageHistory);
        } else {
            /* This package name is not in our list -> Add it */
            final T newOne = createNew(packageName);
            newOne.setTime(System.currentTimeMillis());
            packageHistory.add(0, newOne);
        }
        final int max = Math.max(0, getMaxLength());
        while (packageHistory.size() > max) {
            packageHistory.remove(packageHistory.size() - 1);
        }
        requestSave();
    }

    /** Queues an asynchronous save. Multiple pending requests are merged so only the latest one is actually saved. */
    protected void requestSave() {
        final long saveRequest = this.saveRequest.incrementAndGet();
        TaskQueue.getQueue().addAsynch(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                if (saveRequest != HistoryManager.this.saveRequest.get()) {
                    /* A newer save request has been queued -> Skip this one */
                    return null;
                }
                save(list());
                return null;
            }
        });
    }

    abstract protected T createNew(String name);

    abstract protected void save(List<T> list);
}
