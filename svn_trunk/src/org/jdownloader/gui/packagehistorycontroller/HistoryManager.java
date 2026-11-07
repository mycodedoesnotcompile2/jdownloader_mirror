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

/**
 * Base class for histories of names (package names, download paths, search terms,...). Keeps the entries sorted newest first without
 * duplicates, limits them to {@link #getMaxLength()} and saves changes asynchronously. Subclasses define what a valid entry is, how
 * entries are created and where they are stored.
 *
 * @param <T>
 *            type of the history entries
 */
public abstract class HistoryManager<T extends HistoryEntry> {
    /** The entries, newest first. Access has to be synchronized on this manager. */
    protected final ArrayList<T> history;

    /**
     * Creates the history from the stored entries and registers a save on shutdown. Invalid entries and duplicates of the stored entries
     * are removed, the list is cut to {@link #getMaxLength()}.
     *
     * @param initialEntries
     *            the stored entries, may be null
     */
    public HistoryManager(final List<T> initialEntries) {
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
        history = new ArrayList<T>();
        if (initialEntries != null) {
            history.addAll(initialEntries);
        }
        cleanup();
    }

    /**
     * Removes invalid entries and duplicates, sorts the list (newest first) and cuts it to {@link #getMaxLength()}. Invalid entries are
     * removed before sorting because sorting would throw a NullPointerException on null entries.
     */
    private void cleanup() {
        for (final Iterator<T> it = history.iterator(); it.hasNext();) {
            final T next = it.next();
            if (next == null || StringUtils.isEmpty(next.getName()) || !isValid(next.getName())) {
                it.remove();
            }
        }
        Collections.sort(history);
        final int max = Math.max(0, getMaxLength());
        final ArrayList<T> kept = new ArrayList<T>();
        for (final T next : history) {
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
        history.clear();
        history.addAll(kept);
    }

    /**
     * Replaces the whole history, e.g. because the underlying config value has been changed from outside. Timestamps of the given entries
     * are kept. Does not trigger a save.
     */
    protected synchronized void setEntries(final List<T> entries) {
        history.clear();
        if (entries != null) {
            history.addAll(entries);
        }
        cleanup();
    }

    /** Max number of history entries. */
    protected abstract int getMaxLength();

    /** Returns true if both names are considered to be the same history entry. */
    protected boolean isSameName(final String name1, final String name2) {
        return name1.equalsIgnoreCase(name2);
    }

    /** Returns a copy of the history, newest entry first. */
    public synchronized List<T> list() {
        return new ArrayList<T>(history);
    }

    /** Removes all entries and saves the empty history. */
    public synchronized void clear() {
        history.clear();
        requestSave();
    }

    /** Removes the entry with the given name. Returns true if an entry has been removed. */
    public synchronized boolean remove(String name) {
        final T existing = get(name);
        if (existing == null) {
            return false;
        }
        history.remove(existing);
        requestSave();
        return true;
    }

    /** Returns true if the history contains an entry with the given name, see {@link #isSameName(String, String)}. */
    public synchronized boolean contains(String name) {
        return get(name) != null;
    }

    /** Returns the entry with the given name, see {@link #isSameName(String, String)}, or null if there is none. */
    protected synchronized T get(String name) {
        if (StringUtils.isEmpty(name)) {
            return null;
        }
        for (final T existing : history) {
            if (isSameName(existing.getName(), name)) {
                return existing;
            }
        }
        return null;
    }

    /* Counter of the save requests, used to merge multiple pending requests in requestSave */
    private final AtomicLong saveRequest = new AtomicLong(-1);

    /** Returns true if the given name may be part of the history. Entries with an invalid name are not added and removed on load. */
    protected boolean isValid(String input) {
        return true;
    }

    /**
     * Adds the name as newest entry and saves the history. An entry with the same name is not duplicated but moved to the top and gets
     * its last used time updated. Invalid names are ignored, as well as all names if the history is disabled (max length 0). The oldest
     * entries are dropped if the history gets longer than {@link #getMaxLength()}.
     */
    public synchronized void add(String name) {
        if (StringUtils.isEmpty(name) || !isValid(name)) {
            return;
        } else if (getMaxLength() <= 0) {
            /* History is disabled -> Nothing to add, avoid useless saves of the empty list */
            return;
        }
        final T existing = get(name);
        if (existing != null) {
            /*
             * Entry with the same name has been found -> Update name (spelling may differ) and last used timestamp and re-sort list so it
             * will be the first entry next time.
             */
            existing.setName(name);
            existing.setTime(System.currentTimeMillis());
            Collections.sort(history);
        } else {
            /* This name is not in our list -> Add it */
            final T newOne = createNew(name);
            newOne.setTime(System.currentTimeMillis());
            history.add(0, newOne);
        }
        final int max = Math.max(0, getMaxLength());
        while (history.size() > max) {
            history.remove(history.size() - 1);
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

    /** Creates a new entry with the given name. */
    abstract protected T createNew(String name);

    /** Stores the given entries (e.g. in the config). Called asynchronously via {@link #requestSave()} and on shutdown. */
    abstract protected void save(List<T> list);
}
