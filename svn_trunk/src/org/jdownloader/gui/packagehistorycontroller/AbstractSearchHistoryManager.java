package org.jdownloader.gui.packagehistorycontroller;

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.ConfigEvent;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.storage.config.handler.ObjectKeyHandler;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.EventSuppressor;

/**
 * Base class for the history of search terms of a search field. Handles the persistence in a config value, the matching used by the
 * popup and the inline completion.
 */
public abstract class AbstractSearchHistoryManager extends HistoryManager<SearchHistoryEntry> implements GenericConfigEventListener<Object> {
    private final ObjectKeyHandler historyKey;

    /**
     * @param initialEntries
     *            the stored entries
     * @param historyKey
     *            the config value which stores the entries. Modifications from outside (e.g. advanced settings) are applied to the history.
     */
    protected AbstractSearchHistoryManager(final List<SearchHistoryEntry> initialEntries, final ObjectKeyHandler historyKey) {
        super(initialEntries);
        this.historyKey = historyKey;
        historyKey.getEventSender().addListener(this);
    }

    /** Writes the entries to the config value. */
    protected abstract void saveToConfig(List<SearchHistoryEntry> list);

    @Override
    protected boolean isValid(String input) {
        return !StringUtils.isEmpty(input) && input.trim().length() > 0;
    }

    @Override
    protected void save(List<SearchHistoryEntry> list) {
        final Thread thread = Thread.currentThread();
        /* Our own save must not be applied to the history again */
        final EventSuppressor<ConfigEvent> eventSuppressor = new EventSuppressor<ConfigEvent>() {
            @Override
            public boolean suppressEvent(ConfigEvent eventType) {
                return Thread.currentThread() == thread;
            }
        };
        historyKey.getEventSender().addEventSuppressor(eventSuppressor);
        try {
            saveToConfig(list);
        } finally {
            historyKey.getEventSender().removeEventSuppressor(eventSuppressor);
        }
    }

    @Override
    protected SearchHistoryEntry createNew(String name) {
        return new SearchHistoryEntry(name);
    }

    /**
     * Returns all search terms (newest first) which contain the given text (case insensitive). An entry which equals the given text is
     * not returned as selecting it would not change anything. Returns all entries if the given text is empty.
     */
    public List<String> listMatching(final String text) {
        final List<SearchHistoryEntry> list = list();
        final ArrayList<String> ret = new ArrayList<String>();
        final String search = text == null ? "" : text.toLowerCase();
        for (final SearchHistoryEntry entry : list) {
            final String name = entry.getName();
            if (search.length() == 0) {
                ret.add(name);
            } else if (!isSameName(name, text) && name.toLowerCase().contains(search)) {
                ret.add(name);
            }
        }
        return ret;
    }

    /**
     * Returns the newest search term which starts with the given text (case insensitive) and is longer than it, or null if there is none.
     * Used for inline completion.
     */
    public String getCompletion(final String text) {
        if (StringUtils.isEmpty(text)) {
            return null;
        }
        for (final SearchHistoryEntry entry : list()) {
            final String name = entry.getName();
            /* regionMatches instead of toLowerCase().startsWith: Caller uses text.length() as index into the returned name */
            if (name.length() > text.length() && name.regionMatches(true, 0, text, 0, text.length())) {
                return name;
            }
        }
        return null;
    }

    @Override
    public void onConfigValueModified(KeyHandler<Object> keyHandler, Object newValue) {
        if (newValue == null) {
            setEntries(null);
        } else if (newValue instanceof List) {
            final List<SearchHistoryEntry> entries = new ArrayList<SearchHistoryEntry>();
            for (final Object item : (List<?>) newValue) {
                if (item instanceof SearchHistoryEntry) {
                    entries.add((SearchHistoryEntry) item);
                }
            }
            setEntries(entries);
        }
    }

    @Override
    public void onConfigValidatorError(KeyHandler<Object> keyHandler, Object invalidValue, ValidationException validateException) {
    }
}
