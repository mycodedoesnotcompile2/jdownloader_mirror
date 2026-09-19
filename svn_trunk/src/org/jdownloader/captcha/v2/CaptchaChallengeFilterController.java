package org.jdownloader.captcha.v2;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.appwork.shutdown.ShutdownController;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.IO;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.ConfigEvent;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.EventSuppressor;
import org.appwork.utils.event.predefined.changeevent.ChangeEvent;
import org.appwork.utils.event.predefined.changeevent.ChangeEventSender;
import org.jdownloader.logging.LogController;

/**
 * Central singleton that manages the global list of {@link CaptchaChallengeFilter} rules. The rules used to be stored per captcha solver
 * inside each solver's own config; they now live in one place ({@link CaptchaChallengeFilterSettings}) and every rule carries its own solver
 * assignment ({@link CaptchaChallengeFilter#getSolver()}).
 *
 * Modelled after {@link org.jdownloader.controlling.filter.LinkFilterController}: loads on init, saves on shutdown and on every
 * modification, keeps a precompiled view for cheap matching and notifies listeners via a {@link ChangeEventSender}.
 */
public class CaptchaChallengeFilterController {
    private static final CaptchaChallengeFilterController INSTANCE = new CaptchaChallengeFilterController(false);

    public static CaptchaChallengeFilterController getInstance() {
        return INSTANCE;
    }

    public static CaptchaChallengeFilterController createEmptyTestInstance() {
        return new CaptchaChallengeFilterController(true);
    }

    private volatile ArrayList<CaptchaChallengeFilter>       filters;
    private volatile List<CompiledCaptchaChallengeFilter>    compiled = new ArrayList<CompiledCaptchaChallengeFilter>();
    private final CaptchaChallengeFilterSettings             config;
    private final KeyHandler<Object>                         filterListHandler;
    private final ChangeEventSender                          eventSender;
    private final boolean                                    testInstance;

    private CaptchaChallengeFilterController(final boolean testInstance) {
        this.eventSender = new ChangeEventSender();
        this.testInstance = testInstance;
        if (!testInstance) {
            config = JsonConfig.create(CaptchaChallengeFilterSettings.class);
            filterListHandler = config._getStorageHandler().getKeyHandler("FilterList");
            filters = readConfig();
            filterListHandler.getEventSender().addListener(new GenericConfigEventListener<Object>() {
                @Override
                public void onConfigValueModified(final KeyHandler<Object> keyHandler, final Object newValue) {
                    filters = readConfig();
                    update();
                }

                @Override
                public void onConfigValidatorError(final KeyHandler<Object> keyHandler, final Object invalidValue, final ValidationException validateException) {
                }
            });
            ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
                @Override
                public void onShutdown(final ShutdownRequest shutdownRequest) {
                    save(filters);
                }

                @Override
                public long getMaxDuration() {
                    return 0;
                }

                @Override
                public String toString() {
                    return "save captcha challenge filters...";
                }
            });
        } else {
            config = null;
            filterListHandler = null;
            filters = new ArrayList<CaptchaChallengeFilter>();
        }
        updateInternal();
    }

    public ChangeEventSender getEventSender() {
        return eventSender;
    }

    public boolean isTestInstance() {
        return testInstance;
    }

    private ArrayList<CaptchaChallengeFilter> readConfig() {
        if (config == null) {
            return new ArrayList<CaptchaChallengeFilter>();
        }
        final ArrayList<CaptchaChallengeFilter> stored = config.getFilterList();
        if (stored == null) {
            return new ArrayList<CaptchaChallengeFilter>();
        }
        return stored;
    }

    /**
     * Rebuilds the precompiled filter view from the current filter list. Only enabled and valid rules are compiled; the resulting list is
     * ordered by {@link CaptchaChallengeFilter#getPosition()} so matching honours the user-defined order.
     */
    private void updateInternal() {
        final ArrayList<CaptchaChallengeFilter> source = new ArrayList<CaptchaChallengeFilter>(filters);
        Collections.sort(source, new Comparator<CaptchaChallengeFilter>() {
            @Override
            public int compare(final CaptchaChallengeFilter o1, final CaptchaChallengeFilter o2) {
                final int p1 = o1.getPosition();
                final int p2 = o2.getPosition();
                if (p1 < p2) {
                    return -1;
                } else if (p1 > p2) {
                    return 1;
                } else {
                    return 0;
                }
            }
        });
        final ArrayList<CompiledCaptchaChallengeFilter> newCompiled = new ArrayList<CompiledCaptchaChallengeFilter>();
        for (int i = 0; i < source.size(); i++) {
            final CaptchaChallengeFilter filter = source.get(i);
            if (!filter._isValid()) {
                /* Skip disabled/broken/empty rules */
                continue;
            }
            try {
                final CompiledCaptchaChallengeFilter compiledFilter = new CompiledCaptchaChallengeFilter(filter);
                if (compiledFilter.isValid()) {
                    newCompiled.add(compiledFilter);
                }
            } catch (final Throwable e) {
                filter._setBroken(true);
                LogController.CL().log(e);
            }
        }
        this.compiled = newCompiled;
        if (eventSender.hasListener()) {
            eventSender.fireEvent(new ChangeEvent(CaptchaChallengeFilterController.this));
        }
    }

    public void update() {
        synchronized (this) {
            updateInternal();
        }
    }

    /**
     * Persists the current filter list and rebuilds the compiled view. Use this after editing existing rule objects in place (e.g. from the
     * settings table), since those edits do not go through {@link #add}/{@link #remove}/{@link #set}.
     */
    public void persist() {
        synchronized (this) {
            save(filters);
        }
        update();
    }

    /** Returns a copy of the current filter list. */
    public List<CaptchaChallengeFilter> list() {
        synchronized (this) {
            return new ArrayList<CaptchaChallengeFilter>(filters);
        }
    }

    /** Returns the subset of rules that are assigned to the given solver (plus rules that apply to all solvers). */
    public List<CaptchaChallengeFilter> listForSolver(final String solverId) {
        final ArrayList<CaptchaChallengeFilter> ret = new ArrayList<CaptchaChallengeFilter>();
        synchronized (this) {
            for (int i = 0; i < filters.size(); i++) {
                final CaptchaChallengeFilter filter = filters.get(i);
                if (appliesToSolver(filter, solverId)) {
                    ret.add(filter);
                }
            }
        }
        return ret;
    }

    public void add(final CaptchaChallengeFilter filter) {
        if (filter == null) {
            return;
        }
        final ArrayList<CaptchaChallengeFilter> single = new ArrayList<CaptchaChallengeFilter>();
        single.add(filter);
        addAll(single);
    }

    public void addAll(final List<CaptchaChallengeFilter> all) {
        if (all == null || all.isEmpty()) {
            return;
        }
        synchronized (this) {
            boolean modified = false;
            for (int i = 0; i < all.size(); i++) {
                final CaptchaChallengeFilter filter = all.get(i);
                if (filter == null) {
                    continue;
                }
                filters.add(filter);
                modified = true;
            }
            if (!modified) {
                return;
            }
            save(filters);
        }
        update();
    }

    public void remove(final CaptchaChallengeFilter filter) {
        if (filter == null) {
            return;
        }
        synchronized (this) {
            if (!filters.remove(filter)) {
                return;
            }
            save(filters);
        }
        update();
    }

    /** Moves the given rule one position up or down and re-numbers all positions accordingly. */
    public void move(final CaptchaChallengeFilter filter, final boolean up) {
        if (filter == null) {
            return;
        }
        synchronized (this) {
            final int index = filters.indexOf(filter);
            if (index < 0) {
                return;
            }
            final int target = up ? index - 1 : index + 1;
            if (target < 0 || target >= filters.size()) {
                return;
            }
            filters.remove(index);
            filters.add(target, filter);
            for (int i = 0; i < filters.size(); i++) {
                filters.get(i).setPosition(i);
            }
            save(filters);
        }
        update();
    }

    /** Writes the given rules as JSON to the target file, overwriting an existing file. */
    public void exportList(final File target, final List<CaptchaChallengeFilter> rules) {
        try {
            if (target.exists() && !target.delete()) {
                throw new IOException("Could not delete/overwrite:" + target);
            }
            IO.writeStringToFile(target, JSonStorage.serializeToJson(rules));
        } catch (final IOException e) {
            LogController.CL().log(e);
        }
    }

    /** Reads rules from the given JSON file and adds them via {@link #addAll(java.util.List)}. */
    public void importList(final File file) {
        final ArrayList<CaptchaChallengeFilter> contents;
        try {
            contents = JSonStorage.restoreFromString(IO.readFileToString(file), new TypeRef<ArrayList<CaptchaChallengeFilter>>() {
            });
        } catch (final Throwable e) {
            LogController.CL().log(e);
            return;
        }
        addAll(contents);
    }

    public void set(final List<CaptchaChallengeFilter> newList) {
        synchronized (this) {
            final ArrayList<CaptchaChallengeFilter> replacement = new ArrayList<CaptchaChallengeFilter>();
            if (newList != null) {
                replacement.addAll(newList);
            }
            filters = replacement;
            save(filters);
        }
        update();
    }

    private void save(final ArrayList<CaptchaChallengeFilter> toSave) {
        if (config == null) {
            return;
        }
        final EventSuppressor<ConfigEvent> eventSuppressor;
        if (filterListHandler != null) {
            final Thread thread = Thread.currentThread();
            eventSuppressor = new EventSuppressor<ConfigEvent>() {
                @Override
                public boolean suppressEvent(final ConfigEvent eventType) {
                    return Thread.currentThread() == thread;
                }
            };
            filterListHandler.getEventSender().addEventSuppressor(eventSuppressor);
        } else {
            eventSuppressor = null;
        }
        try {
            config.setFilterList(toSave);
        } finally {
            if (filterListHandler != null) {
                filterListHandler.getEventSender().removeEventSuppressor(eventSuppressor);
            }
        }
    }

    /** Returns the global master toggle for the filter list. */
    public boolean isFilterListEnabled() {
        if (config == null) {
            return true;
        }
        return config.isFilterListEnabled();
    }

    public void setFilterListEnabled(final boolean enabled) {
        if (config == null) {
            return;
        }
        config.setFilterListEnabled(enabled);
    }

    /**
     * Evaluates the given challenge against the configured filter rules for the given solver. Only rules assigned to that solver (or to all
     * solvers) are considered; they are evaluated in position order and the first matching rule wins.
     *
     * @param c
     *            the challenge to check
     * @param solverId
     *            identifier of the solver asking (its {@link SolverService#getName()})
     * @return the filter result: NOT_FILTERED (allowed), FILTERED_BLACKLIST (blocked) or FILTERED_WHITELIST (explicitly allowed)
     */
    public CaptchaChallengeFilterResult getFilterResult(final Challenge<?> c, final String solverId) {
        if (!isFilterListEnabled()) {
            /* Filter list disabled by user -> nothing is filtered */
            return CaptchaChallengeFilterResult.NOT_FILTERED;
        }
        final List<CompiledCaptchaChallengeFilter> compiledList = this.compiled;
        if (compiledList == null || compiledList.isEmpty()) {
            return CaptchaChallengeFilterResult.NOT_FILTERED;
        }
        for (int i = 0; i < compiledList.size(); i++) {
            final CompiledCaptchaChallengeFilter compiledFilter = compiledList.get(i);
            if (!appliesToSolver(compiledFilter.getFilter(), solverId)) {
                /* Rule belongs to a different solver */
                continue;
            }
            if (!compiledFilter.matches(c)) {
                /* Rule does not match this challenge */
                continue;
            }
            switch (compiledFilter.getFilterType()) {
            case WHITELIST:
                return CaptchaChallengeFilterResult.FILTERED_WHITELIST;
            case BLACKLIST:
            default:
                return CaptchaChallengeFilterResult.FILTERED_BLACKLIST;
            }
        }
        /* No rule matched -> default behaviour: nothing is filtered */
        return CaptchaChallengeFilterResult.NOT_FILTERED;
    }

    /** Returns true if the given rule applies to the given solver. An empty/null solver assignment means the rule applies to all solvers. */
    private boolean appliesToSolver(final CaptchaChallengeFilter filter, final String solverId) {
        final String assigned = filter.getSolver();
        if (StringUtils.isEmpty(assigned)) {
            return true;
        }
        return StringUtils.equalsIgnoreCase(assigned, solverId);
    }
}
