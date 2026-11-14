package org.jdownloader.gui.views.components.packagetable;

import java.util.HashSet;
import java.util.Set;

import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

/**
 * Keeps selection and scroll position of a {@link PackageControllerTable} when filters are switched. One instance per table.
 *
 * Two kinds of filters are handled differently:
 *
 * Search (setFilterActive): A short "look something up" action. When the search starts, the state of the table is remembered. When it
 * ends, that state is restored, but only if the user did not select anything in the meantime: any selection change made by the user while
 * searching drops the remembered state (e.g. search, select, clear search, move the selected links). Then the current selection is taken
 * over instead. Scrolling does not drop it.
 *
 * Filters and views which may stay active for hours (linkgrabber quick filters, download list views) never go back to an older state, the
 * user would not expect that hours later. On every switch only the current state is taken over (keepStateAcrossRebuild): selected nodes
 * stay selected, without a selection the scroll position is kept. Switching them while a search is active does not drop the remembered
 * state of the search.
 */
public class UnfilteredTableState {
    private final PackageControllerTable<?, ?> table;
    /* Search sources which currently filter the table */
    private final Set<Object>                  activeSources = new HashSet<Object>();
    /* State of the table from before the search started, null if nothing is remembered (or dropped by a user selection) */
    private volatile TableStateSnapshot        unfiltered    = null;
    /* > 0 while this class applies a state itself: these selection changes are no user selection. EDT only. */
    private int                                applying      = 0;

    public UnfilteredTableState(final PackageControllerTable<?, ?> table) {
        this.table = table;
        table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(final ListSelectionEvent e) {
                if (applying > 0 || table.getModel().isTableStructureChanging()) {
                    /* Selection restored by a rebuild of the model (incl. guessSelectedRows) or by this class: not made by the user */
                    return;
                }
                onUserSelection();
            }
        });
    }

    /** @return true if any search source currently filters the table */
    public synchronized boolean isFiltered() {
        return activeSources.size() > 0;
    }

    /**
     * The user changed the selection: a remembered state from before the search is dropped, the current selection is taken over when the
     * search ends. Called by the selection listener; selections made programmatically on behalf of the user during a rebuild of the model
     * (e.g. QuickViewSelection of the linkgrabber sidebar) have to call it themselves.
     */
    public void onUserSelection() {
        if (unfiltered != null && isFiltered()) {
            unfiltered = null;
        }
    }

    /**
     * Reports whether the given search source filters the table. Has to be called after the filter itself has been changed, but before
     * recreateModel: the table still shows the old rows then, so its state can be remembered, and the restore only runs after a rebuild
     * which already uses the new filter. Changing the search text while searching is no start or end.
     */
    public void setFilterActive(final Object source, final boolean active) {
        final boolean started;
        final boolean ended;
        synchronized (this) {
            final boolean wasFiltered = activeSources.size() > 0;
            if (active) {
                activeSources.add(source);
            } else {
                activeSources.remove(source);
            }
            final boolean filtered = activeSources.size() > 0;
            started = !wasFiltered && filtered;
            ended = wasFiltered && !filtered;
        }
        /* Snapshots are created outside of the lock: create blocks until the EDT has read the table */
        if (started) {
            /*
             * Search starts: Remember the table as it is right now, before the filter is applied. unfiltered is only set here if a previous
             * snapshot has not been restored yet (user started a new search too fast), then the older one is kept.
             */
            if (unfiltered == null) {
                unfiltered = TableStateSnapshot.create(table);
            }
        } else if (ended) {
            final TableStateSnapshot searchState = TableStateSnapshot.create(table);
            TableStateSnapshot.runAfterRebuild(table, new Runnable() {
                @Override
                public void run() {
                    if (isFiltered()) {
                        /* A new search has been started in the meantime: Keep the snapshot for its end */
                        return;
                    }
                    final TableStateSnapshot snapshot = unfiltered;
                    unfiltered = null;
                    apply(new Runnable() {
                        @Override
                        public void run() {
                            if (snapshot != null) {
                                /* The user did not select anything while searching: back to the state from before the search */
                                snapshot.restore(table);
                            } else {
                                /* The user selected something (or nothing was remembered): take over the current state */
                                searchState.carryOverSelectionOrPosition(table);
                            }
                        }
                    });
                }
            });
        }
    }

    /**
     * For filters and views which may stay active for a long time (linkgrabber quick filters, download list views): takes over the current
     * state into the table rebuilt with the switched filter, see TableStateSnapshot.carryOverSelectionOrPosition. Nothing older is
     * restored. Has to be called after the filter has been switched, but before recreateModel.
     */
    public void keepStateAcrossRebuild() {
        final TableStateSnapshot current = TableStateSnapshot.create(table);
        TableStateSnapshot.runAfterRebuild(table, new Runnable() {
            @Override
            public void run() {
                apply(new Runnable() {
                    @Override
                    public void run() {
                        current.carryOverSelectionOrPosition(table);
                    }
                });
            }
        });
    }

    /** Runs a state change of this class (EDT), its selection changes are not counted as user selection. */
    private void apply(final Runnable task) {
        applying++;
        try {
            task.run();
        } finally {
            applying--;
        }
    }
}
