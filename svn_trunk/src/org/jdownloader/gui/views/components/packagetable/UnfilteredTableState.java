package org.jdownloader.gui.views.components.packagetable;

import java.util.HashSet;
import java.util.Set;

/**
 * Remembers selection and scroll position of a {@link PackageControllerTable} from before any filter got applied (search, linkgrabber quick
 * filters) and restores them once all filters are gone again. One instance per table, all filter sources of that table share it: the
 * snapshot is taken when the first source gets active (nothing filtered -> filtered) and restored when the last one gets inactive
 * (filtered -> nothing filtered). Switching between filters, or changing a filter while another one stays active, does not touch it.
 *
 * When filtering ends, a selection which the user changed in the filtered view (selected something which was not selected before) is
 * taken over into the unfiltered table. Otherwise the snapshot is restored: this brings back the selected nodes which got hidden by the
 * filter, and the scroll position.
 *
 * Only changes made by the user start or end the remembering. Automatic changes (e.g. new links of a hoster whose quick filter is
 * disabled, stored filter states after a restart) only update which sources are active: no snapshot is taken for them, and if all
 * filters vanish automatically, a remembered snapshot is dropped instead of restored.
 */
public class UnfilteredTableState {
    private final PackageControllerTable<?, ?> table;
    /* Filter sources which currently filter the table */
    private final Set<Object>                  activeSources = new HashSet<Object>();
    /* State of the table from before filtering started, null if nothing is remembered. Cleared when it has been restored. */
    private volatile TableStateSnapshot        unfiltered    = null;

    public UnfilteredTableState(final PackageControllerTable<?, ?> table) {
        this.table = table;
    }

    /** @return true if any source currently filters the table */
    public synchronized boolean isFiltered() {
        return activeSources.size() > 0;
    }

    /**
     * Reports whether the given source filters the table. Has to be called after the filter itself has been changed (filter added /
     * removed, flag switched), but before recreateModel: the table still shows the old rows then, so its state can be remembered, and the
     * restore only runs after a rebuild which already uses the new filters.
     *
     * @param source
     *            identifies the filter source, e.g. the search field
     * @param active
     *            true if the source filters the table now
     * @param userAction
     *            true if the user made this change, false for automatic changes
     */
    public void setFilterActive(final Object source, final boolean active, final boolean userAction) {
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
             * Filtering starts: Remember the table as it is right now, before the filter is applied. unfiltered is only set here if a
             * previous snapshot has not been restored yet (user started filtering again too fast), then the older one is kept.
             */
            if (userAction && unfiltered == null) {
                unfiltered = TableStateSnapshot.create(table);
            }
        } else if (ended) {
            if (userAction) {
                restoreAfterRebuild(TableStateSnapshot.create(table));
            } else {
                /* Filters vanished automatically: The view changes without the user doing anything, do not jump around */
                unfiltered = null;
            }
        }
    }

    /**
     * As soon as the table has been rebuilt without filters: Takes over the selection the user made in the filtered view, or else restores
     * the state from before filtering.
     *
     * @param filteredState
     *            state of the filtered view, created before the rebuild
     */
    private void restoreAfterRebuild(final TableStateSnapshot filteredState) {
        final TableStateSnapshot snapshot = unfiltered;
        if (snapshot == null) {
            return;
        }
        TableStateSnapshot.runAfterRebuild(table, new Runnable() {
            @Override
            public void run() {
                if (isFiltered() || unfiltered != snapshot) {
                    /* Filtering has started again in the meantime: Keep the snapshot for the next end */
                    return;
                }
                unfiltered = null;
                if (filteredState.hasSelectionNotIn(snapshot) && filteredState.carryOverSelection(table)) {
                    return;
                }
                snapshot.restore(table);
            }
        });
    }
}
