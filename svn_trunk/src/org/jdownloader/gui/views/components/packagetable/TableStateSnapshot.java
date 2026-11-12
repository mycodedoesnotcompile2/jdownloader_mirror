package org.jdownloader.gui.views.components.packagetable;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import javax.swing.DefaultListSelectionModel;
import javax.swing.JViewport;
import javax.swing.ListSelectionModel;

import org.appwork.utils.swing.EDT;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTableModelData.PackageControllerTableModelDataPackage;

import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;

/**
 * Selection (including lead and anchor) and scroll position of a {@link PackageControllerTable}, e.g. from before a filter (search, view)
 * got applied. Both are lost when the table model is rebuilt with another filter, so they are remembered before and restored afterwards.
 *
 * The scroll position is stored as the first visible node plus the pixel offset of its row, not as plain y value, because the rows may
 * change in the meantime (nodes added or removed, other filter).
 *
 * All nodes are only weakly referenced, so a remembered state does not keep removed nodes in memory.
 *
 * A node counts as present in the table if it has its own row, or if it is a child hidden inside a collapsed package which passes the
 * filter. In the latter case its package is used instead (selected / scrolled to), the expanded state is not changed.
 *
 * If nothing was selected and the table was scrolled to its very beginning or end, that edge is restored instead of the first visible node:
 * new links are usually added at the end of the list, the user expects to see them again (and not the row which was the last one before
 * they were added).
 */
public class TableStateSnapshot<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> {
    private final PackageControllerTableSelectionModelInfo<PackageType, ChildrenType> selectionModelInfo;

    private TableStateSnapshot(PackageControllerTableSelectionModelInfo<PackageType, ChildrenType> selectionModelInfo) {
        this.selectionModelInfo = selectionModelInfo;
    }

    /**
     * Remembers selection and scroll position of the table. May be called from any thread: the table may only be accessed in the EDT,
     * therefore the values are read via EDTHelper (blocks until done).
     */
    public static <PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> TableStateSnapshot<PackageType, ChildrenType> create(final PackageControllerTable<PackageType, ChildrenType> table) {
        return new EDT<TableStateSnapshot<PackageType, ChildrenType>, RuntimeException>() {
            @Override
            protected TableStateSnapshot<PackageType, ChildrenType> runInEDT() throws RuntimeException {
                final SelectionInfo<PackageType, ChildrenType> selectionInfo = table.getSelectionInfo(true, true);
                if (selectionInfo instanceof PackageControllerTableSelectionModelInfo) {
                    return new TableStateSnapshot<PackageType, ChildrenType>((PackageControllerTableSelectionModelInfo<PackageType, ChildrenType>) selectionInfo);
                } else {
                    return new TableStateSnapshot<PackageType, ChildrenType>(new PackageControllerTableSelectionModelInfo<PackageType, ChildrenType>() {
                        @Override
                        public SelectionInfo<PackageType, ChildrenType> getSelectionInfo() {
                            return selectionInfo;
                        }

                        @Override
                        public AbstractNode getSelectionModelLead() {
                            return null;
                        }

                        @Override
                        public AbstractNode getSelectionModelAnchor() {
                            return null;
                        }

                        @Override
                        public AbstractNode getSelectionModelFirstVisible() {
                            return null;
                        }

                        @Override
                        public EDGE getSelectionModeltEdge() {
                            return EDGE.NONE;
                        }

                        @Override
                        public int getSelectionModelOffset() {
                            return 0;
                        }
                    });
                }
            }
        }.waitForAndSwallowInterrupt();
    }

    /**
     * Takes over the remembered selection into the (rebuilt) table: selects all remembered nodes which are present in the table, restores
     * lead and anchor and scrolls the lead node (or else the topmost selected node) into view, but only if it is not visible yet. Has to
     * run in the EDT after the table has been rebuilt.
     *
     * @return false if none of the remembered selected nodes is present in the table (nothing has been changed then)
     */
    public boolean carryOverSelection(final PackageControllerTable<?, ?> table) {
        final RowLookup lookup = new RowLookup(table.getModel());
        final List<AbstractNode> selection = selectionModelInfo.getSelectionInfo().getRawSelection();
        final List<AbstractNode> nodes = lookup.resolveAll(selection);
        if (nodes.size() == 0) {
            return false;
        }
        applySelection(table, lookup, nodes);
        int row = lookup.getRow(lookup.resolve(selectionModelInfo.getSelectionModelLead()));
        if (row < 0) {
            /* Lead node is not present: Topmost selected row */
            for (final AbstractNode node : nodes) {
                final int nodeRow = lookup.getRow(node);
                if (nodeRow >= 0 && (row < 0 || nodeRow < row)) {
                    row = nodeRow;
                }
            }
        }
        if (row >= 0) {
            /* Only the vertical position is adjusted, scrollRectToVisible does not scroll if the row is visible already */
            final Rectangle rowRect = table.getCellRect(row, 0, true);
            final Rectangle visible = table.getVisibleRect();
            rowRect.x = visible.x;
            rowRect.width = visible.width;
            table.scrollRectToVisible(rowRect);
        }
        return true;
    }

    /**
     * @return true if this snapshot has a selected node which is not selected in the other snapshot, i.e. the selection has been extended
     *         or changed in between. False if nothing is selected, or only nodes which were already selected in the other one.
     */
    public boolean hasSelectionNotIn(final TableStateSnapshot<PackageType, ChildrenType> other) {
        final Map<AbstractNode, Boolean> otherSelection = new IdentityHashMap<AbstractNode, Boolean>();
        for (final AbstractNode node : other.selectionModelInfo.getSelectionInfo().getRawSelection()) {
            if (node != null) {
                otherSelection.put(node, Boolean.TRUE);
            }
        }
        for (final AbstractNode node : selectionModelInfo.getSelectionInfo().getRawSelection()) {
            if (node != null && !otherSelection.containsKey(node)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Restores selection (including lead and anchor) and scroll position. Has to run in the EDT after the table has been rebuilt. Whatever
     * does not exist anymore (removed, finished or filtered nodes) is skipped, the remaining state is restored as far as possible.
     */
    public void restore(final PackageControllerTable<?, ?> table) {
        final RowLookup lookup = new RowLookup(table.getModel());
        /* Also clears the selection if nothing was selected */
        final List<AbstractNode> selection = selectionModelInfo.getSelectionInfo().getRawSelection();
        applySelection(table, lookup, lookup.resolveAll(selection));
        if (!(table.getParent() instanceof JViewport)) {
            return;
        }
        if (selection.size() == 0 && restoreEdge(table, (JViewport) table.getParent())) {
            return;
        }
        final AbstractNode node = selectionModelInfo.getSelectionModelFirstVisible();
        final AbstractNode resolved = lookup.resolve(node);
        final int row = lookup.getRow(resolved);
        if (row < 0) {
            /* The node is gone (or filtered): Leave the scroll position as it is */
            return;
        }
        /* The offset belongs to the row of the node itself, not to the row of its collapsed package */
        final int offset = resolved == node ? selectionModelInfo.getSelectionModelOffset() : 0;
        final JViewport viewport = (JViewport) table.getParent();
        final Point position = viewport.getViewPosition();
        /* Do not scroll beyond the end if the table has become shorter */
        final int maxY = Math.max(0, table.getPreferredSize().height - viewport.getExtentSize().height);
        position.y = Math.min(maxY, Math.max(0, table.getCellRect(row, 0, true).y + offset));
        viewport.setViewPosition(position);
    }

    /**
     * Nothing was selected and the table was scrolled to its beginning or end: scrolls to that edge again, the rows there may have changed
     * in the meantime (e.g. links added at the end).
     *
     * @return false if the table was not at an edge (nothing has been changed then)
     */
    private boolean restoreEdge(final PackageControllerTable<?, ?> table, final JViewport viewport) {
        final Point position = viewport.getViewPosition();
        switch (selectionModelInfo.getSelectionModeltEdge()) {
        case TOP:
            position.y = 0;
            break;
        case BOTTOM:
            position.y = Math.max(0, table.getPreferredSize().height - viewport.getExtentSize().height);
            break;
        default:
            return false;
        }
        viewport.setViewPosition(position);
        return true;
    }

    /** Selects the given (resolved) nodes and restores lead and anchor if they are present. */
    private void applySelection(final PackageControllerTable<?, ?> table, final RowLookup lookup, final List<AbstractNode> nodes) {
        table.getModel().setSelectedObjects(nodes);
        if (nodes.size() == 0) {
            return;
        }
        final int anchorRow = lookup.getRow(lookup.resolve(selectionModelInfo.getSelectionModelAnchor()));
        final int leadRow = lookup.getRow(lookup.resolve(selectionModelInfo.getSelectionModelLead()));
        final ListSelectionModel selectionModel = table.getSelectionModel();
        /* Both setAnchorSelectionIndex and moveLeadSelectionIndex do not change the selection itself */
        if (anchorRow >= 0) {
            selectionModel.setAnchorSelectionIndex(anchorRow);
        }
        if (leadRow >= 0 && selectionModel instanceof DefaultListSelectionModel) {
            ((DefaultListSelectionModel) selectionModel).moveLeadSelectionIndex(leadRow);
        }
    }

    /**
     * Finds the rows of nodes in the current table data. Does not use PackageControllerTableModel.getRowforObject: That one searches the
     * packages linearly for every single node, while a selection may contain thousands of nodes. Here the rows are indexed once.
     */
    private static class RowLookup {
        private final PackageControllerTableModel<?, ?> model;
        private final Map<AbstractNode, Integer>        rows              = new IdentityHashMap<AbstractNode, Integer>();
        /* Child hidden inside a collapsed package (but passing the filter) -> its package. Created on first use. */
        private Map<AbstractNode, AbstractNode>         collapsedChildren = null;

        private RowLookup(final PackageControllerTableModel<?, ?> model) {
            this.model = model;
            int row = 0;
            for (final AbstractNode node : model.getTableData()) {
                rows.put(node, row++);
            }
        }

        private int getRow(final AbstractNode node) {
            final Integer row = node == null ? null : rows.get(node);
            return row == null ? -1 : row.intValue();
        }

        /**
         * @return the node itself if it has its own row, its package if it is hidden inside a collapsed package, else null
         */
        private AbstractNode resolve(final AbstractNode node) {
            if (node == null) {
                return null;
            } else if (rows.containsKey(node)) {
                return node;
            } else if (!(node instanceof AbstractPackageChildrenNode)) {
                return null;
            }
            if (collapsedChildren == null) {
                collapsedChildren = new IdentityHashMap<AbstractNode, AbstractNode>();
                for (final PackageControllerTableModelDataPackage<?, ?> dataPackage : model.getTableData().getModelDataPackages()) {
                    if (dataPackage.isExpanded()) {
                        continue;
                    }
                    /* For a collapsed package these are the children passing the filter, they just have no rows */
                    final List<?> children = dataPackage.getVisibleChildren();
                    if (children == null) {
                        continue;
                    }
                    final AbstractNode pkg = (AbstractNode) dataPackage.getPackage();
                    for (final Object child : children) {
                        collapsedChildren.put((AbstractNode) child, pkg);
                    }
                }
            }
            final AbstractNode pkg = collapsedChildren.get(node);
            return pkg != null && rows.containsKey(pkg) ? pkg : null;
        }

        /** Resolves all still existing nodes, without duplicates (several children may resolve to the same package). */
        private List<AbstractNode> resolveAll(final List<AbstractNode> references) {
            final LinkedHashSet<AbstractNode> ret = new LinkedHashSet<AbstractNode>();
            for (final AbstractNode reference : references) {
                final AbstractNode resolved = resolve(reference);
                if (resolved != null) {
                    ret.add(resolved);
                }
            }
            return new ArrayList<AbstractNode>(ret);
        }
    }

    /**
     * Runs the given task in the EDT as soon as the table shows data rebuilt with the current filters. Has to be called after the filter
     * change, see PackageControllerTableModel.runAfterRebuild: a rebuild which was already running before (with the old filters) does not
     * trigger the task.
     */
    public static void runAfterRebuild(final PackageControllerTable<?, ?> table, final Runnable task) {
        table.getModel().runAfterRebuild(task);
    }
}
