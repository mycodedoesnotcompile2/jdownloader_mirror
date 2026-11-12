package org.jdownloader.gui.views.components.packagetable;

import java.awt.Point;
import java.util.List;

import javax.swing.JViewport;

import org.appwork.utils.event.queue.Queue;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTableModelData.PackageControllerTableModelDataPackage;

import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;

public class PackageControllerTableModelSelectionInfo<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> extends SelectionInfo<PackageType, ChildrenType> implements PackageControllerTableSelectionModelInfo<PackageType, ChildrenType> {
    private final PackageControllerTableModelData<PackageType, ChildrenType> tableModelData;
    private final AbstractNode                                               rawContext;
    private final AbstractNode                                               lead;
    private final AbstractNode                                               anchor;
    private final EDGE                                                       edge;
    private final AbstractNode                                               firstVisible;
    private final int                                                        offset;

    protected PackageControllerTableModelSelectionInfo(final AbstractNode contextObject, final PackageControllerTableModel<PackageType, ChildrenType> tableModel) {
        super(tableModel.getController());
        this.rawContext = contextObject;
        this.tableModelData = tableModel.getTableData();
        final PackageControllerTable<PackageType, ChildrenType> table = tableModel.getTable();
        this.lead = getObjectByRow(tableModel, table.getSelectionModel().getLeadSelectionIndex());
        this.anchor = getObjectByRow(tableModel, table.getSelectionModel().getAnchorSelectionIndex());
        {
            EDGE edge = EDGE.NONE;
            AbstractNode firstVisible = null;
            int offset = 0;
            if (table.getParent() instanceof JViewport) {
                final JViewport viewport = (JViewport) table.getParent();
                final Point position = viewport.getViewPosition();
                final int maxY = table.getPreferredSize().height - viewport.getExtentSize().height;
                if (maxY > 0) {
                    if (position.y <= 0) {
                        edge = EDGE.TOP;
                    } else if (position.y >= maxY) {
                        edge = EDGE.BOTTOM;
                    }
                }
                /* Row at the top edge of the viewport, may be only partly visible */
                final int row = table.rowAtPoint(new Point(0, position.y));
                if (row >= 0) {
                    firstVisible = getObjectByRow(tableModel, row);
                    offset = position.y - table.getCellRect(row, 0, true).y;
                }
            }
            this.firstVisible = firstVisible;
            this.edge = edge;
            this.offset = offset;
        }
        aggregate();
    }

    protected static AbstractNode getObjectByRow(final PackageControllerTableModel<?, ?> model, final int row) {
        return row >= 0 && row < model.getRowCount() ? model.getObjectbyRow(row) : null;
    }

    @Override
    public AbstractNode getRawContext() {
        return rawContext;
    }

    @Override
    public List<AbstractNode> getRawSelection() {
        return tableModelData;
    }

    @Override
    protected void aggregate(Queue queue) {
        super.aggregate(null);
    }

    @Override
    protected void aggregate() {
        for (final PackageControllerTableModelDataPackage modelDataPackage : tableModelData.getModelDataPackages()) {
            final PackageType pkg = (PackageType) modelDataPackage.getPackage();
            final int index = children.size();
            final List<? extends AbstractNode> visible = modelDataPackage.getVisibleChildren();
            for (final AbstractNode node : visible) {
                children.add((ChildrenType) node);
            }
            final int size = visible.size();
            final PackageView<PackageType, ChildrenType> packageView = new PackageView<PackageType, ChildrenType>() {
                @Override
                public List<ChildrenType> getChildren() {
                    return children.subList(index, index + size);
                }

                @Override
                public PackageType getPackage() {
                    return pkg;
                }

                @Override
                public boolean isPackageSelected() {
                    return true;
                }

                @Override
                public boolean isExpanded() {
                    return modelDataPackage.isExpanded();
                }

                @Override
                public List<ChildrenType> getSelectedChildren() {
                    return children.subList(index, index + size);
                }
            };
            addPackageView(packageView, pkg);
        }
    }

    @Override
    public AbstractNode getSelectionModelLead() {
        return lead;
    }

    @Override
    public AbstractNode getSelectionModelAnchor() {
        return anchor;
    }

    @Override
    public AbstractNode getSelectionModelFirstVisible() {
        return firstVisible;
    }

    @Override
    public EDGE getSelectionModeltEdge() {
        return edge;
    }

    @Override
    public int getSelectionModelOffset() {
        return offset;
    }

    @Override
    public SelectionInfo<PackageType, ChildrenType> getSelectionInfo() {
        return this;
    }
}
