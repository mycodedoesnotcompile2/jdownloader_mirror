package org.jdownloader.controlling.contextmenu;

import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;

import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.EDTSelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;

public abstract class CustomizableTableContextAppAction<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> extends CustomizableSelectionAppAction<PackageType, ChildrenType> {
    protected TableContext tableContext;

    public CustomizableTableContextAppAction(boolean empty, boolean selection) {
        super();
        initTableContext(empty, selection);

    }

    public CustomizableTableContextAppAction() {
        super();
    }

    protected void initTableContext(boolean empty, boolean selection) {
        tableContext = new TableContext(empty, selection);
        addContextSetup(0, tableContext);
    }

    protected void removeTableContext() {
        removeContextSetup(tableContext);
    }

    @Override
    protected void requestUpdateSelection(final Object requestor) {
        final SelectionType selectionType = getSelectionType();
        getSelection(new EDTSelectionInfoCallback<PackageType, ChildrenType>() {
            @Override
            public boolean isCancelled() {
                return false;
            }

            public void onSelectionInfo(org.jdownloader.gui.views.SelectionInfo<PackageType, ChildrenType> selectionInfo) {
                onRequestUpdateSelection(requestor, selectionType, selectionInfo);
            };
        }, selectionType);
    }

    @Override
    protected void onRequestUpdateSelection(Object requestor, SelectionType selectionType, SelectionInfo<PackageType, ChildrenType> selectionInfo) {
        final boolean hasSelectoin = SelectionInfo.isNotEmpty(selectionInfo);
        if (tableContext != null) {
            if (hasSelectoin) {
                if (tableContext.isItemVisibleForSelections()) {
                    setVisible(true);
                    setEnabled(true);
                } else {
                    setVisible(false);
                    setEnabled(false);
                }
            } else {
                if (tableContext.isItemVisibleForEmptySelection()) {
                    setVisible(true);
                    setEnabled(true);
                } else {
                    setVisible(false);
                    setEnabled(false);
                }
            }
        } else if (!hasSelectoin) {
            setVisible(false);
            setEnabled(false);
        } else if (hasSelectoin) {
            setVisible(true);
            setEnabled(true);
        }
    }

    public TableContext getTableContext() {
        return tableContext;
    }

}
