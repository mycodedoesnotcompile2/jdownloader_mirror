package org.jdownloader.controlling.contextmenu;

import java.awt.event.ActionEvent;

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
    public void requestUpdate(Object requestor) {
        super.requestUpdate(requestor);
        requestTableContextUpdate();
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final SelectionType selectionType = getSelectionType();
        getSelection(new EDTSelectionInfoCallback<PackageType, ChildrenType>() {

            @Override
            public void onSelectionInfo(final SelectionInfo<PackageType, ChildrenType> selectionInfo) {
                onActionPerformed(e, selectionType, selectionInfo);
            }

            @Override
            public boolean isCancelled() {
                return false;
            }
        }, selectionType);
    }

    protected SelectionType getSelectionType() {
        return SelectionType.SELECTED;
    }

    protected void onActionPerformed(final ActionEvent e, SelectionType selectionType, SelectionInfo<PackageType, ChildrenType> selectionInfo) {
    }

    protected void requestTableContextUpdate() {
        boolean has = !isEmptyContext();
        if (tableContext != null) {
            if (has) {
                if (tableContext.isItemVisibleForSelections()) {
                    setVisible(true);
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
        } else if (!has) {
            setVisible(false);
            setEnabled(false);
        } else if (has) {
            setVisible(true);
        }
    }

    protected boolean isEmptyContext() {
        return !hasSelection(getSelection());
    }

    public TableContext getTableContext() {
        return tableContext;
    }

}
