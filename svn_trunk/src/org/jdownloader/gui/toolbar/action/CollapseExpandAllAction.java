package org.jdownloader.gui.toolbar.action;

import java.awt.event.ActionEvent;
import java.util.ArrayList;

import jd.controlling.packagecontroller.AbstractPackageNode;

import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.EDTSelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;

public class CollapseExpandAllAction extends SelectionBasedToolbarAction {
    public CollapseExpandAllAction() {
        setIconKey(IconKey.ICON_LIST);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        final PackageControllerTable table = getTable();
        if (table != null) {
            table.getSelectionInfo(new EDTSelectionInfoCallback() {

                @Override
                public boolean isCancelled() {
                    return false;
                }

                @Override
                public void onSelectionInfo(SelectionInfo selectionInfo) {
                    boolean allexpaned = true;
                    final ArrayList<AbstractPackageNode> list = new ArrayList<AbstractPackageNode>();
                    for (Object p : selectionInfo.getPackageViews()) {
                        final PackageView pv = (PackageView) p;
                        if (!pv.isExpanded()) {
                            allexpaned = false;
                        }
                        list.add(pv.getPackage());
                    }
                    table.getModel().setFilePackageExpand(!allexpaned, list.toArray(new AbstractPackageNode[] {}));
                }
            }, SelectionType.ALL);
        }
    }

    @Override
    protected String createTooltip() {
        return _GUI.T.CollapseExpandAllAction_CollapseExpandAllAction();
    }

    @Override
    public void onKeyModifier(int parameter) {
    }

    @Override
    protected void onSelectionUpdate() {
        if (getTable() == null) {
            setEnabled(false);
            return;
        } else {
            setEnabled(true);
        }

    }

}
