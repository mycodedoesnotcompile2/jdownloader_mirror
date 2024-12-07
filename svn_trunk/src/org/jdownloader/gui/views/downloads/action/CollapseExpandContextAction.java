package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.util.ArrayList;

import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.gui.swing.jdgui.MainTabbedPane;

import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.EDTSelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.translate._JDT;

public class CollapseExpandContextAction extends CustomizableTableContextAppAction implements ActionContext {
    public CollapseExpandContextAction() {
        super(true, true);
        setIconKey(IconKey.ICON_LIST);

        setTooltipText(_GUI.T.CollapseExpandAllAction_CollapseExpandAllAction());
        updateLabelAndIcon();
    }

    private void updateLabelAndIcon() {
        if (isSelectionOnly()) {
            setName(_GUI.T.CollapseExpandAllAction_CollapseExpandAllAction_selectiononly());
        } else {
            setName(_GUI.T.CollapseExpandAllAction_CollapseExpandAllAction_());
        }
    }

    @Override
    public void initContextDefaults() {
        super.initContextDefaults();
        setSelectionOnly(false);
    }

    private boolean selectionOnly = false;

    @Override
    public void requestUpdate(Object requestor) {
        super.requestUpdate(requestor);
        updateLabelAndIcon();
    }

    public static String getTranslationForSelectionOnly() {
        return _JDT.T.CollapseExpandContextAction_getTranslationForSelectionOnly();
    }

    @Customizer(link = "#getTranslationForSelectionOnly")
    public boolean isSelectionOnly() {
        return selectionOnly;

    }

    public void setSelectionOnly(boolean selectionOnly) {
        this.selectionOnly = selectionOnly;
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
            }, isSelectionOnly() ? SelectionType.SELECTED : SelectionType.ALL);
        }
    }

    private PackageControllerTable<?, ?> getTable() {
        if (MainTabbedPane.getInstance().isDownloadView()) {
            return DownloadsTable.getInstance();
        } else if (MainTabbedPane.getInstance().isLinkgrabberView()) {
            return LinkGrabberTable.getInstance();
        }
        return null;
    }

}
