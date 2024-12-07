package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;

import jd.gui.UserIO;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;

public class PackageNameAction extends CustomizableTableContextAppAction<FilePackage, DownloadLink> {

    private static final long   serialVersionUID = -5155537516674035401L;
    private final static String NAME             = _GUI.T.gui_table_contextmenu_editpackagename();

    public PackageNameAction() {
        setName(NAME);
        setIconKey(IconKey.ICON_EDIT);
    }

    @Override
    protected void onActionPerformed(ActionEvent e, SelectionType selectionType, SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        final String name = UserIO.getInstance().requestInputDialog(0, _GUI.T.gui_linklist_editpackagename_message(), selectionInfo.getFirstPackage().getName());
        if (name != null) {
            for (final PackageView<FilePackage, DownloadLink> packagee : selectionInfo.getPackageViews()) {
                packagee.getPackage().setName(name);
            }
        }
    }
}