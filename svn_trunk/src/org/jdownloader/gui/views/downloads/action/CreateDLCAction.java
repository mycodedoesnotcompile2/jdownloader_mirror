package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;

import jd.controlling.TaskQueue;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.appwork.utils.event.queue.QueueAction;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.dlc.DLCFactory;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.logging.LogController;

public class CreateDLCAction extends CustomizableTableContextAppAction<FilePackage, DownloadLink> {

    private static final long   serialVersionUID = 7244681674979415222L;

    private final static String NAME             = _GUI.T.gui_table_contextmenu_dlc();

    public CreateDLCAction() {
        super();
        setIconKey(IconKey.ICON_LOGO_DLC);
        setName(NAME);
    }

    @Override
    protected void onActionPerformed(ActionEvent e, SelectionType selectionType, final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {

            @Override
            protected Void run() throws RuntimeException {
                final DLCFactory plugin = new DLCFactory();
                plugin.setLogger(LogController.CL(CreateDLCAction.class));
                plugin.createDLC(selectionInfo.getChildren());
                return null;
            }
        });
    }

}