package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;

import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.gui.swing.jdgui.MainTabbedPane;
import jd.gui.swing.jdgui.interfaces.View;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.appwork.swing.exttable.ExtTableEvent;
import org.appwork.swing.exttable.ExtTableListener;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.event.GUIEventSender;
import org.jdownloader.gui.toolbar.action.AbstractToolBarAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.DownloadsView;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.downloads.table.DownloadsTableModel;

public class ResumeToolbarAction extends AbstractToolBarAction implements ExtTableListener {
    /**
     *
     */
    private static final long   serialVersionUID = 1L;
    private final static String NAME             = _GUI.T.gui_table_contextmenu_resume();

    public ResumeToolbarAction() {
        setIconKey(IconKey.ICON_RESUME);
        setName(NAME);
        GUIEventSender.getInstance().addListener(this, true);
        onGuiMainTabSwitch(null, MainTabbedPane.getInstance().getSelectedView());
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (isEnabled()) {
            DownloadsTable.getInstance().getSelectionInfo(new SelectionInfoCallback<FilePackage, DownloadLink>() {
                @Override
                public boolean isCancelled() {
                    return false;
                }

                @Override
                public void onSelectionInfo(SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
                    DownloadWatchDog.getInstance().resume(selectionInfo.getChildren());
                }
            }, SelectionType.SELECTED);
        }
    }

    @Override
    public void onGuiMainTabSwitch(View oldView, final View newView) {
        if (newView instanceof DownloadsView) {
            DownloadsTableModel.getInstance().getTable().getEventSender().addListener(this, true);
            updateState();
        } else {
            DownloadsTableModel.getInstance().getTable().getEventSender().removeListener(this);
            setEnabled(false);
        }
    }

    private void updateState() {
        DownloadsTable.getInstance().getSelectionInfo(new SelectionInfoCallback<FilePackage, DownloadLink>() {
            @Override
            public boolean isCancelled() {
                return false;
            }

            @Override
            public void onSelectionInfo(final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        setEnabled(!selectionInfo.isEmpty());
                    }
                };
            }
        }, SelectionType.SELECTED);
    }

    @Override
    protected String createTooltip() {
        return NAME;
    }

    @Override
    public void onExtTableEvent(ExtTableEvent<?> event) {
        switch (event.getType()) {
        case SELECTION_CHANGED:
            updateState();
            break;
        default:
            break;
        }
    }
}
