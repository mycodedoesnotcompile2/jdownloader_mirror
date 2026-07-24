package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.util.List;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.swing.exttable.ExtTableEvent;
import org.appwork.swing.exttable.ExtTableListener;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.controlling.FileCreationManager.DeleteOption;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.controlling.download.DownloadControllerListener;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.event.GUIEventSender;
import org.jdownloader.gui.toolbar.action.AbstractToolBarAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.DownloadsView;
import org.jdownloader.gui.views.downloads.action.ResetSettings.ResetDownloadlinkActionConfirmDialogMode;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.downloads.table.DownloadsTableModel;
import org.jdownloader.plugins.config.Order;

import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.packagecontroller.AbstractNode;
import jd.gui.swing.jdgui.MainTabbedPane;
import jd.gui.swing.jdgui.interfaces.View;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLinkProperty;
import jd.plugins.FilePackage;
import jd.plugins.FilePackageProperty;

public class ResetToolbarAction extends AbstractToolBarAction implements ActionContext, ExtTableListener, DownloadControllerListener {
    private static final long                        serialVersionUID     = 1L;
    private final static String                      NAME                 = _GUI.T.gui_table_contextmenu_reset();
    private static final ResetSettings               DEFAULT_SETTINGS     = new ResetSettings();
    private DeleteOption                             deleteMode           = DEFAULT_SETTINGS.getDeleteMode();
    private boolean                                  includeDisabledLinks = DEFAULT_SETTINGS.isIncludeDisabledLinks();
    private ResetDownloadlinkActionConfirmDialogMode confirmDialogMode    = DEFAULT_SETTINGS.getConfirmDialogMode();
    private final DelayedRunnable                    delayer;

    public ResetToolbarAction() {
        setIconKey(IconKey.ICON_UNDO);
        setName(NAME);
        delayer = new DelayedRunnable(500, 1500) {
            @Override
            public void delayedrun() {
                updateState();
            }
        };
        GUIEventSender.getInstance().addListener(this, true);
        DownloadController.getInstance().addListener(this, true);
        onGuiMainTabSwitch(null, MainTabbedPane.getInstance().getSelectedView());
    }

    @Override
    public void initContextDefaults() {
        setVisibleInLinkgrabberTab(false);
        setVisibleInAllTabs(false);
    }

    public static String getTranslationForDeleteMode() {
        return "Delete mode";
    }

    public static String getTranslationForIncludeDisabledLinks() {
        return "Include disabled links";
    }

    public static String getTranslationForConfirmDialogMode() {
        return "Show confirmation dialog";
    }

    @Customizer(link = "#getTranslationForDeleteMode")
    @Order(10)
    public DeleteOption getDeleteMode() {
        return deleteMode;
    }

    public void setDeleteMode(DeleteOption mode) {
        this.deleteMode = mode;
    }

    @Customizer(link = "#getTranslationForIncludeDisabledLinks")
    @Order(20)
    public boolean isIncludeDisabledLinks() {
        return includeDisabledLinks;
    }

    public void setIncludeDisabledLinks(boolean includeDisabledLinks) {
        this.includeDisabledLinks = includeDisabledLinks;
        updateState();
    }

    /**
     * The customizer settings dialog doesn't call the setters directly: it stores the changed values and re-applies all
     * @Customizer properties via CustomizableAppAction#fill(...), which is also what requestUpdate(...) triggers. Without this
     * override, toggling includeDisabledLinks (or the other settings) in the dialog wouldn't reliably refresh this button's
     * enabled state.
     */
    @Override
    public void requestUpdate(Object requestor) {
        super.requestUpdate(requestor);
        updateState();
    }

    @Customizer(link = "#getTranslationForConfirmDialogMode")
    @Order(30)
    public ResetDownloadlinkActionConfirmDialogMode getConfirmDialogMode() {
        return confirmDialogMode;
    }

    public void setConfirmDialogMode(ResetDownloadlinkActionConfirmDialogMode mode) {
        this.confirmDialogMode = mode;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (!isEnabled()) {
            return;
        }
        DownloadsTable.getInstance().getSelectionInfo(new SelectionInfoCallback<FilePackage, DownloadLink>() {
            @Override
            public boolean isCancelled() {
                return false;
            }

            @Override
            public void onSelectionInfo(SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
                final ResetSettings settings = new ResetSettings();
                settings.setDeleteMode(getDeleteMode());
                settings.setIncludeDisabledLinks(isIncludeDisabledLinks());
                settings.setConfirmDialogMode(getConfirmDialogMode());
                DownloadTabActionUtils.resetLinksRequest(selectionInfo.getChildren(), settings);
            }
        }, SelectionType.SELECTED);
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
                        if (SelectionInfo.isEmpty(selectionInfo)) {
                            setEnabled(false);
                        } else if (isIncludeDisabledLinks()) {
                            setEnabled(true);
                        } else {
                            setEnabled(DownloadTabActionUtils.containsEnabledLink(selectionInfo.getChildren()));
                        }
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

    @Override
    public void onDownloadControllerAddedPackage(FilePackage pkg) {
    }

    @Override
    public void onDownloadControllerRemovedLinklist(List<DownloadLink> list) {
        delayer.resetAndStart();
    }

    @Override
    public void onDownloadControllerRemovedPackage(FilePackage pkg) {
    }

    @Override
    public void onDownloadControllerStructureRefresh() {
        delayer.resetAndStart();
    }

    @Override
    public void onDownloadControllerStructureRefresh(AbstractNode node, Object param) {
        delayer.resetAndStart();
    }

    @Override
    public void onDownloadControllerStructureRefresh(FilePackage pkg) {
        delayer.resetAndStart();
    }

    @Override
    public void onDownloadControllerUpdatedData(DownloadLink downloadlink) {
        delayer.resetAndStart();
    }

    @Override
    public void onDownloadControllerUpdatedData(DownloadLink downloadlink, DownloadLinkProperty property) {
        delayer.resetAndStart();
    }

    @Override
    public void onDownloadControllerUpdatedData(FilePackage pkg) {
    }

    @Override
    public void onDownloadControllerUpdatedData(FilePackage pkg, FilePackageProperty property) {
    }
}
