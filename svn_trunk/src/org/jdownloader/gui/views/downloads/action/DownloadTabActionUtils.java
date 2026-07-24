package org.jdownloader.gui.views.downloads.action;

import java.util.ArrayList;
import java.util.List;

import jd.controlling.TaskQueue;
import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.gui.UserIO;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.WarnLevel;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.appwork.uio.CloseReason;
import org.appwork.uio.UIOManager;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.controlling.DownloadLinkAggregator;
import org.jdownloader.controlling.FileCreationManager.DeleteOption;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.DeleteFileOptions;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import org.jdownloader.utils.JDFileUtils;

public class DownloadTabActionUtils {
    private static final String RESET_DONT_SHOW_AGAIN_KEY = "org.jdownloader.gui.views.downloads.action.ResetAction";

    /**
     * Shared by the reset context menu action and its toolbar counterpart: filters the selection according to
     * settings.isIncludeDisabledLinks(), decides whether the confirm dialog is needed (depending on
     * settings.getConfirmDialogMode() & settings.getDeleteMode()) and finally triggers the reset.
     */
    public static void resetLinksRequest(final List<DownloadLink> selection, final ResetSettings settings) {
        if (selection == null || selection.isEmpty()) {
            return;
        }
        final boolean includeDisabledLinks = settings.isIncludeDisabledLinks();
        final List<DownloadLink> filteredSelection;
        if (includeDisabledLinks) {
            filteredSelection = selection;
        } else {
            filteredSelection = new ArrayList<DownloadLink>();
            for (final DownloadLink link : selection) {
                if (link.isEnabled()) {
                    filteredSelection.add(link);
                }
            }
            if (filteredSelection.isEmpty()) {
                return;
            }
        }
        final DeleteOption deleteMode = settings.getDeleteMode();
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                final DownloadLinkAggregator agg = new DownloadLinkAggregator();
                agg.setLocalFileUsageEnabled(true);
                agg.update(filteredSelection);
                final boolean showConfirmDialog;
                switch (settings.getConfirmDialogMode()) {
                case NEVER:
                    showConfirmDialog = false;
                    break;
                case IF_FILES_ARE_DELETED:
                    showConfirmDialog = deleteMode != DeleteOption.NO_DELETE && agg.getLocalFileCount() > 0;
                    break;
                case ALWAYS:
                default:
                    showConfirmDialog = true;
                    break;
                }
                final String question_text;
                if (deleteMode == DeleteOption.NO_DELETE) {
                    question_text = _GUI.T.gui_downloadlist_reset_nodelete(agg.getTotalCount(), SizeFormatter.formatBytes(agg.getBytesLoaded()), agg.getLocalFileCount());
                } else if (deleteMode == DeleteOption.RECYCLE) {
                    question_text = _GUI.T.gui_downloadlist_reset_recycle(agg.getTotalCount(), SizeFormatter.formatBytes(agg.getBytesLoaded()), agg.getLocalFileCount());
                } else {
                    question_text = _GUI.T.gui_downloadlist_reset_delete(agg.getTotalCount(), SizeFormatter.formatBytes(agg.getBytesLoaded()), agg.getLocalFileCount());
                }
                new EDTHelper<Void>() {
                    @Override
                    public Void edtRun() {
                        if (showConfirmDialog) {
                            final ConfirmDialog confirmDialog = new ConfirmDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL, _GUI.T.jd_gui_userio_defaulttitle_confirm(), question_text, UserIO.getDefaultIcon(question_text), null, null) {
                                @Override
                                public String getDontShowAgainKey() {
                                    return RESET_DONT_SHOW_AGAIN_KEY;
                                }
                            };
                            try {
                                Dialog.getInstance().showDialog(confirmDialog);
                            } catch (DialogClosedException e) {
                                e.printStackTrace();
                                return null;
                            } catch (DialogCanceledException e) {
                                e.printStackTrace();
                                return null;
                            }
                        }
                        DownloadWatchDog.getInstance().reset(filteredSelection, settings);
                        return null;
                    }
                }.start(true);
                return null;
            };
        });
    }

    /**
     * True if at least one of the given links is enabled. Shared enable-state check for the reset context menu action and its
     * toolbar counterpart.
     */
    public static boolean containsEnabledLink(final List<DownloadLink> links) {
        if (links == null) {
            return false;
        }
        for (final DownloadLink link : links) {
            if (link.isEnabled()) {
                return true;
            }
        }
        return false;
    }

    public static void deleteLinksRequest(final SelectionInfo<FilePackage, DownloadLink> si, final String msg, final DeleteFileOptions mode, final boolean byPassDialog) {
        final DownloadLinkAggregator agg = new DownloadLinkAggregator();
        agg.setMirrorHandlingEnabled(false);
        agg.setLocalFileUsageEnabled(true);
        agg.update(si.getChildren());
        if (agg.getTotalCount() > 0) {
            new EDTHelper<Void>() {
                @Override
                public Void edtRun() {
                    WarnLevel level = WarnLevel.LOW;
                    switch (mode) {
                    case REMOVE_LINKS_AND_DELETE_FILES:
                        if (agg.getBytesLoaded() > 0) {
                            level = WarnLevel.SEVERE;
                        } else {
                            level = WarnLevel.NORMAL;
                        }
                        break;
                    case REMOVE_LINKS_AND_RECYCLE_FILES:
                        if (agg.getBytesLoaded() > 0) {
                            level = WarnLevel.SEVERE;
                        } else if (agg.getFinishedCount() != agg.getTotalCount()) {
                            level = WarnLevel.NORMAL;
                        }
                        break;
                    case REMOVE_LINKS_ONLY:
                        if (agg.getBytesLoaded() > 0) {
                            level = WarnLevel.SEVERE;
                        } else if (agg.getFinishedCount() != agg.getTotalCount()) {
                            level = WarnLevel.NORMAL;
                        }
                        break;
                    }
                    final boolean finalByPassDialog;
                    if (!JDGui.bugme(level)) {
                        finalByPassDialog = true;
                    } else {
                        finalByPassDialog = byPassDialog;
                    }
                    if (!finalByPassDialog && !CFG_GUI.CFG.isBypassAllRlyDeleteDialogsEnabled()) {
                        final ConfirmDeleteLinksDialog dialog = new ConfirmDeleteLinksDialog(msg + "\r\n" + _GUI.T.DeleteSelectionAction_actionPerformed_affected2(agg.getTotalCount(), SizeFormatter.formatBytes(agg.getBytesLoaded()), DownloadController.getInstance().getChildrenCount() - agg.getTotalCount(), agg.getLocalFileCount()), agg.getBytesLoaded());
                        dialog.setRecycleSupported(JDFileUtils.isTrashSupported());
                        dialog.setMode(mode);
                        dialog.show();
                        if (dialog.getCloseReason() == CloseReason.OK) {
                            switch (dialog.getMode()) {
                            case REMOVE_LINKS_ONLY:
                                DownloadController.getInstance().removeChildren(si.getChildren());
                                break;
                            case REMOVE_LINKS_AND_DELETE_FILES:
                                DownloadController.getInstance().removeChildren(si.getChildren());
                                DownloadWatchDog.getInstance().delete(si.getChildren(), DeleteOption.NULL);
                                break;
                            case REMOVE_LINKS_AND_RECYCLE_FILES:
                                DownloadController.getInstance().removeChildren(si.getChildren());
                                DownloadWatchDog.getInstance().delete(si.getChildren(), DeleteOption.RECYCLE);
                                break;
                            }
                        }
                    } else {
                        switch (mode) {
                        case REMOVE_LINKS_ONLY:
                            DownloadController.getInstance().removeChildren(si.getChildren());
                            break;
                        case REMOVE_LINKS_AND_DELETE_FILES:
                            DownloadController.getInstance().removeChildren(si.getChildren());
                            DownloadWatchDog.getInstance().delete(si.getChildren(), DeleteOption.NULL);
                            break;
                        case REMOVE_LINKS_AND_RECYCLE_FILES:
                            DownloadController.getInstance().removeChildren(si.getChildren());
                            // TODO: different handling dialog<->no dialog
                            DownloadWatchDog.getInstance().delete(si.getChildren(), JDFileUtils.isTrashSupported() ? DeleteOption.RECYCLE : DeleteOption.NULL);
                            break;
                        }
                    }
                    return null;
                }
            }.start(true);
        }
    }
}
