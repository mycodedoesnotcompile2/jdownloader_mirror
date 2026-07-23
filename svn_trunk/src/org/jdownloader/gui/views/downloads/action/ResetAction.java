package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.config.annotations.LabelInterface;
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
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.plugins.config.Order;

import jd.controlling.TaskQueue;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.gui.UserIO;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

public class ResetAction extends CustomizableTableContextAppAction<FilePackage, DownloadLink> implements ActionContext {
    private static final long   serialVersionUID     = -5583373118359478729L;
    private final static String NAME                 = _GUI.T.gui_table_contextmenu_reset();
    private DeleteOption        deleteMode           = DeleteOption.NULL;
    private boolean             includeDisabledLinks = true;
    private ConfirmDialogMode   confirmDialogMode    = ConfirmDialogMode.ALWAYS;

    public static enum ConfirmDialogMode implements LabelInterface {
        ALWAYS {
            @Override
            public String getLabel() {
                return "Always";
            }
        },
        IF_FILES_ARE_DELETED {
            @Override
            public String getLabel() {
                return "If at least one file will be deleted";
            }
        },
        NEVER {
            @Override
            public String getLabel() {
                return "Never";
            }
        };
    }

    public ResetAction() {
        setIconKey(IconKey.ICON_UNDO);
        setName(NAME);
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
    }

    @Customizer(link = "#getTranslationForConfirmDialogMode")
    @Order(30)
    public ConfirmDialogMode getConfirmDialogMode() {
        return confirmDialogMode;
    }

    public void setConfirmDialogMode(ConfirmDialogMode mode) {
        this.confirmDialogMode = mode;
    }

    @Override
    public boolean isEnabled() {
        if (!super.isEnabled()) {
            return false;
        }
        if (isIncludeDisabledLinks()) {
            return true;
        }
        for (final DownloadLink link : getSelection().getChildren()) {
            if (link.isEnabled()) {
                return true;
            }
        }
        return false;
    }

    private void reset(final List<DownloadLink> selection) {
        if (selection == null) {
            return;
        } else if (selection.isEmpty()) {
            return;
        }
        final List<DownloadLink> filteredSelection;
        if (isIncludeDisabledLinks()) {
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
        final DeleteOption deleteMode = this.getDeleteMode();
        final ConfirmDialogMode confirmDialogMode = this.getConfirmDialogMode();
        final ResetSettings settings = new ResetSettings();
        settings.setDeleteMode(this.getDeleteMode());
        settings.setIncludeDisabledLinks(this.isIncludeDisabledLinks());
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                final DownloadLinkAggregator agg = new DownloadLinkAggregator();
                agg.setLocalFileUsageEnabled(true);
                agg.update(filteredSelection);
                final boolean showConfirmDialog;
                switch (confirmDialogMode) {
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
                            ConfirmDialog confirmDialog = new ConfirmDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL, _GUI.T.jd_gui_userio_defaulttitle_confirm(), question_text, UserIO.getDefaultIcon(question_text), null, null) {
                                @Override
                                public String getDontShowAgainKey() {
                                    return "org.jdownloader.gui.views.downloads.action.ResetAction";
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

    public void actionPerformed(ActionEvent e) {
        if (!isEnabled()) {
            return;
        }
        final SelectionInfo<FilePackage, DownloadLink> rawSelection = getSelection();
        reset(rawSelection.getChildren());
    }
}
