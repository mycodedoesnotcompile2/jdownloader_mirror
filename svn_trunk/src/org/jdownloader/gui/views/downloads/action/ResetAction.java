package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.util.List;

import org.appwork.uio.UIOManager;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.controlling.DownloadLinkAggregator;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.downloads.action.ResetSettings.DeleteMode;
import org.jdownloader.gui.views.downloads.action.ResetSettings.DisabledItemsBehavior;
import org.jdownloader.plugins.config.Order;

import jd.controlling.TaskQueue;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.gui.UserIO;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

public class ResetAction extends CustomizableTableContextAppAction<FilePackage, DownloadLink> implements ActionContext {
    private static final long     serialVersionUID      = -5583373118359478729L;
    private final static String   NAME                  = _GUI.T.gui_table_contextmenu_reset();
    private DeleteMode            deleteMode            = DeleteMode.AUTO;
    private DisabledItemsBehavior disabledItemsBehavior = DisabledItemsBehavior.AUTO;

    public ResetAction() {
        setIconKey(IconKey.ICON_UNDO);
        setName(NAME);
    }

    public static String getTranslationForDeleteMode() {
        return "Delete mode";
    }

    public static String getTranslationForDisabledItemsBehavior() {
        return "Disabled items behavior";
    }

    @Customizer(link = "#getTranslationForDeleteMode")
    @Order(10)
    public DeleteMode getDeleteMode() {
        return deleteMode;
    }

    public void setDeleteMode(DeleteMode mode) {
        this.deleteMode = mode;
    }

    @Customizer(link = "#getTranslationForDisabledItemsBehavior")
    @Order(20)
    public DisabledItemsBehavior getDisabledItemsBehavior() {
        return disabledItemsBehavior;
    }

    public void setDisabledItemsBehavior(DisabledItemsBehavior behavior) {
        this.disabledItemsBehavior = behavior;
    }

    private void reset(final List<DownloadLink> selection) {
        if (selection == null) {
            return;
        } else if (selection.isEmpty()) {
            return;
        }
        final DeleteMode deleteMode = this.getDeleteMode();
        // TODO: Maybe grey out this action if user wants to have disabled links excluded && user has only selected disabled items?
        final ResetSettings settings = new ResetSettings();
        settings.setDeleteMode(this.getDeleteMode());
        settings.setDisabledItemsBehavior(this.getDisabledItemsBehavior());
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                final DownloadLinkAggregator agg = new DownloadLinkAggregator();
                agg.setLocalFileUsageEnabled(true);
                agg.update(selection);
                final String question_text;
                if (deleteMode == DeleteMode.MOVE_TO_TRASH) {
                    question_text = _GUI.T.gui_downloadlist_reset_recycle(agg.getTotalCount(), SizeFormatter.formatBytes(agg.getBytesLoaded()), agg.getLocalFileCount());
                } else {
                    question_text = _GUI.T.gui_downloadlist_reset_delete(agg.getTotalCount(), SizeFormatter.formatBytes(agg.getBytesLoaded()), agg.getLocalFileCount());
                }
                new EDTHelper<Void>() {
                    @Override
                    public Void edtRun() {
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
                        DownloadWatchDog.getInstance().reset(selection, settings);
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