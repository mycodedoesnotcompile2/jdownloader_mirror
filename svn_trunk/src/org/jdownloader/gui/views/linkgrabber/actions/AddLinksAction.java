package org.jdownloader.gui.views.linkgrabber.actions;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.linkgrabber.addlinksdialog.AddLinksDialog;

import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkCollector.MoveToDownloadlistMode;

public class AddLinksAction extends CustomizableAppAction implements ActionContext {
    /**
     *
     */
    private static final long serialVersionUID = -1824957567580275989L;

    public static final String MOVE_TO_DOWNLOADLIST_MODE = "moveToDownloadlistMode";
    private MoveToDownloadlistMode moveToDownloadlistMode = MoveToDownloadlistMode.UNCHANGED;

    public static String getTranslationMoveToDownloadlistMode() {
        return _GUI.T.PackagizerFilterRuleDialog_layoutDialogContent_moveToDownloadlistMode();
    }

    /**
     * @return how the added links are moved to the downloadlist
     */
    @Customizer(link = "#getTranslationMoveToDownloadlistMode")
    public MoveToDownloadlistMode getMoveToDownloadlistMode() {
        return moveToDownloadlistMode;
    }

    /**
     * @param moveToDownloadlistMode
     *            how the added links are moved to the downloadlist
     */
    public void setMoveToDownloadlistMode(MoveToDownloadlistMode moveToDownloadlistMode) {
        if (moveToDownloadlistMode == null) {
            moveToDownloadlistMode = MoveToDownloadlistMode.UNCHANGED;
        }
        this.moveToDownloadlistMode = moveToDownloadlistMode;
    }

    public AddLinksAction(String string) {
        super();
        setName(string);
        setIconKey(IconKey.ICON_ADD);
        setTooltipText(_GUI.T.AddLinksAction_AddLinksAction_tt());
        setAccelerator(KeyEvent.VK_L, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
    }

    public AddLinksAction() {
        this(_GUI.T.AddLinksToLinkgrabberAction());
    }

    @Override
    public void setEnabled(boolean newValue) {
        super.setEnabled(true);
    }

    public void actionPerformed(ActionEvent e) {
        new Thread("AddLinksAction") {
            public void run() {
                try {
                    AddLinksDialog dialog = new AddLinksDialog();
                    dialog.setMoveToDownloadlistMode(getMoveToDownloadlistMode());
                    UIOManager.I().show(null, dialog);
                    dialog.throwCloseExceptions();
                    final LinkCollectingJob crawljob = dialog.getReturnValue();
                    AddLinksProgress d = new AddLinksProgress(crawljob);
                    if (d.isHiddenByDontShowAgain()) {
                        d.getAddLinksDialogThread(crawljob, null).start();
                    } else {
                        Dialog.getInstance().showDialog(d);
                    }
                } catch (DialogNoAnswerException e1) {
                }
            }
        }.start();
    }
}
