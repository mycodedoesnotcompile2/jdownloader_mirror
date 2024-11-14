package jd.gui.swing.jdgui.views.settings.panels.accountmanager;

import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.AbstractAction;

import jd.controlling.AccountController;
import jd.controlling.TaskQueue;
import jd.controlling.accountchecker.AccountChecker;
import jd.plugins.Account;

import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.helpdialogs.HelpDialog;
import org.jdownloader.gui.helpdialogs.MessageConfig;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;

public class RefreshAction extends AbstractAction {
    /**
     *
     */
    private static final long  serialVersionUID = 1L;
    private List<AccountEntry> selection;
    private boolean            ignoreSelection  = false;

    public RefreshAction() {
        this(null);
        this.putValue(AbstractAction.SMALL_ICON, new AbstractIcon(IconKey.ICON_REFRESH, 20));
        ignoreSelection = true;
    }

    public RefreshAction(List<AccountEntry> selectedObjects) {
        selection = selectedObjects;
        this.putValue(NAME, _GUI.T.settings_accountmanager_refresh());
        this.putValue(AbstractAction.SMALL_ICON, new AbstractIcon(IconKey.ICON_REFRESH, 16));
    }

    public void actionPerformed(ActionEvent e) {
        if (!isEnabled()) {
            /* Account was disabled -> Do nothing */
            return;
        }
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                boolean containedMultihosterAccount = false;
                if (selection == null) {
                    for (Account acc : AccountController.getInstance().list()) {
                        if (acc == null || acc.isEnabled() == false || acc.isValid() == false || acc.isTempDisabled()) {
                            continue;
                        }
                        AccountChecker.getInstance().check(acc, true);
                        containedMultihosterAccount |= acc.isMultiHost();
                    }
                } else {
                    for (AccountEntry accEntry : selection) {
                        Account acc = accEntry.getAccount();
                        if (acc == null || acc.isEnabled() == false) {
                            continue;
                        }
                        AccountChecker.getInstance().check(acc, true);
                        containedMultihosterAccount |= acc.isMultiHost();
                    }
                }
                if (containedMultihosterAccount) {
                    displayMultihosterDetailOverviewHelpDialog();
                }
                return null;
            }
        });
    }

    public static void displayMultihosterDetailOverviewHelpDialog() {
        HelpDialog.showIfAllowed(new MessageConfig(null, "downloadtabe_sortwarner", Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, "Multihoster detail overview", "Click on the wrench symbol to get a more detailed overview of the supported hosts.\r\nThe detailed overview allows you to disable specific hosts, view host specific limits and, in case of problems, see the reason of failure.\r\nAlternatively, you can navigate to this information via Settings -> Plugins", new AbstractIcon(IconKey.ICON_SORT, 32)));
    }

    @Override
    public boolean isEnabled() {
        if (ignoreSelection) {
            return true;
        } else {
            return selection != null && selection.size() > 0;
        }
    }
}
