package jd.gui.swing.jdgui.views.settings.panels.accountmanager;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractAction;

import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.helpdialogs.HelpDialog;
import org.jdownloader.gui.helpdialogs.MessageConfig;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;

import jd.controlling.AccountController;
import jd.controlling.TaskQueue;
import jd.controlling.accountchecker.AccountChecker;
import jd.plugins.Account;

public class RefreshAction extends AbstractAction {
    /**
     *
     */
    private static final long        serialVersionUID = 1L;
    private final List<AccountEntry> selection;

    public RefreshAction() {
        selection = null;
        this.putValue(NAME, _GUI.T.settings_accountmanager_refresh());
        this.putValue(AbstractAction.SMALL_ICON, new AbstractIcon(IconKey.ICON_REFRESH, 16));
    }

    public RefreshAction(List<AccountEntry> selectedObjects) {
        selection = selectedObjects != null ? selectedObjects : new ArrayList<AccountEntry>();
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
                final List<Account> accountsToCheck = getAccountsToCheck();
                if (accountsToCheck == null || accountsToCheck.isEmpty()) {
                    /* Do nothing. This can happen if e.g. all selected items are disabled. */
                    return null;
                }
                boolean containedCheckedMultihosterAccount = false;
                for (final Account acc : accountsToCheck) {
                    AccountChecker.getInstance().check(acc, true);
                    containedCheckedMultihosterAccount |= acc.isMultiHost();
                }
                if (containedCheckedMultihosterAccount) {
                    displayMultihosterDetailOverviewHelpDialog();
                }
                return null;
            }
        });
    }

    private List<Account> getAccountsToCheck() {
        final List<Account> accountsToCheck = new ArrayList<Account>();
        if (selection == null) {
            /* All [enabled] accounts */
            for (final Account acc : AccountController.getInstance().list()) {
                if (acc.isEnabled() && acc.isValid()) {
                    accountsToCheck.add(acc);
                }
            }
        } else {
            /* Selected [enabled] accounts only */
            for (final AccountEntry accEntry : selection) {
                final Account acc = accEntry.getAccount();
                if (acc == null) {
                    continue;
                }
                accountsToCheck.add(acc);
            }
        }
        if (accountsToCheck.isEmpty()) {
            /* Do nothing. This can happen if e.g. all selected items are disabled. */
            return null;
        }
        return accountsToCheck;
    }

    public static void displayMultihosterDetailOverviewHelpDialog() {
        HelpDialog.showIfAllowed(new MessageConfig(null, "multihoster_table_detail_overview_hint", Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, _GUI.T.multihost_detailed_host_do_not_show_again_info_about_multi_host_overview_table_title(), _GUI.T.multihost_detailed_host_do_not_show_again_info_about_multi_host_overview_table_message(), new AbstractIcon(IconKey.ICON_SORT, 32)));
    }

    @Override
    public boolean isEnabled() {
        return selection == null || selection.size() > 0;
    }
}
