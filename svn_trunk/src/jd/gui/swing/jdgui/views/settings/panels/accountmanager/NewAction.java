package jd.gui.swing.jdgui.views.settings.panels.accountmanager;

import java.awt.event.ActionEvent;

import jd.gui.swing.dialog.AddAccountDialog;
import jd.gui.swing.jdgui.views.settings.ConfigurationView;
import jd.plugins.Account;
import jd.plugins.PluginForHost;

import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.components.AbstractAddAction;

public class NewAction extends AbstractAddAction {
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public NewAction() {
        super();
    }

    public void actionPerformed(ActionEvent e) {
        /*
         * no need for IOEQ or Thread, as we want to show Dialog and that blocks
         * EDT anyway
         */
        final Account addedAccount;
        if (e.getSource() instanceof PluginForHost) {
            addedAccount = AddAccountDialog.showDialog((PluginForHost) e.getSource(), null);
        } else {
            addedAccount = AddAccountDialog.showDialog(null, null);
        }
        if (addedAccount != null) {
            /* Highlight the new account in the account manager table (selected as soon as it appears there). */
            ConfigurationView.getInstance().getSubPanel(AccountManagerSettings.class).getAccountManager().selectAccount(addedAccount);
        }
    }

    @Override
    public String getTooltipText() {
        return _GUI.T.action_add_premium_account_tooltip();
    }

}
