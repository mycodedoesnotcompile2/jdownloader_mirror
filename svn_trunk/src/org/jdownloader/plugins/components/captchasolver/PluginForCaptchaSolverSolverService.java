package org.jdownloader.plugins.components.captchasolver;

import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
import java.util.Currency;
import java.util.List;

import javax.swing.Icon;

import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;

import jd.controlling.AccountController;
import jd.gui.swing.dialog.AddAccountDialog;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.components.premiumbar.ServiceCollection;
import jd.gui.swing.jdgui.components.premiumbar.ServicePanelExtender;
import jd.gui.swing.jdgui.views.settings.ConfigurationView;
import jd.gui.swing.jdgui.views.settings.panels.accountmanager.AccountManagerSettings;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class PluginForCaptchaSolverSolverService extends AbstractSolverService implements ServicePanelExtender {
    protected final abstractPluginForCaptchaSolver plugin;

    public PluginForCaptchaSolverSolverService(final abstractPluginForCaptchaSolver plugin) {
        if (plugin == null) {
            throw new IllegalArgumentException();
        }
        this.plugin = plugin;
    }

    /** Returns the captcha types supported by the underlying plugin, used e.g. for the "Supported by this service" overview. */
    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        return this.plugin.getSupportedCaptchaTypes();
    }

    @Override
    public SolverType getType() {
        return SolverType.EXTERNAL;
    }

    @Override
    public String getDescription() {
        return _GUI.T.CaptchaSolverService_description();
    }

    @Override
    public Double getBalance() {
        final List<Account> validAccounts = AccountController.getInstance().getValidAccounts(plugin.getHost());
        if (validAccounts == null || validAccounts.isEmpty()) {
            return null;
        }
        double balance = 0;
        boolean found = false;
        for (final Account account : validAccounts) {
            final AccountInfo ai = account.getAccountInfo();
            if (ai == null) {
                continue;
            }
            balance += ai.getAccountBalance();
            found = true;
        }
        return found ? Double.valueOf(balance) : null;
    }

    @Override
    public Currency getBalanceCurrency() {
        final List<Account> validAccounts = AccountController.getInstance().getValidAccounts(plugin.getHost());
        if (validAccounts == null || validAccounts.isEmpty()) {
            return null;
        }
        /* In practice all accounts of one solver share the same currency, so the first one that has one is used for the total. */
        for (final Account account : validAccounts) {
            final AccountInfo ai = account.getAccountInfo();
            if (ai != null && ai.getCurrency() != null) {
                return ai.getCurrency();
            }
        }
        return null;
    }

    /**
     * The states the Status column can be in for this solver, in priority order: a globally disabled usage-of-solver-accounts setting
     * takes priority over everything else (it blocks the solver regardless of account state), followed by having no account at all
     * (routine "Add Account" case), followed by having accounts that all exist but are unusable (disabled or invalid; needs the user's
     * attention, unlike the routine "no account yet" case), and finally the normal ready state.
     */
    private enum Status {
        ACCOUNTS_GLOBALLY_DISABLED,
        NO_ACCOUNT,
        ACCOUNTS_UNUSABLE,
        READY
    }

    private Status getResolvedStatus() {
        if (!CFG_GENERAL.CFG.isUseAvailableCaptchaSolverAccounts()) {
            return Status.ACCOUNTS_GLOBALLY_DISABLED;
        }
        final List<Account> allAccounts = AccountController.getInstance().list(plugin.getHost());
        if (allAccounts == null || allAccounts.isEmpty()) {
            return Status.NO_ACCOUNT;
        }
        final List<Account> validAccounts = AccountController.getInstance().getValidAccounts(plugin.getHost());
        if (validAccounts == null || validAccounts.isEmpty()) {
            /* Accounts exist, but every single one is currently disabled or invalid (error state). */
            return Status.ACCOUNTS_UNUSABLE;
        }
        return Status.READY;
    }

    @Override
    public String getStatusText() {
        if (getResolvedStatus() != Status.READY) {
            /* Not ready -> an action button (see getStatusActionName()) is shown instead. */
            return null;
        }
        final List<Account> validAccounts = AccountController.getInstance().getValidAccounts(plugin.getHost());
        final Double balance = getBalance();
        if (balance == null) {
            return _GUI.T.CaptchaSolverService_status_ready();
        }
        final Currency currency = getBalanceCurrency();
        /* Same balance formatting regardless of account count, consistent with the Balance column and single-account solvers. */
        final String balanceText = AccountInfo.formatCaptchaSolverBalance(balance.doubleValue(), currency);
        if (validAccounts.size() > 1) {
            return _GUI.T.CaptchaSolverService_status_ready_accounts(Integer.toString(validAccounts.size()), balanceText);
        }
        /* Single account: keep the established "Ready | Balance: <localized amount>" layout. */
        return _GUI.T.CaptchaSolverService_status_ready_balance(balanceText);
    }

    @Override
    public String getStatusActionName() {
        switch (getResolvedStatus()) {
        case ACCOUNTS_GLOBALLY_DISABLED:
            return _GUI.T.SolverOrderTableModel_status_enableSolverAccounts();
        case NO_ACCOUNT:
            return _GUI.T.lit_add_account();
        case ACCOUNTS_UNUSABLE:
            return _GUI.T.SolverOrderTableModel_status_accountsUnusable();
        case READY:
        default:
            return null;
        }
    }

    @Override
    public boolean isStatusActionWarning() {
        switch (getResolvedStatus()) {
        case ACCOUNTS_GLOBALLY_DISABLED:
        case ACCOUNTS_UNUSABLE:
            return true;
        case NO_ACCOUNT:
        case READY:
        default:
            /* "Add Account" is a routine action, not a warning. */
            return false;
        }
    }

    @Override
    public void onStatusAction() {
        switch (getResolvedStatus()) {
        case ACCOUNTS_GLOBALLY_DISABLED:
            try {
                CFG_GENERAL.USE_AVAILABLE_CAPTCHA_SOLVER_ACCOUNTS.setValue(true);
            } catch (final ValidationException e) {
                /* Should never happen for a plain boolean key. */
                e.printStackTrace();
            }
            return;
        case ACCOUNTS_UNUSABLE:
            openAccountManager(getFirstAccountOrNull());
            return;
        case NO_ACCOUNT:
        case READY:
        default:
            AddAccountDialog.showDialog(plugin, null);
            return;
        }
    }

    private Account getFirstAccountOrNull() {
        final List<Account> allAccounts = AccountController.getInstance().list(plugin.getHost());
        if (allAccounts == null || allAccounts.isEmpty()) {
            return null;
        }
        return allAccounts.get(0);
    }

    /**
     * Opens the Account Manager settings tab (its "Accounts" sub-tab) and, if given, selects/highlights the given account in the account
     * list. Mirrors {@code jd.plugins.PluginConfigPanelNG#switchToAccountManager}.
     */
    private static void openAccountManager(final Account accountToSelect) {
        JsonConfig.create(GraphicalUserInterfaceSettings.class).setConfigViewVisible(true);
        JDGui.getInstance().setContent(ConfigurationView.getInstance(), true);
        ConfigurationView.getInstance().setSelectedSubPanel(AccountManagerSettings.class);
        final AccountManagerSettings accountManagerSettings = ConfigurationView.getInstance().getSubPanel(AccountManagerSettings.class);
        if (accountManagerSettings != null) {
            accountManagerSettings.getAccountManager().setTab(0);
            if (accountToSelect != null) {
                accountManagerSettings.getAccountManager().selectAccount(accountToSelect);
            }
        }
    }

    @Override
    public String getName() {
        return this.plugin.getHost();
    }

    @Override
    public String getID() {
        return this.plugin.getHost();
    }

    @Override
    public Icon getIcon(int size) {
        return DomainInfo.getInstance(plugin.getHost());
    }

    /** Returns the underlying plugin's own captcha solver config (per-plugin storage). */
    @Override
    public CaptchaSolverConfigV3 getConfigV3() {
        return PluginJsonConfig.get(plugin.getLazyP(), plugin.getConfigInterface());
    }

    /**
     * Returns this service's server-side max-simultaneous-captchas limit (see
     * {@link abstractPluginForCaptchaSolver#getServerSideMaxSimultaneousCaptchaThreadsLimit(Account)}), for display purposes (e.g. the
     * captcha types table's info line). None of the current overrides actually depend on the account they are given, so null is passed
     * here.
     */
    public int getServerSideMaxSimultaneousCaptchaThreadsLimit() {
        return plugin.getServerSideMaxSimultaneousCaptchaThreadsLimit(null);
    }

    /**
     * Returns this service's server-side max polling time in milliseconds (see
     * {@link abstractPluginForCaptchaSolver#getServerSideMaxPollingTimeoutMillis()}), for display purposes.
     */
    public long getServerSideMaxPollingTimeoutMillis() {
        return plugin.getServerSideMaxPollingTimeoutMillis();
    }

    @Override
    public String getBuyURL() {
        return plugin.getBuyPremiumUrl();
    }

    @Override
    public String getHelpArticleURL() {
        return "https://support.jdownloader.org/knowledgebase/article/error-skipped-captcha-is-required";
    }

    @Override
    public void extendServicePabel(List<ServiceCollection<?>> services) {
    }
}
