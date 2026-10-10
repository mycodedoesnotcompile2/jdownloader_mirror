package org.jdownloader.plugins.components.captchasolver;

import java.util.Currency;
import java.util.List;

import javax.swing.Icon;

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

import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfig;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.host.LazyHostPluginFilter;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;

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
     * The states the Status column can be in for this solver, in priority order: a globally disabled usage-of-solver-accounts setting takes
     * priority over everything else (it blocks the solver regardless of account state), followed by having no account at all (routine "Add
     * Account" case), followed by having accounts that all exist but are unusable (disabled or invalid; needs the user's attention, unlike
     * the routine "no account yet" case), and finally the normal ready state.
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
        final String statusText = _GUI.T.CaptchaSolverService_status_ready_balance(balanceText);
        if (isStatusTextWarning()) {
            return statusText + validAccounts.get(0).getLowBalanceStatusSuffix();
        }
        return statusText;
    }

    /**
     * True if the solver has exactly one usable account and its credits are below the user's warning threshold (see
     * {@link Account#isLowBalance()}). With several accounts the status text only shows the total balance, which says
     * nothing about the single accounts, so no warning is shown then.
     */
    @Override
    public boolean isStatusTextWarning() {
        final List<Account> validAccounts = AccountController.getInstance().getValidAccounts(plugin.getHost());
        return validAccounts != null && validAccounts.size() == 1 && validAccounts.get(0).isLowBalance();
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
            /* Captcha solver table: restrict the hoster chooser to captcha solver plugins (no other hosters). */
            AddAccountDialog.showDialog(plugin, null, LazyHostPluginFilter.ALL_CAPTCHA_SOLVERS);
            return;
        }
    }

    /**
     * Opens the Account Manager and, if this solver already has at least one account, selects/highlights its first one. Used by the solver
     * table's context menu action for external solvers; available regardless of the solver's ready state.
     */
    public void openAccountManagerSelectingFirstMatchingAccount() {
        openAccountManager(getFirstAccountOrNull());
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
    public static void openAccountManager(final Account accountToSelect) {
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
        return getPluginConfig();
    }

    /** Same as {@link #getConfigV3()}, but typed as the plugin config, which also carries the settings only external solvers have. */
    public CaptchaSolverPluginConfig getPluginConfig() {
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

    /**
     * Returns this service's server-side minimum polling interval in milliseconds (see
     * {@link abstractPluginForCaptchaSolver#getServerSideMinPollingIntervalMillis()}), 0 = none, for display purposes.
     */
    public long getServerSideMinPollingIntervalMillis() {
        return plugin.getServerSideMinPollingIntervalMillis();
    }

    @Override
    public String getBuyURL() {
        return plugin.getBuyPremiumUrl();
    }

    /**
     * Opens the "buy premium" page of this solver in the browser. Uses the same affiliate/redirect link logic as the account manager's
     * buy/renew actions ({@link AccountController#openAfflink}).
     *
     * @param source
     *            Identifies where the link was opened from (part of the redirect link)
     */
    public void openBuyPage(final String source) {
        AccountController.openAfflink(plugin.getLazyP(), plugin, source);
    }

    @Override
    public String getHelpArticleURL() {
        return "https://support.jdownloader.org/knowledgebase/article/error-skipped-captcha-is-required";
    }

    @Override
    public void extendServicePabel(List<ServiceCollection<?>> services) {
    }
}
