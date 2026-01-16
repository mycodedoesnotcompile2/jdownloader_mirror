package jd.captcha;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.solver.solver9kw.Captcha9kwSettings;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfig;

import jd.controlling.AccountController;
import jd.plugins.Account;

/** Class designed to migrate existing captcha solver config to new captcha solver plugin system. */
public class CaptchaSolverSettingsMigration {
    final AccountController ac = AccountController.getInstance();

    public ArrayList<Account> getExistingAccounts(final String domain) {
        return ac.list(domain);
    }

    /** Returns account for given domain with the apikey as username or apikey as password (fallback). */
    public Account getExistingAccount(final String domain, final String apikey) {
        if (domain == null || StringUtils.isEmpty(apikey)) {
            throw new IllegalArgumentException();
        }
        final List<Account> accs = this.getExistingAccounts(domain);
        if (accs == null || accs.isEmpty()) {
            return null;
        }
        Account accByPassword = null;
        for (final Account account : accs) {
            if (apikey.equals(account.getUser())) {
                return account;
            }
            /* A lot of apikey accounts will have no username but only password which is the api key */
            if (apikey.equals(account.getPass())) {
                accByPassword = account;
            }
        }
        if (accByPassword != null) {
            return accByPassword;
        }
        return null;
    }

    /** Returns account for given domain with exactly the username and password. */
    public Account getExistingAccount(final String domain, final String username, final String password) {
        if (domain == null || StringUtils.isEmpty(username) || StringUtils.isEmpty(password)) {
            throw new IllegalArgumentException();
        }
        final List<Account> accs = this.getExistingAccounts(domain);
        if (accs == null || accs.isEmpty()) {
            return null;
        }
        for (final Account account : accs) {
            if (username.equals(account.getUser()) && password.equals(account.getPass())) {
                return account;
            }
        }
        return null;
    }

    public void migrate() {
        migrate_anti_captcha_com();
        migrate_cheap_captcha();
        migrate_deathbycaptcha();
        migrate_endcaptcha();
        migrate_imagetyperz();
        migrate_2captcha();
        migrate_9kw();
        // TODO: Save a property somewhere aka "migration_conmpleted_timestamp" so we can early abort this next time
    }

    public void migrate_anti_captcha_com() {
        // TODO
    }

    public void migrate_cheap_captcha() {
        // TODO
    }

    public void migrate_deathbycaptcha() {
        // TODO
    }

    public void migrate_endcaptcha() {
        // TODO
    }

    public void migrate_imagetyperz() {
        // TODO
    }

    public void migrate_2captcha() {
        // TODO
    }

    public void migrate_9kw() {
        final Captcha9kwSettings cfgOld = JsonConfig.create(Captcha9kwSettings.class);
        final CaptchaSolverPluginConfig cfgNew = JsonConfig.create(CaptchaSolverPluginConfig.class);
        /* Migrate account */
        String apikey = cfgOld.getApiKey();
        if (apikey != null) {
            apikey = apikey.trim();
        }
        // final PluginForCaptchaSolverNineKw plg = new PluginForCaptchaSolverNineKw(null);
        boolean userHasAccount = false;
        boolean userHasEnabledExistingAccount = false;
        if (apikey != null && apikey.matches("[a-zA-Z0-9]{10,}")) {
            final List<Account> accs = this.getExistingAccounts("9kw.eu");
            final Account existingAccount = getExistingAccount("9kw.eu", apikey);
            if (accs != null) {
                // TODO: Check if an account with same apikey already exists
            } else {
                final Account acc = new Account(null, apikey);
                if (!cfgOld.isEnabledGlobally() && !cfgOld.isEnabled()) {
                    /* Disable account if it was disabled by old config. */
                    acc.setEnabled(false, false);
                } else {
                    userHasEnabledExistingAccount = true;
                }
                ac.addAccount(acc);
            }
            userHasAccount = true;
        }
        /* Migrate settings, migrate all that are different from the defaults */
        /* Migrate black-/whitelist settings */
        final String blacklistStr = cfgOld.getblacklist();
        final String whitelistStr = cfgOld.getwhitelist();
        final List<String> blacklist = commaSeparatedDomainsToList(blacklistStr);
        final List<String> whitelist = commaSeparatedDomainsToList(whitelistStr);
        if (blacklist != null && blacklist.size() > 0) {
            /* Only migrate blacklist settings if user has a blacklist with at least 1 valid entry */
            cfgNew.setDomainBlacklist(listToCommaSeparatedDomains(blacklist));
            if (!cfgOld.getblacklistcheck()) {
                /* Only migrate disabled state if user has blacklist entries && blacklist is disabled. */
                cfgNew.setDomainBlacklistEnabled(false);
            }
        }
        if (whitelist != null && whitelist.size() > 0) {
            cfgNew.setDomainWhitelist(listToCommaSeparatedDomains(whitelist));
            if (!cfgOld.getwhitelistcheck()) {
                /* Only migrate disabled state if user has whitelist entries && whitelist is disabled. */
                cfgNew.setDomainWhitelistEnabled(false);
            }
        }
        if (userHasAccount && userHasEnabledExistingAccount) {
            /* Migrate some settings only if user has an active account */
            if (!cfgOld.isfeedback()) {
                cfgNew.setEnableCaptchaFeedback(false);
            }
        }
        System.out.print("9kw migration successful");
    }

    /**
     * Helper functions down below. These might be copies from other places but we don't care since this class shall be thrown away anyways.
     */
    private boolean looksLikeValidDomain(final String domain) {
        if (domain == null || domain.length() == 0) {
            return false;
        }
        if (!domain.contains(".")) {
            return false;
        }
        // Regex pattern for domain validation
        // Allows: letters, numbers, hyphens; must not start or end with hyphen
        final String domainPattern = "^([a-z0-9]([a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9]([a-z0-9-]*[a-z0-9])?$";
        return domain.matches(domainPattern);
    }

    private List<String> commaSeparatedDomainsToList(final String input) {
        final List<String> ret = new ArrayList<String>();
        if (input == null || input.trim().length() == 0) {
            return null;
        }
        final String[] parts = input.split(",");
        for (int i = 0; i < parts.length; i++) {
            final String domain = parts[i].trim().toLowerCase(Locale.ROOT);
            if (looksLikeValidDomain(domain) && !ret.contains(domain)) {
                ret.add(domain);
            }
        }
        if (ret.size() > 0) {
            return ret;
        }
        return null;
    }

    private String listToCommaSeparatedDomains(final List<String> domains) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < domains.size(); i++) {
            if (i > 0) {
                sb.append(",");
            }
            sb.append(domains.get(i));
        }
        return sb.toString();
    }
}