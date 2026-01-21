package jd.captcha;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.solver.antiCaptchaCom.AntiCaptchaComConfigInterface;
import org.jdownloader.captcha.v2.solver.cheapcaptcha.CheapCaptchaConfigInterface;
import org.jdownloader.captcha.v2.solver.dbc.DeathByCaptchaSettings;
import org.jdownloader.captcha.v2.solver.endcaptcha.EndCaptchaConfigInterface;
import org.jdownloader.captcha.v2.solver.imagetyperz.ImageTyperzConfigInterface;
import org.jdownloader.captcha.v2.solver.solver9kw.Captcha9kwSettings;
import org.jdownloader.captcha.v2.solver.twocaptcha.TwoCaptchaConfigInterface;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfig;

import jd.controlling.AccountController;
import jd.parser.Regex;
import jd.plugins.Account;

/** Class designed to migrate existing captcha solver config to new captcha solver plugin system. */
public class CaptchaSolverSettingsMigration {
    final AccountController ac                                  = AccountController.getInstance();
    private List<String>    namesOfServicesWithMigratedAccounts = new ArrayList<String>();

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
        /**
         * TODO: <br>
         * - Currently "CaptchaSolverPluginConfig" is used as new config everywhere which is still wrong as each solver will have its own
         * config <br>
         * - add migration for all existing solvers <br>
         * - add migration of settings for non account based solvers <br>
         * - test migration
         */
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
        final String host = "anti-captcha.com";
        final AntiCaptchaComConfigInterface cfgOld = JsonConfig.create(AntiCaptchaComConfigInterface.class);
        final CaptchaSolverPluginConfig cfgNew = JsonConfig.create(CaptchaSolverPluginConfig.class);
        /* Migrate account */
        String apikey = cfgOld.getApiKey();
        if (apikey != null) {
            apikey = apikey.trim();
        }
        boolean userHasAccount = false;
        boolean userHasEnabledExistingAccount = false;
        if (apikey != null && apikey.matches("[a-f0-9]{32}")) {
            final Account existingAccount = getExistingAccount(host, apikey);
            if (existingAccount != null) {
                // TODO: Check if an account with same apikey already exists
                System.out.print("Same " + host + " account already exists");
            } else {
                final Account acc = new Account(null, apikey);
                if (!cfgOld.isEnabled()) {
                    /* Disable account if it was disabled by old config. */
                    acc.setEnabled(false, false);
                } else {
                    userHasEnabledExistingAccount = true;
                }
                ac.addAccount(acc);
                namesOfServicesWithMigratedAccounts.add(host);
            }
            userHasAccount = true;
        }
        /* Migrate settings, migrate all that are different from the defaults */
        /* Migrate black-/whitelist settings */
        final List<String> blacklist = this.getValidatedDomainList(cfgOld.getBlacklistEntries());
        final List<String> whitelist = this.getValidatedDomainList(cfgOld.getWhitelistEntries());
        if (blacklist != null && blacklist.size() > 0) {
            /* Only migrate blacklist settings if user has a blacklist with at least 1 valid entry */
            cfgNew.setDomainBlacklist(listToCommaSeparatedDomains(blacklist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                /* Only migrate disabled state if user has blacklist entries && blacklist is disabled. */
                cfgNew.setDomainBlacklistEnabled(false);
            }
        }
        if (whitelist != null && whitelist.size() > 0) {
            cfgNew.setDomainWhitelist(listToCommaSeparatedDomains(whitelist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                /* Only migrate disabled state if user has whitelist entries && whitelist is disabled. */
                cfgNew.setDomainWhitelistEnabled(false);
            }
        }
        if (userHasAccount && userHasEnabledExistingAccount) {
            /* Migrate some settings only if user has an active account */
            if (!cfgOld.isFeedBackSendingEnabled()) {
                cfgNew.setEnableCaptchaFeedback(false);
            }
        }
        System.out.print(host + " migration successful");
    }

    public void migrate_cheap_captcha() {
        final String host = "cheapcaptcha.com";
        final CheapCaptchaConfigInterface cfgOld = JsonConfig.create(CheapCaptchaConfigInterface.class);
        final CaptchaSolverPluginConfig cfgNew = JsonConfig.create(CaptchaSolverPluginConfig.class);
        /* Migrate account */
        String username = cfgOld.getUserName();
        if (username != null) {
            username = username.trim();
        }
        String password = cfgOld.getPassword();
        if (password != null) {
            password = password.trim();
        }
        boolean userHasAccount = false;
        boolean userHasEnabledExistingAccount = false;
        if (!StringUtils.isEmpty(username) && !StringUtils.isEmpty(password)) {
            final Account existingAccountViaUserPW = getExistingAccount(host, username, password);
            if (existingAccountViaUserPW != null) {
                // TODO: Check if an account with same apikey already exists
                System.out.print("Same " + host + " account already exists");
            } else {
                final Account acc = new Account(username, password);
                if (!cfgOld.isEnabled()) {
                    /* Disable account if it was disabled by old config. */
                    acc.setEnabled(false, false);
                } else {
                    userHasEnabledExistingAccount = true;
                }
                ac.addAccount(acc);
                namesOfServicesWithMigratedAccounts.add(host);
            }
            userHasAccount = true;
        }
        /* Migrate settings, migrate all that are different from the defaults */
        /* Migrate black-/whitelist settings */
        final List<String> blacklist = this.getValidatedDomainList(cfgOld.getBlacklistEntries());
        final List<String> whitelist = this.getValidatedDomainList(cfgOld.getWhitelistEntries());
        if (blacklist != null && blacklist.size() > 0) {
            /* Only migrate blacklist settings if user has a blacklist with at least 1 valid entry */
            cfgNew.setDomainBlacklist(listToCommaSeparatedDomains(blacklist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                /* Only migrate disabled state if user has blacklist entries && blacklist is disabled. */
                cfgNew.setDomainBlacklistEnabled(false);
            }
        }
        if (whitelist != null && whitelist.size() > 0) {
            cfgNew.setDomainWhitelist(listToCommaSeparatedDomains(whitelist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                /* Only migrate disabled state if user has whitelist entries && whitelist is disabled. */
                cfgNew.setDomainWhitelistEnabled(false);
            }
        }
        if (userHasAccount && userHasEnabledExistingAccount) {
            /* Migrate some settings only if user has an active account */
            if (!cfgOld.isFeedBackSendingEnabled()) {
                cfgNew.setEnableCaptchaFeedback(false);
            }
        }
        System.out.print(host + " migration successful");
    }

    public void migrate_deathbycaptcha() {
        final String host = "deathbycaptcha.com";
        final DeathByCaptchaSettings cfgOld = JsonConfig.create(DeathByCaptchaSettings.class);
        final CaptchaSolverPluginConfig cfgNew = JsonConfig.create(CaptchaSolverPluginConfig.class);
        /* Migrate account */
        String username = cfgOld.getUserName();
        if (username != null) {
            username = username.trim();
        }
        String passwordOrApitoken = cfgOld.getPassword();
        if (passwordOrApitoken != null) {
            passwordOrApitoken = passwordOrApitoken.trim();
        }
        boolean userHasAccount = false;
        boolean userHasEnabledExistingAccount = false;
        final Pattern apikeypattern = Pattern.compile("^[a-zA-Z0-9]{100,}$");
        final boolean isApikey = passwordOrApitoken != null && new Regex(passwordOrApitoken, apikeypattern).patternFind();
        if ((!StringUtils.isEmpty(username) && !StringUtils.isEmpty(passwordOrApitoken)) || isApikey) {
            final Account existingAccount = getExistingAccount(host, username, passwordOrApitoken);
            Account existingAccountViaApikey = null;
            if (isApikey) {
                existingAccountViaApikey = getExistingAccount(host, passwordOrApitoken);
            }
            if (existingAccount != null) {
                System.out.print("Same " + host + " account already exists via user:pw");
            } else if (existingAccountViaApikey != null) {
                System.out.print("Same " + host + " account already exists via apikey");
            } else {
                final Account acc = new Account(username, passwordOrApitoken);
                if (!cfgOld.isEnabled()) {
                    acc.setEnabled(false, false);
                } else {
                    userHasEnabledExistingAccount = true;
                }
                ac.addAccount(acc);
                namesOfServicesWithMigratedAccounts.add(host);
            }
            userHasAccount = true;
        }
        /* Migrate settings, migrate all that are different from the defaults */
        /* Migrate black-/whitelist settings */
        final List<String> blacklist = this.getValidatedDomainList(cfgOld.getBlacklistEntries());
        final List<String> whitelist = this.getValidatedDomainList(cfgOld.getWhitelistEntries());
        if (blacklist != null && blacklist.size() > 0) {
            cfgNew.setDomainBlacklist(listToCommaSeparatedDomains(blacklist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                cfgNew.setDomainBlacklistEnabled(false);
            }
        }
        if (whitelist != null && whitelist.size() > 0) {
            cfgNew.setDomainWhitelist(listToCommaSeparatedDomains(whitelist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                cfgNew.setDomainWhitelistEnabled(false);
            }
        }
        if (userHasAccount && userHasEnabledExistingAccount) {
            if (!cfgOld.isFeedBackSendingEnabled()) {
                cfgNew.setEnableCaptchaFeedback(false);
            }
        }
        System.out.print(host + " migration successful");
    }

    public void migrate_endcaptcha() {
        final String host = "endcaptcha.com";
        final EndCaptchaConfigInterface cfgOld = JsonConfig.create(EndCaptchaConfigInterface.class);
        final CaptchaSolverPluginConfig cfgNew = JsonConfig.create(CaptchaSolverPluginConfig.class);
        /* Migrate account */
        String username = cfgOld.getUserName();
        if (username != null) {
            username = username.trim();
        }
        String password = cfgOld.getPassword();
        if (password != null) {
            password = password.trim();
        }
        boolean userHasAccount = false;
        boolean userHasEnabledExistingAccount = false;
        if (!StringUtils.isEmpty(username) && !StringUtils.isEmpty(password)) {
            final Account existingAccount = getExistingAccount(host, username, password);
            if (existingAccount != null) {
                // TODO: Check if an account with same apikey already exists
                System.out.print("Same " + host + " account already exists via user:pw");
            } else {
                final Account acc = new Account(username, password);
                if (!cfgOld.isEnabled()) {
                    /* Disable account if it was disabled by old config. */
                    acc.setEnabled(false, false);
                } else {
                    userHasEnabledExistingAccount = true;
                }
                ac.addAccount(acc);
                namesOfServicesWithMigratedAccounts.add(host);
            }
            userHasAccount = true;
        }
        /* Migrate settings, migrate all that are different from the defaults */
        /* Migrate black-/whitelist settings */
        final List<String> blacklist = this.getValidatedDomainList(cfgOld.getBlacklistEntries());
        final List<String> whitelist = this.getValidatedDomainList(cfgOld.getWhitelistEntries());
        if (blacklist != null && blacklist.size() > 0) {
            /* Only migrate blacklist settings if user has a blacklist with at least 1 valid entry */
            cfgNew.setDomainBlacklist(listToCommaSeparatedDomains(blacklist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                /* Only migrate disabled state if user has blacklist entries && blacklist is disabled. */
                cfgNew.setDomainBlacklistEnabled(false);
            }
        }
        if (whitelist != null && whitelist.size() > 0) {
            cfgNew.setDomainWhitelist(listToCommaSeparatedDomains(whitelist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                /* Only migrate disabled state if user has whitelist entries && whitelist is disabled. */
                cfgNew.setDomainWhitelistEnabled(false);
            }
        }
        if (userHasAccount && userHasEnabledExistingAccount) {
            /* Migrate some settings only if user has an active account */
            if (!cfgOld.isFeedBackSendingEnabled()) {
                cfgNew.setEnableCaptchaFeedback(false);
            }
        }
        System.out.print(host + " migration successful");
    }

    public void migrate_imagetyperz() {
        final String host = "imagetyperz.com";
        final ImageTyperzConfigInterface cfgOld = JsonConfig.create(ImageTyperzConfigInterface.class);
        final CaptchaSolverPluginConfig cfgNew = JsonConfig.create(CaptchaSolverPluginConfig.class);
        /* Migrate account */
        String username = cfgOld.getUserName();
        if (username != null) {
            username = username.trim();
        }
        String password = cfgOld.getPassword();
        if (password != null) {
            password = password.trim();
        }
        boolean userHasAccount = false;
        boolean userHasEnabledExistingAccount = false;
        if (!StringUtils.isEmpty(username) && !StringUtils.isEmpty(password)) {
            final Account existingAccount = getExistingAccount(host, username, password);
            if (existingAccount != null) {
                // TODO: Check if an account with same apikey already exists
                System.out.print("Same " + host + " account already exists via user:pw");
            } else {
                final Account acc = new Account(username, password);
                if (!cfgOld.isEnabled()) {
                    /* Disable account if it was disabled by old config. */
                    acc.setEnabled(false, false);
                } else {
                    userHasEnabledExistingAccount = true;
                }
                ac.addAccount(acc);
                namesOfServicesWithMigratedAccounts.add(host);
            }
            userHasAccount = true;
        }
        /* Migrate settings, migrate all that are different from the defaults */
        /* Migrate black-/whitelist settings */
        final List<String> blacklist = this.getValidatedDomainList(cfgOld.getBlacklistEntries());
        final List<String> whitelist = this.getValidatedDomainList(cfgOld.getWhitelistEntries());
        if (blacklist != null && blacklist.size() > 0) {
            /* Only migrate blacklist settings if user has a blacklist with at least 1 valid entry */
            cfgNew.setDomainBlacklist(listToCommaSeparatedDomains(blacklist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                /* Only migrate disabled state if user has blacklist entries && blacklist is disabled. */
                cfgNew.setDomainBlacklistEnabled(false);
            }
        }
        if (whitelist != null && whitelist.size() > 0) {
            cfgNew.setDomainWhitelist(listToCommaSeparatedDomains(whitelist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                /* Only migrate disabled state if user has whitelist entries && whitelist is disabled. */
                cfgNew.setDomainWhitelistEnabled(false);
            }
        }
        if (userHasAccount && userHasEnabledExistingAccount) {
            /* Migrate some settings only if user has an active account */
            if (!cfgOld.isFeedBackSendingEnabled()) {
                cfgNew.setEnableCaptchaFeedback(false);
            }
        }
        System.out.print(host + " migration successful");
    }

    public void migrate_2captcha() {
        final String host = "2captcha.com";
        final TwoCaptchaConfigInterface cfgOld = JsonConfig.create(TwoCaptchaConfigInterface.class);
        final CaptchaSolverPluginConfig cfgNew = JsonConfig.create(CaptchaSolverPluginConfig.class);
        /* Migrate account */
        String apikey = cfgOld.getApiKey();
        if (apikey != null) {
            apikey = apikey.trim();
        }
        boolean userHasAccount = false;
        boolean userHasEnabledExistingAccount = false;
        if (apikey != null && apikey.matches("[a-f0-9]{32}")) {
            final Account existingAccount = getExistingAccount(host, apikey);
            if (existingAccount != null) {
                // TODO: Check if an account with same apikey already exists
                System.out.print("Same " + host + " account already exists via apikey");
            } else {
                final Account acc = new Account(apikey, apikey);
                if (!cfgOld.isEnabled()) {
                    /* Disable account if it was disabled by old config. */
                    acc.setEnabled(false, false);
                } else {
                    userHasEnabledExistingAccount = true;
                }
                ac.addAccount(acc);
                namesOfServicesWithMigratedAccounts.add(host);
            }
            userHasAccount = true;
        }
        /* Migrate settings, migrate all that are different from the defaults */
        /* Migrate black-/whitelist settings */
        final List<String> blacklist = this.getValidatedDomainList(cfgOld.getBlacklistEntries());
        final List<String> whitelist = this.getValidatedDomainList(cfgOld.getWhitelistEntries());
        if (blacklist != null && blacklist.size() > 0) {
            /* Only migrate blacklist settings if user has a blacklist with at least 1 valid entry */
            cfgNew.setDomainBlacklist(listToCommaSeparatedDomains(blacklist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                /* Only migrate disabled state if user has blacklist entries && blacklist is disabled. */
                cfgNew.setDomainBlacklistEnabled(false);
            }
        }
        if (whitelist != null && whitelist.size() > 0) {
            cfgNew.setDomainWhitelist(listToCommaSeparatedDomains(whitelist));
            if (!cfgOld.isBlackWhiteListingEnabled()) {
                /* Only migrate disabled state if user has whitelist entries && whitelist is disabled. */
                cfgNew.setDomainWhitelistEnabled(false);
            }
        }
        if (userHasAccount && userHasEnabledExistingAccount) {
            /* Migrate some settings only if user has an active account */
            if (!cfgOld.isFeedBackSendingEnabled()) {
                cfgNew.setEnableCaptchaFeedback(false);
            }
        }
        System.out.print(host + " migration successful");
    }

    public void migrate_9kw() {
        final String host = "9kw.eu";
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
            final Account existingAccount = getExistingAccount(host, apikey);
            if (existingAccount != null) {
                // TODO: Check if an account with same apikey already exists
                System.out.print("Same " + host + " account already exists");
            } else {
                final Account acc = new Account(null, apikey);
                if (!cfgOld.isEnabledGlobally() && !cfgOld.isEnabled()) {
                    /* Disable account if it was disabled by old config. */
                    acc.setEnabled(false, false);
                } else {
                    userHasEnabledExistingAccount = true;
                }
                ac.addAccount(acc);
                namesOfServicesWithMigratedAccounts.add(host);
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
        System.out.print(host + " migration successful");
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
        if (input == null || input.trim().length() == 0) {
            return null;
        }
        final List<String> ret = new ArrayList<String>();
        final String[] parts = input.split(",");
        for (int i = 0; i < parts.length; i++) {
            final String domain = parts[i];
            ret.add(domain);
        }
        return getValidatedDomainList(ret);
    }

    /**
     * Removes invalid elements from given domain list. <br>
     * Returns either null or an arraylist with at least one valid element.
     */
    private List<String> getValidatedDomainList(final List<String> domains) {
        if (domains == null || domains.isEmpty()) {
            return null;
        }
        final List<String> ret = new ArrayList<String>();
        for (String domain : domains) {
            domain = domain.toLowerCase(Locale.ROOT).trim();
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