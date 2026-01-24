package org.jdownloader.captcha.v2;

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
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigAntiCaptchaCom;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigCheapcaptchaCom;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigDeathbycaptcha;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigEndcaptcha;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigImagetyperz;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigNinekw;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigTwoCaptcha;

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
        final CaptchaSolverPluginConfigAntiCaptchaCom cfgNew = JsonConfig.create(CaptchaSolverPluginConfigAntiCaptchaCom.class);
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
                System.out.print("Same " + host + " account already exists");
            } else {
                final Account acc = new Account(null, apikey);
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
        final List<String> blacklist = cfgOld.getBlacklistEntries();
        final List<String> whitelist = cfgOld.getWhitelistEntries();
        final boolean blackWhiteListingEnabled = cfgOld.isBlackWhiteListingEnabled();
        final List<CaptchaChallengeFilter> filters = migrateBlackWhiteListToFilters(blacklist, blackWhiteListingEnabled, whitelist, blackWhiteListingEnabled);
        if (filters != null) {
            cfgNew.setFilterList(filters);
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
        final CaptchaSolverPluginConfigCheapcaptchaCom cfgNew = JsonConfig.create(CaptchaSolverPluginConfigCheapcaptchaCom.class);
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
                System.out.print("Same " + host + " account already exists");
            } else {
                final Account acc = new Account(username, password);
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
        final List<String> blacklist = cfgOld.getBlacklistEntries();
        final List<String> whitelist = cfgOld.getWhitelistEntries();
        final boolean blackWhiteListingEnabled = cfgOld.isBlackWhiteListingEnabled();
        final List<CaptchaChallengeFilter> filters = migrateBlackWhiteListToFilters(blacklist, blackWhiteListingEnabled, whitelist, blackWhiteListingEnabled);
        if (filters != null) {
            cfgNew.setFilterList(filters);
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
        final CaptchaSolverPluginConfigDeathbycaptcha cfgNew = JsonConfig.create(CaptchaSolverPluginConfigDeathbycaptcha.class);
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
        final List<String> blacklist = cfgOld.getBlacklistEntries();
        final List<String> whitelist = cfgOld.getWhitelistEntries();
        final boolean blackWhiteListingEnabled = cfgOld.isBlackWhiteListingEnabled();
        final List<CaptchaChallengeFilter> filters = migrateBlackWhiteListToFilters(blacklist, blackWhiteListingEnabled, whitelist, blackWhiteListingEnabled);
        if (filters != null) {
            cfgNew.setFilterList(filters);
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
        final CaptchaSolverPluginConfigEndcaptcha cfgNew = JsonConfig.create(CaptchaSolverPluginConfigEndcaptcha.class);
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
                System.out.print("Same " + host + " account already exists via user:pw");
            } else {
                final Account acc = new Account(username, password);
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
        final List<String> blacklist = cfgOld.getBlacklistEntries();
        final List<String> whitelist = cfgOld.getWhitelistEntries();
        final boolean blackWhiteListingEnabled = cfgOld.isBlackWhiteListingEnabled();
        final List<CaptchaChallengeFilter> filters = migrateBlackWhiteListToFilters(blacklist, blackWhiteListingEnabled, whitelist, blackWhiteListingEnabled);
        if (filters != null) {
            cfgNew.setFilterList(filters);
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
        final CaptchaSolverPluginConfigImagetyperz cfgNew = JsonConfig.create(CaptchaSolverPluginConfigImagetyperz.class);
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
                System.out.print("Same " + host + " account already exists via user:pw");
            } else {
                final Account acc = new Account(username, password);
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
        final List<String> blacklist = cfgOld.getBlacklistEntries();
        final List<String> whitelist = cfgOld.getWhitelistEntries();
        final boolean blackWhiteListingEnabled = cfgOld.isBlackWhiteListingEnabled();
        final List<CaptchaChallengeFilter> filters = migrateBlackWhiteListToFilters(blacklist, blackWhiteListingEnabled, whitelist, blackWhiteListingEnabled);
        if (filters != null) {
            cfgNew.setFilterList(filters);
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
        final CaptchaSolverPluginConfigTwoCaptcha cfgNew = JsonConfig.create(CaptchaSolverPluginConfigTwoCaptcha.class);
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
                System.out.print("Same " + host + " account already exists via apikey");
            } else {
                final Account acc = new Account(apikey, apikey);
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
        final List<String> blacklist = cfgOld.getBlacklistEntries();
        final List<String> whitelist = cfgOld.getWhitelistEntries();
        final boolean blackWhiteListingEnabled = cfgOld.isBlackWhiteListingEnabled();
        final List<CaptchaChallengeFilter> filters = migrateBlackWhiteListToFilters(blacklist, blackWhiteListingEnabled, whitelist, blackWhiteListingEnabled);
        if (filters != null) {
            cfgNew.setFilterList(filters);
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
        final CaptchaSolverPluginConfigNinekw cfgNew = JsonConfig.create(CaptchaSolverPluginConfigNinekw.class);
        /* Migrate account */
        String apikey = cfgOld.getApiKey();
        if (apikey != null) {
            apikey = apikey.trim();
        }
        boolean userHasAccount = false;
        boolean userHasEnabledExistingAccount = false;
        if (apikey != null && apikey.matches("[a-zA-Z0-9]{10,}")) {
            final Account existingAccount = getExistingAccount(host, apikey);
            if (existingAccount != null) {
                System.out.print("Same " + host + " account already exists");
            } else {
                final Account acc = new Account(null, apikey);
                if (!cfgOld.isEnabledGlobally() && !cfgOld.isEnabled()) {
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
        /* Migrate black-/whitelist settings - 9kw has TWO sources for each list! */
        /* Merge comma-separated string lists with array lists */
        List<String> blacklist = commaSeparatedStringsToList(cfgOld.getblacklist());
        final List<String> blacklist2 = cfgOld.getBlacklistEntries();
        if (blacklist == null) {
            blacklist = blacklist2;
        } else if (blacklist2 != null) {
            /* Merge both blacklists, avoid duplicates */
            for (int i = 0; i < blacklist2.size(); i++) {
                final String entry = blacklist2.get(i);
                if (entry != null && !blacklist.contains(entry)) {
                    blacklist.add(entry);
                }
            }
        }
        List<String> whitelist = commaSeparatedStringsToList(cfgOld.getwhitelist());
        final List<String> whitelist2 = cfgOld.getWhitelistEntries();
        if (whitelist == null) {
            whitelist = whitelist2;
        } else if (whitelist2 != null) {
            /* Merge both whitelists, avoid duplicates */
            for (int i = 0; i < whitelist2.size(); i++) {
                final String entry = whitelist2.get(i);
                if (entry != null && !whitelist.contains(entry)) {
                    whitelist.add(entry);
                }
            }
        }
        final boolean blacklistEnabled = cfgOld.getblacklistcheck();
        final boolean whitelistEnabled = cfgOld.getwhitelistcheck();
        final List<CaptchaChallengeFilter> filters = migrateBlackWhiteListToFilters(blacklist, blacklistEnabled, whitelist, whitelistEnabled);
        if (filters != null) {
            cfgNew.setFilterList(filters);
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
    /**
     * Converts comma-separated string to list of trimmed non-empty strings.
     *
     * @param input
     *            Comma-separated string
     * @return List of strings or null if input is empty
     */
    private List<String> commaSeparatedStringsToList(final String input) {
        if (input == null || input.trim().length() == 0) {
            return null;
        }
        final List<String> ret = new ArrayList<String>();
        final String[] parts = input.split(",");
        for (int i = 0; i < parts.length; i++) {
            final String entry = parts[i].trim();
            if (entry.length() > 0 && !ret.contains(entry)) {
                ret.add(entry);
            }
        }
        if (ret.size() > 0) {
            return ret;
        }
        return null;
    }

    /**
     * Converts a list of domain/regex strings to CaptchaChallengeFilter objects.
     *
     * @param domainsOrRegexes
     *            List of strings (can be plain domains or regex patterns)
     * @param filterType
     *            BLACKLIST or WHITELIST
     * @return List of valid CaptchaChallengeFilter objects, or null if no valid filters could be created
     */
    public List<CaptchaChallengeFilter> domainsToFilters(final List<String> domainsOrRegexes, final CaptchaChallengeFilter.CaptchaFilterType filterType) {
        if (domainsOrRegexes == null || domainsOrRegexes.isEmpty() || filterType == null) {
            return null;
        }
        final List<CaptchaChallengeFilter> filters = new ArrayList<CaptchaChallengeFilter>();
        int position = 0;
        for (String domainOrRegex : domainsOrRegexes) {
            if (domainOrRegex == null) {
                continue;
            }
            domainOrRegex = domainOrRegex.trim();
            if (domainOrRegex.length() == 0) {
                /* Skip empty strings */
                continue;
            }
            final CaptchaChallengeFilter filter = domainOrRegexToFilter(domainOrRegex, filterType);
            if (filter != null) {
                filter.setPosition(position);
                filters.add(filter);
                position++;
            }
        }
        if (filters.size() > 0) {
            return filters;
        }
        return null;
    }

    /**
     * Converts a single domain/regex string to a CaptchaChallengeFilter. Automatically detects if the input is a regex pattern or plain
     * domain.
     *
     * @param domainOrRegex
     *            String (can be plain domain or regex pattern)
     * @param filterType
     *            BLACKLIST or WHITELIST
     * @return CaptchaChallengeFilter or null if input is invalid
     */
    private CaptchaChallengeFilter domainOrRegexToFilter(final String domainOrRegex, final CaptchaChallengeFilter.CaptchaFilterType filterType) {
        if (domainOrRegex == null || domainOrRegex.trim().length() == 0 || filterType == null) {
            return null;
        }
        final boolean isRegex = isRegexPattern(domainOrRegex);
        if (!isRegex && !looksLikeValidDomain(domainOrRegex)) {
            /* Neither valid domain nor valid regex pattern */
            return null;
        }
        final CaptchaChallengeFilter filter = new CaptchaChallengeFilter();
        if (isRegex) {
            /* Keep regex patterns as-is (case-sensitive) */
            filter.setDomain(domainOrRegex);
        } else {
            /* Normalize plain domains to lowercase */
            filter.setDomain(domainOrRegex.toLowerCase(Locale.ROOT));
        }
        filter.setRegex(isRegex);
        filter.setFilterType(filterType);
        filter.setEnabled(true);
        return filter;
    }

    /**
     * Checks if a string looks like a valid domain name.
     */
    private boolean looksLikeValidDomain(final String str) {
        if (str == null || str.length() == 0) {
            return false;
        }
        if (!str.contains(".")) {
            return false;
        }
        /* Regex pattern for domain validation */
        /* Allows: letters, numbers, hyphens; must not start or end with hyphen */
        final String domainPattern = "^([a-z0-9]([a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9]([a-z0-9-]*[a-z0-9])?$";
        return str.matches(domainPattern);
    }

    /**
     * Checks if a string is a valid regex pattern.
     *
     * @param str
     *            String to check
     * @return true if string compiles as valid regex pattern, false otherwise
     */
    private boolean isRegexPattern(final String str) {
        if (str == null || str.length() == 0) {
            return false;
        }
        /* Check if it looks like a regex pattern (contains regex special chars) */
        if (!containsRegexSpecialChars(str)) {
            return false;
        }
        /* Try to compile as regex */
        try {
            Pattern.compile(str);
            return true;
        } catch (final Exception e) {
            return false;
        }
    }

    /**
     * Checks if a string contains typical regex special characters. This is a heuristic to distinguish plain domains from regex patterns.
     */
    private boolean containsRegexSpecialChars(final String str) {
        if (str == null) {
            return false;
        }
        /* Common regex special characters that wouldn't appear in plain domains */
        final String regexChars = ".*+?[]{}()^$|\\";
        for (int i = 0; i < regexChars.length(); i++) {
            if (str.indexOf(regexChars.charAt(i)) != -1) {
                return true;
            }
        }
        return false;
    }

    /**
     * Migrates old blacklist/whitelist settings to new CaptchaChallengeFilter list.
     *
     * @param blacklist
     *            List of blacklisted domains/patterns
     * @param blacklistEnabled
     *            Whether blacklist is enabled
     * @param whitelist
     *            List of whitelisted domains/patterns
     * @param whitelistEnabled
     *            Whether whitelist is enabled
     * @return List of CaptchaChallengeFilter objects, or null if no valid filters were created
     */
    public List<CaptchaChallengeFilter> migrateBlackWhiteListToFilters(final List<String> blacklist, final boolean blacklistEnabled, final List<String> whitelist, final boolean whitelistEnabled) {
        final List<CaptchaChallengeFilter> allFilters = new ArrayList<CaptchaChallengeFilter>();
        int position = 0;
        /* Process blacklist */
        if (blacklist != null && blacklist.size() > 0) {
            final List<CaptchaChallengeFilter> blacklistFilters = domainsToFilters(blacklist, CaptchaChallengeFilter.CaptchaFilterType.BLACKLIST);
            if (blacklistFilters != null) {
                for (int i = 0; i < blacklistFilters.size(); i++) {
                    final CaptchaChallengeFilter filter = blacklistFilters.get(i);
                    filter.setPosition(position);
                    position++;
                    if (!blacklistEnabled) {
                        /* Disable filter if blacklist is disabled */
                        filter.setEnabled(false);
                    }
                    allFilters.add(filter);
                }
            }
        }
        /* Process whitelist */
        if (whitelist != null && whitelist.size() > 0) {
            final List<CaptchaChallengeFilter> whitelistFilters = domainsToFilters(whitelist, CaptchaChallengeFilter.CaptchaFilterType.WHITELIST);
            if (whitelistFilters != null) {
                for (int i = 0; i < whitelistFilters.size(); i++) {
                    final CaptchaChallengeFilter filter = whitelistFilters.get(i);
                    filter.setPosition(position);
                    position++;
                    if (!whitelistEnabled) {
                        /* Disable filter if whitelist is disabled */
                        filter.setEnabled(false);
                    }
                    allFilters.add(filter);
                }
            }
        }
        if (allFilters.size() > 0) {
            return allFilters;
        }
        return null;
    }
}