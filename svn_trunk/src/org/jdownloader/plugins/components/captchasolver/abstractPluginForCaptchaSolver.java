package org.jdownloader.plugins.components.captchasolver;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.DebugMode;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.ChallengeVetoReason;
import org.jdownloader.captcha.v2.PluginChallengeSolver;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.CaptchaType;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.PluginForHost;

/**
 * Abstract base class for captcha solver plugins.
 */
public abstract class abstractPluginForCaptchaSolver extends PluginForHost {
    public <T> PluginChallengeSolver<T> getPluginChallengeSolver(final Challenge<T> c, Account account) throws Exception {
        final abstractPluginForCaptchaSolver plugin = getNewPluginInstance(getLazyP());
        plugin.setBrowser(plugin.createNewBrowserInstance());
        return new PluginChallengeSolver<T>(plugin, account);
    }

    /**
     * Constructor for the plugin.
     *
     * @param wrapper
     *            The plugin wrapper
     */
    public abstractPluginForCaptchaSolver(PluginWrapper wrapper) {
        super(wrapper);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            this.enablePremium(getBuyPremiumUrl());
        }
    }

    /**
     * Returns the features supported by this plugin.
     *
     * @return Array of supported features
     */
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    public abstract String getBuyPremiumUrl();

    /**
     * Reports a captcha as invalid.
     *
     * @param response
     *            The captcha response to report as invalid
     * @return true if the report was successfully sent, false otherwise
     */
    public abstract boolean setInvalid(final AbstractResponse<?> response, final Account account) throws Exception;

    /**
     * Reports a captcha as valid.
     *
     * @param response
     *            The captcha response to report as valid
     * @return true if the report was successfully sent, false otherwise
     */
    public abstract boolean setValid(final AbstractResponse<?> response, final Account account) throws Exception;

    public boolean setUnused(final AbstractResponse<?> response, final Account account) throws Exception {
        return false;
    }

    /**
     * Returns the list of captcha types supported by this solver. <br>
     * Important: If a solver supports all reCaptcha captcha types, return RECAPTCHA_V2, RECAPTCHA_V2_ENTERPRISE AND RECAPTCHA_V2_INVISIBLE
     * !
     *
     *
     *
     * @return List of supported captcha types
     */
    public abstract List<CAPTCHA_TYPE> getSupportedCaptchaTypes();

    public List<CAPTCHA_TYPE> getUserDisabledCaptchaTypes(final Account account) {
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            return null;
        }
        final List<CAPTCHA_TYPE> disabled_captcha_types = new ArrayList<CAPTCHA_TYPE>();
        for (final CAPTCHA_TYPE ctype : CAPTCHA_TYPE.values()) {
            final CaptchaType captchaType = new CaptchaType(ctype);
            captchaType.setAccountInfo(ai);
            if (!captchaType.isEnabled()) {
                disabled_captcha_types.add(ctype);
            }
        }
        return disabled_captcha_types;
    }

    /**
     * Determines whether the user should be notified when the account balance is low.
     *
     * @return true if the user should be notified on low balance, false otherwise
     */
    protected boolean notifyOnLowBalance(final Account account) {
        // TODO: Implement logic
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return true;
        }
        return cfg.isWarnOnLowCredits();
    }

    /** Returns interval used for polling when waiting for captcha solution from solver. */
    public int getPollingIntervalMillis(final Account account) {
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return 5000;
        }
        return cfg.getPollingIntervalSeconds() * 1000;
    }

    protected int getMaxSimultaneousCaptchas(final Account account) {
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return Integer.MAX_VALUE;
        }
        return cfg.getMaxSimultaneousCaptchas();
    }

    public boolean isDomainBlacklistEnabled() {
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return false;
        }
        return cfg.isDomainBlacklistEnabled();
    }

    public List<String> getBlacklistedDomains(final Account account) {
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return null;
        }
        return commaSeparatedDomainsToList(cfg.getDomainBlacklist());
    }

    public boolean isDomainWhitelistEnabled() {
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return false;
        }
        return cfg.isDomainWhitelistEnabled();
    }

    public List<String> getWhitelistedDomains(final Account account) {
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return null;
        }
        return commaSeparatedDomainsToList(cfg.getDomainWhitelist());
    }

    private List<String> commaSeparatedDomainsToList(final String input) {
        final List<String> ret = new ArrayList<String>();
        if (input == null || input.trim().length() == 0) {
            return null;
        }
        final String[] parts = input.split(",");
        for (int i = 0; i < parts.length; i++) {
            final String domain = parts[i].trim().toLowerCase(Locale.ROOT);
            if (isValidDomain(domain) && !ret.contains(domain)) {
                ret.add(domain);
            }
        }
        if (ret.size() > 0) {
            return ret;
        }
        return null;
    }

    private final boolean isValidDomain(final String domain) {
        if (domain == null || domain.length() == 0) {
            return false;
        }
        // TODO: Move this function somewhere else
        // Domain must contain at least one dot
        if (!domain.contains(".")) {
            return false;
        }
        // Regex pattern for domain validation
        // Allows: letters, numbers, hyphens; must not start or end with hyphen
        final String domainPattern = "^([a-z0-9]([a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9]([a-z0-9-]*[a-z0-9])?$";
        return domain.matches(domainPattern);
    }

    /** Returns interval used for polling when waiting for captcha solution from solver. */
    public int getMaxCaptchasPerHour(final Account account) {
        // TODO: Implement functionality
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return 1000;
        }
        return cfg.getMaxCaptchasPerHour();
    }

    /**
     * Fetches account information for a given account. This abstract method overrides the one from PluginForHost and forces subclasses to
     * implement it specifically for captcha solver services.
     *
     * @param account
     *            The account to fetch information for
     * @return The account information
     * @throws Exception
     *             If an error occurs during the fetch operation
     */
    @Override
    public abstract AccountInfo fetchAccountInfo(final Account account) throws Exception;

    public abstract void solve(CESSolverJob<?> job, Account account) throws Exception;

    protected static void checkInterruption() throws InterruptedException {
        if (Thread.interrupted()) {
            throw new InterruptedException();
        }
    }

    @Override
    public AvailableStatus requestFileInformation(DownloadLink parameter) throws Exception {
        /* Must override but should never be called. */
        throw new WTFException();
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception {
        /* Must override but should never be called. */
        throw new WTFException();
    }

    /** Returns false if the solver does not have enough balance to solve the given captcha challenge. */
    public boolean enoughBalanceFor(final Challenge<?> c, final Account account) {
        // TODO: Implement logic
        return true;
    }

    /**
     * Checks if this solver can handle a specific challenge.
     *
     * @param c
     *            The challenge to check
     * @return null if this solver can handle the challenge, ChallengeVetoReason otherwise
     */
    public final ChallengeVetoReason getVetoReason(final Challenge<?> c, Account account) {
        if (!account.isEnabled()) {
            return ChallengeVetoReason.ACCOUNT_DISABLED;
        }
        if (!account.isValid()) {
            return ChallengeVetoReason.ACCOUNT_IN_ERROR_STATE;
        }
        if (!this.enoughBalanceFor(c, account)) {
            return ChallengeVetoReason.ACCOUNT_NOT_ENOUGH_CREDITS;
        }
        return null;
    }

    private CaptchaSolverPluginConfig getDefaultConfig() {
        // TODO: Maybe ensure that every captcha solver plugin has a config or throw exception
        final Object cfgO = this.getConfigInterface();
        if (cfgO == null) {
            logger.warning("Solver has no config");
            return null;
        }
        if (!(cfgO instanceof CaptchaSolverPluginConfig)) {
            logger.warning("Unexpected solver config type");
            return null;
        }
        final CaptchaSolverPluginConfig cfg = (CaptchaSolverPluginConfig) cfgO;
        return cfg;
    }
}