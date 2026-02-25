package org.jdownloader.plugins.components.captchasolver;

import java.util.ArrayList;
import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.DebugMode;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.ChallengeVetoReason;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.PluginChallengeSolver;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfig;
import org.jdownloader.plugins.config.PluginJsonConfig;
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
    public <T> PluginChallengeSolver<T> getPluginChallengeSolver(final Challenge<T> c, final Account account) throws Exception {
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

    public List<FeedbackType> getSupportedFeedbackTypes() {
        return null;
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
    public boolean setValid(final AbstractResponse<?> response, final Account account) throws Exception {
        return false;
    }

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

    public boolean isEnableCaptchaFeedback(final Account account) {
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return true;
        }
        return cfg.isEnableCaptchaFeedback();
    }

    /**
     * Determines whether the user should be notified when the account balance is low.
     *
     * @return true if the user should be notified on low balance, false otherwise
     */
    public boolean notifyOnLowBalance(final Account account) {
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

    public int getMaxSimultaneousCaptchas(final Account account) {
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return Integer.MAX_VALUE;
        }
        return cfg.getMaxSimultaneousCaptchas();
    }

    public boolean isFilterListEnabled() {
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return true;
        }
        return cfg.isFilterListEnabled();
    }

    public List<CaptchaChallengeFilter> getCaptchaChallengeFilterList() {
        final CaptchaSolverPluginConfig cfg = getDefaultConfig();
        if (cfg == null) {
            return null;
        }
        return cfg.getFilterList();
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

    /**
     * Returns false if the solver does not have enough balance to solve the given captcha challenge. <br>
     */
    public boolean enoughBalanceFor(final Challenge<?> c, final Account account) {
        if (account.getAccountInfo() != null && account.getAccountInfo().getAccountBalance() <= 0) {
            return false;
        }
        return true;
    }

    /**
     * Checks if this solver can handle a specific challenge.
     *
     * @param c
     *            The challenge to check
     * @return null if this solver can handle the challenge, ChallengeVetoReason otherwise
     */
    public final ChallengeVetoReason getVetoReason(final Challenge<?> c, final Account account) {
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
        /*
         * TODO: Maybe ensure that every captcha solver plugin has a config or throw exception <br> Every captcha solver plugin should have
         * a config.
         */
        final Object cfgO = this.getConfigInterface();
        if (cfgO == null) {
            // TODO: Remove this fallback
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                logger.warning("Solver has no config");
                return PluginJsonConfig.get(CaptchaSolverPluginConfig.class);
            } else {
                throw new IllegalArgumentException("Solver has no config");
            }
        }
        if (!(cfgO instanceof CaptchaSolverPluginConfig)) {
            /* Developer mistake */
            throw new IllegalArgumentException("Unexpected solver config type");
        }
        final CaptchaSolverPluginConfig cfg = (CaptchaSolverPluginConfig) cfgO;
        return cfg;
    }

    /** Down below there are methods which we don't need but they need to be overridden. */
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
}