package org.jdownloader.plugins.components.captchasolver;

import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.DebugMode;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.PluginChallengeSolver;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.PluginForHost;

/**
 * Abstract base class for captcha solver plugins.
 */
public abstract class abstractPluginForCaptchaSolver extends PluginForHost {
    /** Returns false if the solver does not have enough balance to solve the given captcha challenge. */
    protected boolean enoughBalanceFor(final Challenge<?> c, final Account account) throws Exception {
        return true;
    }

    /**
     * Checks if this solver can handle a specific challenge.
     *
     * @param c
     *            The challenge to check
     * @return true if this solver can handle the challenge, false otherwise
     */
    public final boolean canHandle(final Challenge<?> c) {
        if (!validateBlackWhite(c)) {
            return false;
        }
        final List<CAPTCHA_TYPE> supportedTypes = this.getSupportedCaptchaTypes();
        for (final CAPTCHA_TYPE supportedType : supportedTypes) {
            if (supportedType.canHandle(c)) {
                return true;
            }
        }
        return false;
    }

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
    public abstract java.util.List<CAPTCHA_TYPE> getSupportedCaptchaTypes();

    /**
     * Determines whether the user should be notified when the account balance is low.
     *
     * @return true if the user should be notified on low balance, false otherwise
     */
    protected boolean notifyOnLowBalance(final Account account) {
        // TODO: Implement setting and logic
        return true;
    }

    /** Returns interval used for polling when waiting for captcha solution from solver. */
    public int getPollingIntervalMillis(final Account account) {
        return 5000;
    }

    protected int getMaxSimultaneousCaptchas(final Account account) {
        return Integer.MAX_VALUE;
    }

    protected List<String> getBlacklistedDomains(final Account account) {
        return null;
    }

    protected List<String> getWhitelistedDomains(final Account account) {
        return null;
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

    public boolean validateBlackWhite(Challenge<?> c) {
        // TODO: Add functionality
        return true;
    }

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
}