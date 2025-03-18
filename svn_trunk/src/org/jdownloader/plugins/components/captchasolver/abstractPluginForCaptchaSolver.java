package org.jdownloader.plugins.components.captchasolver;

import org.appwork.utils.DebugMode;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.PluginForHost;

/**
 * Abstract base class for captcha solver plugins.
 */
public abstract class abstractPluginForCaptchaSolver extends PluginForHost {
    // Minimum balance threshold for notifications
    private static final double MIN_BALANCE_THRESHOLD = 1.0; // $1.00 USD/EUR

    /**
     * Enum representing different types of captchas.
     */
    public enum CAPTCHA_TYPE {
        IMAGE,
        RECAPTCHA_V2,
        HCAPTCHA,
        CUTCAPTCHA,
        SINGLE_CLICK_CAPTCHA,
        MULTI_CLICK_CAPTCHA,
        /* Types that exist but we don't support. */
        GEETEST_V1,
        GEETEST_V4,
        KEY_CAPTCHA
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
    // @Override
    // public boolean isPremiumEnabled() {
    // /* Every paid captcha solver needs a paid "premium" account in order to use it. */
    // return true;
    // }

    public abstract String getBuyPremiumUrl();

    /**
     * Returns the features supported by this plugin.
     *
     * @return Array of supported features
     */
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    /**
     * Reports a captcha as invalid.
     *
     * @param response
     *            The captcha response to report as invalid
     * @return true if the report was successfully sent, false otherwise
     */
    protected abstract boolean setInvalid(final AbstractResponse<?> response);

    /**
     * Reports a captcha as valid.
     *
     * @param response
     *            The captcha response to report as valid
     * @return true if the report was successfully sent, false otherwise
     */
    protected abstract boolean setValid(final AbstractResponse<?> response);

    /**
     * Checks if this solver can handle a specific challenge.
     *
     * @param c
     *            The challenge to check
     * @return true if this solver can handle the challenge, false otherwise
     */
    public abstract boolean canHandle(final Challenge<?> c);

    /**
     * Returns the list of captcha types supported by this solver.
     *
     * @return List of supported captcha types
     */
    public abstract java.util.List<CAPTCHA_TYPE> getSupportedCaptchaTypes();

    /**
     * Aborts a captcha solving request.
     *
     * @param response
     *            The captcha response to abort
     * @return true if the abort request was successfully sent, false otherwise
     */
    public boolean abortCaptcha(final AbstractResponse<?> response) {
        return false;
    }

    /**
     * Determines whether the user should be notified when the account balance is low.
     *
     * @return true if the user should be notified on low balance, false otherwise
     */
    public boolean notifyOnLowBalance() {
        // TODO: Add setting logic
        return true;
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

    protected void solveCES(CESSolverJob<String> job) throws InterruptedException, SolverException {
    }

    protected static void checkInterruption() throws InterruptedException {
        if (Thread.interrupted()) {
            throw new InterruptedException();
        }
    }

    @Override
    public AvailableStatus requestFileInformation(DownloadLink parameter) throws Exception {
        // MUST OVERRIDE
        return null;
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception {
        // MUST OVERRIDE
    }
}