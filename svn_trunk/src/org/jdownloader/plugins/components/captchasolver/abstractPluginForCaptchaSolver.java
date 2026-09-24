package org.jdownloader.plugins.components.captchasolver;

import java.util.ArrayList;
import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.logging2.LogInterface;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.CaptchaSolverCaptchaTypesSettingsPanelBuilder.AccountCaptchaTypeAccessor;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.ChallengeVetoReason;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.PluginChallengeSolver;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

/**
 * Abstract base class for captcha solver plugins.
 */
public abstract class abstractPluginForCaptchaSolver extends PluginForHost {
    public <T> PluginChallengeSolver<T> getPluginChallengeSolver(final Challenge<T> c, final Account account) throws Exception {
        final abstractPluginForCaptchaSolver plugin = getNewPluginInstance(getLazyP());
        /*
         * Use the logger of the plugin that triggered the challenge (e.g. the hoster/crawler plugin) if available, so solver requests show
         * up next to the download/crawl that caused them; fall back to this solver's own logger otherwise. Debug/verbose are always enabled
         * here so solver browser requests actually get logged (mirrors the old CESChallengeSolver#createNewBrowserInstance behavior).
         */
        final Plugin challengePlugin = c != null ? c.getPlugin() : null;
        LogInterface logger = challengePlugin != null ? challengePlugin.getLogger() : null;
        if (logger == null) {
            logger = plugin.getLogger();
        }
        plugin.setLogger(logger);
        final Browser br = plugin.createNewBrowserInstance();
        br.setLogger(logger);
        br.setDebug(true);
        br.setVerbose(true);
        plugin.setBrowser(br);
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
        /* All captcha solver plugins have account support. */
        this.enablePremium(getBuyPremiumUrl());
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

    /**
     * Returns list of captcha types supported by this account. <br>
     * Most solvers support all captcha types for all account types, only some (e.g. captchas.io) may support some captcha types only for
     * specific / more expensive account types.
     */
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes(final Account account) {
        return getSupportedCaptchaTypes();
    }

    /**
     * This can be used so that a plugin can provide this information in advance so that e.g. users can lookup this info in a comparison
     * table aka "Does this solver allow for response-feedback"?
     */
    public List<FeedbackType> getSupportedFeedbackTypes() {
        return null;
    }

    public abstract String getBuyPremiumUrl();

    /** Returns captcha challenge that this plugin is currently processing. */
    private Challenge<?> c = null;

    public Challenge<?> getCurrentCaptchaChallenge() {
        return this.c;
    }

    public void setCurrentCaptchaChallenge(Challenge<?> c) {
        this.c = c;
    }

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

    public List<CAPTCHA_TYPE> getUserDisabledCaptchaTypes(final Account account) {
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            return null;
        }
        final List<CAPTCHA_TYPE> disabled_captcha_types = new ArrayList<CAPTCHA_TYPE>();
        final AccountCaptchaTypeAccessor ata = new AccountCaptchaTypeAccessor(account);
        for (final CAPTCHA_TYPE ctype : CAPTCHA_TYPE.values()) {
            if (!ata.isEnabled(ctype)) {
                disabled_captcha_types.add(ctype);
            }
        }
        return disabled_captcha_types;
    }

    public boolean isEnableCaptchaFeedback(final Account account) {
        return getDefaultConfig().isEnableCaptchaFeedback();
    }

    /**
     * Determines whether the user should be notified when the account balance is low.
     *
     * @return true if the user should be notified on low balance, false otherwise
     */
    public boolean notifyOnLowBalance(final Account account) {
        // TODO: Implement logic
        return getDefaultConfig().isWarnOnLowCredits();
    }

    /** Returns interval used for polling when waiting for captcha solution from solver. */
    public int getPollingIntervalMillis(final Account account) {
        return getDefaultConfig().getPollingIntervalSeconds() * 1000;
    }

    /**
     * Returns the maximum number of captchas this solver service itself allows to be solved simultaneously, as enforced server-side by the
     * service (not to be confused with the user's own local JDownloader setting, {@link CaptchaSolverConfigV3#getMaxSimultaneousCaptchas()}).
     * May depend on the given account (e.g. plan/tier-based limits), but does not have to. <br>
     * {@link Integer#MAX_VALUE} = unlimited/unknown (default, no server-side limit documented for this service).
     */
    public int getServerSideMaxSimultaneousCaptchaThreadsLimit(final Account account) {
        return Integer.MAX_VALUE;
    }

    /**
     * Returns the maximum time in milliseconds this solver service itself keeps polling a submitted task available/pollable before giving up
     * on it server-side (e.g. the task result expires and further status checks will fail). This is a property of the remote service, not a
     * user setting. <br>
     * {@link Long#MAX_VALUE} = unlimited/unknown (default, no server-side polling timeout documented for this service).
     */
    public long getServerSideMaxPollingTimeoutMillis() {
        return Long.MAX_VALUE;
    }

    /** Returns interval used for polling when waiting for captcha solution from solver. */
    public int getMaxCaptchasPerHour(final Account account) {
        // TODO: Implement functionality
        return getDefaultConfig().getMaxCaptchasPerHour();
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
     * Waits between two consecutive polling attempts while waiting for a captcha solution to become available. Subclasses must call this
     * inside their polling loop instead of sleeping themselves, right before each polling request. <br>
     * Does not enforce {@link #getServerSideMaxPollingTimeoutMillis()} itself: that limit already flows into
     * {@link PluginChallengeSolver#getTimeoutMillis()} / {@link ChallengeSolver#getFinalTimeoutMillis()}, which arms a timer in
     * {@link JobRunnable} that kills (interrupts) this solver once it is exceeded -- checked here via the plain
     * {@link InterruptedException} below, no separate timeout tracking needed. <br>
     * Sleeps in at most 1 second chunks (mirrors {@link jd.plugins.PluginForHost#sleep(long, jd.plugins.DownloadLink)}) instead of one
     * long {@link Thread#sleep(long)}, so an interrupt is noticed within a second instead of only after the full polling interval. <br>
     * Logs the approximate polling attempt number (derived from elapsed time / interval, no extra counter state needed), the total elapsed
     * time and the configured polling interval.
     */
    protected void waitDuringPolling(final Challenge<?> challenge, final Account account) throws InterruptedException {
        checkInterruption();
        final long intervalMillis = getPollingIntervalMillis(account);
        final long elapsedMillis = System.currentTimeMillis() - challenge.getCreated();
        final long approximateAttempt = intervalMillis > 0 ? elapsedMillis / intervalMillis + 1 : 1;
        getLogger().info("Captcha polling attempt #" + approximateAttempt + ", elapsed " + elapsedMillis + "ms, interval " + intervalMillis + "ms");
        long remainingMillis = intervalMillis;
        while (remainingMillis > 0) {
            Thread.sleep(Math.min(1000L, remainingMillis));
            remainingMillis -= 1000L;
            checkInterruption();
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
    public ChallengeVetoReason getVetoReason(final Challenge<?> c, final Account account) {
        return null;
    }

    @Override
    public Class<? extends CaptchaSolverPluginConfig> getConfigInterface() {
        return CaptchaSolverPluginConfig.class;
    }

    protected CaptchaSolverPluginConfig getDefaultConfig() {
        final Class<? extends CaptchaSolverPluginConfig> configInterfaceClass = this.getConfigInterface();
        final CaptchaSolverPluginConfig cfg = get(configInterfaceClass);
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