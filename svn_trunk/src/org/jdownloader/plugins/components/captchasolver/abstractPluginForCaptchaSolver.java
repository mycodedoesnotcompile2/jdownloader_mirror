package org.jdownloader.plugins.components.captchasolver;

import java.util.EnumSet;
import java.util.Set;
import java.util.ArrayList;
import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.LogInterface;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.CaptchaSolverCaptchaTypesSettingsPanelBuilder.AccountCaptchaTypeAccessor;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.ChallengeSolver.ChallengeVetoReason;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.JobRunnable;
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
import jd.plugins.Plugin;
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

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader " + this.getVersion());
        br.setFollowRedirects(true);
        return br;
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
     * @return Set of supported captcha types
     */
    public abstract Set<CAPTCHA_TYPE> getSupportedCaptchaTypes();

    /**
     * Returns list of captcha types supported by this account. <br>
     * Most solvers support all captcha types for all account types, only some (e.g. captchas.io) may support some captcha types only for
     * specific / more expensive account types.
     */
    public Set<CAPTCHA_TYPE> getSupportedCaptchaTypes(final Account account) {
        return getSupportedCaptchaTypes();
    }

    /**
     * Returns the captcha feedback types this solver supports: e.g. reporting valid/invalid captchas. Default: none. Plugins whose service
     * offers captcha feedback have to override this together with {@link #setValid}, {@link #setInvalid} and/or {@link #abortCaptcha}. Null
     * or an empty set means that the solver does not offer any captcha feedback. <br>
     * Also used so that users can lookup this info in advance, e.g. in a comparison table aka "Does this solver allow for
     * response-feedback"?
     */
    public Set<FeedbackType> getSupportedFeedbackTypes() {
        return EnumSet.noneOf(FeedbackType.class);
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
    public boolean setInvalid(final AbstractResponse<?> response, final Account account) throws Exception {
        return false;
    }

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

    /**
     * Reports a captcha response as unused: the solver DID deliver an answer, but JDownloader did not use it, e.g. because the answer
     * arrived after the job was already solved/skipped/timed out, or because another solver's differing answer was used instead. <br>
     * Not to be confused with {@link #abortCaptcha(String, Account)}, which is for challenges that have NO answer (yet).
     *
     * @param response
     *            The captcha response that was not used
     * @return true if the report was successfully sent, false otherwise
     */
    public boolean setUnused(final AbstractResponse<?> response, final Account account) throws Exception {
        return false;
    }

    /**
     * Aborts a captcha task on the server side of the solver service because JDownloader no longer waits for its answer: JDownloader ran
     * into its own timeout, the user skipped/aborted the captcha, or another solver solved it first. At that point there is no answer
     * from this solver. The service should stop working on the task and ideally not charge for it. Only called if the plugin declares {@link FeedbackType#ABORT_CAPTCHAS} in
     * {@link #getSupportedFeedbackTypes()}. Plugins that declare it must override this.
     *
     * @param response
     *            Placeholder response without a value: it only carries the task ID the solver service assigned to the captcha, see
     *            {@link AbstractResponse#getCaptchaSolverTaskID()}.
     * @return true if the task was aborted successfully, false otherwise
     */
    public boolean abortCaptcha(final AbstractResponse<?> response, final Account account) throws Exception {
        return false;
    }

    public Set<CAPTCHA_TYPE> getUserDisabledCaptchaTypes(final Account account) {
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            return null;
        }
        final Set<CAPTCHA_TYPE> disabled_captcha_types = EnumSet.noneOf(CAPTCHA_TYPE.class);
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
     * Returns interval used for polling when waiting for captcha solution from solver: the user's configured polling interval, but never
     * shorter than the minimum the service itself requires ({@link #getServerSideMinPollingIntervalMillis()}).
     */
    public int getPollingIntervalMillis(final Account account) {
        return (int) Math.max(getDefaultConfig().getPollingIntervalSeconds() * 1000, getServerSideMinPollingIntervalMillis());
    }

    /**
     * Returns the minimum time in milliseconds this solver service itself wants between two polling requests (e.g. documented rate limit or
     * recommended polling interval of the service's API). The user's polling interval setting cannot go below this value, see
     * {@link #getPollingIntervalMillis(Account)}. This is a property of the remote service, not a user setting. <br>
     * 0 = no minimum (default, no minimum documented for this service).
     */
    public long getServerSideMinPollingIntervalMillis() {
        return 0;
    }

    /**
     * Returns the maximum number of captchas this solver service itself allows to be solved simultaneously <b>per account</b>, as enforced
     * server-side by the service (not to be confused with the user's own local JDownloader setting, which applies to the solver as a whole:
     * {@link CaptchaSolverConfigV3#getMaxSimultaneousCaptchas()}). May depend on the given account (e.g. plan/tier-based limits), but does
     * not have to. <br>
     * {@link Integer#MAX_VALUE} = unlimited/unknown (default, no server-side limit documented for this service).
     */
    public int getServerSideMaxSimultaneousCaptchaThreadsLimit(final Account account) {
        return Integer.MAX_VALUE;
    }

    /**
     * Returns the maximum time in milliseconds this solver service itself keeps polling a submitted task available/pollable before giving
     * up on it server-side (e.g. the task result expires and further status checks will fail). This is a property of the remote service,
     * not a user setting. <br>
     * {@link Long#MAX_VALUE} = unlimited/unknown (default, no server-side polling timeout documented for this service).
     */
    public long getServerSideMaxPollingTimeoutMillis() {
        return Long.MAX_VALUE;
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
     * Sleeps in at most 1 second chunks (mirrors {@link jd.plugins.PluginForHost#sleep(long, jd.plugins.DownloadLink)}) instead of one long
     * {@link Thread#sleep(long)}, so an interrupt is noticed within a second instead of only after the full polling interval. <br>
     * Logs the approximate polling attempt number (derived from elapsed time / interval, no extra counter state needed), the total elapsed
     * time and the configured polling interval.
     */
    protected void waitDuringPolling(final Challenge<?> challenge, final Account account) throws InterruptedException {
        checkInterruption();
        final long intervalMillis = getPollingIntervalMillis(account);
        final long elapsedMillis = System.currentTimeMillis() - challenge.getCreated();
        final long approximateAttempt = intervalMillis > 0 ? elapsedMillis / intervalMillis + 1 : 1;
        final long maxMillis = new PluginChallengeSolver<Object>(this, account).getFinalTimeoutMillis();
        final StringBuilder sb = new StringBuilder();
        sb.append("Captcha polling attempt #").append(approximateAttempt);
        sb.append(", elapsed ").append(TimeFormatter.formatMilliSeconds(elapsedMillis, 0));
        if (maxMillis > 0) {
            sb.append(" of max ").append(TimeFormatter.formatMilliSeconds(maxMillis, 0));
            sb.append(" (").append(Math.min(100, elapsedMillis * 100 / maxMillis)).append("%)");
        }
        sb.append(", interval ").append(TimeFormatter.formatMilliSeconds(intervalMillis, 0));
        getLogger().info(sb.toString());
        long remainingMillis = intervalMillis;
        while (remainingMillis > 0) {
            Thread.sleep(Math.min(1000L, remainingMillis));
            remainingMillis -= 1000L;
            checkInterruption();
        }
    }

    /**
     * Returns false if the solver does not have enough balance to solve the given captcha challenge. <br>
     * In theory if we know how much a challenge would cost for the current solver, plugins cloud override this and place individual checks.
     * <br>
     * In reality this is a small detail which we can often ignore.
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

    /** Public because the AccountController needs the low credits settings after an account check. */
    public CaptchaSolverPluginConfig getDefaultConfig() {
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