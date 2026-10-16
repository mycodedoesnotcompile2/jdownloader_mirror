package org.jdownloader.captcha.v2;

import java.util.Set;
import java.util.ArrayList;
import java.util.List;

import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfig;

import jd.controlling.captcha.SkipException;
import jd.plugins.Account;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

/**
 * A ChallengeSolver implementation that uses a plugin account for solving challenges. This allows solvers to use account-specific
 * credentials or features.
 */
public class PluginChallengeSolver<T> extends ChallengeSolver<T> {
    protected final Account                        account;
    protected final abstractPluginForCaptchaSolver plugin;

    public PluginChallengeSolver(abstractPluginForCaptchaSolver plugin, Account account) {
        if (plugin == null || account == null) {
            throw new IllegalArgumentException();
        }
        final ChallengeResponseController crp = ChallengeResponseController.getInstance();
        SolverService svs = ChallengeResponseController.getInstance().getServiceByID(plugin.getHost());
        if (svs == null) {
            /* Plugin appeared/updated after ChallengeResponseController's one-time startup scan (e.g. hot-loaded, no restart yet). */
            svs = new PluginForCaptchaSolverSolverService(plugin);
            crp.addSolverService(svs);
        }
        this.service = svs;
        this.account = account;
        this.plugin = plugin;
    }

    @Override
    public boolean isEnabled() {
        return this.service.getConfigV3().isEnabled() && this.account.isEnabled();
    }

    /**
     * Combines the user's configured max polling time (if enabled) with the solver service's own server-side limit
     * ({@link abstractPluginForCaptchaSolver#getServerSideMaxPollingTimeoutMillis()}), whichever is smaller. Both of those return
     * {@link Long#MAX_VALUE} for "no limit"; that value must never reach {@link JobRunnable}'s
     * {@link org.appwork.scheduler.DelayedRunnable} as a real delay (huge millisecond delays risk overflow in the underlying scheduler), so
     * it is translated back to this class' own "no timeout" sentinel, -1, same as the default {@link ChallengeSolver#getTimeoutMillis()}.
     */
    @Override
    public long getTimeoutMillis() {
        final CaptchaSolverConfigV3 cfg = service.getConfigV3();
        final long configured = cfg.isMaxPollingTimeoutEnabled() ? cfg.getMaxPollingTimeoutSeconds() * 1000L : Long.MAX_VALUE;
        final long serverSide = plugin.getServerSideMaxPollingTimeoutMillis();
        final long combined = Math.min(configured, serverSide);
        if (combined == Long.MAX_VALUE) {
            return -1;
        }
        return Math.max(1L, combined);
    }

    /**
     * The user's configured max simultaneous captchas limit (if enabled), but always at least 1. It is the user's limit for this solver as
     * a whole, i.e. it applies to all accounts of the solver service together (the per-account server-side limit is handled separately by
     * {@link #getFinalMaxCaptchaThreadsPerAccount()}).
     */
    @Override
    public final int getFinalMaxCaptchaThreads() {
        final CaptchaSolverConfigV3 cfg = service.getConfigV3();
        final int configured = cfg.isLimitMaxSimultaneousCaptchasEnabled() ? cfg.getMaxSimultaneousCaptchas() : Integer.MAX_VALUE;
        return Math.max(1, configured);
    }

    /**
     * The solver service's own server-side limit for this solver's account
     * ({@link abstractPluginForCaptchaSolver#getServerSideMaxSimultaneousCaptchaThreadsLimit(Account)}), but always at least 1. It applies
     * to every account on its own, so a second account of the same solver service has its own, independent budget.
     */
    @Override
    public final int getFinalMaxCaptchaThreadsPerAccount() {
        return Math.max(1, plugin.getServerSideMaxSimultaneousCaptchaThreadsLimit(account));
    }

    /**
     * The enabled custom limit rules of this solver (they apply to the solver as a whole, i.e. to all of its accounts together), or null if
     * the user has not switched custom limits on or no rule is enabled. The rule list in the config is the very list the settings table
     * edits, so it is copied here (as a snapshot) before it gets iterated.
     */
    @Override
    public List<CaptchaSolverLimitRule> getCustomLimitRules() {
        final CaptchaSolverPluginConfig cfg = ((PluginForCaptchaSolverSolverService) service).getPluginConfig();
        if (!cfg.isCustomLimitsEnabled()) {
            return null;
        }
        final ArrayList<CaptchaSolverLimitRule> stored = cfg.getLimitRules();
        if (stored == null) {
            return null;
        }
        final List<CaptchaSolverLimitRule> ret = new ArrayList<CaptchaSolverLimitRule>();
        for (final CaptchaSolverLimitRule rule : stored.toArray(new CaptchaSolverLimitRule[0])) {
            if (rule.isEnabled()) {
                ret.add(rule);
            }
        }
        return ret.isEmpty() ? null : ret;
    }

    /*
     * Distinguishes the various per-account/per-plugin instances of this solver in log output (otherwise they all just print
     * "PluginChallengeSolver", see ChallengeSolver#toString()).
     */
    @Override
    public String toString() {
        return super.toString() + "@" + plugin.getHost();
    }

    public Account getAccount() {
        return account;
    }

    @Override
    public Set<FeedbackType> getSupportedFeedbackTypes() {
        return plugin.getSupportedFeedbackTypes();
    }

    @Override
    public Set<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        return plugin.getSupportedCaptchaTypes(account);
    }

    @Override
    public Set<CAPTCHA_TYPE> getUserDisabledCaptchaTypes() {
        return this.plugin.getUserDisabledCaptchaTypes(account);
    }

    @Override
    public ChallengeVetoReason getChallengeVetoReason(Challenge<?> c) {
        if (!account.isEnabled()) {
            return ChallengeVetoReason.ACCOUNT_DISABLED;
        }
        if (!account.isValid()) {
            return ChallengeVetoReason.ACCOUNT_IN_ERROR_STATE;
        }
        if (!plugin.enoughBalanceFor(c, account)) {
            return ChallengeVetoReason.ACCOUNT_NOT_ENOUGH_CREDITS;
        }
        /* Check if plugin provides a veto reason */
        final ChallengeVetoReason veto = plugin.getVetoReason(c, account);
        if (veto != null) {
            return veto;
        }
        final Set<CAPTCHA_TYPE> all_supported_captcha_types = this.plugin.getSupportedCaptchaTypes();
        final Set<CAPTCHA_TYPE> account_supported_captcha_types = this.plugin.getSupportedCaptchaTypes(this.account);
        final CAPTCHA_TYPE ctype = CAPTCHA_TYPE.getCaptchaTypeForChallenge(c);
        if (ctype != null && all_supported_captcha_types != null && account_supported_captcha_types != null && all_supported_captcha_types.contains(ctype) && !account_supported_captcha_types.contains(ctype)) {
            return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER_ACCOUNT;
        }
        final ChallengeVetoReason superVeto = super.getChallengeVetoReason(c);
        if (superVeto != null) {
            return superVeto;
        }
        /*
         * Checked last: this solver could solve the challenge, so the only reason left not to use it is that it would exceed one of the
         * user's custom limits (and the check logs which one, which is only interesting for challenges the solver is eligible for).
         */
        if (ChallengeResponseController.getInstance().isCustomLimitReached(this)) {
            return ChallengeVetoReason.CUSTOM_LIMIT_REACHED;
        }
        return null;
    }

    /* The feedback type is already checked by ChallengeSolver#setValid. */
    @Override
    protected boolean onValid(final AbstractResponse<?> response) {
        if (!plugin.isEnableCaptchaFeedback(account)) {
            /* Feedback is disabled by user */
            return false;
        }
        try {
            return this.plugin.setValid(response, account);
        } catch (Exception e) {
            // TODO: Handle plugin/account related exceptions
            e.printStackTrace();
            return false;
        }
    }

    /* The feedback type is already checked by ChallengeSolver#setInvalid. */
    @Override
    protected boolean onInvalid(final AbstractResponse<?> response) {
        if (!plugin.isEnableCaptchaFeedback(account)) {
            /* Feedback is disabled by user */
            return false;
        }
        try {
            return this.plugin.setInvalid(response, account);
        } catch (Exception e) {
            // TODO: Handle plugin/account related exceptions
            e.printStackTrace();
            return false;
        }
    }

    /* The feedback type is already checked by ChallengeSolver#setUnused. */
    @Override
    protected boolean onUnused(final AbstractResponse<?> response) {
        if (!plugin.isEnableCaptchaFeedback(account)) {
            /* Feedback is disabled by user */
            return false;
        }
        try {
            return this.plugin.setUnused(response, account);
        } catch (Exception e) {
            // TODO: Handle plugin/account related exceptions
            e.printStackTrace();
            return false;
        }
    }

    /**
     * Aborts the captcha task on the server side of the solver service if JDownloader no longer waits for its answer, so the service stops
     * working on it and (ideally) does not charge for it: the solver or the whole job ran into a timeout, or the job ended without this
     * solver's answer (skipped/aborted by the user, solved by another solver). Does nothing if the plugin does not support
     * {@link FeedbackType#ABORT_CAPTCHAS}, the user disabled captcha feedback, no task was submitted or this solver already delivered an
     * answer (that one gets {@link #setUnused(AbstractResponse)}/valid/invalid feedback instead).
     */
    private void abortServerSideIfNotNeeded(final SolverJob<T> job, final CESSolverJob<T> cesJob) {
        final String taskID = cesJob.getCaptchaSolverTaskID();
        if (taskID == null || cesJob.isAnswerDelivered()) {
            return;
        }
        final boolean timedOut = job.isSolverTimedOut(this);
        if (!timedOut && job.isAlive()) {
            /* Job still wants an answer: the solver just ended on its own (e.g. unsolved), the task is finished on the server side. */
            return;
        }
        final Set<FeedbackType> feedbacktypes = this.getSupportedFeedbackTypes();
        if (feedbacktypes == null || !feedbacktypes.contains(FeedbackType.ABORT_CAPTCHAS) || !plugin.isEnableCaptchaFeedback(account)) {
            return;
        }
        /* Clear a pending interrupt (kill interrupts the solver thread), otherwise the abort request would fail immediately. */
        Thread.interrupted();
        final String reason = timedOut ? "timeout" : "job no longer needs an answer";
        try {
            /* No answer exists, so build a value-less response that carries the task ID. */
            final AbstractResponse<T> placeholder = new AbstractResponse<T>(job.getChallenge(), this, null);
            placeholder.setCaptchaSolverTaskID(taskID);
            final boolean aborted = plugin.abortCaptcha(placeholder, account);
            job.logSolverStatus(this, "Server side abort of task " + taskID + " (" + reason + "): " + (aborted ? "success" : "failed"));
        } catch (final Exception e) {
            job.logSolverStatus(this, "Server side abort of task " + taskID + " (" + reason + ") failed: " + e);
            plugin.getLogger().log(e);
        }
    }

    /** Returns the error text of the solver service/plugin if there is one, else a generic description of the exception. */
    private static String getFailureReason(final Exception e) {
        final String message = e.getMessage();
        if (message != null && message.trim().length() > 0) {
            return message.trim();
        } else if (e instanceof PluginException) {
            return LinkStatus.toString(((PluginException) e).getLinkStatus());
        }
        return e.getClass().getSimpleName();
    }

    @Override
    public void solve(SolverJob<T> job) throws InterruptedException, SolverException, SkipException {
        final CESSolverJob<T> cesJob = new CESSolverJob<T>(job, this);
        try {
            cesJob.showBubble();
            plugin.setCurrentCaptchaChallenge(job.getChallenge());
            plugin.solve(cesJob, account);
        } catch (final PluginException e) {
            // TODO: Set detailed failure feedback on SolverJob e.g. if failure was account related.
            if (e.getLinkStatus() == LinkStatus.ERROR_CAPTCHA) {
                /*
                 * Captcha could not be solved (e.g. unsolvable captcha). This is a problem of the captcha itself and not of the account ->
                 * Only log it and do not set an account error.
                 */
                job.logSolverStatus(this, "FAILED (captcha could not be solved: " + e.getMessage() + ")");
                cesJob.setFailed(getFailureReason(e));
                plugin.getLogger().log(e);
            } else {
                job.logSolverStatus(this, "FAILED (account/plugin error: " + LinkStatus.toString(e.getLinkStatus()) + " " + e.getMessage() + ")");
                cesJob.setFailed(getFailureReason(e));
                plugin.handleAccountException(account, plugin.getLogger(), e);
            }
        } catch (InterruptedException e) {
            /* Not a failure: the solver was stopped from outside (job solved/skipped, other solver chosen, timeout). */
            if (job.isSolverTimedOut(this)) {
                job.logSolverStatus(this, "TIMED OUT (solver or job timeout, solver was interrupted)");
                cesJob.setTimedOut();
            } else {
                job.logSolverStatus(this, "INTERRUPTED/CANCELED");
                cesJob.setAborted();
            }
            plugin.getLogger().log(e);
        } catch (Exception e) {
            job.logSolverStatus(this, "FAILED (" + e + ")");
            cesJob.setFailed(getFailureReason(e));
            /* E.g. SolverException with the error text of the solver service: Make sure it ends up in the plugin log. */
            plugin.getLogger().log(e);
            // TODO
            e.printStackTrace();
        } finally {
            abortServerSideIfNotNeeded(job, cesJob);
            cesJob.hideBubble();
            // TODO: Check if it's fine to nullify here
            plugin.setCurrentCaptchaChallenge(null);
        }
    }
}