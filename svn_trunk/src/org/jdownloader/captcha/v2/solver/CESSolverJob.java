package org.jdownloader.captcha.v2.solver;

import javax.swing.Icon;

import org.appwork.utils.logging2.LogSource;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.captcha.v2.solverjob.SolverJob.SolverOutcome;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.notify.captcha.CESBubble;
import org.jdownloader.gui.notify.captcha.CESBubbleSupport;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;

public class CESSolverJob<T> {
    private final SolverJob<T>       job;
    private final ChallengeSolver<T> solver;
    private volatile SolverStatus    status;
    private CESBubble                bubble;
    private volatile boolean         answered;
    private volatile boolean         answerDelivered;
    /** True once a final status (failed, aborted, answer rejected) was set, which hideBubble must not overwrite. */
    private volatile boolean         finalStatusSet;
    /** Task ID the solver service assigned to the submitted captcha, or null as long as nothing has been submitted yet. */
    private volatile String          captchaSolverTaskID;

    /**
     * Plugins must call this right after the captcha was submitted to the solver service, so JDownloader is able to abort the task on the
     * server side (see {@link FeedbackType#ABORT_CAPTCHAS}) if the captcha runs into a timeout.
     */
    public void setCaptchaSolverTaskID(final String captchaSolverTaskID) {
        this.captchaSolverTaskID = captchaSolverTaskID;
    }

    public String getCaptchaSolverTaskID() {
        return captchaSolverTaskID;
    }

    /** True if this solver delivered an answer that the job accepted. */
    public boolean isAnswered() {
        return answered;
    }

    /** True once a final status (failed, timed out, aborted, answer rejected) was set. */
    public boolean isFinalStatusSet() {
        return finalStatusSet;
    }

    /** True if this solver delivered an answer at all, no matter whether the job accepted it (it may have arrived too late). */
    public boolean isAnswerDelivered() {
        return answerDelivered;
    }

    public SolverJob<T> getJob() {
        return job;
    }

    /**
     * The {@link ChallengeSolver} (e.g. a {@code PluginChallengeSolver}) that owns this job. Plugin solvers must attribute the
     * {@link AbstractResponse} they build to this instance (not to themselves) so that {@code AbstractResponse#setValidation} can find a
     * real {@link ChallengeSolver} and actually deliver setValid()/setInvalid()/setUnused() feedback.
     */
    public ChallengeSolver<T> getSolver() {
        return solver;
    }

    public CESSolverJob(SolverJob<T> job, ChallengeSolver<T> solver) {
        this.job = job;
        this.solver = solver;
    }

    public Challenge<T> getChallenge() {
        return job.getChallenge();
    }

    public void waitFor(int timeout, ChallengeSolver<?>... instances) throws InterruptedException {
        job.waitFor(timeout, instances);
    }

    public LogSource getLogger() {
        return job.getLogger();
    }

    public void setStatus(SolverStatus solverStatus) {
        this.status = solverStatus;
        if (solverStatus != null) {
            job.logSolverStatus(solver, "UI status: " + solverStatus.getLabel());
        }
        if (bubble != null) {
            bubble.update();
        }
    }

    /**
     * Sets the final status "failed" with the given reason (e.g. the error text of the solver service). It is not replaced by the generic
     * "No Answer!" in {@link #hideBubble()}.
     */
    public void setFailed(final String reason) {
        finalStatusSet = true;
        setStatus(new SolverStatus(_GUI.T.CESSolverJob_status_failed(reason == null || reason.length() == 0 ? "-" : reason), NewTheme.I().getIcon(IconKey.ICON_BAD, 20)));
    }

    /** Sets the final status "timed out": the solver was stopped because it (or the whole job) ran into its timeout. */
    public void setTimedOut() {
        finalStatusSet = true;
        setStatus(new SolverStatus(_GUI.T.CESSolverJob_status_timedOut(), NewTheme.I().getIcon(IconKey.ICON_WAIT, 20)));
    }

    /**
     * Sets the final status "aborted": the solver did not fail but was stopped from outside (job solved/skipped, another solver chosen,
     * timeout, ...). It is not replaced by the generic "No Answer!" in {@link #hideBubble()}.
     */
    public void setAborted() {
        finalStatusSet = true;
        setStatus(new SolverStatus(_GUI.T.CESSolverJob_status_aborted(), NewTheme.I().getIcon(IconKey.ICON_STOP, 20)));
    }

    public SolverStatus getStatus() {
        return status;
    }

    public void setStatus(String label, Icon icon) {
        setStatus(new SolverStatus(label, icon));
        if (bubble != null) {
            bubble.update();
        }
    }

    public void showBubble() throws InterruptedException {
        if (org.appwork.utils.Application.isHeadless()) {
            /* No GUI -> no bubble. */
            return;
        } else if (!CESBubbleSupport.getInstance().isEnabled()) {
            /* Bubbles are disabled */
            return;
        } else if (job.isForcedStart(solver)) {
            /*
             * The user explicitly chose this solver in a captcha dialog ("use external solver") -> send the captcha right away, without the
             * bubble that delays the upload and offers to abort it. The dialog solver may not be marked as done yet, so hasManualSolver()
             * cannot be used to detect this.
             */
            getLogger().info("Bubble not shown: solver " + solver + " was chosen by the user -> not delaying the captcha upload");
            return;
        } else if (!hasManualSolver()) {
            /*
             * The bubble delays the upload to give the user a chance to solve the captcha manually or to abort the upload. Without a manual
             * solver that delay is pointless -> send the captcha right away.
             */
            getLogger().info("Bubble is enabled but not shown: no manual solver available for this challenge, not delaying the captcha upload to " + solver + " | Solvers: " + job.getSolverList());
            return;
        }
        showBubble(CFG_CAPTCHA.CFG.getExternalCaptchaSolverChanceToAbortExternalCaptchaSolver());
    }

    /**
     * True if the job has a solver that is still working on the challenge and requires a human: dialog, browser or remote API (e.g. MyJD)
     * solver. Automatic local solvers and external solvers do not count.
     */
    private boolean hasManualSolver() {
        for (final ChallengeSolver<T> other : job.getSolverList()) {
            if (job.isDone(other)) {
                continue;
            }
            switch (other.getSolverType()) {
            case JD_LOCAL_DIALOG:
            case JD_LOCAL_BROWSER:
            case JD_REMOTE_API:
                return true;
            case JD_LOCAL:
            case EXTERNAL:
            default:
                break;
            }
        }
        return false;
    }

    public void showBubble(int timeout) throws InterruptedException {
        if (!org.appwork.utils.Application.isHeadless()) {
            bubble = CESBubbleSupport.getInstance().show(solver, this, timeout);
        }
    }

    public void setAnswer(AbstractResponse<?> abstractResponse) {
        answerDelivered = true;
        if (job.addAnswer((AbstractResponse<T>) abstractResponse)) {
            answered = true;
            job.setSolverResult(solver, SolverOutcome.ANSWERED, null);
            setStatus(_GUI.T.DeathByCaptchaSolver_solveBasicCaptchaChallenge_answer(abrev(abstractResponse.getValue() + "")), NewTheme.I().getIcon(IconKey.ICON_OK, 20));
        } else {
            /* Answer rejected by the job: keep this status instead of the generic "No Answer!" later. */
            finalStatusSet = true;
            job.setSolverResult(solver, SolverOutcome.ANSWER_REJECTED, null);
            setStatus(_GUI.T.DeathByCaptchaSolver_solveBasicCaptchaChallenge_answer_bad(abrev(abstractResponse.getValue() + "")), NewTheme.I().getIcon(IconKey.ICON_BAD, 20));
        }
    }

    private String abrev(String string) {
        if (string.length() <= 10) {
            return string;
        }
        return string.substring(0, 10) + "...";
    }

    public void hideBubble() {
        if (!answered && !finalStatusSet) {
            setStatus(SolverStatus.UNSOLVED);
            if (bubble != null) {
                bubble.update();
            }
        }
        if (bubble != null) {
            bubble.hideBubble(5000);
        }
    }
}
