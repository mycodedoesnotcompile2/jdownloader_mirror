package org.jdownloader.captcha.v2.solver.gui;

import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
import org.jdownloader.captcha.v2.solver.service.DialogSolverService;
import org.jdownloader.captcha.v2.solverjob.ChallengeSolverJobListener;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.settings.staticreferences.CFG_SILENTMODE;

import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.gui.swing.jdgui.JDGui;

public abstract class AbstractDialogSolver<T> extends ChallengeSolver<T> {

    public AbstractDialogSolver(int i) {
        super(DialogSolverService.getInstance(), i);
    }

    /** Silent mode with "Skip the Captcha" or "Do not show a dialog, but try auto solver": The user cannot be asked, so no dialog. */
    @Override
    public ChallengeVetoReason getChallengeVetoReason(final Challenge<?> c) {
        final ChallengeVetoReason superVeto = super.getChallengeVetoReason(c);
        if (superVeto != null) {
            return superVeto;
        }
        if (AbstractSolverService.isManualSolvingBlockedBySilentMode()) {
            return ChallengeVetoReason.SILENT_MODE;
        }
        return null;
    }

    /**
     * Has to be called before the dialog is shown. The silent mode may have been activated after the job was created (then this solver
     * was not vetoed, see {@link #getChallengeVetoReason(Challenge)}).
     *
     * @return true if the silent mode does not allow to ask the user: The solver must end without showing the dialog. This does not
     *         abort the job, the other solvers can still solve the challenge, see {@link #isAskingUserBlockedBySilentMode(SolverJob)}.
     */
    public boolean checkSilentMode(final SolverJob<T> job) throws SkipException, InterruptedException {
        if (isAskingUserBlockedBySilentMode(job)) {
            return true;
        }
        /*
         * Old silent mode handling, kept as a safety net: The cases "Skip the Captcha" and "Do not show a dialog, but try auto solver" are
         * already handled above (the dialog is not shown, the other solvers continue), so only "Wait in Background ..." can get here, which
         * just continues (the dialog handler waits until the silent mode ends or the timeout is reached). The other two cases below are
         * not reachable anymore; note that SKIP_LINK would abort the whole job (also for the other solvers), unlike the new handling.
         */
        if (JDGui.getInstance().isSilentModeActive()) {
            switch (CFG_SILENTMODE.CFG.getOnCaptchaDuringSilentModeAction()) {
            case WAIT_IN_BACKGROUND_UNTIL_WINDOW_GETS_FOCUS_OR_TIMEOUT:
                break;
            case DISABLE_DIALOG_SOLVER:
                /* Old: Show no dialog, but skip the link as soon as the job is done without a solution. Now done by the new handling. */
                job.getEventSender().addListener(new ChallengeSolverJobListener() {
                    @Override
                    public void onSolverTimedOut(ChallengeSolver<?> parameter) {
                    }

                    @Override
                    public void onSolverStarts(ChallengeSolver<?> parameter) {
                    }

                    @Override
                    public void onSolverJobReceivedNewResponse(AbstractResponse<?> response) {
                    }

                    @Override
                    public void onSolverDone(ChallengeSolver<?> solver) {
                        if (job.isDone()) {
                            if (!job.isSolved()) {
                                ChallengeResponseController.getInstance().setSkipRequest(SkipRequest.BLOCK_ALL_CAPTCHAS, AbstractDialogSolver.this, job.getChallenge());
                            }
                            job.getEventSender().removeListener(this);
                        }
                    }
                });
                return false;
            case SKIP_LINK:
                /* Old: Skips the whole link immediately (all solvers). Now the link is only skipped if no other solver solved it. */
                throw new SkipException(job.getChallenge(), SkipRequest.BLOCK_ALL_CAPTCHAS);
            }
        }
        checkInterruption();
        return false;
    }
}
