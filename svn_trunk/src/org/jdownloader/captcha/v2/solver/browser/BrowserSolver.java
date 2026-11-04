package org.jdownloader.captcha.v2.solver.browser;

import java.util.EnumSet;
import java.util.Set;

import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.solver.gui.DialogBasicCaptchaSolver;
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
import org.jdownloader.captcha.v2.solver.service.BrowserSolverService;
import org.jdownloader.captcha.v2.solverjob.ChallengeSolverJobListener;
import org.jdownloader.captcha.v2.solverjob.ResponseList;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.settings.advanced.AdvancedConfigManager;
import org.jdownloader.settings.staticreferences.CFG_SILENTMODE;

import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.gui.swing.jdgui.JDGui;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class BrowserSolver extends ChallengeSolver<String> {
    private static final BrowserSolver INSTANCE = new BrowserSolver();

    public static BrowserSolver getInstance() {
        return INSTANCE;
    }

    protected final BrowserCaptchaSolverConfigV3 config;
    private volatile BrowserDialogHandler         handler;

    private BrowserSolver() {
        super(BrowserSolverService.getInstance(), 1);
        config = BrowserSolverService.getInstance().getConfig();
        threadPool.allowCoreThreadTimeOut(true);
        AdvancedConfigManager.getInstance().register(BrowserSolverService.getInstance().getConfig());
    }

    @Override
    public Class<String> getResultType() {
        return String.class;
    }

    @Override
    public ChallengeVetoReason getUnsupportedReason(Challenge<?> c) {
        /* Always ask the super class first, then check the solver specific things. */
        final ChallengeVetoReason superVeto = super.getUnsupportedReason(c);
        if (superVeto != null) {
            return superVeto;
        }
        if (isSpecialReCaptchaEnterpriseChallenge(c)) {
            /* Special return false */
            return ChallengeVetoReason.UNSUPPORTED_FOR_INTERNAL_SPECIAL_REASONS;
        } else if (!BrowserSolverService.getInstance().isOpenBrowserSupported()) {
            return ChallengeVetoReason.UNSUPPORTED_BROWSER_NO_URL_OPEN;
        } else if (BrowserSolverService.getInstance().isCustomBrowserMissing()) {
            /* The user configured a custom browser which does not exist (anymore). Do not silently fall back to the OS default browser. */
            return ChallengeVetoReason.UNSUPPORTED_BROWSER_NOT_FOUND;
        }
        /* No veto = Given challenge can be solved by this solver. */
        return null;
    }

    /** Silent mode with "Skip the Captcha" or "Do not show a dialog, but try auto solver": The user cannot be asked, so no browser. */
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

    @Override
    public Set<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        return EnumSet.of(CAPTCHA_TYPE.RECAPTCHA_V3, CAPTCHA_TYPE.RECAPTCHA_V2, CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE, CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE, CAPTCHA_TYPE.HCAPTCHA);
    }

    public static boolean isSpecialReCaptchaEnterpriseChallenge(final Challenge<?> c) {
        if (c instanceof RecaptchaV2Challenge && ((RecaptchaV2Challenge) c).isEnterprise() && "filer.net".equals(c.getHost())) {
            // TODO: current version of the browser extension doesn't support special version of Recaptcha Enterprise used by filer.net
            return true;
        }
        return false;
    }

    /**
     * Has to be called before the browser is opened. The silent mode may have been activated after the job was created (then this solver
     * was not vetoed, see {@link #getChallengeVetoReason(Challenge)}).
     *
     * @return true if the silent mode does not allow to ask the user: The solver must end without opening the browser. This does not
     *         abort the job, the other solvers can still solve the challenge, see {@link #isAskingUserBlockedBySilentMode(SolverJob)}.
     */
    public boolean checkSilentMode(final SolverJob<String> job) throws SkipException, InterruptedException {
        if (isAskingUserBlockedBySilentMode(job)) {
            return true;
        }
        /*
         * Old silent mode handling, kept as a safety net: The cases "Skip the Captcha" and "Do not show a dialog, but try auto solver" are
         * already handled above (the browser is not opened, the other solvers continue), so only "Wait in Background ..." can get here,
         * which just continues (the dialog handler waits until the silent mode ends or the timeout is reached). The other two cases below
         * are not reachable anymore; note that SKIP_LINK would abort the whole job (also for the other solvers), unlike the new handling.
         */
        if (JDGui.getInstance().isSilentModeActive()) {
            switch (CFG_SILENTMODE.CFG.getOnCaptchaDuringSilentModeAction()) {
            case WAIT_IN_BACKGROUND_UNTIL_WINDOW_GETS_FOCUS_OR_TIMEOUT:
                break;
            case DISABLE_DIALOG_SOLVER:
                /* Old: Do not open the browser, but skip the link as soon as the job is done without a solution. Now done above. */
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
                                ChallengeResponseController.getInstance().setSkipRequest(SkipRequest.BLOCK_ALL_CAPTCHAS, BrowserSolver.this, job.getChallenge());
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

    @Override
    public void solve(final SolverJob<String> job) throws InterruptedException, SkipException {
        synchronized (DialogBasicCaptchaSolver.getInstance()) {
            if (job.isDone()) {
                return;
            }
            if (job.getChallenge() instanceof AbstractBrowserChallenge) {
                ChallengeSolverJobListener jacListener = null;
                if (checkSilentMode(job)) {
                    /* Silent mode: Do not ask the user, the other solvers continue. */
                    return;
                }
                AbstractBrowserChallenge captchaChallenge = (AbstractBrowserChallenge) job.getChallenge();
                // we do not need another queue
                handler = new BrowserDialogHandler(captchaChallenge);
                job.getEventSender().addListener(jacListener = new ChallengeSolverJobListener() {
                    @Override
                    public void onSolverTimedOut(ChallengeSolver<?> parameter) {
                    }

                    @Override
                    public void onSolverStarts(ChallengeSolver<?> parameter) {
                    }

                    @Override
                    public void onSolverJobReceivedNewResponse(AbstractResponse<?> response) {
                        final ResponseList<String> resp = job.getResponse();
                        final BrowserDialogHandler hndlr = handler;
                        if (hndlr != null && resp != null) {
                            hndlr.setSuggest(resp.getValue());
                            job.getLogger().info("Received Suggestion: " + resp);
                        }
                    }

                    @Override
                    public void onSolverDone(ChallengeSolver<?> solver) {
                    }
                });
                try {
                    final ResponseList<String> resp = job.getResponse();
                    if (resp != null) {
                        handler.setSuggest(resp.getValue());
                    }
                    checkInterruption();
                    handler.run();
                    if (handler.isSilentModeTimeout()) {
                        /* The browser dialog was never shown because of the silent mode: Only this solver ends, the other solvers continue. */
                        endBecauseOfSilentMode(job, "The dialog was not shown because of the silent mode");
                        return;
                    }
                    if (handler.getExternalSolverToUse() != null) {
                        /* The user chose an external solver in the dialog: No answer from this solver, the chosen solver takes over. */
                        endBecauseUserChoseExternalSolver(job, handler.getExternalSolverToUse());
                        return;
                    }
                    final String response = handler.getResult();
                    if (response != null) {
                        job.addAnswer(new BrowserResponse(captchaChallenge, this, response));
                    }
                } finally {
                    job.getLogger().info("Dialog closed. Response far: " + job.getResponse());
                    if (jacListener != null) {
                        job.getEventSender().removeListener(jacListener);
                    }
                    handler = null;
                }
            }
        }
    }

    public void requestFocus(Challenge<?> challenge) {
        final BrowserDialogHandler hndlr = handler;
        if (hndlr != null) {
            hndlr.requestFocus();
        }
    }
}
