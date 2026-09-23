package org.jdownloader.captcha.v2.solver.gui;

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.config.JsonConfig;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickedPoint;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ClickCaptchaResponse;
import org.jdownloader.captcha.v2.solver.jac.JACSolver;
import org.jdownloader.captcha.v2.solver.service.DialogSolverService;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.settings.advanced.AdvancedConfigManager;

import jd.controlling.captcha.CaptchaSettings;
import jd.controlling.captcha.ClickCaptchaDialogHandler;
import jd.controlling.captcha.SkipException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class DialogClickCaptchaSolver extends AbstractDialogSolver<ClickedPoint> {
    private CaptchaSettings           config;
    private ClickCaptchaDialogHandler handler;

    private DialogClickCaptchaSolver() {
        super(1);
        config = JsonConfig.create(CaptchaSettings.class);
        AdvancedConfigManager.getInstance().register(DialogSolverService.getInstance().getConfigV3());
    }

    private static final DialogClickCaptchaSolver INSTANCE = new DialogClickCaptchaSolver();

    public static DialogClickCaptchaSolver getInstance() {
        return INSTANCE;
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        return types;
    }

    @Override
    public ChallengeVetoReason getChallengeVetoReason(Challenge<?> c) {
        if (c instanceof ClickCaptchaChallenge) {
            /* Looks good -> Let upper handling decide for VetoReason */
            return super.getChallengeVetoReason(c);
        } else {
            return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER;
        }
    }

    public void requestFocus(Challenge<?> challenge) {
        ClickCaptchaDialogHandler hndlr = handler;
        if (hndlr != null) {
            hndlr.requestFocus();
        }
    }

    @Override
    public void solve(SolverJob<ClickedPoint> solverJob) throws InterruptedException, SkipException {
        synchronized (DialogBasicCaptchaSolver.getInstance()) {
            if (solverJob.isDone()) {
                return;
            }
            if (solverJob.getChallenge() instanceof ClickCaptchaChallenge) {
                solverJob.getLogger().info("Waiting for JAC (Click/Mouse)");
                solverJob.waitFor(9, JACSolver.getInstance());
                solverJob.getLogger().info("JAC (Click/Mouse) is done. Response so far: " + solverJob.getResponse());
                checkSilentMode(solverJob);
                ClickCaptchaChallenge captchaChallenge = (ClickCaptchaChallenge) solverJob.getChallenge();
                checkInterruption();
                handler = new ClickCaptchaDialogHandler(captchaChallenge);
                handler.run();
                final ClickedPoint result = handler.getResult();
                if (result != null) {
                    solverJob.addAnswer(new ClickCaptchaResponse(captchaChallenge, this, result));
                }
            }
        }
    }
}
