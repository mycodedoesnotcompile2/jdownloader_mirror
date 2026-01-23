package org.jdownloader.captcha.v2.solver.solver9kw;

import java.io.IOException;

import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickedPoint;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;

import jd.http.Browser;

public class Captcha9kwSolverClick extends AbstractCaptcha9kwSolver<ClickedPoint> {
    private static final Captcha9kwSolverClick INSTANCE = new Captcha9kwSolverClick();

    public static Captcha9kwSolverClick getInstance() {
        return INSTANCE;
    }

    @Override
    public Class<ClickedPoint> getResultType() {
        return ClickedPoint.class;
    }

    private Captcha9kwSolverClick() {
        super();
        NineKwSolverService.getInstance().setClickSolver(this);
    }

    @Override
    public boolean isEnabled() {
        return config.ismouse() && config.isEnabledGlobally();
    }

    @Override
    public ChallengeVetoReason getChallengeVetoReason(Challenge<?> c) {
        if (c instanceof ClickCaptchaChallenge) {
            return super.getChallengeVetoReason(c);
        } else {
            return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER;
        }
    }

    @Override
    protected void solveCES(CESSolverJob<ClickedPoint> solverJob) throws InterruptedException, SolverException {
        checkInterruption();
        ClickCaptchaChallenge captchaChallenge = (ClickCaptchaChallenge) solverJob.getChallenge();
        RequestOptions options = prepare(solverJob);
        if (options.getMoreoptions().containsKey("userconfirm")) {
            options.getMoreoptions().remove("userconfirm");
        } else {
            options.setConfirm(config.ismouseconfirm());
        }
        try {
            final byte[] data = IO.readFile(captchaChallenge.getImageFile());
            UrlQuery qi = createQueryForUpload(solverJob, options, data).appendEncoded("mouse", "1");
            UrlQuery queryPoll = createQueryForPolling().appendEncoded("mouse", "1");
            Browser br = createNewBrowserInstance(captchaChallenge);
            br.setAllowedResponseCodes(new int[] { 500 });
            String captchaID = upload(br, solverJob, qi);
            poll(br, options, solverJob, captchaID, queryPoll);
        } catch (IOException e) {
            solverJob.getChallenge().sendStatsError(this, e);
            setdebug(solverJob, "Interrupted: " + e);
            counterInterrupted.incrementAndGet();
            solverJob.getLogger().log(e);
        }
    }

    @Override
    protected boolean validateLogins() {
        return StringUtils.isNotEmpty(config.getApiKey()) && config.ismouse();
    }

    @Override
    protected void parseResponse(CESSolverJob<ClickedPoint> solverJob, Challenge<ClickedPoint> captchaChallenge, String captchaID, String antwort) {
        String[] splitResult = antwort.split("x");
        solverJob.setAnswer(new Captcha9kwClickResponse(captchaChallenge, this, new ClickedPoint(Integer.parseInt(splitResult[0]), Integer.parseInt(splitResult[1])), captchaID));
    }
}
