package org.jdownloader.captcha.v2.solver.browser;

import java.util.EnumSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.utils.Time;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.solver.gui.DialogBasicCaptchaSolver;
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

    /**
     * If a skip request arrives within this many milliseconds after the auto clicker released the mouse button, we assume the auto clicker
     * missed the captcha and hit one of our own skip buttons by accident.
     */
    private static final long                    AUTO_CLICK_SKIP_PROTECTION_MS = 50;
    protected final BrowserCaptchaSolverConfigV3 config;
    private volatile BrowserDialogHandler        handler;
    /* Timestamp of the last auto click (mouse release), -1 if there was never one */
    private final AtomicLong                     lastAutoClickTimestamp        = new AtomicLong(-1);

    private BrowserSolver() {
        super(BrowserSolverService.getInstance(), 1);
        config = BrowserSolverService.getInstance().getConfig();
        threadPool.allowCoreThreadTimeOut(true);
        AdvancedConfigManager.getInstance().register(BrowserSolverService.getInstance().getConfig());
    }

    /** Call right after the auto clicker released the mouse button. */
    public void markAutoClick() {
        lastAutoClickTimestamp.set(Time.systemIndependentCurrentJVMTimeMillis());
    }

    /**
     * Returns the milliseconds since the last auto click or -1 if there was never one. <br>
     * Part of workaround for auto click clicking wrong position, see: https://svn.jdownloader.org/issues/88375
     */
    public long getMillisSinceAutoClick() {
        final long last = lastAutoClickTimestamp.get();
        if (last < 0) {
            return -1;
        }
        return Time.systemIndependentCurrentJVMTimeMillis() - last;
    }

    /** Returns true if the auto clicker clicked just now, so a click on one of our own buttons was most likely a misclick. */
    public boolean hasRecentlyClicked() {
        final long passed = getMillisSinceAutoClick();
        return passed >= 0 && passed <= AUTO_CLICK_SKIP_PROTECTION_MS;
    }

    @Override
    public Class<String> getResultType() {
        return String.class;
    }

    @Override
    public ChallengeVetoReason getChallengeVetoReason(Challenge<?> c) {
        if (isSpecialReCaptchaEnterpriseChallenge(c)) {
            /* Special return false */
            return ChallengeVetoReason.UNSUPPORTED_FOR_INTERNAL_SPECIAL_REASONS;
        } else if (!BrowserSolverService.getInstance().isOpenBrowserSupported()) {
            return ChallengeVetoReason.UNSUPPORTED_BROWSER_NO_URL_OPEN;
        } else if (BrowserSolverService.getInstance().isCustomBrowserMissing()) {
            /* The user configured a custom browser which does not exist (anymore). Do not silently fall back to the OS default browser. */
            return ChallengeVetoReason.UNSUPPORTED_BROWSER_NOT_FOUND;
        } else {
            return super.getChallengeVetoReason(c);
        }
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

    public void checkSilentMode(final SolverJob<String> job) throws SkipException, InterruptedException {
        if (JDGui.getInstance().isSilentModeActive()) {
            switch (CFG_SILENTMODE.CFG.getOnCaptchaDuringSilentModeAction()) {
            case WAIT_IN_BACKGROUND_UNTIL_WINDOW_GETS_FOCUS_OR_TIMEOUT:
                break;
            case DISABLE_DIALOG_SOLVER:
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
                                ChallengeResponseController.getInstance().setSkipRequest(SkipRequest.SINGLE, BrowserSolver.this, job.getChallenge());
                            }
                            job.getEventSender().removeListener(this);
                        }
                    }
                });
                return;
            case SKIP_LINK:
                throw new SkipException(job.getChallenge(), SkipRequest.SINGLE);
            }
        }
        checkInterruption();
    }

    @Override
    public void solve(final SolverJob<String> job) throws InterruptedException, SkipException {
        synchronized (DialogBasicCaptchaSolver.getInstance()) {
            if (job.isDone()) {
                return;
            }
            if (job.getChallenge() instanceof AbstractBrowserChallenge) {
                ChallengeSolverJobListener jacListener = null;
                checkSilentMode(job);
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
