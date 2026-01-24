package org.jdownloader.captcha.v2.solver;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.plugins.SkipReason;

import jd.SecondLevelLaunch;
import jd.gui.swing.jdgui.components.premiumbar.ServicePanel;
import jd.http.Browser;
import jd.plugins.Plugin;

/* TODO: Delete this once transition to new plugin based external captcha solvers is done. */
public abstract class CESChallengeSolver<T> extends ChallengeSolver<T> {
    // protected final static CaptchaSettings SETTINGS = JsonConfig.create(CaptchaSettings.class);
    protected int getDefaultWaitForOthersTimeout() {
        return 60000;
    }

    public CESChallengeSolver(int threadCount) {
        super(null, threadCount);
    }

    public CESChallengeSolver(SolverService service, int threadCount) {
        super(service, threadCount);
    }

    protected abstract LogSource getLogger();

    @Override
    public boolean isEnabled() {
        return super.isEnabled() && this.validateLogins();
    }

    final public void solve(final SolverJob<T> job) throws InterruptedException, SolverException {
        if (!validateLogins()) {
            return;
        } else if (!isEnabled()) {
            return;
        } else if (getChallengeVetoReason(job.getChallenge()) != null) {
            return;
        }
        checkInterruption();
        final CESSolverJob<T> cesJob = new CESSolverJob<T>(job);
        try {
            solveCES(cesJob);
        } finally {
            cesJob.hideBubble();
        }
    }

    protected Browser createNewBrowserInstance(final Challenge<?> challenge) {
        final Browser br = new Browser();
        LogInterface logger = null;
        final Plugin plugin;
        if (challenge != null && (plugin = challenge.getPlugin()) != null) {
            logger = plugin.getLogger();
        }
        if (logger == null) {
            logger = getLogger();
        }
        if (logger != null) {
            br.setLogger(logger);
        }
        br.setDebug(true);
        br.setVerbose(true);
        return br;
    }

    protected void solveCES(CESSolverJob<T> job) throws InterruptedException, SolverException {
        Challenge<?> challenge = job.getChallenge();
        if (challenge instanceof RecaptchaV2Challenge) {
            challenge = ((RecaptchaV2Challenge) challenge).createBasicCaptchaChallenge(true);
            if (challenge == null) {
                throw new SolverException(SkipReason.PHANTOM_JS_MISSING.getExplanation(null));
            }
        }
        solveBasicCaptchaChallenge(job, (BasicCaptchaChallenge) challenge);
    }

    protected abstract void solveBasicCaptchaChallenge(CESSolverJob<T> job, BasicCaptchaChallenge challenge) throws InterruptedException, SolverException;

    /** Override this to only return true if login looks to be valid. */
    protected boolean validateLogins() {
        return false;
    }

    protected void initServicePanel(final KeyHandler... handlers) {
        if (org.appwork.utils.Application.isHeadless()) {
            return;
        }
        SecondLevelLaunch.GUI_COMPLETE.executeWhenReached(new Runnable() {
            @SuppressWarnings("unchecked")
            public void run() {
                for (KeyHandler k : handlers) {
                    k.getEventSender().addListener(new GenericConfigEventListener<Object>() {
                        @Override
                        public void onConfigValidatorError(KeyHandler<Object> keyHandler, Object invalidValue, ValidationException validateException) {
                        }

                        @Override
                        public void onConfigValueModified(KeyHandler<Object> keyHandler, Object newValue) {
                            ServicePanel.getInstance().requestUpdate(true);
                        }
                    });
                }
            }
        });
    }

    public String getAccountStatusString() {
        return null;
    }

    /** Override if API key login is used for this solvers' account functionality. */
    protected boolean looksLikeValidAPIKey(final String str) {
        return false;
    }
}
