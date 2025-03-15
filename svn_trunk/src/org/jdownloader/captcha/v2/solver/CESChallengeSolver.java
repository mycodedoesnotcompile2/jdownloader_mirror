package org.jdownloader.captcha.v2.solver;

import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.plugins.SkipReason;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;

import jd.SecondLevelLaunch;
import jd.controlling.captcha.CaptchaSettings;
import jd.gui.swing.jdgui.components.premiumbar.ServicePanel;
import jd.http.Browser;

public abstract class CESChallengeSolver<T> extends ChallengeSolver<T> {
    protected final static CaptchaSettings SETTINGS = JsonConfig.create(CaptchaSettings.class);

    protected int getDefaultWaitForOthersTimeout() {
        return 60000;
    }

    public CESChallengeSolver(int threadCount) {
        super(null, threadCount);
    }

    public CESChallengeSolver(SolverService service, int threadCount) {
        super(service, threadCount);
    }

    protected boolean isAccountLoginSupported(Challenge<?> c) {
        return !c.isAccountLogin() || SETTINGS.isCaptchaExchangeForAccountLoginEnabled();
    }

    public boolean canHandle(Challenge<?> c) {
        return isAccountLoginSupported(c) && super.canHandle(c);
    }

    public FEATURE[] getFeatures() {
        // Implementation would depend on the hoster and its capabilities
        // This is a placeholder implementation
        // return new FEATURE[] { FEATURE.API_KEY_LOGIN };
        return null;
    }

    final public void solve(final SolverJob<T> job) throws InterruptedException, SolverException {
        if (!validateLogins()) {
            return;
        } else if (!isEnabled()) {
            return;
        } else if (!canHandle(job.getChallenge())) {
            return;
        }
        checkInterruption();
        CESSolverJob<T> cesJob = new CESSolverJob<T>(job);
        try {
            solveCES(cesJob);
        } finally {
            cesJob.hideBubble();
        }
    }
    /**
     * Enum representing different types of captchas. <br>
     * Mockup code
     */
    // public enum CAPTCHA_TYPE {
    // IMAGE,
    // RECAPTCHA_V2,
    // HCAPTCHA,
    // FUNCAPTCHA,
    // GEETEST,
    // KEYCAPTCHA
    // }

    /** Placeholder / mockup code */
    // public java.util.List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
    // java.util.List<CAPTCHA_TYPE> types = new java.util.ArrayList<CAPTCHA_TYPE>();
    // types.add(CAPTCHA_TYPE.IMAGE);
    // return types;
    // }
    protected Browser createNewBrowserInstance() {
        return new Browser();
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

    public boolean hasFeature(final LazyPlugin.FEATURE feature) {
        if (feature == null) {
            return false;
        }
        final LazyPlugin.FEATURE[] features = getFeatures();
        if (features == null) {
            return false;
        }
        for (int i = 0; i < features.length; i++) {
            if (features[i] == feature) {
                return true;
            }
        }
        return false;
    }
}
