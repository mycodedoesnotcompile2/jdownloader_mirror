package org.jdownloader.captcha.v2.solver.jac;

import java.awt.Image;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;

import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.logging2.LogSource;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.logging.LogController;

import jd.captcha.JACMethod;
import jd.captcha.JAntiCaptcha;
import jd.captcha.LetterComperator;
import jd.captcha.pixelgrid.Captcha;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class JACSolver extends ChallengeSolver<String> {
    private static final double              THRESHOLD_DEFAULT = 0.85;
    private final JacSolverConfigV3          config;
    private static final JACSolver           INSTANCE          = new JACSolver();
    private final HashMap<String, AutoTrust> threshold;
    private final LogSource                  logger;

    /**
     * get the only existing instance of JACSolver. This is a singleton
     *
     * @return
     */
    public static JACSolver getInstance() {
        return JACSolver.INSTANCE;
    }

    /**
     * Create a new instance of JACSolver. This is a singleton class. Access the only existing instance by using {@link #getInstance()}.
     */
    @Override
    public JacSolverService getService() {
        return (JacSolverService) super.getService();
    }

    private JACSolver() {
        super(new JacSolverService(), 5);
        config = JsonConfig.create(JacSolverConfigV3.class);
        logger = LogController.getInstance().getLogger(JACSolver.class.getName());
        final HashMap<String, AutoTrust> threshold = config.getJACThreshold();
        if (threshold == null) {
            this.threshold = new HashMap<String, AutoTrust>();
        } else {
            this.threshold = new HashMap<String, AutoTrust>(threshold);
        }
        ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
            @Override
            public void onShutdown(ShutdownRequest shutdownRequest) {
                config.setJACThreshold(JACSolver.this.threshold);
            }
        });
    }

    @Override
    public long getTimeoutMillis() {
        return 30000;
    }

    /**
     * JAC can only solve plain image captchas (see {@link #solve(SolverJob)}: only {@link BasicCaptchaChallenge} is handled), NOT the click
     * captcha types.
     */
    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        return types;
    }

    /**
     * Rejects every challenge JAC cannot solve as early as possible, so that JAC is not even added to the solver list of the job. The
     * conditions are the ones {@link #solve(SolverJob)} used to check itself (and silently return without an answer on failure).
     */
    @Override
    public ChallengeVetoReason getChallengeVetoReason(final Challenge<?> c) {
        if (!(c instanceof BasicCaptchaChallenge)) {
            /* Only plain text image captchas are supported. */
            return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER;
        }
        /* JAC can only solve captchas it has a method (trained data) for. The result is cached, so no disk access on every call. */
        if (!JACMethod.hasMethod(c.getTypeID())) {
            return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER;
        }
        return super.getChallengeVetoReason(c);
    }

    @Override
    public void enqueue(SolverJob<String> job) {
        if (getChallengeVetoReason(job.getChallenge()) == null) {
            super.enqueue(job);
        }
    }

    @Override
    public void solve(SolverJob<String> job) throws InterruptedException, SolverException {
        /* Challenge is guaranteed to be a BasicCaptchaChallenge with an existing JAC method, see getChallengeVetoReason(). */
        final BasicCaptchaChallenge captchaChallenge = (BasicCaptchaChallenge) job.getChallenge();
        try {
            job.getLogger().info("JACSolver handles " + job);
            job.getChallenge().sendStatsSolving(this);
            checkInterruption();
            final JAntiCaptcha jac = new JAntiCaptcha(captchaChallenge.getTypeID());
            checkInterruption();
            final Image captchaImage = ImageProvider.read(captchaChallenge.getImageFile());
            checkInterruption();
            final Captcha captcha = jac.createCaptcha(captchaImage);
            checkInterruption();
            final String captchaCode = jac.checkCaptcha(captchaChallenge.getImageFile(), captcha);
            if (StringUtils.isEmpty(captchaCode)) {
                return;
            }
            if (jac.isExtern()) {
                /* external captchaCode Response */
                job.addAnswer(new CaptchaResponse(captchaChallenge, this, captchaCode));
            } else {
                /* internal captchaCode Response */
                final LetterComperator[] lcs = captcha.getLetterComperators();
                double vp = 0.0;
                if (lcs != null && lcs.length > 0) {
                    for (final LetterComperator element : lcs) {
                        if (element == null) {
                            vp = 0;
                            break;
                        }
                        vp += element.getValityPercent();
                    }
                    vp /= lcs.length;
                }
                int trust = 120 - (int) vp;
                final int orgTrust = trust;
                synchronized (threshold) {
                    final AutoTrust trustValue = threshold.get(getTrustID(captchaChallenge));
                    if (trustValue != null && trust > trustValue.getValue() * THRESHOLD_DEFAULT) {
                        trust = 100;
                    }
                }
                job.addAnswer(new JACCaptchaResponse(captchaChallenge, this, captchaCode, trust, orgTrust));
            }
        } catch (IOException e) {
            job.getChallenge().sendStatsError(this, e);
            throw new SolverException(e);
        }
    }

    /** Returns the id under which the dynamic trust threshold is stored: "host_captchatypeid". */
    private static String getTrustID(final Challenge<?> challenge) {
        return (challenge.getHost() + "_" + challenge.getTypeID()).toLowerCase(Locale.ENGLISH);
    }

    @Override
    public boolean setValid(AbstractResponse<?> response) {
        if (response.getSolver() != this) {
            return false;
        }
        if (response instanceof JACCaptchaResponse) {
            final int priority = ((JACCaptchaResponse) response).getUnmodifiedTrustValue();
            final Challenge<?> challenge = response.getChallenge();
            if (challenge instanceof BasicCaptchaChallenge) {
                final String trustID = getTrustID(challenge);
                synchronized (threshold) {
                    AutoTrust trustValue = threshold.get(trustID);
                    if (trustValue == null) {
                        trustValue = new AutoTrust(priority);
                        threshold.put(trustID, trustValue);
                    }
                    trustValue.add(priority);
                    logger.info("New JAC Threshold for " + trustID + " : " + trustValue.getValue() + "(" + trustValue.getCounter() + ")");
                }
            }
        }
        return true;
    }

    @Override
    public boolean setInvalid(AbstractResponse<?> response) {
        if (!(response instanceof JACCaptchaResponse)) {
            return false;
        }
        final int priority = ((JACCaptchaResponse) response).getUnmodifiedTrustValue();
        final Challenge<?> challenge = response.getChallenge();
        if (challenge instanceof BasicCaptchaChallenge) {
            final String trustID = getTrustID(challenge);
            synchronized (threshold) {
                final AutoTrust trustValue = threshold.get(trustID);
                if (trustValue != null) {
                    logger.info("JAC Failure for " + trustID + "; : TrustValue " + priority + "; Dynamic Trust: " + trustValue.getValue() + "(" + trustValue.getCounter() + ") Detected: " + response.getValue());
                    // increase trustValue!
                    trustValue.add((int) (priority * (1d + (1d - THRESHOLD_DEFAULT) * 2)));
                    logger.info("New JAC Threshold for " + trustID + " : " + trustValue.getValue() + "(" + trustValue.getCounter() + ")");
                }
            }
        }
        return true;
    }
}
