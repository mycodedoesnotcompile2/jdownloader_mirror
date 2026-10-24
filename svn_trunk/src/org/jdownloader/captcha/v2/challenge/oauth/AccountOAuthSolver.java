package org.jdownloader.captcha.v2.challenge.oauth;

import java.util.EnumSet;
import java.util.Set;
import java.util.ArrayList;
import java.util.List;

import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.solver.jac.JACSolver;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solverjob.SolverJob;

import jd.controlling.captcha.SkipException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class AccountOAuthSolver extends ChallengeSolver<Boolean> {
    private static final AccountOAuthSolver INSTANCE = new AccountOAuthSolver();

    public static AccountOAuthSolver getInstance() {
        return INSTANCE;
    }

    @Override
    public ChallengeVetoReason getUnsupportedReason(Challenge<?> c) {
        /* Super class is not asked on purpose: It vetoes solvers without supported captcha types, but OAuth challenges are no captchas. */
        if (!(c instanceof OAuthChallenge)) {
            return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER;
        }
        /* No veto = Given challenge can be solved by this solver. */
        return null;
    }

    @Override
    public boolean isEnabled() {
        // I see no reason why we should disable it.
        return true;
    }

    @Override
    public Set<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        /* OAuth challenges are no captchas and have no CAPTCHA_TYPE, so this solver legitimately supports no captcha type. */
        return EnumSet.noneOf(CAPTCHA_TYPE.class);
    }

    private AccountOAuthSolver() {
        super(JACSolver.getInstance().getService(), 1);
    }

    @Override
    public void solve(SolverJob<Boolean> job) throws InterruptedException, SolverException, SkipException {
        if (job.getChallenge() instanceof OAuthChallenge) {
            final OAuthChallenge challenge = ((OAuthChallenge) job.getChallenge());
            try {
                if (challenge instanceof AccountLoginOAuthChallenge) {
                    if (((AccountLoginOAuthChallenge) challenge).autoSolveChallenge(job)) {
                        job.addAnswer(new AbstractResponse<Boolean>(challenge, this, true));
                        return;
                    }
                }
            } catch (Throwable e) {
                job.getLogger().log(e);
                challenge.sendStatsError(this, e);
            }
        }
    }
}
