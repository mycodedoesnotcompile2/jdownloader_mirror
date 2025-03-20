package org.jdownloader.captcha.v2;

import jd.controlling.captcha.SkipException;
import jd.plugins.Account;

import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;

/**
 * A ChallengeSolver implementation that uses a plugin account for solving challenges. This allows solvers to use account-specific
 * credentials or features.
 */
public class PluginChallengeSolver<T> extends ChallengeSolver<T> {
    protected final Account                        account;
    protected final abstractPluginForCaptchaSolver plugin;

    public PluginChallengeSolver(Account account, abstractPluginForCaptchaSolver plugin, SolverService solverService) {
        super(solverService, 0);
        this.account = account;
        this.plugin = plugin;
    }

    public Account getAccount() {
        return account;
    }

    @Override
    protected boolean validateLogins() {
        return true;
    }

    @Override
    public boolean canHandle(Challenge<?> c) {
        return plugin.canHandle(c);
    }

    @Override
    public void solve(SolverJob<T> job) throws InterruptedException, SolverException, SkipException {
        final CESSolverJob<T> cesJob = new CESSolverJob<T>(job);
        try {
            plugin.solve(cesJob, account);
        } finally {
            cesJob.hideBubble();
        }
    }

}