package org.jdownloader.captcha.v2;

import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;

import jd.controlling.captcha.SkipException;
import jd.plugins.Account;
import jd.plugins.PluginException;

/**
 * A ChallengeSolver implementation that uses a plugin account for solving challenges. This allows solvers to use account-specific
 * credentials or features.
 */
public class PluginChallengeSolver<T> extends ChallengeSolver<T> {
    protected final Account                        account;
    protected final abstractPluginForCaptchaSolver plugin;

    public PluginChallengeSolver(abstractPluginForCaptchaSolver plugin, Account account, SolverService solverService) {
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
    public boolean setValid(AbstractResponse<?> response) {
        try {
            return this.plugin.setValid(response, account);
        } catch (Exception e) {
            // TODO: Handle plugin/account related exceptions
            e.printStackTrace();
            return false;
        }
    }

    @Override
    public boolean setInvalid(AbstractResponse<?> response) {
        try {
            return this.plugin.setInvalid(response, account);
        } catch (Exception e) {
            // TODO: Handle plugin/account related exceptions
            e.printStackTrace();
            return false;
        }
    }

    public boolean setUnused(AbstractResponse<?> response) {
        try {
            return this.plugin.setUnused(response, account);
        } catch (Exception e) {
            // TODO: Handle plugin/account related exceptions
            e.printStackTrace();
            return false;
        }
    }

    @Override
    public void solve(SolverJob<T> job) throws InterruptedException, SolverException, SkipException {
        final CESSolverJob<T> cesJob = new CESSolverJob<T>(job);
        try {
            plugin.solve(cesJob, account);
        } catch (final PluginException e) {
            plugin.handleAccountException(account, plugin.getLogger(), e);
        } catch (Exception e) {
            // TODO
            e.printStackTrace();
        } finally {
            cesJob.hideBubble();
        }
    }
}