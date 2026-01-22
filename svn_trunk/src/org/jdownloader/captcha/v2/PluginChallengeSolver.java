package org.jdownloader.captcha.v2;

import java.util.List;

import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;

import jd.controlling.captcha.SkipException;
import jd.plugins.Account;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.PluginException;

/**
 * A ChallengeSolver implementation that uses a plugin account for solving challenges. This allows solvers to use account-specific
 * credentials or features.
 */
public class PluginChallengeSolver<T> extends ChallengeSolver<T> {
    protected final Account                        account;
    protected final abstractPluginForCaptchaSolver plugin;

    public PluginChallengeSolver(abstractPluginForCaptchaSolver plugin, Account account) {
        if (plugin == null || account == null) {
            throw new IllegalArgumentException();
        }
        this.service = new PluginForCaptchaSolverSolverService(plugin);
        this.account = account;
        this.plugin = plugin;
    }

    /** Returns false if the solver does not have enough balance to solve the given captcha challenge. */
    protected boolean enoughBalanceFor(final Challenge<?> c, final Account account) throws Exception {
        return plugin.enoughBalanceFor(c, account);
    }

    @Override
    public SolverType getSolverType() {
        return SolverType.EXTERNAL;
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        return plugin.getSupportedCaptchaTypes();
    }

    @Override
    public List<CAPTCHA_TYPE> getUserDisabledCaptchaTypes() {
        return this.plugin.getUserDisabledCaptchaTypes(account);
    }

    public Account getAccount() {
        return account;
    }

    @Override
    public boolean isEnabled() {
        return this.account.isEnabled();
    }

    @Override
    protected boolean validateLogins() {
        // TODO: Remove this in the future as logins are controlled by the plugin in the future.
        // return true;
        return this.isEnabled();
    }

    @Override
    public ChallengeVetoReason getChallengeVetoReason(Challenge<?> c) {
        // TODO: Only call plugin.canHandle if there is an override or always call both
        final ChallengeVetoReason veto = plugin.getVetoReason(c, account);
        if (veto != null) {
            return veto;
        } else {
            return super.getChallengeVetoReason(c);
        }
    }

    @Override
    public boolean isFilterListEnabled() {
        return plugin.isFilterListEnabled();
    }

    @Override
    public List<CaptchaChallengeFilter> getCaptchaChallengeFilterList() {
        // TODO: Make this abstract
        return this.plugin.getCaptchaChallengeFilterList();
    }

    @Override
    public boolean isDomainBlacklistEnabled() {
        return plugin.isDomainBlacklistEnabled();
    }

    @Override
    public List<String> getBlacklistedDomains() {
        return plugin.getBlacklistedDomains(account);
    }

    @Override
    public boolean isDomainWhitelistEnabled() {
        return plugin.isDomainWhitelistEnabled();
    }

    @Override
    public List<String> getWhitelistedDomains() {
        return plugin.getWhitelistedDomains(account);
    }

    @Override
    public boolean setValid(AbstractResponse<?> response) {
        /*
         * TODO: Find a good place where to check if captcha feedback is disabled in plugin settings, then possibly don't call the feedback
         * method.
         */
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
        /*
         * TODO: Find a good place where to check if captcha feedback is disabled in plugin settings, then possibly don't call the feedback
         * method.
         */
        try {
            return this.plugin.setInvalid(response, account);
        } catch (Exception e) {
            // TODO: Handle plugin/account related exceptions
            e.printStackTrace();
            return false;
        }
    }

    public boolean setUnused(AbstractResponse<?> response) {
        /*
         * TODO: Find a good place where to check if captcha feedback is disabled in plugin settings, then possibly don't call the feedback
         * method.
         */
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