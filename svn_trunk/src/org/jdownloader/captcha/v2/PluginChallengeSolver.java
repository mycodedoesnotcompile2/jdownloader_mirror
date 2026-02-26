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
        final ChallengeResponseController crp = ChallengeResponseController.getInstance();
        SolverService svs = ChallengeResponseController.getInstance().getServiceByID(plugin.getHost());
        if (svs == null) {
            // TODO: Check if this is necessary
            svs = new PluginForCaptchaSolverSolverService(plugin);
            crp.addSolverService(svs);
        }
        this.service = svs;
        this.account = account;
        this.plugin = plugin;
    }

    @Override
    public boolean isEnabled() {
        return this.service.isEnabled() && this.account.isEnabled();
    }

    public Account getAccount() {
        return account;
    }

    @Override
    public SolverType getSolverType() {
        return SolverType.EXTERNAL;
    }

    @Override
    public List<FeedbackType> getSupportedFeedbackTypes() {
        return plugin.getSupportedFeedbackTypes();
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        return plugin.getSupportedCaptchaTypes(account);
    }

    @Override
    public List<CAPTCHA_TYPE> getUserDisabledCaptchaTypes() {
        return this.plugin.getUserDisabledCaptchaTypes(account);
    }

    @Override
    public ChallengeVetoReason getChallengeVetoReason(Challenge<?> c) {
        final ChallengeVetoReason veto = plugin.getVetoReason(c, account);
        if (veto != null) {
            return veto;
        }
        final List<CAPTCHA_TYPE> all_supported_captcha_types = this.plugin.getSupportedCaptchaTypes();
        final List<CAPTCHA_TYPE> account_supported_captcha_types = this.plugin.getSupportedCaptchaTypes(this.account);
        final CAPTCHA_TYPE ctype = CAPTCHA_TYPE.getCaptchaTypeForChallenge(c);
        if (ctype != null && all_supported_captcha_types != null && account_supported_captcha_types != null && all_supported_captcha_types.contains(ctype) && !account_supported_captcha_types.contains(ctype)) {
            return ChallengeVetoReason.UNSUPPORTED_BY_SOLVER_ACCOUNT;
        }
        return super.getChallengeVetoReason(c);
    }

    @Override
    public boolean isFilterListEnabled() {
        return plugin.isFilterListEnabled();
    }

    @Override
    public List<CaptchaChallengeFilter> getCaptchaChallengeFilterList() {
        return this.plugin.getCaptchaChallengeFilterList();
    }

    @Override
    public boolean setValid(AbstractResponse<?> response) {
        final List<FeedbackType> feedbacktypes = this.getSupportedFeedbackTypes();
        if (feedbacktypes != null && !feedbacktypes.contains(FeedbackType.REPORT_VALID_CAPTCHAS)) {
            /* Feedback type is not supported by plugin. */
            return false;
        }
        if (!plugin.isEnableCaptchaFeedback(account)) {
            /* Feedback is disabled by user */
            return false;
        }
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
        final List<FeedbackType> feedbacktypes = this.getSupportedFeedbackTypes();
        if (feedbacktypes != null && !feedbacktypes.contains(FeedbackType.REPORT_INVALID_CAPTCHAS)) {
            /* Feedback type is not supported by plugin. */
            return false;
        }
        if (!plugin.isEnableCaptchaFeedback(account)) {
            /* Feedback is disabled by user */
            return false;
        }
        try {
            return this.plugin.setInvalid(response, account);
        } catch (Exception e) {
            // TODO: Handle plugin/account related exceptions
            e.printStackTrace();
            return false;
        }
    }

    @Override
    public boolean setUnused(AbstractResponse<?> response) {
        final List<FeedbackType> feedbacktypes = this.getSupportedFeedbackTypes();
        if (feedbacktypes != null && !feedbacktypes.contains(FeedbackType.ABORT_CAPTCHAS)) {
            /* Feedback type is not supported by plugin. */
            return false;
        }
        if (!plugin.isEnableCaptchaFeedback(account)) {
            /* Feedback is disabled by user */
            return false;
        }
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
            plugin.setCurrentCaptchaChallenge(job.getChallenge());
            plugin.solve(cesJob, account);
        } catch (final PluginException e) {
            // TODO: Set detailed failure feedback on SolverJob e.g. if failure was account related.
            plugin.handleAccountException(account, plugin.getLogger(), e);
        } catch (Exception e) {
            // TODO
            e.printStackTrace();
        } finally {
            cesJob.hideBubble();
            // TODO: Check if it's fine to nullify here
            plugin.setCurrentCaptchaChallenge(null);
        }
    }
}