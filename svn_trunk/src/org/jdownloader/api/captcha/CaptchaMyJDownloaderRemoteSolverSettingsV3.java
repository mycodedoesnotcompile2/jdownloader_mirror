package org.jdownloader.api.captcha;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
import org.jdownloader.plugins.config.Order;

public interface CaptchaMyJDownloaderRemoteSolverSettingsV3 extends CaptchaSolverConfigV3 {
    /* Overrides the default of the base config. The annotations of the overridden method are not inherited and have to be repeated. */
    @AboutConfig
    @DescriptionForConfigEntry("How long (in seconds) this solver may work on a captcha without a solution before the next solver (see order in the solver table) is started in parallel to it. 0 = start the next solver immediately. If this solver is done without a solution, the next solver is started right away.")
    @SpinnerValidator(min = 0, max = SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_MAX, step = 1)
    @DefaultIntValue(SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_JD_REMOTE_MYJD)
    @Order(900)
    int getStartNextSolverAfterSeconds();
}
