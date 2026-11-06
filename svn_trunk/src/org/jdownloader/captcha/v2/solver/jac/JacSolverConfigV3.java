package org.jdownloader.captcha.v2.solver.jac;

import java.util.HashMap;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
import org.jdownloader.plugins.config.Order;

/**
 * Config of the local JAC (JAntiCaptcha) solver. Replaces the old JACSolverConfig (which was based on the deprecated
 * ChallengeSolverConfig). The JAC specific threshold values are engine internal and thus hidden from the GUI.
 */
public interface JacSolverConfigV3 extends CaptchaSolverConfigV3 {
    /* Overrides the default of the base config. The annotations of the overridden method are not inherited and have to be repeated. */
    @AboutConfig
    @DescriptionForConfigEntry("How long (in seconds) this solver may work on a captcha without a solution before the next solver (see order in the solver table) is started in parallel to it. 0 = start the next solver immediately. If this solver is done without a solution, the next solver is started right away.")
    @SpinnerValidator(min = 0, max = SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_MAX, step = 1)
    @DefaultIntValue(SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_JD_LOCAL)
    @Order(900)
    int getStartNextSolverAfterSeconds();

    @AboutConfig(inGUIVisible = false)
    @Order(10000)
    @DefaultOnNull
    HashMap<String, AutoTrust> getJACThreshold();

    void setJACThreshold(HashMap<String, AutoTrust> map);

    @AboutConfig(inGUIVisible = false)
    @DefaultIntValue(90)
    @DescriptionForConfigEntry("Do not Change me unless you know 100000% what this value is used for!")
    @Order(10001)
    int getDefaultJACTrustThreshold();

    void setDefaultJACTrustThreshold(int value);
}
