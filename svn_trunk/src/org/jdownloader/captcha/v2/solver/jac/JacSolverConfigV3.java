package org.jdownloader.captcha.v2.solver.jac;

import java.util.HashMap;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.plugins.config.Order;

/**
 * Config of the local JAC (JAntiCaptcha) solver. Replaces the old JACSolverConfig (which was based on the deprecated
 * ChallengeSolverConfig). The JAC specific threshold values are engine internal and thus hidden from the GUI.
 */
public interface JacSolverConfigV3 extends CaptchaSolverConfigV3 {
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
