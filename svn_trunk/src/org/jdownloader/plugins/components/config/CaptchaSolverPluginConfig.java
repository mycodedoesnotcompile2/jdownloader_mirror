package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultDoubleValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.DoubleSpinnerValidator;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.plugins.config.Order;

/**
 * Base config for captcha solver plugins. In addition to the generic {@link CaptchaSolverConfigV3} settings it carries the settings which
 * only make sense for external (plugin based) captcha solver services, e.g. the low-credits warning and the API polling interval.
 */
public interface CaptchaSolverPluginConfig extends CaptchaSolverConfigV3 {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getWarnOnLowCredits_label() {
            return "Warn on low credits";
        }

        public String getLowCreditsWarningThreshold_label() {
            return "Low credits warning threshold (in currency of captcha solver service)";
        }

        public String getPollingIntervalSeconds_label() {
            return "Polling interval in seconds";
        }
    }

    @AboutConfig
    @DescriptionForConfigEntry("Display a warning when account credits fall below the specified threshold")
    @DefaultBooleanValue(true)
    @Order(300)
    boolean isWarnOnLowCredits();

    void setWarnOnLowCredits(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Minimum credit balance before warning is displayed (in currency of captcha solver service)")
    @DoubleSpinnerValidator(min = 0.1, max = 10, step = 0.1)
    @DefaultDoubleValue(0.5)
    @Order(350)
    double getLowCreditsWarningThreshold();

    void setLowCreditsWarningThreshold(double threshold);

    @AboutConfig
    @DescriptionForConfigEntry("Polling interval in seconds for captcha status checks")
    @SpinnerValidator(min = 2, max = 30, step = 1)
    @DefaultIntValue(5)
    @Order(700)
    int getPollingIntervalSeconds();

    void setPollingIntervalSeconds(int seconds);
}
