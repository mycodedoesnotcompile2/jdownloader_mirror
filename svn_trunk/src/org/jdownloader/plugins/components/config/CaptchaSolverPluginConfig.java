package org.jdownloader.plugins.components.config;

import java.util.ArrayList;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultDoubleValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.DoubleSpinnerValidator;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.captcha.v2.CaptchaSolverLimitRule;
import org.jdownloader.plugins.config.Order;

/**
 * Base config for captcha solver plugins. In addition to the generic {@link CaptchaSolverConfigV3} settings it carries the settings which
 * only make sense for external (plugin based) captcha solvers, e.g. the low-credits warning and the API polling interval.
 */
public interface CaptchaSolverPluginConfig extends CaptchaSolverConfigV3 {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getWarnOnLowCredits_label() {
            return "Warn on low credits (notification after every account check)";
        }

        public String getLowCreditsWarningThreshold_label() {
            return "Low credits warning threshold (in currency of the external captcha solver)";
        }

        public String getPollingIntervalSeconds_label() {
            return "Polling interval in seconds";
        }
    }

    @AboutConfig
    @DescriptionForConfigEntry("Warn when the credits of an account fall below the specified threshold. After each account check, the account status text shows a hint while the credits are low, and a notification is shown after every account check. The notification has a button to hide it for the account until JDownloader is restarted.")
    @DefaultBooleanValue(true)
    @Order(300)
    boolean isWarnOnLowCredits();

    void setWarnOnLowCredits(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Credit balance below which the low credits warning is triggered (in currency of the external captcha solver). Only used if 'Warn on low credits' is enabled.")
    @DoubleSpinnerValidator(min = 0.1, max = 10, step = 0.1)
    @DefaultDoubleValue(0.5)
    @Order(350)
    double getLowCreditsWarningThreshold();

    void setLowCreditsWarningThreshold(double threshold);

    @AboutConfig
    @DescriptionForConfigEntry("Polling interval in seconds for captcha status checks")
    @SpinnerValidator(min = 1, max = 15, step = 1)
    @DefaultIntValue(5)
    @Order(700)
    int getPollingIntervalSeconds();

    void setPollingIntervalSeconds(int seconds);

    @AboutConfig(inGUIVisible = false)
    @DescriptionForConfigEntry("Enable the custom limit rules (see getLimitRules). If disabled, all rules are ignored.")
    @DefaultBooleanValue(false)
    @Order(9000)
    boolean isCustomLimitsEnabled();

    void setCustomLimitsEnabled(boolean b);

    @AboutConfig(inGUIVisible = false)
    @DescriptionForConfigEntry("Custom limit rules (max captchas per time interval). null = never initialized (example rules are created when custom limits get enabled for the first time), empty = deliberately cleared by the user.")
    /*
     * The default is deliberately null and NOT an empty list: an empty list would mean "cleared by the user", so no example rules would
     * ever be created.
     */
    @Order(9001)
    ArrayList<CaptchaSolverLimitRule> getLimitRules();

    void setLimitRules(ArrayList<CaptchaSolverLimitRule> rules);
}
