package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultDoubleValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DoubleSpinnerValidator;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "2captcha.com", type = Type.HOSTER)
public interface CaptchaSolverPluginConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getPollingIntervalSeconds_label() {
            return "Polling interval in seconds";
        }

        public String getEnableCaptchaFeedback_label() {
            return "Report correct/incorrect captcha feedback to captcha service";
        }

        public String getWarnOnLowCredits_label() {
            return "Warn on low credits";
        }

        public String getLowCreditsWarningThreshold_label() {
            return "Low credits warning threshold (in currency of captcha solver service)";
        }

        public String getMaxParallelCaptchas_label() {
            return "Max parallel captchas";
        }
    }

    @AboutConfig
    @SpinnerValidator(min = 2, max = 300, step = 1)
    @DefaultIntValue(5)
    @Order(10)
    int getPollingIntervalSeconds();

    void setPollingIntervalSeconds(int seconds);

    @AboutConfig
    @DefaultBooleanValue(true)
    @Order(20)
    boolean isEnableCaptchaFeedback();

    void setEnableCaptchaFeedback(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @Order(30)
    boolean isWarnOnLowCredits();

    void setWarnOnLowCredits(boolean b);

    @AboutConfig
    @DoubleSpinnerValidator(min = 0.1, max = 10, step = 0.1)
    @DefaultDoubleValue(0.5)
    @Order(35)
    double getLowCreditsWarningThreshold();

    void setLowCreditsWarningThreshold(double threshold);

    @AboutConfig
    @SpinnerValidator(min = 1, max = 20, step = 1)
    @DefaultIntValue(1)
    @Order(40)
    int getMaxParallelCaptchas();

    void setMaxParallelCaptchas(int max);
}