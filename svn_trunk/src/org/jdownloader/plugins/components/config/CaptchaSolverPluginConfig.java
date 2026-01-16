package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultDoubleValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
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
        public String getEnableCaptchaFeedback_label() {
            return "Report correct/incorrect captchas to captcha service?";
        }

        public String getWarnOnLowCredits_label() {
            return "Warn on low credits";
        }

        public String getLowCreditsWarningThreshold_label() {
            return "Low credits warning threshold (in currency of captcha solver service)";
        }

        public String getEnableDomainWhitelist_label() {
            return "Enable domain whitelist";
        }

        public String getDomainWhitelistEnabled_label() {
            return "Enable domain blacklist";
        }

        public String getDomainWhitelist_label() {
            return "Domain whitelist";
        }

        public String getDomainBlacklist_label() {
            return "Domain blacklist";
        }

        public String getMaxCaptchasPerHourEnabled_label() {
            return "Limit max captchas per hour?";
        }

        public String getMaxCaptchasPerHour_label() {
            return "Max captchas per hour";
        }

        public String getLimitMaxParallelCaptchasEnabled_label() {
            return "Limit max parallel captchas?";
        }

        public String getMaxParallelCaptchas_label() {
            return "Max parallel captchas";
        }

        public String getPollingIntervalSeconds_label() {
            return "Polling interval in seconds";
        }
    }

    @AboutConfig
    @DescriptionForConfigEntry("Send correct/incorrect captcha feedback to captcha service to improve recognition accuracy and avoid wasting credits")
    @DefaultBooleanValue(true)
    @Order(200)
    boolean isEnableCaptchaFeedback();

    void setEnableCaptchaFeedback(boolean b);

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
    @DescriptionForConfigEntry("Enable domain whitelist")
    @DefaultBooleanValue(true)
    @Order(500)
    boolean isDomainWhitelistEnabled();

    void setDomainWhitelistEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Comma-separated list of domains where captcha solving is enabled")
    @DefaultStringValue("")
    @Order(550)
    String getDomainWhitelist();

    void setDomainWhitelist(String domains);

    @AboutConfig
    @DescriptionForConfigEntry("Enable domain blacklist")
    @DefaultBooleanValue(true)
    @Order(551)
    boolean isDomainBlacklistEnabled();

    void setDomainBlacklistEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Comma-separated list of domains where captcha solving is disabled")
    @DefaultStringValue("")
    @Order(550)
    String getDomainBlacklist();

    void setDomainBlacklist(String domains);

    @AboutConfig
    @DescriptionForConfigEntry("Limits max number of parallel captchas")
    @DefaultBooleanValue(false)
    @Order(600)
    boolean isLimitMaxParallelCaptchasEnabled();

    void setLimitMaxParallelCaptchasEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Maximum number of captchas to solve in parallel")
    @SpinnerValidator(min = 1, max = 100, step = 1)
    @DefaultIntValue(100)
    @Order(601)
    int getMaxParallelCaptchas();

    void setMaxParallelCaptchas(int max);

    @AboutConfig
    @DescriptionForConfigEntry("Limit max captchas per hour")
    @DefaultBooleanValue(false)
    @Order(650)
    boolean isMaxCaptchasPerHourEnabled();

    void setMaxCaptchasPerHourEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Max captchas per hour")
    @SpinnerValidator(min = 1, max = 10000, step = 1)
    @DefaultIntValue(1000)
    @Order(651)
    int getMaxCaptchasPerHour();

    void setMaxCaptchasPerHour(int max);

    @AboutConfig
    @DescriptionForConfigEntry("Polling interval in seconds for captcha status checks")
    @SpinnerValidator(min = 2, max = 30, step = 1)
    @DefaultIntValue(5)
    @Order(700)
    int getPollingIntervalSeconds();

    void setPollingIntervalSeconds(int seconds);
}