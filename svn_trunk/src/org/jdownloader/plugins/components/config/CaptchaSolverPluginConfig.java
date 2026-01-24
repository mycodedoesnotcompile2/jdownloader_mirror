package org.jdownloader.plugins.components.config;

import java.util.List;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultDoubleValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultJsonObject;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.DoubleSpinnerValidator;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;

public interface CaptchaSolverPluginConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getEnabled_label() {
            return "Enable this captcha solver?";
        }

        public String getEnableCaptchaFeedback_label() {
            return "Report correct/incorrect captchas to captcha service?";
        }

        public String getWarnOnLowCredits_label() {
            return "Warn on low credits";
        }

        public String getLowCreditsWarningThreshold_label() {
            return "Low credits warning threshold (in currency of captcha solver service)";
        }

        public String getFilterListEnabled_label() {
            return "Enable filter list";
        }

        public String getFilterList_label() {
            return "Filter list";
        }

        public String getMaxCaptchasPerHourEnabled_label() {
            return "Limit max captchas per hour?";
        }

        public String getMaxCaptchasPerHour_label() {
            return "Max captchas per hour";
        }

        public String getLimitMaxSimultaneousCaptchasEnabled_label() {
            return "Limit max simultaneous captchas?";
        }

        public String getMaxSimultaneousCaptchas_label() {
            return "Max simultaneous captchas";
        }

        public String getPollingIntervalSeconds_label() {
            return "Polling interval in seconds";
        }
    }

    @AboutConfig
    @DescriptionForConfigEntry("Enable/Disable this captcha solver service")
    @DefaultBooleanValue(true)
    @Order(100)
    boolean isEnabled();

    void setEnabled(boolean b);

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
    @DescriptionForConfigEntry("Enable filter list")
    @DefaultBooleanValue(true)
    @Order(500)
    boolean isFilterListEnabled();

    void setFilterListEnabled(boolean b);

    @AboutConfig
    @Order(501)
    @DescriptionForConfigEntry("Filter list")
    @DefaultOnNull
    @DefaultJsonObject("[{\"name\":\"Example Block Example.com\",\"domain\":\"example.com\",\"captchaTypes\":[],\"regex\":false,\"enabled\":false,\"captchaRequestTypes\":[],\"filterType\":\"BLACKLIST\",\"broken\":false,\"id\":null,\"created\":1706000000000,\"position\":0},{\"name\":\"Example Allow Mega.nz - All Types\",\"domain\":\"mega\\.nz\",\"captchaTypes\":[\"IMAGE\",\"IMAGE_SINGLE_CLICK_CAPTCHA\",\"IMAGE_MULTI_CLICK_CAPTCHA\",\"RECAPTCHA_V3\",\"RECAPTCHA_V3_ENTERPRISE\",\"RECAPTCHA_V2_INVISIBLE\",\"RECAPTCHA_V2_ENTERPRISE\",\"RECAPTCHA_V2\",\"HCAPTCHA\",\"CUTCAPTCHA\",\"GEETEST_V1\",\"GEETEST_V4\",\"CLOUDFLARE_TURNSTILE\",\"MT_CAPTCHA\",\"FRIENDLY_CAPTCHA\"],\"regex\":true,\"enabled\":false,\"captchaRequestTypes\":[\"HOSTER_LOGIN\",\"HOSTER\",\"DECRYPTER\"],\"filterType\":\"WHITELIST\",\"broken\":false,\"id\":null,\"created\":1706100000000,\"position\":5}]")
    // TODO: Add better default (json) value?
    List<CaptchaChallengeFilter> getFilterList();

    void setFilterList(List<CaptchaChallengeFilter> list);

    @AboutConfig
    @DescriptionForConfigEntry("Limits max number of parallel captchas")
    @DefaultBooleanValue(false)
    @Order(600)
    boolean isLimitMaxSimultaneousCaptchasEnabled();

    void setLimitMaxSimultaneousCaptchasEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Maximum number of captchas to solve at the same time")
    @SpinnerValidator(min = 1, max = 100, step = 1)
    @DefaultIntValue(100)
    @Order(601)
    int getMaxSimultaneousCaptchas();

    void setMaxSimultaneousCaptchas(int max);

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