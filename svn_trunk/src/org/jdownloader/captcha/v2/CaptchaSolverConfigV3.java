package org.jdownloader.captcha.v2;

import java.util.Map;
import java.util.Set;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumArrayValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public interface CaptchaSolverConfigV3 extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getEnabled_label() {
            return "Enable this captcha solver?";
        }

        public String getEnableCaptchaFeedback_label() {
            return "Report correct/incorrect captchas to captcha service?";
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
    }

    @AboutConfig(inGUIVisible = false)
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

    @AboutConfig(inGUIVisible = false)
    @DescriptionForConfigEntry("ENUM StringList of captcha types which are disabled for this solver.")
    @Order(800)
    @DefaultEnumArrayValue(value = {})
    @DefaultOnNull
    Set<CAPTCHA_TYPE> getDisabledCaptchaTypes();

    void setDisabledCaptchaTypes(Set<CAPTCHA_TYPE> enumset);

    @AboutConfig(inGUIVisible = false)
    @DescriptionForConfigEntry("Solver-timing map (other solver id -> milliseconds to wait for it). Empty means automatic. Hidden in GUI for now.")
    @Order(900)
    @DefaultOnNull
    Map<String, Integer> getWaitForOthers();

    void setWaitForOthers(Map<String, Integer> map);
}