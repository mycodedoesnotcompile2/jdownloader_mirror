package org.jdownloader.captcha.v2;

import java.util.Map;
import java.util.Set;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumArrayValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultJsonObject;
import org.appwork.storage.config.annotations.DefaultLongValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
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
            return "Report correct/incorrect captchas to the captcha solver?";
        }

        public String getLimitMaxSimultaneousCaptchasEnabled_label() {
            return "Limit max simultaneous captchas?";
        }

        public String getMaxSimultaneousCaptchas_label() {
            return "Max simultaneous captchas";
        }

        public String getMaxPollingTimeoutEnabled_label() {
            return "Limit max captcha solving time?";
        }

        public String getMaxPollingTimeoutSeconds_label() {
            return "Max captcha solving time (seconds)";
        }

        public String getStartNextSolverAfterSeconds_label() {
            return "Start the next solver after (seconds)";
        }
    }

    @AboutConfig(inGUIVisible = false)
    @DescriptionForConfigEntry("Enable/Disable this captcha solver")
    @DefaultBooleanValue(true)
    @Order(100)
    boolean isEnabled();

    void setEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Send correct/incorrect captcha feedback to the captcha solver to improve recognition accuracy and avoid wasting credits")
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
    @DescriptionForConfigEntry("Limit the maximum time to wait/poll for a captcha solution before giving up")
    @DefaultBooleanValue(false)
    @Order(675)
    boolean isMaxPollingTimeoutEnabled();

    void setMaxPollingTimeoutEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Maximum time in seconds to wait/poll for a captcha solution before giving up")
    @SpinnerValidator(min = 1, max = 3600, step = 1)
    @DefaultIntValue(300)
    @Order(676)
    int getMaxPollingTimeoutSeconds();

    void setMaxPollingTimeoutSeconds(int seconds);

    @AboutConfig(inGUIVisible = false)
    @DescriptionForConfigEntry("ENUM StringList of captcha types which are disabled for this solver.")
    @Order(800)
    @DefaultEnumArrayValue(value = {})
    @DefaultOnNull
    Set<CAPTCHA_TYPE> getDisabledCaptchaTypes();

    void setDisabledCaptchaTypes(Set<CAPTCHA_TYPE> enumset);

    /**
     * 2026-11-05: The old per solver settings "wait for other solvers" (a map: other solver ID -> milliseconds) are gone without a
     * migration, because they were very confusing. See {@link ChallengeResponseController} for the order of the solvers.
     */
    @AboutConfig
    @DescriptionForConfigEntry("How long (in seconds) this solver may work on a captcha without a solution before the next solver (see order in the solver table) is started in parallel to it. 0 = start the next solver immediately. If this solver is done without a solution, the next solver is started right away.")
    @SpinnerValidator(min = 0, max = SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_MAX, step = 1)
    /*
     * The default of the solver types is set in their own config interface, which overrides this method with its own default. All numbers
     * are defined in ChallengeSolver.SolverType (DEFAULT_START_NEXT_AFTER_SECONDS_*). The annotations have to be repeated in the overriding method, they are not inherited.
     */
    @DefaultIntValue(SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_DEFAULT)
    @Order(900)
    int getStartNextSolverAfterSeconds();

    void setStartNextSolverAfterSeconds(int seconds);

    @AboutConfig(inGUIVisible = false)
    @DescriptionForConfigEntry("Like 'Start the next solver after (seconds)', but for single captcha types. Key: name of the captcha type (CAPTCHA_TYPE), value: seconds (0 = start the next solver immediately). If a captcha type has no entry, 'Start the next solver after (seconds)' is used. Edited in the timing dialog of the solver table.")
    @DefaultJsonObject("{}")
    @DefaultOnNull
    @Order(901)
    Map<String, Integer> getStartNextSolverAfterSecondsByCaptchaType();

    void setStartNextSolverAfterSecondsByCaptchaType(Map<String, Integer> map);

    @AboutConfig(inGUIVisible = false)
    @DescriptionForConfigEntry("Timestamp (ms) of the successful migration of this solver's old settings (CaptchaSolverSettingsMigration). -1 = not migrated (yet) or nothing to migrate.")
    @DefaultLongValue(-1)
    @Order(1000)
    long getMigrateTimestamp();

    void setMigrateTimestamp(long timestamp);
}