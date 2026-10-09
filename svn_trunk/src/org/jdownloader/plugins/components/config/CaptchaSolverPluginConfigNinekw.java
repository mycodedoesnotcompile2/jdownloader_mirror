package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultDoubleValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.DoubleSpinnerValidator;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "9kw.eu", type = Type.CAPTCHA)
public interface CaptchaSolverPluginConfigNinekw extends CaptchaSolverPluginConfig {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getLowBalanceWarningThreshold_label() {
            return "Low credits warning threshold";
        }

        public String getSelfsolve_label() {
            return "Only let my own 9kw workers solve my captchas";
        }

        public String getConfirm_label() {
            return "Confirm answers by a second 9kw user";
        }

        public String getPrio_label() {
            return "Captcha priority (0-20)";
        }
    }

    @AboutConfig
    @DescriptionForConfigEntry("9kw uses credits instead of a currency, so this is the credits balance below which the low balance warning is triggered. As a rule of thumb, 10 credits are enough to have one captcha solved. Only used if 'Warn on low balance' is enabled.")
    @DoubleSpinnerValidator(min = 500, max = 10000, step = 1000)
    @DefaultDoubleValue(1000)
    @Order(350)
    double getLowBalanceWarningThreshold();

    void setLowBalanceWarningThreshold(double credits);

    @AboutConfig
    @DescriptionForConfigEntry("Only let your own 9kw workers solve your captchas (9kw parameter 'selfsolve'). Captchas are not solved by other 9kw users. Only useful if you run your own 9kw worker. Sent with the upload query of each captcha.")
    @DefaultBooleanValue(false)
    @Order(1000)
    boolean isSelfsolve();

    void setSelfsolve(boolean selfsolve);

    @AboutConfig
    @DescriptionForConfigEntry("Let 9kw have each answer confirmed by a second 9kw user before it is returned to you (9kw parameter 'confirm'). This is slower and usually costs more credits, but wrong answers are less likely. Sent with the upload query of each captcha.")
    @DefaultBooleanValue(false)
    @Order(1100)
    boolean isConfirm();

    void setConfirm(boolean confirm);

    @AboutConfig
    @DescriptionForConfigEntry("Priority of your captchas at 9kw (9kw parameter 'prio'), 0-20. Captchas with a higher priority are picked up earlier by the 9kw workers, but cost more credits. 0 = default priority. Sent with the upload query of each captcha.")
    @SpinnerValidator(min = 0, max = 20, step = 1)
    @DefaultIntValue(0)
    @Order(1300)
    int getPrio();

    void setPrio(int num);
}
