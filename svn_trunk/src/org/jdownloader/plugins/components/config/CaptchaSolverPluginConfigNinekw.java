package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "9kw.eu", type = Type.CAPTCHA)
public interface CaptchaSolverPluginConfigNinekw extends CaptchaSolverPluginConfig {
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
