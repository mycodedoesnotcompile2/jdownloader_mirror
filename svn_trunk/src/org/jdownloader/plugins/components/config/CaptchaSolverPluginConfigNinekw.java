package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "9kw.eu", type = Type.CAPTCHA)
public interface CaptchaSolverPluginConfigNinekw extends CaptchaSolverPluginConfig {
    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(1000)
    boolean isSelfsolve();

    void setSelfsolve(boolean selfsolve);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(1100)
    boolean isConfirm();

    void setConfirm(boolean confirm);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 20, step = 1)
    @DefaultIntValue(0)
    @Order(1300)
    int getPrio();

    void setPrio(int num);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 9999, step = 1)
    @DefaultIntValue(0)
    @Order(1400)
    int getHour();

    void setHour(int num);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 9999, step = 1)
    @DefaultIntValue(0)
    @Order(1500)
    int getMinute();

    void setMinute(int num);
}