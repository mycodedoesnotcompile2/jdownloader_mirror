package org.jdownloader.captcha.v2.solver.browser;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.captcha.v2.CaptchaSolverConfigV3;
import org.jdownloader.plugins.config.Order;

public interface BrowserCaptchaSolverConfigV3 extends CaptchaSolverConfigV3 {
    @DefaultBooleanValue(true)
    @AboutConfig
    @Order(2000)
    boolean isAutoClickEnabled();

    void setAutoClickEnabled(boolean b);

    @DefaultIntValue(500)
    @SpinnerValidator(min = 0, max = 10000, step = 500)
    @DescriptionForConfigEntry("Delay to auto click the captcha challenge in milliseconds")
    @AboutConfig
    @Order(3000)
    int getAutoClickDelay();

    void setAutoClickDelay(int delay);

    @DefaultIntValue(1000)
    @SpinnerValidator(min = 1000, max = 45000, step = 1000)
    @DescriptionForConfigEntry("Delay to auto open the captcha challenge in browser")
    @AboutConfig
    @Order(4000)
    int getAutoOpenDelay();

    void setAutoOpenDelay(int num);

    @AboutConfig
    @DefaultBooleanValue(true)
    @Order(5000)
    boolean isAutoOpenBrowserEnabled();

    void setAutoOpenBrowserEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Example: [ \"C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe\", \"%s\" ]")
    @Order(6000)
    String[] getBrowserCommandline();

    void setBrowserCommandline(String[] cmd);

    @DefaultIntValue(24613)
    @SpinnerValidator(min = 0, max = 65535, step = 1)
    @AboutConfig
    @Order(7000)
    int getLocalHttpPort();

    void setLocalHttpPort(int port);
}