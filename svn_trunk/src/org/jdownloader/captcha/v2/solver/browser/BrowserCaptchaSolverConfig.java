package org.jdownloader.captcha.v2.solver.browser;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.captcha.v2.ChallengeSolverConfig;

public interface BrowserCaptchaSolverConfig extends ChallengeSolverConfig {
    @DefaultBooleanValue(true)
    @AboutConfig
    boolean isAutoClickEnabled();

    void setAutoClickEnabled(boolean b);

    @DefaultIntValue(500)
    @SpinnerValidator(min = 0, max = 10000, step = 500)
    @DescriptionForConfigEntry("Delay to auto click the captcha challenge in milliseconds")
    @AboutConfig
    int getAutoClickDelay();

    void setAutoClickDelay(int delay);

    @DefaultIntValue(1000)
    @SpinnerValidator(min = 1000, max = 45000, step = 1000)
    @DescriptionForConfigEntry("Delay to auto open the captcha challenge in browser")
    @AboutConfig
    int getAutoOpenDelay();

    void setAutoOpenDelay(int delay);

    @AboutConfig
    @DefaultBooleanValue(true)
    boolean isAutoOpenBrowserEnabled();

    void setAutoOpenBrowserEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Example: [ \"C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe\", \"%s\" ]")
    String[] getBrowserCommandline();

    void setBrowserCommandline(String[] cmd);

    @DefaultIntValue(24613)
    @SpinnerValidator(min = 0, max = 65535, step = 1)
    @AboutConfig
    int getLocalHttpPort();

    void setLocalHttpPort(int port);
}
