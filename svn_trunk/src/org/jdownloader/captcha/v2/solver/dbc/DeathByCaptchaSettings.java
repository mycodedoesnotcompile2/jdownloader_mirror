package org.jdownloader.captcha.v2.solver.dbc;

import java.util.HashMap;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultJsonObject;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.RequiresRestart;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.captcha.v2.ChallengeSolverConfig;

public interface DeathByCaptchaSettings extends ChallengeSolverConfig {
    @AboutConfig
    @DescriptionForConfigEntry("Your deathbycaptcha.com Username")
    String getUserName();

    void setUserName(String jser);

    @AboutConfig
    @DescriptionForConfigEntry("Your deathbycaptcha.com Password or API key")
    String getPassword();

    void setPassword(String jser);

    @AboutConfig
    @RequiresRestart("A JDownloader Restart is required after changes")
    @DefaultIntValue(5)
    @SpinnerValidator(min = 0, max = 25)
    @DescriptionForConfigEntry("Max. Captchas Parallel")
    int getThreadpoolSize();

    void setThreadpoolSize(int size);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("Activate the Captcha Feedback")
    boolean isFeedBackSendingEnabled();

    void setFeedBackSendingEnabled(boolean b);

    @AboutConfig
    @DefaultJsonObject("{\"jdownloader.org\":60000}")
    @DescriptionForConfigEntry("Host bound Waittime before using CES. Use CaptchaExchangeChanceToSkipBubbleTimeout for a global timeout")
    HashMap<String, Integer> getBubbleTimeoutByHostMap();

    void setBubbleTimeoutByHostMap(HashMap<String, Integer> map);

    @AboutConfig
    @DefaultBooleanValue(false)
    boolean isEnabled();

    @AboutConfig
    @DescriptionForConfigEntry("Proxy URL(and credentials) required by deathbycaptcha.com for reCAPTCHAv2 Enterprise, e.g. http://user:pass@127.0.0.1:3128")
    String getEnterpriseRecaptchaProxy();

    void setEnterpriseRecaptchaProxy(String proxy);

    @AboutConfig
    @DefaultStringValue("HTTP")
    @DescriptionForConfigEntry("Proxy type required by deathbycaptcha.com for reCAPTCHAv2 Enterprise, e.g. HTTP, SOCKS4, SOCKS5")
    String getEnterpriseRecaptchaProxyType();

    void setEnterpriseRecaptchaProxyType(String proxyType);
}
