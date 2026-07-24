package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "deathbycaptcha.com", type = Type.CAPTCHA)
public interface CaptchaSolverPluginConfigDeathbycaptcha extends CaptchaSolverPluginConfig {
    @AboutConfig
    @DescriptionForConfigEntry("Proxy URL(and credentials) required by deathbycaptcha.com for reCAPTCHAv2 Enterprise, e.g. http://user:pass@127.0.0.1:3128")
    @Order(900)
    String getEnterpriseRecaptchaProxy();

    void setEnterpriseRecaptchaProxy(String proxy);

    @AboutConfig
    @DefaultStringValue("HTTP")
    @DescriptionForConfigEntry("Proxy type required by deathbycaptcha.com for reCAPTCHAv2 Enterprise, e.g. HTTP, SOCKS4, SOCKS5")
    @Order(901)
    String getEnterpriseRecaptchaProxyType();

    void setEnterpriseRecaptchaProxyType(String proxyType);
}