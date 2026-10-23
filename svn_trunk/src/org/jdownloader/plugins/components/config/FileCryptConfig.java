package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "filecrypt.cc", type = Type.CRAWLER)
public interface FileCryptConfig extends PluginConfigInterface {
    final String                    text_CrawlMode   = "Crawl mode";
    final String                    text_EnableProxy = "Enable proxy (required for external powCAPTCHA solving)";
    final String                    text_ProxyString = "Enter proxy (required for external powCAPTCHA solving)";
    public static final TRANSLATION TRANSLATION      = new TRANSLATION();

    public static class TRANSLATION {
        public String getCrawlMode_label() {
            return text_CrawlMode;
        }

        public String getEnableProxy_label() {
            return text_EnableProxy;
        }

        public String getProxyString_label() {
            return text_ProxyString;
        }
    }

    public static enum CrawlMode implements LabelInterface {
        PREFER_GIVEN_MIRROR_ID {
            @Override
            public String getLabel() {
                return "Crawl only pre-selected mirror if it contains available links";
            }
        },
        CRAWL_ALL_AVAILABLE_MIRRORS {
            @Override
            public String getLabel() {
                return "Crawl all available mirrors";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("CRAWL_ALL_AVAILABLE_MIRRORS")
    @Order(20)
    @DescriptionForConfigEntry(text_CrawlMode)
    CrawlMode getCrawlMode();

    void setCrawlMode(CrawlMode cm);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(30)
    @DescriptionForConfigEntry(text_EnableProxy)
    boolean isEnableProxy();

    void setEnableProxy(boolean b);

    @AboutConfig
    @DefaultStringValue("")
    @Order(40)
    @DescriptionForConfigEntry("Proxy used for filecrypt.cc. Format: http://user:password@host:port, socks4://user:password@host:port or socks5://user:password@host:port. Example: socks5://myuser:mypass@127.0.0.1:1080. Note: This proxy may be sent to captcha solver services so that they can solve captchas through it.")
    String getProxyString();

    void setProxyString(String proxy);
}
