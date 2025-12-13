package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "filecrypt.cc", type = Type.CRAWLER)
public interface FileCryptConfig extends PluginConfigInterface {
    final String                    text_CrawlMode = "Crawl mode";
    public static final TRANSLATION TRANSLATION    = new TRANSLATION();

    public static class TRANSLATION {
        public String getCrawlMode_label() {
            return text_CrawlMode;
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
}
