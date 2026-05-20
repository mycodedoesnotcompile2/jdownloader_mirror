package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "nhentai.to", type = Type.CRAWLER)
public interface NhentaiNetCrawlerConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getCrawlerAccessMode_label() {
            return "Crawler access mode";
        }
    }

    public static enum CrawlerAccessMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto (prefer API)";
            }
        },
        API {
            @Override
            public String getLabel() {
                return "API";
            }
        },
        WEBSITE {
            @Override
            public String getLabel() {
                return "Website";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("AUTO")
    @DescriptionForConfigEntry("Use nhentai.net API or website crawling?")
    @Order(10)
    CrawlerAccessMode getCrawlerAccessMode();

    void setCrawlerAccessMode(CrawlerAccessMode mode);
}
