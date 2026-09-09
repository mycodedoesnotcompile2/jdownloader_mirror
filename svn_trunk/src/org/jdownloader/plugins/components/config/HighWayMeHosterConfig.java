package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "high-way.me", type = Type.HOSTER)
public interface HighWayMeHosterConfig extends PluginConfigInterface {
    public static final HighWayMeHosterConfig.TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getCloudCrawlerAddOnlyDownloadableItems_label() {
            return "Cloud crawler: Add only downloadable elements";
        }
    }

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(10)
    boolean isCloudCrawlerAddOnlyDownloadableItems();

    void setCloudCrawlerAddOnlyDownloadableItems(boolean b);
}
