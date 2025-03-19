package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "hide.cx", type = Type.CRAWLER)
public interface HideCxConfig extends PluginConfigInterface {
    @AboutConfig
    // @DefaultStringValue("")
    @DescriptionForConfigEntry("You can find your API key here: https://hide.cx/settings?tab=keys")
    @Order(10)
    String getAPIKey();

    void setAPIKey(String str);
}