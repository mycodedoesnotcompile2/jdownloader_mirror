package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "dropbox.com", type = Type.HOSTER)
public interface DropBoxConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        // public String getUseAPI_label() {
        // return "BETA: Use API (in account mode)?";
        // // return "BETA: Use API (in account mode)? [Recommended, changing this setting does not have any effect at the moment!]";
        // // return "Use API (in account mode)? [Recommended]";
        // }
        public String getAskIfSubfoldersShouldBeCrawled_label() {
            return "Folder crawler: Ask if subfolders should be crawled?";
        }

        public String getEnableFastLinkcheckForSingleFiles_label() {
            return "Enable fast linkcheck for single files [enabled = file information may not be be displayed until download is started]?";
        }

        public String getUserAgent_label() {
            return "User-Agent";
        }
    }
    // @DefaultBooleanValue(false)
    // @AboutConfig
    // @DescriptionForConfigEntry("If enabled, API will be used")
    // @Order(10)
    // boolean isUseAPI();
    //
    // void setUseAPI(boolean b);

    @DefaultBooleanValue(false)
    @AboutConfig
    @DescriptionForConfigEntry("Disabled = Do not ask and always crawl subfolders")
    @Order(20)
    boolean isAskIfSubfoldersShouldBeCrawled();

    void setAskIfSubfoldersShouldBeCrawled(boolean b);

    @DefaultBooleanValue(true)
    @AboutConfig
    @Order(30)
    boolean isEnableFastLinkcheckForSingleFiles();

    void setEnableFastLinkcheckForSingleFiles(boolean b);

    @AboutConfig
    @DefaultStringValue("JDDEFAULT")
    @Order(40)
    String getUserAgent();

    public void setUserAgent(final String userAgent);
}