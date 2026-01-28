package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.TakeValueFromSubconfig;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "gofile.io", type = Type.HOSTER)
public interface GofileIoConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getAllowMaliciousFileDownload_label() {
            return "Allow download of files flagged as 'malicious' by download website?";
        }
    }

    @AboutConfig
    @DefaultBooleanValue(false)
    // @DescriptionForConfigEntry("Allow download of files flagged as 'malicious' by gofile.io?")
    @TakeValueFromSubconfig("allow_download_of_files_flagged_as_malicious")
    @Order(10)
    boolean isAllowMaliciousFileDownload();

    void setAllowMaliciousFileDownload(boolean b);
}