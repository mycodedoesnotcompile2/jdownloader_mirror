package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "bunkrr.su", type = Type.HOSTER)
public interface BunkrConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getFixFilename_label() {
            return "Fix filenames (remove internal file ID suffix)";
        }

        public String getMinimumFileSizePercentage_label() {
            return "Minimum file size percentage";
        }
    }

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("Bunkr filenames sometimes contain an internal file ID suffix. Enable to remove it.")
    @Order(10)
    boolean isFixFilename();

    void setFixFilename(boolean b);

    @AboutConfig
    @SpinnerValidator(min = 5, max = 100, step = 1)
    @DefaultIntValue(50)
    @DescriptionForConfigEntry("Files smaller than this percentage of the expected size will be marked as 'File too small'.")
    @Order(20)
    int getMinimumFileSizePercentage();

    void setMinimumFileSizePercentage(int num);
}