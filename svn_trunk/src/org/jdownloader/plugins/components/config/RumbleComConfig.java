package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "rumble.com", type = Type.CRAWLER)
public interface RumbleComConfig extends PluginConfigInterface {
    public static enum QualitySelectionMode implements LabelInterface {
        BEST {
            @Override
            public String getLabel() {
                return "Best quality";
            }
        },
        WORST {
            @Override
            public String getLabel() {
                return "Worst quality";
            }
        },
        SELECTED_ONLY {
            @Override
            public String getLabel() {
                return "Selected quality only (fallback = all)";
            }
        },
        ALL {
            @Override
            public String getLabel() {
                return "All available qualities";
            }
        };
    }

    public static enum Format {
        MP4,
        HLS,
        WEBM,
        TAR,
        ALL
    }

    public static enum Quality implements LabelInterface {
        Q2160 {
            @Override
            public String getLabel() {
                return "2160p (4k)";
            }
        },
        Q1440 {
            @Override
            public String getLabel() {
                return "1440p (2k)";
            }
        },
        Q1080 {
            @Override
            public String getLabel() {
                return "1080p (FHD)";
            }
        },
        Q720 {
            @Override
            public String getLabel() {
                return "720p (HD)";
            }
        },
        Q480 {
            @Override
            public String getLabel() {
                return "480p";
            }
        },
        Q360 {
            @Override
            public String getLabel() {
                return "360p";
            }
        },
        Q240 {
            @Override
            public String getLabel() {
                return "240p";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("ALL")
    @DefaultOnNull
    @DescriptionForConfigEntry("Define how this plugin should pick your desired qualities")
    @Order(10)
    QualitySelectionMode getQualitySelectionMode();

    void setQualitySelectionMode(QualitySelectionMode mode);

    @AboutConfig
    @DefaultEnumValue("Q1080")
    @DefaultOnNull
    @Order(20)
    @DescriptionForConfigEntry("Best will be used if selected preferred quality does not exist")
    RumbleComConfig.Quality getPreferredQuality();

    void setPreferredQuality(RumbleComConfig.Quality quality);

    @AboutConfig
    @DefaultEnumValue("MP4")
    @DefaultOnNull
    @Order(30)
    @DescriptionForConfigEntry("Best will be used if selected preferred format does not exist")
    RumbleComConfig.Format getPreferredFormat();

    void setPreferredFormat(RumbleComConfig.Format format);

}