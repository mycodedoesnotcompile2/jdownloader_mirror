package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "xtremestream.co", type = Type.HOSTER)
public interface XtremestreamCoConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getPreferredDownloadType_label() {
            return "Preferred download type";
        }

        public String getPreferredStreamQuality_label() {
            return "Preferred stream video quality";
        }
    }

    @AboutConfig
    @DefaultEnumValue("AUTO")
    @DescriptionForConfigEntry("Prefer download via official download button or download of stream?")
    @Order(10)
    DownloadType getPreferredDownloadType();

    void setPreferredDownloadType(DownloadType type);

    public static enum Quality implements LabelInterface {
        Q360 {
            @Override
            public String getLabel() {
                return "360p";
            }
        },
        Q480 {
            @Override
            public String getLabel() {
                return "480p";
            }
        },
        Q720 {
            @Override
            public String getLabel() {
                return "720p";
            }
        },
        Q1080 {
            @Override
            public String getLabel() {
                return "1080p";
            }
        },
        Q2160 {
            @Override
            public String getLabel() {
                return "2160p";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("Q1080")
    @DescriptionForConfigEntry("If your preferred stream video quality is not found, best quality will be downloaded instead.")
    @Order(20)
    Quality getPreferredStreamQuality();

    void setPreferredStreamQuality(Quality quality);

    public static enum DownloadType implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto";
            }
        },
        OFFICIAL_DOWNLOAD {
            @Override
            public String getLabel() {
                return "Official download";
            }
        },
        STREAM_DOWNLOAD {
            @Override
            public String getLabel() {
                return "Stream download";
            }
        };
    }
}