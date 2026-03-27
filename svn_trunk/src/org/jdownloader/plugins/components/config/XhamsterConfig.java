package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.TakeValueFromSubconfig;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "xhamster.com", type = Type.HOSTER)
public interface XhamsterConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getPreferredFormat_label() {
            return "Preferred format";
        }

        public String getFilenameId_label() {
            return "Videos: Change file name to 'filename_VideoID.ext' e.g. 'test_48604.mp4'?";
        }

        public String getPremiumDownloadMode_label() {
            return "Premium (faphouse) download mode";
        }
    }

    public static enum PreferredFormat implements LabelInterface {
        BEST {
            @Override
            public String getLabel() {
                return "Best available";
            }
        },
        Q144 {
            @Override
            public String getLabel() {
                return "144p";
            }
        },
        Q240 {
            @Override
            public String getLabel() {
                return "240p";
            }
        },
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
        Q960 {
            @Override
            public String getLabel() {
                return "960p";
            }
        },
        Q1080 {
            @Override
            public String getLabel() {
                return "1080p";
            }
        },
        Q1440 {
            @Override
            public String getLabel() {
                return "1440p";
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
    @DefaultEnumValue("BEST")
    @DefaultOnNull
    @Order(10)
    PreferredFormat getPreferredFormat();

    void setPreferredFormat(PreferredFormat format);

    @AboutConfig
    @DefaultBooleanValue(true)
    @TakeValueFromSubconfig("Filename_id")
    @Order(20)
    boolean isFilenameId();

    void setFilenameId(boolean b);

    public static enum PremiumDownloadMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto: Prefer official download, allow fallback to stream download";
            }
        },
        STREAM_DOWNLOAD_ONLY {
            @Override
            public String getLabel() {
                return "Stream download only";
            }
        },
        OFFICIAL_DOWNLOAD_ONLY {
            @Override
            public String getLabel() {
                return "Official download only";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("AUTO")
    @DefaultOnNull
    @Order(30)
    PremiumDownloadMode getPremiumDownloadMode();

    void setPremiumDownloadMode(PremiumDownloadMode mode);
}