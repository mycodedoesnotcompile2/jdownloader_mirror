package org.jdownloader.plugins.components.config;

import java.util.Set;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "app.frame.io", type = Type.CRAWLER)
public interface AppFrameIoConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getPreferredQuality_label() {
            return "Preferred quality";
        }

        public String getQualityMode_label() {
            return "Quality mode";
        }
    }

    public static enum MODE implements LabelInterface {
        ALL {
            @Override
            public String getLabel() {
                return "All";
            }
        },
        SELECTED {
            @Override
            public String getLabel() {
                return "Selected only";
            }
        },
        BEST {
            @Override
            public String getLabel() {
                return "Best";
            }
        },
        BEST_SELECTED {
            @Override
            public String getLabel() {
                return "Best of selection";
            }
        }
    }

    public static enum QUALITY implements LabelInterface {
        ORIGINAL {
            @Override
            public String getLabel() {
                return "Original";
            }
        },
        Q2160P {
            @Override
            public String getLabel() {
                return "2160p (4k)";
            }
        },
        Q1080P {
            @Override
            public String getLabel() {
                return "1080p";
            }
        },
        Q720P {
            @Override
            public String getLabel() {
                return "720p";
            }
        },
        Q540P {
            @Override
            public String getLabel() {
                return "540p";
            }
        },
        Q360P {
            @Override
            public String getLabel() {
                return "360p";
            }
        };
    }

    @AboutConfig
    @Order(100)
    @DefaultOnNull
    Set<QUALITY> getPreferredQuality();

    void setPreferredQuality(Set<QUALITY> quality);

    @AboutConfig
    @Order(110)
    @DefaultEnumValue("ALL")
    @DefaultOnNull
    MODE getQualityMode();

    void setQualityMode(MODE mode);
}