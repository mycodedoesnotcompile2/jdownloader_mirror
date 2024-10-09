package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;

public interface EvilangelCoreConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getPreferredQuality_label() {
            return "Preferred quality";
        }

        public String getBestQualitySelectionMethod_label() {
            return "Best quality selection method";
        }
    }

    public static enum Quality implements LabelInterface {
        BEST {
            @Override
            public String getLabel() {
                return "Best";
            }
        },
        Q2160 {
            @Override
            public String getLabel() {
                return "4k 2160p";
            }
        },
        Q1080 {
            @Override
            public String getLabel() {
                return "Full HD 1080p";
            }
        },
        Q720 {
            @Override
            public String getLabel() {
                return "HD 720p";
            }
        },
        Q540 {
            @Override
            public String getLabel() {
                return "540p";
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
        },
        Q160 {
            @Override
            public String getLabel() {
                return "160p";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("BEST")
    @Order(10)
    @DescriptionForConfigEntry("Best will be used if selected preferred quality does not exist")
    EvilangelComConfig.Quality getPreferredQuality();

    void setPreferredQuality(EvilangelComConfig.Quality quality);

    public static enum BestSelectionMethod implements LabelInterface {
        RESOLUTION {
            @Override
            public String getLabel() {
                return "Video resolution";
            }
        },
        FILE_SIZE {
            @Override
            public String getLabel() {
                return "File size";
            }
        }
    }

    @AboutConfig
    @DefaultEnumValue("RESOLUTION")
    @Order(20)
    @DescriptionForConfigEntry("Defines how the best video quality will be determined.")
    BestSelectionMethod getBestQualitySelectionMethod();

    void setBestQualitySelectionMethod(BestSelectionMethod method);
}