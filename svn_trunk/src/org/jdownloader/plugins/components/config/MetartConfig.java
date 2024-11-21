package org.jdownloader.plugins.components.config;

import java.util.Set;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "metart.com", type = Type.HOSTER)
public interface MetartConfig extends PluginConfigInterface {
    final String              text_PhotoCrawlMode = "Photo crawl mode";
    final String              text_VideoCrawlMode = "Video crawl mode";
    public static TRANSLATION TRANSLATION         = new TRANSLATION();

    public static enum MediaQuality implements LabelInterface {
        LOW {
            @Override
            public String getLabel() {
                return "Low";
            }
        },
        MEDIUM {
            @Override
            public String getLabel() {
                return "Medium";
            }
        },
        HIGH {
            @Override
            public String getLabel() {
                return "High";
            }
        };
    }

    public static class TRANSLATION {
        public String getPhotoCrawlMode_label() {
            return text_PhotoCrawlMode;
        }

        public String getVideoCrawlMode_label() {
            return text_VideoCrawlMode;
        }

        public String getMediaQualitiesPhotosLoose_label() {
            return "Qualities for loose photos";
        }

        public String getMediaQualitiesPhotosZip_label() {
            return "Qualities for .zip photos";
        }

        public String getMediaQualitiesVideos_label() {
            return "Qualities for videos";
        }
    }

    @AboutConfig
    @Order(110)
    // @DefaultEnumArrayValue(value = { "HIGH" })
    // @DefaultEnumValue("HIGH")
    @DefaultOnNull
    Set<MediaQuality> getMediaQualitiesPhotosLoose();

    void setMediaQualitiesPhotosLoose(Set<MediaQuality> quality);

    @AboutConfig
    @Order(120)
    // @DefaultEnumArrayValue(value = { "HIGH" })
    // @DefaultEnumValue("HIGH")
    @DefaultOnNull
    Set<MediaQuality> getMediaQualitiesPhotosZip();

    void setMediaQualitiesPhotosZip(Set<MediaQuality> quality);

    @AboutConfig
    @Order(130)
    // @DefaultEnumArrayValue(value = { "HIGH" })
    // @DefaultEnumValue("HIGH")
    @DefaultOnNull
    Set<MediaQuality> getMediaQualitiesVideos();

    void setMediaQualitiesVideos(Set<MediaQuality> quality);

    public static enum PhotoCrawlMode implements LabelInterface {
        ZIP_BEST {
            @Override
            public String getLabel() {
                return ".zip BEST quality";
            }
        },
        PHOTOS_BEST {
            @Override
            public String getLabel() {
                return "Loose photos BEST quality";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("PHOTOS_BEST")
    @Order(20)
    @DescriptionForConfigEntry(text_PhotoCrawlMode)
    PhotoCrawlMode getPhotoCrawlMode();

    void setPhotoCrawlMode(PhotoCrawlMode mode);

    public static enum VideoCrawlMode implements LabelInterface {
        ALL {
            @Override
            public String getLabel() {
                return "All qualities";
            }
        },
        BEST {
            @Override
            public String getLabel() {
                return "Best quality only";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("ALL")
    @Order(30)
    @DescriptionForConfigEntry(text_VideoCrawlMode)
    VideoCrawlMode getVideoCrawlMode();

    void setVideoCrawlMode(VideoCrawlMode mode);
}
