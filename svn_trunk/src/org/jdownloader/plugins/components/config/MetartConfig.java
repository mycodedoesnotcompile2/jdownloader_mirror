package org.jdownloader.plugins.components.config;

import java.util.Set;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumArrayValue;
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
    // class EnumSetValidator extends AbstractValidator<Set<MediaQuality>> {
    // @Override
    // public void validate(KeyHandler<Set<MediaQuality>> keyHandler, Set<MediaQuality> value) throws ValidationException {
    // if (value == null) {
    // throw new ValidationException("Cannot be null");
    // } else if (value.isEmpty()) {
    // throw new ValidationException("Cannot be empty");
    // }
    // }
    // }
    public static TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getVideoCrawlMode_label() {
            return "Video crawl mode";
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

    public interface MediaQualityDetails {
        String[] getInternalValues();
    }

    public static enum PhotoQuality implements LabelInterface, MediaQualityDetails {
        HIGH {
            @Override
            public String getLabel() {
                return "High";
            }

            @Override
            public String[] getInternalValues() {
                return new String[] { "high" };
            }
        },
        MEDIUM {
            @Override
            public String getLabel() {
                return "Medium";
            }

            @Override
            public String[] getInternalValues() {
                return new String[] { "med", "medium" };
            }
        },
        LOW {
            @Override
            public String getLabel() {
                return "Low";
            }

            @Override
            public String[] getInternalValues() {
                return new String[] { "low" };
            }
        }
    }

    public interface VideoQualityDetails {
        String getInternalValue();
    }

    public static enum VideoQuality implements LabelInterface, VideoQualityDetails {
        Q2160P {
            @Override
            public String getLabel() {
                return "4k UHD";
            }

            @Override
            public String getInternalValue() {
                return "4k";
            }
        },
        Q1080P {
            @Override
            public String getLabel() {
                return "1080p HD";
            }

            @Override
            public String getInternalValue() {
                return "1080p";
            }
        },
        Q720P {
            @Override
            public String getLabel() {
                return "720p HD";
            }

            @Override
            public String getInternalValue() {
                return "720p";
            }
        },
        Q360P {
            @Override
            public String getLabel() {
                return "360p";
            }

            @Override
            public String getInternalValue() {
                return "360p";
            }
        },
        Q270P {
            @Override
            public String getLabel() {
                return "270p";
            }

            @Override
            public String getInternalValue() {
                return "270p";
            }
        },
        QWMV {
            @Override
            public String getLabel() {
                return "WMV";
            }

            @Override
            public String getInternalValue() {
                return "wmv";
            }
        },
        QDIVX {
            @Override
            public String getLabel() {
                return "DivX AVI";
            }

            @Override
            public String getInternalValue() {
                return "avi";
            }
        }
    };

    @AboutConfig
    @Order(110)
    @DefaultEnumArrayValue(value = { "HIGH" })
    @DefaultOnNull
    Set<PhotoQuality> getMediaQualitiesPhotosLoose();

    void setMediaQualitiesPhotosLoose(Set<PhotoQuality> quality);

    @AboutConfig
    @Order(120)
    @DefaultEnumArrayValue(value = {})
    @DefaultOnNull
    Set<PhotoQuality> getMediaQualitiesPhotosZip();

    void setMediaQualitiesPhotosZip(Set<PhotoQuality> quality);

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

    public static enum VideoCrawlMode implements LabelInterface {
        BEST {
            @Override
            public String getLabel() {
                return "Best quality only";
            }
        },
        ALL {
            @Override
            public String getLabel() {
                return "All qualities";
            }
        },
        ALL_SELECTED {
            @Override
            public String getLabel() {
                return "All selected qualities";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("ALL")
    @Order(135)
    @DescriptionForConfigEntry("Customize video crawler behavior")
    VideoCrawlMode getVideoCrawlMode();

    void setVideoCrawlMode(VideoCrawlMode mode);

    @AboutConfig
    @Order(140)
    // @DefaultEnumArrayValue(value = { "HIGH" })
    // @DefaultEnumValue("HIGH")
    @DefaultOnNull
    Set<VideoQuality> getMediaQualitiesVideos();

    void setMediaQualitiesVideos(Set<VideoQuality> quality);
}
