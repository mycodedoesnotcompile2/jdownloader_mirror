package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.TakeValueFromSubconfig;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "deviantart.com", type = Type.HOSTER)
public interface DeviantArtComConfig extends PluginConfigInterface {
    // final String text_PreferServerFilename = "Prefer server filename?";
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getFastLinkcheckForSingleItems_label() {
            return "Fast linkcheck for single image files (filesize might not be shown until dl is started)?";
        }
        // public String getPreferServerFilename_label() {
        // return text_PreferServerFilename;
        // }

        public String getDownloadMode_label() {
            return "Image download mode";
        }

        public String getArtCrawlMode_label() {
            return "Crawl mode for '/art/' links";
        }
    }

    @AboutConfig
    @DefaultBooleanValue(false)
    @TakeValueFromSubconfig("SKIP_FILESIZECHECK") // backward compatibility
    @Order(10)
    boolean isFastLinkcheckForSingleItems();

    void setFastLinkcheckForSingleItems(boolean b);
    // @AboutConfig
    // @DefaultBooleanValue(false)
    // @TakeValueFromSubconfig("FilenameFromServer") // backward compatibility
    // @DescriptionForConfigEntry(text_PreferServerFilename)
    // @Order(20)
    // boolean isPreferServerFilename();
    //
    // void setPreferServerFilename(boolean b);

    public static enum ArtCrawlMode implements LabelInterface {
        SLOW_LOOK_FOR_MULTI_IMAGE_GALLERIES {
            @Override
            public String getLabel() {
                return "Slow: Check for multi image galleries";
            }
        },
        FAST_CRAWL_IGNORE_MULTI_IMAGE_GALLERIES {
            @Override
            public String getLabel() {
                return "Fast: Do not check for multi image galleries";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("SLOW_LOOK_FOR_MULTI_IMAGE_GALLERIES")
    @Order(20)
    @DescriptionForConfigEntry("Set this to fast if you already know that none of the items you add leads to a multi image gallery.")
    @DefaultOnNull
    ArtCrawlMode getArtCrawlMode();

    void setArtCrawlMode(final ArtCrawlMode mode);

    public static enum ImageDownloadMode implements LabelInterface {
        OFFICIAL_DOWNLOAD_ELSE_PREVIEW {
            @Override
            public String getLabel() {
                return "Prefer official download, download preview as fallback";
            }
        },
        OFFICIAL_DOWNLOAD_ONLY {
            @Override
            public String getLabel() {
                return "Official download only (account required)";
            }
        },
        HTML {
            @Override
            public String getLabel() {
                return "Download HTML of webpage instead of media";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("OFFICIAL_DOWNLOAD_ELSE_PREVIEW")
    @Order(30)
    @DefaultOnNull
    ImageDownloadMode getImageDownloadMode();

    void setImageDownloadMode(final ImageDownloadMode mode);
}