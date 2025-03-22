package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.TakeValueFromSubconfig;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "instagram.com", type = Type.HOSTER)
public interface InstagramConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getPostCrawlerAddPostDescriptionAsTextfile_label() {
            return "Post crawler: Add post description as textfile?";
        }

        public String getPostCrawlerPackagenameSchemeType_label() {
            return "Post crawler: Package name scheme type for /p/<id>";
        }

        public String getPostCrawlerPackagenameScheme_label() {
            return "Post crawler: Custom package name scheme for /p/<id>";
        }

        public String getStoryPackagenameSchemeType_label() {
            return "Story crawler: Package name scheme type for /stories/username/<storyID>/ URLs";
        }

        public String getStoryPackagenameScheme_label() {
            return "Story crawler: Custom package name scheme for /stories/username/<storyID>/";
        }

        public String getStoriesHighlightsPackagenameSchemeType_label() {
            return "Story highlights crawler: Package name scheme type for /stories/highlights/<storyID>/";
        }

        public String getStoriesHighlightsPackagenameScheme_label() {
            return "Story highlights crawler: Custom package name scheme for /stories/highlights/<storyID>/";
        }

        public String getFilenameType_label() {
            return "File name type for all crawled media items";
        }

        public String getFilenameScheme_label() {
            return "Custom filenames: Filename scheme";
        }

        public String getAddDateToFilenames_label() {
            return "Default file names: Include date (yyyy-MM-dd) in filenames?";
        }

        public String getAddOrderidToFilenames_label() {
            return "Default file names: Include 'order-ID' in file names if an album contains more than one element?";
        }

        public String getAddShortcodeToFilenames_label() {
            return "Default file names: Include 'shortcode' in file names if it is available?";
        }

        public String getMediaQualityDownloadMode_label() {
            return "Media quality download mode.\r\nOriginal quality = bigger filesize, without image-effects, works only when an account is available.";
        }

        public String getProfileCrawlerMaxItemsLimit_label() {
            return "Profile crawler: Only grab X latest posts? [0 = disable, -1 = crawl all]";
        }

        public String getProfileCrawlerCrawlStory_label() {
            return "Profile crawler: Crawl story?";
        }

        public String getProfileCrawlerCrawlStoryHighlights_label() {
            return "Profile crawler: Crawl story highlights?";
        }

        public String getProfileCrawlerCrawlProfilePicture_label() {
            return "Profile crawler: Crawl profile picture?";
        }

        public String getProfileCrawlerReelsPaginationMaxItemsPerPage_label() {
            return "Profile reels crawler: Max items per pagination (higher value = faster crawl process, can result in account ban!)";
        }

        public String getProfileTaggedCrawledMaxItemsLimit_label() {
            return "Tagged profile crawler: How many items shall be grabbed for /profile/tagged/? [0 = disable tagged profile crawler]";
        }

        public String getHashtagCrawlerMaxItemsLimit_label() {
            return "Hashtag crawler: How many items shall be grabbed for /explore/tags/<tagName>? [0 = disable]";
        }

        public String getSearchCrawlerMaxItemsLimit_label() {
            return "Search crawler: How many items shall be grabbed for /explore/search/keyword/?q=<searchTerm>? [0 = disable]";
        }

        public String getActionOnRateLimitReached_label() {
            return "Crawler: Action on rate limit reached";
        }

        public String getGlobalRequestIntervalLimitMilliseconds_label() {
            return "Global request limit for domains 'instagram.com' and 'cdninstagram.com' in milliseconds [0 = no limit]";
        }

        public String getEnforceLoginIfAccountIsAvailable_label() {
            return "Debug: Enforce login if account is available?";
        }
    }

    @AboutConfig
    @DefaultBooleanValue(true)
    @Order(1)
    boolean isPostCrawlerAddPostDescriptionAsTextfile();

    void setPostCrawlerAddPostDescriptionAsTextfile(boolean b);

    public static enum SinglePostPackagenameSchemeType implements LabelInterface {
        UPLOADER {
            @Override
            public String getLabel() {
                return "*uploader*";
            }
        },
        UPLOADER_MAIN_CONTENT_ID {
            @Override
            public String getLabel() {
                return "*uploader* - *main_content_id*";
            }
        },
        CUSTOM {
            @Override
            public String getLabel() {
                return "Custom";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("UPLOADER")
    @Order(2)
    SinglePostPackagenameSchemeType getPostCrawlerPackagenameSchemeType();

    void setPostCrawlerPackagenameSchemeType(final SinglePostPackagenameSchemeType namingSchemeType);

    @AboutConfig
    @DefaultStringValue("*date*_*uploader* - *main_content_id*")
    @Order(3)
    String getPostCrawlerPackagenameScheme();

    void setPostCrawlerPackagenameScheme(String str);

    public static enum StoryPackagenameSchemeType implements LabelInterface {
        DEFAULT_1 {
            @Override
            public String getLabel() {
                return "story - *uploader*";
            }
        },
        CUSTOM {
            @Override
            public String getLabel() {
                return "Custom";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("DEFAULT_1")
    @Order(4)
    StoryPackagenameSchemeType getStoryPackagenameSchemeType();

    void setStoryPackagenameSchemeType(final StoryPackagenameSchemeType namingSchemeType);

    @AboutConfig
    @DefaultStringValue("*date*_*uploader*")
    @Order(5)
    String getStoryPackagenameScheme();

    void setStoryPackagenameScheme(String str);

    public static enum StoriesHighlightsPackagenameSchemeType implements LabelInterface {
        DEFAULT_1 {
            @Override
            public String getLabel() {
                return "story highlights - *uploader* - *title*";
            }
        },
        CUSTOM {
            @Override
            public String getLabel() {
                return "Custom";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("DEFAULT_1")
    @Order(6)
    StoriesHighlightsPackagenameSchemeType getStoriesHighlightsPackagenameSchemeType();

    void setStoriesHighlightsPackagenameSchemeType(final StoriesHighlightsPackagenameSchemeType namingSchemeType);

    @AboutConfig
    @DefaultStringValue("*date*_*uploader* - *title*")
    @Order(7)
    String getStoriesHighlightsPackagenameScheme();

    void setStoriesHighlightsPackagenameScheme(String str);

    public static enum FilenameType implements LabelInterface {
        DEFAULT {
            @Override
            public String getLabel() {
                return "Default";
            }
        },
        SERVER {
            @Override
            public String getLabel() {
                return "Server filenames";
            }
        },
        CUSTOM {
            @Override
            public String getLabel() {
                return "Custom";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("DEFAULT")
    @Order(10)
    FilenameType getFilenameType();

    void setFilenameType(final FilenameType filenameNamingSchemeType);

    @AboutConfig
    @DefaultStringValue("*date*_*uploader* - *main_content_id* *orderid*_of_*orderid_max* - *shortcode**ext*")
    @Order(15)
    String getFilenameScheme();

    void setFilenameScheme(String str);

    @AboutConfig
    @DefaultBooleanValue(false)
    @TakeValueFromSubconfig("ADD_DATE_TO_FILENAMES")
    @Order(20)
    boolean isAddDateToFilenames();

    void setAddDateToFilenames(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    @TakeValueFromSubconfig("ADD_ORDERID_TO_FILENAMES")
    @Order(30)
    boolean isAddOrderidToFilenames();

    void setAddOrderidToFilenames(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    @TakeValueFromSubconfig("ADD_SHORTCODE_TO_FILENAMES")
    @Order(40)
    boolean isAddShortcodeToFilenames();

    void setAddShortcodeToFilenames(boolean b);

    public static enum MediaQualityDownloadMode implements LabelInterface {
        DEFAULT_QUALITY {
            @Override
            public String getLabel() {
                return "Default Instagram quality";
            }
        },
        PREFER_ORIGINAL_QUALITY {
            @Override
            public String getLabel() {
                return "Prefer original quality (account required, on failure = fallback to default quality)";
            }
        },
        ENFORCE_ORIGINAL_QUALITY {
            @Override
            public String getLabel() {
                return "Enforce original quality (account required, on failure = display error message)";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("DEFAULT_QUALITY")
    @Order(50)
    @DefaultOnNull
    MediaQualityDownloadMode getMediaQualityDownloadMode();

    void setMediaQualityDownloadMode(final MediaQualityDownloadMode mediaQualityDownloadMode);

    @AboutConfig
    @SpinnerValidator(min = -1, max = 1024, step = 1)
    @DefaultIntValue(-1)
    @Order(70)
    int getProfileCrawlerMaxItemsLimit();

    void setProfileCrawlerMaxItemsLimit(int items);

    @AboutConfig
    @DefaultBooleanValue(true)
    @Order(71)
    boolean isProfileCrawlerCrawlStory();

    void setProfileCrawlerCrawlStory(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @Order(72)
    boolean isProfileCrawlerCrawlStoryHighlights();

    void setProfileCrawlerCrawlStoryHighlights(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @Order(73)
    boolean isProfileCrawlerCrawlProfilePicture();

    void setProfileCrawlerCrawlProfilePicture(boolean b);

    @AboutConfig
    @SpinnerValidator(min = 1, max = 100, step = 1)
    @DefaultIntValue(12)
    @Order(85)
    int getProfileCrawlerReelsPaginationMaxItemsPerPage();

    void setProfileCrawlerReelsPaginationMaxItemsPerPage(int items);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 10000, step = 25)
    @DefaultIntValue(25)
    @Order(85)
    int getProfileTaggedCrawledMaxItemsLimit();

    void setProfileTaggedCrawledMaxItemsLimit(int items);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 10000, step = 25)
    @DefaultIntValue(25)
    @TakeValueFromSubconfig("ONLY_GRAB_X_ITEMS_HASHTAG_CRAWLER_NUMBER")
    @Order(90)
    int getHashtagCrawlerMaxItemsLimit();

    void setHashtagCrawlerMaxItemsLimit(int items);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 10000, step = 25)
    @DefaultIntValue(25)
    @Order(91)
    int getSearchCrawlerMaxItemsLimit();

    void setSearchCrawlerMaxItemsLimit(int items);

    public static enum ActionOnRateLimitReached implements LabelInterface {
        CONTINUE {
            @Override
            public String getLabel() {
                return "Wait and try again";
            }
        },
        ABORT {
            @Override
            public String getLabel() {
                return "Abort crawl process";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("CONTINUE")
    @Order(500)
    @DefaultOnNull
    ActionOnRateLimitReached getActionOnRateLimitReached();

    void setActionOnRateLimitReached(final ActionOnRateLimitReached action);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 60000, step = 100)
    @DefaultIntValue(400)
    @Order(510)
    int getGlobalRequestIntervalLimitMilliseconds();

    void setGlobalRequestIntervalLimitMilliseconds(int milliseconds);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(600)
    boolean isEnforceLoginIfAccountIsAvailable();

    void setEnforceLoginIfAccountIsAvailable(boolean b);
}