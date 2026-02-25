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
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "kemono.cr", type = Type.CRAWLER)
public interface KemonoPartyConfig extends PluginConfigInterface {
    final String                    text_CrawlHttpLinksFromPostContent       = "Crawl http links in post text?";
    final String                    text_PostTextLinkFilterMode              = "Post text link filter mode:";
    final String                    text_PostTextLinkFilterPattern           = "Post text link filter pattern (regex):";
    final String                    text_TextCrawlMode                       = "When to add post text content as .txt file:";
    public static final TRANSLATION TRANSLATION                              = new TRANSLATION();

    public static class TRANSLATION {
        public String getCrawlHttpLinksFromPostContent_label() {
            return text_CrawlHttpLinksFromPostContent;
        }

        public String getPostTextLinkFilterMode_label() {
            return text_PostTextLinkFilterMode;
        }

        public String getPostTextLinkFilterPattern_label() {
            return text_PostTextLinkFilterPattern;
        }

        public String getTextCrawlMode_label() {
            return text_TextCrawlMode;
        }

        public String getEnableProfileCrawlerAdvancedDupeFiltering_label() {
            return "Profile crawler: Enable advanced filtering of duplicates via sha256 hash?";
        }

        public String getPerPostURLPackageEnabled_label() {
            return "Profile crawler: Place each post results into separate package?";
        }
    }

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry(text_CrawlHttpLinksFromPostContent)
    @Order(10)
    boolean isCrawlHttpLinksFromPostContent();

    void setCrawlHttpLinksFromPostContent(boolean b);

    public static enum PostTextLinkFilterMode implements LabelInterface {
        DISABLED {
            @Override
            public String getLabel() {
                return "Disabled (crawl all links)";
            }
        },
        WHITELIST {
            @Override
            public String getLabel() {
                return "Whitelist (only crawl matching links)";
            }
        },
        BLACKLIST {
            @Override
            public String getLabel() {
                return "Blacklist (exclude matching links)";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("DISABLED")
    @DescriptionForConfigEntry(text_PostTextLinkFilterMode)
    @Order(11)
    PostTextLinkFilterMode getPostTextLinkFilterMode();

    void setPostTextLinkFilterMode(PostTextLinkFilterMode mode);

    @AboutConfig
    @DescriptionForConfigEntry(text_PostTextLinkFilterPattern)
    @Order(12)
    String getPostTextLinkFilterPattern();

    void setPostTextLinkFilterPattern(String pattern);

    public static enum TextCrawlMode implements LabelInterface {
        ALWAYS {
            @Override
            public String getLabel() {
                return "Always if text is available";
            }
        },
        ONLY_IF_NO_MEDIA_ITEMS_ARE_FOUND {
            @Override
            public String getLabel() {
                return "Only if no media items are found and text is available";
            }
        },
        NEVER {
            @Override
            public String getLabel() {
                return "Never";
            }
        };
    }

    public static enum PostRevisionMode implements LabelInterface {
        DEFAULT {

            @Override
            public PostRevisionMode getMode() {
                return def;
            }

            @Override
            public String getLabel() {
                return "(Default)" + def.getLabel();
            }
        },
        LATEST {
            @Override
            public String getLabel() {
                return "Only process newest/head revision of a post";
            }
        },
        SELECTED {
            @Override
            public String getLabel() {
                return "Only process a specific requested or newest/head revision of a post";
            }
        },
        ALL {
            @Override
            public String getLabel() {
                return "Always process all revisions of a post";
            }
        };

        private static final PostRevisionMode def = PostRevisionMode.LATEST;

        public PostRevisionMode getMode() {
            return this;
        }
    }

    @AboutConfig
    @DefaultEnumValue("ONLY_IF_NO_MEDIA_ITEMS_ARE_FOUND")
    @DescriptionForConfigEntry(text_TextCrawlMode)
    @Order(20)
    TextCrawlMode getTextCrawlMode();

    void setTextCrawlMode(TextCrawlMode mode);

    @AboutConfig
    @DefaultEnumValue("DEFAULT")
    @DescriptionForConfigEntry("Define the revision(s) of a post to be processed")
    @DefaultOnNull
    @Order(21)
    PostRevisionMode getPostRevisionMode();

    void setPostRevisionMode(PostRevisionMode mode);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("Filters duplicates during crawl process via sha256 file-hashes.")
    @Order(30)
    boolean isEnableProfileCrawlerAdvancedDupeFiltering();

    void setEnableProfileCrawlerAdvancedDupeFiltering(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(40)
    boolean isPerPostURLPackageEnabled();

    void setPerPostURLPackageEnabled(boolean b);
}