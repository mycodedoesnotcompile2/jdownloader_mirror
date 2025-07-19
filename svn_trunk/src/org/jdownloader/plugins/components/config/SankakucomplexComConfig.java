package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "sankakucomplex.com", type = Type.HOSTER)
public interface SankakucomplexComConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getSetCommaSeparatedTagsOfPostsAsComment_label() {
            return "Set comma separated tags of posts as comment?";
        }

        public String getBookTagCrawlerMaxPageLimit_label() {
            return "Book tag crawler: Max page limit (-1 = no limit, 0 = disabled)";
        }

        public String getPostTagCrawlerMaxPageLimit_label() {
            return "Post tag crawler: Max page limit (-1 = no limit, 0 = disabled)";
        }

        public String getPostTagCrawlerAccessMode_label() {
            return "Post tag crawler: Access mode";
        }

        public String getLinkcheckAccessMode_label() {
            return "Linkcheck: Access mode";
        }

        public String getCrawlerFastLinkcheckEnabled_label() {
            return "Crawler: Enable fast linkcheck";
        }
    }

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(10)
    boolean isSetCommaSeparatedTagsOfPostsAsComment();

    void setSetCommaSeparatedTagsOfPostsAsComment(boolean b);

    @AboutConfig
    @SpinnerValidator(min = -1, max = 10000, step = 1)
    @DefaultIntValue(1)
    @Order(100)
    int getBookTagCrawlerMaxPageLimit();

    void setBookTagCrawlerMaxPageLimit(int pages);

    @AboutConfig
    @SpinnerValidator(min = -1, max = 10000, step = 1)
    @DefaultIntValue(1)
    @Order(110)
    int getPostTagCrawlerMaxPageLimit();

    void setPostTagCrawlerMaxPageLimit(int pages);

    public static enum AccessMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto";
            }
        },
        API {
            @Override
            public String getLabel() {
                return "API";
            }
        },
        WEBSITE {
            @Override
            public String getLabel() {
                return "Website";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("AUTO")
    @Order(120)
    AccessMode getPostTagCrawlerAccessMode();

    void setPostTagCrawlerAccessMode(final AccessMode mode);

    @AboutConfig
    @DefaultEnumValue("AUTO")
    @Order(130)
    AccessMode getLinkcheckAccessMode();

    void setLinkcheckAccessMode(final AccessMode mode);

    @AboutConfig
    @DescriptionForConfigEntry("Disabled = It will take much longer until crawled items will appear as each post will be checked individually. Only disable this if you know what you're doing! Disabling this can cause your IP to get blocked by sankakucomplex!")
    @DefaultBooleanValue(true)
    @Order(140)
    boolean isCrawlerFastLinkcheckEnabled();

    void setCrawlerFastLinkcheckEnabled(boolean b);
}