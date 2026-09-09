package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "naughtyblog.my", type = Type.CRAWLER)
public interface NaughtyBlgOrgConfig extends PluginConfigInterface {
    public static final NaughtyBlgOrgConfig.TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getCrawlCaptchaProtectedSpareLinks_label() {
            return "Also crawl captcha protected 'Spare links'?";
        }

        public String getPreviewCrawlMode_label() {
            return "Preview crawl behavior:";
        }
    }

    @AboutConfig
    @DescriptionForConfigEntry("Also crawl captcha protected 'Spare links'?")
    @DefaultBooleanValue(true)
    @Order(10)
    boolean isCrawlCaptchaProtectedSpareLinks();

    void setCrawlCaptchaProtectedSpareLinks(boolean b);

    public static enum PreviewCrawlMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto (only crawl previews if no other links are found)";
            }
        },
        ALWAYS {
            @Override
            public String getLabel() {
                return "Always crawl previews";
            }
        },
        NEVER {
            @Override
            public String getLabel() {
                return "Never crawl previews";
            }
        },
        ONLY_IF_NO_OTHER_LINKS_ARE_FOUND {
            @Override
            public String getLabel() {
                return "Only crawl previews if no other links are found";
            }
        },
        PREVIEW_ONLY {
            @Override
            public String getLabel() {
                return "Preview only (crawl only previews if preview links are found)";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("AUTO")
    @DescriptionForConfigEntry("Preview crawl behavior")
    @Order(20)
    PreviewCrawlMode getPreviewCrawlMode();

    void setPreviewCrawlMode(PreviewCrawlMode mode);
}
