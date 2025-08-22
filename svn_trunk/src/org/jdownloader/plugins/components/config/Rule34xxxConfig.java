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

@PluginHost(host = "rule34.xxx", type = Type.CRAWLER)
public interface Rule34xxxConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getPreferServerFilenamesOverPluginDefaultFilenames_label() {
            return "Use server side/original filenames instead of plugin filenames?";
        }

        public String getAPIUser_label() {
            return "API user id from rule34.xxx/index.php?page=account&s=options | Only numbers";
        }

        public String getAPIKey_label() {
            return "API key from rule34.xxx/index.php?page=account&s=options | [a-f0-9]{128}";
        }
    }

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("Prefer server filenames over plugins' default filenames?")
    @Order(10)
    boolean isPreferServerFilenamesOverPluginDefaultFilenames();

    void setPreferServerFilenamesOverPluginDefaultFilenames(boolean b);

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
    @Order(11)
    AccessMode getCrawlerAccessMode();

    void setCrawlerAccessMode(final AccessMode mode);

    @AboutConfig
    @Order(20)
    String getAPIUser();

    void setAPIUser(String str);

    @AboutConfig
    @Order(30)
    String getAPIKey();

    void setAPIKey(String str);
}