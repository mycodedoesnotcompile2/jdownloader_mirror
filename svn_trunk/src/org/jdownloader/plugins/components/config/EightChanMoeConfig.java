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

@PluginHost(host = "8chan.moe", type = Type.CRAWLER)
public interface EightChanMoeConfig extends PluginConfigInterface {
    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("Prefer server filenames over plugins' default filenames?")
    @Order(10)
    boolean isPreferServerFilenamesOverPluginDefaultFilenames();

    void setPreferServerFilenamesOverPluginDefaultFilenames(boolean b);

    public static enum POSTANCHORMODE implements LabelInterface {
        POST_ONLY {
            @Override
            public String getLabel() {
                return "Only download post content, or nothing if empty/offline";
            }
        },
        POST_OR_THREAD {
            @Override
            public String getLabel() {
                return "Only download post content, or whole thread when empty/offline";
            }
        },
        THREAD {
            @Override
            public String getLabel() {
                return "Always download whole thread, never individual posts";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("THREAD")
    @DefaultOnNull
    @DescriptionForConfigEntry("What to do if the link contains an anchor with post id, eg /1.html#2 ?")
    @Order(20)
    POSTANCHORMODE getPostAnchorMode();

    void setPostAnchorMode(POSTANCHORMODE mode);

}