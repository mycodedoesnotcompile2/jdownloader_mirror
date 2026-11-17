package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "hanime.tv", type = Type.CRAWLER)
public interface HanimeTvConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getPreferredStreamQuality_label() {
            return "Preferred quality of stream links (HLS)";
        }

        public String getPreferredDownloadQuality_label() {
            return "Preferred quality of download links (pixeldrain)";
        }
    }

    public static enum QUALITY implements LabelInterface {
        /** Best available quality. Also used as fallback if the preferred quality is not available. */
        BEST("Best", -1),
        Q1080P("1080p", 1080),
        Q720P("720p", 720),
        Q480P("480p", 480),
        Q360P("360p", 360);

        private final String label;
        private final int    height;

        private QUALITY(final String label, final int height) {
            this.label = label;
            this.height = height;
        }

        @Override
        public String getLabel() {
            return label;
        }

        /** Returns the video height in pixels or -1 for BEST. */
        public int getHeight() {
            return height;
        }
    }

    @AboutConfig
    @Order(100)
    @DefaultEnumValue("BEST")
    @DefaultOnNull
    QUALITY getPreferredStreamQuality();

    void setPreferredStreamQuality(QUALITY quality);

    @AboutConfig
    @Order(110)
    @DefaultEnumValue("BEST")
    @DefaultOnNull
    QUALITY getPreferredDownloadQuality();

    void setPreferredDownloadQuality(QUALITY quality);
}
