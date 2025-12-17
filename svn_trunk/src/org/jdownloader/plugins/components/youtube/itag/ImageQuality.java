package org.jdownloader.plugins.components.youtube.itag;

import org.appwork.storage.flexijson.mapper.FlexiEnumFallback;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.youtube.YT_STATICS;
import org.jdownloader.plugins.components.youtube.variants.AbstractVariant;
import org.jdownloader.plugins.components.youtube.variants.ImagePlaylistCoverVariant;
import org.jdownloader.plugins.components.youtube.variants.ImageVariant;

public enum ImageQuality {
    HIGHEST(4, 10) {
        @Override
        public String getLocaleName() {
            return _GUI.T.YoutubeVariant_name_IMAGE_MAX();
        }

        public String getLocaleTag() {
            return _GUI.T.YoutubeVariant_filenametag_IMAGE_MAX();
        }
    },
    HIGH(3, 10) {
        @Override
        public String getLocaleName() {
            return _GUI.T.YoutubeVariant_name_IMAGE_HQ();
        }

        public String getLocaleTag() {
            return _GUI.T.YoutubeVariant_filenametag_IMAGE_HQ();
        }
    },
    @FlexiEnumFallback("NORMAL")
    MEDIUM(2, 10) {
        @Override
        public String getLocaleName() {
            return _GUI.T.YoutubeVariant_name_IMAGE_MQ();
        }

        public String getLocaleTag() {
            return _GUI.T.YoutubeVariant_filenametag_IMAGE_MQ();
        }
    },
    LOW(1, 10) {
        @Override
        public String getLocaleName() {
            return _GUI.T.YoutubeVariant_name_IMAGE_LQ();
        }

        public String getLocaleTag() {
            return _GUI.T.YoutubeVariant_filenametag_IMAGE_LQ();
        }
    };
    private double rating = -1;

    private ImageQuality(double rating, double modifier) {
        this.rating = rating / modifier;
    }

    public abstract String getLocaleName();

    public abstract String getLocaleTag();

    public double getRating() {
        return rating;
    }

    public void setRating(double rating) {
        this.rating = rating;
    }

    public static ImageQuality getByVariant(AbstractVariant o1) {
        if (o1 instanceof ImageVariant) {
            return ((ImageVariant) o1).getImageQuality();
        } else if (o1 instanceof ImagePlaylistCoverVariant) {
            return ((ImagePlaylistCoverVariant) o1).getImageQuality();
        } else {
            return null;
        }
    }

    public static int getSortId(AbstractVariant v) {
        final ImageQuality res = getByVariant(v);
        if (res == null) {
            return -1;
        } else {
            final Number intObj = YT_STATICS.SORTIDS_IMAGE_QUALITY.get(res);
            if (intObj == null) {
                return -1;
            } else {
                return intObj.intValue();
            }
        }
    }
}
