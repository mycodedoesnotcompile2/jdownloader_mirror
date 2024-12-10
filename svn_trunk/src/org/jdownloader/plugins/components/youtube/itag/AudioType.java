package org.jdownloader.plugins.components.youtube.itag;

import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.components.youtube.YT_STATICS;
import org.jdownloader.plugins.components.youtube.variants.AbstractVariant;
import org.jdownloader.plugins.components.youtube.variants.AudioInterface;

public enum AudioType implements LabelInterface {
    ORIGINAL("Original"), // xtags=acont%3Doriginal%
    DUBBED("Dubbed"), // xtags=acont%3Ddubbed
    DUBBED_AUTO("Dubbed Auto");// xtags=acont%3Ddubbed-auto

    private final String label;

    private AudioType(String label) {
        this.label = label;
    }

    @Override
    public String getLabel() {
        return label;
    }

    public static AudioType getAudioType(AudioInterface variant) {
        final String lngId = variant.getAudioId();
        if (lngId == null) {
            return null;
        } else if (lngId.endsWith(".10")) {
            return AudioType.DUBBED_AUTO;
        } else if (lngId.endsWith(".4")) {
            return AudioType.ORIGINAL;
        } else if (lngId.endsWith(".3")) {
            return AudioType.DUBBED;
        } else {
            return null;
        }
    }

    private static AudioType getByVariant(AbstractVariant o1) {
        if (o1 instanceof AudioInterface) {
            return getAudioType((AudioInterface) o1);
        } else {
            return null;
        }
    }

    public static int getSortId(AbstractVariant v) {
        final AudioType res = getByVariant(v);
        if (res == null) {
            return -1;
        } else {
            final Number intObj = YT_STATICS.SORTIDS_AUDIO_TYPE.get(res);
            if (intObj == null) {
                return -1;
            } else {
                return intObj.intValue();
            }
        }
    }
}
