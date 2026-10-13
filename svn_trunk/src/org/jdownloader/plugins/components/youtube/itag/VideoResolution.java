package org.jdownloader.plugins.components.youtube.itag;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.appwork.storage.config.annotations.IntegerInterface;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.components.youtube.YT_STATICS;
import org.jdownloader.plugins.components.youtube.variants.AbstractVariant;
import org.jdownloader.plugins.components.youtube.variants.ImageVariant;
import org.jdownloader.plugins.components.youtube.variants.VideoVariant;

public enum VideoResolution implements LabelInterface, IntegerInterface {
    // Order is default quality sort order
    P_7680(4320, 7680), // upright of P_4320
    P_4320(7680, 4320),
    P_3840(2160, 3840), // upright of P_2160
    P_2160(3840, 2160),
    P_1920(1080, 1920),
    P_1440(2560, 1440),
    P_1280(2560, 1280),
    P_1080(1920, 1080),
    @Deprecated
    P_608_1080(608, 1080),
    P_720(1280, 720),
    @Deprecated
    P_1080_608(1080, 608),
    P_480(640, 480),
    P_360(480, 360),
    P_270(480, 270),
    P_240(352, 240),
    P_180(320, 180),
    P_144(256, 144),
    P_90(120, 90),
    P_72(128, 72);

    private static VideoResolution getBestMatchingVideoResolution(VideoVariant videoVariant) {
        final int height = videoVariant.getGenericInfo().getHeight();
        final List<VideoResolution> ret = new ArrayList<VideoResolution>();
        for (VideoResolution res : VideoResolution.values()) {
            if (height >= res.height) {
                if (ret.size() == 0) {
                    ret.add(res);
                } else if (height == res.height) {
                    ret.add(res);
                }
            }
        }
        if (ret.size() > 1) {
            final int width = videoVariant.getGenericInfo().getWidth();
            final Iterator<VideoResolution> it = ret.iterator();
            while (it.hasNext()) {
                final VideoResolution next = it.next();
                if (next.width < width) {
                    it.remove();
                }
            }
        }
        if (ret.size() == 1) {
            return ret.get(0);
        }
        return null;

    }

    private final int height;
    private final int width;

    private VideoResolution(int width, int height) {
        this.height = height;
        this.width = width;
    }

    public int getHeight() {
        return height;
    }

    public String getLabel() {
        return height + "p";
    }

    public int getWidth() {
        return width;
    }

    @Override
    public int getInt() {
        return height;
    }

    public static VideoResolution getByHeight(int height) {
        for (final VideoResolution r : values()) {
            if (r.getHeight() == height) {
                return r;
            }
        }
        return null;
    }

    private static VideoResolution getByVariant(AbstractVariant o1) {
        if (o1 instanceof VideoVariant) {
            final VideoResolution best = VideoResolution.getBestMatchingVideoResolution((VideoVariant) o1);
            if (best != null) {
                // try to find best matching VideoResolution based on actual video height/width and not on ITAG infos
                return best;
            }
            return ((VideoVariant) o1).getVideoResolution();
        } else if (false && o1 instanceof ImageVariant) {
            // now has it's own sorter/handling in ImageQuality and QualitySortIdentifier
            return getByHeight(((ImageVariant) o1).getHeight());
        } else {
            return null;
        }
    }

    public static int getSortId(AbstractVariant v) {
        final VideoResolution res = getByVariant(v);
        if (res == null) {
            return -1;
        } else {
            final Number intObj = YT_STATICS.SORTIDS_VIDEO_RESOLUTION.get(res);
            if (intObj == null) {
                return -1;
            } else {
                return intObj.intValue();
            }
        }
    }
}
