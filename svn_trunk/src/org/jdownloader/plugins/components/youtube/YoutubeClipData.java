package org.jdownloader.plugins.components.youtube;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import jd.plugins.DownloadLink;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.youtube.itag.YoutubeITAG;
import org.jdownloader.plugins.components.youtube.variants.AbstractVariant;
import org.jdownloader.plugins.components.youtube.variants.AudioInterface;
import org.jdownloader.plugins.components.youtube.variants.DescriptionVariantInfo;
import org.jdownloader.plugins.components.youtube.variants.SubtitleVariant;
import org.jdownloader.plugins.components.youtube.variants.SubtitleVariantInfo;
import org.jdownloader.plugins.components.youtube.variants.VariantBase;
import org.jdownloader.plugins.components.youtube.variants.VariantInfo;
import org.jdownloader.plugins.components.youtube.variants.VideoVariant;
import org.jdownloader.plugins.components.youtube.variants.YoutubeSubtitleStorable;
import org.jdownloader.settings.staticreferences.CFG_YOUTUBE;

public class YoutubeClipData {
    /**
     *
     */
    public String                             user                     = null;
    public String                             user_alternative         = null;
    public long                               datePublished            = -1;
    public String                             error                    = null;
    public boolean                            ageCheck                 = false;
    public String                             title                    = null;
    public String                             title_alternative        = null;
    public String                             description              = null;
    public String                             description_alternative  = null;
    public String                             channelTitle             = null;
    public String                             channelTitle_alternative = null;
    public int                                channelSize              = -1;

    public String                             videoID                  = null;
    public String                             atID                     = null;

    public String                             category                 = null;
    public int                                duration                 = -1;
    public String                             chapters                 = null;
    public String                             channelID                = null;
    public long                               dateUploaded             = -1;
    public Boolean                            isLiveNow                = null;
    public long                               dateLivestreamStart      = -1;
    public long                               dateLivestreamEnd        = -1;
    public String                             userGooglePlusID         = null;
    public VideoVariant                       bestVideoItag            = null;
    public Map<YoutubeITAG, StreamCollection> streams;
    public ArrayList<YoutubeSubtitleStorable> subtitles;
    public HashMap<String, String>            keywords3D;
    public HashSet<String>                    keywords;
    public String                             approxThreedLayout       = null;
    public String                             views                    = null;

    /**
     * Checks the keywords and guesses if this is a sbs or hou 3d video, but not declared as 3d by youtube
     *
     * @return
     */
    public boolean guessSBSorHOU3D() {
        if (keywords == null) {
            return false;
        }
        final StringBuilder sb = new StringBuilder();
        for (String s : keywords) {
            sb.append(" ").append(s.toLowerCase(Locale.ENGLISH));
        }
        if (title != null) {
            sb.append(" ").append(title.toLowerCase(Locale.ENGLISH));
        }
        if (description != null) {
            sb.append(" ").append(description.toLowerCase(Locale.ENGLISH));
        }
        final String str = sb.toString();
        // should we be using sb instead of title here?
        if (title == null || !title.contains("3d")) {
            return false;
        }
        if (str.contains("sbs")) {
            return true;
        } else if (str.contains("side") && str.contains("by")) {
            return true;
        } else if (str.contains("hou")) {
            return true;
        } else if (str.contains("cardboard")) {
            return true;
        } else {
            return false;
        }
    }

    public Projection getProjection() {
        int highestProjection = -1;
        for (Entry<YoutubeITAG, StreamCollection> s : streams.entrySet()) {
            for (YoutubeStreamData stream : s.getValue()) {
                highestProjection = Math.max(highestProjection, stream.getProjectionType());
            }
        }
        int layout3D = -1;
        try {
            layout3D = approxThreedLayout == null ? -1 : Integer.parseInt(approxThreedLayout);
        } catch (Throwable e) {
        }
        if (highestProjection == 0) {
            return Projection.NORMAL;
        } else if (highestProjection == 2 && layout3D != 3) {
            return Projection.SPHERICAL;
        } else if ((highestProjection == 2 && layout3D == 3) || highestProjection == 3) {
            return Projection.SPHERICAL_3D;
        } else if (guessSBSorHOU3D() || is3D()) {
            return Projection.ANAGLYPH_3D;
        } else {
            return Projection.NORMAL;
        }
    }

    private boolean is3D() {
        // from yt player js
        if ("1".equals(approxThreedLayout)) {
            return true;
        } else if (keywords3D != null) {
            final String enable = keywords3D.get("enable");
            if (enable == null) {
                return false;
            } else if (StringUtils.equals(enable, "true")) {
                return true;
            } else if (StringUtils.equals(enable, "LR")) {
                return true;
            } else if (StringUtils.equals(enable, "RL")) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    @Override
    public String toString() {
        return videoID + "/" + title;
    }

    public YoutubeClipData(final String videoID) {
        this.videoID = videoID;
    }

    public boolean copyTo(YoutubeClipData dest) {
        if (dest.videoID == null || StringUtils.equals(videoID, dest.videoID)) {
            dest.videoID = videoID;
            dest.title = title;
            dest.title_alternative = title_alternative;
            dest.category = category;
            dest.channelTitle = channelTitle;
            dest.chapters = chapters;
            dest.channelTitle_alternative = channelTitle_alternative;
            dest.channelSize = channelSize;
            dest.user = user;
            dest.atID = atID;
            dest.user_alternative = user_alternative;
            dest.bestVideoItag = bestVideoItag;
            dest.datePublished = datePublished;
            dest.dateLivestreamStart = dateLivestreamStart;
            dest.dateLivestreamEnd = dateLivestreamEnd;
            dest.userGooglePlusID = userGooglePlusID;
            dest.channelID = channelID;
            dest.duration = duration;
            dest.dateUploaded = dateUploaded;
            dest.views = views;
            dest.description = description;
            dest.description_alternative = description_alternative;
            dest.streams = streams;
            dest.subtitles = subtitles;
            dest.keywords = keywords;
            dest.keywords3D = keywords3D;
            dest.error = error;
            dest.ageCheck = ageCheck;
            dest.approxThreedLayout = approxThreedLayout;
            return true;
        } else {
            return false;
        }
    }

    public void copyToDownloadLink(final DownloadLink dest) {
        setValue(dest, YoutubeHelper.YT_TITLE, title);
        setValue(dest, YoutubeHelper.YT_TITLE_ALTERNATIVE, title_alternative);
        setValue(dest, YoutubeHelper.YT_CATEGORY, category);
        setValue(dest, YoutubeHelper.YT_3D, is3D());
        setValue(dest, YoutubeHelper.YT_CHANNEL_TITLE, channelTitle);
        setValue(dest, YoutubeHelper.YT_CHANNEL_TITLE_ALTERNATIVE, channelTitle_alternative);
        setValue(dest, YoutubeHelper.YT_CHANNEL_SIZE, channelSize);
        setValue(dest, YoutubeHelper.YT_USER_NAME, user);
        setValue(dest, YoutubeHelper.YT_USER_NAME_ALTERNATIVE, user_alternative);
        if (bestVideoItag != null) {
            setValue(dest, YoutubeHelper.YT_BEST_VIDEO, bestVideoItag.getBaseVariant().getiTagVideo().name());
            setValue(dest, YoutubeHelper.YT_BEST_VIDEO_HEIGHT, String.valueOf(bestVideoItag.getVideoHeight()));
        }
        setValue(dest, YoutubeHelper.YT_CHAPTERS, chapters);
        setValue(dest, YoutubeHelper.YT_DATE, datePublished);
        setValue(dest, YoutubeHelper.YT_GOOGLE_PLUS_ID, userGooglePlusID);
        setValue(dest, YoutubeHelper.YT_CHANNEL_ID, channelID);
        setValue(dest, YoutubeHelper.YT_ATID, atID);
        setValue(dest, YoutubeHelper.YT_DURATION, duration);
        setValue(dest, YoutubeHelper.YT_DATE_UPLOAD, dateUploaded);
        setValue(dest, YoutubeHelper.YT_DATE_LIVESTREAM_START, dateLivestreamStart);
        setValue(dest, YoutubeHelper.YT_DATE_LIVESTREAM_END, dateLivestreamEnd);
        setValue(dest, YoutubeHelper.YT_VIEWS, views);
        dest.getTempProperties().setProperty(YoutubeHelper.YT_DESCRIPTION, description);
        dest.getTempProperties().setProperty(YoutubeHelper.YT_DESCRIPTION_ALTERNATIVE, description_alternative);
    }

    protected boolean setValue(DownloadLink dest, String key, Object value) {
        if (dest.hasProperty(key) || value == null) {
            return false;
        } else if (value instanceof String) {
            if (StringUtils.isEmpty((String) value)) {
                return false;
            } else {
                dest.setProperty(key, value);
                return true;
            }
        } else if (value instanceof Number) {
            if (((Number) value).longValue() == -1) {
                return false;
            } else {
                dest.setProperty(key, value);
                return true;
            }
        } else {
            dest.setProperty(key, value);
            return true;
        }
    }

    public StreamCollection getStreams(final YoutubeITAG itag, AbstractVariant variant) {
        if (itag == null) {
            return null;
        }
        StreamCollection ret = streams.get(itag);
        if (ret == null) {
            // check alternatives
            List<YoutubeITAG> alternatives = YoutubeITAG.getTagList(itag.getITAG());
            if (alternatives != null) {
                for (YoutubeITAG tag : alternatives) {
                    ret = streams.get(tag);
                    if (ret != null) {
                        break;
                    }
                }
            }
        }
        if (ret != null && itag.getAudioCodec() != null && variant instanceof AudioInterface) {
            final String audioId = ((AudioInterface) variant).getAudioId();
            ret = splitByLngId(ret).get(audioId);
        }
        return ret;
    }

    public List<VariantInfo> findSubtitleVariants() {
        final List<VariantInfo> allSubtitles = new ArrayList<VariantInfo>();
        for (final YoutubeSubtitleStorable si : subtitles) {
            final SubtitleVariantInfo vi = new SubtitleVariantInfo(new SubtitleVariant(si), this);
            allSubtitles.add(vi);
        }
        return allSubtitles;
    }

    public ArrayList<VariantInfo> findDescriptionVariant() {
        final ArrayList<VariantInfo> descriptions = new ArrayList<VariantInfo>();
        final String descText = description;
        if (StringUtils.isNotEmpty(descText)) {
            final DescriptionVariantInfo vi = new DescriptionVariantInfo(descText, this);
            descriptions.add(vi);
        }
        return descriptions;
    }

    private Map<String, StreamCollection> splitByLngId(StreamCollection collection) {
        final Map<String, StreamCollection> map = new HashMap<String, StreamCollection>();
        for (final YoutubeStreamData stream : collection) {
            final String lng = stream.getLngId();
            StreamCollection col = map.get(lng);
            if (col == null) {
                col = new StreamCollection();
                map.put(lng, col);
            }
            col.add(stream);
        }
        return map;
    }

    public List<VariantInfo> findVariants() {
        return findVariants(streams);
    }

    public List<VariantInfo> findVariants(final Map<YoutubeITAG, StreamCollection> streams) {
        final ArrayList<VariantInfo> ret = new ArrayList<VariantInfo>();
        for (VariantBase v : VariantBase.values()) {
            if (v.isVideoToolRequired() && !CFG_YOUTUBE.CFG.isExternMultimediaToolUsageEnabled()) {
                continue;
            }
            if (!v.isValidFor(this)) {
                continue;
            }
            // System.out.println("test for " + v);
            StreamCollection audios = null;
            StreamCollection video = null;
            StreamCollection data = null;
            boolean valid = v.getiTagVideo() != null || v.getiTagAudio() != null || v.getiTagData() != null;
            if (v.getiTagVideo() != null) {
                video = streams.get(v.getiTagVideo());
                if (video == null) {
                    valid = false;
                }
            }
            if (v.getiTagAudio() != null) {
                audios = streams.get(v.getiTagAudio());
                if (audios == null) {
                    valid = false;
                }
            }
            if (v.getiTagData() != null) {
                data = streams.get(v.getiTagData());
                if (data == null) {
                    valid = false;
                }
            }
            if (valid) {
                if (audios != null) {
                    for (StreamCollection audio : splitByLngId(audios).values()) {
                        final AbstractVariant abstractVariant = AbstractVariant.get(v, this, audio, video, data);
                        if (abstractVariant != null) {
                            final VariantInfo vi = new VariantInfo(abstractVariant, audio, video, data);
                            ret.add(vi);
                        }
                    }
                } else {
                    final AbstractVariant abstractVariant = AbstractVariant.get(v, this, audios, video, data);
                    if (abstractVariant != null) {
                        final VariantInfo vi = new VariantInfo(abstractVariant, audios, video, data);
                        ret.add(vi);
                    }
                }
            }
        }
        return ret;
    }
}