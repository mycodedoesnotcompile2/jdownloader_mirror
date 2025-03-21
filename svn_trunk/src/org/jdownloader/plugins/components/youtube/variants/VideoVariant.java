package org.jdownloader.plugins.components.youtube.variants;

import java.util.List;
import java.util.Locale;

import javax.swing.Icon;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.extmanager.Log;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.components.youtube.Projection;
import org.jdownloader.plugins.components.youtube.YoutubeClipData;
import org.jdownloader.plugins.components.youtube.YoutubeConfig;
import org.jdownloader.plugins.components.youtube.YoutubeStreamData;
import org.jdownloader.plugins.components.youtube.itag.AudioBitrate;
import org.jdownloader.plugins.components.youtube.itag.AudioCodec;
import org.jdownloader.plugins.components.youtube.itag.AudioType;
import org.jdownloader.plugins.components.youtube.itag.VideoCodec;
import org.jdownloader.plugins.components.youtube.itag.VideoResolution;
import org.jdownloader.plugins.components.youtube.itag.YoutubeITAG;
import org.jdownloader.plugins.components.youtube.variants.generics.GenericVideoInfo;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.translate._JDT;

import jd.plugins.DownloadLink;

public class VideoVariant extends AbstractVariant<GenericVideoInfo> implements VideoInterface, AudioInterface {
    public VideoVariant(VariantBase base) {
        super(base);
    }

    @Override
    public String createAdvancedName() {
        switch (getProjection()) {
        case SPHERICAL:
            return "360° VR, " + super.createAdvancedName();
        case ANAGLYPH_3D:
            return "3D, " + super.createAdvancedName();
        case SPHERICAL_3D:
            return "360° VR, 3D, " + super.createAdvancedName();
        case NORMAL:
        default:
            return super.createAdvancedName();
        }
    }

    @Override
    public void setJson(String jsonString) {
        setGenericInfo(JSonStorage.restoreFromString(jsonString, new TypeRef<GenericVideoInfo>() {
        }));
    }

    protected String uniqueIDString = null;

    public synchronized String _getUniqueId() {
        if (uniqueIDString == null) {
            uniqueIDString = super._getUniqueId();
            final String aId = getGenericInfo().getaId();
            if (aId != null) {
                uniqueIDString += ".aid" + aId;
            }
            if (getGenericInfo().isDrc()) {
                uniqueIDString += ".drc";
            }
        }
        return uniqueIDString;
    }

    private static final Icon   VIDEO           = new AbstractIcon(IconKey.ICON_VIDEO, 16);
    private static final String TYPE_ID_PATTERN = PluginJsonConfig.get(YoutubeConfig.class).getVariantNamePatternVideo();

    @Override
    public String _getName(Object caller) {
        String id = TYPE_ID_PATTERN;
        id = id.replace("*CONTAINER*", getBaseVariant().getContainer().name() + "");
        id = id.replace("*HEIGHT*", getVideoHeight() + "");
        id = id.replace("*FPS*", getVideoFrameRate() + "");
        id = id.replace("*AUDIO_CODEC*", getAudioCodec().getLabel() + "");
        id = id.replace("*VIDEO_CODEC*", getVideoCodec() + "");
        id = id.replace("*AUDIO_BITRATE*", getAudioBitrate().getKbit() + "");
        id = id.replace("*LNG*", StringUtils.valueOrEmpty(AudioVariant.getAudioIdForPattern(this)));
        switch (getProjection()) {
        case SPHERICAL:
            id = id.replace("*360*", "[360°]");
            id = id.replace("*3D*", "");
            break;
        case ANAGLYPH_3D:
            id = id.replace("*3D*", "[3D]");
            id = id.replace("*360*", "");
            break;
        case SPHERICAL_3D:
            id = id.replace("*3D*", "[3D]");
            id = id.replace("*360*", "[360°]");
            break;
        default:
        case NORMAL:
            id = id.replace("*360*", "");
            id = id.replace("*3D*", "");
            break;
        }
        switch (getiTagAudioOrVideoItagEquivalent().getAudioCodec()) {
        case AAC_SPATIAL:
        case VORBIS_SPATIAL:
        case OPUS_SPATIAL:
            id = id.replace("*SPATIAL*", _JDT.T.YOUTUBE_surround());
            break;
        default:
            id = id.replace("*SPATIAL*", "");
        }
        id = id.trim().replace(" - ", "-").replaceAll("[ ]+", " ");
        return id.trim();
    }

    public String getTypeId() {
        String id = TYPE_ID_PATTERN;
        id = id.replace("*CONTAINER*", getBaseVariant().getContainer().name() + "");
        id = id.replace("*HEIGHT*", getVideoHeight() + "");
        id = id.replace("*FPS*", getVideoFrameRate() + "");
        id = id.replace("*AUDIO_CODEC*", getAudioCodec() + "");
        id = id.replace("*VIDEO_CODEC*", getVideoCodec() + "");
        id = id.replace("*AUDIO_BITRATE*", getAudioBitrate().getKbit() + "");
        id = id.replace("*LNG*", StringUtils.valueOrEmpty(getAudioId()));
        switch (getProjection()) {
        case SPHERICAL:
            id = id.replace("*360*", "360°");
            id = id.replace("*3D*", "");
            break;
        case ANAGLYPH_3D:
            id = id.replace("*3D*", "3D");
            id = id.replace("*360*", "");
            break;
        case SPHERICAL_3D:
            id = id.replace("*3D*", "3D");
            id = id.replace("*360*", "360°");
            break;
        case NORMAL:
        default:
            id = id.replace("*360*", "");
            id = id.replace("*3D*", "");
            break;
        }
        switch (getiTagAudioOrVideoItagEquivalent().getAudioCodec()) {
        case AAC_SPATIAL:
        case VORBIS_SPATIAL:
        case OPUS_SPATIAL:
            id = id.replace("*SURROUND*", "Spatial");
            break;
        default:
            id = id.replace("*SURROUND*", "");
        }
        id = id.trim().replaceAll("\\s+", "_").toUpperCase(Locale.ENGLISH);
        return id;
    }

    public Projection getProjection() {
        return getGenericInfo().getProjection();
    }

    @Override
    protected void fill(YoutubeClipData vid, List<YoutubeStreamData> audio, List<YoutubeStreamData> video, List<YoutubeStreamData> data) {
        if (vid != null) {
            //
            getGenericInfo().setProjection(vid.getProjection());
        }
        if (getBaseVariant().name().contains("_3D")) {
            getGenericInfo().setProjection(Projection.ANAGLYPH_3D);
        }
        if (video != null) {
            for (YoutubeStreamData stream : video) {
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    if (video.size() > 1) {
                        DebugMode.debugger();
                    }
                }
                if (stream.getHeight() > 0) {
                    getGenericInfo().setHeight(stream.getHeight());
                }
                if (stream.getWidth() > 0) {
                    getGenericInfo().setWidth(stream.getWidth());
                }
                if (stream.getFps() != null) {
                    try {
                        int intf = Integer.parseInt(new Regex(stream.getFps(), "(\\d+)").getMatch(0));
                        getGenericInfo().setFps(intf);
                    } catch (Throwable e) {
                        Log.log(e);
                    }
                }
            }
        }
        if (audio != null && vid != null) {
            for (final YoutubeStreamData a : audio) {
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    if (audio.size() > 1) {
                        DebugMode.debugger();
                    } else if (getGenericInfo().getaId() != null && !StringUtils.equals(getGenericInfo().getaId(), a.getLngId())) {
                        DebugMode.debugger();
                    } else if (getGenericInfo().isDrc() != a.isDrc()) {
                        DebugMode.debugger();
                    }
                }
                getGenericInfo().setaId(a.getLngId());
                getGenericInfo().setDrc(a.isDrc());
                if (a.getBitrate() > 0 && vid.duration > 0 && a.getContentLength() > 0) {
                    final long abr = (8 * a.getContentLength()) / (1024l * vid.duration / 1000);
                    getGenericInfo().setaBitrate((int) abr);
                    break;
                }
            }
        }
    }

    @Override
    public int getVideoHeight() {
        int height = getGenericInfo().getHeight();
        if (height < 3) {
            height = getiTagVideo().getVideoResolution().getHeight();
        }
        return height;
    }

    @Override
    public int getVideoFrameRate() {
        int fps = getGenericInfo().getFps();
        if (fps < 3) {
            fps = (int) Math.ceil(getiTagVideo().getVideoFrameRate().getFps());
        }
        return fps;
    }

    @Override
    public Icon _getIcon(Object caller) {
        return VIDEO;
    }

    @Override
    public String getStandardGroupingID() {
        return getGroup().name() + "_" + getProjection().name();
    }

    @Override
    public String getFileNamePattern(final DownloadLink downloadLink) {
        return PluginJsonConfig.get(YoutubeConfig.class).getVideoFilenamePattern();
    }

    @Override
    public String getFileNameQualityTag() {
        switch (getProjection()) {
        case SPHERICAL:
            return getVideoHeight() + "p " + getVideoFrameRate() + "fps" + " 360VR";
        case ANAGLYPH_3D:
            return getVideoHeight() + "p " + getVideoFrameRate() + "fps" + " 3D";
        case SPHERICAL_3D:
            return getVideoHeight() + "p " + getVideoFrameRate() + "fps" + " 360VR 3D";
        case NORMAL:
        default:
            return getVideoHeight() + "p " + getVideoFrameRate() + "fps";
        }
    }

    public AudioCodec getAudioCodec() {
        return getiTagAudioOrVideoItagEquivalent().getAudioCodec();
    }

    public AudioBitrate getAudioBitrate() {
        final int bitRate = getGenericInfo().getaBitrate();
        if (bitRate > 0) {
            return AudioBitrate.getByInt(bitRate);
        } else {
            return getiTagAudioOrVideoItagEquivalent().getAudioBitrate();
        }
    }

    @Override
    public VideoCodec getVideoCodec() {
        final YoutubeITAG itag = getiTagVideo();
        if (itag == null) {
            return null;
        } else {
            return itag.getVideoCodec();
        }
    }

    @Override
    public VideoResolution getVideoResolution() {
        final YoutubeITAG itag = getiTagVideo();
        if (itag == null) {
            return null;
        } else {
            return itag.getVideoResolution();
        }
    }

    @Override
    public int getVideoWidth() {
        int width = getGenericInfo().getWidth();
        if (width < 3) {
            width = getiTagVideo().getVideoResolution().getWidth();
        }
        return width;
    }

    @Override
    public YoutubeITAG getAudioITAG() {
        return getiTagAudioOrVideoItagEquivalent();
    }

    @Override
    public YoutubeITAG getVideoITAG() {
        return getiTagVideo();
    }

    @Override
    public String getAudioId() {
        return getGenericInfo().getaId();
    }

    @Override
    public boolean isDrc() {
        return getGenericInfo().isDrc();
    }

    @Override
    public AudioType getAudioType() {
        return AudioType.getAudioType(this);
    }

    @Override
    public Locale getAudioLocale() {
        return getGenericInfo()._getLocale();
    }
}
