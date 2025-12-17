package org.jdownloader.plugins.components.youtube;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.txtresource.TranslationFactory;
import org.jdownloader.plugins.components.youtube.itag.AudioBitrate;
import org.jdownloader.plugins.components.youtube.itag.AudioCodec;
import org.jdownloader.plugins.components.youtube.itag.AudioType;
import org.jdownloader.plugins.components.youtube.itag.ImageQuality;
import org.jdownloader.plugins.components.youtube.itag.QualitySortIdentifier;
import org.jdownloader.plugins.components.youtube.itag.VideoCodec;
import org.jdownloader.plugins.components.youtube.itag.VideoFrameRate;
import org.jdownloader.plugins.components.youtube.itag.VideoResolution;
import org.jdownloader.plugins.components.youtube.variants.FileContainer;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.settings.staticreferences.CFG_YOUTUBE;

public class YT_STATICS {
    public static final YoutubeConfig           CFG = PluginJsonConfig.get(YoutubeConfig.class);
    public static Map<VideoResolution, Integer> SORTIDS_VIDEO_RESOLUTION;
    public static Map<ImageQuality, Integer>    SORTIDS_IMAGE_QUALITY;
    public static Map<VideoCodec, Integer>      SORTIDS_VIDEO_CODEC;
    public static Map<VideoFrameRate, Integer>  SORTIDS_VIDEO_FRAMERATE;
    public static Map<AudioBitrate, Integer>    SORTIDS_AUDIO_BITRATE;
    public static List<QualitySortIdentifier>   SORTIDS;
    public static Map<AudioCodec, Integer>      SORTIDS_AUDIO_CODEC;
    public static Map<AudioType, Integer>       SORTIDS_AUDIO_TYPE;
    public static Map<FileContainer, Integer>   SORTIDS_FILE_CONTAINER;
    public static Map<String, Integer>          SUBTITLE_PREFERRENCE_MAP;
    static {
        updateSorterMaps();
        GenericConfigEventListener<String[]> listener = new GenericConfigEventListener<String[]>() {
            @Override
            public void onConfigValidatorError(KeyHandler<String[]> keyHandler, String[] invalidValue, ValidationException validateException) {
            }

            @Override
            public void onConfigValueModified(KeyHandler<String[]> keyHandler, String[] newValue) {
                updateSorterMaps();
            }
        };
        CFG_YOUTUBE.QUALITY_SORT_IDENTIFIER_ORDER.getEventSender().addListener(listener);
        CFG_YOUTUBE.QUALITY_SORT_IDENTIFIER_ORDER_AUDIO_BITRATE.getEventSender().addListener(listener);
        CFG_YOUTUBE.QUALITY_SORT_IDENTIFIER_ORDER_AUDIO_CODEC.getEventSender().addListener(listener);
        CFG_YOUTUBE.QUALITY_SORT_IDENTIFIER_ORDER_AUDIO_TYPE.getEventSender().addListener(listener);
        CFG_YOUTUBE.QUALITY_SORT_IDENTIFIER_ORDER_VIDEO_CODEC.getEventSender().addListener(listener);
        CFG_YOUTUBE.QUALITY_SORT_IDENTIFIER_ORDER_VIDEO_FRAMERATE.getEventSender().addListener(listener);
        CFG_YOUTUBE.QUALITY_SORT_IDENTIFIER_ORDER_RESOLUTION.getEventSender().addListener(listener);
        CFG_YOUTUBE.QUALITY_SORT_IDENTIFIER_ORDER_FILETYPE.getEventSender().addListener(listener);
        CFG_YOUTUBE.QUALITY_SORT_IDENTIFIER_ORDER_IMAGE_QUALITY.getEventSender().addListener(listener);
    }

    private static void updateSorterMaps() {
        SORTIDS_VIDEO_RESOLUTION = update(VideoResolution.class, CFG.getQualitySortIdentifierOrderResolution());
        SORTIDS_IMAGE_QUALITY = update(ImageQuality.class, CFG.getQualitySortIdentifierOrderImageQuality());
        SORTIDS_VIDEO_CODEC = update(VideoCodec.class, CFG.getQualitySortIdentifierOrderVideoCodec());
        SORTIDS_VIDEO_FRAMERATE = update(VideoFrameRate.class, CFG.getQualitySortIdentifierOrderVideoFramerate());
        SORTIDS_AUDIO_BITRATE = update(AudioBitrate.class, CFG.getQualitySortIdentifierOrderAudioBitrate());
        SORTIDS_AUDIO_CODEC = update(AudioCodec.class, CFG.getQualitySortIdentifierOrderAudioCodec());
        SORTIDS_AUDIO_TYPE = update(AudioType.class, CFG.getQualitySortIdentifierOrderAudioType());
        SORTIDS_FILE_CONTAINER = update(FileContainer.class, CFG.getQualitySortIdentifierOrderFiletype());
        final ArrayList<QualitySortIdentifier> sortIds = new ArrayList<QualitySortIdentifier>();
        for (String s : CFG.getQualitySortIdentifierOrder()) {
            sortIds.add(QualitySortIdentifier.valueOf(s));
        }
        SORTIDS = sortIds;
        updateSubtitlesSorter();
    }

    protected static void updateSubtitlesSorter() {
        final Map<String, Integer> prefSubtitles = new HashMap<String, Integer>();
        final String[] prefs = CFG_YOUTUBE.CFG.getPreferedSubtitleLanguages();
        int prefID = 0;
        if (prefs != null && prefs.length > 0) {
            for (int i = 0; i < prefs.length; i++) {
                if (prefs[i] != null) {
                    prefSubtitles.put(prefs[i].toLowerCase(Locale.ENGLISH), prefID++);
                }
            }
        }
        prefSubtitles.put(TranslationFactory.getDesiredLanguage().toLowerCase(Locale.ENGLISH), prefID++);
        prefSubtitles.put(Locale.getDefault().getLanguage().toLowerCase(Locale.ENGLISH), prefID++);
        prefSubtitles.put("en", prefID++);
        SUBTITLE_PREFERRENCE_MAP = prefSubtitles;
    }

    private static <T extends Enum> Map<T, Integer> update(Class<T> class1, String[] values) {
        final Map<T, Integer> ret = new LinkedHashMap<T, Integer>();
        final List<T> lst = defaultEnumList(class1, values);
        for (int i = 0; i < lst.size(); i++) {
            ret.put(lst.get(i), lst.size() - i);
        }
        return ret;
    }

    public static <T extends Enum> List<T> defaultEnumList(Class<T> cls, String[] values) {
        if (values == null) {
            values = new String[] {};
        }
        final Set<T> ret = new LinkedHashSet<T>();
        for (final String value : values) {
            try {
                final Field field = cls.getDeclaredField(value);
                final Object enumValue = field.get(null);
                if (enumValue != null) {
                    ret.add((T) enumValue);
                }
            } catch (Throwable e) {
            }
        }
        for (final Enum enumConstant : cls.getEnumConstants()) {
            try {
                ret.add((T) enumConstant);
            } catch (Throwable e) {
            }
        }
        return new ArrayList<T>(ret);
    }
}