package org.jdownloader.plugins.components.youtube;

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.Storage;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.AbstractCustomValueGetter;
import org.appwork.storage.config.annotations.CustomStorageName;
import org.appwork.storage.config.annotations.CustomValueGetter;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultFactory;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultJsonObject;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.storage.config.annotations.RequiresRestart;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.appwork.storage.config.annotations.StorageHandlerFactoryAnnotation;
import org.appwork.storage.config.defaults.AbstractDefaultFactory;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.youtube.configpanel.YoutubeVariantCollection;
import org.jdownloader.plugins.components.youtube.itag.AudioBitrate;
import org.jdownloader.plugins.components.youtube.itag.AudioCodec;
import org.jdownloader.plugins.components.youtube.itag.AudioType;
import org.jdownloader.plugins.components.youtube.itag.QualitySortIdentifier;
import org.jdownloader.plugins.components.youtube.itag.VideoCodec;
import org.jdownloader.plugins.components.youtube.itag.VideoFrameRate;
import org.jdownloader.plugins.components.youtube.itag.VideoResolution;
import org.jdownloader.plugins.components.youtube.keepForCompatibilitye.YoutubeCompatibility;
import org.jdownloader.plugins.components.youtube.variants.FileContainer;
import org.jdownloader.plugins.components.youtube.variants.VariantGroup;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;
import org.jdownloader.translate._JDT;

@CustomStorageName("youtube/Youtube")
@StorageHandlerFactoryAnnotation(YoutubeConfigStorageHandlerFactory.class)
@PluginHost(host = "youtube.com", type = Type.HOSTER)
public interface YoutubeConfig extends PluginConfigInterface {
    public abstract static class AbstractEnumOrderFixList extends AbstractCustomValueGetter<String[]> {
        private Class<? extends Enum> cls;

        public AbstractEnumOrderFixList(Class<? extends Enum> cls) {
            this.cls = cls;
        }

        @Override
        public String[] getValue(KeyHandler<String[]> keyHandler, String[] value) {
            List<? extends Enum> enums = YT_STATICS.defaultEnumList(cls, value);
            String[] ret = new String[enums.size()];
            for (int i = 0; i < ret.length; i++) {
                ret[i] = enums.get(i).name();
            }
            return ret;
        }
    }

    class DefaultConvertSubtitleVariantMode extends AbstractDefaultFactory<SubtitleVariantMode> {
        @Override
        public SubtitleVariantMode getDefaultValue(KeyHandler<SubtitleVariantMode> keyHandler) {
            final Storage storage;
            if (keyHandler != null && (storage = keyHandler.getStorageHandler().getPrimitiveStorage(keyHandler)) != null) {
                final String oldKey = "subtitlecopyforeachvideovariant";
                final Object oldValue = storage.get(oldKey, null);
                if (oldValue != null && StringUtils.equalsIgnoreCase("false", oldValue.toString())) {
                    storage.remove(oldKey);
                    return SubtitleVariantMode.DISABLED;
                }
            }
            return SubtitleVariantMode.COPY_AND_KEEP;
        }
    }

    public static enum SubtitleVariantMode {
        DISABLED,
        COPY_AND_KEEP,
        COPY_AND_DELETE
    }

    public static enum IfUrlisAPlaylistAction implements LabelInterface {
        ASK {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_IfUrlisAPlaylistAction_ASK();
            }
        },
        NOTHING {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_IfUrlisAPlaylistAction_NOTHING();
            }
        },
        PROCESS {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_IfUrlisAPlaylistAction_PROCESS();
            }
        };
    }

    public static enum IfUrlisAVideoAndPlaylistAction implements LabelInterface {
        ASK {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_IfUrlisAVideoAndPlaylistAction_ASK();
            }
        },
        NOTHING {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_IfUrlisAVideoAndPlaylistAction_NOTHING();
            }
        },
        PLAYLIST_ONLY {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_IfUrlisAVideoAndPlaylistAction_PLAYLIST_ONLY();
            }
        },
        VIDEO_ONLY {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_IfUrlisAVideoAndPlaylistAction_VIDEO_ONLY();
            }
        };
    }

    public static enum ProfileCrawlMode implements LabelInterface {
        PLAYLIST {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_ProfileCrawlModePlaylist();
            }
        },
        USER {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_ProfileCrawlModeVideosTab();
            }
        };
    }

    public static class QualitySortIdentifierOrderAudioBitrate extends AbstractEnumOrderFixList {
        public QualitySortIdentifierOrderAudioBitrate() {
            super(AudioBitrate.class);
        }
    }

    public static class QualitySortIdentifierOrderAudioCodec extends AbstractEnumOrderFixList {
        public QualitySortIdentifierOrderAudioCodec() {
            super(AudioCodec.class);
        }
    }

    public static class QualitySortIdentifierOrderAudioType extends AbstractEnumOrderFixList {
        public QualitySortIdentifierOrderAudioType() {
            super(AudioType.class);
        }
    }

    public static class QualitySortIdentifierOrderDefaultGetter extends AbstractEnumOrderFixList {
        public QualitySortIdentifierOrderDefaultGetter() {
            super(QualitySortIdentifier.class);
        }
    }

    public static class QualitySortIdentifierOrderFiletype extends AbstractEnumOrderFixList {
        public QualitySortIdentifierOrderFiletype() {
            super(FileContainer.class);
        }
    }

    public static class QualitySortIdentifierOrderResolution extends AbstractEnumOrderFixList {
        public QualitySortIdentifierOrderResolution() {
            super(VideoResolution.class);
        }
    }

    public static class QualitySortIdentifierOrderVideoCodec extends AbstractEnumOrderFixList {
        public QualitySortIdentifierOrderVideoCodec() {
            super(VideoCodec.class);
        }
    }

    public static class QualitySortIdentifierOrderVideoFramerate extends AbstractEnumOrderFixList {
        public QualitySortIdentifierOrderVideoFramerate() {
            super(VideoFrameRate.class);
        }
    }

    public static final double AVOID_DOUBLE_MOD = 100000d;
    static Object              NOTHING          = YoutubeCompatibility.moveJSonFiles("youtube/Youtube");

    @AboutConfig
    @DefaultStringValue(value = "*VIDEO_NAME* (*AUDIO_BITRATE*kbit_*AUDIO_CODEC**-LNG[DISPLAY]*).*EXT*")
    @DefaultOnNull
    String getAudioFilenamePattern();

    @AboutConfig
    @DefaultJsonObject("[]")
    List<AudioBitrate> getBlacklistedAudioBitrates();

    @AboutConfig
    List<AudioCodec> getBlacklistedAudioCodecs();

    @AboutConfig
    @DefaultJsonObject("[]")
    List<FileContainer> getBlacklistedFileContainers();

    @AboutConfig
    @DefaultJsonObject("[]")
    List<VariantGroup> getBlacklistedGroups();

    @AboutConfig
    @DefaultJsonObject("[]")
    List<Projection> getBlacklistedProjections();

    @AboutConfig
    @DefaultJsonObject("[]")
    List<VideoResolution> getBlacklistedResolutions();

    @AboutConfig
    @DefaultJsonObject("[]")
    List<VideoCodec> getBlacklistedVideoCodecs();

    @AboutConfig
    @DefaultJsonObject("[]")
    List<VideoFrameRate> getBlacklistedVideoFramerates();

    @DefaultJsonObject("[]")
    List<AudioBitrate> getChooseVariantDialogBlacklistedAudioBitrates();

    @DefaultJsonObject("[]")
    List<AudioCodec> getChooseVariantDialogBlacklistedAudioCodecs();

    @DefaultJsonObject("[]")
    List<FileContainer> getChooseVariantDialogBlacklistedFileContainers();

    @DefaultJsonObject("[]")
    List<VariantGroup> getChooseVariantDialogBlacklistedGroups();

    @DefaultJsonObject("[]")
    List<Projection> getChooseVariantDialogBlacklistedProjections();

    @DefaultJsonObject("[]")
    List<VideoResolution> getChooseVariantDialogBlacklistedResolutions();

    @DefaultJsonObject("[]")
    List<VideoCodec> getChooseVariantDialogBlacklistedVideoCodecs();

    @DefaultJsonObject("[]")
    List<VideoFrameRate> getChooseVariantDialogBlacklistedVideoFramerates();

    @AboutConfig
    @DefaultEnumValue("P_4320")
    @DefaultOnNull
    VideoResolution getMaxVideoResolution();

    void setMaxVideoResolution(VideoResolution resolution);

    @AboutConfig
    @DefaultIntValue(15)
    int getChunksCount();

    List<YoutubeVariantCollection> getCollections();

    @AboutConfig
    @DefaultOnNull
    @DefaultStringValue("*VIDEO_NAME* (*QUALITY*).*EXT*")
    String getDescriptionFilenamePattern();

    @AboutConfig
    List<VariantIDStorable> getDisabledVariants();

    // @DefaultBooleanValue(false)
    // @AboutConfig
    // boolean isPreferHttpsEnabled();
    //
    // void setPreferHttpsEnabled(boolean b);
    // @DefaultBooleanValue(true)
    // @AboutConfig
    // boolean isBestGroupVariantEnabled();
    //
    // void setBestGroupVariantEnabled(boolean b);
    @AboutConfig
    @DescriptionForConfigEntry("Use this if you want to get more than 1 subtitle per video. [\"de\", \"ar\", \"zh-HK\"] or [\"*\"] for all")
    List<String> getExtraSubtitles();

    @AboutConfig
    @DescriptionForConfigEntry("Use this if you want to get auto translated subtitles. [\"de\", \"en\", \"ar\", \"zh-HK\", \"ru\", \"tr\"]")
    List<String> getAutoTranslatedSubtitles();

    @AboutConfig
    @Deprecated
    String getFilenamePattern();

    @DefaultOnNull
    @AboutConfig
    @DefaultStringValue("*VIDEO_NAME* (*QUALITY*).*EXT*")
    String getImageFilenamePattern();

    @AboutConfig
    @DefaultEnumValue("ASK")
    YoutubeConfig.IfUrlisAPlaylistAction getLinkIsPlaylistUrlAction();

    @AboutConfig
    @DefaultEnumValue("ASK")
    YoutubeConfig.IfUrlisAVideoAndPlaylistAction getLinkIsVideoAndPlaylistUrlAction();

    @AboutConfig
    @DescriptionForConfigEntry("Define how channels/profiles should be crawled. Channels can be crawled as their 'Uploaded by' playlist which can be found on the website under '/@channelname/featured' -> 'Play all' or just all videos you can see on the website under '/@channelname/videos' -> Tab called 'Videos'.")
    @DefaultEnumValue("PLAYLIST")
    YoutubeConfig.ProfileCrawlMode getProfileCrawlMode();

    void setProfileCrawlMode(YoutubeConfig.ProfileCrawlMode mode);

    @AboutConfig
    @DescriptionForConfigEntry("Define max items to be crawled when channel/playlist/profile is added. -1 = unlimited, 0 = disable channel/playlist/profile crawler")
    @SpinnerValidator(min = -1, max = 100000, step = 100)
    @DefaultIntValue(-1)
    int getPlaylistAndProfileCrawlerMaxItemsLimit();

    void setPlaylistAndProfileCrawlerMaxItemsLimit(int i);

    public static enum ChannelCrawlerSortMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto";
            }
        },
        LATEST {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_ChannelCrawlerSortModeLatest();
            }
        },
        POPULAR {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_ChannelCrawlerSortModePopular();
            }
        },
        OLDEST {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_ChannelCrawlerSortModeOldest();
            }
        };
    }

    @AboutConfig
    @DescriptionForConfigEntry("Define in which order items of crawled channels should roughtly be added. This has no influence on the final sort order in the linkgrabber! It only has an influence on what is roughly crawled first.")
    @DefaultEnumValue("AUTO")
    YoutubeConfig.ChannelCrawlerSortMode getChannelCrawlerPreferredSortMode();

    void setChannelCrawlerPreferredSortMode(YoutubeConfig.ChannelCrawlerSortMode sort);

    public static enum ChannelPlaylistCrawlerPackagingMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto";
            }
        },
        GROUP_ALL_VIDEOS_AS_SINGLE_PACKAGE {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_ChannelPlaylistCrawlerPackagingModeSingle();
            }
        },
        GROUP_EACH_VIDEO_INDIVIDUALLY {
            @Override
            public String getLabel() {
                return _JDT.T.YoutubeDash_ChannelPlaylistCrawlerPackagingModePerVideo();
            }
        };
    }

    @AboutConfig
    @DescriptionForConfigEntry("Channels/playlist crawler: Grouping mode")
    @DefaultEnumValue("AUTO")
    YoutubeConfig.ChannelPlaylistCrawlerPackagingMode getChannelPlaylistCrawlerPackagingMode();

    void setChannelPlaylistCrawlerPackagingMode(YoutubeConfig.ChannelPlaylistCrawlerPackagingMode mode);

    @AboutConfig
    @DescriptionForConfigEntry("If enabled, list of crawled videoIDs will be reversed before processing. Important: This will also change the index numbers inside the YouTube URLs you get in JD later on! This setting is only applied if no crawl limit is set or if the playlist contains less items than the crawl limit.")
    @DefaultBooleanValue(false)
    boolean isProcessPlaylistItemsInReverseOrder();

    void setProcessPlaylistItemsInReverseOrder(boolean b);

    public static enum PlaylistDupeDetectionMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto";
            }
        },
        ALLOW_PLAYLIST_AND_SINGLE_VIDEO {
            @Override
            public String getLabel() {
                return "Allow single video and same video as part of playlist";
            }
        },
        AVOID_DUPLICATES {
            @Override
            public String getLabel() {
                return "Use global duplicate detection: Avoid duplicates";
            }
        };
    }

    @AboutConfig
    @DescriptionForConfigEntry("Controls duplicate detection behavior for videos added as part of a playlist")
    @DefaultEnumValue("AUTO")
    @DefaultOnNull
    @RequiresRestart("A JDownloader Restart is Required")
    YoutubeConfig.PlaylistDupeDetectionMode getPlaylistDupeDetectionMode();

    void setPlaylistDupeDetectionMode(YoutubeConfig.PlaylistDupeDetectionMode mode);

    @AboutConfig
    @DescriptionForConfigEntry("Subtitle Variant Mode - Controls how downloaded subtitle files are processed to match their corresponding video files.\r\n\r\nDISABLED: Subtitle files are left with their original filenames as downloaded, with no additional processing or renaming.\r\n\r\nCOPY_AND_KEEP: Subtitle files are copied and renamed to match their associated video files (e.g., \"Video Title.English.srt\"), while the original subtitle files are preserved. This creates both the original and the renamed versions.\r\n\r\nCOPY_AND_DELETE: Subtitle files are renamed to match their associated video files and the original subtitle files are deleted after successful processing. This results in only the renamed subtitle files that correspond to video filenames.\r\n\r\nThe renamed subtitle files use the video's base filename with the subtitle language appended (when available), making them compatible with media players that automatically load subtitles based on matching filenames.")
    @DefaultFactory(DefaultConvertSubtitleVariantMode.class)
    YoutubeConfig.SubtitleVariantMode getSubtitleVariantMode();

    public void setSubtitleVariantMode(YoutubeConfig.SubtitleVariantMode mode);

    @AboutConfig
    @DefaultOnNull
    @DefaultStringValue("*VIDEO_NAME*")
    String getPackagePattern();

    void setPackagePattern(String pattern);

    @AboutConfig
    @DefaultOnNull
    @DefaultStringValue("*CHANNEL_NAME*")
    String getPackagePatternForChannelPackages();

    void setPackagePatternForChannelPackages(String pattern);

    @AboutConfig
    @DefaultOnNull
    @DefaultStringValue("*PLAYLIST_CREATOR* - *PLAYLIST_TITLE*")
    String getPackagePatternForPlaylists();

    void setPackagePatternForPlaylists(String pattern);

    @AboutConfig
    String[] getPreferedSubtitleLanguages();

    @AboutConfig
    @CustomValueGetter(QualitySortIdentifierOrderDefaultGetter.class)
    String[] getQualitySortIdentifierOrder();

    @CustomValueGetter(QualitySortIdentifierOrderAudioBitrate.class)
    @AboutConfig
    String[] getQualitySortIdentifierOrderAudioBitrate();

    @AboutConfig
    @CustomValueGetter(QualitySortIdentifierOrderAudioCodec.class)
    String[] getQualitySortIdentifierOrderAudioCodec();

    @AboutConfig
    @CustomValueGetter(QualitySortIdentifierOrderAudioType.class)
    String[] getQualitySortIdentifierOrderAudioType();

    // @AboutConfig
    // ArrayList<YoutubeCustomVariantStorable> getCustomVariants();
    //
    // void setCustomVariants(ArrayList<YoutubeCustomVariantStorable> list);
    @AboutConfig
    @CustomValueGetter(QualitySortIdentifierOrderFiletype.class)
    String[] getQualitySortIdentifierOrderFiletype();

    @AboutConfig
    @CustomValueGetter(QualitySortIdentifierOrderResolution.class)
    String[] getQualitySortIdentifierOrderResolution();

    @CustomValueGetter(QualitySortIdentifierOrderVideoCodec.class)
    @AboutConfig
    String[] getQualitySortIdentifierOrderVideoCodec();

    @CustomValueGetter(QualitySortIdentifierOrderVideoFramerate.class)
    @AboutConfig
    String[] getQualitySortIdentifierOrderVideoFramerate();

    @AboutConfig
    @DefaultOnNull
    @DefaultStringValue("*VIDEO_NAME** (LNG[DISPLAY])*.*EXT*")
    String getSubtitleFilenamePattern();

    @AboutConfig
    ArrayList<String> getSubtitleWhiteList();

    @AboutConfig
    @DescriptionForConfigEntry("ID Pattern for dupe filtering. Tags: *CONTAINER*,*AUDIO_BITRATE*,*AUDIO_CODEC*,*DEMUX*,*SPATIAL*,*LNG*")
    @DefaultOnNull
    @DefaultStringValue("*LNG* *AUDIO_BITRATE* *LNG* *SPATIAL* kbit/s.*CONTAINER*")
    @RequiresRestart("A JDownloader Restart is Required")
    String getVariantNamePatternAudio();

    @AboutConfig
    @DefaultStringValue("*LNG* *3D* *360* *HEIGHT*p *FPS*fps - *VIDEO_CODEC*-Video & *AUDIO_CODEC*-Audio.*CONTAINER*")
    @RequiresRestart("A JDownloader Restart is Required")
    @DescriptionForConfigEntry("ID Pattern for dupe filtering. Tags: *CONTAINER*,*HEIGHT*,*FPS*,*AUDIO_CODEC*,*VIDEO_CODEC*,*3D*,*AUDIO_BITRATE*,*SPATIAL*,*LNG*")
    @DefaultOnNull
    String getVariantNamePatternVideo();

    @AboutConfig
    @DefaultOnNull
    @DefaultStringValue("*3D* *360* *VIDEO_NAME* (*H*p_*FPS*fps_*VIDEO_CODEC*-*AUDIO_BITRATE*kbit_*AUDIO_CODEC**-LNG[DISPLAY]*).*EXT*")
    String getVideoFilenamePattern();

    @AboutConfig
    @DefaultBooleanValue(false)
    @RequiresRestart("A JDownloader Restart is Required")
    boolean isAdvancedVariantNamesEnabled();

    @DefaultBooleanValue(false)
    @AboutConfig
    boolean isAndroidSupportEnabled();

    @AboutConfig
    @DefaultBooleanValue(true)
    boolean isChooseAlternativeForMassChangeOrAddDialog();

    @DefaultBooleanValue(false)
    @AboutConfig
    boolean isCustomChunkValueEnabled();

    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("Disable this if you do not want to use the new DASH Format. This will disable AUDIO only Downloads, and High Quality Video Downloads")
    @AboutConfig
    boolean isExternMultimediaToolUsageEnabled();

    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("Disable this if you do not want to mux new DASH Format. This will result in seperate audio/video streams.")
    @AboutConfig
    boolean isDASHMuxingEnabled();

    void setDASHMuxingEnabled(boolean b);

    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("Appends String representing internal variant information ('#variant=...') to URL the user gets when copying URL of added item.")
    @AboutConfig
    boolean isEnableIncludeVariantStringInContentURLs();

    void setAdvancedVariantNamesEnabled(boolean b);

    void setAndroidSupportEnabled(boolean b);

    void setAudioFilenamePattern(String name);

    void setBlacklistedAudioBitrates(List<AudioBitrate> v);

    void setBlacklistedAudioCodecs(List<AudioCodec> v);

    void setBlacklistedFileContainers(List<FileContainer> v);

    void setBlacklistedGroups(List<VariantGroup> v);

    void setBlacklistedProjections(List<Projection> v);

    void setBlacklistedResolutions(List<VideoResolution> v);

    void setBlacklistedVideoCodecs(List<VideoCodec> v);

    void setBlacklistedVideoFramerates(List<VideoFrameRate> v);

    void setChooseAlternativeForMassChangeOrAddDialog(boolean v);

    void setChooseVariantDialogBlacklistedAudioBitrates(List<AudioBitrate> v);

    void setChooseVariantDialogBlacklistedAudioCodecs(List<AudioCodec> v);

    void setChooseVariantDialogBlacklistedFileContainers(List<FileContainer> v);

    void setChooseVariantDialogBlacklistedGroups(List<VariantGroup> v);

    void setChooseVariantDialogBlacklistedProjections(List<Projection> v);

    void setChooseVariantDialogBlacklistedResolutions(List<VideoResolution> v);

    void setChooseVariantDialogBlacklistedVideoCodecs(List<VideoCodec> v);

    void setChooseVariantDialogBlacklistedVideoFramerates(List<VideoFrameRate> v);

    void setChunksCount(int count);

    @AboutConfig
    void setCollections(List<YoutubeVariantCollection> links);

    void setCustomChunkValueEnabled(boolean b);

    void setDescriptionFilenamePattern(String name);

    void setDisabledVariants(List<VariantIDStorable> variants);

    void setExternMultimediaToolUsageEnabled(boolean b);

    void setExtraSubtitles(List<String> list);

    void setAutoTranslatedSubtitles(List<String> list);

    @Deprecated
    void setFilenamePattern(String name);

    void setImageFilenamePattern(String name);

    void setLinkIsPlaylistUrlAction(YoutubeConfig.IfUrlisAPlaylistAction action);

    void setLinkIsVideoAndPlaylistUrlAction(YoutubeConfig.IfUrlisAVideoAndPlaylistAction action);

    void setPreferedSubtitleLanguages(String[] lngKeys);

    void setQualitySortIdentifierOrder(String[] s);

    void setQualitySortIdentifierOrderAudioBitrate(String[] s);

    void setQualitySortIdentifierOrderAudioCodec(String[] s);

    void setQualitySortIdentifierOrderAudioType(String[] s);

    void setQualitySortIdentifierOrderFiletype(String[] s);

    void setQualitySortIdentifierOrderResolution(String[] s);

    void setQualitySortIdentifierOrderVideoCodec(String[] s);

    void setQualitySortIdentifierOrderVideoFramerate(String[] s);

    void setEnableIncludeVariantStringInContentURLs(boolean b);

    void setSubtitleFilenamePattern(String name);

    void setSubtitleWhiteList(ArrayList<String> list);

    void setVariantNamePatternAudio(String type);

    void setVariantNamePatternVideo(String type);

    void setVideoFilenamePattern(String name);

    @AboutConfig
    @DescriptionForConfigEntry("If enabled, playlist position will be added to the beginning of all filenames regardless of the customizes filename patterns.")
    @DefaultBooleanValue(false)
    boolean isPlaylistItemsIncludePlaylistPositionAtBeginningOfFilenames();

    void setPlaylistItemsIncludePlaylistPositionAtBeginningOfFilenames(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("Preload 200kb and use FFProbe to detect the actual Audio Bitrate.")
    boolean isDoExtendedAudioBitrateLookupEnabled();

    void setDoExtendedAudioBitrateLookupEnabled(boolean b);

    @AboutConfig
    @DefaultIntValue(5)
    @DescriptionForConfigEntry("If a Variant is not available, JD will try up to * alternatives")
    int getAutoAlternativeSearchDepths();

    void setAutoAlternativeSearchDepths(int i);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("If a Variant is not available, JD will try to find alternatives")
    boolean isAutoAlternativeSearchEnabled();

    void setAutoAlternativeSearchEnabled(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("If disabled, JD will ignore the Youtube Collections, but create an extra link for every variant")
    boolean isCollectionMergingEnabled();

    void setCollectionMergingEnabled(boolean b);

    @RequiresRestart("A JDownloader Restart is Required")
    @AboutConfig
    @DefaultBooleanValue(true)
    boolean isSegmentLoadingEnabled();

    void setSegmentLoadingEnabled(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    boolean isMetaDataEnabled();

    void setMetaDataEnabled(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("If enabled, crawler will avoid video items that already exist in the linkgrabber at a very early stage. This can speed up processing of added YT video links.")
    boolean isCrawlDupeCheckEnabled();

    void setCrawlDupeCheckEnabled(boolean b);
}