//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.decrypter;

import java.awt.Dialog.ModalityType;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.parser.UrlQuery;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.plugins.components.google.GoogleHelper;
import org.jdownloader.plugins.components.youtube.ClipDataCache;
import org.jdownloader.plugins.components.youtube.Projection;
import org.jdownloader.plugins.components.youtube.StreamCollection;
import org.jdownloader.plugins.components.youtube.VariantIDStorable;
import org.jdownloader.plugins.components.youtube.YoutubeClipData;
import org.jdownloader.plugins.components.youtube.YoutubeConfig;
import org.jdownloader.plugins.components.youtube.YoutubeConfig.ChannelCrawlerSortMode;
import org.jdownloader.plugins.components.youtube.YoutubeConfig.ChannelPlaylistCrawlerPackagingMode;
import org.jdownloader.plugins.components.youtube.YoutubeConfig.IfUrlisAPlaylistAction;
import org.jdownloader.plugins.components.youtube.YoutubeConfig.IfUrlisAVideoAndPlaylistAction;
import org.jdownloader.plugins.components.youtube.YoutubeConfig.ProfileCrawlMode;
import org.jdownloader.plugins.components.youtube.YoutubeHelper;
import org.jdownloader.plugins.components.youtube.YoutubeStreamData;
import org.jdownloader.plugins.components.youtube.configpanel.AbstractVariantWrapper;
import org.jdownloader.plugins.components.youtube.configpanel.YoutubeVariantCollection;
import org.jdownloader.plugins.components.youtube.itag.AudioBitrate;
import org.jdownloader.plugins.components.youtube.itag.AudioCodec;
import org.jdownloader.plugins.components.youtube.itag.VideoCodec;
import org.jdownloader.plugins.components.youtube.itag.VideoFrameRate;
import org.jdownloader.plugins.components.youtube.itag.VideoResolution;
import org.jdownloader.plugins.components.youtube.variants.AbstractVariant;
import org.jdownloader.plugins.components.youtube.variants.AudioInterface;
import org.jdownloader.plugins.components.youtube.variants.FileContainer;
import org.jdownloader.plugins.components.youtube.variants.ImageVariant;
import org.jdownloader.plugins.components.youtube.variants.SubtitleVariant;
import org.jdownloader.plugins.components.youtube.variants.VariantGroup;
import org.jdownloader.plugins.components.youtube.variants.VariantInfo;
import org.jdownloader.plugins.components.youtube.variants.VideoVariant;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;
import org.jdownloader.settings.staticreferences.CFG_YOUTUBE;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.packagecontroller.AbstractNodeVisitor;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 51258 $", interfaceVersion = 3, names = {}, urls = {})
public class TbCmV2 extends PluginForDecrypt {
    /* Shorted wait time between requests when JDownloader is run in IDE to allow for faster debugging. */
    private static final int DDOS_WAIT_MAX        = Application.isJared(null) ? 1000 : 10;
    private static final int DDOS_INCREASE_FACTOR = 15;

    public TbCmV2(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public void init() {
        Browser.setRequestIntervalLimitGlobal(this.getHost(), 100);
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "youtube.com", "music.youtube.com", "youtube-nocookie.com", "yt.not.allowed", "youtube.googleapis.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            String pattern = "https?://(?:(?:www|m)\\.)?" + buildHostsPatternPart(domains) + "/";
            pattern += "(";
            pattern += "embed(\\?v=|/)" + VIDEO_ID_PATTERN + ".*";
            pattern += "|live/" + VIDEO_ID_PATTERN;
            pattern += "|watch.*";
            pattern += "|shorts/" + VIDEO_ID_PATTERN + ".*";
            pattern += "|(?:view_play_list|playlist)\\?.+";
            pattern += "|watch_videos\\?.+";
            pattern += "|video_ids=.+";
            pattern += "|channel/.+";
            pattern += "|c/.+";
            pattern += "|user/.+";
            pattern += "|@.+";
            pattern += ")";
            ret.add(pattern);
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    /**
     * Returns a playlistID from provided url.
     */
    private String getListIDFromUrl(final String url) {
        try {
            final UrlQuery query = UrlQuery.parse(url);
            String playlistID = query.get("list");
            if (playlistID == null) {
                /* Older URLs */
                playlistID = query.get("p");
            }
            return playlistID;
        } catch (final MalformedURLException e) {
            e.printStackTrace();
            return null;
        }
    }

    public static final String VIDEO_ID_PATTERN = "([A-Za-z0-9\\-_]{11})";

    public static boolean isValidVideoID(final String id) {
        if (id == null) {
            return false;
        } else if (id.matches(VIDEO_ID_PATTERN)) {
            return true;
        } else {
            return false;
        }
    }

    public static String getVideoIDFromUrl(final String url) {
        String vuid = new Regex(url, "(?i)v=" + VIDEO_ID_PATTERN).getMatch(0);
        if (vuid != null) {
            return vuid;
        }
        vuid = new Regex(url, "(?i)/v/" + VIDEO_ID_PATTERN).getMatch(0);
        if (vuid != null) {
            return vuid;
        }
        vuid = new Regex(url, "(?i)/shorts/" + VIDEO_ID_PATTERN).getMatch(0);
        if (vuid != null) {
            return vuid;
        }
        vuid = new Regex(url, "(?i)/embed/(?!videoseries\\?)" + VIDEO_ID_PATTERN).getMatch(0);
        if (vuid != null) {
            return vuid;
        }
        vuid = new Regex(url, "(?i)/live/" + VIDEO_ID_PATTERN).getMatch(0);
        return vuid;
    }

    private String getChannelIDFromUrl(final String url) {
        return new Regex(url, "(?i)/channel/([^/\\?]+)").getMatch(0);
    }

    private String getUsernameFromUrl(final String url) {
        String userName = new Regex(url, "(?i)/user/([^/\\?#]+)").getMatch(0);
        if (userName != null) {
            return userName;
        }
        userName = new Regex(url, "(?i)https?://[^/]+/@([^/\\?#]+)").getMatch(0);
        if (userName != null) {
            return userName;
        }
        userName = new Regex(url, "(?i)https?://[^/]+/c/([^/\\?#]+)").getMatch(0);
        return userName;
    }

    /** Returns supported tab names from URLs e.g. "shorts" or "videos". */
    private static String getChannelTabNameFromURL(final String url) {
        final String ret = new Regex(url, "(?i)/(featured|playlists|releases|shorts|streams|videos).*$").getMatch(0);
        if (ret != null) {
            return ret.toLowerCase(Locale.ENGLISH);
        } else {
            return ret;
        }
    }

    private boolean linkCollectorContainsEntryByID(final String videoID) {
        final AtomicBoolean containsFlag = new AtomicBoolean(false);
        LinkCollector.getInstance().visitNodes(new AbstractNodeVisitor<CrawledLink, CrawledPackage>() {
            @Override
            public Boolean visitPackageNode(CrawledPackage pkg) {
                if (containsFlag.get()) {
                    return null;
                } else {
                    return Boolean.TRUE;
                }
            }

            @Override
            public Boolean visitChildrenNode(CrawledLink node) {
                if (containsFlag.get()) {
                    return null;
                } else {
                    if (StringUtils.equalsIgnoreCase(getHost(), node.getHost())) {
                        final DownloadLink downloadLink = node.getDownloadLink();
                        if (downloadLink != null && StringUtils.equals(videoID, downloadLink.getStringProperty(YoutubeHelper.YT_ID))) {
                            containsFlag.set(true);
                            return null;
                        }
                    }
                    return Boolean.TRUE;
                }
            }
        }, true);
        return containsFlag.get();
    }

    private YoutubeConfig                    cfg;
    private static Object                    DIALOGLOCK = new Object();
    private String                           videoID;
    private String                           playlistID;
    private String                           channelID;
    private String                           userName;
    private Map<String, Map<String, Object>> globalPropertiesForDownloadLink;
    private YoutubeHelper                    helper;

    @Override
    protected DownloadLink createOfflinelink(final String link, final String filename, final String message) {
        final DownloadLink ret = super.createOfflinelink(link, filename, message);
        logger.log(new Exception("Debug:" + filename + "|" + message));
        return ret;
    }

    protected Object putGlobalProperty(final String targetID, final String key, final Object value) {
        Map<String, Object> map = globalPropertiesForDownloadLink.get(targetID);
        if (map == null) {
            map = new HashMap<String, Object>();
            globalPropertiesForDownloadLink.put(targetID, map);
        }
        return map.put(key, value);
    }

    protected Object getGlobalProperty(final String targetID, final String key) {
        Map<String, Object> map = globalPropertiesForDownloadLink.get(targetID);
        if (map == null) {
            return null;
        }
        return map.get(key);
    }

    protected void setGlobalProperty(final String targetID, final DownloadLink dest) {
        final Map<String, Object> globalProperties = globalPropertiesForDownloadLink.get(null);
        if (globalProperties != null) {
            for (final Entry<String, Object> es : globalProperties.entrySet()) {
                if (es.getKey() != null && !dest.hasProperty(es.getKey())) {
                    dest.setProperty(es.getKey(), es.getValue());
                }
            }
        }
        final Map<String, Object> targetProperties = targetID != null ? globalPropertiesForDownloadLink.get(targetID) : null;
        if (targetProperties != null) {
            for (final Entry<String, Object> es : targetProperties.entrySet()) {
                if (es.getKey() != null && !dest.hasProperty(es.getKey())) {
                    dest.setProperty(es.getKey(), es.getValue());
                }
            }
        }
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        // nullify, for debugging purposes!
        videoID = null;
        playlistID = null;
        channelID = null;
        userName = null;
        globalPropertiesForDownloadLink = new HashMap<String, Map<String, Object>>();
        cfg = PluginJsonConfig.get(YoutubeConfig.class);
        if (StringUtils.containsIgnoreCase(param.getCryptedUrl(), "yt.not.allowed") && !cfg.isAndroidSupportEnabled()) {
            /*
             * Important! Neither touch nor question this as long as there are references to "yt.not.allowed" in
             * jd.controlling.linkcrawler.LinkCrawler.java.
             */
            logger.info("Returning nothing because: Android support is disabled");
            return new ArrayList<DownloadLink>();
        }
        // TODO: Maybe remove this as we're not modifying this URL anymore and also all methods to extract information out of YT URLs work
        // domain-independent.
        String cleanedurl = param.getCryptedUrl();
        final String requestedVariantString = new Regex(cleanedurl, "(?i)\\#variant=(\\S*)").getMatch(0);
        AbstractVariant requestedVariant = null;
        if (StringUtils.isNotEmpty(requestedVariantString)) {
            requestedVariant = AbstractVariant.get(Base64.decodeToString(Encoding.htmlDecode(requestedVariantString)));
            cleanedurl = cleanedurl.replaceAll("(?i)\\#variant=\\S+", "");
        }
        /**
         * 2024-07-05 e.g.
         * https://www.google.com/url?sa=t&source=web&rct=j&opi=123456&url=https://www.youtube.com/watch%3Fv%3DREDACTED&ved=REDACTED
         * &usg=REDACTED </br>
         * We can safely url-decode this URL as the items we are looking for are not encoded anyways, all IDs are [a-z0-9_-]
         */
        cleanedurl = Encoding.htmlDecode(cleanedurl);
        videoID = getVideoIDFromUrl(cleanedurl);
        // for watch_videos, found within youtube.com music
        final String video_ids_comma_separated = new Regex(cleanedurl, "(?i)video_ids=([a-zA-Z0-9\\-_,]+)").getMatch(0);
        if (video_ids_comma_separated != null) {
            // first uid in array is the video the user copy url on.
            videoID = new Regex(video_ids_comma_separated, "(" + VIDEO_ID_PATTERN + ")").getMatch(0);
        }
        playlistID = getListIDFromUrl(cleanedurl);
        userName = getUsernameFromUrl(cleanedurl);
        channelID = getChannelIDFromUrl(cleanedurl);
        final String channelTabName = getChannelTabNameFromURL(cleanedurl);
        final Pattern pattern_channel_legacyurl_2 = Pattern.compile("https?://[^/]+/c/([^/]+).*", Pattern.CASE_INSENSITIVE);
        final Regex legacyurl = new Regex(cleanedurl, "(?i)https?://[^/]+/user/([^/]+).*");
        final Regex legacyurl2 = new Regex(cleanedurl, pattern_channel_legacyurl_2);
        if (new Regex(cleanedurl, pattern_channel_legacyurl_2).patternFind() && channelTabName == null) {
            /* Small workaround because "/c/<username>$" URLs would lead to zero results. */
            cleanedurl = "https://www.youtube.com/c/" + this.userName + "/videos";
        }
        helper = new YoutubeHelper(br, getLogger());
        br.setFollowRedirects(true);
        if (helper.isConsentCookieRequired()) {
            helper.setConsentCookie(br, null);
        }
        helper.login(getLogger(), false);
        if (StringUtils.isEmpty(channelID) && StringUtils.isEmpty(userName) && StringUtils.isEmpty(playlistID) && StringUtils.isEmpty(videoID)) {
            /*
             * This should be a rare case but it can happen since we are supporting a lot of different URL formats and parameters inside
             * those URLs can be anywhere.
             */
            logger.info("Unsupported URL");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String usernameOrChannelID = this.userName != null ? this.userName : this.channelID;
        putGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_ID, playlistID);
        putGlobalProperty(null, YoutubeHelper.YT_CHANNEL_ID, channelID);
        putGlobalProperty(null, YoutubeHelper.YT_USER_NAME, userName);
        /* @Developer: Enable this boolean if pagination is broken and you are unable to quickly fix it. */
        final boolean paginationIsBroken = false;
        final short maxItemsPerPage = 100;
        final ArrayList<YoutubeClipData> videoIdsToAdd = new ArrayList<YoutubeClipData>();
        int userDefinedMaxPlaylistOrProfileItemsLimit = cfg.getPlaylistAndProfileCrawlerMaxItemsLimit();
        final String playlistHandlingLogtextForUserDisabledCrawlerByLimitSetting = "Doing nothing because user has disabled channel/playlist crawler by setting limit to 0";
        String playlistHandlingHumanReadableTypeOfUrlToCrawl = null;
        String playlistHandlingHumanReadableTitle = null;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>() {
            @Override
            public boolean add(DownloadLink e) {
                distribute(e);
                return super.add(e);
            }
        };
        if (StringUtils.isEmpty(playlistID) && StringUtils.isEmpty(userName) && !StringUtils.isEmpty(videoID)) {
            /* Single video */
            final String indexFromAddedURLStr = new Regex(cleanedurl, "(?i)index=(\\d+)").getMatch(0);
            if (indexFromAddedURLStr != null) {
                putGlobalProperty(videoID, YoutubeHelper.YT_PLAYLIST_POSITION, Integer.parseInt(indexFromAddedURLStr));
            }
            videoIdsToAdd.add(new org.jdownloader.plugins.components.youtube.YoutubeClipData(videoID));
        } else {
            /* Channel/Playlist/User or single Video + playlist in one URL. */
            synchronized (DIALOGLOCK) {
                if (this.isAbort()) {
                    logger.info("Thread Aborted!");
                    throw new InterruptedException();
                }
                /* Ask user: Prevents accidental crawling of entire Play-List or Channel-List or User-List. */
                IfUrlisAPlaylistAction playListAction = cfg.getLinkIsPlaylistUrlAction();
                CrawledLink checkSource = getCurrentLink().getSourceLink();
                while (checkSource != null) {
                    // this playlist is result of /releases crawling, don't ask again
                    final String checkURL = checkSource.getURL();
                    if (canHandle(checkURL) && (StringUtils.endsWithCaseInsensitive(checkURL, "/releases") || StringUtils.containsIgnoreCase(checkURL, "/channel/"))) {
                        playListAction = IfUrlisAPlaylistAction.PROCESS;
                        break;
                    } else {
                        checkSource = checkSource.getSourceLink();
                    }
                }
                if (StringUtils.equalsIgnoreCase(channelTabName, "playlists")) {
                    final String title = "All playlists of channel/user " + usernameOrChannelID;
                    if (playListAction == IfUrlisAPlaylistAction.ASK) {
                        String messageDialogText = "<html>JDownloader does not support crawling all playlists of a channel!";
                        messageDialogText += "<br>What would you like JDownloader to do instead?";
                        messageDialogText += "<br><a href=\"https://support.jdownloader.org/knowledgebase/article/collect-and-download-links-from-unsupported-websites\">Hint on how to collect & add all playlists of a YouTube channel</a>";
                        messageDialogText += "<br>If you wish to hide this dialog, you can pre-select your preferred action under Settings -> Plugins -> youtube.com.</html>";
                        final ConfirmDialog confirm = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN | Dialog.STYLE_HTML, title, messageDialogText, null, "Crawl channel videos instead", "Do nothing") {
                            @Override
                            public ModalityType getModalityType() {
                                return ModalityType.MODELESS;
                            }

                            @Override
                            public boolean isRemoteAPIEnabled() {
                                return true;
                            }
                        };
                        try {
                            UIOManager.I().show(ConfirmDialogInterface.class, confirm).throwCloseExceptions();
                            playListAction = IfUrlisAPlaylistAction.PROCESS;
                        } catch (final DialogCanceledException e) {
                            logger.log(e);
                            playListAction = IfUrlisAPlaylistAction.NOTHING;
                        } catch (final DialogClosedException e) {
                            logger.log(e);
                            playListAction = IfUrlisAPlaylistAction.NOTHING;
                        }
                    }
                    logger.info("LinkIsPlaylistUrlAction:" + playListAction);
                    if (playListAction == IfUrlisAPlaylistAction.NOTHING) {
                        return ret;
                    }
                }
                if (playlistID != null) {
                    playlistHandlingHumanReadableTypeOfUrlToCrawl = "Playlist";
                    playlistHandlingHumanReadableTitle = "Playlist | " + playlistID;
                } else if (userName != null && cfg.getProfileCrawlMode() == ProfileCrawlMode.PLAYLIST) {
                    playlistHandlingHumanReadableTypeOfUrlToCrawl = "Playlist of channel uploads";
                    playlistHandlingHumanReadableTitle = "Playlist | Uploads by " + userName;
                } else if (userName != null && "shorts".equals(channelTabName)) {
                    playlistHandlingHumanReadableTypeOfUrlToCrawl = "Channel Shorts";
                    playlistHandlingHumanReadableTitle = "Channel | " + userName + " | Shorts";
                } else if (userName != null && "releases".equals(channelTabName)) {
                    playlistHandlingHumanReadableTypeOfUrlToCrawl = "Channel Releases";
                    playlistHandlingHumanReadableTitle = "Channel | " + userName + " | Releases";
                } else if (userName != null && "streams".equals(channelTabName)) {
                    playlistHandlingHumanReadableTypeOfUrlToCrawl = "Channel Recorded Livestreams";
                    playlistHandlingHumanReadableTitle = "Channel | " + userName + " | Recorded Livestreams";
                } else if (userName != null) {
                    playlistHandlingHumanReadableTypeOfUrlToCrawl = "Channel";
                    playlistHandlingHumanReadableTitle = "Channel | " + userName;
                } else {
                    playlistHandlingHumanReadableTypeOfUrlToCrawl = "Channel";
                    playlistHandlingHumanReadableTitle = "ChannelID | " + channelID;
                }
                final String buttonTextCrawlPlaylistOrProfile;
                if (paginationIsBroken) {
                    buttonTextCrawlPlaylistOrProfile = playlistHandlingHumanReadableTypeOfUrlToCrawl + " [max first " + maxItemsPerPage + " items]";
                } else if (userDefinedMaxPlaylistOrProfileItemsLimit > 0) {
                    buttonTextCrawlPlaylistOrProfile = playlistHandlingHumanReadableTypeOfUrlToCrawl + " [max first " + userDefinedMaxPlaylistOrProfileItemsLimit + " item(s)]";
                } else {
                    buttonTextCrawlPlaylistOrProfile = playlistHandlingHumanReadableTypeOfUrlToCrawl;
                }
                if ((StringUtils.isNotEmpty(playlistID) || StringUtils.isNotEmpty(channelID) || StringUtils.isNotEmpty(userName)) && StringUtils.isEmpty(videoID)) {
                    if (userDefinedMaxPlaylistOrProfileItemsLimit == 0) {
                        logger.info(playlistHandlingLogtextForUserDisabledCrawlerByLimitSetting);
                        return ret;
                    }
                    if (playListAction == IfUrlisAPlaylistAction.ASK) {
                        String messageDialogText = "This URL is a " + playlistHandlingHumanReadableTypeOfUrlToCrawl + " link. What would you like to do?";
                        if (paginationIsBroken) {
                            messageDialogText += "\r\nJDownloader can only crawl the first " + maxItemsPerPage + " items automatically.\r\nIf there are more than " + maxItemsPerPage + " items, you need to use external tools to grab the single URLs to all videos and add those to JD manually.";
                        }
                        messageDialogText += "\r\nIf you wish to hide this dialog, you can pre-select your preferred option under Settings -> Plugins -> youtube.com.";
                        final ConfirmDialog confirm = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, playlistHandlingHumanReadableTitle, messageDialogText, null, buttonTextCrawlPlaylistOrProfile, "Do nothing") {
                            @Override
                            public ModalityType getModalityType() {
                                return ModalityType.MODELESS;
                            }

                            @Override
                            public boolean isRemoteAPIEnabled() {
                                return true;
                            }
                        };
                        try {
                            UIOManager.I().show(ConfirmDialogInterface.class, confirm).throwCloseExceptions();
                            playListAction = IfUrlisAPlaylistAction.PROCESS;
                        } catch (final DialogCanceledException e) {
                            logger.log(e);
                            playListAction = IfUrlisAPlaylistAction.NOTHING;
                        } catch (final DialogClosedException e) {
                            logger.log(e);
                            playListAction = IfUrlisAPlaylistAction.NOTHING;
                        }
                    }
                    logger.info("LinkIsPlaylistUrlAction:" + playListAction);
                    switch (playListAction) {
                    case PROCESS:
                        break;
                    case NOTHING:
                    default:
                        return ret;
                    }
                } else {
                    /* Check if link contains a video and a playlist */
                    IfUrlisAVideoAndPlaylistAction PlaylistVideoAction = cfg.getLinkIsVideoAndPlaylistUrlAction();
                    if ((StringUtils.isNotEmpty(playlistID) || StringUtils.isNotEmpty(video_ids_comma_separated)) && StringUtils.isNotEmpty(videoID)) {
                        if (PlaylistVideoAction == IfUrlisAVideoAndPlaylistAction.ASK) {
                            /* Ask user */
                            final ConfirmDialog confirm = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, "Crawl video " + this.videoID + " or playlist " + this.playlistID, "This YouTube link contains a video and a playlist. What do you want do download?", null, "Only video", buttonTextCrawlPlaylistOrProfile) {
                                @Override
                                public ModalityType getModalityType() {
                                    return ModalityType.MODELESS;
                                }

                                @Override
                                public boolean isRemoteAPIEnabled() {
                                    return true;
                                }
                            };
                            try {
                                UIOManager.I().show(ConfirmDialogInterface.class, confirm).throwCloseExceptions();
                                PlaylistVideoAction = IfUrlisAVideoAndPlaylistAction.VIDEO_ONLY;
                            } catch (final DialogCanceledException e) {
                                logger.log(e);
                                PlaylistVideoAction = IfUrlisAVideoAndPlaylistAction.PLAYLIST_ONLY;
                            } catch (final DialogClosedException e) {
                                logger.log(e);
                                PlaylistVideoAction = IfUrlisAVideoAndPlaylistAction.NOTHING;
                            }
                        }
                        logger.info("LinkIsVideoAndPlaylistUrlAction:" + PlaylistVideoAction);
                        switch (PlaylistVideoAction) {
                        case PLAYLIST_ONLY:
                            break;
                        case VIDEO_ONLY:
                            videoIdsToAdd.add(new org.jdownloader.plugins.components.youtube.YoutubeClipData(videoID));
                            break;
                        default:
                            logger.info("Doing nothing");
                            return ret;
                        }
                    }
                    if (userDefinedMaxPlaylistOrProfileItemsLimit == 0) {
                        /*
                         * Small workaround: User wants us to crawl playlist but set this limit to 0 -> It would be kind of not logical to
                         * ask him first and then do nothing so let's remove that limit in this case.
                         */
                        userDefinedMaxPlaylistOrProfileItemsLimit = -1;
                    }
                }
            }
        }
        FilePackage channelOrPlaylistPackage = null;
        String singleVideoPackageNamePatternOverride = null;
        if (videoIdsToAdd.isEmpty()) {
            try {
                /* Channel/Profile/Playlist */
                if (userDefinedMaxPlaylistOrProfileItemsLimit == 0) {
                    /* This should never happen but it is a double-check left here on purpose. */
                    logger.info(playlistHandlingLogtextForUserDisabledCrawlerByLimitSetting);
                    return ret;
                }
                if (legacyurl.patternFind() || legacyurl2.patternFind()) {
                    /* Workaround / legacy handling for such old URLs. */
                    // TODO: Maybe add check/error handling for offline/invalid channels
                    /*
                     * Such old URLs can redirect to other usernames e.g. /user/nameOld -> Redirects to '/@nameNew/videos' while we can't
                     * just call '/@nameOld/videos'
                     */
                    logger.info("Checking for changed username | Currently known username: " + this.userName);
                    helper.getPage(br, cleanedurl);
                    checkBasicErrors(br);
                    helper.parse(br);
                    final Map<String, Object> root = helper.getYtInitialData();
                    final Map<String, Object> channelMetadataRenderer = (Map<String, Object>) JavaScriptEngineFactory.walkJson(root, "metadata/channelMetadataRenderer");
                    final String vanityChannelUrl = channelMetadataRenderer.get("vanityChannelUrl").toString();
                    final String externalId = channelMetadataRenderer.get("externalId").toString();
                    if (vanityChannelUrl == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    final String confirmedRealUsername = new Regex(vanityChannelUrl, "@([^/]+)").getMatch(0);
                    if (confirmedRealUsername == null) {
                        logger.info("Looks like this channel does not have any @username username -> Continue with: " + br.getURL());
                    } else if (confirmedRealUsername.equals(this.userName)) {
                        logger.info("Username has not changed and remains: " + this.userName);
                    } else {
                        logger.info("Username has changed! Old: " + this.userName + " | New: " + confirmedRealUsername);
                        this.userName = confirmedRealUsername;
                    }
                    /* Grab and set additional information now that we've already opened the page. */
                    this.channelID = externalId;
                }
                if (cfg.getProfileCrawlMode() == ProfileCrawlMode.PLAYLIST && StringUtils.isEmpty(playlistID)) {
                    /* Crawl profile as playlist -> Look for playlistID of default profile playlist "Uploads by <username>". */
                    if (!StringUtils.isEmpty(userName)) {
                        /*
                         * the user channel parser only parses 1050 videos. this workaround finds the user channel playlist and parses this
                         * playlist instead
                         */
                        logger.info("Trying to find playlistID for profile-playlist 'Uploads by " + userName + "'");
                        if (channelID == null) {
                            logger.info("Trying to find channelID");
                            final String tabName = channelTabName == null ? "featured" : channelTabName;
                            if (br.getRequest() == null || !StringUtils.endsWithCaseInsensitive(br.getURL(), "/" + tabName)) {
                                helper.getPage(br, "https://www.youtube.com/@" + userName + "/" + tabName);
                            }
                            checkBasicErrors(br);
                            helper.parse(br);
                            // channel title isn't user_name. user_name is /user/ reference. check logic in YoutubeHelper.extractData()!
                            final String channelTitle = extractWebsiteTitle(br);
                            if (channelTitle != null) {
                                putGlobalProperty(null, YoutubeHelper.YT_CHANNEL_TITLE, channelTitle);
                            }
                            putGlobalProperty(null, YoutubeHelper.YT_USER_NAME, userName);
                            // you can convert channelid UC[STATICHASH] (UserChanel) ? to UU[STATICHASH] (UsersUpload) which is covered
                            // below
                            channelID = getChannelID(helper, br);
                            if (channelID == null) {
                                logger.info("Unable to find playlistID -> Crawler is broken or profile does not exist");
                                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                            }
                        }
                        /* channelID starts with "UC". We can build channel-playlist out of channel-ID. */
                        playlistID = "UU" + channelID.substring(2);
                    } else if (!StringUtils.isEmpty(channelID)) {
                        /*
                         * you can not use this with /c or /channel based urls, it will pick up false positives. see
                         * https://www.youtube.com/channel/UCOSGEokQQcdAVFuL_Aq8dlg, it will find list=PLc-T0ryHZ5U_FtsfHQopuvQugBvRoVR3j
                         * which only contains 27 videos not the entire channels 112
                         */
                        logger.info("Trying to find playlistID for channel-playlist 'Uploads by " + channelID + "'");
                        helper.getPage(br, YoutubeHelper.getBaseURL() + "/channel/" + channelID);
                        checkBasicErrors(br);
                        playlistID = br.getRegex("(?i)list=([A-Za-z0-9\\-_]+)\"[^<>]+play-all-icon-btn").getMatch(0);
                        if (StringUtils.isEmpty(playlistID) && channelID.startsWith("UC")) {
                            /* channel has no play all button. */
                            playlistID = "UU" + channelID.substring(2);
                        }
                        if (playlistID == null) {
                            logger.info("Unable to find playlistID -> Crawler is broken or profile does not exist");
                            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                        }
                    }
                    if (usernameOrChannelID != null) {
                        this.displayBubbleNotification("Channel/Profile will be crawled as playlist", "As wished per plugin settings, channel " + usernameOrChannelID + " will be crawled as playlist 'Uploads by <username>'.");
                    }
                }
                if (channelID != null) {
                    putGlobalProperty(null, YoutubeHelper.YT_CHANNEL_ID, channelID);
                }
                final ArrayList<YoutubeClipData> playlist = crawlPlaylistOrChannel(helper, br, playlistID, userName, channelID, cleanedurl, userDefinedMaxPlaylistOrProfileItemsLimit);
                if (playlist != null && playlist.size() > 0) {
                    videoIdsToAdd.addAll(playlist);
                    final ChannelPlaylistCrawlerPackagingMode mode = cfg.getChannelPlaylistCrawlerPackagingMode();
                    if (mode == ChannelPlaylistCrawlerPackagingMode.AUTO || mode == ChannelPlaylistCrawlerPackagingMode.GROUP_ALL_VIDEOS_AS_SINGLE_PACKAGE) {
                        String channelOrPlaylistPackageNamePattern;
                        if (this.playlistID != null) {
                            channelOrPlaylistPackageNamePattern = cfg.getPackagePatternForPlaylists();
                        } else {
                            channelOrPlaylistPackageNamePattern = cfg.getPackagePatternForChannelPackages();
                        }
                        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                            /* Dev only: Include number of expected items in packagename for better overview/debugging. */
                            channelOrPlaylistPackageNamePattern = "[" + videoIdsToAdd.size() + " videos] " + channelOrPlaylistPackageNamePattern;
                        }
                        if (mode == ChannelPlaylistCrawlerPackagingMode.AUTO && YoutubeHelper.namePatternContainsSingleVideoSpecificEntries(channelOrPlaylistPackageNamePattern)) {
                            logger.info("User put video-specific replacement patterns into channel/playlist package name pattern in auto mode -> Using that pattern as single video pattern instead: " + channelOrPlaylistPackageNamePattern);
                            singleVideoPackageNamePatternOverride = channelOrPlaylistPackageNamePattern;
                        } else {
                            channelOrPlaylistPackage = FilePackage.getInstance();
                            channelOrPlaylistPackage.setAllowMerge(true);
                            final DownloadLink dummy = this.createDownloadlink("ytdummy");
                            setGlobalProperty(null, dummy);
                            final String formattedPackagename = YoutubeHelper.applyReplacer(channelOrPlaylistPackageNamePattern, helper, dummy);
                            if (!StringUtils.isEmpty(formattedPackagename)) {
                                /* Formatted result is valid -> Use it */
                                channelOrPlaylistPackage.setName(formattedPackagename);
                            } else {
                                /* Formatted result is invalid -> Fallback */
                                logger.info("Invalid result of formattedPackagename -> Fallback to defaults");
                                final String playlistTitle = (String) getGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_TITLE);
                                final String channelName = (String) getGlobalProperty(null, YoutubeHelper.YT_CHANNEL_TITLE);
                                if (playlistTitle != null) {
                                    channelOrPlaylistPackage.setName(playlistTitle);
                                } else {
                                    // final String internalContainerURL =
                                    // helper.getChannelPlaylistCrawlerContainerUrlOverride(param.getCryptedUrl());
                                    String packagename;
                                    if (channelName != null) {
                                        packagename = channelName;
                                    } else if (this.userName != null) {
                                        packagename = this.userName;
                                    } else {
                                        packagename = playlistHandlingHumanReadableTitle;
                                    }
                                    if ("shorts".equals(channelTabName)) {
                                        packagename += " | Shorts";
                                    } else if ("releases".equals(channelTabName)) {
                                        packagename += " | Releases";
                                    }
                                    channelOrPlaylistPackage.setName(packagename);
                                }
                            }
                            final String playlistDescription = (String) getGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_DESCRIPTION);
                            if (playlistDescription != null) {
                                channelOrPlaylistPackage.setComment(playlistDescription);
                            }
                            /*
                             * Set package key if possible so that should the user stop the crawl process and crawl the same item again, new
                             * items will end up in the same package.
                             */
                            if (this.playlistID != null) {
                                /* Playlist or profile crawled as playlist. */
                                channelOrPlaylistPackage.setPackageKey("ytplaylist://" + this.playlistID);
                            } else if (this.channelID != null) {
                                /* User and channel are basically the same, we just prefer to use the channelID if it is given. */
                                channelOrPlaylistPackage.setPackageKey("ytchannel://" + this.channelID);
                            } else if (this.userName != null) {
                                channelOrPlaylistPackage.setPackageKey("ytuser://" + this.userName);
                            }
                        }
                    }
                } else {
                    // TODO: Check if this is still needed
                    videoIdsToAdd.addAll(parseVideoIds(video_ids_comma_separated));
                }
            } catch (final InterruptedException ie) {
                /* Always thow InterruptedException */
                throw ie;
            } catch (final Exception e) {
                if (videoID != null) {
                    /* Last resort fallback */
                    logger.info("Playlist handling failed -> Processing single videoID from URL as fallback");
                    videoIdsToAdd.add(new org.jdownloader.plugins.components.youtube.YoutubeClipData(videoID));
                } else {
                    throw e;
                }
            }
        }
        reversePlaylistPositions: if (this.playlistID != null && cfg.isProcessPlaylistItemsInReverseOrder()) {
            logger.info("Processing crawled playlist in reverse order");
            final Number playlistSizeO = (Number) this.getGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_SIZE);
            if (playlistSizeO == null) {
                logger.info("Cannot reverse playlist positions because playlist size is not known");
                break reversePlaylistPositions;
            }
            Collections.reverse(videoIdsToAdd);
            /* Correct positions in video elements */
            final int playlistSize = playlistSizeO.intValue();
            for (final YoutubeClipData vid : videoIdsToAdd) {
                final int playListPosition = (Integer) getGlobalProperty(vid.videoID, YoutubeHelper.YT_PLAYLIST_POSITION);
                final int newPlaylistPosition = (playlistSize - playListPosition) + 1;
                putGlobalProperty(vid.videoID, YoutubeHelper.YT_PLAYLIST_POSITION, newPlaylistPosition);
            }
        }
        final boolean isChannelPlaylist = playlistID != null && playlistID.startsWith("UU");
        final boolean isCrawlDupeCheckEnabled = cfg.isCrawlDupeCheckEnabled();
        final Set<String> videoIDsdupeCheck = new HashSet<String>();
        int videoidindex = -1;
        String lastVideoFailedErrorMessage = null;
        for (final YoutubeClipData crawledvid : videoIdsToAdd) {
            if (this.isAbort()) {
                /**
                 * IMPORTANT: DO NOT MOVE THIS CHECK ANYWHERE ELSE!! <br>
                 * Otherwise abort may never happen when all video items run into errors!
                 */
                throw new InterruptedException("Aborted!");
            }
            videoidindex++;
            logger.info("Processing item " + videoidindex + "/" + videoIdsToAdd.size() + " | VideoID: " + crawledvid);
            String mainVideoDupeID = crawledvid.videoID;
            final String playlistID = (String) getGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_ID);
            if (playlistID != null && YoutubeHelper.enablePlaylistSpecialDupeCheck() && !isChannelPlaylist) {
                /* This allows one video to be added multiple times in context of different playlists and also without playlist context. */
                mainVideoDupeID += "/playlist/" + playlistID;
            }
            if (isCrawlDupeCheckEnabled && linkCollectorContainsEntryByID(mainVideoDupeID)) {
                logger.info("CrawlDupeCheck skip:" + crawledvid.videoID);
                continue;
            } else if (!videoIDsdupeCheck.add(mainVideoDupeID)) {
                logger.info("Duplicated Video skip:" + mainVideoDupeID);
                continue;
            }
            YoutubeClipData vid = crawledvid;
            try {
                /*
                 * Check for error message which will make all videos fail e.g. "Sign in to confirm you’re not a bot" to massively speed up
                 * crawl process for items that would fail either way.
                 */
                if (lastVideoFailedErrorMessage != null && lastVideoFailedErrorMessage.matches("(?i)Sign in to confirm.*")) {
                    throw new Exception(lastVideoFailedErrorMessage);
                }
                // make sure that we reload the video
                final boolean hasCache = ClipDataCache.hasCache(helper, crawledvid.videoID);
                try {
                    vid = ClipDataCache.load(helper, crawledvid);
                } catch (Exception e) {
                    logger.log(e);
                    if (hasCache) {
                        ClipDataCache.clearCache(vid.videoID);
                        vid = ClipDataCache.load(helper, vid);
                    } else {
                        throw e;
                    }
                }
            } catch (final Exception e) {
                logger.log(e);
                String errormessage = null;
                try {
                    errormessage = e.getMessage().toString();
                } catch (NullPointerException npe) {
                    // e.message can be null...
                }
                if (errormessage != null && StringUtils.isEmpty(vid.error)) {
                    vid.error = errormessage;
                }
                lastVideoFailedErrorMessage = vid.error;
                final DownloadLink offlineVideo = createOfflinelink(YoutubeHelper.generateContentURL(vid.videoID), "Error - " + vid.videoID + (vid.title != null ? " [" + vid.title + "]:" : "") + " " + vid.error, vid.error);
                if (channelOrPlaylistPackage != null) {
                    offlineVideo._setFilePackage(channelOrPlaylistPackage);
                }
                ret.add(offlineVideo);
                continue;
            }
            final List<AbstractVariant> enabledVariants = new ArrayList<AbstractVariant>(AbstractVariant.listVariants());
            final HashSet<VariantGroup> enabledVariantGroups = new HashSet<VariantGroup>();
            final VideoResolution maxVideoResolution = CFG_YOUTUBE.CFG.getMaxVideoResolution();
            {
                // nest this, so we don't have variables table full of entries that get called only once
                final List<VariantIDStorable> disabled = CFG_YOUTUBE.CFG.getDisabledVariants();
                final HashSet<String> disabledIds = new HashSet<String>();
                if (disabled != null) {
                    for (VariantIDStorable v : disabled) {
                        disabledIds.add(v.createUniqueID());
                    }
                }
                final HashSet<AudioBitrate> disabledAudioBitrates = createHashSet(CFG_YOUTUBE.CFG.getBlacklistedAudioBitrates());
                final HashSet<AudioCodec> disabledAudioCodecs = createHashSet(CFG_YOUTUBE.CFG.getBlacklistedAudioCodecs());
                final HashSet<FileContainer> disabledFileContainers = createHashSet(CFG_YOUTUBE.CFG.getBlacklistedFileContainers());
                final HashSet<VariantGroup> disabledGroups = createHashSet(CFG_YOUTUBE.CFG.getBlacklistedGroups());
                if (channelOrPlaylistPackage == null) {
                    disabledGroups.add(VariantGroup.IMAGE_PLAYLIST_COVER);
                }
                final HashSet<Projection> disabledProjections = createHashSet(CFG_YOUTUBE.CFG.getBlacklistedProjections());
                final HashSet<VideoResolution> disabledResolutions = createHashSet(CFG_YOUTUBE.CFG.getBlacklistedResolutions());
                final HashSet<VideoCodec> disabledVideoCodecs = createHashSet(CFG_YOUTUBE.CFG.getBlacklistedVideoCodecs());
                final HashSet<VideoFrameRate> disabledFramerates = createHashSet(CFG_YOUTUBE.CFG.getBlacklistedVideoFramerates());
                for (final Iterator<AbstractVariant> it = enabledVariants.iterator(); it.hasNext();) {
                    final AbstractVariant cur = it.next();
                    if (disabledGroups.contains(cur.getGroup())) {
                        it.remove();
                        continue;
                    } else if (disabledFileContainers.contains(cur.getContainer())) {
                        it.remove();
                        continue;
                    }
                    if (cur instanceof AudioInterface) {
                        if (disabledAudioBitrates.contains(((AudioInterface) cur).getAudioBitrate())) {
                            it.remove();
                            continue;
                        }
                        if (disabledAudioCodecs.contains(((AudioInterface) cur).getAudioCodec())) {
                            it.remove();
                            continue;
                        }
                    }
                    if (cur instanceof VideoVariant) {
                        if (disabledVideoCodecs.contains(((VideoVariant) cur).getVideoCodec())) {
                            it.remove();
                            continue;
                        }
                        if (disabledResolutions.contains(((VideoVariant) cur).getVideoResolution())) {
                            it.remove();
                            continue;
                        }
                        if (disabledFramerates.contains(((VideoVariant) cur).getiTagVideo().getVideoFrameRate())) {
                            it.remove();
                            continue;
                        }
                        if (disabledProjections.contains(((VideoVariant) cur).getProjection())) {
                            it.remove();
                            continue;
                        }
                    }
                    if (cur instanceof ImageVariant) {
                        final int height = ((ImageVariant) cur).getHeight();
                        if (disabledResolutions.contains(VideoResolution.getByHeight(height))) {
                            it.remove();
                            continue;
                        }
                    }
                    if (disabledIds.contains(new AbstractVariantWrapper(cur).getVariableIDStorable().createUniqueID())) {
                        it.remove();
                        continue;
                    }
                    enabledVariantGroups.add(cur.getGroup());
                }
            }
            // write all available variants to groups and allVariants
            List<VariantInfo> foundVariants = vid.findVariants();
            VideoVariant bestVideoResolution = null;
            {
                final Iterator<VariantInfo> it = foundVariants.iterator();
                while (it.hasNext()) {
                    final VariantInfo vi = it.next();
                    if (vi.getVariant() instanceof VideoVariant) {
                        final VideoVariant videoVariant = (VideoVariant) vi.getVariant();
                        if (bestVideoResolution == null || bestVideoResolution.getVideoHeight() < videoVariant.getVideoHeight()) {
                            bestVideoResolution = videoVariant;
                        }
                        if (videoVariant.getVideoHeight() > maxVideoResolution.getHeight()) {
                            it.remove();
                        }
                    }
                }
            }
            vid.bestVideoItag = bestVideoResolution;
            final List<VariantInfo> subtitles = enabledVariantGroups.contains(VariantGroup.SUBTITLES) ? vid.findSubtitleVariants() : new ArrayList<VariantInfo>();
            final ArrayList<VariantInfo> descriptions = enabledVariantGroups.contains(VariantGroup.DESCRIPTION) ? vid.findDescriptionVariant() : new ArrayList<VariantInfo>();
            if (subtitles != null) {
                foundVariants.addAll(subtitles);
            }
            if (descriptions != null) {
                foundVariants.addAll(descriptions);
            }
            List<YoutubeVariantCollection> links = YoutubeVariantCollection.load();
            if (requestedVariant != null) {
                // create a dummy collection
                links = new ArrayList<YoutubeVariantCollection>();
                final ArrayList<VariantIDStorable> varList = new ArrayList<VariantIDStorable>();
                varList.add(new VariantIDStorable(requestedVariant));
                links.add(new YoutubeVariantCollection("Dummy", varList));
            }
            final HashMap<String, AbstractVariant> allowedVariantsMap = new HashMap<String, AbstractVariant>();
            for (AbstractVariant v : enabledVariants) {
                final VariantIDStorable storable = new VariantIDStorable(v);
                allowedVariantsMap.put(storable.createUniqueID(), v);
            }
            final HashMap<VariantInfo, String[]> foundVariableMap = new HashMap<VariantInfo, String[]>();
            for (VariantInfo v : foundVariants) {
                final VariantIDStorable storable = new VariantIDStorable(v.getVariant());
                foundVariableMap.put(v, new String[] { storable.createUniqueID(), storable.createGroupingID(), storable.getContainer() });
            }
            if (CFG_YOUTUBE.CFG.isCollectionMergingEnabled()) {
                for (final YoutubeVariantCollection yvc : links) {
                    if (!yvc.isEnabled()) {
                        continue;
                    }
                    final ArrayList<VariantInfo> linkVariants = new ArrayList<VariantInfo>();
                    final ArrayList<VariantInfo> cutLinkVariantsDropdown = new ArrayList<VariantInfo>();
                    final HashSet<String> customAlternateSet = yvc.createUniqueIDSetForDropDownList();
                    if (customAlternateSet.size() > 0) {
                        for (Entry<VariantInfo, String[]> foundVariant : foundVariableMap.entrySet()) {
                            final String uId = foundVariant.getValue()[0];
                            if (customAlternateSet.contains(uId)) {
                                if (allowedVariantsMap.containsKey(uId)) {
                                    final VariantInfo variant = foundVariant.getKey();
                                    cutLinkVariantsDropdown.add(variant);
                                    helper.extendedDataLoading(variant, foundVariants);
                                }
                            }
                        }
                    }
                    if (StringUtils.isNotEmpty(yvc.getGroupingID())) {
                        final String groupingID = yvc.getGroupingID();
                        for (Entry<VariantInfo, String[]> foundVariant : foundVariableMap.entrySet()) {
                            final String gId = foundVariant.getValue()[1];
                            final String cId = foundVariant.getValue()[2];
                            if (StringUtils.equals(groupingID, gId) || StringUtils.equals(groupingID, cId)) {
                                final String uId = foundVariant.getValue()[0];
                                if (allowedVariantsMap.containsKey(uId)) {
                                    final VariantInfo variant = foundVariant.getKey();
                                    linkVariants.add(variant);
                                    helper.extendedDataLoading(variant, foundVariants);
                                }
                            }
                        }
                    } else if (yvc.getVariants() != null && yvc.getVariants().size() > 0) {
                        HashSet<String> idSet = yvc.createUniqueIDSet();
                        for (Entry<VariantInfo, String[]> foundVariant : foundVariableMap.entrySet()) {
                            final String uId = foundVariant.getValue()[0];
                            if (idSet.contains(uId)) {
                                if (allowedVariantsMap.containsKey(uId)) {
                                    final VariantInfo variant = foundVariant.getKey();
                                    linkVariants.add(variant);
                                    helper.extendedDataLoading(variant, foundVariants);
                                }
                            }
                        }
                    } else {
                        continue;
                    }
                    Collections.sort(cutLinkVariantsDropdown, new Comparator<VariantInfo>() {
                        @Override
                        public int compare(VariantInfo o1, VariantInfo o2) {
                            return o2.compareTo(o1);
                        }
                    });
                    Collections.sort(linkVariants, new Comparator<VariantInfo>() {
                        @Override
                        public int compare(VariantInfo o1, VariantInfo o2) {
                            return o2.compareTo(o1);
                        }
                    });
                    // remove dupes
                    VariantInfo last = null;
                    for (final Iterator<VariantInfo> it = linkVariants.iterator(); it.hasNext();) {
                        VariantInfo cur = it.next();
                        if (last != null) {
                            if (StringUtils.equals(cur.getVariant().getTypeId(), last.getVariant().getTypeId())) {
                                it.remove();
                                continue;
                            }
                        }
                        last = cur;
                    }
                    last = null;
                    for (final Iterator<VariantInfo> it = cutLinkVariantsDropdown.iterator(); it.hasNext();) {
                        VariantInfo cur = it.next();
                        if (last != null) {
                            if (StringUtils.equals(cur.getVariant().getTypeId(), last.getVariant().getTypeId())) {
                                it.remove();
                                continue;
                            }
                        }
                        last = cur;
                    }
                    if (linkVariants.size() == 0) {
                        continue;
                    }
                    DownloadLink lnk = createLink(yvc, linkVariants.get(0), cutLinkVariantsDropdown.size() > 0 ? cutLinkVariantsDropdown : linkVariants, channelOrPlaylistPackage, singleVideoPackageNamePatternOverride);
                    ret.add(lnk);
                    addExtraSubtitles: if (true) {
                        if (linkVariants.get(0).getVariant().getGroup() != VariantGroup.SUBTITLES) {
                            /* This is not a subtitle item */
                            break addExtraSubtitles;
                        }
                        final List<String> extras = CFG_YOUTUBE.CFG.getExtraSubtitles();
                        if (extras == null || extras.size() == 0) {
                            /* No extra subtitles wished by user */
                            break addExtraSubtitles;
                        }
                        /* Add users' desired extra subtitle variants */
                        for (final String s : extras) {
                            if (s == null) {
                                continue;
                            }
                            /* Walk through all our variants to find the ones we want. */
                            for (final VariantInfo vi : linkVariants) {
                                if (!(vi.getVariant() instanceof SubtitleVariant)) {
                                    /* Skip non-subtitle items. */
                                    continue;
                                }
                                if ("*".equals(s)) {
                                    /* User wants all available subtitles as separate entries in linkgrabber. */
                                    lnk = createLink(yvc, vi, cutLinkVariantsDropdown.size() > 0 ? cutLinkVariantsDropdown : linkVariants, channelOrPlaylistPackage, singleVideoPackageNamePatternOverride);
                                    ret.add(lnk);
                                } else if (StringUtils.equalsIgnoreCase(((SubtitleVariant) vi.getVariant()).getGenericInfo().getLanguage(), s)) {
                                    /* User wants specific subtitle(s) as separate entry/entries in linkgrabber. */
                                    lnk = createLink(yvc, vi, cutLinkVariantsDropdown.size() > 0 ? cutLinkVariantsDropdown : linkVariants, channelOrPlaylistPackage, singleVideoPackageNamePatternOverride);
                                    ret.add(lnk);
                                    break;
                                }
                            }
                        }
                    }
                }
            } else {
                ArrayList<VariantInfo> linkVariants = new ArrayList<VariantInfo>();
                for (Entry<VariantInfo, String[]> foundVariant : foundVariableMap.entrySet()) {
                    final String uId = foundVariant.getValue()[0];
                    if (allowedVariantsMap.containsKey(uId)) {
                        final VariantInfo variant = foundVariant.getKey();
                        linkVariants.add(variant);
                        helper.extendedDataLoading(variant, foundVariants);
                    }
                }
                Collections.sort(linkVariants, new Comparator<VariantInfo>() {
                    @Override
                    public int compare(VariantInfo o1, VariantInfo o2) {
                        return o2.compareTo(o1);
                    }
                });
                // remove dupes
                // System.out.println("Link " + l.getName());
                VariantInfo last = null;
                for (final Iterator<VariantInfo> it = linkVariants.iterator(); it.hasNext();) {
                    VariantInfo cur = it.next();
                    if (last != null) {
                        if (StringUtils.equals(cur.getVariant().getTypeId(), last.getVariant().getTypeId())) {
                            it.remove();
                            continue;
                        }
                    }
                    last = cur;
                }
                for (final VariantInfo vi : linkVariants) {
                    final ArrayList<VariantInfo> lst = new ArrayList<VariantInfo>();
                    lst.add(vi);
                    final DownloadLink lnk = createLink(new YoutubeVariantCollection(), vi, lst, channelOrPlaylistPackage, singleVideoPackageNamePatternOverride);
                    ret.add(lnk);
                }
            }
        }
        return ret;
    }

    private String getChannelID(final YoutubeHelper helper, final Browser br) {
        String channelID = helper != null ? helper.getChannelIdFromMaps() : null;
        if (channelID == null) {
            channelID = br.getRegex("<meta itemprop=\"channelId\" content=\"(UC[A-Za-z0-9\\-_]+)\"").getMatch(0);
            if (channelID == null) {
                channelID = br.getRegex("yt\\.setConfig\\(\\s*'CHANNEL_ID'\\s*,\\s*\"(UC[A-Za-z0-9\\-_]+)\"").getMatch(0);
                if (channelID == null) {
                    channelID = br.getRegex("rssURL\"\\s*:\\s*\"https?://[^\"]*channel_ID=(UC[A-Za-z0-9\\-_]+)\"").getMatch(0);
                }
            }
        }
        return channelID;
    }

    private <T> HashSet<T> createHashSet(List<T> list) {
        HashSet<T> ret = new HashSet<T>();
        if (list != null) {
            ret.addAll(list);
        }
        return ret;
    }

    private DownloadLink createLink(YoutubeVariantCollection yvc, VariantInfo variantInfo, List<VariantInfo> alternatives, final FilePackage groupPackage, final String packageNamePatternOverride) {
        YoutubeClipData clip = null;
        if (clip == null && variantInfo.getVideoStreams() != null) {
            clip = variantInfo.getVideoStreams().get(0).getClip();
        }
        if (clip == null && variantInfo.getAudioStreams() != null) {
            clip = variantInfo.getAudioStreams().get(0).getClip();
        }
        if (clip == null && variantInfo.getDataStreams() != null) {
            clip = variantInfo.getDataStreams().get(0).getClip();
        }
        boolean hasVariants = false;
        ArrayList<String> altIds = new ArrayList<String>();
        if (alternatives != null) {
            for (VariantInfo vi : alternatives) {
                if (!StringUtils.equals(variantInfo.getVariant()._getUniqueId(), vi.getVariant()._getUniqueId())) {
                    hasVariants = true;
                }
                altIds.add(vi.getVariant().getStorableString());
            }
        }
        final String linkID;
        if (VariantGroup.IMAGE_PLAYLIST_COVER.equals(variantInfo.getVariant().getBaseVariant().getGroup()) && getGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_ID) != null) {
            final String playListID = (String) getGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_ID);
            linkID = YoutubeHelper.createLinkID(playListID, variantInfo.getVariant());
        } else {
            linkID = YoutubeHelper.createLinkID(clip.videoID, variantInfo.getVariant());
        }
        final DownloadLink ret = createDownloadlink(linkID);
        final YoutubeHelper helper;
        if (this.helper != null) {
            helper = this.helper;
        } else {
            helper = new YoutubeHelper(br, getLogger());
        }
        if (clip != null) {
            ClipDataCache.referenceLink(helper, ret, clip);
            ret.setProperty(YoutubeHelper.YT_ID, clip.videoID);
        }
        if (yvc != null) {
            ret.setProperty(YoutubeHelper.YT_COLLECTION, yvc.getName());
        }
        setGlobalProperty(clip != null ? clip.videoID : null, ret);
        if (clip != null) {
            clip.copyToDownloadLink(ret);
        }
        // thislink.getTempProperties().setProperty(YoutubeHelper.YT_VARIANT_INFO, variantInfo);
        ret.setVariantSupport(hasVariants);
        ret.setProperty(YoutubeHelper.YT_VARIANTS, altIds);
        // Object cache = downloadLink.getTempProperties().getProperty(YoutubeHelper.YT_VARIANTS, null);
        // thislink.setProperty(YoutubeHelper.YT_VARIANT, variantInfo.getVariant()._getUniqueId());
        YoutubeHelper.writeVariantToDownloadLink(ret, variantInfo.getVariant());
        // variantInfo.fillExtraProperties(thislink, alternatives);
        final String filename = helper.createFilename(ret);
        ret.setFinalFileName(filename);
        ret.setLinkID(linkID);
        if (groupPackage != null) {
            groupPackage.add(ret);
        } else {
            final String fpName;
            if (packageNamePatternOverride != null) {
                fpName = helper.replaceVariables(ret, packageNamePatternOverride);
            } else {
                fpName = helper.replaceVariables(ret, helper.getConfig().getPackagePattern());
            }
            // req otherwise returned "" value = 'various', regardless of user settings for various!
            if (StringUtils.isNotEmpty(fpName)) {
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(fpName);
                /* let the packagizer merge several packages that have the same name because users are used to this default feature. */
                fp.setAllowMerge(true);
                // fp.setPackageKey("ytvideo://" + clip.videoID);
                fp.add(ret);
            }
        }
        if (clip != null) {
            long estimatedFileSize = 0;
            final AbstractVariant variant = variantInfo.getVariant();
            switch (variant.getType()) {
            case VIDEO:
            case DASH_AUDIO:
            case DASH_VIDEO:
                final StreamCollection audioStreams = clip.getStreams(variant.getBaseVariant().getiTagAudio(), variant);
                if (audioStreams != null && audioStreams.size() > 0) {
                    for (YoutubeStreamData stream : audioStreams) {
                        if (stream.getContentLength() > 0) {
                            estimatedFileSize += stream.getContentLength();
                            break;
                        } else if (stream.estimatedContentLength() > 0) {
                            estimatedFileSize += stream.estimatedContentLength();
                            break;
                        }
                    }
                }
                final StreamCollection videoStreams = clip.getStreams(variant.getBaseVariant().getiTagVideo(), variant);
                if (videoStreams != null && videoStreams.size() > 0) {
                    for (YoutubeStreamData stream : videoStreams) {
                        if (stream.getContentLength() > 0) {
                            estimatedFileSize += stream.getContentLength();
                            break;
                        } else if (stream.estimatedContentLength() > 0) {
                            estimatedFileSize += stream.estimatedContentLength();
                            break;
                        }
                    }
                }
                break;
            case IMAGE:
                final StreamCollection dataStreams = clip.getStreams(variant.getiTagData(), variant);
                if (dataStreams != null && dataStreams.size() > 0) {
                    for (YoutubeStreamData stream : dataStreams) {
                        if (stream.getContentLength() > 0) {
                            estimatedFileSize += stream.getContentLength();
                            break;
                        } else if (stream.estimatedContentLength() > 0) {
                            estimatedFileSize += stream.estimatedContentLength();
                            break;
                        }
                    }
                }
                break;
            default:
                break;
            }
            if (estimatedFileSize > 0) {
                ret.setDownloadSize(estimatedFileSize);
            }
        }
        ret.setAvailableStatus(AvailableStatus.TRUE);
        return ret;
    }

    private ArrayList<YoutubeClipData> crawlPlaylistOrChannel(final YoutubeHelper helper, final Browser br, final String playlistID, final String userName, final String channelID, final String referenceUrl, final int maxItemsLimit) throws Exception {
        if (StringUtils.isEmpty(playlistID) && StringUtils.isEmpty(userName) && StringUtils.isEmpty(channelID)) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        } else if (maxItemsLimit == 0) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        helper.setPlaylistID(playlistID);
        br.getHeaders().put("Accept-Charset", null);
        String userOrPlaylistURL;
        String desiredChannelTab = null;
        final String channelTabFromURL = getChannelTabNameFromURL(referenceUrl);
        String humanReadableTitle;
        if (playlistID != null) {
            if (YoutubeHelper.looksLikeRadioPlaylistURL(referenceUrl)) {
                userOrPlaylistURL = YoutubeHelper.generateRadioPlaylistURL(playlistID);
            } else {
                /* Normal/user generated playlist */
                userOrPlaylistURL = YoutubeHelper.generatePlaylistURL(playlistID);
            }
            humanReadableTitle = "Playlist " + playlistID;
            if (YoutubeHelper.looksLikeRadioPlaylistURL(br.getURL())) {
                /* It's a mix playlist auto created by youtube -> Set "YouTube" as name of creator of this playlist. */
                putGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_CREATOR, "YouTube");
            }
        } else if (channelID != null) {
            /* Channel via channelID (legacy - urls containing only channelID are not common anymore) */
            if (channelTabFromURL == null) {
                /* do not modify channelID links */
                userOrPlaylistURL = referenceUrl;
            } else {
                userOrPlaylistURL = YoutubeHelper.getChannelURLOLD(channelID, channelTabFromURL);
                desiredChannelTab = channelTabFromURL;
            }
            humanReadableTitle = "ChannelID " + channelID;
        } else {
            /* Channel/User */
            if (channelTabFromURL == null) {
                userOrPlaylistURL = YoutubeHelper.getChannelURL(userName, "videos");
                desiredChannelTab = "Videos";
            } else {
                userOrPlaylistURL = YoutubeHelper.getChannelURL(userName, channelTabFromURL);
                desiredChannelTab = channelTabFromURL;
            }
            humanReadableTitle = "Channel " + userName;
        }
        List<Map<String, Object>> alerts = null;
        String errorOrWarningMessage = null;
        URL originalURL = null;
        Map<String, Object> ytConfigData = null;
        final Set<String> playListDupes = new HashSet<String>();
        Integer totalNumberofItems = null;
        String userWishedSortTitle = null;
        /* Check if user wishes different sort than default */
        final ChannelCrawlerSortMode sortMode = cfg.getChannelCrawlerPreferredSortMode();
        if (sortMode == ChannelCrawlerSortMode.LATEST) {
            /* 2023-07-21: Serverside default */
            userWishedSortTitle = "Latest";
        } else if (sortMode == ChannelCrawlerSortMode.POPULAR) {
            userWishedSortTitle = "Popular";
        } else if (sortMode == ChannelCrawlerSortMode.OLDEST) {
            userWishedSortTitle = "Oldest";
        }
        String activeSort = "Untouched/Default";
        String sortToken = null;
        short run = -1;
        Map<String, Object> rootMap;
        List<Map<String, Object>> varray = null;
        boolean abortPaginationAfterFirstPage = false;
        /* Use integer array here so that other functions can alter this value. */
        int[] videoPositionCounter = new int[1];
        videoPositionCounter[0] = 0;
        do {
            run++;
            helper.getPage(br, userOrPlaylistURL);
            if (playlistID != null) {
                helper.crawlCoverData(playlistID, true);
            }
            checkBasicErrors(br);
            if (originalURL == null) {
                originalURL = br._getURL();
            }
            final String playListTitleHTML = extractWebsiteTitle(br);
            if (playListTitleHTML != null) {
                putGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_TITLE, Encoding.htmlDecode(playListTitleHTML).trim());
            }
            helper.parse(br);
            rootMap = helper.getYtInitialData();
            if (rootMap == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            ytConfigData = (Map<String, Object>) JavaScriptEngineFactory.walkJson(rootMap, "responseContext/webResponseContextExtensionData/ytConfigData");
            final Map<String, Object> autoGeneratexYoutubeMixPlaylistProbe = (Map<String, Object>) JavaScriptEngineFactory.walkJson(rootMap, "contents/twoColumnWatchNextResults/playlist/playlist");
            if (autoGeneratexYoutubeMixPlaylistProbe != null && Boolean.TRUE.equals(autoGeneratexYoutubeMixPlaylistProbe.get("isInfinite"))) {
                /* Auto generated "youtube mix" playlist */
                putGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_TITLE, autoGeneratexYoutubeMixPlaylistProbe.get("title").toString());
                varray = (List<Map<String, Object>>) autoGeneratexYoutubeMixPlaylistProbe.get("contents");
                /* Such playlists can contain an infinite amount of items -> Stop after first page */
                abortPaginationAfterFirstPage = true;
            } else if (run == 0) {
                /* Channel or playlist */
                final List<String> availableChannelTabs = new ArrayList<String>();
                Map<String, Object> featuredtab = null;
                Map<String, Object> videostab = null;
                Map<String, Object> shortstab = null;
                Map<String, Object> releasestab = null;
                Map<String, Object> playlisttab = null;
                Map<String, Object> selectedtab = null;
                final List<Map<String, Object>> tabs = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(rootMap, "contents/twoColumnBrowseResultsRenderer/tabs");
                handleTabs: if (true) {
                    if (tabs == null) {
                        logger.warning("Failed to find any tabs");
                        break handleTabs;
                    }
                    for (final Map<String, Object> tab : tabs) {
                        /* We will get this one if a real playlist is our currently opened tab. */
                        final Map<String, Object> tabRenderer = (Map<String, Object>) tab.get("tabRenderer");
                        final Object varrayPlaylistProbe = JavaScriptEngineFactory.walkJson(tabRenderer, "content/sectionListRenderer/contents/{}/itemSectionRenderer/contents/{}/playlistVideoListRenderer/contents");
                        final Object varrayChannelShortsPlaylist = JavaScriptEngineFactory.walkJson(tabRenderer, "content/sectionListRenderer/contents/{}/itemSectionRenderer/contents/{}/richGridRenderer/contents");
                        if (varrayPlaylistProbe != null) {
                            /* Real playlist */
                            playlisttab = tab;
                            varray = (List<Map<String, Object>>) varrayPlaylistProbe;
                            break;
                        } else if (varrayChannelShortsPlaylist != null) {
                            /* Channel playlist when channel only contains shorts */
                            varray = (List<Map<String, Object>>) varrayChannelShortsPlaylist;
                        } else if (tabRenderer != null) {
                            /* Channel/User */
                            final String tabTitle = (String) tabRenderer.get("title");// is localized!
                            final String browseEndpoint_Params = (String) JavaScriptEngineFactory.walkJson(tabRenderer, "endpoint/browseEndpoint/params");
                            final String webCommandMetadata_url = (String) JavaScriptEngineFactory.walkJson(tabRenderer, "endpoint/commandMetadata/webCommandMetadata/url");
                            final Boolean selected = (Boolean) tabRenderer.get("selected");
                            List<Map<String, Object>> varrayTmp = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(tabRenderer, "content/richGridRenderer/contents");
                            if (varrayTmp == null) {
                                varrayTmp = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(tabRenderer, "content/sectionListRenderer/contents/{}/itemSectionRenderer/contents/{}/shelfRenderer/content/horizontalListRenderer/items");
                            }
                            if (tabTitle != null) {
                                availableChannelTabs.add(tabTitle);
                            }
                            if (StringUtils.endsWithCaseInsensitive(webCommandMetadata_url, "/shorts") || StringUtils.equals(browseEndpoint_Params, "EgZzaG9ydHPyBgUKA5oBAA%3D%3D")) {
                                shortstab = tab;
                                if (Boolean.TRUE.equals(selected) && varrayTmp != null) {
                                    varray = varrayTmp;
                                }
                            } else if (StringUtils.endsWithCaseInsensitive(webCommandMetadata_url, "/videos") || StringUtils.equals(browseEndpoint_Params, "EgZ2aWRlb3PyBgQKAjoA")) {
                                videostab = tab;
                                if (Boolean.TRUE.equals(selected) && varrayTmp != null) {
                                    varray = varrayTmp;
                                }
                            } else if (StringUtils.endsWithCaseInsensitive(webCommandMetadata_url, "/releases") || StringUtils.equals(browseEndpoint_Params, "EghyZWxlYXNlc_IGBQoDsgEA")) {
                                releasestab = tab;
                                if (Boolean.TRUE.equals(selected) && varrayTmp != null) {
                                    varray = varrayTmp;
                                }
                            } else if (StringUtils.endsWithCaseInsensitive(webCommandMetadata_url, "/featured") || StringUtils.equals(browseEndpoint_Params, "EghmZWF0dXJlZPIGBAoCMgA%3D")) {
                                featuredtab = tab;
                                if (Boolean.TRUE.equals(selected) && varrayTmp != null) {
                                    varray = varrayTmp;
                                }
                            } else if (Boolean.TRUE.equals(selected) && varrayTmp != null) {
                                selectedtab = tab;
                                varray = varrayTmp;
                            }
                        }
                    }
                    logger.info("Available channel tabs: " + availableChannelTabs);
                    if (shortstab == null && "shorts".equals(channelTabFromURL) && run == 0 && videostab != null) {
                        logger.info("User wanted shorts but channel doesn't contain shorts tab -> Fallback to Videos tab and re-do loop with that URL");
                        if (channelID != null) {
                            userOrPlaylistURL = YoutubeHelper.getChannelURLOLD(userName, "videos");
                            desiredChannelTab = "Videos";
                        } else {
                            /* Channel/User */
                            userOrPlaylistURL = YoutubeHelper.getChannelURL(userName, "videos");
                            desiredChannelTab = "Videos";
                        }
                        continue;
                    } else if (videostab == null && ("videos".equals(channelTabFromURL) || channelTabFromURL == null) && run == 0 && shortstab != null) {
                        logger.info("User wanted videos but channel doesn't contain videos tab -> Fallback to shorts tab and re-do loop with that URL");
                        if (channelID != null) {
                            userOrPlaylistURL = YoutubeHelper.getChannelURLOLD(userName, "shorts");
                            desiredChannelTab = "Shorts";
                        } else {
                            /* Channel/User */
                            userOrPlaylistURL = YoutubeHelper.getChannelURL(userName, "shorts");
                            desiredChannelTab = "Shorts";
                        }
                        continue;
                    }
                }
            }
            /**
             * This message can also contain information like "2 unavailable videos won't be displayed in this list". </br>
             * Only mind this errormessage if we can't find any content.
             */
            alerts = (List<Map<String, Object>>) rootMap.get("alerts");
            errorOrWarningMessage = null;
            /* Find last errormessage. Mostly there is one one available anyways. */
            if (alerts != null && alerts.size() > 0) {
                for (final Map<String, Object> alert : alerts) {
                    Map<String, Object> alertRenderer = (Map<String, Object>) alert.get("alertRenderer");
                    if (alertRenderer == null) {
                        alertRenderer = (Map<String, Object>) alert.get("alertWithButtonRenderer");
                    }
                    if (alertRenderer == null) {
                        logger.warning("Failed to find alertRenderer in alert: " + alert);
                        continue;
                    }
                    errorOrWarningMessage = (String) JavaScriptEngineFactory.walkJson(alertRenderer, "text/runs/{0}/text"); // Playlist(?)
                    if (errorOrWarningMessage == null) {
                        errorOrWarningMessage = (String) JavaScriptEngineFactory.walkJson(alertRenderer, "text/simpleText"); // Channel
                    }
                }
            }
            if (varray == null && errorOrWarningMessage != null) {
                throw new DecrypterRetryException(RetryReason.FILE_NOT_FOUND, "CHANNEL_OR_PLAYLIST_OFFLINE_" + humanReadableTitle, errorOrWarningMessage);
            } else if (errorOrWarningMessage != null) {
                /* For example "Unavailable videos are hidden" */
                logger.info("Found warning message for playlist/channel: " + errorOrWarningMessage);
                this.displayBubbleNotification(humanReadableTitle + " | Warning", errorOrWarningMessage);
            }
            /* Find extra information about playlist */
            final String playlistOwnerName = this.findPlaylistOwnerName(rootMap);
            if (playlistOwnerName != null) {
                putGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_CREATOR, playlistOwnerName);
            }
            final String playlistTitle = this.findPlaylistTitle(rootMap);
            if (playlistTitle != null) {
                putGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_TITLE, playlistTitle);
            }
            final String playlistDescription = this.findPlaylistDescription(rootMap);
            if (playlistDescription != null) {
                putGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_DESCRIPTION, playlistDescription);
            }
            /**
             * Find extra information about channel </br>
             * Do not do this if tab is e.g. "shorts" as we'd then pickup an incorrect number. YT ui does not display the total number of
             * shorts of a user.
             */
            String videosCountText = findNumberOfVideosText(rootMap);
            final Map<String, Object> channelHeaderRenderer = (Map<String, Object>) JavaScriptEngineFactory.walkJson(rootMap, "header/c4TabbedHeaderRenderer");
            if (videosCountText == null && channelHeaderRenderer != null && StringUtils.equalsIgnoreCase(desiredChannelTab, "Videos")) {
                videosCountText = (String) JavaScriptEngineFactory.walkJson(channelHeaderRenderer, "videosCountText/runs/{0}/text");
            }
            if (videosCountText != null) {
                videosCountText = videosCountText.replaceAll("(\\.|,)", "");
                if (videosCountText.equalsIgnoreCase("No videos") || videosCountText.equals("0")) {
                    /* Profile with no videos at all (or empty playlist but not sure if that can even exist) */
                    throw new DecrypterRetryException(RetryReason.EMPTY_PROFILE);
                }
                if (videosCountText.matches("\\d+")) {
                    totalNumberofItems = Integer.valueOf(videosCountText);
                } else {
                    logger.warning("Found non-number videosCountText: " + videosCountText);
                }
            }
            if (userWishedSortTitle != null) {
                /* User wishes custom sort which needs to be done serverside. */
                sortToken = (String) this.findSortToken(rootMap, userWishedSortTitle);
                if (sortToken == null) {
                    logger.info("Unable to sort by '" + userWishedSortTitle + "': Either this item is not sortable or it is already sorted in the wished order");
                } else {
                    logger.info("Token for sort by '" + userWishedSortTitle + "' is: " + sortToken);
                    activeSort = userWishedSortTitle;
                }
            }
            break;
        } while (run < 1);
        if (!StringUtils.equals(originalURL.toString(), br.getURL())) {
            logger.info("Channel/playlist URL used differs from URL that was initially added: Original: " + originalURL.toString() + " | Actually used: " + br.getURL());
            helper.setChannelPlaylistCrawlerContainerUrlOverride(br.getURL());
        }
        final ArrayList<YoutubeClipData> ret = new ArrayList<YoutubeClipData>();
        humanReadableTitle += " sorted by " + activeSort;
        int round = 0;
        final String INNERTUBE_CLIENT_NAME = helper.getYtCfgSet() != null ? String.valueOf(helper.getYtCfgSet().get("INNERTUBE_CONTEXT_CLIENT_NAME")) : null;
        final String INNERTUBE_API_KEY = helper.getYtCfgSet() != null ? String.valueOf(helper.getYtCfgSet().get("INNERTUBE_API_KEY")) : null;
        final String INNERTUBE_CLIENT_VERSION = helper.getYtCfgSet() != null ? String.valueOf(helper.getYtCfgSet().get("INNERTUBE_CLIENT_VERSION")) : null;
        /* Now stuff that is required when user is logged in. */
        final String DELEGATED_SESSION_ID = helper.getYtCfgSet() != null ? String.valueOf(helper.getYtCfgSet().get("DELEGATED_SESSION_ID")) : null;
        boolean reachedUserDefinedMaxItemsLimit = false;
        int numberofSkippedDuplicates = 0;
        int internalGuessedPaginationSize = -1;
        boolean paginationFailedForUnknownReasons = false;
        pagination: do {
            /* First try old HTML handling though by now [June 2023] all data is provided via json. */
            String nextPageToken = null;
            final int crawledItemsSizeOld = playListDupes.size();
            if (sortToken != null && round == 0) {
                logger.info("Round 0 goes into sorting list via sort token: " + sortToken);
                nextPageToken = sortToken;
            } else {
                if (varray == null) {
                    /* This should never happen. */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                int numberOfVideoItemsOnThisPage = 0;
                final ArrayList<YoutubeClipData> newItems = processVideoArray(varray, playListDupes, maxItemsLimit, videoPositionCounter);
                ret.addAll(newItems);
                // Check if we reached our limit
                reachedUserDefinedMaxItemsLimit = (maxItemsLimit > 0 && playListDupes.size() >= maxItemsLimit);
                if (reachedUserDefinedMaxItemsLimit) {
                    logger.info("Stopping because: Reached max items limit of " + maxItemsLimit);
                    break pagination;
                }
                final Set<String> continuationTokens = findAllContinuationTokens(varray);
                if (continuationTokens.size() == 1) {
                    nextPageToken = continuationTokens.iterator().next();
                } else if (continuationTokens.size() > 1) {
                    logger.warning("Found multiple possible continuationTokens: " + continuationTokens);
                    // Take the first one or implement more sophisticated selection
                    nextPageToken = continuationTokens.iterator().next();
                }
                if (internalGuessedPaginationSize == -1) {
                    internalGuessedPaginationSize = numberOfVideoItemsOnThisPage;
                }
                /* Check for some abort conditions */
                final int numberofNewItemsThisRun = playListDupes.size() - crawledItemsSizeOld;
                logger.info("Crawled page " + round + " | Found items on this page: " + numberofNewItemsThisRun + " | Found items so far: " + playListDupes.size() + "/" + totalNumberofItems + " | nextPageToken = " + nextPageToken + " | Max items limit: " + maxItemsLimit + " | activeSort: " + activeSort + " | Internal guessed pagination size: " + internalGuessedPaginationSize + " | videoPositionCounter: " + videoPositionCounter[0]);
                if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    throw new InterruptedException();
                } else if (reachedUserDefinedMaxItemsLimit) {
                    logger.info("Stopping because: Reached max items limit of " + maxItemsLimit);
                    break pagination;
                } else if (numberofNewItemsThisRun == 0) {
                    logger.info("Stopping because: No new videoIDs found on current page");
                    break pagination;
                } else if (nextPageToken == null) {
                    logger.info("Stopping because: No next page found");
                    break pagination;
                } else if (abortPaginationAfterFirstPage) {
                    logger.info("Stopping because: abortPaginationAfterFirstPage == true");
                    break pagination;
                } else {
                    /* Continue to next page */
                }
            }
            /* Try to continue to next page */
            if (StringUtils.isEmpty(INNERTUBE_CLIENT_NAME) || StringUtils.isEmpty(INNERTUBE_API_KEY) || StringUtils.isEmpty(INNERTUBE_CLIENT_VERSION)) {
                /* This should never happen. */
                logger.info("Stopping because: Pagination is broken due to missing 'INNERTUBE' variable");
                break pagination;
            }
            final Map<String, Object> context = new HashMap<String, Object>();
            final Map<String, Object> client = new HashMap<String, Object>();
            /* Field "visitorData" is required for e.g. /@profile/shorts" but not for "real" playlists.yin */
            String visitorData = ytConfigData != null ? (String) ytConfigData.get("visitorData") : null;
            if (visitorData != null) {
                client.put("visitorData", visitorData);
            }
            client.put("clientName", INNERTUBE_CLIENT_NAME);
            client.put("clientVersion", INNERTUBE_CLIENT_VERSION);
            client.put("originalUrl", originalURL.toString());
            context.put("client", client);
            final Map<String, Object> paginationPostData = new HashMap<String, Object>();
            paginationPostData.put("context", context);
            paginationPostData.put("continuation", nextPageToken);
            round = antiDdosSleep(round);
            /* Set headers on every run as some tokens (Authorization header!) contain timestamps so they can expire. */
            prepBrowserWebAPI(br, helper.getAccountLoggedIn());
            if (DELEGATED_SESSION_ID != null) {
                br.getHeaders().put("X-Goog-Pageid", DELEGATED_SESSION_ID);
            }
            br.postPageRaw("/youtubei/v1/browse?key=" + INNERTUBE_API_KEY + "&prettyPrint=false", JSonStorage.serializeToJson(paginationPostData));
            rootMap = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            try {
                final List<Map<String, Object>> onResponseReceivedActions = (List<Map<String, Object>>) rootMap.get("onResponseReceivedActions");
                final Map<String, Object> lastReceivedAction = onResponseReceivedActions.get(onResponseReceivedActions.size() - 1);
                varray = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(lastReceivedAction, "appendContinuationItemsAction/continuationItems");
                if (varray == null) {
                    /* E.g. at the beginning after sorting */
                    varray = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(lastReceivedAction, "reloadContinuationItemsCommand/continuationItems");
                }
            } catch (final Exception e) {
                if (ret.isEmpty()) {
                    /* Zero results found before -> Throw exception */
                    throw e;
                } else {
                    logger.log(e);
                    if (alerts != null && alerts.size() > 0) {
                        /**
                         * 2023-08-03: E.g. playlist with 700 videos but 680 of them are hidden/unavailable which means first pagination
                         * attempt will fail. </br>
                         * Even via website this seems to be and edge case as the loading icon will never disappear and no error is
                         * displayed.
                         */
                        logger.info("Pagination failed -> Possible reason: " + errorOrWarningMessage);
                    } else {
                        logger.warning("Pagination failed for unknown reasons");
                        paginationFailedForUnknownReasons = true;
                    }
                    break pagination;
                }
            }
        } while (true);
        int missingVideos = 0;
        if (totalNumberofItems != null) {
            putGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_SIZE, totalNumberofItems);
            /*
             * If user set limit we maybe didn't crawl all items but then that was on purpose -> Do not calculate number of "missing" items
             * as they are not missing but they got skipped.
             */
            if (!reachedUserDefinedMaxItemsLimit) {
                missingVideos = totalNumberofItems.intValue() - ret.size();
            }
        } else if (!reachedUserDefinedMaxItemsLimit && !paginationFailedForUnknownReasons) {
            /* We could not determine the number of items in this channel/playlist -> Use total number of found items as this number. */
            logger.info("Unable to determine PLAYLIST_SIZE -> Using number of found items as replacement");
            putGlobalProperty(null, YoutubeHelper.YT_PLAYLIST_SIZE, ret.size());
        }
        /* Number of skipped and missing items are two different numbers! */
        missingVideos -= numberofSkippedDuplicates;
        String bubblenotificationText = "Finished finding all items of " + humanReadableTitle;
        bubblenotificationText += "\r\nFound " + ret.size() + " of " + (totalNumberofItems != null ? totalNumberofItems : "??") + " videos in " + humanReadableTitle;
        if (missingVideos > 0) {
            bubblenotificationText += "\r\nMissing videos: " + missingVideos + " due to  skipped duplicates, offline or hidden videos by YouTube";
            bubblenotificationText += "\r\nReasons for missing videos: Videos that are hidden/offline/private/GEO-blocked";
        }
        if (numberofSkippedDuplicates > 0) {
            bubblenotificationText += "\r\nSkipped duplicates: " + numberofSkippedDuplicates;
            bubblenotificationText += "\r\nPlaylists can contain the same item multiple times but JD can only add it once to linkgrabber.";
        }
        bubblenotificationText += "\r\nProcessing results. This will take a while.";
        this.displayBubbleNotification(humanReadableTitle + " items", bubblenotificationText);
        logger.info("parsePlaylist method returns: " + ret.size() + " VideoIDs | Number of possibly missing videos [due to duplicate/private/offline/GEO-block or bug in plugin]: " + missingVideos + " | Skipped duplicates: " + numberofSkippedDuplicates);
        return ret;
    }

    /**
     * Recursively extracts YouTube video data from a JSON structure. Collects all video items, without limiting the number of items
     * collected.
     *
     * @param jsonObject
     *            The JSON object or array to search through
     * @param result
     *            Collection to store the found data Structure is: [videoId, playlistId, videoPositionCounter]
     * @param videoPositionCounter
     *            Counter for playlist positions, passed as an array to allow modification
     */
    private void collectYoutubeClipData(Object jsonObject, List<Object[]> result, int[] videoPositionCounter) {
        if (jsonObject == null) {
            return;
        }
        if (jsonObject instanceof Map) {
            Map<String, Object> map = (Map<String, Object>) jsonObject;
            // Direct video ID extraction from various paths
            String videoId = extractVideoId(map);
            String playlistId = extractPlaylistId(map);
            if (playlistId != null) {
                // TODO: Remove this. It looks to be not needed anymore.s
                // Handle playlist IDs
                List<Map<String, Object>> playlistVideos = extractPlaylistVideos(map);
                if (playlistVideos != null) {
                    for (Map<String, Object> video : playlistVideos) {
                        videoId = extractVideoIdFromChildRenderer(video);
                        if (videoId != null) {
                            videoPositionCounter[0]++;
                            result.add(new Object[] { videoId, playlistId, videoPositionCounter[0] });
                        }
                    }
                } else {
                    // Store the playlist ID itself to handle it in the calling code
                    result.add(new Object[] { null, playlistId, -1 });
                }
            } else if (videoId != null) {
                videoPositionCounter[0]++;
                // Store videoId and position - process duplicates later
                result.add(new Object[] { videoId, null, videoPositionCounter[0] });
            }
            // Recursively search all values
            for (Object value : map.values()) {
                if (value instanceof Map || value instanceof List) {
                    collectYoutubeClipData(value, result, videoPositionCounter);
                }
            }
        } else if (jsonObject instanceof List) {
            List<Object> list = (List<Object>) jsonObject;
            for (Object item : list) {
                if (item instanceof Map || item instanceof List) {
                    collectYoutubeClipData(item, result, videoPositionCounter);
                }
            }
        } else {
            logger.warning("WTF jsonObject has unsupported type");
        }
    }

    /**
     * Extracts a video ID from various possible paths in a JSON object
     */
    private String extractVideoId(Map<String, Object> map) {
        // Try all the different paths where video IDs might be found
        String videoId = (String) JavaScriptEngineFactory.walkJson(map, "playlistVideoRenderer/videoId");
        if (videoId == null) {
            videoId = (String) JavaScriptEngineFactory.walkJson(map, "richItemRenderer/content/videoRenderer/videoId");
        }
        if (videoId == null) {
            videoId = (String) JavaScriptEngineFactory.walkJson(map, "richItemRenderer/content/reelItemRenderer/videoId");
        }
        if (videoId == null) {
            videoId = (String) JavaScriptEngineFactory.walkJson(map, "playlistPanelVideoRenderer/videoId");
        }
        if (videoId == null) {
            videoId = (String) JavaScriptEngineFactory.walkJson(map, "richItemRenderer/content/shortsLockupViewModel/onTap/innertubeCommand/reelWatchEndpoint/videoId");
        }
        if (videoId == null) {
            videoId = (String) JavaScriptEngineFactory.walkJson(map, "videoRenderer/videoId");
        }
        if (videoId == null) {
            videoId = (String) JavaScriptEngineFactory.walkJson(map, "reelItemRenderer/videoId");
        }
        return videoId;
    }

    /**
     * Extracts a playlist ID from the JSON object
     */
    private String extractPlaylistId(Map<String, Object> map) {
        String playlistId = (String) JavaScriptEngineFactory.walkJson(map, "richItemRenderer/content/playlistRenderer/playlistId");
        if (playlistId == null) {
            playlistId = (String) JavaScriptEngineFactory.walkJson(map, "gridPlaylistRenderer/playlistId");
        }
        return playlistId;
    }

    /**
     * Extracts a video ID from a child video renderer
     */
    private String extractVideoIdFromChildRenderer(Map<String, Object> video) {
        Map<String, Object> childRenderer = (Map<String, Object>) video.get("childVideoRenderer");
        if (childRenderer != null) {
            return (String) childRenderer.get("videoId");
        }
        return null;
    }

    /**
     * Extracts the playlist videos from a map
     */
    private List<Map<String, Object>> extractPlaylistVideos(Map<String, Object> map) {
        List<Map<String, Object>> videos = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(map, "richItemRenderer/content/playlistRenderer/videos");
        if (videos == null) {
            videos = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(map, "gridPlaylistRenderer/items");
        }
        return videos;
    }

    /**
     * Finds continuation tokens in the data structure
     *
     * @param jsonObject
     *            The JSON object to search in
     * @return A set of continuation tokens found
     */
    private Set<String> findAllContinuationTokens(Object jsonObject) {
        Set<String> tokens = new HashSet<String>();
        collectContinuationTokens(jsonObject, tokens);
        return tokens;
    }

    /**
     * Recursively collects continuation tokens from the JSON structure
     */
    private void collectContinuationTokens(final Object jsonObject, final Set<String> tokens) {
        if (jsonObject == null) {
            return;
        }
        if (jsonObject instanceof Map) {
            final Map<String, Object> map = (Map<String, Object>) jsonObject;
            final Map<String, Object> continuationEndpoint = (Map<String, Object>) JavaScriptEngineFactory.walkJson(map, "continuationItemRenderer/continuationEndpoint");
            if (continuationEndpoint != null) {
                String token = (String) JavaScriptEngineFactory.walkJson(continuationEndpoint, "continuationCommand/token");
                if (token != null) {
                    tokens.add(token);
                } else {
                    /* 2025-05-19: E.g. playlist when logged in, json looks different */
                    final List<Map<String, Object>> commands = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(continuationEndpoint, "commandExecutorCommand/commands");
                    if (commands != null && commands.size() > 0) {
                        for (final Map<String, Object> command : commands) {
                            final Map<String, Object> continuationCommand = (Map<String, Object>) command.get("continuationCommand");
                            if (continuationCommand != null) {
                                tokens.add(continuationCommand.get("token").toString());
                                break;
                            }
                        }
                    }
                }
            }
            // Recursively check all values
            for (Object value : map.values()) {
                if (value instanceof Map || value instanceof List) {
                    collectContinuationTokens(value, tokens);
                }
            }
        } else if (jsonObject instanceof List) {
            final List<Object> list = (List<Object>) jsonObject;
            for (Object item : list) {
                if (item instanceof Map || item instanceof List) {
                    collectContinuationTokens(item, tokens);
                }
            }
        }
    }

    private ArrayList<YoutubeClipData> processVideoArray(List<Map<String, Object>> varray, Set<String> playListDupes, final int maxItemsLimit, int[] videoPositionCounter) {
        final ArrayList<YoutubeClipData> result = new ArrayList<YoutubeClipData>();
        // Collect all video data without filtering
        final List<Object[]> collectedData = new ArrayList<Object[]>();
        collectYoutubeClipData(varray, collectedData, videoPositionCounter);
        // Process the collected data with filtering logic
        int numberofSkippedDuplicates = 0;
        for (Object[] item : collectedData) {
            String videoId = (String) item[0];
            String playlistId = (String) item[1];
            int position = (Integer) item[2];
            if (this.isAbort()) {
                logger.info("Thread Aborted!");
                break;
            }
            if (videoId != null) {
                // Process video IDs
                if (!playListDupes.add(videoId)) {
                    logger.info("Skipping dupe: " + videoId + " | Position: " + position);
                    numberofSkippedDuplicates++;
                    continue;
                }
                YoutubeClipData clipData = new YoutubeClipData(videoId);
                result.add(clipData);
                putGlobalProperty(videoId, YoutubeHelper.YT_PLAYLIST_POSITION, position);
                if (maxItemsLimit > 0 && playListDupes.size() >= maxItemsLimit) {
                    logger.info("Reached user-defined max items limit: " + maxItemsLimit);
                    break;
                }
            } else if (playlistId != null) {
                // Handle playlist ID - for example, crawling it separately or adding it as a special entry
                logger.info("Found playlist ID: " + playlistId);
                // Implementation depends on your requirements
            }
        }
        logger.info("Processed items: " + collectedData.size() + " | Added items: " + result.size() + "  | Skipped duplicates: " + numberofSkippedDuplicates);
        return result;
    }

    private void checkBasicErrors(final Browser br) throws PluginException {
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    /**
     * Recursive function designed to find token which is needed to alter the serverside order of any items inside channel overview tabs (of
     * "/@username/overview").
     */
    private Object findSortToken(final Object o, final String sortText) {
        if (o instanceof Map) {
            final Map<String, Object> map = (Map<String, Object>) o;
            final String thisSortText = (String) JavaScriptEngineFactory.walkJson(map, "chipCloudChipRenderer/text/simpleText");
            final String thisContinuationCommand = (String) JavaScriptEngineFactory.walkJson(map, "chipCloudChipRenderer/navigationEndpoint/continuationCommand/token");
            /*
             * If isSelected is true, that is our current sort -> In that case we do not want to return anything if that is the sort we want
             * as we already have it and we do not want the upper handling to just reload the list in the order we already have.
             */
            final Boolean isSelected = (Boolean) JavaScriptEngineFactory.walkJson(map, "chipCloudChipRenderer/isSelected");
            if (sortText.equalsIgnoreCase(thisSortText) && thisContinuationCommand != null && !Boolean.TRUE.equals(isSelected)) {
                return thisContinuationCommand;
            }
            for (final Map.Entry<String, Object> entry : map.entrySet()) {
                // final String key = entry.getKey();
                final Object value = entry.getValue();
                if (value instanceof List || value instanceof Map) {
                    final Object pico = findSortToken(value, sortText);
                    if (pico != null) {
                        return pico;
                    }
                }
            }
            return null;
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final Object ret = findSortToken(arrayo, sortText);
                    if (ret != null) {
                        return ret;
                    }
                }
            }
            return null;
        } else {
            return o;
        }
    }

    /** Recursive function that finds all continuationTokens in given json item. */
    private void findContinuationTokens(final Set<String> results, final Object o) {
        final Set<Object> found = search(o, null, "continuationEndpoint/continuationCommand/token");
        if (found != null && found.size() > 0) {
            for (Object elem : found) {
                results.add(elem.toString());
            }
        }
    }

    private Set<Object> search(final Object object, Set<Object> results, final String path) {
        if (object instanceof Map) {
            final Map<String, Object> map = (Map<String, Object>) object;
            Object value = JavaScriptEngineFactory.walkJson(map, path);
            if (value != null) {
                if (results == null) {
                    results = new LinkedHashSet<Object>();
                }
                results.add(value);
            }
            for (final Map.Entry<String, Object> entry : map.entrySet()) {
                // final String key = entry.getKey();
                value = entry.getValue();
                if (value instanceof List || value instanceof Map) {
                    final Set<Object> found = search(value, results, path);
                    if (found != null) {
                        if (results == null) {
                            results = found;
                        } else {
                            results.addAll(found);
                        }
                    }
                }
            }
        } else if (object instanceof List) {
            final List<Object> array = (List) object;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final Set<Object> found = search(arrayo, results, path);
                    if (found != null) {
                        if (results == null) {
                            results = found;
                        } else {
                            results.addAll(found);
                        }
                    }
                }
            }
        }
        return results;
    }

    private String findPlaylistDescription(final Object o) {
        Set<Object> found = search(o, null, "playlistMetadataRenderer/description");
        found = search(o, found, "playlistSidebarPrimaryInfoRenderer/description/simpleText");
        if (found != null && found.size() > 0) {
            return found.iterator().next().toString();
        }
        return null;
    }

    private String findPlaylistOwnerName(final Object o) {
        final Set<Object> found = search(o, null, "playlistSidebarSecondaryInfoRenderer/videoOwner/videoOwnerRenderer/title/runs/{0}/text");
        if (found != null && found.size() > 0) {
            return found.iterator().next().toString();
        }
        return null;
    }

    private String findPlaylistTitle(final Object o) {
        final Set<Object> found = search(o, null, "playlistMetadataRenderer/title");
        if (found != null && found.size() > 0) {
            return found.iterator().next().toString();
        }
        return null;
    }

    private String findNumberOfVideosText(final Object o) {
        if (o instanceof Map) {
            String result = null;
            final Map<String, Object> map = (Map<String, Object>) o;
            final Object metadataPartsO = map.get("metadataParts");
            if (metadataPartsO instanceof List) {
                final List<Object> metadataParts = (List<Object>) metadataPartsO;
                for (final Object metadataPartO : metadataParts) {
                    final Set<Object> found = search(metadataPartO, null, "text/content");
                    if (found != null) {
                        for (final Object elem : found) {
                            final String text = elem.toString();
                            if (text != null && text.matches("(?i)[0-9\\.,]+\\s*Videos")) {
                                return text.replaceFirst("(?i)\\s*Videos", "").replace(",", ".");
                            }
                        }
                    }
                }
            }
            for (final Map.Entry<String, Object> entry : map.entrySet()) {
                // final String key = entry.getKey();
                final Object value = entry.getValue();
                if (value instanceof List || value instanceof Map) {
                    result = findNumberOfVideosText(value);
                    if (result != null) {
                        return result;
                    }
                }
            }
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final String result = findNumberOfVideosText(arrayo);
                    if (result != null) {
                        return result;
                    }
                }
            }
        }
        return null;
    }

    public static Browser prepBrowserWebAPI(final Browser br, final Account account) throws PluginException {
        return GoogleHelper.prepBrowserWebAPI(br, account, "youtube.com");
    }

    protected String extractWebsiteTitle(final Browser br) {
        return Encoding.htmlOnlyDecode(br.getRegex("<meta name=\"title\"\\s+[^<>]*content=\"(.*?)(?:\\s*-\\s*Youtube\\s*)?\"").getMatch(0));
    }

    /**
     * @param round
     * @return
     * @throws InterruptedException
     */
    protected int antiDdosSleep(int round) throws InterruptedException {
        sleep(((DDOS_WAIT_MAX * (Math.min(DDOS_INCREASE_FACTOR, round++))) / DDOS_INCREASE_FACTOR), getCurrentLink().getCryptedLink());
        return round;
    }

    /**
     * parses 'video_ids=' array, primarily used with watch_videos link
     */
    public ArrayList<YoutubeClipData> parseVideoIds(String video_ids) throws IOException, InterruptedException {
        // /watch_videos?title=Trending&video_ids=0KSOMA3QBU0,uT3SBzmDxGk,X7Xf8DsTWgs,72WhEqeS6AQ,Qc9c12q3mrc,6l7J1i1OkKs,zeu2tI-tqvs,o3mP3mJDL2k,jYdaQJzcAcw&feature=c4-overview&type=0&more_url=
        ArrayList<YoutubeClipData> ret = new ArrayList<YoutubeClipData>();
        int counter = 1;
        if (StringUtils.isNotEmpty(video_ids)) {
            String[] videos = new Regex(video_ids, "(" + VIDEO_ID_PATTERN + ")").getColumn(0);
            if (videos != null) {
                for (String vid : videos) {
                    ret.add(new YoutubeClipData(vid));
                }
            }
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}