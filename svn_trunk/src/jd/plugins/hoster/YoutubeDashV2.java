package jd.plugins.hoster;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.ComboBoxModel;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.Property;
import jd.config.SubConfiguration;
import jd.controlling.downloadcontroller.DownloadSession;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogJob;
import jd.controlling.downloadcontroller.ExceptionRunnable;
import jd.controlling.downloadcontroller.FileIsLockedException;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.linkchecker.LinkChecker;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcrawler.CheckableLink;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractNodeNotifier;
import jd.http.Browser;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.http.requests.GetRequest;
import jd.http.requests.HeadRequest;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountRequiredException;
import jd.plugins.BrowserAdapter;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.DownloadLinkDatabindingInterface;
import jd.plugins.FilePackage;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginBrowser;
import jd.plugins.PluginConfigPanelNG;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.PluginProgress;
import jd.plugins.download.DownloadInterface;
import jd.plugins.download.DownloadLinkDownloadable;
import jd.plugins.download.Downloadable;
import jd.plugins.download.HashResult;
import jd.plugins.download.raf.ChunkRange;

import org.appwork.exceptions.WTFException;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.action.BasicAction;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Files;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.logging2.extmanager.LoggerFactory;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpserver.HttpServer;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.parser.UrlQuery;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.ProgressDialog;
import org.appwork.utils.swing.dialog.ProgressDialog.ProgressGetter;
import org.jdownloader.controlling.DefaultDownloadLinkViewImpl;
import org.jdownloader.controlling.DownloadLinkView;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.controlling.ffmpeg.FFMpegException;
import org.jdownloader.controlling.ffmpeg.FFMpegProgress;
import org.jdownloader.controlling.ffmpeg.FFmpeg;
import org.jdownloader.controlling.ffmpeg.FFmpegMetaData;
import org.jdownloader.controlling.ffmpeg.FFmpegMetaData.MetaDataEntry;
import org.jdownloader.controlling.linkcrawler.LinkVariant;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.downloader.segment.Segment;
import org.jdownloader.downloader.segment.SegmentDownloader;
import org.jdownloader.downloader.text.TextDownloader;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo.PluginView;
import org.jdownloader.gui.views.linkgrabber.columns.VariantColumn;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.DownloadPluginProgress;
import org.jdownloader.plugins.SkipReason;
import org.jdownloader.plugins.SkipReasonException;
import org.jdownloader.plugins.components.youtube.ClipDataCache;
import org.jdownloader.plugins.components.youtube.StreamCollection;
import org.jdownloader.plugins.components.youtube.YoutubeClipData;
import org.jdownloader.plugins.components.youtube.YoutubeConfig;
import org.jdownloader.plugins.components.youtube.YoutubeFinalLinkResource;
import org.jdownloader.plugins.components.youtube.YoutubeHelper;
import org.jdownloader.plugins.components.youtube.YoutubeHostPluginInterface;
import org.jdownloader.plugins.components.youtube.YoutubeLinkGrabberExtender;
import org.jdownloader.plugins.components.youtube.YoutubeStreamData;
import org.jdownloader.plugins.components.youtube.choosevariantdialog.YoutubeVariantSelectionDialog;
import org.jdownloader.plugins.components.youtube.configpanel.YoutubeDashConfigPanel;
import org.jdownloader.plugins.components.youtube.keepForCompatibility.SubtitleVariantOld;
import org.jdownloader.plugins.components.youtube.variants.AbstractVariant;
import org.jdownloader.plugins.components.youtube.variants.AudioVariant;
import org.jdownloader.plugins.components.youtube.variants.DownloadType;
import org.jdownloader.plugins.components.youtube.variants.FileContainer;
import org.jdownloader.plugins.components.youtube.variants.SubtitleVariant;
import org.jdownloader.plugins.components.youtube.variants.SubtitleVariantInfo;
import org.jdownloader.plugins.components.youtube.variants.VariantGroup;
import org.jdownloader.plugins.components.youtube.variants.VariantInfo;
import org.jdownloader.plugins.components.youtube.variants.VideoVariant;
import org.jdownloader.plugins.components.youtube.variants.YoutubeSubtitleStorable;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.host.PluginFinder;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.settings.staticreferences.CFG_YOUTUBE;

@HostPlugin(revision = "$Revision: 51444 $", interfaceVersion = 3, names = { "youtube.com" }, urls = { "youtubev2://.+" })
public class YoutubeDashV2 extends PluginForHost implements YoutubeHostPluginInterface {
    private static final String    YT_ALTERNATE_VARIANT = "YT_ALTERNATE_VARIANT";
    private static final String    DASH_AUDIO_FINISHED  = "DASH_AUDIO_FINISHED";
    private static final String    DASH_AUDIO_ITAG      = "DASH_AUDIO_ITAG";
    private static final String    DASH_VIDEO_ITAG      = "DASH_VIDEO_ITAG";
    private static final String    DASH_VIDEO_FINISHED  = "DASH_VIDEO_FINISHED";
    private static final String    DASH_AUDIO_LOADED    = "DASH_AUDIO_LOADED";
    private static final String    DASH_VIDEO_LOADED    = "DASH_VIDEO_LOADED";
    private final String           DASH_AUDIO_CHUNKS    = "DASH_AUDIO_CHUNKS";
    private final String           DASH_VIDEO_CHUNKS    = "DASH_VIDEO_CHUNKS";
    private YoutubeDashConfigPanel configPanel;

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING, LazyPlugin.FEATURE.COOKIE_LOGIN_ONLY };
    }

    /**
     * Maybe useful in the future - this guy has a lot of knowledge and makes good scripts:
     * https://github.com/bromix/plugin.video.youtube/tree/master/resources/lib
     */
    @Override
    public String getAGBLink() {
        return "https://youtube.com/t/terms";
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return YoutubeConfig.class;
    }

    @Override
    public boolean isSpeedLimited(DownloadLink link, Account account) {
        return false;
    }

    @Override
    public String getPluginContentURL(final DownloadLink link) {
        final String videoID = link.getStringProperty(YoutubeHelper.YT_ID);
        final String playlistID = link.getStringProperty(YoutubeHelper.YT_PLAYLIST_ID);
        AbstractVariant variant = null;
        try {
            variant = getVariant(link);
        } catch (final Throwable e) {
            logger.log(e);
        }
        String url = YoutubeHelper.generateSingleVideoContentURL(videoID, playlistID, link.getIntegerProperty(YoutubeHelper.YT_PLAYLIST_POSITION, -1));
        if (PluginJsonConfig.get(YoutubeConfig.class).isEnableIncludeVariantStringInContentURLs() && variant != null) {
            /* Add this to URL so if user adds this again, crawler knows which variant to crawl. */
            url += "#variant=" + Encoding.urlEncode(Base64.encode(variant.getStorableString()));
        }
        return url;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkID = link.getSetLinkID();
        if (linkID != null) {
            final String playlistID = link.getStringProperty(YoutubeHelper.YT_PLAYLIST_ID);
            if (playlistID != null && YoutubeHelper.enablePlaylistSpecialDupeCheck()) {
                return linkID + "#playlist=" + playlistID;
            }
            return linkID;
        }
        return super.getLinkID(link);
    }

    @Override
    public PluginForHost assignPlugin(PluginFinder pluginFinder, final DownloadLink link) {
        final PluginForHost ret = super.assignPlugin(pluginFinder, link);
        final long convertTimestamp = 1650639863343l;
        if (ret != null && (link.getCreated() < convertTimestamp && link.getLongProperty("assignPlugin", -1l) != 3)) {
            try {
                final AbstractVariant variant = getVariant(link, false);
                if (variant != null && !(variant instanceof SubtitleVariant)) {
                    // update linkID due to changed JSON parser/formatter
                    // see AbstractVariant._getUniqueId
                    final String youtubeID = link.getStringProperty(YoutubeHelper.YT_ID);
                    if (StringUtils.isNotEmpty(youtubeID)) {
                        final String linkID = YoutubeHelper.createLinkID(youtubeID, variant);
                        if (StringUtils.isNotEmpty(linkID)) {
                            link.setLinkID(linkID);
                            link.setPluginPatternMatcher(linkID);
                            link.setProperty("assignPlugin", 3);
                        }
                    }
                }
            } catch (PluginException e) {
                LogController.CL(true).log(e);
            }
        }
        return ret;
    }

    @Override
    public Browser createNewBrowserInstance() {
        return new PluginBrowser<YoutubeDashV2>(this) {
            {
                setDebug(true);
            }
        };
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        new YoutubeHelper(br, getLogger()).login(account, true);
        final AccountInfo ai = new AccountInfo();
        ai.setStatus(_GUI.T.lit_account_is_ok());
        return ai;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    public static final class DashDownloadPluginProgress extends DownloadPluginProgress {
        private final long           totalSize;
        private final PluginProgress progress;
        private final long           chunkOffset;

        public DashDownloadPluginProgress(Downloadable downloadable, DownloadInterface downloadInterface, Color color, long totalSize, PluginProgress progress, long chunkOffset) {
            super(downloadable, downloadInterface, color);
            this.totalSize = totalSize;
            this.progress = progress;
            this.chunkOffset = chunkOffset;
        }

        @Override
        public long getCurrent() {
            final long ret = chunkOffset + ((DownloadInterface) progress.getProgressSource()).getTotalLinkBytesLoadedLive();
            return ret;
        }

        @Override
        public long getTotal() {
            return totalSize;
        }

        public long getDuration() {
            return System.currentTimeMillis() - startTimeStamp;
        }

        public long getSpeed() {
            return ((DownloadInterface) progress.getProgressSource()).getManagedConnetionHandler().getSpeed();
        }
    }

    @Override
    public ConfigContainer getConfig() {
        throw new WTFException("Not implemented");
    }

    @Override
    public SubConfiguration getPluginConfig() {
        throw new WTFException("Not implemented");
    }

    public YoutubeDashV2(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www.youtube.com/");
    }

    @Override
    public PluginConfigPanelNG createConfigPanel() {
        YoutubeDashConfigPanel panel = this.configPanel;
        if (panel == null) {
            panel = this.configPanel = new YoutubeDashConfigPanel(getDescription());
        }
        return panel;
    }

    public String getMirrorID(DownloadLink link) {
        return "Youtube:" + link.getStringProperty(YoutubeHelper.YT_VARIANT) + link.getName() + "_" + link.getView().getBytesTotal();
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink downloadLink) throws Exception {
        return requestFileInformation(downloadLink, false);
    }

    private final String ERROR_AGE_RESTRICTED_CONTENT = "Account needed to access age restricted content";

    private AvailableStatus requestFileInformation(final DownloadLink downloadLink, final boolean isDownload) throws Exception {
        final String id = downloadLink.getStringProperty(YoutubeHelper.YT_ID);
        final YoutubeHelper helper = new YoutubeHelper(br, getLogger());
        helper.setupProxy();
        final AbstractVariant variant = getVariant(downloadLink);
        {
            logger.info("requestFileInformation:" + id + "|" + variant.getVariantDetails());
            final String linkid = downloadLink.getLinkID();
            if (linkid != null && !isDownload) {
                switch (variant.getType()) {
                case SUBTITLES:
                case VIDEO:
                case DASH_AUDIO:
                case DASH_VIDEO: {
                    final String newID = YoutubeHelper.createLinkID(id, variant);
                    if (!newID.equals(linkid)) {
                        downloadLink.setLinkID(newID);
                        return AvailableStatus.TRUE;
                    }
                }
                break;
                case IMAGE: {
                    final String newID;
                    if (VariantGroup.IMAGE_PLAYLIST_COVER.equals(variant.getBaseVariant().getGroup()) && downloadLink.hasProperty(YoutubeHelper.YT_PLAYLIST_ID)) {
                        newID = YoutubeHelper.createLinkID(downloadLink.getStringProperty(YoutubeHelper.YT_PLAYLIST_ID), variant);
                    } else {
                        newID = YoutubeHelper.createLinkID(id, variant);
                    }
                    if (!newID.equals(linkid)) {
                        downloadLink.setLinkID(newID);
                        return AvailableStatus.TRUE;
                    }
                }
                break;
                case DESCRIPTION:
                    break;
                default:
                    break;
                }
            }
        }
        boolean verifiedSize = true;
        long totalSize = -1;
        // youtube uses redirects - maybe for load balancing
        br.setFollowRedirects(true);
        switch (variant.getType()) {
        case SUBTITLES:
            for (int i = 0; i < 2; i++) {
                final VariantInfo urls = getAndUpdateVariantInfo(downloadLink, true);
                if (urls == null || urls.getDataStreams() == null || urls.getDataStreams().size() == 0) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                final Browser br2 = br.cloneBrowser();
                br2.getHeaders().remove("Accept-Encoding");
                URLConnectionAdapter con = null;
                IOException ioe = null;
                try {
                    con = br2.openGetConnection(urls.getDataStreams().get(0).getUrl());
                } catch (final IOException e) {
                    ioe = e;
                    logger.log(e);
                } finally {
                    if (con != null) {
                        con.disconnect();
                    }
                }
                if (con == null || !con.getContentType().startsWith("text/xml") || con.getResponseCode() != 200) {
                    if (i == 0) {
                        resetStreamUrls(downloadLink);
                        continue;
                    } else if (ioe != null) {
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, null, ioe);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                    }
                }
                if (con.getCompleteContentLength() > 0) {
                    totalSize = con.getCompleteContentLength();
                }
                break;
            }
            break;
        case DESCRIPTION:
            return requestFileInformationDescription(downloadLink, id, helper);
        case IMAGE:
            for (int i = 0; i < 2; i++) {
                final VariantInfo urls = getAndUpdateVariantInfo(downloadLink, true);
                if (urls == null || urls.getDataStreams() == null || urls.getDataStreams().size() == 0) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                URLConnectionAdapter con = null;
                IOException ioe = null;
                try {
                    con = br.openGetConnection(urls.getDataStreams().get(0).getUrl());
                } catch (final IOException e) {
                    ioe = e;
                    logger.log(e);
                } finally {
                    if (con != null) {
                        con.disconnect();
                    }
                }
                if (con == null || !this.looksLikeDownloadableContent(con)) {
                    if (i == 0) {
                        resetStreamUrls(downloadLink);
                        continue;
                    } else if (ioe != null) {
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, null, ioe);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                    }
                }
                totalSize = con.getCompleteContentLength();
                break;
            }
            break;
        case HLS_VIDEO:
            return AvailableStatus.TRUE;
        default:
            br.setFollowRedirects(true);
            final HashSet<LinkVariant> checkedAlternatives = new HashSet<LinkVariant>();
            final AbstractVariant orgVariant = getVariant(downloadLink);
            try {
                int maxAlternativesChecks = CFG_YOUTUBE.CFG.isAutoAlternativeSearchEnabled() ? CFG_YOUTUBE.CFG.getAutoAlternativeSearchDepths() : 1;
                final boolean hasCache = ClipDataCache.hasCache(helper, downloadLink);
                boolean triedToResetTheCache = !hasCache;
                test: while (maxAlternativesChecks-- >= 0 || triedToResetTheCache) {
                    final AbstractVariant currentVariant = getVariant(downloadLink);
                    checkedAlternatives.add(currentVariant);
                    YoutubeFinalLinkResource cache = null;
                    URLConnectionAdapter lastCon = null;
                    try {
                        // do no set variant, do this inloop
                        totalSize = 0;
                        boolean ok = false;
                        final VariantInfo urls = getAndUpdateVariantInfo(downloadLink, true);
                        logger.info("Try " + urls);
                        YoutubeFinalLinkResource workingVideoStream = null;
                        YoutubeFinalLinkResource workingAudioStream = null;
                        YoutubeFinalLinkResource workingDataStream = null;
                        if (urls != null && urls.getVideoStreams() != null) {
                            PluginException firstException = null;
                            for (final YoutubeStreamData si : urls.getVideoStreams()) {
                                cache = new YoutubeFinalLinkResource(si);
                                if (cache.getSegments() != null) {
                                    verifiedSize = false;
                                    Long estimatedSize = si.getContentLength();
                                    if (estimatedSize == -1) {
                                        estimatedSize = si.estimatedContentLength();
                                    }
                                    if (estimatedSize == -1) {
                                        estimatedSize = guessTotalSize(cache.getBaseUrl(), cache.getSegments());
                                        if (estimatedSize != null && estimatedSize == -1) {
                                            if (firstException == null) {
                                                firstException = new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                                            }
                                            continue;
                                        }
                                    }
                                    workingVideoStream = cache;
                                    if (estimatedSize != null) {
                                        totalSize += estimatedSize;
                                        ok |= estimatedSize > 0;
                                        if (estimatedSize > 0) {
                                            logger.info("update estimatedContentLength! itag=" + workingVideoStream.getItag() + " from=" + workingVideoStream.getEstimatedContentLength() + " to=" + estimatedSize);
                                            workingVideoStream.setEstimatedContentLength(estimatedSize);
                                        }
                                    } else {
                                        ok = true;
                                    }
                                    firstException = null;
                                    break;
                                } else {
                                    final String url = cache.getBaseUrl();
                                    // if (false && vv.getQualityRating() > VideoResolution.P_360.getRating()) {
                                    // url = url.replace("signature=", "signature=BAD");
                                    // }
                                    IOException ioe = null;
                                    try {
                                        lastCon = null;
                                        lastCon = br.openRequestConnection(new HeadRequest(url));
                                    } catch (IOException e) {
                                        ioe = e;
                                        logger.log(e);
                                    } finally {
                                        if (lastCon != null) {
                                            lastCon.disconnect();
                                        }
                                    }
                                    if (lastCon != null && lastCon.getResponseCode() == 200) {
                                        workingVideoStream = cache;
                                        // downloadLink.setProperty(YoutubeHelper.YT_STREAM_DATA_VIDEO, cache);
                                        final long contentLenght = lastCon.getCompleteContentLength();
                                        if (contentLenght > 0) {
                                            totalSize += contentLenght;
                                            if (workingVideoStream.getContentLength() != contentLenght) {
                                                logger.info("update contentLength! itag=" + workingVideoStream.getItag() + " from=" + workingVideoStream.getContentLength() + " to=" + contentLenght);
                                                workingVideoStream.setContentLength(contentLenght);
                                            }
                                        }
                                        firstException = null;
                                        ok |= true;
                                        break;
                                    } else {
                                        if (si.getSrc() != null) {
                                            logger.info("Failed for Stream Source: " + si.getSrc());
                                        }
                                        if (lastCon != null && lastCon.getResponseCode() == 403) {
                                            firstException = new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE);
                                        } else if (ioe != null) {
                                            firstException = new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, null, ioe);
                                        } else if (firstException == null) {
                                            firstException = new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                                        }
                                        continue;
                                    }
                                }
                            }
                            if (firstException != null) {
                                throw firstException;
                            }
                        }
                        if ((variant.getiTagAudioOrVideoItagEquivalent() != variant.getiTagVideo() || !ok) && urls != null && urls.getAudioStreams() != null) {
                            PluginException firstException = null;
                            for (final YoutubeStreamData si : urls.getAudioStreams()) {
                                cache = new YoutubeFinalLinkResource(si);
                                if (cache.getSegments() != null) {
                                    verifiedSize = false;
                                    Long estimatedSize = si.getContentLength();
                                    if (estimatedSize == -1) {
                                        estimatedSize = si.estimatedContentLength();
                                    }
                                    if (estimatedSize == -1) {
                                        estimatedSize = guessTotalSize(cache.getBaseUrl(), cache.getSegments());
                                        if (estimatedSize != null && estimatedSize == -1) {
                                            // if (i == 0) {
                                            // resetStreamUrls(downloadLink);
                                            // continue;
                                            // }
                                            if (si.getSrc() != null) {
                                                logger.info("Stream Source: " + si.getSrc());
                                            }
                                            if (firstException == null) {
                                                firstException = new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                                            }
                                            continue;
                                        }
                                    }
                                    workingAudioStream = cache;
                                    if (estimatedSize != null) {
                                        totalSize += estimatedSize;
                                        ok |= estimatedSize > 0;
                                        if (estimatedSize > 0) {
                                            logger.info("update estimatedContentLength! itag=" + workingAudioStream.getItag() + " from=" + workingAudioStream.getEstimatedContentLength() + " to=" + estimatedSize);
                                            workingAudioStream.setEstimatedContentLength(estimatedSize);
                                        }
                                    } else {
                                        ok = true;
                                    }
                                    firstException = null;
                                    break;
                                } else {
                                    final String url = cache.getBaseUrl();
                                    IOException ioe = null;
                                    try {
                                        lastCon = null;
                                        lastCon = br.openRequestConnection(new HeadRequest(url));
                                    } catch (IOException e) {
                                        ioe = e;
                                        logger.log(e);
                                    } finally {
                                        if (lastCon != null) {
                                            lastCon.disconnect();
                                        }
                                    }
                                    if (lastCon != null && lastCon.getResponseCode() == 200) {
                                        workingAudioStream = cache;
                                        final long contentLenght = lastCon.getCompleteContentLength();
                                        if (contentLenght > 0) {
                                            totalSize += contentLenght;
                                            if (workingAudioStream.getContentLength() != contentLenght) {
                                                logger.info("update contentLength! itag=" + workingAudioStream.getItag() + " from=" + workingAudioStream.getContentLength() + " to=" + contentLenght);
                                                workingAudioStream.setContentLength(contentLenght);
                                            }
                                        }
                                        firstException = null;
                                        ok |= true;
                                        break;
                                    } else {
                                        if (lastCon != null && lastCon.getResponseCode() == 403) {
                                            firstException = new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE);
                                        } else if (ioe != null) {
                                            firstException = new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, null, ioe);
                                        } else if (firstException == null) {
                                            firstException = new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                                        }
                                        continue;
                                    }
                                }
                            }
                            if (firstException != null) {
                                throw firstException;
                            }
                        }
                        downloadLink.setProperty(YoutubeHelper.YT_STREAM_DATA_AUDIO, workingAudioStream);
                        downloadLink.setProperty(YoutubeHelper.YT_STREAM_DATA_VIDEO, workingVideoStream);
                        downloadLink.setProperty(YoutubeHelper.YT_STREAM_DATA_DATA, workingDataStream);
                        if (!ok) {
                            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                        }
                        break test;
                    } catch (final PluginException e) {
                        if (e.getMessage() != null) {
                            if (StringUtils.equalsIgnoreCase(e.getMessage(), "This video is private")) {
                                throw e;
                            }
                            if (e.getMessage().contains(ClipDataCache.THE_DOWNLOAD_IS_NOT_AVAILABLE_IN_YOUR_COUNTRY)) {
                                throw e;
                            }
                        }
                        final boolean hasCachedVideo = downloadLink.getProperty(YoutubeHelper.YT_STREAM_DATA_VIDEO) != null;
                        final boolean hasCachedAudio = downloadLink.getProperty(YoutubeHelper.YT_STREAM_DATA_AUDIO) != null;
                        final boolean hasCachedData = downloadLink.getProperty(YoutubeHelper.YT_STREAM_DATA_DATA) != null;
                        if (hasCachedVideo || hasCachedAudio || hasCachedData || !triedToResetTheCache) {
                            // the url has been restored from older cached streamdata
                            resetStreamUrls(downloadLink);
                            if (!triedToResetTheCache) {
                                triedToResetTheCache = true;
                                ClipDataCache.clearCache(downloadLink);
                            }
                            continue test;
                        }
                        // issue Bug #82347
                        // at this point, the urls are freshly reloaded. There are several mysterious mediaurls (the do not contain any ei=
                        // parameter) that will return 403.
                        // if these exist, this code below would break the alternative searching.
                        // so if you ever experience this IP Mismatch issue, double check against this case.
                        // for now, I guess it's safe to remove these lines.
                        // if (con != null && con.getResponseCode() == 403) {
                        // // Probably IP Mismatch
                        // // do not check alternatives
                        // throw e;
                        // }
                        // age Protection. If age protection is active, all requests may return 403 without an youtube account
                        if (lastCon != null && lastCon.getResponseCode() == 403) {
                            final YoutubeClipData clipData = ClipDataCache.hasCache(helper, downloadLink) ? ClipDataCache.get(helper, downloadLink) : null;
                            if (clipData != null && clipData.ageCheck) {
                                throw new AccountRequiredException(ERROR_AGE_RESTRICTED_CONTENT);
                            } else if (cache != null && cache.getThrottle() == 0) {
                                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                            }
                        }
                        LinkVariant alternative = getAlternatives(helper, downloadLink, orgVariant, checkedAlternatives);
                        while (alternative != null) {
                            logger.info("Try next alternative variant: " + alternative);
                            if (alternative instanceof AbstractVariant && cache != null && cache.getItag().getITAG() == ((AbstractVariant) alternative).getiTagAudioOrVideoItagEquivalent().getITAG()) {
                                logger.info("Skip next alternative variant: " + alternative + " because of same iTag:" + cache.getItag().getITAG());
                                checkedAlternatives.add(alternative);
                                alternative = getAlternatives(helper, downloadLink, orgVariant, checkedAlternatives);
                            } else {
                                downloadLink.getTempProperties().setProperty(YT_ALTERNATE_VARIANT, alternative);
                                continue test;
                            }
                        }
                        downloadLink.getTempProperties().removeProperty(YT_ALTERNATE_VARIANT);
                        throw e;
                    }
                }
                final boolean hasCachedVideo = downloadLink.getProperty(YoutubeHelper.YT_STREAM_DATA_VIDEO) != null;
                final boolean hasCachedAudio = downloadLink.getProperty(YoutubeHelper.YT_STREAM_DATA_AUDIO) != null;
                final boolean hasCachedData = downloadLink.getProperty(YoutubeHelper.YT_STREAM_DATA_DATA) != null;
                if (!hasCachedVideo && !hasCachedAudio && !hasCachedData) {
                    final YoutubeClipData clipData = ClipDataCache.hasCache(helper, downloadLink) ? ClipDataCache.get(helper, downloadLink) : null;
                    if (clipData != null && clipData.ageCheck) {
                        throw new AccountRequiredException(ERROR_AGE_RESTRICTED_CONTENT);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                    }
                }
            } finally {
                final AbstractVariant alternative = (AbstractVariant) downloadLink.getTempProperties().getProperty(YT_ALTERNATE_VARIANT);
                if (alternative != null) {
                    downloadLink.getTempProperties().removeProperty(YT_ALTERNATE_VARIANT);
                    LinkCollector.getInstance().setActiveVariantForLink(downloadLink, alternative);
                }
            }
        }
        // HTTP/1.1 403 Forbidden
        // helper.login(false, false);
        // // we should cache this information:
        downloadLink.setFinalFileName(helper.createFilename(downloadLink));
        final String oldLinkName = downloadLink.getStringProperty("name", null);
        if (StringUtils.isNotEmpty(oldLinkName)) {
            // old link?
            downloadLink.setFinalFileName(oldLinkName);
        }
        downloadLink.setInternalTmpFilenameAppend(null);
        final AbstractVariant v = getVariant(downloadLink);
        if (v.hasConverter(downloadLink)) {
            downloadLink.setInternalTmpFilenameAppend(".tmp");
        }
        if (totalSize > 0) {
            if (verifiedSize) {
                downloadLink.setVerifiedFileSize(totalSize);
            } else {
                downloadLink.setDownloadSize(totalSize);
            }
        }
        return AvailableStatus.TRUE;
    }

    private LinkVariant getAlternatives(YoutubeHelper helper, final DownloadLink downloadLink, final AbstractVariant variant, final HashSet<LinkVariant> blacklisted) throws Exception {
        if (!CFG_YOUTUBE.CFG.isAutoAlternativeSearchEnabled() || CFG_YOUTUBE.CFG.getAutoAlternativeSearchDepths() <= 0) {
            // auto alternative search is disabled
            return null;
        }
        if (helper == null) {
            helper = new YoutubeHelper(createNewBrowserInstance(), LoggerFactory.getDefaultLogger());
        }
        final YoutubeClipData clipData = ClipDataCache.get(helper, downloadLink);
        switch (variant.getGroup()) {
        case DESCRIPTION:
            return null;
        case SUBTITLES:
            final List<VariantInfo> subtitles = clipData.findSubtitleVariants();
            if (subtitles != null) {
                for (VariantInfo v : subtitles) {
                    if (StringUtils.equals(v.getVariant()._getUniqueId(), variant._getUniqueId())) {
                        if (!blacklisted.contains(v.getVariant())) {
                            return v.getVariant();
                        }
                    }
                }
                Locale choosenLocale = ((SubtitleVariant) variant).getGenericInfo()._getLocale();
                for (VariantInfo v : subtitles) {
                    Locale vLocale = ((SubtitleVariant) v.getVariant()).getGenericInfo()._getLocale();
                    if (StringUtils.equals(vLocale.getLanguage(), choosenLocale.getLanguage())) {
                        if (!blacklisted.contains(v.getVariant())) {
                            return v.getVariant();
                        }
                    }
                }
            }
            break;
        case AUDIO:
        case IMAGE:
        case VIDEO:
            final List<VariantInfo> alternatives = new ArrayList<VariantInfo>();
            final List<VariantInfo> variants = clipData.findVariants();
            final Iterator<VariantInfo> it = variants.iterator();
            while (it.hasNext()) {
                final VariantInfo next = it.next();
                if (next.getVariant().getGroup() != variant.getGroup()) {
                    it.remove();
                }
            }
            if (variants.size() == 0) {
                return null;
            }
            helper.extendedDataLoading(variants);
            // sorts the best matching variants first. (based on quality rating)
            Collections.sort(variants, new Comparator<VariantInfo>() {
                @Override
                public int compare(VariantInfo o1, VariantInfo o2) {
                    return o2.compareTo(o1);
                }
            });
            for (VariantInfo v : variants) {
                if (StringUtils.equals(v.getVariant()._getUniqueId(), variant._getUniqueId())) {
                    if (!blacklisted.contains(v.getVariant())) {
                        if (!alternatives.contains(v)) {
                            alternatives.add(v);
                        }
                    }
                }
            }
            VariantInfo vCur = null;
            VariantInfo vLast = null;
            for (int i = 0; i < variants.size(); i++) {
                vCur = variants.get(i);
                int comCur = variant.compareTo(vCur.getVariant());
                if (comCur == 0) {
                    if (!blacklisted.contains(vCur.getVariant())) {
                        if (!alternatives.contains(vCur)) {
                            alternatives.add(vCur);
                        }
                    }
                } else if (comCur > 0) {
                    if (vLast != null) {
                        if (!blacklisted.contains(vLast.getVariant())) {
                            if (!alternatives.contains(vLast)) {
                                alternatives.add(vLast);
                            }
                        }
                    } else {
                        if (!blacklisted.contains(vCur.getVariant())) {
                            if (!alternatives.contains(vCur)) {
                                alternatives.add(vCur);
                            }
                        }
                    }
                }
                vLast = vCur;
            }
            for (VariantInfo v : variants) {
                if (StringUtils.equals(v.getVariant().getTypeId(), variant.getTypeId())) {
                    if (!blacklisted.contains(v.getVariant())) {
                        if (!alternatives.contains(v)) {
                            alternatives.add(v);
                        }
                    }
                }
            }
            for (VariantInfo v : variants) {
                if (v.getVariant().getGroup() == variant.getGroup()) {
                    if (v.getVariant().getContainer() == variant.getContainer()) {
                        if (variant instanceof VideoVariant && v.getVariant() instanceof VideoVariant) {
                            if (((VideoVariant) v.getVariant()).getVideoCodec() == ((VideoVariant) variant).getVideoCodec()) {
                                if (((VideoVariant) v.getVariant()).getAudioCodec() == ((VideoVariant) variant).getAudioCodec()) {
                                    if (!blacklisted.contains(v.getVariant())) {
                                        if (!alternatives.contains(v)) {
                                            alternatives.add(v);
                                        }
                                    }
                                }
                            }
                        } else if (variant instanceof AudioVariant && v.getVariant() instanceof AudioVariant) {
                            if (v.getVariant().getContainer() == variant.getContainer()) {
                                if (((AudioVariant) v.getVariant()).getAudioCodec() == ((AudioVariant) variant).getAudioCodec()) {
                                    if (!blacklisted.contains(v.getVariant())) {
                                        if (!alternatives.contains(v)) {
                                            alternatives.add(v);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            for (VariantInfo v : variants) {
                if (v.getVariant().getGroup() == variant.getGroup()) {
                    if (v.getVariant().getContainer() == variant.getContainer()) {
                        if (!blacklisted.contains(v.getVariant())) {
                            if (!alternatives.contains(v)) {
                                alternatives.add(v);
                            }
                        }
                    }
                }
            }
            if (alternatives.size() > 0) {
                if (variant instanceof VideoVariant) {
                    VariantInfo best = null;
                    final VideoVariant last = (VideoVariant) variant;
                    for (VariantInfo alternative : alternatives) {
                        if (alternative.getVariant() instanceof VideoVariant) {
                            final VideoVariant a = (VideoVariant) alternative.getVariant();
                            if (a.getVideoCodec() == last.getVideoCodec() && a.getAudioCodec() == last.getAudioCodec() && a.getContainer() == last.getContainer() && a.getVideoResolution() == last.getVideoResolution()) {
                                if (best == null) {
                                    best = alternative;
                                } else {
                                    best = null;
                                    break;
                                }
                            }
                        }
                    }
                    if (best != null) {
                        return best.getVariant();
                    }
                }
                return alternatives.get(0).getVariant();
            } else {
                return null;
            }
        }
        return null;
    };

    private AvailableStatus requestFileInformationDescription(DownloadLink downloadLink, String id, YoutubeHelper helper) throws Exception, UnsupportedEncodingException {
        String description = downloadLink.getTempProperties().getStringProperty(YoutubeHelper.YT_DESCRIPTION);
        if (StringUtils.isEmpty(description)) {
            final YoutubeClipData vid = ClipDataCache.load(helper, new YoutubeClipData(id));
            downloadLink.getTempProperties().setProperty(YoutubeHelper.YT_DESCRIPTION, description = vid.description);
        }
        downloadLink.setDownloadSize(description.getBytes("UTF-8").length);
        return AvailableStatus.TRUE;
    }

    private Long guessTotalSize(String base, String[] segs) {
        final int jump = Math.max(1, segs.length / 10);
        int segments = 0;
        Long size = null;
        boolean lastFlag = false;
        int fastBreak = 0;
        boolean headRequest = true;
        for (int i = 1; i < segs.length; i += jump) {
            final String url = StringUtils.startsWithCaseInsensitive(segs[i], "http") ? segs[i] : (base + segs[i]);
            try {
                final Request request;
                if (headRequest) {
                    request = new HeadRequest(url);
                } else {
                    request = new GetRequest(url);
                }
                final URLConnectionAdapter con = br.openRequestConnection(request);
                try {
                    if (con.getResponseCode() == 200) {
                        lastFlag = true;
                        segments++;
                        if (size == null) {
                            size = 0l;
                        }
                        if (con.getLongContentLength() > 0) {
                            fastBreak = 0;
                            size += con.getLongContentLength();
                        } else {
                            headRequest = false;
                            if (fastBreak++ > 2) {
                                return null;
                            }
                        }
                    } else {
                        if (lastFlag) {
                            lastFlag = false;
                        } else {
                            if (size == null) {
                                return -1l;
                            } else {
                                return null;
                            }
                        }
                    }
                } finally {
                    con.disconnect();
                }
            } catch (final IOException e) {
                logger.log(e);
                if (lastFlag) {
                    lastFlag = false;
                } else {
                    if (size == null) {
                        return -1l;
                    } else {
                        return null;
                    }
                }
            }
        }
        if (segments > 0 && segs.length > 1 && size != null && size.longValue() > 0) {
            // first segment is a init segment and has only ~802 bytes
            return (segs.length - 1) * (size.longValue() / segments) + 802;
        } else {
            if (size == null) {
                return -1l;
            } else {
                return null;
            }
        }
    }

    private VariantInfo getAndUpdateVariantInfo(final DownloadLink downloadLink, final boolean allowCache) throws Exception {
        return getAndUpdateVariantInfo(downloadLink, allowCache, true);
    }

    private VariantInfo getAndUpdateVariantInfo(final DownloadLink downloadLink, final boolean allowCache, final boolean storeTempProperty) throws Exception {
        final YoutubeFinalLinkResource video = downloadLink.getObjectProperty(YoutubeHelper.YT_STREAM_DATA_VIDEO, YoutubeFinalLinkResource.TYPE_REF);
        final YoutubeFinalLinkResource audio = downloadLink.getObjectProperty(YoutubeHelper.YT_STREAM_DATA_AUDIO, YoutubeFinalLinkResource.TYPE_REF);
        final YoutubeFinalLinkResource data = downloadLink.getObjectProperty(YoutubeHelper.YT_STREAM_DATA_DATA, YoutubeFinalLinkResource.TYPE_REF);
        if (allowCache && (video != null || audio != null || data != null)) {
            // seems like we have cached final informations
            final StreamCollection lstVideo;
            if (video != null) {
                lstVideo = new StreamCollection();
                lstVideo.add(video.toStreamDataObject());
            } else {
                lstVideo = null;
            }
            final StreamCollection lstAudio;
            if (audio != null) {
                lstAudio = new StreamCollection();
                lstAudio.add(audio.toStreamDataObject());
            } else {
                lstAudio = null;
            }
            final StreamCollection lstData;
            if (data != null) {
                lstData = new StreamCollection();
                lstData.add(data.toStreamDataObject());
            } else {
                lstData = null;
            }
            final VariantInfo ret = new VariantInfo(getVariant(downloadLink, storeTempProperty), lstAudio, lstVideo, lstData);
            if (ret.isValid()) {
                return ret;
            }
        }
        return updateUrls(downloadLink);
    }

    protected AbstractVariant getVariant(final DownloadLink downloadLink) throws PluginException {
        return getVariant(downloadLink, true);
    }

    @Override
    public String getPluginCustomURL(DownloadLink link) {
        String ret = super.getPluginCustomURL(link);
        if (ret == null && link != null) {
            try {
                // provide access via customURL to subtitle URL
                final AbstractVariant variant = getVariant(link);
                if (variant != null && DownloadType.SUBTITLES.equals(variant.getType())) {
                    ret = ((YoutubeSubtitleStorable) variant.getGenericInfo()).getFullUrl();
                }
            } catch (Exception e) {
                logger.log(e);
            }
        }
        return ret;
    }

    protected AbstractVariant getVariant(final DownloadLink downloadLink, final boolean storeTempProperty) throws PluginException {
        final Object alternative = downloadLink.getTempProperties().getProperty(YT_ALTERNATE_VARIANT);
        if (alternative != null && alternative instanceof AbstractVariant) {
            return (AbstractVariant) alternative;
        } else {
            final AbstractVariant ret = AbstractVariant.get(downloadLink, storeTempProperty);
            if (ret == null) {
                final String ytV = downloadLink.getStringProperty(YoutubeHelper.YT_VARIANT);
                getLogger().warning("Invalid Variant: " + ytV);
                throw new PluginException(LinkStatus.ERROR_FATAL, "INVALID VARIANT: " + ytV);
            } else {
                return ret;
            }
        }
    }

    private VariantInfo updateUrls(DownloadLink downloadLink) throws Exception {
        final AbstractVariant variant = getVariant(downloadLink);
        final String youtubeID = downloadLink.getStringProperty(YoutubeHelper.YT_ID);
        logger.info("updateUrls:" + youtubeID + "|" + variant.getVariantDetails());
        if (br == null) {
            setBrowser(createNewBrowserInstance());
        }
        final YoutubeClipData clipData = ClipDataCache.get(new YoutubeHelper(br, getLogger()), downloadLink);
        if (variant instanceof SubtitleVariant) {
            final SubtitleVariant stVariant = ((SubtitleVariant) variant);
            for (YoutubeSubtitleStorable si : clipData.subtitles) {
                if (StringUtils.equals(stVariant.getGenericInfo()._getUniqueId(), si._getUniqueId())) {
                    stVariant.setGenericInfo(si);
                    final SubtitleVariantInfo vi = new SubtitleVariantInfo(stVariant, clipData);
                    // downloadLink.getTempProperties().setProperty(YoutubeHelper.YT_VARIANT_INFO, vi);
                    return vi;
                }
            }
            for (YoutubeSubtitleStorable si : clipData.subtitles) {
                if (StringUtils.equals(stVariant.getGenericInfo().getLanguage(), si.getLanguage())) {
                    stVariant.setGenericInfo(si);
                    final SubtitleVariantInfo vi = new SubtitleVariantInfo(stVariant, clipData);
                    // downloadLink.getTempProperties().setProperty(YoutubeHelper.YT_VARIANT_INFO, vi);
                    return vi;
                }
            }
            return null;
        }
        final StreamCollection audioStreams = clipData.getStreams(variant.getBaseVariant().getiTagAudio(), variant);
        final StreamCollection videoStreams = clipData.getStreams(variant.getiTagVideo(), variant);
        final StreamCollection dataStreams = clipData.getStreams(variant.getiTagData(), variant);
        if (variant.getBaseVariant().getiTagAudio() != null && audioStreams == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Invalid Audio Stream");
        } else if (variant.getiTagVideo() != null && videoStreams == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Invalid Video Stream");
        } else if (variant.getiTagData() != null && dataStreams == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Invalid Data Stream");
        }
        final VariantInfo vi = new VariantInfo(variant, audioStreams, videoStreams, dataStreams);
        // downloadLink.getTempProperties().setProperty(YoutubeHelper.YT_VARIANT_INFO, vi);
        return vi;
    }

    public static interface YoutubeProperties extends DownloadLinkDatabindingInterface {
        @Key(DASH_VIDEO_ITAG)
        void setDashVideoITag(final int itag);

        int getDashVideoITag();

        @Key(DASH_AUDIO_ITAG)
        void setDashAudioITag(final int itag);

        int getDashAudioITag();

        @Key(DASH_VIDEO_FINISHED)
        void setDashVideoFinished(boolean b);

        @Key(DASH_AUDIO_FINISHED)
        void setDashAudioFinished(boolean b);

        @Key(DASH_VIDEO_FINISHED)
        boolean isDashVideoFinished();

        @Key(DASH_AUDIO_FINISHED)
        boolean isDashAudioFinished();

        @Key(DASH_VIDEO_LOADED)
        long getDashVideoBytesLoaded();

        @Key(DASH_AUDIO_LOADED)
        long getDashAudioBytesLoaded();

        @Key(DASH_VIDEO_LOADED)
        void setDashVideoBytesLoaded(long bytesLoaded);

        @Key(DASH_AUDIO_LOADED)
        void setDashAudioBytesLoaded(long bytesLoaded);
    }

    private YoutubeFinalLinkResource getYoutubeFinalLinkResource(final DownloadLink downloadLink, final String propertyKey) throws Exception {
        YoutubeFinalLinkResource ret = downloadLink.getObjectProperty(propertyKey, YoutubeFinalLinkResource.TYPE_REF);
        if (ret == null) {
            requestFileInformation(downloadLink);
            ret = downloadLink.getObjectProperty(propertyKey, YoutubeFinalLinkResource.TYPE_REF);
            if (ret == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        return ret;
    }

    private Boolean downloadDashStream(final DownloadLink downloadLink, final YoutubeProperties data, final boolean isVideoStream) throws Exception {
        final long totalSize = downloadLink.getDownloadSize();
        // VariantInfo urls = getUrlPair(downloadLink);
        final String dashName;
        final String dashChunksProperty;
        // final String dashLoadedProperty;
        // final String dashFinishedProperty;
        final long chunkOffset;
        final YoutubeFinalLinkResource streamData;
        final String streamDataID;
        if (isVideoStream) {
            streamDataID = YoutubeHelper.YT_STREAM_DATA_VIDEO;
            streamData = getYoutubeFinalLinkResource(downloadLink, streamDataID);
            dashName = getDashVideoFileName(downloadLink);
            dashChunksProperty = DASH_VIDEO_CHUNKS;
            chunkOffset = 0;
            data.setDashVideoITag(streamData.getItag().getITAG());
        } else {
            streamDataID = YoutubeHelper.YT_STREAM_DATA_AUDIO;
            streamData = getYoutubeFinalLinkResource(downloadLink, streamDataID);
            dashName = getDashAudioFileName(downloadLink);
            dashChunksProperty = DASH_AUDIO_CHUNKS;
            final AbstractVariant variant = getVariant(downloadLink);
            if (variant.getType() == DownloadType.DASH_AUDIO) {
                chunkOffset = 0;
            } else {
                final YoutubeFinalLinkResource video = getYoutubeFinalLinkResource(downloadLink, YoutubeHelper.YT_STREAM_DATA_VIDEO);
                long videoContentLength = video != null ? video.getContentLength() : -1;
                if (videoContentLength == -1) {
                    final String videoDashName = getDashVideoFileName(downloadLink);
                    videoContentLength = Math.max(0, new File(downloadLink.getDownloadDirectory(), videoDashName).length());
                }
                chunkOffset = videoContentLength;
            }
            data.setDashAudioITag(streamData.getItag().getITAG());
        }
        final String dashPath = new File(downloadLink.getDownloadDirectory(), dashName).getAbsolutePath();
        final DownloadLink dashLink = new DownloadLink(this, dashName, getHost(), streamData.getBaseUrl(), true) {
            @Override
            public SingleDownloadController getDownloadLinkController() {
                return downloadLink.getDownloadLinkController();
            }

            @Override
            public void setAvailableStatus(AvailableStatus availableStatus) {
                downloadLink.setAvailableStatus(availableStatus);
            }

            @Override
            public void setAvailable(boolean available) {
                downloadLink.setAvailable(available);
            }
        };
        dashLink.setLivePlugin(this);
        final LinkStatus videoLinkStatus = new LinkStatus(dashLink);
        final String host = Browser.getHost(streamData.getBaseUrl());
        final Downloadable dashDownloadable = new DownloadLinkDownloadable(dashLink) {
            volatile long[] chunkProgress = null;
            {
                final Object ret = downloadLink.getProperty(dashChunksProperty, null);
                if (ret != null) {
                    if (ret instanceof long[] && ((long[]) ret).length > 0) {
                        chunkProgress = (long[]) ret;
                    } else if (ret instanceof List && ((List) ret).size() > 0) {
                        /* restored json-object */
                        final List<Object> list = ((List<Object>) ret);
                        final long[] ret2 = new long[list.size()];
                        for (int i = 0; i < ret2.length; i++) {
                            ret2[i] = Long.valueOf(list.get(i).toString());
                        }
                        chunkProgress = ret2;
                    }
                }
            }

            @Override
            public String getHost() {
                return host;
            }

            @Override
            public void setResumeable(boolean value) {
                downloadLink.setResumeable(value);
            }

            public long[] getChunksProgress() {
                return chunkProgress;
            }

            public void setChunksProgress(final long[] ls) {
                chunkProgress = ls;
                if (ls == null || ls.length == 0) {
                    downloadLink.setProperty(dashChunksProperty, Property.NULL);
                } else {
                    downloadLink.setProperty(dashChunksProperty, Arrays.copyOf(ls, ls.length));
                }
            }

            @Override
            public boolean isResumable() {
                return true;
            }

            @Override
            public void addDownloadTime(long ms) {
                downloadLink.addDownloadTime(ms);
            }

            @Override
            public void setHashResult(HashResult result) {
            }

            @Override
            public String getFinalFileOutput() {
                return dashPath;
            }

            @Override
            public void lockFiles(File... files) throws FileIsLockedException {
                /**
                 * do nothing, handleDownload does all the locking
                 */
            }

            @Override
            public void unlockFiles(File... files) {
                /**
                 * do nothing, handleDownload does all the locking
                 */
            }

            @Override
            public void waitForNextConnectionAllowed() throws InterruptedException {
                YoutubeDashV2.this.waitForNextConnectionAllowed(downloadLink);
            }

            @Override
            public String getFileOutput() {
                return dashPath;
            }

            @Override
            public int getLinkStatus() {
                return videoLinkStatus.getStatus();
            }

            @Override
            public long getVerifiedFileSize() {
                final long ret = streamData.getContentLength();
                if (ret <= 0) {
                    return -1;
                } else {
                    return ret;
                }
            }

            @Override
            public long getKnownDownloadSize() {
                return getVerifiedFileSize();
            }

            @Override
            public void setDownloadTotalBytes(long l) {
            }

            @Override
            public void setLinkStatus(final int finished) {
                if (isVideoStream) {
                    data.setDashVideoFinished(LinkStatus.FINISHED == finished);
                } else {
                    data.setDashAudioFinished(LinkStatus.FINISHED == finished);
                }
            }

            @Override
            public void setVerifiedFileSize(long length) {
                if (length > 0 && streamData.getContentLength() != length) {
                    logger.info("update contentLength! itag=" + streamData.getItag() + " from=" + streamData.getContentLength() + " to=" + length);
                    streamData.setContentLength(length);
                    downloadLink.setProperty(streamDataID, streamData);
                }
            }

            @Override
            public String getFinalFileName() {
                return dashName;
            }

            @Override
            public void setFinalFileName(String newfinalFileName) {
            }

            @Override
            public void setForcedFileName(String newforcedFileName) {
            }

            @Override
            public long getDownloadTotalBytes() {
                if (isVideoStream) {
                    return data.getDashVideoBytesLoaded();
                } else {
                    return data.getDashAudioBytesLoaded();
                }
            }

            @Override
            public void setDownloadBytesLoaded(long bytes) {
                if (isVideoStream) {
                    if (bytes < 0) {
                        data.setDashVideoBytesLoaded(0);
                    } else {
                        data.setDashVideoBytesLoaded(bytes);
                    }
                } else {
                    if (bytes < 0) {
                        data.setDashAudioBytesLoaded(0);
                    } else {
                        data.setDashAudioBytesLoaded(bytes);
                    }
                }
                downloadLink.setDownloadCurrent(chunkOffset + bytes);
            }

            @Override
            public boolean isHashCheckEnabled() {
                return false;
            }

            @Override
            public SingleDownloadController getDownloadLinkController() {
                return downloadLink.getDownloadLinkController();
            }

            final HashMap<PluginProgress, PluginProgress> pluginProgressMap = new HashMap<PluginProgress, PluginProgress>();

            @Override
            public void addPluginProgress(final PluginProgress progress) {
                final PluginProgress mapped;
                synchronized (pluginProgressMap) {
                    if (pluginProgressMap.containsKey(progress)) {
                        mapped = pluginProgressMap.get(progress);
                    } else if (progress != null && progress instanceof DownloadPluginProgress) {
                        mapped = new DashDownloadPluginProgress(this, (DownloadInterface) progress.getProgressSource(), progress.getColor(), totalSize, progress, chunkOffset);
                        pluginProgressMap.put(progress, mapped);
                    } else {
                        mapped = progress;
                    }
                }
                downloadLink.addPluginProgress(mapped);
            }

            @Override
            public boolean removePluginProgress(PluginProgress remove) {
                final PluginProgress mapped;
                synchronized (pluginProgressMap) {
                    if (pluginProgressMap.containsKey(remove)) {
                        mapped = pluginProgressMap.remove(remove);
                    } else {
                        mapped = remove;
                    }
                }
                return downloadLink.removePluginProgress(mapped);
            }
        };
        final YoutubeConfig youtubeConfig = PluginJsonConfig.get(YoutubeConfig.class);
        if (streamData.getSegments() != null) {
            final String[] streamSegments = streamData.getSegments();
            final List<Segment> allSegments = SegmentDownloader.buildSegments(new URL(streamData.getBaseUrl()), streamSegments);
            final List<Segment> downloadSegments = new ArrayList<Segment>(allSegments);
            final String resumeSegmentInfo = downloadLink.getStringProperty(dashChunksProperty + "_segment", null);
            final String resumeSegmentInfos[] = new Regex(resumeSegmentInfo, "^(\\d+)_(\\d+)_(\\d+)_(.+)$").getRow(0);
            int resumeSegmentIndex = -1;
            long resumePosition = -1;
            if (resumeSegmentInfos != null && resumeSegmentInfos.length == 4) {
                final int numSegments = Integer.parseInt(resumeSegmentInfos[0]);
                resumeSegmentIndex = Integer.parseInt(resumeSegmentInfos[1]);
                resumePosition = Long.parseLong(resumeSegmentInfos[2]);
                final String segment = streamSegments[resumeSegmentIndex];
                boolean resume = numSegments == streamSegments.length;
                resume &= resumeSegmentIndex < streamSegments.length;
                resume &= StringUtils.contains(segment, "sq/" + (resumeSegmentIndex) + "/");
                final File resumeFile = new File(dashDownloadable.getFileOutputPart());
                final long resumeFileLength = resumeFile.length();
                resume &= resumeFileLength >= resumePosition;
                if (resume) {
                    downloadSegments.clear();
                    downloadSegments.addAll(allSegments.subList(resumeSegmentIndex, allSegments.size()));
                } else {
                    resumeSegmentIndex = -1;
                    resumePosition = -1;
                }
            }
            final int finalResumeSegmentIndex = resumeSegmentIndex;
            final long finalResumePosition = resumePosition;
            dashDownloadable.setResumeable(true);
            dl = new SegmentDownloader(this, dashLink, dashDownloadable, br, downloadSegments) {
                @Override
                protected Request createSegmentRequest(Segment seg) throws IOException {
                    final Request ret = super.createSegmentRequest(seg);
                    ret.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://www.youtube.com"));
                    ret.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_REFERER, "https://www.youtube.com/"));
                    ret.getHeaders().put(new HTTPHeader("Connection", "close"));
                    return ret;
                }

                @Override
                public boolean isResumedDownload() {
                    return segments.size() < allSegments.size();
                }

                @Override
                protected long onSegmentStart(RandomAccessFile outputStream, Segment segment, URLConnectionAdapter con) throws IOException {
                    final int segmentIndex = allSegments.indexOf(segment);
                    if (finalResumeSegmentIndex > 0 && finalResumeSegmentIndex == segmentIndex) {
                        outputStream.seek(finalResumePosition);
                        bytesWritten = finalResumePosition;
                    }
                    final long ret = bytesWritten;
                    outputStream.getChannel().force(true);
                    if (segmentIndex > 0) {
                        downloadLink.setProperty(dashChunksProperty + "_segment", streamSegments.length + "_" + segmentIndex + "_" + ret + "_" + streamSegments[segmentIndex]);
                    }
                    dashDownloadable.setChunksProgress(new long[] { ret });
                    return ret;
                }

                @Override
                protected boolean retrySegmentConnection(Browser br, Segment segment, int retryCounter) throws InterruptedException, PluginException {
                    final boolean ret = super.retrySegmentConnection(br, segment, retryCounter);
                    if (ret) {
                        YoutubeDashV2.this.sleep(2000, downloadLink);
                    }
                    return ret;
                }
            };
        } else {
            final int maxChunkSize = 1024 * 1024 * 5;// large(r?) requests cause connection to be throttled
            if (streamData.getContentLength() > 0 && streamData.getContentLength() > maxChunkSize) {
                final List<Segment> segments = new ArrayList<Segment>();
                long position = 0;
                long[] chunkProgress = dashDownloadable.getChunksProgress();
                if (chunkProgress == null || chunkProgress.length != 1 || chunkProgress[0] > streamData.getContentLength()) {
                    chunkProgress = null;
                } else {
                    chunkProgress[0] = Math.min(new File(dashDownloadable.getFileOutputPart()).length(), chunkProgress[0]);
                }
                final Random rnd = new Random();
                while (true) {
                    final long remaining = streamData.getContentLength() - position;
                    if (remaining == 0) {
                        break;
                    }
                    final int rndFactor = rnd.nextInt(100 - 90 + 1) + 90;
                    final int chunkLength = (int) Math.min(remaining, (long) (rndFactor / 100.0d * maxChunkSize));
                    final long chunk_to = position + chunkLength - 1;
                    if (chunkProgress == null || chunkProgress[0] < chunk_to) {
                        final Segment segment;
                        if (false) {
                            // range request via header
                            segment = new Segment(streamData.getBaseUrl(), position, chunk_to);
                        } else {
                            // range request via query param
                            String url = streamData.getBaseUrl();
                            if (url.matches("(?i)(\\?|&)range=\\d+")) {
                                url = url.replaceFirst("(range=\\d+-\\d+)", "range=" + position + "-" + chunk_to);
                            } else {
                                url = url + "&range=" + position + "-" + chunk_to;
                            }
                            segment = new Segment(url);
                        }
                        segments.add(segment);
                    }
                    position += chunkLength;
                }
                if (segments.size() == 0) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                dashDownloadable.setResumeable(true);
                dl = new SegmentDownloader(this, dashLink, dashDownloadable, br, segments) {
                    @Override
                    protected Request createSegmentRequest(Segment seg) throws IOException {
                        final Request ret = super.createSegmentRequest(seg);
                        ret.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://www.youtube.com"));
                        ret.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_REFERER, "https://www.youtube.com/"));
                        if (true) {
                            ret.getHeaders().put(new HTTPHeader("Connection", "close"));
                            return ret;
                        } else {
                            jd.http.requests.PostRequest pret = new jd.http.requests.PostRequest(ret);
                            pret.setPostBytes(new byte[] { 120, 0 });
                            return pret;
                        }
                    }

                    @Override
                    public boolean isResumedDownload() {
                        final Segment firstSegment = segments.get(0);
                        final ChunkRange firstChunkRange = firstSegment.getChunkRange();
                        boolean ret = firstChunkRange.isRangeRequested() && firstChunkRange.getFrom() > 0;
                        if (!ret) {
                            final String rangeStart = new Regex(firstSegment.getUrl(), "range=(\\d+)").getMatch(0);
                            if (rangeStart != null && Long.parseLong(rangeStart) > 0) {
                                ret = true;
                            }
                        }
                        return ret;
                    }

                    @Override
                    protected long onSegmentStart(RandomAccessFile outputStream, Segment segment, URLConnectionAdapter con) throws IOException {
                        final long position;
                        if (segment.getChunkRange() != null && segment.getChunkRange().getFrom() > 0) {
                            position = super.onSegmentStart(outputStream, segment, con);
                        } else {
                            final String rangeStart = new Regex(segment.getUrl(), "range=(\\d+)").getMatch(0);
                            position = Long.parseLong(rangeStart);
                            outputStream.seek(position);
                            bytesWritten = position;
                        }
                        outputStream.getChannel().force(true);
                        dashDownloadable.setChunksProgress(new long[] { position });
                        return position;
                    }

                    @Override
                    protected boolean isSegmentConnectionValid(Segment segment, URLConnectionAdapter con) throws IOException, PluginException {
                        final boolean ret = super.isSegmentConnectionValid(segment, con);
                        if (ret) {
                            final String query = con.getURL().getQuery();
                            if (query.contains("range=")) {
                                final long from = Long.parseLong(new Regex(query, "range=(\\d+)").getMatch(0));
                                final long to = Long.parseLong(new Regex(query, "range=\\d+-(\\d+)").getMatch(0));
                                final long length = to - from + 1;
                                if (con.getContentLength() > 0 && length != con.getContentLength()) {
                                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                                }
                            } else if (con.getCompleteContentLength() > 0 && streamData.getContentLength() != con.getCompleteContentLength()) {
                                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                            }
                        }
                        return ret;
                    }

                    @Override
                    protected boolean retrySegmentConnection(Browser br, Segment segment, int retryCounter) throws InterruptedException, PluginException {
                        final boolean ret = super.retrySegmentConnection(br, segment, retryCounter);
                        if (ret) {
                            YoutubeDashV2.this.sleep(2000, downloadLink);
                        }
                        return ret;
                    }
                };
            } else {
                final GetRequest request = new GetRequest(streamData.getBaseUrl());
                final List<HTTPProxy> possibleProxies = br.getProxy().getProxiesByURL(request.getURL());
                request.setProxy((possibleProxies == null || possibleProxies.size() == 0) ? null : possibleProxies.get(0));
                br.getHeaders().put(new HTTPHeader("Connection", "close", false));
                dashLink.setProperty(DirectHTTP.PROPERTY_ServerComaptibleForByteRangeRequest, true);
                dl = BrowserAdapter.openDownload(br, dashDownloadable, request, true, getChunksPerStream(youtubeConfig));
                if (!this.dl.getConnection().isContentDisposition() && !this.dl.getConnection().getContentType().startsWith("video") && !this.dl.getConnection().getContentType().startsWith("audio") && !this.dl.getConnection().getContentType().startsWith("application")) {
                    br.followConnection(true);
                    if (dl.getConnection().getResponseCode() == 500) {
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, _GUI.T.hoster_servererror("Youtube"), 5 * 60 * 1000l);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
            }
        }
        final boolean ret = dl.startDownload();
        if (dl.externalDownloadStop()) {
            return null;
        } else {
            return ret;
        }
    }

    @Override
    public void handleFree(final DownloadLink downloadLink) throws Exception {
        handlePremium(downloadLink, null);
    }

    private int getChunksPerStream(YoutubeConfig cfg) {
        if (!cfg.isCustomChunkValueEnabled()) {
            return 0;
        }
        int maxChunks = cfg.getChunksCount();
        if (maxChunks <= 0) {
            maxChunks = 0;
        }
        return maxChunks;
    }

    public boolean hasConfig() {
        return true;
    }

    private FFmpegMetaData getFFmpegMetaData(final DownloadLink downloadLink) {
        if (downloadLink != null && PluginJsonConfig.get(YoutubeConfig.class).isMetaDataEnabled()) {
            final FFmpegMetaData ffMpegMetaData = new FFmpegMetaData();
            ffMpegMetaData.setTitle(downloadLink.getStringProperty(YoutubeHelper.YT_TITLE, null));
            ffMpegMetaData.setArtist(downloadLink.getStringProperty(YoutubeHelper.YT_CHANNEL_TITLE, null));
            String contentURL = downloadLink.getContentUrl();
            if (contentURL != null) {
                // TODO: maybe add settings for this
                contentURL = YoutubeHelper.generateSingleVideoContentURL(downloadLink.getStringProperty(YoutubeHelper.YT_ID, null));
                ffMpegMetaData.setComment(contentURL);
            }
            final long timestamp = downloadLink.getLongProperty(YoutubeHelper.YT_DATE, -1);
            if (timestamp > 0) {
                final GregorianCalendar calendar = new GregorianCalendar();
                calendar.setTimeInMillis(timestamp);
                ffMpegMetaData.setYear(calendar);
            }
            final String chapters = downloadLink.getStringProperty(YoutubeHelper.YT_CHAPTERS, null);
            if (chapters != null) {
                try {
                    final Map<String, Object> map = restoreFromString(chapters, TypeRef.MAP);
                    if (map != null && map.get("chapters") != null) {
                        final List<MetaDataEntry> metaDataEntries = new ArrayList<FFmpegMetaData.MetaDataEntry>();
                        Map<String, Object> previousChapter = null;
                        for (final Map<String, Object> currentChapter : (List<Map<String, Object>>) map.get("chapters")) {
                            if (previousChapter == null) {
                                previousChapter = currentChapter;
                            } else {
                                final String title = (String) previousChapter.get("title");
                                final long startTimeStamp = ((Number) previousChapter.get("timeRangeStartMillis")).longValue();
                                final long endTimeStamp = ((Number) currentChapter.get("timeRangeStartMillis")).longValue() - 1;
                                final MetaDataEntry metaEntry = new MetaDataEntry("CHAPTER");
                                metaEntry.put("TIMEBASE", "1/1000");
                                metaEntry.put("START", Long.toString(startTimeStamp));
                                metaEntry.put("END", Long.toString(endTimeStamp));
                                metaEntry.put("title", title);
                                metaDataEntries.add(metaEntry);
                                previousChapter = currentChapter;
                            }
                        }
                        if (previousChapter != null) {
                            final String title = (String) previousChapter.get("title");
                            final long startTimeStamp = ((Number) previousChapter.get("timeRangeStartMillis")).longValue();
                            final long endTimeStamp = downloadLink.getIntegerProperty(YoutubeHelper.YT_DURATION, -1) * 1000l;
                            if (endTimeStamp > startTimeStamp) {
                                final MetaDataEntry metaEntry = new MetaDataEntry("CHAPTER");
                                metaEntry.put("TIMEBASE", "1/1000");
                                metaEntry.put("START", Long.toString(startTimeStamp));
                                metaEntry.put("END", Long.toString(endTimeStamp));
                                metaEntry.put("title", title);
                                metaDataEntries.add(metaEntry);
                            }
                        }
                        for (final MetaDataEntry metaDataEntry : metaDataEntries) {
                            ffMpegMetaData.addEntry(metaDataEntry);
                        }
                    }
                } catch (Exception e) {
                    logger.log(e);
                }
            }
            if (!ffMpegMetaData.isEmpty()) {
                return ffMpegMetaData;
            }
        }
        return null;
    }

    @Override
    public FFmpeg getFFmpeg(final Browser br, final DownloadLink downloadLink) {
        final FFmpegMetaData ffMpegMetaData = getFFmpegMetaData(downloadLink);
        if (ffMpegMetaData != null && !ffMpegMetaData.isEmpty()) {
            return new FFmpeg(br) {
                private final UniqueAlltimeID metaDataProcessID = new UniqueAlltimeID();
                private HttpServer            httpServer        = null;
                private File                  metaFile          = null;

                private final boolean isWriteFileEnabled() {
                    return true;
                }

                @Override
                public LogInterface getLogger() {
                    return YoutubeDashV2.this.getLogger();
                }

                @Override
                protected void parseLine(boolean isStdout, String line) {
                    if (line != null && StringUtils.contains(line, "Input/output error") && StringUtils.contains(line, "/meta")) {
                        PluginJsonConfig.get(YoutubeConfig.class).setMetaDataEnabled(false);
                        if (logger != null) {
                            logger.severe("Firewall/AV blocks JDownloader<->ffmpeg meta data communication. Auto disable meta data support!");
                        }
                    }
                }

                private final HttpServer startHttpServer() {
                    try {
                        final HttpServer httpServer = new HttpServer(0);
                        httpServer.setLocalhostOnly(true);
                        httpServer.registerRequestHandler(new HttpRequestHandler() {
                            @Override
                            public boolean onPostRequest(PostRequest request, HttpResponse response) throws BasicRemoteAPIException {
                                return false;
                            }

                            @Override
                            public boolean onGetRequest(org.appwork.utils.net.httpserver.requests.GetRequest request, HttpResponse response) throws BasicRemoteAPIException {
                                try {
                                    final String id = request.getParameterbyKey("id");
                                    if (id != null && metaDataProcessID.getID() == Long.parseLong(request.getParameterbyKey("id")) && "/meta".equals(request.getRequestedPath())) {
                                        if (logger != null) {
                                            logger.info("Providing ffmpeg meta data");
                                        }
                                        final String content = ffMpegMetaData.getFFmpegMetaData();
                                        final byte[] bytes = content.getBytes("UTF-8");
                                        response.setResponseCode(HTTPConstants.ResponseCode.get(200));
                                        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=utf-8"));
                                        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(bytes.length)));
                                        final OutputStream out = response.getOutputStream(true);
                                        out.write(bytes);
                                        out.flush();
                                        return true;
                                    }
                                } catch (final IOException e) {
                                    if (logger != null) {
                                        logger.log(e);
                                    }
                                }
                                return false;
                            }
                        });
                        httpServer.start();
                        if (logger != null) {
                            logger.info("Opened http server to serve meta on port " + httpServer.getPort());
                        }
                        return httpServer;
                    } catch (final IOException e) {
                        if (logger != null) {
                            logger.log(e);
                        }
                    }
                    return null;
                }

                private File writeMetaFile() {
                    final File ret = Application.getTempResource("ffmpeg_meta_" + UniqueAlltimeID.create());
                    try {
                        IO.writeStringToFile(ret, ffMpegMetaData.getFFmpegMetaData());
                        if (logger != null) {
                            logger.info("Wrote meta to " + ret);
                        }
                        return ret;
                    } catch (final Throwable e) {
                        ret.delete();
                        if (logger != null) {
                            logger.log(e);
                        }
                    }
                    return null;
                }

                private void stopMetaFileProvider() {
                    final File metaFile = this.metaFile;
                    if (metaFile != null) {
                        this.metaFile = null;
                        metaFile.delete();
                    }
                    final HttpServer httpServer = this.httpServer;
                    if (httpServer != null) {
                        this.httpServer = null;
                        httpServer.stop();
                    }
                }

                @Override
                protected boolean demux(FFMpegProgress progress, String out, String audioIn, String[] demuxCommands) throws InterruptedException, IOException, FFMpegException {
                    if (isWriteFileEnabled()) {
                        metaFile = writeMetaFile();
                    } else {
                        httpServer = startHttpServer();
                    }
                    try {
                        if (httpServer != null || metaFile != null) {
                            final ArrayList<String> newDemuxCommands = new ArrayList<String>();
                            boolean metaParamsAdded = false;
                            String lastDemuxCommand = null;
                            for (final String demuxCommand : demuxCommands) {
                                if ("%audio".equals(lastDemuxCommand) && !metaParamsAdded) {
                                    newDemuxCommands.add("-i");
                                    if (httpServer != null) {
                                        newDemuxCommands.add("http://" + httpServer.getServerAddress() + "/meta?id=" + metaDataProcessID.getID());
                                    } else {
                                        final String path = metaFile.getAbsolutePath();
                                        if (CrossSystem.isWindows() && path.length() > 259) {
                                            // https://msdn.microsoft.com/en-us/library/aa365247.aspx
                                            newDemuxCommands.add("\\\\?\\" + path);
                                        } else {
                                            newDemuxCommands.add(path);
                                        }
                                    }
                                    newDemuxCommands.add("-map_metadata");
                                    newDemuxCommands.add("1");
                                    metaParamsAdded = true;
                                }
                                newDemuxCommands.add(demuxCommand);
                                lastDemuxCommand = demuxCommand;
                            }
                            if ("%audio".equals(lastDemuxCommand) && !metaParamsAdded) {
                                newDemuxCommands.add("-i");
                                if (httpServer != null) {
                                    newDemuxCommands.add("http://" + httpServer.getServerAddress() + "/meta?id=" + metaDataProcessID.getID());
                                } else {
                                    final String path = metaFile.getAbsolutePath();
                                    if (CrossSystem.isWindows() && path.length() > 259) {
                                        // https://msdn.microsoft.com/en-us/library/aa365247.aspx
                                        newDemuxCommands.add("\\\\?\\" + path);
                                    } else {
                                        newDemuxCommands.add(path);
                                    }
                                }
                                newDemuxCommands.add("-map_metadata");
                                newDemuxCommands.add("1");
                                metaParamsAdded = true;
                            }
                            return super.demux(progress, out, audioIn, newDemuxCommands.toArray(new String[0]));
                        } else {
                            return super.demux(progress, out, audioIn, demuxCommands);
                        }
                    } finally {
                        stopMetaFileProvider();
                    }
                }

                @Override
                protected boolean mux(FFMpegProgress progress, String out, String videoIn, String audioIn, String[] muxCommands) throws InterruptedException, IOException, FFMpegException {
                    if (isWriteFileEnabled()) {
                        metaFile = writeMetaFile();
                    } else {
                        httpServer = startHttpServer();
                    }
                    try {
                        if (httpServer != null || metaFile != null) {
                            final ArrayList<String> newMuxCommands = new ArrayList<String>();
                            boolean metaParamsAdded = false;
                            for (final String muxCommand : muxCommands) {
                                if ("-map".equals(muxCommand) && !metaParamsAdded) {
                                    newMuxCommands.add("-i");
                                    if (httpServer != null) {
                                        newMuxCommands.add("http://" + httpServer.getServerAddress() + "/meta?id=" + metaDataProcessID.getID());
                                    } else {
                                        final String path = metaFile.getAbsolutePath();
                                        if (CrossSystem.isWindows() && path.length() > 259) {
                                            // https://msdn.microsoft.com/en-us/library/aa365247.aspx
                                            newMuxCommands.add("\\\\?\\" + path);
                                        } else {
                                            newMuxCommands.add(path);
                                        }
                                    }
                                    newMuxCommands.add("-map_metadata");
                                    newMuxCommands.add("2");
                                    metaParamsAdded = true;
                                }
                                newMuxCommands.add(muxCommand);
                            }
                            return super.mux(progress, out, videoIn, audioIn, newMuxCommands.toArray(new String[0]));
                        } else {
                            return super.mux(progress, out, videoIn, audioIn, muxCommands);
                        }
                    } finally {
                        stopMetaFileProvider();
                    }
                }
            };
        } else {
            return super.getFFmpeg(br, downloadLink);
        }
    }

    public void handleDash(final DownloadLink downloadLink, final YoutubeProperties data, Account account) throws Exception {
        DownloadLinkView oldView = null;
        DefaultDownloadLinkViewImpl newView = null;
        try {
            final AbstractVariant variant = getVariant(downloadLink);
            final boolean isDashAudioOnly = variant.getType() == DownloadType.DASH_AUDIO;
            newView = new DefaultDownloadLinkViewImpl() {
                @Override
                public long getBytesLoaded() {
                    final SingleDownloadController dlc = link.getDownloadLinkController();
                    if (dlc.getDownloadInstance() instanceof SegmentDownloader) {
                        if (isDashAudioOnly) {
                            final long audio = data.getDashAudioBytesLoaded();
                            return audio;
                        } else {
                            final long video = data.getDashVideoBytesLoaded();
                            if (!data.isDashVideoFinished()) {
                                return video;
                            } else {
                                final long audio = data.getDashAudioBytesLoaded();
                                return video + audio;
                            }
                        }
                    } else {
                        return super.getBytesLoaded();
                    }
                }
            };
            oldView = downloadLink.setView(newView);
            // debug
            requestFileInformation(downloadLink, true);
            final SingleDownloadController dlc = downloadLink.getDownloadLinkController();
            final List<File> locks = new ArrayList<File>();
            HttpServer httpServer = null;
            try {
                new DownloadLinkDownloadable(downloadLink).checkIfWeCanWrite(new ExceptionRunnable() {
                    @Override
                    public void run() throws Exception {
                        locks.addAll(listProcessFiles(downloadLink));
                        try {
                            for (final File lock : locks) {
                                boolean ret = false;
                                try {
                                    dlc.lockFile(lock);
                                    ret = true;
                                } catch (FileIsLockedException e) {
                                    throw e;
                                } finally {
                                    logger.info("Lock:" + lock + "=" + ret);
                                }
                            }
                        } catch (FileIsLockedException e) {
                            for (final File lock : locks) {
                                dlc.unlockFile(lock);
                            }
                            throw e;
                        }
                    }
                }, null);
                final String videoStreamPath = getVideoStreamPath(downloadLink);
                final File videoStreamFile = videoStreamPath != null ? new File(videoStreamPath) : null;
                final Boolean videoStreamLoaded;
                if (videoStreamFile != null) {
                    if (videoStreamFile.isFile() && videoStreamFile.length() > 0) {
                        data.setDashVideoFinished(true);
                        videoStreamLoaded = true;
                    } else {
                        videoStreamLoaded = false;
                    }
                } else {
                    videoStreamLoaded = null;
                }
                boolean loadVideo = !data.isDashVideoFinished();
                if (videoStreamPath == null || isDashAudioOnly) {
                    /* Skip video if just audio should be downloaded */
                    loadVideo = false;
                } else {
                    loadVideo |= Boolean.FALSE.equals(videoStreamLoaded);
                    if (loadVideo) {
                        data.setDashVideoFinished(false);
                    }
                }
                final String audioStreamPath = getAudioStreamPath(downloadLink);
                final File audioStreamFile = audioStreamPath != null ? new File(audioStreamPath) : null;
                final Boolean audioStreamLoaded;
                if (audioStreamFile != null) {
                    if (audioStreamFile.isFile() && audioStreamFile.length() > 0) {
                        audioStreamLoaded = true;
                        data.setDashAudioFinished(true);
                    } else {
                        audioStreamLoaded = false;
                    }
                } else {
                    audioStreamLoaded = null;
                }
                boolean loadAudio = !data.isDashAudioFinished();
                loadAudio |= Boolean.FALSE.equals(audioStreamLoaded);
                if (loadAudio) {
                    data.setDashAudioFinished(false);
                }
                if (loadVideo) {
                    /* videoStream not finished yet, resume/download it */
                    final Boolean ret = downloadDashStream(downloadLink, data, true);
                    if (ret == null) {
                        return;
                    } else if (!ret) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
                if (loadAudio) {
                    /* audioStream not finished yet, resume/download it */
                    final Boolean ret = downloadDashStream(downloadLink, data, false);
                    if (ret == null) {
                        return;
                    } else if (!ret) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
                logger.info("LoadVideo:" + loadVideo + "|File:" + videoStreamFile + "|Size:" + (videoStreamFile != null ? videoStreamFile.length() : "~"));
                logger.info("LoadAudio:" + loadAudio + "|File:" + audioStreamFile + "|Size:" + (audioStreamFile != null ? audioStreamFile.length() : "~"));
                if (audioStreamFile != null && audioStreamFile.isFile() && !new File(downloadLink.getFileOutput()).exists()) {
                    downloadLink.setAvailable(true);
                    if (!PluginJsonConfig.get(YoutubeConfig.class).isDASHMuxingEnabled()) {
                        boolean finishedFlag = false;
                        if (videoStreamFile != null && videoStreamFile.isFile() && videoStreamFile.length() > 0) {
                            final FileContainer extension = FileContainer.getVideoContainer(variant.getiTagVideo(), variant.getiTagAudioOrVideoItagEquivalent());
                            final File dest = new File(downloadLink.getFileOutput() + "." + extension.getExtension());
                            final boolean renameResult = videoStreamFile.renameTo(dest);
                            logger.info("Rename:" + videoStreamFile + "->" + dest + "|" + renameResult);
                            if (!renameResult) {
                                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                            } else {
                                finishedFlag = true;
                            }
                        }
                        if (audioStreamFile != null && audioStreamFile.isFile() && audioStreamFile.length() > 0) {
                            final FileContainer extension = FileContainer.getAudioContainer(variant.getiTagVideo(), variant.getiTagAudioOrVideoItagEquivalent());
                            final File dest = new File(downloadLink.getFileOutput() + "." + extension.getExtension());
                            final boolean renameResult = audioStreamFile.renameTo(dest);
                            logger.info("Rename:" + audioStreamFile + "->" + dest + "|" + renameResult);
                            if (!renameResult) {
                                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                            } else {
                                finishedFlag = true;
                            }
                        }
                        if (finishedFlag) {
                            downloadLink.getLinkStatus().setStatus(LinkStatus.FINISHED);
                            return;
                        } else {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        }
                    } else {
                        final FFmpeg ffmpeg = getFFmpeg(br.cloneBrowser(), downloadLink);
                        /* audioStream also finished */
                        /* Do we need an exception here? If a Video is downloaded it is always finished before the audio part. TheCrap */
                        if (videoStreamFile != null && videoStreamFile.isFile()) {
                            boolean deleteFlag = false;
                            final FFMpegProgress progress = new FFMpegProgress();
                            try {
                                progress.setProgressSource(this);
                                downloadLink.addPluginProgress(progress);
                                switch (variant.getContainer()) {
                                case WEBM:
                                    if (ffmpeg.muxToWebm(progress, downloadLink.getFileOutput(), videoStreamPath, audioStreamPath)) {
                                        downloadLink.getLinkStatus().setStatus(LinkStatus.FINISHED);
                                        deleteFlag = true;
                                    } else {
                                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, _GUI.T.YoutubeDash_handleFree_error_());
                                    }
                                    break;
                                case MKV:
                                    if (ffmpeg.muxToMkv(progress, downloadLink.getFileOutput(), videoStreamPath, audioStreamPath)) {
                                        downloadLink.getLinkStatus().setStatus(LinkStatus.FINISHED);
                                        deleteFlag = true;
                                    } else {
                                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, _GUI.T.YoutubeDash_handleFree_error_());
                                    }
                                    break;
                                case MP4:
                                default:
                                    if (ffmpeg.muxToMp4(progress, downloadLink.getFileOutput(), videoStreamPath, audioStreamPath)) {
                                        downloadLink.getLinkStatus().setStatus(LinkStatus.FINISHED);
                                        deleteFlag = true;
                                    } else {
                                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, _GUI.T.YoutubeDash_handleFree_error_());
                                    }
                                }
                            } catch (FFMpegException e) {
                                if (FFMpegException.ERROR.DISK_FULL.equals(e.getError())) {
                                    final File incomplete = new File(downloadLink.getFileOutput());
                                    if (incomplete.isFile()) {
                                        incomplete.delete();
                                    }
                                    throw new SkipReasonException(SkipReason.DISK_FULL, e);
                                } else {
                                    throw e;
                                }
                            } finally {
                                downloadLink.removePluginProgress(progress);
                                if (deleteFlag) {
                                    if (videoStreamFile != null) {
                                        videoStreamFile.delete();
                                    }
                                    if (audioStreamFile != null) {
                                        audioStreamFile.delete();
                                    }
                                }
                            }
                        } else {
                            boolean deleteFlag = false;
                            final FFMpegProgress progress = new FFMpegProgress();
                            try {
                                progress.setProgressSource(this);
                                downloadLink.addPluginProgress(progress);
                                switch (variant.getContainer()) {
                                case AAC:
                                    if (ffmpeg.generateAac(progress, downloadLink.getFileOutput(), audioStreamPath)) {
                                        downloadLink.getLinkStatus().setStatus(LinkStatus.FINISHED);
                                        deleteFlag = true;
                                    } else {
                                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, _GUI.T.YoutubeDash_handleFree_error_() + ":" + variant.getContainer());
                                    }
                                    break;
                                case M4A:
                                    if (ffmpeg.generateM4a(progress, downloadLink.getFileOutput(), audioStreamPath)) {
                                        downloadLink.getLinkStatus().setStatus(LinkStatus.FINISHED);
                                        deleteFlag = true;
                                    } else {
                                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, _GUI.T.YoutubeDash_handleFree_error_() + ":" + variant.getContainer());
                                    }
                                    break;
                                case OGG:
                                    if (ffmpeg.generateOggAudio(progress, downloadLink.getFileOutput(), audioStreamPath)) {
                                        downloadLink.getLinkStatus().setStatus(LinkStatus.FINISHED);
                                        deleteFlag = true;
                                    } else {
                                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, _GUI.T.YoutubeDash_handleFree_error_() + ":" + variant.getContainer());
                                    }
                                    break;
                                case OPUS:
                                    if (ffmpeg.generateOpusAudio(progress, downloadLink.getFileOutput(), audioStreamPath)) {
                                        downloadLink.getLinkStatus().setStatus(LinkStatus.FINISHED);
                                        deleteFlag = true;
                                    } else {
                                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, _GUI.T.YoutubeDash_handleFree_error_() + ":" + variant.getContainer());
                                    }
                                    break;
                                case MKV:
                                    if (ffmpeg.generateMkvAudio(progress, downloadLink.getFileOutput(), audioStreamPath)) {
                                        downloadLink.getLinkStatus().setStatus(LinkStatus.FINISHED);
                                        deleteFlag = true;
                                    } else {
                                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, _GUI.T.YoutubeDash_handleFree_error_() + ":" + variant.getContainer());
                                    }
                                    break;
                                default:
                                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, _GUI.T.YoutubeDash_handleFree_error_() + ":" + variant.getContainer());
                                }
                            } catch (FFMpegException e) {
                                if (FFMpegException.ERROR.DISK_FULL.equals(e.getError())) {
                                    final File incomplete = new File(downloadLink.getFileOutput());
                                    if (incomplete.isFile()) {
                                        incomplete.delete();
                                    }
                                    throw new SkipReasonException(SkipReason.DISK_FULL, e);
                                } else {
                                    throw e;
                                }
                            } finally {
                                downloadLink.removePluginProgress(progress);
                                if (deleteFlag) {
                                    if (audioStreamFile != null) {
                                        audioStreamFile.delete();
                                    }
                                }
                            }
                        }
                    }
                }
            } catch (final FileIsLockedException e) {
                logger.log(e);
                throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS, null, e);
            } finally {
                if (httpServer != null) {
                    httpServer.stop();
                }
                for (final File lock : locks) {
                    dlc.unlockFile(lock);
                }
            }
        } finally {
            if (oldView != null) {
                downloadLink.setView(oldView);
            }
        }
    }

    private void avoidRetryLoop(PluginException pluginException, final DownloadLink downloadLink, AbstractVariant variant) throws Exception {
        if (!isAbort() && (pluginException.getLinkStatus() == LinkStatus.ERROR_DOWNLOAD_INCOMPLETE || pluginException.getLinkStatus() == LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE)) {
            final String key;
            final int maxRetry;
            switch (variant.getType()) {
            case VIDEO:
                key = "incomplete_" + variant.getiTagAudioOrVideoItagEquivalent().getITAG();
                maxRetry = DebugMode.TRUE_IN_IDE_ELSE_FALSE ? 1 : 5;
                break;
            default:
                key = "incomplete_" + variant.getType();
                maxRetry = DebugMode.TRUE_IN_IDE_ELSE_FALSE ? 1 : 5;
                break;
            }
            final int incomplete = downloadLink.getIntegerProperty(key, 0) + 1;
            downloadLink.setProperty(key, incomplete);
            if (incomplete > maxRetry) {
                if (variant.getType().equals(DownloadType.VIDEO)) {
                    final HashSet<LinkVariant> checkedAlternatives = new HashSet<LinkVariant>();
                    checkedAlternatives.add(variant);
                    final YoutubeHelper helper = new YoutubeHelper(br.cloneBrowser(), logger);
                    LinkVariant alternative = getAlternatives(helper, downloadLink, variant, checkedAlternatives);
                    while (alternative != null) {
                        if (alternative instanceof AbstractVariant && downloadLink.hasProperty("incomplete_" + ((AbstractVariant) alternative).getiTagAudioOrVideoItagEquivalent().getITAG())) {
                            logger.info("Variant:" + variant + " failed too often! Skip alternative variant:" + alternative);
                            checkedAlternatives.add(alternative);
                            alternative = getAlternatives(helper, downloadLink, variant, checkedAlternatives);
                        } else {
                            logger.info("Variant:" + variant + " failed too often! Auto try alternative variant:" + alternative);
                            final LinkVariant finalAlternative = alternative;
                            downloadLink.getDownloadLinkController().getJobsAfterDetach().add(new DownloadWatchDogJob() {
                                @Override
                                public void interrupt() {
                                }

                                @Override
                                public void execute(DownloadSession currentSession) {
                                    /* now we can reset the link */
                                    DownloadWatchDog.getInstance().reset(Arrays.asList(new DownloadLink[] { downloadLink }));
                                }

                                @Override
                                public boolean isHighPriority() {
                                    return false;
                                }
                            });
                            downloadLink.getDownloadLinkController().getJobsAfterDetach().add(new DownloadWatchDogJob() {
                                @Override
                                public void interrupt() {
                                }

                                @Override
                                public void execute(DownloadSession currentSession) {
                                    /* now we can reset the link */
                                    setActiveVariantByLink(downloadLink, finalAlternative);
                                }

                                @Override
                                public boolean isHighPriority() {
                                    return false;
                                }
                            });
                            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, _GUI.T.hoster_servererror("Youtube"), 1 * 60 * 1000l, pluginException);
                        }
                    }
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, null, pluginException);
            }
        }
        throw pluginException;
    }

    @Override
    public void handlePremium(final DownloadLink downloadLink, Account account) throws Exception {
        final YoutubeProperties data = downloadLink.bindData(YoutubeProperties.class);
        final YoutubeHelper helper = new YoutubeHelper(br, getLogger());
        AbstractVariant variant = getVariant(downloadLink);
        if (account != null) {
            helper.login(account, false);
        }
        // if (!Application.isJared(null)) throw new RuntimeException("Shit happened");
        boolean resume = true;
        switch (variant.getType()) {
        case DESCRIPTION:
            downloadDescription(downloadLink);
            return;
        case IMAGE:
            this.setBrowserExclusive();
            this.requestFileInformation(downloadLink, true);
            this.dl = jd.plugins.BrowserAdapter.openDownload(this.br, downloadLink, getAndUpdateVariantInfo(downloadLink, true).getDataStreams().get(0).getUrl(), resume, 1);
            if (!this.dl.getConnection().isContentDisposition() && !this.dl.getConnection().getContentType().startsWith("image/")) {
                try {
                    br.followConnection(true);
                } catch (final IOException e) {
                    logger.log(e);
                }
                final PluginException e;
                if (dl.getConnection().getResponseCode() == 500) {
                    e = new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, _GUI.T.hoster_servererror("Youtube"), 5 * 60 * 1000l);
                } else {
                    e = new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, _GUI.T.hoster_servererror("Youtube"), 1 * 60 * 1000l);
                }
                avoidRetryLoop(e, downloadLink, variant);
            }
            try {
                if (!this.dl.startDownload()) {
                    throw new PluginException(LinkStatus.ERROR_RETRY);
                }
            } catch (final PluginException e) {
                avoidRetryLoop(e, downloadLink, variant);
            }
            break;
        case SUBTITLES:
            this.setBrowserExclusive();
            this.requestFileInformation(downloadLink, true);
            this.dl = jd.plugins.BrowserAdapter.openDownload(this.br, downloadLink, getAndUpdateVariantInfo(downloadLink, true).getDataStreams().get(0).getUrl(), resume, 1);
            if (!this.dl.getConnection().isContentDisposition() && !this.dl.getConnection().getContentType().startsWith("text/xml")) {
                try {
                    br.followConnection(true);
                } catch (final IOException e) {
                    logger.log(e);
                }
                final PluginException e;
                if (dl.getConnection().getResponseCode() == 500) {
                    e = new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, _GUI.T.hoster_servererror("Youtube"), 5 * 60 * 1000l);
                } else {
                    e = new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, _GUI.T.hoster_servererror("Youtube"), 1 * 60 * 1000l);
                }
                avoidRetryLoop(e, downloadLink, variant);
            }
            try {
                if (!this.dl.startDownload()) {
                    throw new PluginException(LinkStatus.ERROR_RETRY);
                }
            } catch (final PluginException e) {
                avoidRetryLoop(e, downloadLink, variant);
            }
            break;
        case VIDEO:
            if (variant.getBaseVariant().name().contains("DEMUX") || variant.getBaseVariant().name().contains("MP3")) {
                checkFFmpeg(downloadLink, _GUI.T.YoutubeDash_handleDownload_youtube_demux());
            }
            this.setBrowserExclusive();
            //
            this.requestFileInformation(downloadLink, true);
            // downloadLink.setInternalTmpFilenameAppend(fileName);
            final YoutubeFinalLinkResource sd = getYoutubeFinalLinkResource(downloadLink, YoutubeHelper.YT_STREAM_DATA_VIDEO);
            br.getHeaders().put(new HTTPHeader("Connection", "close", false));
            downloadLink.setProperty(DirectHTTP.PROPERTY_ServerComaptibleForByteRangeRequest, true);
            this.dl = jd.plugins.BrowserAdapter.openDownload(this.br, downloadLink, sd.getBaseUrl(), resume, getChunksPerStream(PluginJsonConfig.get(YoutubeConfig.class)));
            if (!this.dl.getConnection().isContentDisposition() && !this.dl.getConnection().getContentType().startsWith("video") && !this.dl.getConnection().getContentType().startsWith("application")) {
                try {
                    br.followConnection(true);
                } catch (final IOException e) {
                    logger.log(e);
                }
                final PluginException e;
                if (dl.getConnection().getResponseCode() == 500) {
                    e = new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, _GUI.T.hoster_servererror("Youtube"), 5 * 60 * 1000l);
                } else {
                    e = new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, _GUI.T.hoster_servererror("Youtube"), 1 * 60 * 1000l);
                }
                avoidRetryLoop(e, downloadLink, variant);
            }
            try {
                if (!this.dl.startDownload()) {
                    throw new PluginException(LinkStatus.ERROR_RETRY);
                }
            } catch (final PluginException e) {
                avoidRetryLoop(e, downloadLink, variant);
            }
            break;
        case HLS_VIDEO:
            checkFFmpeg(downloadLink, "HLS Download");
            dl = new HLSDownloader(downloadLink, br, getAndUpdateVariantInfo(downloadLink, true).getVideoStreams().get(0).getUrl()) {
                @Override
                protected boolean isMapMetaDataEnabled() {
                    return PluginJsonConfig.get(YoutubeConfig.class).isMetaDataEnabled();
                }

                @Override
                protected FFmpegMetaData getFFmpegMetaData() {
                    return YoutubeDashV2.this.getFFmpegMetaData(downloadLink);
                }
            };
            ((HLSDownloader) dl).setAcceptDownloadStopAsValidEnd(true);
            if (!this.dl.startDownload()) {
                throw new PluginException(LinkStatus.ERROR_RETRY);
            }
            break;
        case DASH_AUDIO:
        case DASH_VIDEO:
            checkFFmpeg(downloadLink, _GUI.T.YoutubeDash_handleDownload_youtube_dash());
            handleDash(downloadLink, data, null);
            break;
        }
        if (variant.hasConverter(downloadLink)) {
            long lastMod = new File(downloadLink.getFileOutput()).lastModified();
            variant.convert(downloadLink, this);
            try {
                if (lastMod > 0 && JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified()) {
                    new File(downloadLink.getFileOutput()).setLastModified(lastMod);
                }
            } catch (final Throwable e) {
                LogSource.exception(logger, e);
            }
        }
        // PostProcessing
        switch (variant.getType()) {
        case SUBTITLES:
            // rename subtitles to match the videos.
            // this code
            final YoutubeConfig.SubtitleVariantMode subtitleVariantMode = CFG_YOUTUBE.CFG.getSubtitleVariantMode();
            if (subtitleVariantMode != null && !YoutubeConfig.SubtitleVariantMode.DISABLED.equals(subtitleVariantMode)) {
                final FilePackage pkg = downloadLink.getParentNode();
                final boolean readL2 = pkg.getModifyLock().readLock();
                final File source = new File(downloadLink.getFileOutput(false, false));
                try {
                    logger.info("PostProcessing for Subtitle:" + subtitleVariantMode + "|exists:" + source.isFile());
                    boolean successfulFlag = false;
                    boolean keepOriginal = false;
                    if (source.isFile()) {
                        final String myID = downloadLink.getStringProperty(YoutubeHelper.YT_ID, null);
                        for (DownloadLink child : pkg.getChildren()) {
                            try {
                                String fileName = child.getForcedFileName();
                                if (fileName == null) {
                                    fileName = child.getFinalFileName();
                                }
                                if (myID.equals(child.getStringProperty(YoutubeHelper.YT_ID, null))) {
                                    final AbstractVariant v = getVariant(child);
                                    switch (v.getGroup()) {
                                    case VIDEO:
                                        final String ext = Files.getExtension(fileName);
                                        if (StringUtils.isNotEmpty(ext)) {
                                            final String base = fileName.substring(0, fileName.length() - ext.length() - 1);
                                            final String displayLanguage;
                                            if (variant instanceof SubtitleVariant) {
                                                displayLanguage = ((SubtitleVariant) variant).getDisplayLanguage();
                                            } else {
                                                final Locale locale = new SubtitleVariantOld(downloadLink.getStringProperty(YoutubeHelper.YT_SUBTITLE_CODE, ""))._getLocale();
                                                displayLanguage = locale.getDisplayLanguage();
                                            }
                                            final File dest;
                                            if (StringUtils.isEmpty(displayLanguage)) {
                                                dest = new File(source.getParentFile(), base + ".srt");
                                            } else {
                                                dest = new File(source.getParentFile(), base + "." + displayLanguage + ".srt");
                                            }
                                            if (dest.equals(source)) {
                                                logger.info("PostProcessing for Subtitle:" + subtitleVariantMode + "|KeepOriginal source:'" + source + "' because equals dest:" + dest);
                                                keepOriginal = true;
                                            } else {
                                                logger.info("PostProcessing for Subtitle:" + subtitleVariantMode + "|Copy source:'" + source + "' to dest:" + dest);
                                                IO.copyFile(source, dest);
                                                successfulFlag = true;
                                            }
                                            try {
                                                if (JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified()) {
                                                    dest.setLastModified(source.lastModified());
                                                }
                                            } catch (final Throwable e) {
                                                getLogger().log(e);
                                            }
                                        }
                                        break;
                                    default:
                                        break;
                                    }
                                }
                            } catch (Exception e) {
                                getLogger().log(e);
                            }
                        }
                    }
                    if (!keepOriginal && successfulFlag && YoutubeConfig.SubtitleVariantMode.COPY_AND_DELETE.equals(subtitleVariantMode)) {
                        logger.info("PostProcessing for Subtitle:" + subtitleVariantMode + "|Delete source:" + source);
                        source.delete();
                    }
                } finally {
                    pkg.getModifyLock().readUnlock(readL2);
                }
            }
            break;
        }
    }

    private void downloadDescription(DownloadLink downloadLink) throws Exception {
        if (requestFileInformation(downloadLink) != AvailableStatus.TRUE) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String descriptionText = downloadLink.getTempProperties().getStringProperty(YoutubeHelper.YT_DESCRIPTION, null);
        if (descriptionText == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = new TextDownloader(this, downloadLink, descriptionText);
        dl.startDownload();
    }

    @Override
    public boolean isProxyRotationEnabledForLinkChecker() {
        return super.isProxyRotationEnabledForLinkChecker();
    }

    @Override
    public void resetDownloadlink(DownloadLink downloadLink) {
        if (downloadLink != null) {
            resetStreamUrls(downloadLink);
            final YoutubeProperties data = downloadLink.bindData(YoutubeProperties.class);
            downloadLink.getTempProperties().removeProperty("ratebypass");
            data.setDashAudioBytesLoaded(0);
            data.setDashAudioFinished(false);
            data.setDashAudioITag(-1);
            data.setDashVideoBytesLoaded(0);
            data.setDashVideoFinished(false);
            data.setDashVideoITag(-1);
            downloadLink.removeProperty(DASH_VIDEO_CHUNKS);
            downloadLink.removeProperty(DASH_AUDIO_CHUNKS);
            for (final Entry<String, Object> entry : downloadLink.getProperties().entrySet()) {
                if (StringUtils.startsWithCaseInsensitive(entry.getKey(), "incomplete_")) {
                    downloadLink.removeProperty(entry.getKey());
                }
            }
            ClipDataCache.clearCache(downloadLink);
        }
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public String getDescription() {
        return "JDownloader's YouTube Plugin helps downloading videos from youtube.com. YouTube provides different audio/video formats and qualities. JDownloader is also able to extract the audio track after download, and save it as a separate file.";
    }

    protected FilePair[] listFilePairsToMove(DownloadLink link, String currentDirectory, String currentName, String newDirectory, String newName) {
        List<FilePair> ret = new ArrayList<PluginForHost.FilePair>();
        ret.add(new FilePair(new File(new File(currentDirectory), currentName + ".part"), new File(new File(newDirectory), newName + ".part")));
        ret.add(new FilePair(new File(new File(currentDirectory), currentName), new File(new File(newDirectory), newName)));
        try {
            AbstractVariant<?> variant = getVariant(link);
            if (variant != null) {
                for (File f : variant.listProcessFiles(link)) {
                    FilePair fp = new FilePair(new File(new File(currentDirectory), f.getName()), new File(new File(newDirectory), newName + f.getName().substring(currentName.length())));
                    ret.add(fp);
                    fp = new FilePair(new File(new File(currentDirectory), f.getName() + ".part"), new File(new File(newDirectory), newName + f.getName().substring(currentName.length()) + ".part"));
                    ret.add(fp);
                }
                switch (variant.getType()) {
                case DASH_AUDIO:
                case DASH_VIDEO:
                    String vs = getVideoStreamPath(link);
                    String as = getAudioStreamPath(link);
                    if (StringUtils.isNotEmpty(vs)) {
                        // aac only does not have video streams
                        // ret.add(new File(vs));
                        // ret.add(new File(vs + ".part"));
                        ret.add(new FilePair(new File(new File(currentDirectory), new File(vs).getName() + ".part"), new File(new File(newDirectory), new File(vs).getName() + ".part")));
                        ret.add(new FilePair(new File(new File(currentDirectory), new File(vs).getName()), new File(new File(newDirectory), new File(vs).getName())));
                    }
                    if (StringUtils.isNotEmpty(as)) {
                        ret.add(new FilePair(new File(new File(currentDirectory), new File(as).getName() + ".part"), new File(new File(newDirectory), new File(as).getName() + ".part")));
                        ret.add(new FilePair(new File(new File(currentDirectory), new File(as).getName()), new File(new File(newDirectory), new File(as).getName())));
                    }
                    break;
                default:
                }
            }
        } catch (PluginException e) {
            e.printStackTrace();
        }
        return ret.toArray(new FilePair[] {});
    }

    @Override
    public List<File> listProcessFiles(DownloadLink link) {
        final List<File> ret = super.listProcessFiles(link);
        try {
            final AbstractVariant variant = getVariant(link);
            if (variant == null) {
                return ret;
            }
            ret.addAll(variant.listProcessFiles(link));
            switch (variant.getType()) {
            case DASH_AUDIO:
            case DASH_VIDEO:
                final String vs = getVideoStreamPath(link);
                final String as = getAudioStreamPath(link);
                if (StringUtils.isNotEmpty(vs)) {
                    // aac only does not have video streams
                    ret.add(new File(vs));
                    ret.add(new File(vs + ".part"));
                }
                if (StringUtils.isNotEmpty(as)) {
                    ret.add(new File(as));
                    ret.add(new File(as + ".part"));
                }
                break;
            default:
            }
        } catch (PluginException e) {
            logger.log(e);
        }
        return ret;
    }

    public String getAudioStreamPath(DownloadLink link) throws PluginException {
        final String audioFilenName = getDashAudioFileName(link);
        if (StringUtils.isEmpty(audioFilenName)) {
            return null;
        } else {
            return new File(link.getDownloadDirectory(), audioFilenName).getAbsolutePath();
        }
    }

    public String getDashAudioFileName(DownloadLink link) throws PluginException {
        final AbstractVariant var = getVariant(link);
        switch (var.getType()) {
        case DASH_AUDIO:
        case DASH_VIDEO:
            // add both - audio and videoid to the path. else we might get conflicts if we download 2 qualities with the same audiostream
            return link.getStringProperty(YoutubeHelper.YT_ID, null) + "_" + var.getBaseVariant().name() + "_" + Hash.getMD5(var._getUniqueId()) + ".dashAudio";
        default:
            logger.info("getDashAudioFileName for '" + var.getType() + "'?");
            return null;
        }
    }

    public String getVideoStreamPath(DownloadLink link) throws PluginException {
        final String videoFileName = getDashVideoFileName(link);
        if (StringUtils.isEmpty(videoFileName)) {
            return null;
        } else {
            return new File(link.getDownloadDirectory(), videoFileName).getAbsolutePath();
        }
    }

    public String getDashVideoFileName(DownloadLink link) throws PluginException {
        final AbstractVariant var = getVariant(link);
        switch (var.getType()) {
        case DASH_AUDIO:
        case DASH_VIDEO:
            // add both - audio and videoid to the path. else we might get conflicts if we download 2 qualities with the same audiostream
            return link.getStringProperty(YoutubeHelper.YT_ID, null) + "_" + var.getBaseVariant().name() + "_" + Hash.getMD5(var._getUniqueId()) + ".dashVideo";
        default:
            logger.info("getDashVideoFileName for '" + var.getType() + "'?");
            return null;
        }
    }

    @Override
    public LinkVariant getActiveVariantByLink(DownloadLink downloadLink) {
        try {
            return getVariant(downloadLink);
        } catch (PluginException e) {
            logger.log(e);
            return null;
        }
    }

    @Override
    public void setActiveVariantByLink(DownloadLink downloadLink, LinkVariant variant) {
        final YoutubeConfig cfg = PluginJsonConfig.get(YoutubeConfig.class);
        if (variant == null) {
            return;
        }
        final AbstractNodeNotifier nodeChangeListener = downloadLink.getNodeChangeListener();
        downloadLink.setContentUrl("https://www.youtube.com" + "/watch?v=" + downloadLink.getStringProperty(YoutubeHelper.YT_ID) + "#variant=" + Encoding.urlEncode(Base64.encode(((AbstractVariant) variant).getStorableString())));
        if (variant instanceof AbstractVariant) {
            AbstractVariant v = (AbstractVariant) variant;
            // reset Streams urls. we need new ones
            resetStreamUrls(downloadLink);
            downloadLink.setDownloadSize(-1);
            downloadLink.setVerifiedFileSize(-1);
            if (downloadLink.hasProperty(LinkCollector.SOURCE_VARIANT_ID)) {
                downloadLink.removeProperty(YoutubeHelper.YT_COLLECTION);
            }
            List<LinkVariant> variants = getVariantsByLink(downloadLink);
            boolean has = false;
            for (LinkVariant vv : variants) {
                if (StringUtils.equals(vv._getUniqueId(), variant._getUniqueId())) {
                    has = true;
                    break;
                }
            }
            if (!has) {
                // extend variants list
                List<String> altIds = new ArrayList<String>();
                altIds.add(((AbstractVariant) variant).getStorableString());
                String[] variantIds = getVariantsIDList(downloadLink);
                for (String s : variantIds) {
                    altIds.add(s);
                }
                downloadLink.setProperty(YoutubeHelper.YT_VARIANTS, altIds);
                downloadLink.getTempProperties().removeProperty(YoutubeHelper.YT_VARIANTS);
            }
            YoutubeHelper.writeVariantToDownloadLink(downloadLink, v);
            if (br == null) {
                setBrowser(createNewBrowserInstance());
            }
            final String filename = new YoutubeHelper(br, getLogger()).createFilename(downloadLink);
            downloadLink.setFinalFileName(filename);
            if (nodeChangeListener instanceof CrawledLink && ((CrawledLink) nodeChangeListener).isNameSet()) {
                final CrawledLink cl = (CrawledLink) nodeChangeListener;
                final String oldName = cl.getName();
                final String newExt = Files.getExtension(filename);
                final String oldExt = Files.getExtension(oldName);
                if (newExt != null && oldExt != null) {
                    cl.setName(oldName.replaceFirst("(\\." + oldExt + ")$", "." + newExt));
                }
            }
            final String linkID = YoutubeHelper.createLinkID(downloadLink.getStringProperty(YoutubeHelper.YT_ID), (AbstractVariant) variant);
            downloadLink.setPluginPatternMatcher(linkID);
            downloadLink.setContentUrl("https://www.youtube.com" + "/watch?v=" + downloadLink.getStringProperty(YoutubeHelper.YT_ID) + "#variant=" + Encoding.urlEncode(Base64.encode(v.getStorableString())));
            downloadLink.setLinkID(linkID);
        }
        if (downloadLink.getStringProperty(YoutubeHelper.YT_TITLE, null) == null) {
            // old link?
            String oldLinkName = downloadLink.getStringProperty("name", null);
            downloadLink.setFinalFileName(oldLinkName);
        }
    }

    protected void resetStreamUrls(DownloadLink downloadLink) {
        // downloadLink.getTempProperties().setProperty(YoutubeHelper.YT_VARIANT_INFO, Property.NULL);
        downloadLink.removeProperty(YoutubeHelper.YT_STREAM_DATA_VIDEO);
        downloadLink.removeProperty(YoutubeHelper.YT_STREAM_DATA_AUDIO);
        downloadLink.removeProperty(YoutubeHelper.YT_STREAM_DATA_DATA);
    }

    public boolean hasVariantToChooseFrom(DownloadLink downloadLink) {
        String[] variantIds = getVariantsIDList(downloadLink);
        return variantIds != null && variantIds.length > 0;
    }

    private String[] getVariantsIDList(DownloadLink downloadLink) {
        // old compatibility
        Object jsonString = downloadLink.getProperty(YoutubeHelper.YT_VARIANTS);
        if (jsonString != null && jsonString instanceof String) {
            String[] lst = restoreFromString((String) jsonString, TypeRef.STRING_ARRAY);
            downloadLink.setProperty(YoutubeHelper.YT_VARIANTS, lst);
        }
        String subtitles = downloadLink.getStringProperty(YoutubeHelper.YT_SUBTITLE_CODE_LIST);
        if (StringUtils.isNotEmpty(subtitles)) {
            downloadLink.removeProperty(YoutubeHelper.YT_SUBTITLE_CODE_LIST);
            String[] queryList = restoreFromString(subtitles, TypeRef.STRING_ARRAY);
            ArrayList<String> subtitleIDs = new ArrayList<String>();
            if (queryList != null) {
                for (String q : queryList) {
                    try {
                        SubtitleVariant v = new SubtitleVariant();
                        v.setGenericInfo(new YoutubeSubtitleStorable());
                        UrlQuery info;
                        info = Request.parseQuery(q);
                        v.getGenericInfo().setKind(info.get("kind"));
                        v.getGenericInfo().setLanguage(info.get("lng"));
                        v.getGenericInfo().setSourceLanguage(info.get("src"));
                        subtitleIDs.add(v.getStorableString());
                    } catch (MalformedURLException e) {
                        e.printStackTrace();
                    }
                }
            }
            downloadLink.setProperty(YoutubeHelper.YT_VARIANTS, subtitleIDs.toArray(new String[] {}));
        }
        return downloadLink.getObjectProperty(YoutubeHelper.YT_VARIANTS, TypeRef.STRING_ARRAY);
    }

    @Override
    public List<LinkVariant> getVariantsByLink(final DownloadLink downloadLink) {
        if (hasVariantToChooseFrom(downloadLink) == false) {
            return null;
        }
        try {
            Object cache = downloadLink.getTempProperties().getProperty(YoutubeHelper.YT_VARIANTS, null);
            if (cache != null) {
                return (List<LinkVariant>) cache;
            }
            List<LinkVariant> ret = new ArrayList<LinkVariant>();
            String[] variantIds = getVariantsIDList(downloadLink);
            for (String s : variantIds) {
                final AbstractVariant vv = AbstractVariant.get(s);
                if (vv != null) {
                    ret.add(vv);
                }
            }
            downloadLink.getTempProperties().setProperty(YoutubeHelper.YT_VARIANTS, ret);
            return ret;
        } catch (Throwable e) {
            logger.log(e);
            return null;
        }
    }

    @Override
    public JComponent getVariantPopupComponent(DownloadLink downloadLink) {
        return super.getVariantPopupComponent(downloadLink);
    }

    @Override
    public boolean fillVariantsPopup(final VariantColumn variantColumn, final JPopupMenu popup, final AbstractNode value, final LinkVariant selected, final ComboBoxModel<LinkVariant> dm) {
        final CrawledLink link = (CrawledLink) value;
        VariantGroup group = ((AbstractVariant) selected).getGroup();
        switch (group) {
        case AUDIO:
        case IMAGE:
        case IMAGE_PLAYLIST_COVER:
        case VIDEO:
        case SUBTITLES:
            popup.add(new JMenuItem(new BasicAction() {
                {
                    setSmallIcon(new AbstractIcon(IconKey.ICON_REFRESH, 18));
                    setName(_GUI.T.youtube_choose_variant());
                }

                @Override
                public void actionPerformed(final ActionEvent e) {
                    showChangeOrAddVariantDialog(link, (AbstractVariant) selected);
                }
            }));
            popup.add(new JMenuItem(new BasicAction() {
                {
                    setSmallIcon(new AbstractIcon(IconKey.ICON_ADD, 18));
                    setName(_GUI.T.youtube_add_variant());
                }

                @Override
                public void actionPerformed(final ActionEvent e) {
                    showChangeOrAddVariantDialog(link, null);
                }
            }));
        }
        variantColumn.fillPopupWithPluginSettingsButton(popup, link);
        popup.add(new JSeparator());
        variantColumn.fillPopupWithVariants(popup, value, selected, dm);
        return true;
    }

    @Override
    public List<JComponent> extendLinkgrabberContextMenu(final AtomicBoolean isCancelled, final JComponent parent, final PluginView<CrawledLink> pv, Collection<PluginView<CrawledLink>> allPvs) {
        return new YoutubeLinkGrabberExtender(this, parent, pv, allPvs).run();
    }

    @Override
    public boolean onLinkCollectorDupe(CrawledLink existingLink, CrawledLink newLink) {
        // merge Variants
        DownloadLink existingDlink = existingLink.getDownloadLink();
        DownloadLink newDLink = newLink.getDownloadLink();
        List<LinkVariant> variantsExisting = getVariantsByLink(existingDlink);
        List<LinkVariant> variantsNewLink = getVariantsByLink(newDLink);
        // clear cache
        ArrayList<LinkVariant> ret = new ArrayList<LinkVariant>();
        HashSet<String> dupe = new HashSet<String>();
        if (variantsExisting != null) {
            for (LinkVariant v : variantsExisting) {
                if (dupe.add(((AbstractVariant) v).getTypeId())) {
                    ret.add(v);
                }
            }
        }
        if (variantsNewLink != null) {
            for (LinkVariant v : variantsNewLink) {
                if (dupe.add(((AbstractVariant) v).getTypeId())) {
                    ret.add(v);
                }
            }
        }
        Collections.sort(ret, new Comparator<LinkVariant>() {
            @Override
            public int compare(LinkVariant o1, LinkVariant o2) {
                AbstractVariant a1 = (AbstractVariant) o1;
                AbstractVariant a2 = (AbstractVariant) o2;
                return a2.compareTo(a1);
            }
        });
        ArrayList<String> newIDList = new ArrayList<String>();
        for (LinkVariant vi : ret) {
            newIDList.add(((AbstractVariant) vi).getStorableString());
        }
        if (newIDList.size() != variantsExisting.size()) {
            existingDlink.getTempProperties().removeProperty(YoutubeHelper.YT_VARIANTS);
            existingDlink.setProperty(YoutubeHelper.YT_VARIANTS, newIDList);
            existingDlink.setVariantSupport(newIDList.size() > 1);
            // setActiveVariantByLink(existingDlink, ret.get(0));
        }
        return false;
    }

    public boolean onLinkCrawlerDupeFilterEnabled(CrawledLink existingLink, CrawledLink newLink) {
        return false;
    }

    @Override
    public boolean allowHandle(final DownloadLink downloadLink, final PluginForHost plugin) {
        return downloadLink.getHost().equalsIgnoreCase(plugin.getHost());
    }

    protected void setConfigElements() {
    }

    @Override
    public void reset() {
    }

    @Override
    public void showChangeOrAddVariantDialog(final CrawledLink link, final AbstractVariant s) {
        ProgressGetter pg = new ProgressGetter() {
            @Override
            public void run() throws Exception {
                final YoutubeHelper helper;
                final YoutubeClipData clipData = ClipDataCache.get(helper = new YoutubeHelper(createNewBrowserInstance(), getLogger()), link.getDownloadLink());
                final ArrayList<VariantInfo> vs = new ArrayList<VariantInfo>();
                vs.addAll(clipData.findVariants());
                vs.addAll(clipData.findDescriptionVariant());
                vs.addAll(clipData.findSubtitleVariants());
                helper.extendedDataLoading(vs);
                new Thread("Choose Youtube Variant") {
                    public void run() {
                        YoutubeVariantSelectionDialog d;
                        try {
                            UIOManager.I().show(null, d = new YoutubeVariantSelectionDialog(link, s, clipData, vs)).throwCloseExceptions();
                            if (s == null) {
                                java.util.List<CheckableLink> checkableLinks = new ArrayList<CheckableLink>(1);
                                List<LinkVariant> variants = d.getVariants();
                                for (LinkVariant v : variants) {
                                    CrawledLink newLink = LinkCollector.getInstance().addAdditional(link, v);
                                    if (newLink != null) {
                                        checkableLinks.add(newLink);
                                    }
                                }
                                LinkChecker<CheckableLink> linkChecker = new LinkChecker<CheckableLink>(true);
                                linkChecker.check(checkableLinks);
                            } else {
                                LinkVariant variant = d.getVariant();
                                if (variant != null) {
                                    LinkCollector.getInstance().setActiveVariantForLink(link, variant);
                                }
                            }
                        } catch (DialogClosedException e) {
                            e.printStackTrace();
                        } catch (DialogCanceledException e) {
                            e.printStackTrace();
                        }
                    };
                }.start();
            }

            @Override
            public String getString() {
                return null;
            }

            @Override
            public int getProgress() {
                return -1;
            }

            @Override
            public String getLabelString() {
                return null;
            }
        };
        ProgressDialog dialog = new ProgressDialog(pg, 0, _GUI.T.lit_please_wait(), _GUI.T.youtube_scan_variants(), new AbstractIcon(IconKey.ICON_WAIT, 32));
        UIOManager.I().show(null, dialog);
    }
}
