package jd.plugins.decrypter;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.TreeSet;
import java.util.UUID;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.SubConfiguration;
import jd.controlling.ProgressController;
import jd.controlling.linkcrawler.LinkCrawlerDeepInspector;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;
import jd.plugins.hoster.ORFMediathek;

@DecrypterPlugin(revision = "$Revision: 52980 $", interfaceVersion = 2, names = {}, urls = {})
public class OrfAt extends PluginForDecrypt {
    public OrfAt(PluginWrapper wrapper) {
        super(wrapper);
    }

    public void init() {
        super.init();
        this.cfg = SubConfiguration.getConfig("orf.at");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.AUDIO_STREAMING, LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "orf.at" });
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
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            String pattern = "https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "/.+";
            ret.add(pattern);
        }
        return ret.toArray(new String[0]);
    }

    private final Pattern                           PATTERN_TYPE_OLD                  = Pattern.compile("(?i)https?://tvthek\\.orf\\.at/(?:index\\.php/)?(?:programs?|topic|profile)/.+");
    private final Pattern                           PATTERN_BROADCAST_OLD             = Pattern.compile("(?i)https?://([a-z0-9]+)\\.orf\\.at/(?:player|programm)/(\\d+)/([a-zA-Z0-9]+).*");
    private final Pattern                           PATTERN_BROADCAST_NEW             = Pattern.compile("(?i)https?://radiothek\\.orf\\.at/([a-z0-9]+)/(\\d+)/([a-zA-Z0-9]+).*");
    private final Pattern                           PATTERN_BROADCAST_2               = Pattern.compile("(?i)https://[^/]+/radio/([a-z0-9]+)/sendung/(\\d+)/([\\w\\-]+).*");
    private final Pattern                           PATTERN_ARTICLE                   = Pattern.compile("(?i)https?://([a-z0-9]+)\\.orf\\.at/artikel/(\\d+)/([a-zA-Z0-9]+).*");
    private final Pattern                           PATTERN_PODCAST                   = Pattern.compile("(?i)https?://[^/]+/podcasts?/([a-z0-9]+)/([A-Za-z0-9\\-]+)(/([a-z0-9\\-]+))?.*");
    private final Pattern                           PATTERN_COLLECTION                = Pattern.compile("(?i)^(https?://.*(?:/collection|podcast/highlights))/(\\d+)(/(\\d+)(/[a-z0-9\\-]+)?)?.*");
    private final Pattern                           PATTERN_VIDEO                     = Pattern.compile("(?i)^https?://[^/]+/program/(\\w+)/(\\w+)\\.html.*");
    private final Pattern                           PATTERN_ON_ORF                    = Pattern.compile("(?i)https?://on\\.orf\\.at/.+");
    private final String                            API_BASE                          = "https://audioapi.orf.at";
    private final String                            PROPERTY_SLUG                     = "slug";
    /* E.g. https://radiothek.orf.at/ooe --> "ooe" --> Channel == "oe2o" */
    private static Map<String, Map<String, Object>> CHANNEL_CACHE                     = new LinkedHashMap<String, Map<String, Object>>() {
                                                                                          protected boolean removeEldestEntry(Map.Entry<String, Map<String, Object>> eldest) {
                                                                                              return size() > 50;
                                                                                          };
                                                                                      };
    public SubConfiguration                         cfg                               = null;
    /**
     * If true, we assume progressive streams max out at 720p and HLS streams max out at 1080p. This means: if the user wants BEST, they get
     * HLS regardless of the HLS_STREAM setting, since HLS can offer a higher quality than progressive.
     */
    public static final boolean                     PROGRESSIVE_MAX_720P              = true;
    /**
     * If true, assume that once we find which HLS "quality" identifier's master playlist actually works for one segment of a video, all
     * other segments of that same video are available in that same identifier/resolution too, and we can go straight to it instead of
     * probing every candidate again. Set to false to always probe every candidate for every segment.
     */
    public static final boolean                     CACHE_HLS_QUALITY_ACROSS_SEGMENTS = true;
    /**
     * If true, only ever do one real (HEAD-request) filesize measurement per progressive quality identifier per video (whichever segment
     * triggers it first) instead of one per segment. That single sample's bytes/second rate (filesize / that segment's duration_seconds) is
     * then used to estimate every other segment's filesize of that same quality from its own duration_seconds, instead of measuring it too.
     * Trades filesize accuracy for far fewer HTTP requests during crawling.
     */
    public static final boolean                     FILESIZE_MAGIC_MODE               = true;

    /** Wrapper for podcast URLs containing md5 file-hashes inside URL. */
    protected DownloadLink createPodcastDownloadlink(final String directurl) throws MalformedURLException {
        final DownloadLink link = this.createDownloadlink(directurl, true);
        final UrlQuery query = UrlQuery.parse(directurl);
        final String md5Hash = query.get("etag");
        if (md5Hash != null) {
            link.setMD5Hash(md5Hash);
        }
        return link;
    }

    @Override
    protected DownloadLink createDownloadlink(final String directurl) {
        final DownloadLink link = super.createDownloadlink(directurl);
        link.setProperty(DirectHTTP.PROPERTY_REQUEST_TYPE, "GET");
        return link;
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        Regex broadcastOld = null;
        Regex broadcastNew = null;
        Regex broadcast2 = null;
        if (PATTERN_TYPE_OLD.matcher(param.getCryptedUrl()).matches()) {
            return this.crawlOrfmediathekOld(param);
        } else if (PATTERN_ON_ORF.matcher(param.getCryptedUrl()).matches()) {
            return this.crawlOrfmediathekNew(param, param.getCryptedUrl());
        } else if (PATTERN_ARTICLE.matcher(param.getCryptedUrl()).matches()) {
            return this.crawlArticle(param);
        } else if (PATTERN_COLLECTION.matcher(param.getCryptedUrl()).matches()) {
            return this.crawlCollection(param);
        } else if (PATTERN_PODCAST.matcher(param.getCryptedUrl()).matches()) {
            return this.crawlPodcast(param);
        } else if ((broadcastOld = new Regex(param.getCryptedUrl(), PATTERN_BROADCAST_OLD)).patternFind()) {
            final String broadCastID = broadcastOld.getMatch(2);
            final String broadcastDay = broadcastOld.getMatch(1);
            final String domainID = broadcastOld.getMatch(0);
            return crawlProgramm(domainID, broadCastID, broadcastDay);
        } else if ((broadcastNew = new Regex(param.getCryptedUrl(), PATTERN_BROADCAST_NEW)).patternFind()) {
            final String broadCastID = broadcastNew.getMatch(2);
            final String broadcastDay = broadcastNew.getMatch(1);
            final String domainID = broadcastNew.getMatch(0);
            return crawlProgramm(domainID, broadCastID, broadcastDay);
        } else if ((broadcast2 = new Regex(param.getCryptedUrl(), PATTERN_BROADCAST_2)).patternFind()) {
            final String domainID = broadcast2.getMatch(0);
            final String broadcastID = broadcast2.getMatch(1);
            return crawlBroadcast(domainID, broadcastID);
        } else if (PATTERN_VIDEO.matcher(param.getCryptedUrl()).matches()) {
            return crawlVideo(param);
        } else {
            /* Unsupported URL -> Developer mistake */
            logger.info("Unsupported URL: " + param);
            return new ArrayList<DownloadLink>(0);
        }
    }

    private ArrayList<DownloadLink> crawlOrfmediathekNewEncryptedID(final String encryptedID, String sourceurl) throws Exception {
        if (StringUtils.isEmpty(encryptedID)) {
            throw new IllegalArgumentException();
        }
        br.getPage("https://api-tvthek.orf.at/api/v4.3/public/episode/encrypted/" + Encoding.urlEncode(encryptedID));
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* E.g. {"error":{"code":404,"message":"Not Found"}} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> _embedded = (Map<String, Object>) entries.get("_embedded");
        final String contentIDSlashPlaylistIDSlashVideoID = entries.get("id").toString();
        final List<Map<String, Object>> segments = (List<Map<String, Object>>) _embedded.get("segments");
        final String dateStr = entries.get("date").toString();
        final String dateWithoutTime = new Regex(dateStr, "(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        final String mainVideoTitle = entries.get("title").toString();
        final String description = (String) entries.get("description");
        // final Boolean is_drm_protected = (Boolean) entries.get("is_drm_protected");
        // if (Boolean.TRUE.equals(is_drm_protected)) {
        // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "DRM protected");
        // }
        if (sourceurl == null) {
            sourceurl = entries.get("share_body").toString();
            if (StringUtils.isEmpty(sourceurl)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        final HashSet<String> allFMTs = new HashSet<String>();
        /* Collected across all segments (unlike segmentAvailableHeights, which is per-segment) -> sorted, no duplicates. */
        final TreeSet<Integer> allVideoHeights = new TreeSet<Integer>();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(dateWithoutTime + " - " + mainVideoTitle);
        if (description != null) {
            fp.setComment(description);
        }
        fp.setPackageKey("orfmediathek://video/" + contentIDSlashPlaylistIDSlashVideoID);
        /* 2026-07-14: Age restriction used to be skippable via gapless streams but not anymore. */
        final boolean has_active_youth_protection = ((Boolean) entries.get("has_active_youth_protection")).booleanValue();
        if (has_active_youth_protection) {
            throw new DecrypterRetryException(RetryReason.AGE_VERIFICATION_REQUIRED);
        }
        final ORFMediathek hosterplugin = (ORFMediathek) this.getNewPluginForHostInstance("orf.at");
        final Map<Integer, Long> cumulativeFilesizeMap = new HashMap<Integer, Long>();
        /* Selected video qualities by height (in pixels). Progressive delivery only exists for 360/540/720 (Q4A/Q6A/Q8C). */
        final List<Integer> selectedQualities = new ArrayList<Integer>();
        if (cfg == null || cfg.getBooleanProperty(ORFMediathek.Q_HEIGHT_288, ORFMediathek.Q_HEIGHT_288_default)) {
            selectedQualities.add(288);
        }
        if (cfg == null || cfg.getBooleanProperty(ORFMediathek.Q_HEIGHT_360, ORFMediathek.Q_HEIGHT_360_default)) {
            selectedQualities.add(360);
        }
        if (cfg == null || cfg.getBooleanProperty(ORFMediathek.Q_HEIGHT_540, ORFMediathek.Q_HEIGHT_540_default)) {
            selectedQualities.add(540);
        }
        if (cfg == null || cfg.getBooleanProperty(ORFMediathek.Q_HEIGHT_576, ORFMediathek.Q_HEIGHT_576_default)) {
            selectedQualities.add(576);
        }
        if (cfg == null || cfg.getBooleanProperty(ORFMediathek.Q_HEIGHT_720, ORFMediathek.Q_HEIGHT_720_default)) {
            selectedQualities.add(720);
        }
        if (cfg == null || cfg.getBooleanProperty(ORFMediathek.Q_HEIGHT_1080, ORFMediathek.Q_HEIGHT_1080_default)) {
            selectedQualities.add(1080);
        }
        final int subtitleFormatSettingInt = cfg.getIntegerProperty(ORFMediathek.SETTING_SELECTED_SUBTITLE_FORMAT, ORFMediathek.SETTING_SELECTED_SUBTITLE_FORMAT_default);
        final String subtitle_ext;
        final String subtitle_key_name;
        switch (subtitleFormatSettingInt) {
        case 0:
            subtitle_ext = ".smi";
            subtitle_key_name = "sami_url";
            break;
        case 1:
            subtitle_ext = ".srt";
            subtitle_key_name = "srt_url";
            break;
        case 2:
            subtitle_ext = ".ttml";
            subtitle_key_name = "ttml_url";
            break;
        case 3:
            subtitle_ext = ".vtt";
            subtitle_key_name = "vtt_url";
            break;
        case 4:
            subtitle_ext = ".xml";
            subtitle_key_name = "xml_url";
            break;
        default:
            logger.info("Unsupported subtitle selection:" + subtitleFormatSettingInt);
            subtitle_ext = ".vtt";
            subtitle_key_name = "vtt_url";
            break;
        }
        final Map<String, Object> gapless_subtitlemap = (Map<String, Object>) _embedded.get("subtitle");
        final String gapless_subtitleurl = gapless_subtitlemap != null ? (String) gapless_subtitlemap.get(subtitle_key_name) : null;
        String thumbnailurlFromFirstSegment = null;
        final boolean settingPreferBestVideo = cfg != null ? cfg.getBooleanProperty(ORFMediathek.Q_BEST, ORFMediathek.Q_BEST_default) : false;
        final boolean settingEnableFastCrawl = cfg != null ? cfg.getBooleanProperty(ORFMediathek.SETTING_ENABLE_FAST_CRAWL, ORFMediathek.SETTING_ENABLE_FAST_CRAWL_default) : true;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final boolean isCrawlGaplessAndVideoChapters;
        boolean isCrawlGaplessOnly;
        if (cfg == null) {
            isCrawlGaplessAndVideoChapters = true;
            isCrawlGaplessOnly = false;
        } else {
            final int videoFormatSettingInt = cfg.getIntegerProperty(ORFMediathek.SETTING_SELECTED_VIDEO_FORMAT, ORFMediathek.SETTING_SELECTED_VIDEO_FORMAT_default);
            isCrawlGaplessAndVideoChapters = videoFormatSettingInt == 0;
            isCrawlGaplessOnly = videoFormatSettingInt == 2;
        }
        /*
         * Whether the user wants gapless results at all (either alongside chapters or exclusively) -> also needed below to decide whether
         * to bother probing HLS qualities per segment.
         */
        final boolean userWantsGapless = isCrawlGaplessAndVideoChapters || isCrawlGaplessOnly;
        /*
         * Only ever written to when CACHE_HLS_QUALITY_ACROSS_SEGMENTS is true; holds the first segment's discovered HLS heights. Declared
         * here (outside crawlSegments) so the gapless handling below can reuse it instead of probing the gapless master's HLS qualities all
         * over again -> the gapless master is just the whole-episode version of the exact same encode ladder.
         */
        final HashSet<Integer> cachedAvailableHeights = new HashSet<Integer>();
        crawlSegments: {
            if (segments == null || segments.isEmpty()) {
                /**
                 * No segments there to crawl, maybe only one gapless item available? <br>
                 * Segments should always exist!
                 */
                break crawlSegments;
            }
            final boolean allowProgressive = cfg != null ? cfg.getBooleanProperty(ORFMediathek.PROGRESSIVE_STREAM, ORFMediathek.PROGRESSIVE_STREAM_default) : true;
            final boolean user_wants_HLS = cfg != null ? cfg.getBooleanProperty(ORFMediathek.HLS_STREAM, ORFMediathek.HLS_STREAM_default) : true;
            /* HDS is permanently broken server-side (since 2024-02-20) -> HDS streams are always skipped, no setting for it anymore. */
            /*
             * Progressive is only ever available up to 720p (see PROGRESSIVE_MAX_720P). If the user selected BEST, or explicitly picked a
             * quality above 720p (currently only 1080p), progressive alone can't satisfy that -> HLS must be used regardless of the HLS
             * setting. A quality below 720p that happens to only exist via HLS (e.g. 288p) does NOT trigger this -> that case is left to
             * the normal "return everything if the selection matches nothing" fallback.
             */
            final boolean wantsQualityAboveProgressiveCap = containsHeightAbove(selectedQualities, 720);
            final boolean crawlHLS = user_wants_HLS || userWantsGapless || ((settingPreferBestVideo || wantsQualityAboveProgressiveCap) && PROGRESSIVE_MAX_720P);
            /*
             * Once we know which HLS "quality" identifier's playlist actually works for one segment, go straight to that one for all other
             * segments of this same video instead of probing all candidates again. Once we know whether HLS offers anything above
             * progressive's cap for one segment, reuse that too and skip the HLS probe entirely for later segments if it doesn't (and the
             * user didn't explicitly ask for HLS). See CACHE_HLS_QUALITY_ACROSS_SEGMENTS.
             */
            String cachedWorkingHlsQuality = null;
            Boolean cachedFoundHeightAboveProgressiveCap = null;
            /*
             * Only ever written to when FILESIZE_MAGIC_MODE is true: bytes/second measured from the one real HEAD request done per
             * progressive quality, used to estimate (instead of measure) the filesize of every other segment of that same quality.
             */
            final Map<Integer, Double> cachedBytesPerSecondByHeight = new HashMap<Integer, Double>();
            int videoPosition = 0;
            for (final Map<String, Object> segment : segments) {
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
                videoPosition++;
                final String segmentID = segment.get("id").toString();
                String thumbnailurl = null;
                final Map<String, Object> thisEmbedded = (Map<String, Object>) segment.get("_embedded");
                final Map<String, Object> segment_embedded_playlist = (Map<String, Object>) thisEmbedded.get("playlist");
                final String segment_embedded_playlist_title = segment_embedded_playlist.get("title").toString();
                thumbnailurl = (String) JavaScriptEngineFactory.walkJson(segment, "_embedded/image/public_urls/highlight_teaser/url");
                if (thumbnailurlFromFirstSegment == null) {
                    thumbnailurlFromFirstSegment = thumbnailurl;
                }
                final List<Map<String, Object>> sources = (List<Map<String, Object>>) segment_embedded_playlist.get("sources");
                final List<Map<String, Object>> subtitlemaps = (List<Map<String, Object>>) segment_embedded_playlist.get("subtitles");
                String subtitleurl = null;
                if (subtitlemaps != null && subtitlemaps.size() > 0) {
                    /* Look for subtitle in user preferred format */
                    for (final Map<String, Object> subtitlemap : subtitlemaps) {
                        final String url = subtitlemap.get("src").toString();
                        final String type = subtitlemap.get("type").toString();
                        if (StringUtils.endsWithCaseInsensitive(url, subtitle_ext) || type.equalsIgnoreCase(subtitle_ext.replace(".", ""))) {
                            subtitleurl = url;
                            break;
                        }
                    }
                    if (subtitleurl == null) {
                        logger.warning("Selected subtitle format hasn't been found -> Looks like the assumption that all formats are always available is wrong");
                    }
                }
                if (subtitleurl == null && segments.size() == 1 && gapless_subtitleurl != null) {
                    /* Only one segment -> We can use gapless subtitle as fallback */
                    subtitleurl = gapless_subtitleurl;
                }
                final Map<Integer, Long> qualityHeightToFilesizeMap = new HashMap<Integer, Long>();
                final List<DownloadLink> videoresults = new ArrayList<DownloadLink>();
                final HashSet<Integer> segmentAvailableHeights = new HashSet<Integer>();
                /**
                 * Collect all distinct HLS master playlist ("playlist.m3u8") URLs. Each of these actually contains all quality renditions
                 * once parsed, so only one needs to be fetched. Q8C is often broken server-side -> its URL is placed last so it is tried
                 * last/as a last resort.
                 */
                boolean skipProgressiveForBest = false;
                find_hls_qualities: if (crawlHLS) {
                    if (CACHE_HLS_QUALITY_ACROSS_SEGMENTS && !user_wants_HLS && allowProgressive && Boolean.FALSE.equals(cachedFoundHeightAboveProgressiveCap)) {
                        /*
                         * We already established for an earlier segment of this video that HLS doesn't offer anything above progressive's
                         * cap, and the user didn't explicitly ask for HLS -> it would get discarded anyway, so don't even probe it here.
                         * Only applies when progressive is actually enabled -> if it's disabled, HLS is our only source and must never be
                         * skipped.
                         */
                        logger.info("Skipping HLS probe for this segment: already known that it offers nothing beyond progressive for this video");
                        break find_hls_qualities;
                    }
                    final LinkedHashMap<String, String> hlsUrlByQuality = new LinkedHashMap<String, String>();
                    for (final Map<String, Object> source : sources) {
                        final String srcUrl = (String) source.get("src");
                        final String srcDelivery = (String) source.get("delivery");
                        final String srcQuality = (String) source.get("quality");
                        final String srcQualityString = (String) source.get("quality_string");
                        allFMTs.add(srcQuality);
                        if (!"hls".equalsIgnoreCase(srcDelivery)) {
                            continue;
                        } else if (StringUtils.equals(srcQuality, "QXADRM") || StringUtils.equalsIgnoreCase(srcQualityString, "adaptiv")) {
                            /* DRM protected or a split audio/video track -> not a usable master playlist. */
                            continue;
                        } else if (!StringUtils.containsIgnoreCase(srcUrl, "playlist.m3u8")) {
                            /* This should never happen */
                            logger.warning("Found HLS url that is not a playlist: " + srcUrl);
                            continue;
                        }
                        hlsUrlByQuality.put(srcQuality, srcUrl);
                    }
                    String hlsMaster = null;
                    String workingHlsQuality = null;
                    final Browser brc = br.cloneBrowser();
                    boolean foundHeightAboveProgressiveCap = false;
                    /*
                     * Heights actually used for THIS segment's results. Must be segment-local: reusing cachedAvailableHeights directly as
                     * the working set would leak heights from earlier segments into every segment's results whenever a fresh probe runs
                     * (always the case with CACHE_HLS_QUALITY_ACROSS_SEGMENTS=false, since cachedWorkingHlsQuality then never gets set and
                     * every segment takes the "else" branch below).
                     */
                    final HashSet<Integer> availableHeightsThisSegment = new HashSet<Integer>();
                    if (CACHE_HLS_QUALITY_ACROSS_SEGMENTS && cachedWorkingHlsQuality != null) {
                        /*
                         * We already know which quality worked for an earlier segment of this same video -> go straight to it instead of
                         * probing every candidate again (that probing is where the time-consuming http requests happen). Assume it exists
                         * for this segment too; if that assumption turns out to be wrong, fail loudly instead of silently falling back.
                         */
                        final String cachedUrl = hlsUrlByQuality.get(cachedWorkingHlsQuality);
                        if (cachedUrl == null) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Expected cached HLS quality " + cachedWorkingHlsQuality + " to exist for this segment but it doesn't");
                        }
                        hlsMaster = cachedUrl;
                        workingHlsQuality = cachedWorkingHlsQuality;
                        foundHeightAboveProgressiveCap = cachedFoundHeightAboveProgressiveCap.booleanValue();
                        availableHeightsThisSegment.addAll(cachedAvailableHeights);
                    } else {
                        final List<String> hlsQualityTryOrder = new ArrayList<String>(hlsUrlByQuality.keySet());
                        if (hlsQualityTryOrder.remove("Q8C")) {
                            /* Q8C is often broken server-side -> tried last/as a last resort. */
                            hlsQualityTryOrder.add("Q8C");
                        }
                        for (final String hlsQualityTag : hlsQualityTryOrder) {
                            if (this.isAbort()) {
                                throw new InterruptedException();
                            }
                            /* Try each candidate until we find one that actually works. */
                            final String hlsPlaylistUrl = hlsUrlByQuality.get(hlsQualityTag);
                            brc.getPage(hlsPlaylistUrl);
                            if (ORFMediathek.isAgeRestricted(brc.getURL())) {
                                /* Item is GEO-blocked */
                                throw new DecrypterRetryException(RetryReason.AGE_VERIFICATION_REQUIRED);
                            } else if (ORFMediathek.isGeoBlocked(brc.getURL())) {
                                /* Item is GEO-blocked */
                                throw new DecrypterRetryException(RetryReason.GEO);
                            } else if (!LinkCrawlerDeepInspector.looksLikeMpegURL(brc.getHttpConnection())) {
                                logger.info("Skipping bad HLS playlist: " + hlsPlaylistUrl);
                                continue;
                            }
                            hlsMaster = hlsPlaylistUrl;
                            workingHlsQuality = hlsQualityTag;
                            break;
                        }
                        if (hlsMaster == null) {
                            logger.warning("Found zero usable HLS qualities");
                            break find_hls_qualities;
                        }
                        final List<HlsContainer> hlsQualities = HlsContainer.getHlsQualities(brc);
                        for (final HlsContainer hlsQuality : hlsQualities) {
                            if (!foundHeightAboveProgressiveCap && hlsQuality.getHeight() > 720) {
                                foundHeightAboveProgressiveCap = true;
                            }
                            availableHeightsThisSegment.add(hlsQuality.getHeight());
                        }
                        cachedFoundHeightAboveProgressiveCap = Boolean.valueOf(foundHeightAboveProgressiveCap);
                        if (CACHE_HLS_QUALITY_ACROSS_SEGMENTS) {
                            cachedWorkingHlsQuality = workingHlsQuality;
                            cachedAvailableHeights.addAll(availableHeightsThisSegment);
                        }
                    }
                    /*
                     * If the user doesn't want HLS, only use it anyway when it actually beats progressive (something above 720p vs.
                     * progressive's assumed 720p cap) and either BEST or a quality above 720p is wanted. If HLS tops out at the same
                     * quality as progressive here, respect the user's choice and skip it entirely so only progressive gets offered.
                     */
                    final boolean useHls = user_wants_HLS || !allowProgressive || (foundHeightAboveProgressiveCap && (settingPreferBestVideo || wantsQualityAboveProgressiveCap) && PROGRESSIVE_MAX_720P);
                    if (!useHls) {
                        logger.info("HLS doesn't offer more than progressive here and user doesn't want HLS -> Skipping HLS results for this segment");
                        break find_hls_qualities;
                    }
                    /*
                     * If the user didn't explicitly ask for HLS and progressive is still available, HLS is only here to supplement
                     * progressive's cap -> only take the heights progressive can't offer (above 720p). Otherwise (HLS explicitly wanted, or
                     * progressive fully disabled) HLS is a full alternative source -> take every height it offers.
                     */
                    final boolean hlsCoversEverything = user_wants_HLS || !allowProgressive;
                    for (final int height : availableHeightsThisSegment) {
                        if (!hlsCoversEverything && height <= 720) {
                            logger.info("Skipping HLS height " + height + "p: already covered by progressive and user doesn't want HLS");
                            continue;
                        }
                        final DownloadLink video = super.createDownloadlink(hlsMaster);
                        video.setDefaultPlugin(hosterplugin);
                        video.setHost(hosterplugin.getHost());
                        video.setContentUrl(sourceurl);
                        video.setProperty(ORFMediathek.PROPERTY_CONTENT_TYPE, ORFMediathek.CONTENT_TYPE_VIDEO);
                        video.setProperty(ORFMediathek.PROPERTY_DIRECTURL, hlsMaster);
                        /* We know the real height here -> no need to guess it from an internal quality identifier later. */
                        video.setProperty(ORFMediathek.PROPERTY_VIDEO_HEIGHT, height);
                        video.setProperty(ORFMediathek.PROPERTY_STREAMING_TYPE, "http");
                        video.setProperty(ORFMediathek.PROPERTY_DELIVERY, "hls");
                        video.setAvailable(true);
                        videoresults.add(video);
                        segmentAvailableHeights.add(height);
                        allVideoHeights.add(height);
                    }
                    if (foundHeightAboveProgressiveCap && settingPreferBestVideo && PROGRESSIVE_MAX_720P) {
                        /*
                         * HLS already gives us the best possible quality here -> no need to also check progressive (saves a http request).
                         * Deliberately BEST-only: in non-BEST/checkbox mode the user may still want a progressive-capable quality (e.g.
                         * 720p) alongside the HLS-only one, so progressive must not be skipped there.
                         */
                        logger.info("Found a quality above progressive's cap via HLS and user wants BEST -> Skipping progressive qualities");
                        skipProgressiveForBest = true;
                    }
                }
                crawl_progressive_videos: {
                    if (skipProgressiveForBest || !allowProgressive) {
                        break crawl_progressive_videos;
                    }
                    /*
                     * This loop only handles progressive items now -> HLS is fully handled by find_hls_qualities above. Note: if this whole
                     * loop is skipped, allFMTs won't contain non-HLS quality identifiers and isProgressiveStreamAvailable stays false,
                     * which only matters for the gapless crawling below (alreadyFoundGaplessProgressive).
                     */
                    final long segmentDurationSeconds = ((Number) segment.get("duration_seconds")).longValue();
                    for (final Map<String, Object> source : sources) {
                        final String src = (String) source.get("src");
                        final String quality = source.get("quality").toString();
                        final String quality_string = source.get("quality_string").toString();
                        final String protocol = source.get("protocol").toString();
                        final String delivery = source.get("delivery").toString();
                        allFMTs.add(quality);
                        if (!"progressive".equalsIgnoreCase(delivery)) {
                            /* HLS is handled separately above; HDS/DASH/RTMP/RTSP are unsupported/broken. */
                            continue;
                        }
                        /* Skip stuff we cannot handle */
                        if (StringUtils.equals(quality, "QXADRM")) {
                            continue;
                        } else if (!"http".equalsIgnoreCase(protocol)) {
                            continue;
                        } else if (quality_string.equalsIgnoreCase("adaptiv")) {
                            /* Split audio/video HLS items -> shouldn't occur for progressive but keep this safety check. */
                            continue;
                        }
                        final Integer qualityHeight = oldQualityIdentifierToHeight(quality);
                        if (qualityHeight == null) {
                            /**
                             * Skip progressive items for which we don't know the height -> no longer selectable. This includes LOW (Q1A),
                             * which used to be legacy .3gp files (2026-06-12), and ADAPTIV.
                             */
                            logger.info("Skipping progressive item with unmapped quality: " + quality);
                            continue;
                        }
                        final DownloadLink video = super.createDownloadlink(src);
                        final boolean looksLikeQualityIsSelected = selectedQualities.contains(qualityHeight) || (settingPreferBestVideo && qualityHeight == 720);
                        progressiveStreamFilesizeCheck: if (looksLikeQualityIsSelected && !settingEnableFastCrawl && !has_active_youth_protection) {
                            final Double cachedBytesPerSecond = FILESIZE_MAGIC_MODE ? cachedBytesPerSecondByHeight.get(qualityHeight) : null;
                            if (cachedBytesPerSecond != null) {
                                /*
                                 * filesizeMagicMode: we already measured this quality once for an earlier segment of this video -> estimate
                                 * this segment's filesize from that rate instead of spending another HEAD request on it.
                                 */
                                final long estimatedFilesize = Math.round(cachedBytesPerSecond.doubleValue() * segmentDurationSeconds);
                                qualityHeightToFilesizeMap.put(qualityHeight, estimatedFilesize);
                                final Long filesizeSumForAllSegmentsOfThisQuality = cumulativeFilesizeMap.get(qualityHeight);
                                if (filesizeSumForAllSegmentsOfThisQuality != null) {
                                    cumulativeFilesizeMap.put(qualityHeight, filesizeSumForAllSegmentsOfThisQuality + estimatedFilesize);
                                } else {
                                    cumulativeFilesizeMap.put(qualityHeight, estimatedFilesize);
                                }
                                break progressiveStreamFilesizeCheck;
                            }
                            logger.info("Checking progressive URL to find filesize: " + src);
                            URLConnectionAdapter con = null;
                            final Browser brc = br.cloneBrowser();
                            try {
                                brc.setFollowRedirects(true);
                                con = brc.openHeadConnection(src);
                                if (ORFMediathek.isInsertVideoNotAvailable(con.getRequest())) {
                                    logger.info("Skipping \"insert_video_not_available\": " + src);
                                    continue;
                                } else if (ORFMediathek.isGeoBlocked(con.getURL().toExternalForm())) {
                                    /* Item is GEO-blocked */
                                    throw new DecrypterRetryException(RetryReason.GEO);
                                } else if (!this.looksLikeDownloadableContent(con)) {
                                    logger.info("Skipping broken progressive video quality: " + src);
                                    continue;
                                }
                                final long filesize = con.getCompleteContentLength();
                                if (filesize <= 0) {
                                    /* This should never happen */
                                    logger.info("Found progressive stream with unknown file size: " + src);
                                    break progressiveStreamFilesizeCheck;
                                }
                                /* Set verified filesize on this particular element as we know its' precise file size. */
                                video.setVerifiedFileSize(filesize);
                                qualityHeightToFilesizeMap.put(qualityHeight, filesize);
                                final Long filesizeSumForAllSegmentsOfThisQuality = cumulativeFilesizeMap.get(qualityHeight);
                                if (filesizeSumForAllSegmentsOfThisQuality != null) {
                                    cumulativeFilesizeMap.put(qualityHeight, filesizeSumForAllSegmentsOfThisQuality + filesize);
                                } else {
                                    cumulativeFilesizeMap.put(qualityHeight, filesize);
                                }
                                if (FILESIZE_MAGIC_MODE && segmentDurationSeconds > 0) {
                                    /*
                                     * Derive the bytes/second rate for this quality from this one real measurement -> used for all its
                                     * other segments.
                                     */
                                    cachedBytesPerSecondByHeight.put(qualityHeight, Double.valueOf(filesize / (double) segmentDurationSeconds));
                                }
                            } catch (final Exception e) {
                                if (e instanceof DecrypterRetryException) {
                                    throw e;
                                }
                                /* Ignore other Exception types */
                                logger.log(e);
                                logger.info("Exception happened during file size check");
                            } finally {
                                try {
                                    brc.followConnection();
                                } catch (final Throwable e) {
                                }
                                try {
                                    con.disconnect();
                                } catch (final Throwable e) {
                                }
                            }
                        }
                        segmentAvailableHeights.add(qualityHeight);
                        allVideoHeights.add(qualityHeight);
                        video.setDefaultPlugin(hosterplugin);
                        video.setHost(hosterplugin.getHost());
                        video.setContentUrl(sourceurl);
                        video.setProperty(ORFMediathek.PROPERTY_CONTENT_TYPE, ORFMediathek.CONTENT_TYPE_VIDEO);
                        video.setProperty(ORFMediathek.PROPERTY_DIRECTURL, src);
                        video.setProperty(ORFMediathek.PROPERTY_INTERNAL_QUALITY, quality);
                        video.setProperty(ORFMediathek.PROPERTY_STREAMING_TYPE, protocol);
                        video.setProperty(ORFMediathek.PROPERTY_DELIVERY, delivery);
                        video.setAvailable(true);
                        videoresults.add(video);
                    }
                }
                /* Set additional properties */
                for (final DownloadLink videoresult : videoresults) {
                    /*
                     * Small trick: Update file sizes of all video items: If filesizes were found from progressive streams we can assume
                     * that the same file streamed via different streaming method will have a very similar size.
                     */
                    if (videoresult.isSizeSet()) {
                        continue;
                    }
                    final Integer height = ORFMediathek.getVideoHeight(videoresult);
                    final Long filesize = height != null ? qualityHeightToFilesizeMap.get(height) : null;
                    if (filesize != null) {
                        videoresult.setDownloadSize(filesize);
                    }
                }
                final Integer bestAvailableHeight = findBestQuality(segmentAvailableHeights);
                final List<DownloadLink> selectedVideoQualities = new ArrayList<DownloadLink>();
                final List<DownloadLink> bestVideos = new ArrayList<DownloadLink>();
                for (final DownloadLink videoresult : videoresults) {
                    /*
                     * Items with unknown height (e.g. LOW/Q1A) are never selectable/best -> getVideoHeight returns null for them. No
                     * delivery-type filter needed here: videoresults only ever contains delivery types that were allowed to be crawled in
                     * the first place (crawlHLS / allowProgressive above).
                     */
                    final Integer resultHeight = ORFMediathek.getVideoHeight(videoresult);
                    if (resultHeight == null) {
                        continue;
                    }
                    if (selectedQualities.contains(resultHeight)) {
                        selectedVideoQualities.add(videoresult);
                    }
                    if (resultHeight.equals(bestAvailableHeight)) {
                        bestVideos.add(videoresult);
                    }
                }
                final List<DownloadLink> chosenVideoResults = new ArrayList<DownloadLink>();
                if (settingPreferBestVideo) {
                    /* Assume that we always find best-results. BEST has no fixed set of desired qualities to compare against. */
                    if (bestVideos.isEmpty()) {
                        logger.warning("BEST quality requested but no video results were found for this segment at all");
                    }
                    chosenVideoResults.addAll(bestVideos);
                } else {
                    if (selectedVideoQualities.size() > 0) {
                        chosenVideoResults.addAll(selectedVideoQualities);
                    } else {
                        /* Fallback */
                        logger.info("None of the desired qualities " + selectedQualities + " were found for this segment -> Returning all available qualities instead");
                        chosenVideoResults.addAll(videoresults);
                    }
                }
                final List<DownloadLink> finalresults = new ArrayList<DownloadLink>();
                finalresults.addAll(chosenVideoResults);
                /* Add a subtitle-result for each chosen video quality */
                if (cfg == null || cfg.getBooleanProperty(ORFMediathek.Q_SUBTITLES, ORFMediathek.Q_SUBTITLES_default) && !StringUtils.isEmpty(subtitleurl)) {
                    for (final DownloadLink chosenVideoResult : chosenVideoResults) {
                        finalresults.add(createSubtitleLink(subtitleurl, subtitle_ext, chosenVideoResult, hosterplugin, sourceurl));
                    }
                }
                if (cfg == null || cfg.getBooleanProperty(ORFMediathek.Q_THUMBNAIL, ORFMediathek.Q_THUMBNAIL_default) && !StringUtils.isEmpty(thumbnailurl)) {
                    finalresults.add(createThumbnailLink(thumbnailurl, hosterplugin));
                }
                /* Add more properties which are the same for all results of this segment */
                for (final DownloadLink result : finalresults) {
                    result.setProperty(ORFMediathek.PROPERTY_VIDEO_POSITION, videoPosition);
                    result.setProperty(ORFMediathek.PROPERTY_VIDEO_POSITION_MAX, segments.size());
                    result.setProperty(ORFMediathek.PROPERTY_TITLE, segment_embedded_playlist_title);
                    result.setProperty(ORFMediathek.PROPERTY_SEGMENT_ID, segmentID);
                    ret.add(result);
                }
            }
        }
        logger.info("Available video qualities (height in px) across all segments of this video: " + allVideoHeights);
        final boolean allowCrawlGapless;
        if (has_active_youth_protection) {
            /**
             * With a bit of luck, this can skip age protection <br>
             * 2026-07-14: Not possible anymore, see upper comments.
             */
            logger.info("Allowing to crawl gapless items to avoid youth protection");
            allowCrawlGapless = true;
            /* Non-gapless items remain youth-blocked so let's discard them if we find gapless items. */
            isCrawlGaplessOnly = true;
        } else if (ret.isEmpty()) {
            /* Found nothing -> Try to crawl gapless items */
            logger.info("Found nothing -> Allow to crawl gapless version as fallback");
            allowCrawlGapless = true;
        } else if (userWantsGapless && segments.size() > 1) {
            logger.info("User prefers gapless and gapless handling is needed because we got more than one segment: " + segments.size());
            allowCrawlGapless = true;
        } else {
            /* Do not crawl gapless streams */
            allowCrawlGapless = false;
        }
        int numberofGaplessItems = 0;
        crawlGapless: {
            if (!allowCrawlGapless) {
                break crawlGapless;
            }
            /*
             * Gapless video handling: Unlike the per-segment sources above (one chunk per chapter), the episode-level HLS sources QXA/QXB
             * are each a single continuous, non-chaptered adaptive stream covering the whole episode -> that's what makes them "gapless".
             * QXA and QXB are equivalent mirrors of the same content, so which one we use doesn't matter -> pick one at random. Which
             * qualities (heights) that master offers is already known from cachedAvailableHeights (probed per-segment above) -> no need to
             * fetch and parse this master's playlist again just to find that out.
             */
            if (cachedAvailableHeights.isEmpty()) {
                /* This should never happen */
                logger.info("No cached HLS qualities available -> Gapless not possible");
                break crawlGapless;
            }
            logger.info("Crawling gapless video stream");
            final Map<String, Object> sourcesForGaplessVideo = (Map<String, Object>) entries.get("sources");
            if (sourcesForGaplessVideo == null || sourcesForGaplessVideo.isEmpty()) {
                logger.info("No gapless sources available -> Returning items we found so far");
                break crawlGapless;
            }
            final List<Map<String, Object>> sources_hls = (List<Map<String, Object>>) sourcesForGaplessVideo.get("hls");
            if (sources_hls == null || sources_hls.isEmpty()) {
                logger.info("No gapless HLS sources available -> Returning items we found so far");
                break crawlGapless;
            }
            final List<Map<String, Object>> gaplessMasterCandidates = new ArrayList<Map<String, Object>>();
            for (final Map<String, Object> source : sources_hls) {
                final String qualityKey = (String) source.get("quality_key");
                if (!"QXA".equals(qualityKey) && !"QXB".equals(qualityKey)) {
                    /* Not a whole-episode adaptive master -> not usable for gapless. */
                    continue;
                } else if (Boolean.TRUE.equals(source.get("is_drm_protected"))) {
                    logger.info("Skipping DRM protected gapless master: " + qualityKey);
                    continue;
                }
                gaplessMasterCandidates.add(source);
            }
            if (gaplessMasterCandidates.isEmpty()) {
                logger.info("No usable (non DRM protected) QXA/QXB gapless master found -> Returning [non-gapless] items we found so far");
                break crawlGapless;
            }
            final Map<String, Object> chosenGaplessMaster = gaplessMasterCandidates.get(new Random().nextInt(gaplessMasterCandidates.size()));
            final String hlsMaster = chosenGaplessMaster.get("src").toString();
            final List<DownloadLink> gaplessresults = new ArrayList<DownloadLink>();
            for (final int height : cachedAvailableHeights) {
                final DownloadLink video = this.createDownloadlink(hlsMaster);
                video.setDefaultPlugin(hosterplugin);
                video.setHost(hosterplugin.getHost());
                video.setContentUrl(sourceurl);
                video.setProperty(ORFMediathek.PROPERTY_CONTENT_TYPE, ORFMediathek.CONTENT_TYPE_VIDEO);
                video.setProperty(ORFMediathek.PROPERTY_DIRECTURL, hlsMaster);
                video.setProperty(ORFMediathek.PROPERTY_VIDEO_HEIGHT, height);
                video.setProperty(ORFMediathek.PROPERTY_STREAMING_TYPE, "http");
                video.setProperty(ORFMediathek.PROPERTY_DELIVERY, "hls");
                final Long guessedFilesize = cumulativeFilesizeMap.get(height);
                if (guessedFilesize != null) {
                    video.setDownloadSize(guessedFilesize);
                }
                gaplessresults.add(video);
            }
            /*
             * Same quality selection already used for the non-gapless (chapters) results above: BEST, else the user's selected qualities,
             * else fallback to everything found.
             */
            final Integer bestGaplessHeight = findBestQuality(cachedAvailableHeights);
            final List<DownloadLink> chosenGaplessVideoResults = new ArrayList<DownloadLink>();
            if (settingPreferBestVideo) {
                for (final DownloadLink gaplessVideoResult : gaplessresults) {
                    final Integer resultHeight = ORFMediathek.getVideoHeight(gaplessVideoResult);
                    if (resultHeight != null && resultHeight.equals(bestGaplessHeight)) {
                        chosenGaplessVideoResults.add(gaplessVideoResult);
                    }
                }
            } else {
                for (final DownloadLink gaplessVideoResult : gaplessresults) {
                    final Integer resultHeight = ORFMediathek.getVideoHeight(gaplessVideoResult);
                    if (resultHeight != null && selectedQualities.contains(resultHeight)) {
                        chosenGaplessVideoResults.add(gaplessVideoResult);
                    }
                }
                if (chosenGaplessVideoResults.isEmpty()) {
                    logger.info("None of the desired qualities " + selectedQualities + " were found in the gapless master -> Returning all available gapless qualities instead");
                    chosenGaplessVideoResults.addAll(gaplessresults);
                }
            }
            /* Add subtitles and thumbnail depending on users' plugin settings. */
            final ArrayList<DownloadLink> gaplessFinalResults = new ArrayList<DownloadLink>();
            for (final DownloadLink chosenVideoResult : chosenGaplessVideoResults) {
                gaplessFinalResults.add(chosenVideoResult);
                if ((cfg == null || cfg.getBooleanProperty(ORFMediathek.Q_SUBTITLES, ORFMediathek.Q_SUBTITLES_default)) && !StringUtils.isEmpty(gapless_subtitleurl)) {
                    gaplessFinalResults.add(createSubtitleLink(gapless_subtitleurl, subtitle_ext, chosenVideoResult, hosterplugin, null));
                }
            }
            if ((cfg == null || cfg.getBooleanProperty(ORFMediathek.Q_THUMBNAIL, ORFMediathek.Q_THUMBNAIL_default)) && !StringUtils.isEmpty(thumbnailurlFromFirstSegment)) {
                gaplessFinalResults.add(createThumbnailLink(thumbnailurlFromFirstSegment, hosterplugin));
            }
            if (isCrawlGaplessOnly) {
                /* Discard previously found results as we want gapless items only. */
                ret.clear();
            }
            for (final DownloadLink result : gaplessFinalResults) {
                result.setProperty(ORFMediathek.PROPERTY_TITLE, mainVideoTitle);
                result.setProperty(ORFMediathek.PROPERTY_SEGMENT_ID, "gapless");
                ret.add(result);
            }
            numberofGaplessItems = gaplessFinalResults.size();
        }
        if (isCrawlGaplessOnly && numberofGaplessItems == 0) {
            logger.info("User wants gapless only but gapless is not available for this item");
        }
        if (ret.isEmpty()) {
            /* Hm no results -> Check if item has been deleted */
            final String killdate = (String) entries.get("killdate");
            if (!StringUtils.isEmpty(killdate)) {
                final long delete_date_millis = TimeFormatter.getMilliSeconds(killdate, "yyyy-MM-dd'T'HH:mm:ssXXX", Locale.ENGLISH);
                if (delete_date_millis < System.currentTimeMillis()) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /**
         * Add more properties which are the same for all results. </br>
         * It is important that all items run through this loop!
         */
        for (final DownloadLink result : ret) {
            if (!result.hasProperty(ORFMediathek.PROPERTY_CONTENT_TYPE)) {
                continue;
            }
            result.setContentUrl(sourceurl);
            result.setProperty(ORFMediathek.PROPERTY_VIDEO_ID, contentIDSlashPlaylistIDSlashVideoID);
            result.setProperty(ORFMediathek.PROPERTY_SOURCEURL, sourceurl);
            if (has_active_youth_protection) {
                result.setProperty(ORFMediathek.PROPERTY_AGE_RESTRICTED, true);
            }
            result.setFinalFileName(ORFMediathek.getFormattedVideoFilename(result));
            result.setAvailable(true);
        }
        fp.addLinks(ret);
        return ret;
    }

    /** Returns the largest (best) of the given heights, or null if the set is empty. LOW (Q1A) is never part of this set. */
    private static Integer findBestQuality(final HashSet<Integer> availableHeights) {
        Integer best = null;
        for (final Integer height : availableHeights) {
            if (best == null || height > best) {
                best = height;
            }
        }
        return best;
    }

    /** Returns true if any of the given heights is greater than thresholdExclusive. */
    private static boolean containsHeightAbove(final List<Integer> heights, final int thresholdExclusive) {
        for (final Integer height : heights) {
            if (height != null && height > thresholdExclusive) {
                return true;
            }
        }
        return false;
    }

    /**
     * Properties that getFormattedVideoFilename/getVideoHeight actually read for a subtitle result too (subtitles are named/keyed the same
     * way as their associated video quality, just with a different extension) -> these are worth copying from the video onto its subtitle.
     * Everything else (in particular DownloadLink.PROPERTY_VERIFIEDFILESIZE, which setVerifiedFileSize() sets on the video) must NOT be
     * copied -> a blanket videoResult.getProperties() copy would otherwise make the subtitle report the video's filesize as its own.
     */
    private static final String[] SUBTITLE_INHERITED_PROPERTIES = new String[] { ORFMediathek.PROPERTY_STREAMING_TYPE, ORFMediathek.PROPERTY_DELIVERY, ORFMediathek.PROPERTY_VIDEO_HEIGHT, ORFMediathek.PROPERTY_INTERNAL_QUALITY };

    private DownloadLink createSubtitleLink(final String subtitleUrl, final String subtitleExt, final DownloadLink videoResult, final ORFMediathek hosterplugin, final String contentUrl) {
        final DownloadLink subtitle = createDownloadlink(subtitleUrl);
        subtitle.setDefaultPlugin(hosterplugin);
        subtitle.setHost(hosterplugin.getHost());
        for (final String propertyKey : SUBTITLE_INHERITED_PROPERTIES) {
            final Object value = videoResult.getProperty(propertyKey);
            if (value != null) {
                subtitle.setProperty(propertyKey, value);
            }
        }
        subtitle.setProperty(ORFMediathek.PROPERTY_DIRECTURL, subtitleUrl);
        subtitle.setProperty(ORFMediathek.PROPERTY_CONTENT_TYPE, ORFMediathek.CONTENT_TYPE_SUBTITLE);
        subtitle.setProperty(ORFMediathek.CONTENT_EXT_HINT, subtitleExt);
        subtitle.setAvailable(true);
        if (contentUrl != null) {
            subtitle.setContentUrl(contentUrl);
        }
        return subtitle;
    }

    private DownloadLink createThumbnailLink(final String thumbnailUrl, final ORFMediathek hosterplugin) {
        final DownloadLink thumbnail = this.createDownloadlink(thumbnailUrl);
        thumbnail.setDefaultPlugin(hosterplugin);
        thumbnail.setHost(hosterplugin.getHost());
        thumbnail.setProperty(ORFMediathek.PROPERTY_CONTENT_TYPE, ORFMediathek.CONTENT_TYPE_IMAGE);
        thumbnail.setProperty(ORFMediathek.PROPERTY_DIRECTURL, thumbnailUrl);
        thumbnail.setAvailable(true);
        return thumbnail;
    }

    private ArrayList<DownloadLink> crawlOrfmediathekNew(final CryptedLink param, final String contenturl) throws Exception {
        final Regex videourl = new Regex(contenturl, "(?i)https?://on\\.orf\\.at/video/(\\d+)(/([\\w\\-]+))?");
        if (!videourl.patternFind()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Unsupported URL");
        }
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String encryptedID = null;
        final String json = br.getRegex("id=\"__NUXT_DATA__\"[^>]*>(\\[[^>]+)<").getMatch(0);
        if (json != null) {
            /* New 2025-03-25 */
            final String encrypted_id_index_str = br.getRegex("\"encrypted_id\":(\\d+)").getMatch(0);
            final List<Object> objects = (List<Object>) restoreFromString(json, TypeRef.OBJECT);
            if (encrypted_id_index_str != null) {
                /* Get ID by known position */
                encryptedID = objects.get(Integer.parseInt(encrypted_id_index_str)).toString();
            } else {
                /* Fallback: Search ID by pattern */
                for (final Object object : objects) {
                    if (!(object instanceof String)) {
                        continue;
                    }
                    final String str = object.toString();
                    if (str.matches("[A-Za-z0-9]{36}")) {
                        encryptedID = str;
                        break;
                    }
                }
            }
        }
        if (encryptedID == null) {
            /* Last resort fallback */
            encryptedID = br.getRegex("\"([A-Za-z0-9]{36})\"").getMatch(0);
        }
        if (encryptedID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return crawlOrfmediathekNewEncryptedID(encryptedID, contenturl);
    }

    @Deprecated
    /** TODO: Delete this once they've fully switched to on.orf.at. */
    private ArrayList<DownloadLink> crawlOrfmediathekOld(final CryptedLink param) throws Exception {
        // final String contenturl = param.getCryptedUrl().replaceFirst("/index\\.php/", "/");
        final boolean rewriteUrlsToNewMediathek = true;
        final String videoIDFromURL = new Regex(param.getCryptedUrl(), "/profile/[^/]+/[^/]+/[^/]+/(\\d+)").getMatch(0);
        if (rewriteUrlsToNewMediathek && videoIDFromURL != null) {
            return this.crawlOrfmediathekNew(param, generateContenturlMediathek(videoIDFromURL, null));
        }
        /* Assume that we got an invalid/unsupported link */
        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
    }

    private static String generateContenturlMediathek(final String videoID, final String slug) {
        String url = "https://on.orf.at/video/" + videoID;
        if (slug != null) {
            url += "/" + slug;
        }
        return url;
    }

    /** Maps the internal (old-style) quality identifier to the height (in pixels) of the corresponding video stream. */
    public static Integer oldQualityIdentifierToHeight(final String internal_identifier) {
        if ("Q0A".equals(internal_identifier)) {
            /* VERYLOW */
            return 288;
        } else if ("Q4A".equals(internal_identifier)) {
            /* MEDIUM */
            return 360;
        } else if ("Q6A".equals(internal_identifier)) {
            /* HIGH */
            return 540;
        } else if ("Q8C".equals(internal_identifier)) {
            /* VERYHIGH */
            return 720;
        } else {
            /* No known height for Q1A (LOW) or anything else (e.g. ADAPTIV). */
            return null;
        }
    }

    private ArrayList<DownloadLink> crawlArticle(final CryptedLink param) throws Exception {
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String title = br.getRegex("\"og:title\"\\s*content\\s*=\\s*\"(.*?)\"").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        final String oon_audioEntries[] = br.getRegex("<div class=\"oon-audio\"(.*?)\\s*</div>").getColumn(0);
        if (oon_audioEntries == null || oon_audioEntries.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        for (final String oon_audioEntry : oon_audioEntries) {
            final String url = new Regex(oon_audioEntry, "data-url\\s*=\\s*\"(https?://.*?)\"").getMatch(0);
            if (url == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final DownloadLink link = createDownloadlink(url);
            link.setContentUrl(contenturl);
            String name = getFileNameFromURL(new URL(url));
            if (title != null) {
                name = title + "-" + name;
                fp.add(link);
            }
            if (name != null) {
                link.setFinalFileName(name);
            }
            link.setProperty(DirectHTTP.FIXNAME, name);
            ret.add(link);
        }
        return ret;
    }

    /** Crawls all episodes of a podcast or only a specific episode. */
    private ArrayList<DownloadLink> crawlPodcast(final CryptedLink param) throws Exception {
        final Regex urlinfo = new Regex(param.getCryptedUrl(), PATTERN_PODCAST);
        final String channelSlug = urlinfo.getMatch(0);
        final String podcastSeriesSlug = urlinfo.getMatch(1);
        final String podcastEpisodeTitleSlug = urlinfo.getMatch(3); // optional
        if (channelSlug == null || podcastSeriesSlug == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Old API call: (2022-09-27: Still working) */
        // br.getPage(API_BASE + "/radiothek/podcast/" + channelSlug + "/" + podcastSeriesSlug + ".json?_o=" + hostFromURL);
        br.getPage(API_BASE + "/radiothek/api/2.0/podcast/" + channelSlug + "/" + podcastSeriesSlug + "?episodes&_o=sound.orf.at");
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* This should never happen but for sure users could modify added URLs and render them invalid. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> payload = (Map<String, Object>) entries.get("payload");
        final String station = payload.get("station").toString();
        final String podcastSlug = payload.get("slug").toString();
        final String podcastDescription = (String) payload.get("description");
        final String author = payload.get("author").toString();
        final String podcastTitle = payload.get("title").toString();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(author + " - " + podcastTitle);
        if (!StringUtils.isEmpty(podcastDescription)) {
            fp.setComment(podcastDescription);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        boolean foundSpecificEpisode = false;
        final List<Map<String, Object>> episodes = (List<Map<String, Object>>) payload.get("episodes");
        for (final Map<String, Object> episode : episodes) {
            String directurl = null;
            final List<Map<String, Object>> enclosures = (List<Map<String, Object>>) episode.get("enclosures");
            for (final Map<String, Object> enclosure : enclosures) {
                if (enclosure.get("type").toString().equals("audio/mpeg")) {
                    directurl = enclosure.get("url").toString();
                    break;
                }
            }
            if (StringUtils.isEmpty(directurl)) {
                /* This should never happen -> Most likely unsupported streaming type */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String episodeSlug = episode.get("slug").toString();
            final DownloadLink link = createPodcastDownloadlink(directurl);
            final String episodeDateStr = episode.get("published").toString();
            final String dateFormatted = new Regex(episodeDateStr, "^(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
            final String filename = dateFormatted + "_" + author + " - " + episode.get("title").toString() + ".mp3";
            link.setFinalFileName(filename);
            link.setDownloadSize(calculateFilesize(((Number) episode.get("duration")).longValue()));
            link.setProperty(DirectHTTP.FIXNAME, filename);
            final String contentURL = (String) JavaScriptEngineFactory.walkJson(episode, "link/url");
            if (!StringUtils.isEmpty(contentURL)) {
                link.setContentUrl(contentURL);
            } else {
                /* ContentURLs are not always given in json e.g. https://sound.orf.at/podcast/tv/report-werkstatt */
                link.setContentUrl("https://sound.orf.at/podcast/" + station + "/" + podcastSlug + "/" + episodeSlug);
            }
            final String description = (String) episode.get("description");
            if (!StringUtils.isEmpty(description)) {
                link.setComment(description);
            }
            link.setAvailable(true);
            link._setFilePackage(fp);
            if (podcastEpisodeTitleSlug != null && episodeSlug.equals(podcastEpisodeTitleSlug)) {
                /* User added link to single episode and we found it --> Clear previous findings and only return that single episode. */
                ret.clear();
                ret.add(link);
                foundSpecificEpisode = true;
                break;
            } else {
                ret.add(link);
            }
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (podcastEpisodeTitleSlug != null && !foundSpecificEpisode) {
            /* Rare case */
            logger.warning("Failed to find specific episode --> Adding all instead");
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlProgramm(final String domainID, final String broadcastID, final String broadcastDay) throws Exception {
        if (broadcastID == null || broadcastDay == null || domainID == null || domainID.length() < 3) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* E.g. change "wien" to "wie": https://wien.orf.at/player/20220925/WTIT */
        final String domainIDCorrected = domainID.toLowerCase(Locale.ENGLISH).substring(0, 3);
        synchronized (CHANNEL_CACHE) {
            /* 2021-03-31 */
            if (!CHANNEL_CACHE.containsKey(domainIDCorrected)) {
                br.getPage("https://assets.orf.at/vue-storyserver/radiothek-item-player/js/app.js");
                final String channelInfoJs = br.getRegex("stations:(\\{.*?\\}\\}\\})\\},").getMatch(0);
                final Map<String, Map<String, Object>> channelInfo = (Map<String, Map<String, Object>>) JavaScriptEngineFactory.jsonToJavaObject(channelInfoJs);
                CHANNEL_CACHE.clear();
                CHANNEL_CACHE.putAll(channelInfo);
                // final String[][] shortnameToChannelNames = br.getRegex("/([^/]+)/json/" +
                // Regex.escape("4.0/broadcast{/programKey}{/broadcastDay}\")}") + ".*?,channel:\"([^\"]+)\"").getMatches();
                // for (final String[] channelInfo : shortnameToChannelNames) {
                // CHANNEL_CACHE.put(channelInfo[0], channelInfo[1]);
                // }
                if (!CHANNEL_CACHE.containsKey(domainIDCorrected)) {
                    /* Most likely invalid domainID. */
                    logger.info("Failed to find channel for: " + domainIDCorrected);
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
            }
        }
        final Map<String, Object> channelInfo = CHANNEL_CACHE.get(domainIDCorrected);
        final Map<String, Object> loopstream = (Map<String, Object>) channelInfo.get("loopstream");
        br.setAllowedResponseCodes(410);
        br.getPage(API_BASE + "/" + domainIDCorrected + "/api/json/current/broadcast/" + broadcastID + "/" + broadcastDay + "?_s=" + System.currentTimeMillis());
        final Map<String, Object> response = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        if (br.getHttpConnection().getResponseCode() == 410 || "Broadcast is no longer available".equals(response.get("message"))) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String broadcastDescription = (String) response.get("description");
        final String broadcastStartISO = response.get("startISO").toString();
        final String broadcastDateFormatted = new Regex(broadcastStartISO, "^(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        // final String broadCastDay = response.get("broadcastDay").toString();
        final String title = (String) response.get("title");
        final String programTitle = (String) response.get("programTitle");
        final List<Map<String, Object>> streams = (List<Map<String, Object>>) response.get("streams");
        String titleBase = response.get("station").toString();
        if (!StringUtils.isEmpty(programTitle)) {
            titleBase += " - " + programTitle + " - " + title;
        } else {
            titleBase += " - " + title;
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setCleanupPackageName(false);
        fp.setName(broadcastDateFormatted + "_" + titleBase);
        if (!StringUtils.isEmpty(broadcastDescription)) {
            fp.setComment(broadcastDescription);
        }
        int position = 1;
        final String userid = UUID.randomUUID().toString();
        final int padLength = StringUtils.getPadLength(streams.size());
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        for (final Map<String, Object> stream : streams) {
            final String loopStreamId = (String) stream.get("loopStreamId");
            if (loopStreamId == null) {
                continue;
            }
            final String startISO = stream.get("startISO").toString();
            final String dateFormatted = new Regex(startISO, "^(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
            final long startTimestamp = ((Number) stream.get("start")).longValue();
            final long endTimestamp = ((Number) stream.get("end")).longValue();
            final long runtimeMilliseconds = endTimestamp - startTimestamp;
            final long startOffset = ((Number) stream.get("startOffset")).longValue();
            final long endOffset = ((Number) stream.get("endOffset")).longValue();
            final long offset = startOffset - endOffset;
            final DownloadLink link = createDownloadlink("directhttp://https://" + loopstream.get("host") + "/?channel=" + loopstream.get("channel") + "&shoutcast=0&player=" + domainIDCorrected + "_v1&referer=" + domainIDCorrected + ".orf.at&_=" + System.currentTimeMillis() + "&userid=" + userid + "&id=" + loopStreamId + "&offset=" + offset + "&offsetende=" + runtimeMilliseconds);
            if (streams.size() > 1) {
                link.setFinalFileName(dateFormatted + "_" + titleBase + "_" + StringUtils.formatByPadLength(padLength, position) + ".mp3");
            } else {
                link.setFinalFileName(dateFormatted + "_" + titleBase + ".mp3");
            }
            link.setDownloadSize(this.calculateFilesize(runtimeMilliseconds));
            link.setAvailable(true);
            link.setLinkID(domainIDCorrected + ".orf.at://" + broadcastID + "/" + broadcastDay + "/" + position);
            ret.add(link);
            fp.add(link);
            position++;
        }
        return ret;
    }

    /** Crawls all items of a collection. A collection can contain single episode of various podcasts. */
    private ArrayList<DownloadLink> crawlCollection(final CryptedLink param) throws IOException, PluginException {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Regex collectionPatternRegex = new Regex(param.getCryptedUrl(), PATTERN_COLLECTION);
        if (!collectionPatternRegex.patternFind()) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String baseURL = collectionPatternRegex.getMatch(0);
        final String collectionID = collectionPatternRegex.getMatch(1);
        // final String collectionTargetItemID = collectionPatternRegex.getMatch(3);
        final String collectionURL = baseURL + "/" + collectionID;
        br.getPage("https://collector.orf.at/api/frontend/collections/" + collectionID + "?_o=sound.orf.at");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> payload = (Map<String, Object>) entries.get("payload");
        final Map<String, Object> collectionContent = (Map<String, Object>) payload.get("content");
        final String collectionTitle = collectionContent.get("title").toString();
        final String collectionDescription = (String) collectionContent.get("description");
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(collectionTitle);
        if (!StringUtils.isEmpty(collectionDescription)) {
            fp.setComment(collectionDescription);
        }
        final List<Map<String, Object>> items = (List<Map<String, Object>>) collectionContent.get("items");
        int progress = 0;
        int numberofOfflineItems = 0;
        for (final Map<String, Object> item : items) {
            progress++;
            final String collectionItemID = item.get("id").toString();
            logger.info("Crawling collection item " + progress + "/" + items.size() + " ID: " + collectionItemID);
            final Map<String, Object> collectionItemContent = (Map<String, Object>) item.get("content");
            final Map<String, Object> target = (Map<String, Object>) item.get("target");
            if ((Boolean) target.get("isGone")) {
                /* This should never happen?! */
                numberofOfflineItems++;
                continue;
            }
            final String collectionItemTitle = collectionItemContent.get("title").toString();
            final String collectionItemStation = collectionItemContent.get("station").toString();
            final String collectionItemContentURL = collectionURL + "/" + collectionItemID + "/" + toSlug(collectionItemTitle);
            final String targetType = target.get("type").toString();
            final Map<String, Object> params = (Map<String, Object>) target.get("params");
            final ArrayList<DownloadLink> thisresults = new ArrayList<DownloadLink>();
            /*
             * If offline exception happens for one item here this seems to mean that the whole collection is offline. Website will then
             * simply redirect to a random other collection.
             */
            if (targetType.equalsIgnoreCase("podcast-episode")) {
                final DownloadLink broadcast = crawlPodcastEpisodeByGUID(params.get("guid").toString());
                thisresults.add(broadcast);
            } else if (targetType.equalsIgnoreCase("broadcastitem")) {
                final DownloadLink podcast = this.crawlBroadcastItem(collectionItemStation, params.get("id").toString());
                thisresults.add(podcast);
            } else if (targetType.equalsIgnoreCase("broadcast")) {
                thisresults.addAll(this.crawlBroadcast(collectionItemStation, params.get("id").toString()));
            } else if (targetType.equalsIgnoreCase("upload")) {
                final DownloadLink upload = this.crawlUpload(params.get("id").toString());
                thisresults.add(upload);
            } else {
                logger.warning("Unsupported targetType " + targetType + " for collection item: " + collectionItemContentURL);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /*
             * Item can be linked directly or as part of a collection. In this case we want to present it to the user as part of a
             * collection as this is what he will get via browser too.
             */
            for (final DownloadLink thisresult : thisresults) {
                thisresult.setContentUrl(collectionItemContentURL);
                thisresult.setContainerUrl(collectionURL);
                thisresult.setAvailable(true);
                thisresult._setFilePackage(fp);
                distribute(thisresult);
                ret.add(thisresult);
            }
        }
        if (ret.isEmpty()) {
            /* Rare case: All items are offline? */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (numberofOfflineItems > 0) {
            logger.info("Number of skipped offline items: " + numberofOfflineItems);
        }
        return ret;
    }

    private DownloadLink crawlPodcastEpisodeByGUID(final String podcastGUID) throws IOException, PluginException {
        br.getPage(API_BASE + "/radiothek/api/2.0/episode/" + podcastGUID + "?_o=sound.orf.at");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> payload = (Map<String, Object>) resp.get("payload");
        final Map<String, Object> podcast = (Map<String, Object>) payload.get("podcast");
        final String author = podcast.get("author").toString();
        final String podcastEpisodeTitle = podcast.get("title").toString();
        String directurl = null;
        final List<Map<String, Object>> enclosures = (List<Map<String, Object>>) payload.get("enclosures");
        for (final Map<String, Object> enclosure : enclosures) {
            if (enclosure.get("type").toString().equals("audio/mpeg")) {
                directurl = enclosure.get("url").toString();
                break;
            }
        }
        if (StringUtils.isEmpty(directurl)) {
            /* Most likely unsupported streaming type */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contentURL = (String) JavaScriptEngineFactory.walkJson(payload, "link/url");
        final DownloadLink link = createPodcastDownloadlink(directurl);
        if (!StringUtils.isEmpty(contentURL)) {
            link.setContentUrl(contentURL);
        }
        link.setProperty(PROPERTY_SLUG, podcast.get("slug"));
        final String dateStr = payload.get("published").toString();
        final String dateFormatted = new Regex(dateStr, "^(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        final String filename = dateFormatted + "_" + author + " - " + podcastEpisodeTitle + " - " + payload.get("title").toString() + ".mp3";
        link.setFinalFileName(filename);
        link.setProperty(DirectHTTP.FIXNAME, filename);
        final String description = (String) payload.get("description");
        if (!StringUtils.isEmpty(description)) {
            link.setComment(description);
        }
        link.setDownloadSize(calculateFilesize(((Number) payload.get("duration")).longValue()));
        link.setAvailable(true);
        return link;
    }

    private DownloadLink crawlBroadcastItem(final String radioStation, final String broadcastID) throws IOException, PluginException {
        if (radioStation == null || broadcastID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(API_BASE + "/" + radioStation + "/api/json/5.0/broadcastitem/" + broadcastID + "?_o=sound.orf.at");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> payload = (Map<String, Object>) resp.get("payload");
        final Map<String, Object> broadcast = (Map<String, Object>) payload.get("broadcast");
        final ArrayList<Map<String, Object>> streams = new ArrayList<Map<String, Object>>();
        streams.add((Map<String, Object>) payload.get("stream"));
        final ArrayList<DownloadLink> results = crawlProcessBroadcastItems(broadcast, streams);
        return results.get(0);
    }

    private ArrayList<DownloadLink> crawlBroadcast(final String radioStation, final String broadcastID) throws IOException, PluginException {
        if (radioStation == null || broadcastID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(API_BASE + "/" + radioStation + "/api/json/5.0/broadcast/" + broadcastID + "?_o=sound.orf.at");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> broadcast = (Map<String, Object>) resp.get("payload");
        return crawlProcessBroadcastItems(broadcast, (List<Map<String, Object>>) broadcast.get("streams"));
    }

    private ArrayList<DownloadLink> crawlProcessBroadcastItems(final Map<String, Object> broadcast, final List<Map<String, Object>> streams) throws IOException, PluginException {
        final String broadcastTitle = broadcast.get("title").toString();
        // final String broadcastSubtitle = (String) broadcast.get("subtitle");
        final String broadcastDescription = (String) broadcast.get("description");
        final String broadcastStartDate = broadcast.get("start").toString();
        final String broadcastStartDateFormatted = new Regex(broadcastStartDate, "^(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        final String station = broadcast.get("station").toString();
        final String titleBase = station + " - " + broadcastTitle;
        final FilePackage fp = FilePackage.getInstance();
        final String packagenameBase = broadcastStartDateFormatted + "_" + titleBase;
        fp.setName(packagenameBase);
        if (!StringUtils.isEmpty(broadcastDescription)) {
            fp.setComment(broadcastDescription);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final int padLength = StringUtils.getPadLength(streams.size());
        int position = 1;
        for (final Map<String, Object> stream : streams) {
            final String streamStartDate = stream.get("start").toString();
            final String streamStartDateFormatted = new Regex(streamStartDate, "^(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
            String filenameBase = streamStartDateFormatted + "_" + titleBase;
            if (streams.size() > 1) {
                filenameBase += " " + StringUtils.formatByPadLength(padLength, position);
            }
            filenameBase += ".mp3";
            final Map<String, Object> urls = (Map<String, Object>) stream.get("urls");
            // final String originalFilename = stream.get("loopStreamId").toString();
            String downloadurl = urls.get("progressive").toString();
            /* Remove placeholders inside URL which we're not filling in. */
            downloadurl = downloadurl.replaceAll("\\{\\&[^\\}]+\\}", "");
            final DownloadLink link = this.createDownloadlink("directhttp://" + downloadurl);
            link.setFinalFileName(filenameBase);
            link.setProperty(DirectHTTP.FIXNAME, filenameBase);
            link.setDownloadSize(calculateFilesize(((Number) stream.get("duration")).longValue()));
            link.setAvailable(true);
            ret.add(link);
            position++;
        }
        return ret;
    }

    private DownloadLink crawlUpload(final String uploadID) throws IOException, PluginException {
        br.getPage(API_BASE + "/radiothek/api/2.0/upload/" + uploadID + "?_o=sound.orf.at");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> payload = (Map<String, Object>) resp.get("payload");
        if ((Boolean) payload.get("isOnline") == Boolean.FALSE) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String moderator = (String) payload.get("moderator");
        final String uploadTitle = payload.get("title").toString();
        String directurl = null;
        final List<Map<String, Object>> enclosures = (List<Map<String, Object>>) payload.get("enclosures");
        for (final Map<String, Object> enclosure : enclosures) {
            if (enclosure.get("type").toString().equals("audio/mpeg")) {
                directurl = enclosure.get("url").toString();
                break;
            }
        }
        if (StringUtils.isEmpty(directurl)) {
            /* Most likely unsupported streaming type */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final DownloadLink link = createPodcastDownloadlink(directurl);
        link.setProperty(PROPERTY_SLUG, payload.get("slug"));
        final String dateStr = payload.get("postDate").toString();
        final String dateFormatted = new Regex(dateStr, "^(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        String filename = dateFormatted + "_" + payload.get("station");
        if (!StringUtils.isEmpty(moderator)) {
            filename += "_ " + moderator;
        }
        filename += " - " + uploadTitle + " - " + payload.get("title").toString() + ".mp3";
        link.setFinalFileName(filename);
        link.setProperty(DirectHTTP.FIXNAME, filename);
        final String description = (String) payload.get("description");
        if (!StringUtils.isEmpty(description)) {
            link.setComment(description);
        }
        link.setDownloadSize(calculateFilesize(((Number) payload.get("duration")).longValue()));
        link.setAvailable(true);
        return link;
    }

    private ArrayList<DownloadLink> crawlVideo(final CryptedLink param) throws Exception {
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final HashSet<String> dupes = new HashSet<String>();
        final String[] programIDs = br.getRegex("data-ppid=\"([a-f0-9\\-]+)\"").getColumn(0);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        for (final String programID : programIDs) {
            if (dupes.add(programID)) {
                ret.addAll(crawlVideoProgramID(programID));
            }
        }
        return ret;
    }

    @Deprecated
    private ArrayList<DownloadLink> crawlVideoProgramID(final String programID) throws Exception {
        if (StringUtils.isEmpty(programID)) {
            throw new IllegalArgumentException();
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getHeaders().put("Origin", "https://tv.orf.at");
        br.getHeaders().put("Referer", "https://tv.orf.at/");
        br.getPage("https://api-tvthek.orf.at/api/v4.2/public/content-by-dds-programplanguid/" + programID);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> episode = (Map<String, Object>) entries.get("episode");
        if (episode == null) {
            /*
             * Item does not exist anymore or hasn't aired yet. Website may show a preview-image and a description but no streamable video
             * content.
             */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String encrypted_id = (String) episode.get("encrypted_id");
        if (!StringUtils.isEmpty(encrypted_id)) {
            /* Use new handling */
            return this.crawlOrfmediathekNewEncryptedID(encrypted_id, null);
        }
        final SubConfiguration cfg = SubConfiguration.getConfig("orf.at");
        // if(Boolean.TRUE.equals(episode.get("is_drm_protected"))) {
        //
        // }
        final String title = episode.get("title").toString();
        final String description = episode.get("description").toString();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        fp.setComment(description);
        /* There are multiple HLS sources available. Looks like mirrors. */
        final Map<String, Object> hlsmap = (Map<String, Object>) JavaScriptEngineFactory.walkJson(episode, "sources/hls/{0}");
        final String hlsMaster = hlsmap.get("src").toString();
        final Browser brc = br.cloneBrowser();
        brc.getPage(hlsMaster);
        final List<HlsContainer> hlsContainers = HlsContainer.getHlsQualities(brc);
        for (final HlsContainer hlsContainer : hlsContainers) {
            final DownloadLink video = this.createDownloadlink(hlsContainer.getDownloadurl());
            video.setFinalFileName(title + "_" + hlsContainer.getHeight() + "p.mp4");
            ret.add(video);
        }
        final String thumbnailurl = (String) episode.get("related_audiodescription_episode_image_url");
        if (thumbnailurl != null && cfg.getBooleanProperty(ORFMediathek.Q_THUMBNAIL, ORFMediathek.Q_THUMBNAIL_default)) {
            final DownloadLink thumbnail = this.createDownloadlink(thumbnailurl);
            thumbnail.setFinalFileName(title + Plugin.getFileNameExtensionFromURL(thumbnailurl));
            thumbnail.setAvailable(true);
            ret.add(thumbnail);
        }
        final Map<String, Object> subtitlemap = (Map<String, Object>) JavaScriptEngineFactory.walkJson(episode, "_embedded/subtitle");
        final String subtitleURL = subtitlemap != null ? (String) subtitlemap.get("srt_url") : null;
        if (!StringUtils.isEmpty(subtitleURL)) {
            final DownloadLink subtitle = this.createDownloadlink(subtitleURL);
            subtitle.setFinalFileName(title + ".srt");
            subtitle.setAvailable(true);
            ret.add(subtitle);
        }
        fp.addLinks(ret);
        return ret;
    }

    public static String toSlug(final String str) {
        final String preparedSlug = str.toLowerCase(Locale.ENGLISH).replace("ü", "u").replace("ä", "a").replace("ö", "o");
        String slug = preparedSlug.replaceAll("[^a-z0-9]", "-");
        /* Remove double-minus */
        slug = slug.replaceAll("-{2,}", "-");
        /* Do not begin with minus */
        if (slug.startsWith("-")) {
            slug = slug.substring(1);
        }
        /* Do not end with minus */
        if (slug.endsWith("-")) {
            slug = slug.substring(0, slug.length() - 1);
        }
        return slug;
    }

    /** Calculates filesizes on the assumption that audio is delivered with quality 192kb/s. */
    private long calculateFilesize(final long durationMilliseconds) {
        final long durationSeconds = durationMilliseconds / 1000;
        return (durationSeconds * 192 * 1024) / 8;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}
