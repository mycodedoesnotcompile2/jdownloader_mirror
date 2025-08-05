//jDownloader - Downloadmanager
//Copyright (C) 2009  JD-Team support@jdownloader.org
//
//This program is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.decrypter;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.SubConfiguration;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.hoster.DailyMotionCom;

//Decrypts embedded videos from dailymotion
@DecrypterPlugin(revision = "$Revision: 51302 $", interfaceVersion = 2, names = { "dailymotion.com" }, urls = { "https?://(?:www\\.)?(dailymotion\\.com|dai\\.ly)/.+" })
public class DailyMotionComDecrypter extends PluginForDecrypt {
    public DailyMotionComDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    private static final String TYPE_DAILY_SHORTENER = "https?://(?:www\\.)?dai\\.ly/.+";
    private static final String TYPE_PLAYLIST        = "https?://(?:www\\.)?dailymotion\\.com/playlist/[A-Za-z0-9\\-_]+(?:/\\d+)?.*?";
    private static final String TYPE_USER            = "https?://(?:www\\.)?dailymotion\\.com/(user/[A-Za-z0-9_\\-]+/\\d+|[^/]+/videos)";
    private static final String TYPE_USER_SEARCH     = "https?://(?:www\\.)?dailymotion\\.com/.*?/user/[^/]+/search/[^/]+/\\d+";
    private static final String TYPE_VIDEO           = "https?://(?:www\\.)?dailymotion\\.com/((?:embed/)?video/[^/]+|swf(?:/video)?/[^/]+)";
    /** API limits for: https://developer.dailymotion.com/api#graph-api */
    private static final short  api_limit_items      = 100;
    private static final short  api_limit_pages      = 100;
    public final static boolean defaultAllowAudio    = true;
    private static Object       ctrlLock             = new Object();

    @Override
    public boolean isProxyRotationEnabledForLinkCrawler() {
        return false;
    }

    @SuppressWarnings("deprecation")
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        String contenturl = param.getCryptedUrl().replace("embed/video/", "video/").replaceAll("\\.com/swf(/video)?/", ".com/video/").replace("http://", "https://");
        br.setFollowRedirects(true);
        DailyMotionCom.prepBrowser(this.br);
        synchronized (ctrlLock) {
            /* Login if account available */
            final Account account = AccountController.getInstance().getValidAccount(getHost());
            if (account != null) {
                final DailyMotionCom dailymotionHosterplugin = (DailyMotionCom) this.getNewPluginForHostInstance(this.getHost());
                dailymotionHosterplugin.login(account, false);
            }
            /* Login end... */
            br.getPage(contenturl);
            if (param.getCryptedUrl().matches(TYPE_DAILY_SHORTENER)) {
                /* Special case: Redirect to other URL */
                if (!this.canHandle(br.getURL())) {
                    logger.info("Redirect to unsupported URL: " + br.getURL());
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else {
                    contenturl = br.getURL();
                }
            }
            checkErrors(br);
            /* video == 'video_item', user == 'user_home' */
            String username = null;
            final Regex profileregex1 = new Regex(param.getCryptedUrl(), "(?i)https?://(?:www\\.)?dailymotion\\.com/(user/([A-Za-z0-9_\\-]+)/\\d+|([^/]+)/videos)");
            if (profileregex1.patternFind()) {
                username = profileregex1.getMatch(2);
                if (username == null) {
                    username = profileregex1.getMatch(1);
                }
            } else {
                username = new Regex(param.getCryptedUrl(), "(?i)https?://(?:www\\.)?dailymotion\\.com/([^/\\?]+)").getMatch(0);
            }
            if (contenturl.matches(TYPE_PLAYLIST)) {
                return crawlPlaylist(contenturl);
            } else if (contenturl.matches(TYPE_VIDEO)) {
                return crawlSingleVideo(param, contenturl, SubConfiguration.getConfig(this.getHost()), false);
            } else if (contenturl.matches(TYPE_USER_SEARCH)) {
                return crawlUserSearch(contenturl);
            } else if (username != null) {
                return crawlUser(username);
            } else {
                /* This should never happen. */
                logger.info("Unsupported linktype: " + contenturl);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
    }

    private void checkErrors(final Browser br) throws PluginException {
        /* 404 */
        if (br.containsHTML("(<title>Dailymotion \\– 404 Not Found</title>|url\\(/images/404_background\\.jpg)") || this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("class=\"forbidden\">Access forbidden</h3>|>You don\\'t have permission to access the requested URL") || this.br.getHttpConnection().getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    /**
     * Crawls all videos of a user. In some cases it is not possible to crawl all videos due to website- AND API limitations (both have the
     * same limits).
     */
    @SuppressWarnings({ "unchecked" })
    private ArrayList<DownloadLink> crawlUser(final String username) throws Exception {
        /*
         * 2019-01-18: The API used in decryptPlaylist can also be used to crawl all videos of a user but as long as this one is working,
         * we'll stick to that.
         */
        logger.info("Crawling user profile: " + username);
        if (username == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username);
        boolean has_more = false;
        int page = 0;
        int numberofVideos = -1;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        do {
            page++;
            final Browser brc = this.br.cloneBrowser();
            brc.setAllowedResponseCodes(400);
            brc.getPage("https://api.dailymotion.com/user/" + username + "/videos?limit=" + api_limit_items + "&page=" + page);
            if (brc.getHttpConnection().getResponseCode() == 400) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (brc.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String json = brc.getRequest().getHtmlCode();
            final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
            if (page == 1) {
                /* Init some variables. */
                numberofVideos = ((Number) entries.get("total")).intValue();
                if (numberofVideos == 0) {
                    logger.info("Profile contains 0 items");
                    throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
                }
            }
            has_more = ((Boolean) entries.get("has_more")).booleanValue();
            final List<Map<String, Object>> list = (List<Map<String, Object>>) entries.get("list");
            for (final Map<String, Object> videomap : list) {
                final String videoid = videomap.get("id").toString();
                final DownloadLink dl = this.createDownloadlink(createVideolink(videoid));
                dl._setFilePackage(fp);
                ret.add(dl);
                distribute(dl);
            }
            logger.info("Crawled page " + page + " | Items on this page: " + list.size() + " | Found total so far: " + ret.size() + "/" + numberofVideos);
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            } else if (!has_more) {
                logger.info("Stopping because: Reached last page: " + page);
                break;
            } else if (page <= api_limit_pages) {
                logger.info("Stopping because: Reached internal API max page limit: " + api_limit_pages);
                break;
            }
        } while (true);
        return ret;
    }

    private String createVideolink(final String videoID) {
        return String.format("https://www.dailymotion.com/video/%s", videoID);
    }

    private ArrayList<DownloadLink> crawlPlaylist(final String contenturl) throws Exception {
        logger.info("Crawling playlist: " + contenturl);
        final String playlist_id = new Regex(contenturl, "/playlist/([^/]+)").getMatch(0);
        if (playlist_id == null) {
            /* Developer mistake! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        prepGraphqlBrowser(br);
        final HashSet<String> dupelist = new HashSet<String>();
        final boolean parseDesiredPageOnly;
        String desiredPage = new Regex(contenturl, "playlist/[A-Za-z0-9]+_[A-Za-z0-9\\-_]+/(\\d+)").getMatch(0);
        if (desiredPage == null) {
            logger.info("Crawling all pages");
            desiredPage = "1";
            parseDesiredPageOnly = false;
        } else {
            logger.info("Only crawling desired page: " + desiredPage);
            parseDesiredPageOnly = true;
        }
        int page = Integer.parseInt(desiredPage);
        int numberofVideos = 0;
        boolean hasMore = false;
        String username = null;
        String playlistTitle = null;
        FilePackage fp = null;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        do {
            final PostRequest playlistPagination = br.createJSonPostRequest(DailyMotionCom.API_BASE_GRAPHQL, "{\"operationName\":\"DESKTOP_COLLECTION_VIDEO_QUERY\",\"variables\":{\"xid\":\"" + playlist_id + "\",\"pageCV\":" + page
                    + ",\"allowExplicit\":false},\"query\":\"fragment COLLECTION_BASE_FRAGMENT on Collection {\\n  id\\n  xid\\n  updatedAt\\n  __typename\\n}\\n\\nfragment COLLECTION_IMAGES_FRAGMENT on Collection {\\n  thumbURLx60: thumbnailURL(size: \\\"x60\\\")\\n  thumbURLx120: thumbnailURL(size: \\\"x120\\\")\\n  thumbURLx180: thumbnailURL(size: \\\"x180\\\")\\n  thumbURLx240: thumbnailURL(size: \\\"x240\\\")\\n  thumbURLx360: thumbnailURL(size: \\\"x360\\\")\\n  thumbURLx480: thumbnailURL(size: \\\"x480\\\")\\n  thumbURLx720: thumbnailURL(size: \\\"x720\\\")\\n  __typename\\n}\\n\\nfragment CHANNEL_BASE_FRAGMENT on Channel {\\n  id\\n  xid\\n  name\\n  displayName\\n  isArtist\\n  logoURL(size: \\\"x60\\\")\\n  isFollowed\\n  accountType\\n  __typename\\n}\\n\\nfragment CHANNEL_IMAGES_FRAGMENT on Channel {\\n  coverURLx375: coverURL(size: \\\"x375\\\")\\n  __typename\\n}\\n\\nfragment CHANNEL_UPDATED_FRAGMENT on Channel {\\n  isFollowed\\n  stats {\\n    views {\\n      total\\n      __typename\\n    }\\n    followers {\\n      total\\n      __typename\\n    }\\n    videos {\\n      total\\n      __typename\\n    }\\n    __typename\\n  }\\n  __typename\\n}\\n\\nfragment CHANNEL_NORMAL_FRAGMENT on Channel {\\n  ...CHANNEL_BASE_FRAGMENT\\n  ...CHANNEL_IMAGES_FRAGMENT\\n  ...CHANNEL_UPDATED_FRAGMENT\\n  __typename\\n}\\n\\nfragment ALTERNATIVE_VIDEO_BASE_FRAGMENT on Video {\\n  id\\n  xid\\n  title\\n  description\\n  thumbnail: thumbnailURL(size: \\\"x240\\\")\\n  thumbURLx60: thumbnailURL(size: \\\"x60\\\")\\n  thumbURLx120: thumbnailURL(size: \\\"x120\\\")\\n  thumbURLx240: thumbnailURL(size: \\\"x240\\\")\\n  thumbURLx360: thumbnailURL(size: \\\"x360\\\")\\n  thumbURLx480: thumbnailURL(size: \\\"x480\\\")\\n  thumbURLx720: thumbnailURL(size: \\\"x720\\\")\\n  thumbURLx1080: thumbnailURL(size: \\\"x1080\\\")\\n  bestAvailableQuality\\n  viewCount\\n  duration\\n  createdAt\\n  isInWatchLater\\n  isLiked\\n  isWatched\\n  isExplicit\\n  canDisplayAds\\n  stats {\\n    views {\\n      total\\n      __typename\\n    }\\n    __typename\\n  }\\n  __typename\\n}\\n\\nfragment COLLECTION_UPDATED_FRAGMENT on Collection {\\n  name\\n  description\\n  stats {\\n    videos {\\n      total\\n      __typename\\n    }\\n    __typename\\n  }\\n  videos(first: 15, page: $pageCV, allowExplicit: $allowExplicit) {\\n    pageInfo {\\n      hasNextPage\\n      nextPage\\n      __typename\\n    }\\n    edges {\\n      node {\\n        __typename\\n        ...ALTERNATIVE_VIDEO_BASE_FRAGMENT\\n        channel {\\n          ...CHANNEL_BASE_FRAGMENT\\n          __typename\\n        }\\n      }\\n      __typename\\n    }\\n    __typename\\n  }\\n  __typename\\n}\\n\\nfragment COLLECTION_FRAGMENT on Collection {\\n  ...COLLECTION_BASE_FRAGMENT\\n  ...COLLECTION_UPDATED_FRAGMENT\\n  ...COLLECTION_IMAGES_FRAGMENT\\n  channel {\\n    ...CHANNEL_NORMAL_FRAGMENT\\n    __typename\\n  }\\n  __typename\\n}\\n\\nquery DESKTOP_COLLECTION_VIDEO_QUERY($xid: String!, $pageCV: Int!, $allowExplicit: Boolean) {\\n  collection(xid: $xid) {\\n    ...COLLECTION_FRAGMENT\\n    __typename\\n  }\\n}\\n\"}");
            br.openRequestConnection(playlistPagination);
            br.loadConnection(null);
            Map<String, Object> entries = JavaScriptEngineFactory.jsonToJavaMap(br.getRequest().getHtmlCode());
            entries = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "data/collection");
            if (page == 1) {
                /* Init some variables. */
                numberofVideos = (int) JavaScriptEngineFactory.toLong(JavaScriptEngineFactory.walkJson(entries, "stats/videos/total"), 0);
                if (numberofVideos == 0) {
                    logger.info("Playlist contains 0 items");
                    throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
                }
                username = (String) JavaScriptEngineFactory.walkJson(entries, "channel/name");
                playlistTitle = (String) entries.get("name");
                fp = FilePackage.getInstance();
                fp.setAllowInheritance(true);
                if (!StringUtils.isEmpty(username) && !StringUtils.isEmpty(playlistTitle)) {
                    fp.setName(username + " - " + playlistTitle);
                } else {
                    fp.setName(playlist_id);
                }
            }
            hasMore = ((Boolean) JavaScriptEngineFactory.walkJson(entries, "videos/pageInfo/hasNextPage")).booleanValue();
            final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(entries, "videos/edges");
            if (ressourcelist == null || ressourcelist.size() == 0) {
                logger.info("Stopping: Found nothing on page: " + page);
                break;
            }
            int numberofNewItems = 0;
            for (final Map<String, Object> videomap : ressourcelist) {
                final Map<String, Object> node = (Map<String, Object>) videomap.get("node");
                final String videoid = (String) node.get("xid");
                if (!dupelist.add(videoid)) {
                    logger.info("Skipping dupe: " + videoid);
                    continue;
                }
                final DownloadLink fina = createDownloadlink(createVideolink(videoid));
                fina._setFilePackage(fp);
                distribute(fina);
                ret.add(fina);
            }
            logger.info("Crawled page " + page + " | Found items on page: " + ressourcelist.size() + " | Found items total: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            } else if (numberofNewItems == 0) {
                logger.info("Stopping because: Failed to find any new items on page: " + page);
                break;
            } else if (parseDesiredPageOnly) {
                logger.info("Stopping because: Crawled desired page: " + page);
                break;
            } else if (!hasMore) {
                logger.info("Stopping because: Reached last page: " + page);
                break;
            }
            page++;
        } while (!this.isAbort());
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    private void prepGraphqlBrowser(final Browser brg) throws Exception {
        final String traffic_segment = brg.getRegex("window\\.__TS__ = (\\d+)").getMatch(0);
        final String client_id = br.getRegex("var r=\"([a-f0-9]{20})").getMatch(0);
        final String client_secret = br.getRegex("o=\"([a-f0-9]{20,})").getMatch(0);
        String visitor_id = br.getRegex("2v1st%22%3A%22([a-f0-9\\-]+)%22%").getMatch(0);
        if (visitor_id == null) {
            visitor_id = brg.getCookie(brg.getHost(), "v1st", Cookies.NOTDELETEDPATTERN);
        }
        if (StringUtils.isEmpty(traffic_segment) || StringUtils.isEmpty(client_id) || StringUtils.isEmpty(client_secret) || StringUtils.isEmpty(visitor_id)) {
            throw new DecrypterException();
        }
        final String auth_url = DailyMotionCom.API_BASE_GRAPHQL + "oauth/token";
        // final String client_scope = (String) entries.get("client_scope");
        // final String product_scope = (String) entries.get("product_scope");
        final UrlQuery query = new UrlQuery();
        query.add("client_id", client_id);
        query.add("client_secret", client_secret);
        query.add("grant_type", "client_credentials");
        query.add("traffic_segment", traffic_segment);
        query.add("visitor_id", visitor_id);
        brg.postPage(auth_url, query);
        final Map<String, Object> entries = restoreFromString(brg.getRequest().getHtmlCode(), TypeRef.MAP);
        final String access_token = (String) entries.get("access_token");
        // final String expires_in = entries.get("expires_in");
        if (StringUtils.isEmpty(access_token)) {
            logger.warning("Failed to find access_token");
            throw new DecrypterException();
        }
        brg.setCookie(brg.getHost(), "client_token", access_token);
        brg.getHeaders().put("authorization", "Bearer " + access_token);
        brg.getHeaders().put("x-dm-appinfo-id", "com.dailymotion.neon");
        brg.getHeaders().put("x-dm-appinfo-type", "website");
        brg.getHeaders().put("x-dm-appinfo-version", "v2019-01-10T13:08:47.423Z");
        brg.getHeaders().put("x-dm-neon-ssr", "0");
        brg.getHeaders().put("x-dm-preferred-country", "de");
        brg.getHeaders().put("accept", "*/*, */*");
        brg.getHeaders().put("accept-language", "de-DE");
        brg.getHeaders().put("origin", "https://www.dailymotion.com");
        brg.getHeaders().put("accept-encoding", "gzip, deflate, br");
        brg.getHeaders().put("content-type", "application/json, application/json");
    }

    private ArrayList<DownloadLink> crawlUserSearch(final String contenturl) throws Exception {
        int pagesNum = 1;
        final String[] page_strs = this.br.getRegex("class=\"foreground2 inverted-link-on-hvr\"> ?(\\d+)</a>").getColumn(0);
        if (page_strs != null) {
            for (final String page_str : page_strs) {
                final int page_int = Integer.parseInt(page_str);
                if (page_int > pagesNum) {
                    pagesNum = page_int;
                }
            }
        }
        final String main_search_url = new Regex(contenturl, "(.+/)\\d+$").getMatch(0);
        final String username = new Regex(contenturl, "/user/([^/]+)/").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username);
        String desiredPage = new Regex(contenturl, "(\\d+)$").getMatch(0);
        if (desiredPage == null) {
            desiredPage = "1";
        }
        boolean parseDesiredPageOnly = false;
        if (Integer.parseInt(desiredPage) != 1) {
            parseDesiredPageOnly = true;
        }
        int currentPage = Integer.parseInt(desiredPage);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        do {
            logger.info("Decrypting page " + currentPage + " / " + pagesNum);
            br.getPage(main_search_url + currentPage);
            final String[] videos = br.getRegex("<a href=\"(/video/[^<>\"]*?)\" class=\"link\"").getColumn(0);
            if (videos == null || videos.length == 0) {
                logger.info("Found no videos on page " + currentPage + " -> Stopping decryption");
                break;
            }
            for (final String videolink : videos) {
                final DownloadLink fina = createDownloadlink(br.getURL(videolink).toString());
                fp.add(fina);
                distribute(fina);
                ret.add(fina);
            }
            logger.info("Crawled page " + currentPage + "/" + pagesNum + " | Items on current page:" + videos.length + " | Found items so far: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            } else if (parseDesiredPageOnly) {
                logger.info("Stopping because: Only one specific page was to be crawled: " + currentPage);
                break;
            } else if (currentPage >= pagesNum) {
                logger.info("Stopping because: Reached last page: " + pagesNum);
                break;
            } else {
                /* Continue to next page */
                currentPage++;
            }
        } while (true);
        if (ret.isEmpty()) {
            logger.info("Found nothing - user probably entered invalid search term(s)");
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        return ret;
    }

    /**
     * 2019-01-18: psp: Issues with http URLs - seems like http urls are not valid anymore/at the moment. Via browser they work sometimes
     * but really slow/often run into timeouts --> I auto-reset settings, disabled http downloads by default and preferred HLS!
     */
    @SuppressWarnings("deprecation")
    public ArrayList<DownloadLink> crawlSingleVideo(final CryptedLink param, final String contenturl, final SubConfiguration cfg, final boolean accessContenturl) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        boolean grab_subtitle = cfg != null ? cfg.getBooleanProperty(DailyMotionCom.ALLOW_SUBTITLE, DailyMotionCom.default_ALLOW_SUBTITLE) : true;
        logger.info("Decrypting single video: " + contenturl);
        if (accessContenturl) {
            br.getPage(contenturl);
            this.checkErrors(br);
        }
        // We can't download live streams
        if (br.containsHTML("DMSTREAMMODE=live")) {
            throw new DecrypterRetryException(RetryReason.UNSUPPORTED_LIVESTREAM);
        }
        final String actualVideoURL = br.getURL();
        final String videoID = new Regex(contenturl, "(?i)dailymotion\\.com/video/([a-z0-9]+)").getMatch(0);
        if (videoID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String channelName = null;
        String passCode = null;
        final String idForGrabbingVideosource;
        if (br.containsHTML("(?i)<title>\\s*Password Protected Video")) {
            final Browser gbr = this.br.cloneBrowser();
            this.prepGraphqlBrowser(gbr);
            passCode = getUserInput("Password?", param);
            gbr.postPageRaw(DailyMotionCom.API_BASE_GRAPHQL, "{\"query\":\"query playerPasswordQuery($videoId:String!,$password:String!){video(xid:$videoId,password:$password){id xid}}\",\"variables\":{\"videoId\":\"" + videoID + "\",\"password\":\"" + passCode + "\"}}");
            final Map<String, Object> pwResponse = restoreFromString(gbr.toString(), TypeRef.MAP);
            if (pwResponse.containsKey("errors")) {
                /*
                 * E.g. {"data": {"video": null}, "errors": [{"message":
                 * "This user does not have access to the video. A password must be provided.", "path": ["video"], "locations": [{"line": 1,
                 * "column": 63}], "type": "not_authorized", "reason": "video_password_protected"}]}
                 */
                throw new DecrypterException(DecrypterException.PASSWORD);
            }
            // final String id = (String) JavaScriptEngineFactory.walkJson(pwResponse, "data/video/id");
            /* Special videoID is created after entering correct password. */
            idForGrabbingVideosource = (String) JavaScriptEngineFactory.walkJson(pwResponse, "data/video/xid");
            if (StringUtils.isEmpty(idForGrabbingVideosource)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } else {
            channelName = br.getRegex("\"owner\":\"([^<>\"]*?)\"").getMatch(0);
            idForGrabbingVideosource = videoID;
        }
        final Browser brc = br.cloneBrowser();
        brc.setFollowRedirects(true);
        final UrlQuery query = new UrlQuery();
        query.add("embedder", Encoding.urlEncode(actualVideoURL));
        query.add("locale", "");
        final String visitor_id = brc.getCookie(brc.getHost(), "v1st", Cookies.NOTDELETEDPATTERN);
        if (visitor_id != null) {
            query.add("dmV1st", Encoding.urlEncode(visitor_id));
        }
        final String ts = br.getCookie(br.getHost(), "ts", Cookies.NOTDELETEDPATTERN);
        if (ts != null) {
            query.add("dmTs", ts);
        }
        query.add("is_native_app", "0");
        query.add("app", "com.dailymotion.neon");
        query.add("client_type", "website");
        query.add("section_type", "player");
        query.add("component_style", "_");
        brc.getPage("https://www.dailymotion.com/player/metadata/video/" + idForGrabbingVideosource + "?" + query.toString());
        /* Collect more video metadata */
        Long date = null;
        String dateFormatted = null;
        final Map<String, Object> map = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        if (channelName == null) {
            channelName = (String) JavaScriptEngineFactory.walkJson(map, "metadata/owner/username");
            if (channelName == null) {
                channelName = (String) JavaScriptEngineFactory.walkJson(map, "owner/username");
                if (channelName == null) {
                    /* 2021-06-17 */
                    channelName = (String) JavaScriptEngineFactory.walkJson(map, "owner/screenname");
                }
            }
        }
        final Number created_time = (Number) map.get("created_time");
        if (created_time != null) {
            dateFormatted = new SimpleDateFormat("yyyy-MM-ddHH:mm:ssz", Locale.ENGLISH).format(new Date(created_time.intValue() * 1000l));
            date = created_time.longValue() * 1000;
        }
        final String title = (String) map.get("title");
        final Map<String, Object> errormap = (Map<String, Object>) map.get("error");
        if (errormap != null) {
            final String errorTitle = StringUtils.valueOfOrNull(errormap.get("title"));
            final Number statusCode = (Number) errormap.get("status_code");
            final Object code = errormap.get("code");
            if ("Video geo-restricted by the owner.".equals(errorTitle) || "DM007".equals(code)) {
                // status_code=403
                throw new DecrypterRetryException(RetryReason.GEO, "Geo-Restricted by owner - " + title + ".mp4");
            } else {
                throw new DecrypterRetryException(RetryReason.FILE_NOT_FOUND, "Error:" + errorTitle + "_" + title + ".mp4");
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        final boolean useNewHandling = true;
        if (useNewHandling) {
            // TODO: Add subtitle support
            final boolean best = cfg != null ? cfg.getBooleanProperty(DailyMotionCom.ALLOW_BEST, false) : false;
            final List<Integer> selectedQualities = new ArrayList<Integer>();
            if (cfg == null || cfg.getBooleanProperty(DailyMotionCom.ALLOW_240, DailyMotionCom.default_ALLOW_RESOLUTION)) {
                selectedQualities.add(240);
            }
            if (cfg == null || cfg.getBooleanProperty(DailyMotionCom.ALLOW_380, DailyMotionCom.default_ALLOW_RESOLUTION)) {
                selectedQualities.add(380);
            }
            if (cfg == null || cfg.getBooleanProperty(DailyMotionCom.ALLOW_480, DailyMotionCom.default_ALLOW_RESOLUTION)) {
                selectedQualities.add(480);
            }
            if (cfg == null || cfg.getBooleanProperty(DailyMotionCom.ALLOW_720, DailyMotionCom.default_ALLOW_RESOLUTION)) {
                selectedQualities.add(720);
            }
            if (cfg == null || cfg.getBooleanProperty(DailyMotionCom.ALLOW_1080, DailyMotionCom.default_ALLOW_RESOLUTION)) {
                selectedQualities.add(1080);
            }
            if (cfg == null || cfg.getBooleanProperty(DailyMotionCom.ALLOW_1440, DailyMotionCom.default_ALLOW_RESOLUTION)) {
                selectedQualities.add(1440);
            }
            if (cfg == null || cfg.getBooleanProperty(DailyMotionCom.ALLOW_2160, DailyMotionCom.default_ALLOW_RESOLUTION)) {
                selectedQualities.add(2160);
            }
            if (selectedQualities.isEmpty() && !best) {
                throw new DecrypterRetryException(RetryReason.PLUGIN_SETTINGS, "USER_DISABLED_ALL_QUALITIES");
            }
            final Map<String, Object> qualitiesmap = (Map<String, Object>) map.get("qualities");
            String hlsMaster = null;
            final Map<String, Object> qualityAuto = (Map<String, Object>) JavaScriptEngineFactory.walkJson(qualitiesmap, "auto/{0}");
            final String autoType = qualityAuto.get("type").toString();
            if (autoType.equals("application/x-mpegURL")) {
                hlsMaster = qualityAuto.get("url").toString();
            }
            if (StringUtils.isEmpty(hlsMaster)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            brc.setFollowRedirects(true);
            brc.getPage(hlsMaster);
            final ArrayList<DownloadLink> selectedFoundQualities = new ArrayList<DownloadLink>();
            final List<HlsContainer> hlsqualities = HlsContainer.getHlsQualities(brc);
            DownloadLink bestQuality = null;
            int highestHeight = -1;
            for (final HlsContainer hlsquality : hlsqualities) {
                final int height = hlsquality.getHeight();
                // if (height <= 0) {
                // continue;
                // }
                final DownloadLink dl = createDownloadlink("https://dailymotion.com/video/" + videoID);
                dl.setProperty(DailyMotionCom.PROPERTY_TYPE, DailyMotionCom.TYPE_VIDEO);
                dl.setProperty(DailyMotionCom.PROPERTY_HLS_MASTER, hlsMaster);
                dl.setProperty(DailyMotionCom.PROPERTY_DIRECTURL, hlsquality.getDownloadurl());
                dl.setProperty(DailyMotionCom.PROPERTY_QUALITY_NAME, hlsquality.getResolution());
                dl.setProperty(DailyMotionCom.PROPERTY_QUALITY_HEIGHT, height);
                dl.setProperty(DailyMotionCom.PROPERTY_CONTENT_URL, contenturl);
                dl.setProperty(DailyMotionCom.PROPERTY_TITLE, title);
                dl.setProperty("plain_ext", ".mp4");
                dl.setProperty(DailyMotionCom.PROPERTY_VIDEO_ID, videoID);
                final String formattedFilename = DailyMotionCom.getFormattedFilename(dl);
                dl.setName(formattedFilename);
                dl.setAvailable(true);
                dl.setContentUrl(contenturl);
                if (bestQuality == null || height > highestHeight) {
                    bestQuality = dl;
                    highestHeight = height;
                }
                if (selectedQualities.contains(height)) {
                    selectedFoundQualities.add(dl);
                }
            }
            if (best) {
                ret.add(bestQuality);
            } else if (selectedFoundQualities.isEmpty()) {
                logger.info("Fallback to BEST video quality because none of users selected qualities were found");
                ret.add(bestQuality);
            } else {
                ret.addAll(selectedFoundQualities);
            }
        } else {
            final String videoSource = brc.getRequest().getHtmlCode();
            /** Crawl subtitles if available and user wants to have it */
            String subsource = new Regex(videoSource, "\"recorded\",(.*?\\}\\})").getMatch(0);
            if (subsource != null && grab_subtitle && !true) {
                // TODO: Fix this
                subsource = subsource.replace("\\/", "/");
                final String[] subtitles = new Regex(subsource, "\"(https?://static\\d+(-ssl)?\\.dmcdn\\.net/static/video/\\d+/\\d+/\\d+:subtitle_[a-z]{1,4}\\.srt(?:\\?\\d+)?)\"").getColumn(0);
                if (subtitles != null && subtitles.length != 0) {
                    final FilePackage fpSub = FilePackage.getInstance();
                    fpSub.setName(title + "_Subtitles");
                    for (final String subtitle : subtitles) {
                        final DownloadLink dl = createDownloadlink(br.getURL("//dailymotion.com/video/" + videoID).toString());
                        dl.setContentUrl(contenturl);
                        final String language = new Regex(subtitle, ".*?\\d+:subtitle_(.{1,4}).srt.*?").getMatch(0);
                        String qualityname = "subtitle";
                        if (language != null) {
                            qualityname += "_" + language;
                        }
                        dl.setProperty("directlink", subtitle);
                        dl.setProperty("type_subtitle", true);
                        dl.setProperty("qualityname", qualityname);
                        dl.setProperty("mainlink", contenturl);
                        dl.setProperty("plain_videoname", title);
                        dl.setProperty("plain_ext", ".srt");
                        dl.setProperty("plain_videoid", videoID);
                        dl.setLinkID("dailymotioncom" + videoID + "_" + qualityname);
                        final String formattedFilename = DailyMotionCom.getFormattedFilename(dl);
                        dl.setName(formattedFilename);
                        fpSub.add(dl);
                        ret.add(dl);
                    }
                }
            }
            LinkedHashMap<String, String[]> QUALITIES = new LinkedHashMap<String, String[]>();
            final String[][] qualities = { { "hd1080URL", "5" }, { "hd720URL", "4" }, { "hqURL", "3" }, { "sdURL", "2" }, { "ldURL", "1" }, { "video_url", "6" } };
            for (final String quality[] : qualities) {
                final String qualityName = quality[0];
                final String qualityNumber = quality[1];
                final String currentQualityUrl = PluginJSonUtils.getJsonValue(videoSource, qualityName);
                if (currentQualityUrl != null) {
                    final String[] dlinfo = new String[4];
                    dlinfo[0] = currentQualityUrl;
                    dlinfo[1] = null;
                    dlinfo[2] = qualityName;
                    dlinfo[3] = qualityNumber;
                    QUALITIES.put(qualityNumber, dlinfo);
                }
            }
            QUALITIES = findVideoQualities(this, br, contenturl, videoSource);
            if (QUALITIES.isEmpty() && ret.size() == 0) {
                logger.warning("Found no quality for link: " + contenturl);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /** Find videolinks END */
            /** Pick qualities, selected by the user START */
            final ArrayList<String> selectedQualities = new ArrayList<String>();
            final boolean best = cfg != null ? cfg.getBooleanProperty(DailyMotionCom.ALLOW_BEST, false) : false;
            int numberOfSelectedQualities = 0;
            for (final String quality : new String[] { "7", "6", "5", "4", "3", "2", "1", "0" }) {
                if (cfg != null ? cfg.getBooleanProperty("ALLOW_" + quality, true) : true) {
                    numberOfSelectedQualities++;
                }
            }
            for (final String quality : new String[] { "7", "6", "5", "4", "3", "2", "1", "0" }) {
                if (selectedQualities.size() > 0 && best) {
                    break;
                }
                for (String foundQuality : QUALITIES.keySet()) {
                    if (foundQuality.startsWith(quality) && (best || numberOfSelectedQualities == 0 || cfg == null || cfg.getBooleanProperty("ALLOW_" + quality, true))) {
                        selectedQualities.add(foundQuality);
                    }
                }
            }
            for (final String selectedQuality : selectedQualities) {
                final DownloadLink dl = setVideoDownloadlink(QUALITIES, videoID, selectedQuality, title, contenturl);
                if (dl == null) {
                    continue;
                }
                dl.setContentUrl(contenturl);
                ret.add(dl);
            }
        }
        /** Pick qualities, selected by the user END */
        if (ret.size() == 0) {
            logger.info("None of the selected qualities were found, decrypting done...");
            return ret;
        }
        /* Set some additional properties */
        for (final DownloadLink dl : ret) {
            dl.setContentUrl(contenturl);
            if (passCode != null) {
                dl.setPasswordProtected(true);
                dl.setDownloadPassword(passCode);
            }
            dl.setProperty(DailyMotionCom.PROPERTY_CHANNEL, channelName);
            dl.setProperty(DailyMotionCom.PROPERTY_DATE_TIMESTAMP, date);
            dl._setFilePackage(fp);
        }
        return ret;
    }

    @SuppressWarnings("unchecked")
    public static LinkedHashMap<String, String[]> findVideoQualities(final Plugin plugin, final Browser br, final String parameter, String videosource) throws Exception {
        final LinkedHashMap<String, String[]> QUALITIES = new LinkedHashMap<String, String[]>();
        final String[][] qualities = { { "hd1080URL", "5" }, { "hd720URL", "4" }, { "hqURL", "3" }, { "sdURL", "2" }, { "ldURL", "1" }, { "video_url", "6" } };
        for (final String quality[] : qualities) {
            final String qualityName = quality[0];
            final String qualityNumber = quality[1];
            final String currentQualityUrl = PluginJSonUtils.getJsonValue(videosource, qualityName);
            if (currentQualityUrl != null) {
                final String[] dlinfo = new String[4];
                dlinfo[0] = currentQualityUrl;
                dlinfo[1] = null;
                dlinfo[2] = qualityName;
                dlinfo[3] = qualityNumber;
                QUALITIES.put(qualityNumber, dlinfo);
            }
        }
        if (QUALITIES.isEmpty() && (videosource.startsWith("{\"context\"") || videosource.contains("\"qualities\""))) {
            /* "New" player July 2015 */
            try {
                final Map<String, Object> map = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(videosource);
                Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.walkJson(map, "metadata/qualities");
                if (entries == null) {
                    entries = (Map<String, Object>) JavaScriptEngineFactory.walkJson(map, "qualities");
                }
                /* TODO: Maybe add HLS support in case it gives us more/other formats/qualities */
                final String[][] qualities_2 = { { "2160@60", "7" }, { "2160", "7" }, { "1440@60", "6" }, { "1440", "6" }, { "1080@60", "5" }, { "1080", "5" }, { "720@60", "4" }, { "720", "4" }, { "480", "3" }, { "380", "2" }, { "240", "1" }, { "144", "0" }, { "auto", "auto" } };
                for (final String quality[] : qualities_2) {
                    final String qualityName = quality[0];
                    final String qualityNumber = quality[1];
                    final Object jsono = entries.get(qualityName);
                    if (jsono != null) {
                        for (int i = 0; i < ((List) jsono).size(); i++) {
                            final String currentQualityType = (String) JavaScriptEngineFactory.walkJson(jsono, "{" + i + "}/type");
                            final String currentQualityUrl = (String) JavaScriptEngineFactory.walkJson(jsono, "{" + i + "}/url");
                            if (currentQualityUrl != null) {
                                final String[] dlinfo = new String[4];
                                dlinfo[0] = currentQualityUrl;
                                dlinfo[1] = null;
                                dlinfo[2] = qualityName;
                                dlinfo[3] = qualityNumber;
                                if (StringUtils.equalsIgnoreCase("application/x-mpegURL", currentQualityType)) {
                                    if (StringUtils.equalsIgnoreCase(dlinfo[3], "auto")) {
                                        try {
                                            // TODO: split auto HLS into multiple entries
                                            final Browser brc = br.cloneBrowser();
                                            brc.setFollowRedirects(true);
                                            brc.getPage(currentQualityUrl);
                                            final HlsContainer hlsBest = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(brc));
                                            if (hlsBest.getHeight() > 1440) {
                                                dlinfo[3] = "7";
                                            } else if (hlsBest.getHeight() > 1080) {
                                                dlinfo[3] = "6";
                                            } else if (hlsBest.getHeight() > 720) {
                                                dlinfo[3] = "5";
                                            } else if (hlsBest.getHeight() > 480) {
                                                dlinfo[3] = "4";
                                            } else if (hlsBest.getHeight() > 380) {
                                                dlinfo[3] = "3";
                                            } else if (hlsBest.getHeight() > 240) {
                                                dlinfo[3] = "2";
                                            } else if (hlsBest.getHeight() > 144) {
                                                dlinfo[3] = "1";
                                            }
                                        } catch (Exception e) {
                                            plugin.getLogger().log(e);
                                        }
                                    }
                                    QUALITIES.put(dlinfo[3] + "_HLS", dlinfo);
                                } else if (StringUtils.equalsIgnoreCase("video/mp4", currentQualityType)) {
                                    QUALITIES.put(dlinfo[3] + "_MP4", dlinfo);
                                } else {
                                    QUALITIES.put(dlinfo[3], dlinfo);
                                }
                            }
                        }
                    }
                }
            } catch (final Throwable e) {
                plugin.getLogger().log(e);
            }
        }
        // List empty or only 1 link found -> Check for (more) links
        if (QUALITIES.isEmpty() || QUALITIES.size() == 1) {
            final String manifestURL = PluginJSonUtils.getJsonValue(videosource, "autoURL");
            if (manifestURL != null) {
                /** HDS */
                final String[] dlinfo = new String[4];
                dlinfo[0] = manifestURL;
                dlinfo[1] = "hds";
                dlinfo[2] = "autoURL";
                dlinfo[3] = "8";
                QUALITIES.put("auto", dlinfo);
            }
            // Try to avoid HDS
            br.getPage("https://www.dailymotion.com/embed/video/" + new Regex(parameter, "([A-Za-z0-9\\-_]+)$").getMatch(0));
            // 19.09.2014
            videosource = br.getRegex("(\"stream_.*)\"swf_url\":").getMatch(0);
            if (videosource == null) {
                // old version. did not work for me today (19.09.2014)
                videosource = br.getRegex("var info = \\{(.*?)\\},").getMatch(0);
            }
            if (videosource != null) {
                videosource = Encoding.htmlDecode(videosource).replace("\\", "");
                final String[][] embedQualities = { { "stream_h264_ld_url", "5" }, { "stream_h264_url", "4" }, { "stream_h264_hq_url", "3" }, { "stream_h264_hd_url", "2" }, { "stream_h264_hd1080_url", "1" } };
                for (final String quality[] : embedQualities) {
                    final String qualityName = quality[0];
                    final String qualityNumber = quality[1];
                    final String currentQualityUrl = PluginJSonUtils.getJsonValue(videosource, qualityName);
                    if (currentQualityUrl != null) {
                        final String[] dlinfo = new String[4];
                        dlinfo[0] = currentQualityUrl;
                        dlinfo[1] = null;
                        dlinfo[2] = qualityName;
                        dlinfo[3] = qualityNumber;
                        QUALITIES.put(qualityNumber, dlinfo);
                    }
                }
            }
        }
        return QUALITIES;
    }

    /* Sync the following functions in hoster- and decrypterplugin */
    @Deprecated
    public static String getVideosource(final Plugin plugin, final Browser br, final String videoID) throws Exception {
        if (videoID != null) {
            final Browser brc = br.cloneBrowser();
            brc.setFollowRedirects(true);
            brc.getPage("https://www.dailymotion.com/player/metadata/video/" + videoID + "?integration=inline&GK_PV5_NEON=1");
            if (brc.getHttpConnection().isOK() && StringUtils.containsIgnoreCase(brc.getHttpConnection().getContentType(), "json")) {
                return brc.toString();
            } else {
                brc.setRequest(null);
                brc.getPage("https://www.dailymotion.com/embed/video/" + videoID);
                final String config = brc.getRegex("var\\s*config\\s*=\\s*(\\{.*?};)\\s*window").getMatch(0);
                return config;
            }
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    @Deprecated
    private DownloadLink setVideoDownloadlink(final LinkedHashMap<String, String[]> foundqualities, final String videoID, final String qualityValue, final String title, final String contenturl) throws ParseException {
        String directlinkinfo[] = foundqualities.get(qualityValue);
        if (directlinkinfo != null) {
            final String directlink = Encoding.htmlDecode(directlinkinfo[0]);
            final DownloadLink dl = createDownloadlink("https://dailymotion.com/video/" + videoID);
            String qualityName = directlinkinfo[1]; // qualityName is dlinfo[2]
            if (qualityName == null) {
                /* For hls urls */
                if (directlink.matches(".+/manifest/.+\\.m3u8.+include=\\d+")) {
                    qualityName = new Regex(directlink, "include=(\\d+)").getMatch(0);
                    if (qualityName.equals("240")) {
                        qualityName = "320x240";
                    } else if (qualityName.equals("380")) {
                        qualityName = "640X380";
                    } else if (qualityName.equals("480")) {
                        qualityName = "640X480";
                    } else if (qualityName.equals("720")) {
                        qualityName = "1280x720";
                    } else {
                        /* TODO / leave that untouched */
                    }
                } else {
                    /* For http urls mostly */
                    // for example H264-320x240
                    qualityName = new Regex(directlink, "cdn/([^<>\"]*?)/video").getMatch(0);
                    /* 2016-10-18: Added "manifest" handling for hls urls. */
                    if (qualityName == null) {
                        // statically set it... better than nothing.
                        if ("1".equalsIgnoreCase(qualityValue)) {
                            qualityName = "H264-1920x1080";
                        } else if ("2".equalsIgnoreCase(qualityValue)) {
                            qualityName = "H264-1280x720";
                        } else if ("3".equalsIgnoreCase(qualityValue)) {
                            qualityName = "H264-848x480";
                        } else if ("4".equalsIgnoreCase(qualityValue)) {
                            qualityName = "H264-512x384";
                        } else if ("5".equalsIgnoreCase(qualityValue)) {
                            qualityName = "H264-320x240";
                        }
                    }
                }
            }
            final String qualityNumber = directlinkinfo[3];
            dl.setProperty("directlink", directlink);
            dl.setProperty("qualityvalue", qualityValue);
            dl.setProperty("qualityname", qualityName);
            dl.setProperty("qualitynumber", qualityNumber);
            dl.setProperty("mainlink", contenturl);
            dl.setProperty("plain_videoname", title);
            dl.setProperty("plain_ext", ".mp4");
            dl.setProperty("plain_videoid", videoID);
            dl.setLinkID("dailymotioncom" + videoID + "_" + qualityName);
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                System.out.println("Linkid: " + "dailymotioncom" + videoID + "_" + qualityName);
            }
            final String formattedFilename = DailyMotionCom.getFormattedFilename(dl);
            dl.setName(formattedFilename);
            dl.setContentUrl(contenturl);
            dl.setAvailable(true);
            logger.info("Creating: " + directlinkinfo[2] + "/" + qualityName + " link");
            logger.info(directlink);
            return dl;
        } else {
            return null;
        }
    }

    /* NO OVERRIDE!! */
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}