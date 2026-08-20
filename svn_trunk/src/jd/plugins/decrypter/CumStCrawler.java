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

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Files;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.config.CumStConfig;
import org.jdownloader.plugins.components.config.CumStConfig.TextCrawlMode;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.controlling.linkcrawler.CrawledLink;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.http.requests.GetRequest;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.CumSt;

@DecrypterPlugin(revision = "$Revision: 53174 $", interfaceVersion = 3, names = {}, urls = {})
public class CumStCrawler extends PluginForDecrypt {
    public CumStCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public void init() {
        for (String host : siteSupportedNames()) {
            Browser.setRequestIntervalLimitGlobal(host, false, 1250);
        }
        super.init();
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "cum.st" }); // onlyfans.com / fansly.com content
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_POST.pattern().substring(1) + "|" + PATTERN_PROFILE.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    /* service = any platform key without slash (e.g. onlyfans, fansly); creator- and post-ids are numeric. */
    private static final Pattern PATTERN_POST    = Pattern.compile("/creators/([^/]+)/(\\d+)/post/(\\d+)(?:\\?[^#]*)?$", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_PROFILE = Pattern.compile("/creators/([^/]+)/(\\d+)(\\?[^#]*)?$", Pattern.CASE_INSENSITIVE);
    private CumSt                hostPlugin      = null;
    private CryptedLink          cl              = null;

    private String getApiBase() {
        return "https://" + getHost() + "/api/v1";
    }

    private String getMediaBaseURL() {
        /* Host that serves the original media files (content-addressed by sha256). */
        return "https://e1." + getHost();
    }

    private CumStConfig cfg = null;

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        cfg = PluginJsonConfig.get(getConfigInterface());
        cl = param;
        if (new Regex(param.getCryptedUrl(), PATTERN_POST).patternFind()) {
            return this.crawlPost(param);
        } else if (new Regex(param.getCryptedUrl(), PATTERN_PROFILE).patternFind()) {
            return this.crawlProfile(param);
        } else {
            /* Unsupported URL --> Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    @Override
    public void clean() {
        cfg = null;
        super.clean();
    }

    private ArrayList<DownloadLink> crawlProfile(final CryptedLink param) throws Exception {
        final Regex urlinfo = new Regex(param.getCryptedUrl(), PATTERN_PROFILE);
        if (!urlinfo.patternFind()) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String service = urlinfo.getMatch(0);
        final String creatorID = urlinfo.getMatch(1);
        final UrlQuery query = UrlQuery.parse(param.getCryptedUrl());
        return crawlProfileAPI(service, creatorID, query);
    }

    /**
     * @param query
     *            : If it contains offset "o", only this offset/page onwards will be crawled starting at that offset.
     */
    private ArrayList<DownloadLink> crawlProfileAPI(final String service, final String creatorID, final UrlQuery query) throws Exception {
        if (service == null || creatorID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final boolean perPostPackageEnabled = cfg.isPerPostURLPackageEnabled();
        final boolean useAdvancedDupecheck = cfg.isEnableProfileCrawlerAdvancedDupeFiltering();
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String creatorName = this.findCreatorName(service, creatorID);
        final FilePackage profileFilePackage = getFilePackageForProfileCrawler(service, creatorID, creatorName);
        int offset = 0;
        String offsetString = null;
        String qString = null;
        if (query != null) {
            qString = query.get("q");
            offsetString = query.getDecoded("o");
            if (offsetString != null && offsetString.matches("^\\d+$")) {
                logger.info("Starting from offset: " + offsetString);
                offset = Integer.parseInt(offsetString);
            }
        }
        if (qString == null) {
            qString = "";
        } else {
            qString = "&q=" + qString;
        }
        int page = 1;
        final int maxItemsPerPage = 50;
        pagination: do {
            getPage(br, this.getApiBase() + "/" + service + "/user/" + Encoding.urlEncode(creatorID) + "/posts?o=" + offset + "&n=" + maxItemsPerPage + qString);
            final Map<String, Object> response = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final List<Map<String, Object>> posts = (List<Map<String, Object>>) response.get("posts");
            if (posts == null || posts.isEmpty()) {
                if (ret.isEmpty()) {
                    if (!StringUtils.isEmpty(qString)) {
                        throw new DecrypterRetryException(RetryReason.EMPTY_SEARCH_QUERY);
                    }
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else {
                    logger.info("Stopping because: Got empty page");
                    break pagination;
                }
            }
            for (final Map<String, Object> post : posts) {
                final String postID = post.get("id").toString();
                final ArrayList<DownloadLink> thisresults = crawlPostAPI(br, service, creatorID, creatorName, postID);
                if (!perPostPackageEnabled) {
                    for (final DownloadLink thisresult : thisresults) {
                        thisresult._setFilePackage(profileFilePackage);
                    }
                }
                distribute(thisresults);
                ret.addAll(thisresults);
            }
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size() + " | Offset: " + offset);
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break pagination;
            } else if (StringUtils.isNotEmpty(offsetString)) {
                logger.info("Stopping because: User provided specific offset to crawl: " + offsetString);
                break pagination;
            } else if (posts.size() < maxItemsPerPage) {
                logger.info("Stopping because: Reached last page(?) Page: " + page);
                break pagination;
            } else {
                /* Continue to next page */
                offset += posts.size();
                page++;
            }
        } while (!this.isAbort());
        /* Advanced dupe check is applied per file inside crawlProcessPostAPI when enabled. */
        if (useAdvancedDupecheck) {
            logger.info("Advanced dupe filtering was enabled");
        }
        return ret;
    }

    private FilePackage getFilePackageForProfileCrawler(final String service, final String creatorID, final String creatorName) {
        final FilePackage fp = FilePackage.getInstance();
        fp.setAllowMerge(true);
        fp.setAllowInheritance(true);
        if (creatorName != null) {
            fp.setName(service + " - " + creatorName);
        } else {
            fp.setName(service + " - " + creatorID);
        }
        fp.setPackageKey(CumSt.UNIQUE_ID_PREFIX + "service/" + service + "/creator/" + creatorID);
        return fp;
    }

    private FilePackage getFilePackageForPostCrawler(final String service, final String creatorID, final String creatorName, final String postID) {
        final FilePackage fp = FilePackage.getInstance();
        if (creatorName != null) {
            fp.setName(service + " - " + creatorName + " - " + postID);
        } else {
            /* Fallback */
            fp.setName(service + " - " + creatorID + " - " + postID);
        }
        fp.setIgnoreVarious(true);
        fp.setPackageKey(CumSt.UNIQUE_ID_PREFIX + "service/" + service + "/creator/" + creatorID + "/post/" + postID);
        return fp;
    }

    private ArrayList<DownloadLink> crawlPost(final CryptedLink param) throws Exception {
        final Regex urlinfo = new Regex(param.getCryptedUrl(), PATTERN_POST);
        if (!urlinfo.patternFind()) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String service = urlinfo.getMatch(0);
        final String creatorID = urlinfo.getMatch(1);
        final String postID = urlinfo.getMatch(2);
        final String creatorName = this.findCreatorName(service, creatorID);
        return crawlPostAPI(br, service, creatorID, creatorName, postID);
    }

    /** API docs: https://cum.st/api-docs */
    private ArrayList<DownloadLink> crawlPostAPI(final Browser br, final String service, final String creatorID, final String creatorName, final String postID) throws Exception {
        if (service == null || creatorID == null || postID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        getPage(br, this.getApiBase() + "/" + service + "/user/" + Encoding.urlEncode(creatorID) + "/post/" + Encoding.urlEncode(postID));
        final Map<String, Object> post = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final HashSet<String> dupes = new HashSet<String>();
        return crawlProcessPostAPI(post, service, creatorID, creatorName, dupes, cfg.isEnableProfileCrawlerAdvancedDupeFiltering());
    }

    /**
     * Processes a map of an API response containing information about a users' post.
     *
     * @throws Exception
     */
    private ArrayList<DownloadLink> crawlProcessPostAPI(final Map<String, Object> postmap, final String service, final String creatorID, final String creatorName, final HashSet<String> dupes, final boolean useAdvancedDupecheck) throws Exception {
        final String postID = postmap.get("id").toString();
        final String posturl = "https://" + getHost() + "/creators/" + service + "/" + creatorID + "/post/" + postID;
        /* Every item has a "published" date (unix timestamp in seconds) */
        final String publishedDateStr = StringUtils.valueOfOrNull(postmap.get("published"));
        /* Raw caption text (may be null) */
        final String postTextContent = (String) postmap.get("caption");
        final ArrayList<DownloadLink> directResults = new ArrayList<DownloadLink>();
        int numberofResultsSimpleCount = 0;
        int index = 0;
        final List<Map<String, Object>> attachments = (List<Map<String, Object>>) postmap.get("attachments");
        if (attachments != null) {
            for (final Map<String, Object> attachment : attachments) {
                if (Boolean.TRUE.equals(attachment.get("locked"))) {
                    /* Paywalled item -> no downloadable media available */
                    continue;
                }
                final DownloadLink media = buildFileDownloadLinkAPI(dupes, useAdvancedDupecheck, attachment, index);
                /* null = item is a duplicate or invalid */
                if (media != null) {
                    directResults.add(media);
                    index++;
                }
                numberofResultsSimpleCount++;
            }
        }
        logger.info("service: " + service + " | CreatorID: " + creatorID + " | PostID: " + postID + " | Attachment items in API response: " + numberofResultsSimpleCount + " | Number of unique file items: " + directResults.size());
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final FilePackage postFilePackage = getFilePackageForPostCrawler(service, creatorID, creatorName, postID);
        if (!StringUtils.isEmpty(postTextContent)) {
            final TextCrawlMode mode = cfg.getTextCrawlMode();
            if (mode == TextCrawlMode.ALWAYS || (mode == TextCrawlMode.ONLY_IF_NO_MEDIA_ITEMS_ARE_FOUND && directResults.isEmpty())) {
                ensureInitHosterplugin();
                final DownloadLink textfile = new DownloadLink(this.hostPlugin, getHost(), posturl);
                textfile.setProperty(CumSt.PROPERTY_TEXT, postTextContent);
                textfile.setFinalFileName(postFilePackage.getName() + ".txt");
                try {
                    textfile.setDownloadSize(postTextContent.getBytes("UTF-8").length);
                } catch (final UnsupportedEncodingException ignore) {
                    ignore.printStackTrace();
                }
                directResults.add(textfile);
            }
        }
        if (cfg.isCrawlHttpLinksFromPostContent()) {
            /* Crawl external http links from the raw caption text. */
            if (!StringUtils.isEmpty(postTextContent)) {
                final List<CrawledLink> postTextContentLinks = getCrawler().find(getLinkCrawlerGeneration(), getCurrentLink(), postTextContent, br.getURL(), false, false);
                if (postTextContentLinks != null) {
                    for (final CrawledLink postTextContentLink : postTextContentLinks) {
                        final String linkURL = postTextContentLink.getURL();
                        try {
                            final URL url = new URL(linkURL);
                            if (!dupes.add(url.getPath())) {
                                continue;
                            }
                        } catch (final MalformedURLException e) {
                            logger.log(e);
                        }
                        ret.add(this.createDownloadlink(linkURL));
                    }
                }
            }
            /* Crawl resolved outbound links (post_link rows). */
            final List<Map<String, Object>> links = (List<Map<String, Object>>) postmap.get("links");
            if (links != null) {
                for (final Map<String, Object> linkmap : links) {
                    final String rawUrl = StringUtils.valueOfOrNull(linkmap.get("rawUrl"));
                    if (StringUtils.isEmpty(rawUrl)) {
                        continue;
                    }
                    try {
                        final URL url = new URL(rawUrl);
                        if (!dupes.add(url.getPath())) {
                            continue;
                        }
                    } catch (final MalformedURLException e) {
                        logger.log(e);
                    }
                    ret.add(this.createDownloadlink(rawUrl));
                }
            }
        }
        for (final DownloadLink result : directResults) {
            if (!StringUtils.isEmpty(postTextContent)) {
                result.setProperty(CumSt.PROPERTY_POST_TEXT, postTextContent);
            }
            if (publishedDateStr != null) {
                result.setProperty(CumSt.PROPERTY_DATE, publishedDateStr);
            }
            result.setProperty(CumSt.PROPERTY_SERVICE, service);
            result.setProperty(CumSt.PROPERTY_CREATOR_ID, creatorID);
            if (creatorName != null) {
                result.setProperty(CumSt.PROPERTY_CREATOR_NAME, creatorName);
            }
            result.setProperty(CumSt.PROPERTY_POST_ID, postID);
            result.setAvailable(true);
            ret.add(result);
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* Set post-URL as container URL on all items. */
            for (final DownloadLink result : ret) {
                result.setContainerUrl(posturl);
            }
        }
        postFilePackage.addLinks(ret);
        return ret;
    }

    private DownloadLink buildFileDownloadLinkAPI(final HashSet<String> dupes, final boolean advancedDupeCheck, final Map<String, Object> filemap, final int index) throws PluginException {
        this.ensureInitHosterplugin();
        final Object storageKeyO = filemap.get("storageKey");
        if (storageKeyO == null) {
            /* No downloadable media available */
            return null;
        }
        final String storageKey = storageKeyO.toString();
        final String variantName = getOriginalVariantName(filemap);
        if (variantName == null) {
            /* No variant to download */
            return null;
        }
        final String url = getMediaBaseURL() + "/media/" + storageKey + "/" + variantName;
        /* Prefer the original filename provided by the API, else derive one from storageKey + variant extension. */
        String filename = (String) filemap.get("originalFilename");
        if (StringUtils.isEmpty(filename)) {
            final String ext = Files.getExtension(variantName, true);
            if (ext != null) {
                filename = storageKey + "." + ext;
            } else {
                filename = storageKey;
            }
        }
        final String sha256hash = (String) filemap.get("sha256");
        final String dupeCheckString;
        if (advancedDupeCheck && sha256hash != null) {
            dupeCheckString = sha256hash;
        } else {
            dupeCheckString = storageKey;
        }
        if (!dupes.add(dupeCheckString)) {
            /* Skip dupe */
            return null;
        }
        final DownloadLink media = new DownloadLink(this.hostPlugin, getHost(), url);
        media.setFinalFileName(filename);
        media.setProperty(CumSt.PROPERTY_BETTER_FILENAME, filename);
        media.setProperty(CumSt.PROPERTY_POST_CONTENT_INDEX, index);
        if (sha256hash != null) {
            media.setSha256Hash(sha256hash);
        }
        final Object bytesO = filemap.get("bytes");
        if (bytesO instanceof Number) {
            media.setDownloadSize(((Number) bytesO).longValue());
        }
        return media;
    }

    /** Returns the name of the "original" variant of a media item, or the first available variant. */
    private String getOriginalVariantName(final Map<String, Object> filemap) {
        final List<Map<String, Object>> variants = (List<Map<String, Object>>) filemap.get("variants");
        if (variants == null || variants.isEmpty()) {
            return null;
        }
        String fallback = null;
        for (final Map<String, Object> variant : variants) {
            final String name = StringUtils.valueOfOrNull(variant.get("name"));
            if (name == null) {
                continue;
            }
            if (fallback == null) {
                fallback = name;
            }
            if (name.toLowerCase(java.util.Locale.ROOT).startsWith("original")) {
                return name;
            }
        }
        return fallback;
    }

    private static Map<String, String> ID_TO_CREATORNAME = new LinkedHashMap<String, String>() {
        protected boolean removeEldestEntry(Map.Entry<String, String> eldest) {
            return size() > 100;
        };
    };

    /**
     * Returns the creators' username for the given service + creatorID. </br>
     * Uses API to find the username and caches the result. </br>
     * Returns null if it is unable to find the username.
     */
    private String findCreatorName(final String service, final String creatorID) throws Exception {
        synchronized (ID_TO_CREATORNAME) {
            if (StringUtils.isEmpty(service) || StringUtils.isEmpty(creatorID)) {
                /* Developer mistake */
                throw new IllegalArgumentException();
            }
            final String key = service + "_" + creatorID;
            String username = ID_TO_CREATORNAME.get(key);
            if (username != null) {
                return username;
            }
            final Browser brc = br.cloneBrowser();
            getPage(brc, this.getApiBase() + "/" + service + "/user/" + Encoding.urlEncode(creatorID) + "/profile");
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            username = StringUtils.valueOfOrNull(entries.get("name"));
            if (StringUtils.isEmpty(username)) {
                return null;
            }
            ID_TO_CREATORNAME.put(key, username);
            return username;
        }
    }

    private void ensureInitHosterplugin() throws PluginException {
        if (this.hostPlugin == null) {
            this.hostPlugin = (CumSt) getNewPluginForHostInstance(getHost());
        }
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* Try to avoid getting blocked / rate-limited. */
        return 1;
    }

    protected void getPage(final Browser br, final String url) throws Exception {
        final int maxTries = 15;
        final Random rnd = new Random();
        for (int i = 0; i <= maxTries; i++) {
            final boolean lastTry = i == maxTries;
            final GetRequest getRequest = br.createGetRequest(url);
            final URLConnectionAdapter con = br.openRequestConnection(getRequest);
            try {
                if (this.isAbort()) {
                    /* Aborted by user */
                    throw new InterruptedException();
                } else if (con.getResponseCode() == 404) {
                    br.followConnection(true);
                    /* E.g. {"error":"..."} */
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else if (con.getResponseCode() == 429) {
                    br.followConnection(true);
                    logger.info("Error 429 too many requests");
                    if (lastTry) {
                        throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT);
                    }
                    final int retrySeconds = 10 + rnd.nextInt(10);
                    final String title = "Rate-Limit reached";
                    String text = "Time until rate-limit reset: Unknown | Attempt " + (i + 1) + "/" + maxTries;
                    text += "\nTry again later or change your IP | Auto retry in " + retrySeconds + " seconds";
                    this.displayBubbleNotification(title, text);
                    this.sleep(retrySeconds * 1000, this.cl);
                    continue;
                } else if (con.getResponseCode() == 502) {
                    br.followConnection(true);
                    logger.info("Error 502 " + con.getResponseMessage());
                    if (lastTry) {
                        throw new DecrypterRetryException(RetryReason.HOST);
                    }
                    final int retrySeconds = 3 + rnd.nextInt(10);
                    this.sleep(retrySeconds * 1000, this.cl);
                    continue;
                } else if (con.getResponseCode() == 503) {
                    br.followConnection(true);
                    logger.info("Error 503 " + con.getResponseMessage());
                    if (lastTry) {
                        throw new DecrypterRetryException(RetryReason.HOST);
                    }
                    final int retrySeconds = 3 + rnd.nextInt(10);
                    this.sleep(retrySeconds * 1000, this.cl);
                    continue;
                } else {
                    br.followConnection();
                    return;
                }
            } finally {
                con.disconnect();
            }
        }
    }

    @Override
    public Class<? extends CumStConfig> getConfigInterface() {
        return CumStConfig.class;
    }
}
