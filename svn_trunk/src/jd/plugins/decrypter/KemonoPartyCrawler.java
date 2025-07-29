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
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.controlling.linkcrawler.CrawledLink;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.http.requests.GetRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;
import jd.plugins.hoster.KemonoParty;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.config.KemonoPartyConfig;
import org.jdownloader.plugins.components.config.KemonoPartyConfig.TextCrawlMode;
import org.jdownloader.plugins.components.config.KemonoPartyConfigCoomerParty;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

@DecrypterPlugin(revision = "$Revision: 51265 $", interfaceVersion = 3, names = {}, urls = {})
public class KemonoPartyCrawler extends PluginForDecrypt {
    public KemonoPartyCrawler(PluginWrapper wrapper) {
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
            Browser.setRequestIntervalLimitGlobal(host, false, 250);
        }
        super.init();
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "coomer.st", "coomer.su", "coomer.party" }); // onlyfans.com content
        ret.add(new String[] { "kemono.cr", "kemono.su", "kemono.party" }); // content of other websites such as patreon.com
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/[^/]+/user/([\\w\\-\\.]+(\\?o=(\\d+))?)(/post/[a-z0-9]+)?");
        }
        return ret.toArray(new String[0]);
    }

    private final String TYPE_PROFILE = "(?i)(?:https?://[^/]+)?/([^/]+)/user/([\\w\\-\\.]+)(\\?o=(\\d+))?$";
    private final String TYPE_POST    = "(?i)(?:https?://[^/]+)?/([^/]+)/user/([\\w\\-\\.]+)/post/([a-z0-9]+)$";
    private KemonoParty  hostPlugin   = null;
    private CryptedLink  cl           = null;

    private String getApiBase() {
        return "https://" + getHost() + "/api/v1";
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        cl = param;
        if (param.getCryptedUrl().matches(TYPE_PROFILE)) {
            return this.crawlProfile(param);
        } else if (param.getCryptedUrl().matches(TYPE_POST)) {
            return this.crawlPost(param);
        } else {
            /* Unsupported URL --> Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    private ArrayList<DownloadLink> crawlProfile(final CryptedLink param) throws Exception {
        final Regex urlinfo = new Regex(param.getCryptedUrl(), TYPE_PROFILE);
        if (!urlinfo.patternFind()) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String service = urlinfo.getMatch(0);
        final String userID = urlinfo.getMatch(1);
        final String startOffsetStr = urlinfo.getMatch(3);
        Integer startOffset = null;
        if (startOffsetStr != null) {
            startOffset = Integer.parseInt(startOffsetStr);
        }
        final boolean useAPI = true;
        if (useAPI) {
            return crawlProfileAPI(service, userID, startOffset);
        } else {
            return crawlProfileWebsite(param, service, userID, startOffset);
        }
    }

    /**
     * @param startOffset
     *            : If provided, only this offset/page will be crawled.
     */
    private ArrayList<DownloadLink> crawlProfileAPI(final String service, final String usernameOrUserID, final Integer startOffset) throws Exception {
        if (service == null || usernameOrUserID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final HashSet<String> dupes = new HashSet<String>();
        final boolean useAdvancedDupecheck = PluginJsonConfig.get(getConfigInterface()).isEnableProfileCrawlerAdvancedDupeFiltering();
        final boolean perPostPackageEnabled = PluginJsonConfig.get(getConfigInterface()).isPerPostURLPackageEnabled();
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final FilePackage profileFilePackage = getFilePackageForProfileCrawler(service, usernameOrUserID);
        int offset = 0;
        if (startOffset != null) {
            logger.info("Starting from offset: " + startOffset);
            offset = startOffset.intValue();
        }
        int page = 1;
        final int maxItemsPerPage = 50;
        int numberofContinuousPagesWithoutAnyNewItems = 0;
        final int maxPagesWithoutNewItems = 15;
        do {
            getPage(br, this.getApiBase() + "/" + service + "/user/" + Encoding.urlEncode(usernameOrUserID) + "?o=" + offset);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final List<Map<String, Object>> posts = (List<Map<String, Object>>) restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
            if (posts == null || posts.isEmpty()) {
                if (ret.isEmpty()) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else {
                    /* This should never happen */
                    logger.info("Stopping because: Got empty page");
                    break;
                }
            }
            final int numberofUniqueItemsOld = dupes.size();
            for (final Map<String, Object> post : posts) {
                final ArrayList<DownloadLink> thisresults = this.crawlProcessPostAPI(post, dupes, useAdvancedDupecheck);
                for (final DownloadLink thisresult : thisresults) {
                    if (!perPostPackageEnabled) {
                        thisresult._setFilePackage(profileFilePackage);
                    }
                    distribute(thisresult);
                }
                ret.addAll(thisresults);
            }
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size() + " | Offset: " + offset);
            final int numberofUniqueItemsNew = dupes.size();
            final int numberofNewItems = numberofUniqueItemsNew - numberofUniqueItemsOld;
            if (numberofNewItems == 0) {
                numberofContinuousPagesWithoutAnyNewItems++;
            } else {
                numberofContinuousPagesWithoutAnyNewItems = 0;
            }
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            } else if (startOffset != null) {
                logger.info("Stopping because: User provided specific offset to crawl: " + startOffset);
                break;
            } else if (numberofContinuousPagesWithoutAnyNewItems >= maxPagesWithoutNewItems) {
                logger.info("Stopping because: Too many pages without any new items: " + maxPagesWithoutNewItems);
                break;
            } else if (posts.size() < maxItemsPerPage) {
                logger.info("Stopping because: Reached last page(?) Page: " + page);
                break;
            } else {
                /* Continue to next page */
                offset += posts.size();
                page++;
            }
        } while (!this.isAbort());
        return ret;
    }

    @Deprecated
    private ArrayList<DownloadLink> crawlProfileWebsite(final CryptedLink param, final String service, final String userID, final Integer startOffset) throws Exception {
        if (service == null || userID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String url = "https://" + this.getHost() + "/" + service + "/user/" + userID;
        if (startOffset != null) {
            logger.info("Starting from offset: " + startOffset);
            url += "?o=" + startOffset;
        }
        getPage(br, url);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.getURL().matches(TYPE_PROFILE)) {
            /* E.g. redirect to main page */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        /* Find number of last page (for logging purposes) */
        int maxpage = getMaxPageWebsite(br, service, userID, 1);
        int totalNumberofItems = -1;
        String totalNumberofItemsStr = br.getRegex("Showing \\d+ - \\d+ of (\\d+)").getMatch(0);
        if (totalNumberofItemsStr != null) {
            totalNumberofItems = Integer.parseInt(totalNumberofItemsStr);
        } else {
            totalNumberofItemsStr = "unknown";
        }
        final FilePackage fp = getFilePackageForProfileCrawler(service, userID);
        final HashSet<String> dupes = new HashSet<String>();
        int page = 1;
        do {
            final String[] posturls = br.getRegex("(?:https?://[^/]+)?/([^/]+)/user/([^/]+)/post/([a-z0-9]+)").getColumn(-1);
            int numberofAddedItems = 0;
            for (String posturl : posturls) {
                posturl = br.getURL(posturl).toString();
                if (dupes.add(posturl)) {
                    final DownloadLink result = this.createDownloadlink(posturl);
                    result._setFilePackage(fp);
                    ret.add(result);
                    distribute(result);
                    numberofAddedItems++;
                }
            }
            maxpage = getMaxPageWebsite(br, service, userID, maxpage);
            logger.info("Crawled page " + page + "/" + maxpage + " | Found items: " + ret.size() + "/" + totalNumberofItemsStr);
            final String nextpageurl = br.getRegex("(/[^\"]+\\?o=\\d+)\"[^>]*>(?:<b>)?\\s*" + (page + 1)).getMatch(0);
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            } else if (ret.size() == totalNumberofItems) {
                logger.info("Stopping because: Found all items");
                break;
            } else if (startOffset != null) {
                logger.info("Stopping because: User provided specific offset to crawl");
                break;
            } else if (nextpageurl == null) {
                /* Additional fail-safe */
                logger.info("Stopping because: Failed to find nextpageurl - last page is: " + br.getURL());
                break;
            } else if (numberofAddedItems == 0) {
                logger.info("Stopping because: Failed to find any [new] items on current page");
                break;
            } else {
                page++;
                getPage(br, nextpageurl);
            }
        } while (true);
        return ret;
    }

    private int getMaxPageWebsite(Browser br, final String service, final String username, int maxPage) {
        final String[] pages = br.getRegex("href=\"/" + Pattern.quote(service) + "/user/" + Pattern.quote(username) + "\\?o=\\d+\"[^>]*>\\s*(?:<b>)?\\s*(\\d+)").getColumn(0);
        int ret = maxPage;
        for (final String pageStr : pages) {
            final int page = Integer.parseInt(pageStr);
            if (page > ret) {
                ret = page;
            }
        }
        return ret;
    }

    private FilePackage getFilePackageForProfileCrawler(final String service, final String userID) {
        final FilePackage fp = FilePackage.getInstance();
        fp.setAllowMerge(true);
        fp.setAllowInheritance(true);
        fp.setName(service + " - " + userID);
        fp.setPackageKey(KemonoParty.UNIQUE_ID_PREFIX + "service/" + service + "/userid/" + userID);
        return fp;
    }

    private FilePackage getFilePackageForPostCrawler(final String service, final String userID, final String postID, final String postTitle) {
        final FilePackage fp = FilePackage.getInstance();
        if (postTitle != null) {
            fp.setName(service + " - " + userID + " - " + postID + " - " + postTitle);
        } else {
            /* Fallback */
            fp.setName(service + " - " + userID + " - " + postID);
        }
        fp.setIgnoreVarious(true);
        fp.setPackageKey(KemonoParty.UNIQUE_ID_PREFIX + "service/" + service + "/userid/" + userID + "/postid/" + postID);
        return fp;
    }

    private ArrayList<DownloadLink> crawlPost(final CryptedLink param) throws Exception {
        final Regex urlinfo = new Regex(param.getCryptedUrl(), TYPE_POST);
        if (!urlinfo.patternFind()) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String service = urlinfo.getMatch(0);
        final String usernameOrUserID = urlinfo.getMatch(1);
        final String postID = urlinfo.getMatch(2);
        final boolean crawlPostAPI = true;
        if (crawlPostAPI) {
            return crawlPostAPI(param, service, usernameOrUserID, postID);
        } else {
            return crawlPostWebsite(param, service, usernameOrUserID, postID);
        }
    }

    /** API docs: https://kemono.su/api/schema */
    private ArrayList<DownloadLink> crawlPostAPI(final CryptedLink param, final String service, final String userID, final String postID) throws Exception {
        if (service == null || userID == null || postID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        getPage(br, this.getApiBase() + "/" + service + "/user/" + userID + "/post/" + postID);
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* E.g. {"error":"Not Found"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        /* 2024-11-06: Looks like they are playing around with API changes. */
        Map<String, Object> post = (Map<String, Object>) entries.get("post");
        if (post == null) {
            post = entries;
        }
        return crawlProcessPostAPI(post, new HashSet<String>(), false);
    }

    /**
     * Processes a map of an API response containing information about a users' post.
     *
     * @throws Exception
     */
    private ArrayList<DownloadLink> crawlProcessPostAPI(final Map<String, Object> postmap, final HashSet<String> dupes, final boolean useAdvancedDupecheck) throws Exception {
        final String service = postmap.get("service").toString();
        final String usernameOrUserID = postmap.get("user").toString();
        final String postID = postmap.get("id").toString();
        final String posturl = "https://" + this.getHost() + "/" + service + "/user/" + usernameOrUserID + "/post/" + postID;
        final String postTitle = postmap.get("title").toString();
        final String publishedDateStr = postmap.get("published").toString();
        final ArrayList<DownloadLink> kemonoResults = new ArrayList<DownloadLink>();
        int numberofResultsSimpleCount = 0;
        int index = 0;
        final Map<String, Object> filemap = (Map<String, Object>) postmap.get("file");
        if (!filemap.isEmpty()) {
            final DownloadLink media = buildFileDownloadLinkAPI(dupes, useAdvancedDupecheck, filemap, index);
            /* null = item is a duplicate */
            if (media != null) {
                kemonoResults.add(media);
                index++;
            }
            numberofResultsSimpleCount++;
        }
        final List<Map<String, Object>> attachments = (List<Map<String, Object>>) postmap.get("attachments");
        if (attachments != null) {
            for (final Map<String, Object> attachment : attachments) {
                final DownloadLink media = buildFileDownloadLinkAPI(dupes, useAdvancedDupecheck, attachment, index);
                /* null = item is a duplicate */
                if (media != null) {
                    kemonoResults.add(media);
                    index++;
                }
                numberofResultsSimpleCount++;
            }
        }
        logger.info("service: " + service + " | UserID: " + usernameOrUserID + " | PostID: " + postID + " | File items in API response: " + numberofResultsSimpleCount + " | Number of unique file items: " + kemonoResults.size());
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final FilePackage postFilePackage = getFilePackageForPostCrawler(service, usernameOrUserID, postID, postTitle);
        String postTextContent = (String) postmap.get("content");
        final KemonoPartyConfig cfg = PluginJsonConfig.get(getConfigInterface());
        if (!StringUtils.isEmpty(postTextContent)) {
            if (cfg.isCrawlHttpLinksFromPostContent()) {
                /* Place number 1 where we can crawl external http links from */
                postTextContent = postTextContent.replaceAll("[\\xA0]+", " ");
                postTextContent = postTextContent.replaceAll("(?i)Key(\\s*for\\s*the\\s*link)?\\s*:", "KEY:");
                postTextContent = postTextContent.replaceAll("(?i)<a[^>]href\\s*=\\s*\"(.*?)\"[^>]*>\\s*\\1\\s*</a>(?:\\s*</p>[^>]*<p>)?\\s*(?:#|KEY:)\\s*([^< ]*)", "<a href=\"$1#$2\"</a>");
                final List<CrawledLink> postTextContentLinks = getCrawler().find(getLinkCrawlerGeneration(), getCurrentLink(), postTextContent, br.getURL(), false, false);
                if (postTextContentLinks != null) {
                    for (CrawledLink postTextContentLink : postTextContentLinks) {
                        try {
                            final URL url = new URL(postTextContentLink.getURL());
                            if (!dupes.add(url.getPath())) {
                                // alread part of attachments
                                continue;
                            }
                        } catch (final MalformedURLException e) {
                            logger.log(e);
                        }
                        ret.add(this.createDownloadlink(postTextContentLink.getURL()));
                    }
                }
            }
            final TextCrawlMode mode = cfg.getTextCrawlMode();
            if (mode == TextCrawlMode.ALWAYS || (mode == TextCrawlMode.ONLY_IF_NO_MEDIA_ITEMS_ARE_FOUND && kemonoResults.isEmpty())) {
                ensureInitHosterplugin();
                final DownloadLink textfile = new DownloadLink(this.hostPlugin, this.getHost(), posturl);
                textfile.setProperty(KemonoParty.PROPERTY_TEXT, postTextContent);
                textfile.setFinalFileName(postFilePackage.getName() + ".txt");
                try {
                    textfile.setDownloadSize(postTextContent.getBytes("UTF-8").length);
                } catch (final UnsupportedEncodingException ignore) {
                    ignore.printStackTrace();
                }
                kemonoResults.add(textfile);
            }
        }
        if (cfg.isCrawlHttpLinksFromPostContent()) {
            /* Place number 2 where we can crawl external http links from */
            final Map<String, Object> embedmap = (Map<String, Object>) postmap.get("embed");
            if (embedmap != null && embedmap.size() > 0) {
                final String url = embedmap.get("url").toString();
                ret.add(this.createDownloadlink(url));
            }
        }
        final String username = this.findUsername(service, usernameOrUserID);
        for (final DownloadLink kemonoResult : kemonoResults) {
            if (!StringUtils.isEmpty(postTitle)) {
                kemonoResult.setProperty(KemonoParty.PROPERTY_TITLE, postTitle);
            }
            if (!StringUtils.isEmpty(postTextContent)) {
                kemonoResult.setProperty(KemonoParty.PROPERTY_POST_TEXT, postTextContent);
            }
            if (publishedDateStr != null) {
                kemonoResult.setProperty(KemonoParty.PROPERTY_DATE, publishedDateStr);
            }
            kemonoResult.setProperty(KemonoParty.PROPERTY_PORTAL, service);
            kemonoResult.setProperty(KemonoParty.PROPERTY_USERID, usernameOrUserID);
            kemonoResult.setProperty(KemonoParty.PROPERTY_USERNAME, username);
            kemonoResult.setProperty(KemonoParty.PROPERTY_POSTID, postID);
            kemonoResult.setAvailable(true);
            /* Add kemono item to our list of total results. */
            ret.add(kemonoResult);
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            // Test 2024-03-25, see: https://board.jdownloader.org/showthread.php?t=95398
            /* Set post-URL as container URL on all items. */
            for (final DownloadLink result : ret) {
                result.setContainerUrl(posturl);
            }
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            // postFilePackage.setName(String.format("[@DEV: %d Expected kemono results] ", kemonoResults.size()) +
            // postFilePackage.getName());
        }
        postFilePackage.addLinks(ret);
        return ret;
    }

    private DownloadLink buildFileDownloadLinkAPI(final HashSet<String> dupes, final boolean advancedDupeCheck, final Map<String, Object> filemap, final int index) throws PluginException {
        this.ensureInitHosterplugin();
        /**
         * 2025-06-02: Looks like the "name" field is not always given though it missing can also mean that the original file is
         * broken/missing on the server. <br>
         * Example: /fanbox/user/64937143/post/2095805
         */
        final String filename = (String) filemap.get("name");
        final String filepath = filemap.get("path").toString();
        String url = "https://" + this.getHost() + "/data" + filepath;
        if (filename != null) {
            url += "?f=" + Encoding.urlEncode(filename);
        }
        final String sha256hash = KemonoParty.getSha256HashFromURL(url);
        final String dupeCheckString;
        if (advancedDupeCheck && sha256hash != null) {
            dupeCheckString = sha256hash;
        } else {
            dupeCheckString = filepath;
        }
        if (!dupes.add(dupeCheckString)) {
            /* Skip dupe */
            return null;
        }
        final DownloadLink media = new DownloadLink(this.hostPlugin, this.getHost(), url);
        if (filename != null) {
            media.setFinalFileName(filename);
            media.setProperty(KemonoParty.PROPERTY_BETTER_FILENAME, filename);
        }
        media.setProperty(KemonoParty.PROPERTY_POST_CONTENT_INDEX, index);
        if (sha256hash != null) {
            media.setSha256Hash(sha256hash);
        }
        return media;
    }

    @Deprecated
    private ArrayList<DownloadLink> crawlPostWebsite(final CryptedLink param, final String service, final String usernameOrUserID, final String postID) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (service == null || usernameOrUserID == null || postID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String posturl = "https://" + this.getHost() + "/" + service + "/user/" + usernameOrUserID + "/post/" + postID;
        br.setAllowedResponseCodes(500);// DDOS-GUARD
        int retry = 3;
        while (retry > 0) {
            getPage(br, posturl);
            if (br.getHttpConnection().getResponseCode() == 500 && !isAbort()) {
                sleep(1000, param);
                retry--;
            } else {
                break;
            }
        }
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.getURL().matches(TYPE_POST)) {
            /* E.g. redirect to main page of user because single post does not exist */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String postTitle = br.getRegex("class=\"post__title\">\\s*<span>([^<]+)</span>").getMatch(0);
        if (postTitle != null) {
            postTitle = Encoding.htmlDecode(postTitle).trim();
        }
        String publishedDateStr = br.getRegex("\"post__published\"[^>]*>\\s*<time[^>]*class\\s*=\\s*\"timestamp[^>]*datetime\\s*=\\s*\"\\s*([0-9\\-: ]+)").getMatch(0);
        if (publishedDateStr == null) {
            publishedDateStr = br.getRegex("<meta name\\s*=\\s*\"published\"\\s*content\\s*=\\s*\"\\s*([0-9\\-: ]+)").getMatch(0);
        }
        final ArrayList<DownloadLink> kemonoResults = new ArrayList<DownloadLink>();
        final String[] directURLs = br.getRegex("\"((https?://[^/]+)?/data/[^\"]+)").getColumn(0);
        if (directURLs != null && directURLs.length > 0) {
            /* Remove duplicates from results so our index will be correct down below. */
            ensureInitHosterplugin();
            final ArrayList<String> videoItemsToSkip = new ArrayList<String>();
            final HashSet<String> dups = new HashSet<String>();
            int index = 0;
            final ArrayList<DownloadLink> videoItemsUnfiltered = new ArrayList<DownloadLink>();
            for (final String directURL : directURLs) {
                final String urlFull = br.getURL(directURL).toString();
                final String sha256hash = KemonoParty.getSha256HashFromURL(urlFull);
                final String dupeCheckString;
                if (sha256hash != null) {
                    dupeCheckString = sha256hash;
                } else {
                    dupeCheckString = new URL(urlFull).getPath();
                }
                if (!dups.add(dupeCheckString)) {
                    /* Skip dupes */
                    continue;
                }
                final DownloadLink media = new DownloadLink(this.hostPlugin, this.getHost(), urlFull);
                media.setProperty(KemonoParty.PROPERTY_POST_CONTENT_INDEX, index);
                boolean isFilenameFromHTML = false;
                String betterFilename = getBetterFilenameFromURL(urlFull);
                if (betterFilename == null) {
                    /* 2023-03-01 */
                    betterFilename = br.getRegex(Pattern.quote(directURL) + "\"\\s+download=\"([^\"]+)\"").getMatch(0);
                    isFilenameFromHTML = true;
                }
                if (!StringUtils.isEmpty(betterFilename)) {
                    betterFilename = Encoding.htmlDecode(betterFilename).trim();
                    media.setFinalFileName(betterFilename);
                    if (isFilenameFromHTML) {
                        media.setProperty(KemonoParty.PROPERTY_BETTER_FILENAME, betterFilename);
                    }
                    media.setProperty(DirectHTTP.FIXNAME, betterFilename);
                    final String internalVideoFilename = new Regex(urlFull, "(?i)([a-f0-9]{64}\\.(m4v|mp4))").getMatch(0);
                    if (internalVideoFilename != null) {
                        videoItemsToSkip.add(internalVideoFilename);
                    }
                }
                if (sha256hash != null) {
                    media.setSha256Hash(sha256hash);
                }
                videoItemsUnfiltered.add(media);
                index++;
            }
            if (videoItemsToSkip.size() > 0) {
                logger.info("Filtering duplicated video items: " + videoItemsUnfiltered);
                for (final DownloadLink link : videoItemsUnfiltered) {
                    boolean filter = false;
                    for (final String videoItemToSkip : videoItemsToSkip) {
                        if (link.getPluginPatternMatcher().endsWith(videoItemToSkip)) {
                            filter = true;
                            break;
                        }
                    }
                    if (!filter) {
                        kemonoResults.add(link);
                    }
                }
            } else {
                kemonoResults.addAll(videoItemsUnfiltered);
            }
        }
        final FilePackage fp = getFilePackageForPostCrawler(service, usernameOrUserID, postID, postTitle);
        final String postTextContent = br.getRegex("<div\\s*class\\s*=\\s*\"post__content\"[^>]*>(.+)</div>\\s*<footer").getMatch(0);
        if (!StringUtils.isEmpty(postTextContent)) {
            final KemonoPartyConfig cfg = PluginJsonConfig.get(getConfigInterface());
            final TextCrawlMode mode = cfg.getTextCrawlMode();
            if (cfg.isCrawlHttpLinksFromPostContent()) {
                final String[] urls = HTMLParser.getHttpLinks(postTextContent, br.getURL());
                if (urls != null && urls.length > 0) {
                    for (final String url : urls) {
                        ret.add(this.createDownloadlink(url));
                    }
                }
            }
            if (mode == TextCrawlMode.ALWAYS || (mode == TextCrawlMode.ONLY_IF_NO_MEDIA_ITEMS_ARE_FOUND && kemonoResults.isEmpty())) {
                ensureInitHosterplugin();
                final DownloadLink textfile = new DownloadLink(this.hostPlugin, this.getHost(), posturl);
                textfile.setProperty(KemonoParty.PROPERTY_TEXT, postTextContent);
                textfile.setFinalFileName(fp.getName() + ".txt");
                try {
                    textfile.setDownloadSize(postTextContent.getBytes("UTF-8").length);
                } catch (final UnsupportedEncodingException ignore) {
                    ignore.printStackTrace();
                }
                kemonoResults.add(textfile);
            }
        }
        final String username = this.findUsername(service, usernameOrUserID);
        for (final DownloadLink kemonoResult : kemonoResults) {
            if (!StringUtils.isEmpty(postTitle)) {
                kemonoResult.setProperty(KemonoParty.PROPERTY_TITLE, postTitle);
            }
            if (publishedDateStr != null) {
                kemonoResult.setProperty(KemonoParty.PROPERTY_DATE, publishedDateStr);
            }
            kemonoResult.setProperty(KemonoParty.PROPERTY_PORTAL, service);
            kemonoResult.setProperty(KemonoParty.PROPERTY_USERID, usernameOrUserID);
            kemonoResult.setProperty(KemonoParty.PROPERTY_USERID, username);
            kemonoResult.setProperty(KemonoParty.PROPERTY_POSTID, postID);
            kemonoResult.setAvailable(true);
            /* Add kemono item to our list of total results. */
            ret.add(kemonoResult);
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            fp.setName(String.format("[@DEV: %d Expected kemono results] ", kemonoResults.size()) + fp.getName());
        }
        fp.addLinks(ret);
        return ret;
    }

    private static Map<String, String> ID_TO_USERNAME = new LinkedHashMap<String, String>() {
                                                          protected boolean removeEldestEntry(Map.Entry<String, String> eldest) {
                                                              return size() > 100;
                                                          };
                                                      };

    /**
     * Returns userID for given username. </br> Uses API to find userID. </br> Throws Exception if it is unable to find userID.
     */
    private String findUsername(final String service, final String usernameOrUserID) throws Exception {
        synchronized (ID_TO_USERNAME) {
            if (StringUtils.isEmpty(usernameOrUserID)) {
                /* Developer mistake */
                throw new IllegalArgumentException();
            }
            if (!usernameOrUserID.matches("\\d+")) {
                /* Not an ID but a username already */
                return usernameOrUserID;
            }
            final String key = service + "_" + usernameOrUserID;
            String username = ID_TO_USERNAME.get(key);
            if (username != null) {
                return username;
            }
            final Browser brc = br.cloneBrowser();
            getPage(brc, this.getApiBase() + "/" + service + "/user/" + usernameOrUserID + "/profile");
            if (brc.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            username = entries.get("name").toString();
            if (StringUtils.isEmpty(username)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            ID_TO_USERNAME.put(key, username);
            return username;
        }
    }

    public static String getBetterFilenameFromURL(final String url) throws MalformedURLException {
        final UrlQuery query = UrlQuery.parse(url);
        final String betterFilename = query.get("f");
        if (betterFilename != null) {
            return Encoding.htmlDecode(betterFilename).trim();
        } else {
            return null;
        }
    }

    private void ensureInitHosterplugin() throws PluginException {
        if (this.hostPlugin == null) {
            this.hostPlugin = (KemonoParty) getNewPluginForHostInstance(this.getHost());
        }
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* Try to avoid getting blocked by DDOS-GUARD / rate-limited. */
        return 1;
    }

    protected void getPage(final Browser br, final String url) throws Exception {
        boolean errorRateLimit = true;
        final int maxtries = 15;
        for (int i = 0; i <= maxtries; i++) {
            GetRequest getRequest = br.createGetRequest(url);
            final URLConnectionAdapter con = br.openRequestConnection(getRequest);
            try {
                if (this.isAbort()) {
                    /* Aborted by user */
                    throw new InterruptedException();
                } else if (con.getResponseCode() == 429) {
                    br.followConnection(true);
                    logger.info("Error 429 too many requests");
                    final int retrySeconds = 10;
                    final String title = "Rate-Limit reached";
                    String text = "Time until rate-limit reset: Unknown | Attempt " + (i + 1) + "/" + maxtries;
                    text += "\nTry again later or change your IP | Auto retry in " + retrySeconds + " seconds";
                    this.displayBubbleNotification(title, text);
                    this.sleep(retrySeconds * 1000, this.cl);
                    continue;
                } else {
                    br.followConnection();
                    errorRateLimit = false;
                    break;
                }
            } finally {
                con.disconnect();
            }
        }
        if (errorRateLimit) {
            throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT);
        }
    }

    @Override
    public Class<? extends KemonoPartyConfig> getConfigInterface() {
        if ("kemono.party".equalsIgnoreCase(getHost())) {
            return KemonoPartyConfig.class;
        } else {
            return KemonoPartyConfigCoomerParty.class;
        }
    }
}
