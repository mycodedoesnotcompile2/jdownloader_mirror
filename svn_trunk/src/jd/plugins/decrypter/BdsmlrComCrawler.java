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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.HTMLParser;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.BdsmlrCom;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52961 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { BdsmlrCom.class })
public class BdsmlrComCrawler extends PluginForDecrypt {
    public BdsmlrComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(new int[] { 500 });
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(jd.plugins.hoster.BdsmlrCom.getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(jd.plugins.hoster.BdsmlrCom.getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(jd.plugins.hoster.BdsmlrCom.getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://([\\w-]+\\.)?" + buildHostsPatternPart(domains) + "(/.+)?");
        }
        return ret.toArray(new String[0]);
    }

    private static final Pattern PATTERN_USER_PROFILE     = Pattern.compile("https?://([\\w-]+)\\.[^/]+(/.+)?", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_USER_PROFILE_NEW = Pattern.compile("/blog/(.+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_POST             = Pattern.compile("/post/(\\d+)", Pattern.CASE_INSENSITIVE);
    /* Direct-URLs typically contain CDN host "cdnXXX." or "ocdnXXX." where "XXX" is numbers. */
    private static final Pattern PATTERN_DIRECT           = Pattern.compile("https?://[^/]+/uploads/.+", Pattern.CASE_INSENSITIVE);
    private static final String  PROPERTY_POST_ID         = "post_id";
    private static final String  API_BASE                 = "https://api-prod.bdsmlr.com/v2/api";

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        if (new Regex(contenturl, PATTERN_DIRECT).patternFind()) {
            /* CDN subdomain: This may happen if user adds a direct-url to an image. */
            final DownloadLink direct = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(contenturl));
            /**
             * Important and this may save us the need for one http request. <br>
             * This is NOT the referer that the browser would use (that would be https://username.bdsmlr.com/") but the url itself as
             * referrer works fine too.
             */
            direct.setReferrerUrl(contenturl);
            final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
            ret.add(direct);
            return ret;
        }
        /* Login if an account exists */
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account != null) {
            final BdsmlrCom hostPlugin = (BdsmlrCom) this.getNewPluginForHostInstance(this.getHost());
            hostPlugin.login(account, false);
        }
        final String postID = new Regex(contenturl, PATTERN_POST).getMatch(0);
        if (postID != null) {
            return crawlPost(param, postID);
        } else {
            String username = new Regex(URLHelper.getUrlWithoutParams(contenturl), PATTERN_USER_PROFILE_NEW).getMatch(0);
            if (username == null) {
                username = new Regex(contenturl, PATTERN_USER_PROFILE).getMatch(0);
            }
            if (username == null) {
                /* Developer mistake or user somehow added an unsupported link. */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            return crawlUser(param, account, username);
        }
    }

    private ArrayList<DownloadLink> crawlPost(final CryptedLink param, final String postID) throws Exception {
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("post_id", Long.parseLong(postID));
        br.getPage(br.createJSonPostRequest(API_BASE + "/get-post-detail", postdata));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> response = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> post = (Map<String, Object>) response.get("post");
        final String username = post.get("blogName").toString().toLowerCase(Locale.ROOT);
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username + "_" + postID);
        return crawlPostItems(param, post, fp);
    }

    /** Builds all downloadable items of a single post (as returned by "get-post-detail" or "list-blog-activity"). */
    private ArrayList<DownloadLink> crawlPostItems(final CryptedLink param, final Map<String, Object> post, final FilePackage fp) throws PluginException {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String postID = post.get("id").toString();
        final String username = post.get("blogName").toString().toLowerCase(Locale.ROOT);
        final String referrerUrl = "https://" + username + "." + this.getHost() + "/";
        final Map<String, Object> mediaRepresentation = (Map<String, Object>) post.get("mediaRepresentation");
        final List<Object> items = (List<Object>) mediaRepresentation.get("items");
        final HashSet<String> dupes = new HashSet<String>();
        for (final Object itemO : items) {
            final Map<String, Object> item = (Map<String, Object>) itemO;
            final Map<String, Object> original = (Map<String, Object>) item.get("original");
            final String directurl = original.get("url").toString();
            if (!dupes.add(directurl)) {
                /* Skip duplicates */
                logger.info("Skipping dupe: " + directurl);
                continue;
            }
            final Regex dateInfo = new Regex(directurl, "/uploads/(?:photos|videos)/(\\d{4})/(\\d{2})/");
            final String year = dateInfo.getMatch(0);
            final String month = dateInfo.getMatch(1);
            final DownloadLink link = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(directurl));
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                /* Make direct-urls easier accessible for plugin developers. */
                link.setContentUrl(directurl);
            } else {
                link.setContentUrl(param.getCryptedUrl());
            }
            if (dupes.size() > 1) {
                link.setFinalFileName(username + "_" + year + "_" + month + "_" + postID + "_" + dupes.size() + Plugin.getFileNameExtensionFromURL(directurl));
            } else {
                link.setFinalFileName(username + "_" + year + "_" + month + "_" + postID + Plugin.getFileNameExtensionFromURL(directurl));
            }
            link.setAvailable(true);
            link.setProperty(PROPERTY_POST_ID, postID);
            /* Important! Some URLs cannot be accessed without the expected referrer value and server will return error 403! */
            link.setReferrerUrl(referrerUrl);
            link._setFilePackage(fp);
            ret.add(link);
            distribute(link);
        }
        if (ret.isEmpty()) {
            /* E.g. posts that only link to an external website. */
            final Map<String, Object> content = (Map<String, Object>) post.get("content");
            final String externalUrl = content != null ? (String) content.get("url") : null;
            if (externalUrl != null) {
                final DownloadLink externalItem = this.createDownloadlink(externalUrl);
                externalItem._setFilePackage(fp);
                ret.add(externalItem);
                distribute(externalItem);
            }
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlUser(final CryptedLink param, final Account account, final String username) throws Exception {
        final String contenturl = param.getCryptedUrl();
        final String searchKeyword = new Regex(contenturl, "(?i)/search/([^/]+)").getMatch(0);
        if (searchKeyword != null) {
            if (account == null) {
                /* 2025-06-03: Accessing search URLs in a logged out state will result in http error 500. */
                throw new AccountRequiredException("Account required to crawl search URLs");
            }
            return crawlUserSearch(param, username, searchKeyword);
        } else {
            return crawlUserBlog(param, username);
        }
    }

    private ArrayList<DownloadLink> crawlUserBlog(final CryptedLink param, final String blog_name) throws Exception {
        logger.info("Crawling all posts from user " + blog_name);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Map<String, Object> blogNameData = new HashMap<String, Object>();
        blogNameData.put("blog_name", blog_name);
        br.getPage(br.createJSonPostRequest(API_BASE + "/blog-tag-affinity", blogNameData));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> blogInfo = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Object blogId = blogInfo.get("blogId");
        if (blogId == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String username = blogInfo.get("blogName").toString();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username);
        fp.setPackageKey("bdsmlr://blog/" + blogId);
        /* E.g. "?activity=like,reblog,post,comment" -> crawl only these activity-kinds instead of the default of all four. */
        final UrlQuery urlQuery = UrlQuery.parse(param.getCryptedUrl());
        final String activityParam = urlQuery.get("activity");
        final List<String> activityKinds;
        if (!StringUtils.isEmpty(activityParam)) {
            activityKinds = Arrays.asList(activityParam.split(","));
            logger.info("Crawling with user defined activityKinds: " + activityKinds);
        } else {
            activityKinds = Arrays.asList("post", "reblog", "like", "comment");
        }
        final HashSet<String> dupes = new HashSet<String>();
        final Map<String, Object> page = new HashMap<String, Object>();
        page.put("page_size", 15);
        int pageNumber = 1;
        pagination: do {
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("blog_id", blogId);
            postdata.put("sort_field", 1);
            postdata.put("order", 2);
            postdata.put("post_types", Arrays.asList(1, 2, 3, 4, 5, 6, 7));
            postdata.put("activity_kinds", activityKinds);
            postdata.put("page", page);
            postdata.put("page_size", 15);
            br.getPage(br.createJSonPostRequest(API_BASE + "/list-blog-activity", postdata));
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> activityResponse = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final List<Object> timelineItems = (List<Object>) activityResponse.get("timelineItems");
            if (timelineItems.isEmpty()) {
                logger.info("Stopping because: Current page contains no more items");
                break pagination;
            }
            int numberofNewItems = 0;
            for (final Object timelineItemO : timelineItems) {
                final Map<String, Object> timelineItem = (Map<String, Object>) timelineItemO;
                final Map<String, Object> post = (Map<String, Object>) timelineItem.get("post");
                if (post == null) {
                    /* Cluster of e.g. likes/comments on other blogs -> not this blog's own content. */
                    continue;
                }
                final String postID = post.get("id").toString();
                if (!dupes.add(postID)) {
                    logger.info("Skipping dupe post: " + postID);
                    continue;
                }
                ret.addAll(crawlPostItems(param, post, fp));
                numberofNewItems++;
            }
            logger.info("Crawled page " + pageNumber + " | New items: " + numberofNewItems + " | Total: " + ret.size());
            if (this.isAbort()) {
                throw new InterruptedException();
            } else if (numberofNewItems == 0) {
                logger.info("Stopping because: Current page contained ONLY duplicates");
                break pagination;
            }
            final Map<String, Object> pageInfo = (Map<String, Object>) activityResponse.get("page");
            final String nextPageToken = pageInfo != null ? (String) pageInfo.get("nextPageToken") : null;
            if (nextPageToken == null) {
                logger.info("Stopping because: nextPageToken is null");
                break pagination;
            }
            page.put("page_token", nextPageToken);
            pageNumber++;
        } while (true);
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_PROFILE);
        }
        return ret;
    }

    /**
     * @deprecated Legacy HTML based crawling, still required for search URLs as the JSON API does not support search (yet).
     */
    @Deprecated
    private ArrayList<DownloadLink> crawlUserSearch(final CryptedLink param, final String username, final String searchKeyword) throws IOException, PluginException, InterruptedException {
        logger.info("Crawling all posts from user " + username + " matching search term '" + searchKeyword + "'");
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("This blog doesn't exist\\.\\s*<br>")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username + " search " + URLEncode.decodeURIComponent(searchKeyword));
        /* First check if there is already some downloadable content in the html of the current page. */
        ret.addAll(crawlPosts(fp));
        if (ret.isEmpty()) {
            logger.info("Didn't find anything in HTML");
        }
        /* Crawl first pagination page which typically contains less items than the rest */
        final String csrftoken = br.getRegex("name=\"csrf-token\" content=\"([^\"]+)\"").getMatch(0);
        if (csrftoken == null) {
            if (br.containsHTML("\"sorry\"\\s*>\\s*Sorry, please login")) {
                throw new AccountRequiredException();
            }
            logger.warning("Pagination failed");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find csrftoken");
        }
        final String infinitescrollDate = br.getRegex("class=\"infinitescroll\" data-time=\"(\\d{4}[^\"]+)\"").getMatch(0);
        if (infinitescrollDate == null) {
            logger.info("Stopping because: Pagination parameter 'infinitescroll' is not available");
            if (ret.isEmpty()) {
                /* No items found so far AND pagination impossible -> Something went seriously wrong */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find infinitescrollDate");
            }
            return ret;
        }
        br.getHeaders().put("Accept", "*/*");
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        br.getHeaders().put("Origin", "https://" + username + "." + this.getHost());
        br.getHeaders().put("Referer", "https://" + username + "." + this.getHost() + "/");
        br.getHeaders().put("X-CSRF-TOKEN", csrftoken);
        UrlQuery query = new UrlQuery();
        query.appendEncoded("scroll", "5");
        query.appendEncoded("timenow", infinitescrollDate);
        query.appendEncoded("blogname", username);
        query.append("keyword", searchKeyword, false);// taken from URL, already encoded
        query.appendEncoded("last", lastPostID);
        br.postPage("/infinitesearch", query);
        ret.addAll(crawlPosts(fp));
        if (this.isAbort()) {
            throw new InterruptedException();
        }
        final HashSet<String> dupes = new HashSet<String>();
        final int maxItemsPerPage = 20;
        int index = 0;
        int page = 1;
        profileLoop: do {
            query = new UrlQuery();
            query.appendEncoded("scroll", Integer.toString(index));
            query.appendEncoded("timenow", Encoding.urlEncode(infinitescrollDate));
            query.appendEncoded("last", lastPostID);
            query.appendEncoded("blogname", username);
            query.append("keyword", searchKeyword, false);// taken from URL, already encoded
            br.postPage("/infinitesearch", query);
            final ArrayList<DownloadLink> results = crawlPosts(fp);
            int numberofNewItems = 0;
            int numberofSkippedDuplicates = 0;
            if (results.isEmpty()) {
                logger.info("Failed to find any results on current page -> Probably it only contains offline items or text content");
            } else {
                for (final DownloadLink result : results) {
                    final String postID = result.getStringProperty(PROPERTY_POST_ID);
                    if (!dupes.add(postID)) {
                        /**
                         * 2023-03-31: This should never happen but it looks like it can happen. </br>
                         * As long as the current page we're crawling contains at least one new item, the crawler will continue even if
                         * there were some dupes.
                         */
                        logger.info("Skipping dupe: " + postID);
                        numberofSkippedDuplicates++;
                        continue;
                    }
                    ret.add(result);
                    numberofNewItems++;
                }
            }
            logger.info("Crawled page " + page + " | Index: " + index + " | New crawled items on this page: " + numberofNewItems + " | Crawled supported items total: " + ret.size() + " | lastPostID: " + lastPostID);
            if (this.isAbort()) {
                /* Aborted by user */
                throw new InterruptedException();
            } else if (lastPostID == null) {
                logger.info("Stopping because: lastPostID is null");
                break profileLoop;
            } else if (numberofNewItems == 0) {
                if (numberofSkippedDuplicates > 0) {
                    logger.info("Stopping because: Current page contained ONLY duplicates");
                    break profileLoop;
                } else {
                    logger.info("Current page contained ONLY deleted posts or unsupported posts which have been skipped: " + lastNumberofPosts);
                }
            }
            index += maxItemsPerPage;
            page++;
        } while (true);
        return ret;
    }

    private String            lastPostID        = null;
    private int               lastNumberofPosts = 0;
    private ArrayList<String> lastPostIDList    = new ArrayList<String>();

    /**
     * @deprecated Legacy HTML based post parsing, only used by the deprecated {@link #crawlUserSearch} anymore.
     */
    @Deprecated
    private ArrayList<DownloadLink> crawlPosts(final FilePackage fp) throws PluginException, IOException {
        lastPostID = null;
        lastNumberofPosts = 0;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String[][] posts = br.getRegex("(<div class=\"wrap-post del(\\d+)\\s*(pubvideo|typeimage|pubimage|typetext|typelink)\\s*\">(.*?)class=\"countinf\")").getMatches();
        for (final String[] postmatches : posts) {
            final String html = postmatches[0];
            final Regex postInfo = new Regex(html, "(https?://([\\w\\-]+)\\.[^/]+/post/(\\d+))");
            String postID = postInfo.getMatch(2);
            if (postID == null) {
                postID = postmatches[1];
            }
            lastPostID = postID;
            final String type = postmatches[2];
            // if (!type.matches("pubvideo|typeimage|pubimage")) {
            // logger.info("Skipping unsupported post type " + type + " | ID: " + postID);
            // continue;
            // }
            String username = postInfo.getMatch(1);
            if (username == null) {
                username = new Regex(br.getURL(), "https?://(.*?)\\.bdsmlr\\.com").getMatch(0);
            }
            String postURL = postInfo.getMatch(0);
            if (postURL == null && StringUtils.isAllNotEmpty(postID, username)) {
                postURL = br.getURL("/post/" + postID).toString();
            }
            if (postURL == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String referer_url = "https://" + username.toLowerCase(Locale.ROOT) + "." + br.getHost(false) + "/";
            if (type.equalsIgnoreCase("typelink")) {
                /* Post containing links to external websites such as youtube.com. */
                int numberofAddedItems = 0;
                final String[] urls = HTMLParser.getHttpLinks(html, br.getURL());
                for (final String url : urls) {
                    if (this.canHandle(url)) {
                        /* Sjip items which would go back into this crawler. */
                        continue;
                    }
                    numberofAddedItems++;
                    final DownloadLink externalItem = this.createDownloadlink(url);
                    if (fp != null) {
                        externalItem._setFilePackage(fp);
                    }
                    ret.add(externalItem);
                    distribute(externalItem);
                }
                logger.info("Added external URLs: " + numberofAddedItems);
                if (numberofAddedItems == 0) {
                    logger.warning("Failed to find any resulting external URLs for post: " + postURL + " type:" + type);
                }
            } else {
                final Regex matches;
                /* Video posts will also contain URLs to video-thumbnails so let's make sure we only grab exactly what we want. */
                if ("pubvideo".equalsIgnoreCase(type)) {
                    matches = new Regex(html, "(?:\"|\\')(https?://[^/]+/uploads/videos/(\\d{4})/(\\d{2})[^\"\\']+\\.mp4)(?:\"|\\')");
                } else {
                    matches = new Regex(html, "(?:\"|\\')(https?://[^/]+/uploads/photos/(\\d{4})/(\\d{2})[^\"\\']+\\.[a-zA-Z0-9]{2,5})(?:\"|\\')");
                }
                if (!matches.patternFind()) {
                    logger.warning("Failed to find any media for post: " + postURL + " type:" + type);
                    continue;
                }
                final String[][] directs = matches.getMatches();
                final HashSet<String> dups = new HashSet<String>();
                for (final String direct[] : directs) {
                    final String directurl = direct[0];
                    if (!dups.add(directurl)) {
                        /* Skip duplicates */
                        logger.info("Skipping dupe: " + directurl);
                        continue;
                    }
                    if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && !new Regex(directurl, PATTERN_DIRECT).patternFind()) {
                        logger.warning("!!DEV!! Found direct-URL which does not fit known direct-URL pattern -> " + directurl);
                    }
                    final String year = direct[1];
                    final String month = direct[2];
                    final DownloadLink link = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(directurl));
                    if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                        /* Make direct-urls easier accessible for plugin developers. */
                        link.setContentUrl(directurl);
                    } else {
                        link.setContentUrl(postURL);
                    }
                    if (dups.size() > 1) {
                        link.setFinalFileName(username + "_" + year + "_" + month + "_" + postID + "_" + dups.size() + Plugin.getFileNameExtensionFromURL(directurl));
                    } else {
                        link.setFinalFileName(username + "_" + year + "_" + month + "_" + postID + Plugin.getFileNameExtensionFromURL(directurl));
                    }
                    if (fp != null) {
                        link._setFilePackage(fp);
                    }
                    link.setAvailable(true);
                    link.setProperty(PROPERTY_POST_ID, postID);
                    /* Important! Some URLs cannot be accessed without the expected referrer value and server will return error 403! */
                    link.setReferrerUrl(referer_url);
                    ret.add(link);
                    distribute(link);
                }
            }
        }
        /* Special handling for possible empty pages with ID for next page */
        final String[] countinfs = br.getRegex("<div class=\"countinf\" data-id=\"(\\d+)\" style=\"display:none\"[^>]*></div>").getColumn(0);
        if (countinfs != null && countinfs.length > 0) {
            final String lastPostID2 = countinfs[countinfs.length - 1];
            if (!lastPostID2.equals(this.lastPostID)) {
                logger.info("Override lastPostID via countinf handling | Old: " + this.lastPostID + " | New: " + lastPostID2);
                this.lastPostID = lastPostID2;
            }
        }
        lastNumberofPosts = posts.length;
        lastPostIDList.add(this.lastPostID);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* 2023-03-31: Debug statement to easily compare pagination IDs of plugin vs. browser. */
            logger.info("lastPostIDList: " + lastPostIDList);
        }
        return ret;
    }
}
