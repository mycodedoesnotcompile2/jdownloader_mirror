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
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

import org.appwork.utils.DebugMode;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
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
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.BdsmlrCom;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 51913 $", interfaceVersion = 3, names = {}, urls = {})
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
            ret.add("https?://[\\w-]+\\." + buildHostsPatternPart(domains) + "(/.+)?");
        }
        return ret.toArray(new String[0]);
    }

    private static final Pattern PATTERN_USER_PROFILE = Pattern.compile("https?://([\\w-]+)\\.[^/]+(/.+)?", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_POST         = Pattern.compile("/post/(\\d+)$", Pattern.CASE_INSENSITIVE);
    /* Direct-URLs typically contain CDN host "cdnXXX." or "ocdnXXX." where "XXX" is numbers. */
    private static final Pattern PATTERN_DIRECT       = Pattern.compile("https?://[^/]+/uploads/.+", Pattern.CASE_INSENSITIVE);
    private static final String  PROPERTY_POST_ID     = "post_id";

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
        if (new Regex(contenturl, PATTERN_POST).patternFind()) {
            return crawlPost(param);
        } else {
            return crawlUser(param, account);
        }
    }

    private ArrayList<DownloadLink> crawlPost(final CryptedLink param) throws IOException, PluginException {
        final String contenturl = param.getCryptedUrl();
        final Regex urlinfo = new Regex(contenturl, PATTERN_POST);
        final String username = urlinfo.getMatch(0);
        final String postID = urlinfo.getMatch(1);
        if (username == null) {
            /* Developer mistake! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 500) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username + "_" + postID);
        return crawlPosts(fp);
    }

    private ArrayList<DownloadLink> crawlUser(final CryptedLink param, final Account account) throws IOException, PluginException, InterruptedException {
        String contenturl = param.getCryptedUrl();
        final String username = new Regex(contenturl, PATTERN_USER_PROFILE).getMatch(0);
        if (username == null) {
            /* Developer mistake! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String searchKeyword = new Regex(contenturl, "(?i)/search/([^/]+)").getMatch(0);
        if (searchKeyword != null) {
            logger.info("Crawling all posts from user " + username + " matching search term '" + searchKeyword + "'");
        } else {
            /* Normalize url */
            contenturl = "https://" + username + "." + getHost() + "/";
            logger.info("Crawling all posts from user " + username);
        }
        if (searchKeyword != null && account == null) {
            /* 2025-06-03: Accessing search URLs in a logged out state will result in http error 500. */
            throw new AccountRequiredException("Account required to crawl search URLs");
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("This blog doesn't exist\\.\\s*<br>")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (searchKeyword != null) {
            fp.setName(username + " search " + URLEncode.decodeURIComponent(searchKeyword));
        } else {
            fp.setName(username);
        }
        if (account == null && br.containsHTML(">\\s*This blog contains adult content and you're only seeing a review of it")) {
            /* Inform user that an account may be needed to crawl all items. */
            this.displayBubbleNotification(fp.getName() + " | Adult content warning", "You may need to add an account to be able to crawl all items.");
        }
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
        if (searchKeyword != null) {
            query.appendEncoded("blogname", username);
            query.append("keyword", searchKeyword, false);// taken from URL, already encoded
            query.appendEncoded("last", lastPostID);
            br.postPage("/infinitesearch", query);
        } else {
            br.postPage("/loadfirst", query);
        }
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
            if (searchKeyword != null) {
                query.appendEncoded("blogname", username);
                query.append("keyword", searchKeyword, false);// taken from URL, already encoded
                br.postPage("/infinitesearch", query);
            } else {
                br.postPage("/infinitepb2/" + username, query);
            }
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
