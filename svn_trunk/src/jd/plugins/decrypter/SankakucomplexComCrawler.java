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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.config.SankakucomplexComConfig;
import org.jdownloader.plugins.components.config.SankakucomplexComConfig.AccessMode;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.SankakucomplexCom;

@DecrypterPlugin(revision = "$Revision: 51305 $", interfaceVersion = 3, names = {}, urls = {})
public class SankakucomplexComCrawler extends PluginForDecrypt {
    public SankakucomplexComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    /*
     * 2025-04-22: Looks like API doesn't work anymore or at least it fails for a lot of items so for now let's always prefer website in
     * auto mode.
     */
    private static final boolean ACCESS_MODE_AUTO_PREFER_API_MODE = false;

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "sankakucomplex.com" });
        return ret;
    }

    @Override
    public void init() {
        super.init();
        Browser.setRequestIntervalLimitGlobal(getHost(), 1000);
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
            String regex = "https?://(?:(beta|www|chan)\\.)?" + buildHostsPatternPart(domains) + "(";
            regex += TYPE_BOOK.pattern();
            regex += "|";
            regex += TYPE_TAGS_BOOKS.pattern();
            regex += "|";
            regex += TYPE_TAGS_POSTS.pattern();
            regex += ")";
            ret.add(regex);
        }
        return ret.toArray(new String[0]);
    }

    private static final Pattern TYPE_BOOK       = Pattern.compile("/(([a-z]{2,3})/?)?books/([A-Za-z0-9]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_TAGS_BOOKS = Pattern.compile("/(([a-z]{2,3})/?)?books\\?tags=([^&]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_TAGS_POSTS = Pattern.compile("/(([a-z]{2,3})/?)?(?:posts)?\\?tags=([^&]+)", Pattern.CASE_INSENSITIVE);
    public static final String   API_BASE        = "https://capi-v2.sankakucomplex.com";
    private SankakucomplexCom    hosterplugin    = null;

    @Override
    public void clean() {
        hosterplugin = null;
        super.clean();
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account != null) {
            hosterplugin = (SankakucomplexCom) this.getNewPluginForHostInstance(this.getHost());
            hosterplugin.login(account, false);
        }
        final String contenturl = param.getCryptedUrl();
        if (new Regex(contenturl, TYPE_TAGS_BOOKS).patternFind()) {
            return crawlTagsBooksAPI(param);
        } else if (new Regex(contenturl, TYPE_BOOK).patternFind()) {
            return crawlBook(param);
        } else {
            return crawlTagsPosts(param, account);
        }
    }

    private ArrayList<DownloadLink> crawlTagsPosts(final CryptedLink param, final Account account) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final SankakucomplexComConfig cfg = PluginJsonConfig.get(SankakucomplexComConfig.class);
        final int maxPage = cfg.getPostTagCrawlerMaxPageLimit();
        if (maxPage == 0) {
            logger.info("Stopping because: User disabled posts tag crawler");
            return ret;
        }
        final Regex urlinfo = new Regex(param.getCryptedUrl(), TYPE_TAGS_POSTS);
        final String language = urlinfo.getMatch(1);
        String tags = urlinfo.getMatch(2);
        if (tags == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        tags = URLEncode.decodeURIComponent(tags.replace("+", " "));
        final AccessMode mode = cfg.getPostTagCrawlerAccessMode();
        /**
         * Some items are only visible for logged in users and are never returned via API. </br>
         * For this reason, some user may prefer website mode.
         */
        if (mode == AccessMode.API || (mode == AccessMode.AUTO && ACCESS_MODE_AUTO_PREFER_API_MODE)) {
            return crawlTagsPostsAPI(account, param, tags, language);
        } else {
            return crawlTagsPostsWebsite(account, param, tags, language);
        }
    }

    private String removeDiv(String input, String divStart) {
        if (divStart == null) {
            return input;
        }
        final int startIndex = input.indexOf(divStart);
        if (startIndex == -1) {
            return input;
        }
        int nextDivIndex = startIndex;
        int divCount = 1;
        while (true) {
            int nextOpen = input.indexOf("<div", nextDivIndex);
            int nextClose = input.indexOf("</div>", nextDivIndex);
            if (nextOpen == -1) {
                break;
            }
            if (nextOpen < nextClose) {
                divCount++;
                nextDivIndex = nextOpen + 1;
            } else if (nextClose < nextOpen) {
                divCount--;
                nextDivIndex = nextClose + "</div>".length();
            }
            if (divCount == 1) {
                break;
            }
        }
        if (startIndex == nextDivIndex) {
            return input;
        }
        // String removeThis= input.substring(startIndex, nextDivIndex);
        final StringBuilder sb = new StringBuilder(input);
        logger.info("removeDiv:" + divStart);
        sb.replace(startIndex, nextDivIndex, "");
        return sb.toString();
    }

    private ArrayList<DownloadLink> crawlTagsPostsWebsite(final Account account, final CryptedLink param, final String tags, final String language) throws Exception {
        if (tags == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String langPart = "";
        if (language != null) {
            langPart = language + "/";
        }
        final String tagsUrlEncoded = URLEncode.encodeURIComponent(tags);
        br.getPage("https://chan.sankakucomplex.com/" + langPart + "posts?tags=" + tagsUrlEncoded);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getURL().contains("/premium")) {
            /* Account required (most times due to adult content so free account is enough). */
            throw new AccountRequiredException();
        }
        if (account != null && !hosterplugin.isLoggedin(br)) {
            hosterplugin.login(account, "https://chan.sankakucomplex.com/" + langPart + "posts?tags=" + tagsUrlEncoded, true);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final SankakucomplexComConfig cfg = PluginJsonConfig.get(SankakucomplexComConfig.class);
        final int maxPageUserLimit = cfg.getPostTagCrawlerMaxPageLimit();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(tags);
        int page = 1;
        String numberofItemsStr = br.getRegex("class=\"tag-count\"[^>]*>([^<]+)</span>").getMatch(0);
        final int numberofItems;
        if (numberofItemsStr == null) {
            logger.warning("Failed to find expected number of items");
            numberofItems = -1;
        } else if (numberofItemsStr.endsWith("K") || numberofItemsStr.endsWith("k")) {
            String numPart = numberofItemsStr.substring(0, numberofItemsStr.length() - 1);
            double num = Double.parseDouble(numPart);
            numberofItems = (int) (num * 1000);
        } else if (numberofItemsStr.matches("\\d+")) {
            numberofItems = Integer.parseInt(numberofItemsStr);
        } else {
            logger.warning("Total number of items string has unsupported format: " + numberofItemsStr);
            numberofItems = -1;
        }
        logger.info("Total number of items: " + numberofItems);
        final HashSet<String> dupes = new HashSet<String>();
        int position = 1;
        pagination: do {
            String html = br.getRequest().getHtmlCode();
            /**
             * Remove unwanted items / ads from html source. Typically this is only relevant for the html code of the first page. </br>
             * Typically this removes two kinds of items: Popular/Recommended items and ads for "Create your own ai art"
             */
            html = removeDiv(html, new Regex(html, "(<div[^>]*id\\s*=\\s*\"popular-preview\"[^>]*>)").getMatch(0));
            html = removeDiv(html, new Regex(html, "(<div[^>]*class\\s*=\\s*\"[^\"]*news-carousel\"[^>]*>)").getMatch(0));
            html = removeDiv(html, new Regex(html, "(<div[^>]*class\\s*=\\s*\"[^\"]*topbar-carousel\"[^>]*>)").getMatch(0));
            html = removeDiv(html, new Regex(html, "(<div[^>]*class\\s*=\\s*\"[^\"]*carousel-data-companion\"[^>]*>)").getMatch(0));
            final String[] postIDs = new Regex(html, "(?i)PostModeMenu.click\\('([A-Za-z0-9]+)'\\)").getColumn(0);
            if (postIDs == null || postIDs.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            int numberofNewItemsThisPage = 0;
            for (final String postID : postIDs) {
                if (!dupes.add(postID)) {
                    /* Skip dupes */
                    continue;
                }
                numberofNewItemsThisPage++;
                final DownloadLink link = this.createDownloadlink(generateSinglePostURL(postID, language, tagsUrlEncoded));
                link.setProperty(SankakucomplexCom.PROPERTY_POSITION_NUMBER, position);
                /* Set temporary filename -> Can be image or video file */
                final String tagsSpaceSeparated = br.getRegex("\"id\":\"" + postID + "\",\"tags\":\"([^\"]+)").getMatch(0);
                if (StringUtils.containsIgnoreCase(tagsSpaceSeparated, "mp4")) {
                    link.setProperty(SankakucomplexCom.PROPERTY_EXT_HINT, "mp4");
                    link.setName(postID + ".mp4");
                } else {
                    /* Image or fallback */
                    link.setName(postID + ".png");
                }
                if (!StringUtils.isEmpty(tagsSpaceSeparated)) {
                    final String tagsCommaSeparated = tagsSpaceSeparated.trim().replace(" ", ",");
                    link.setProperty(SankakucomplexCom.PROPERTY_TAGS_COMMA_SEPARATED, tagsCommaSeparated);
                    if (cfg.isSetCommaSeparatedTagsOfPostsAsComment()) {
                        link.setComment(tagsCommaSeparated);
                    }
                }
                if (cfg.isCrawlerFastLinkcheckEnabled()) {
                    link.setAvailable(true);
                }
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
                position++;
            }
            String nextPageURL = br.getRegex("next-page-url=\"([^\"]+)\"").getMatch(0);
            if (nextPageURL != null) {
                nextPageURL = nextPageURL + "&auto_page=t";// smaller html response
                nextPageURL = Encoding.htmlOnlyDecode(nextPageURL);
            }
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size() + "/" + numberofItemsStr + " | nextPageURL = " + nextPageURL);
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                throw new InterruptedException();
            } else if (page == maxPageUserLimit) {
                logger.info("Stopping because: Reached user defined max page limit of " + maxPageUserLimit);
                this.displayBubbleNotification("Tags: " + tags, "Stopping early because: Reached max user defined page: " + maxPageUserLimit + "\r\nYou can adjust this limit in the plugin settings.");
                break pagination;
            } else if (nextPageURL == null) {
                logger.info("Stopping because: Reached end(?)");
                break pagination;
            } else if (numberofNewItemsThisPage == 0) {
                logger.info("Stopping because: Current page contains no new items");
                break pagination;
            } else if (numberofItems != -1 && ret.size() >= numberofItems) {
                logger.info("Stopping because: Found all items -> " + numberofItems);
                break pagination;
            } else {
                /* Continue to next page */
                br.getPage(nextPageURL);
                page++;
                continue pagination;
            }
        } while (true);
        if (ret.size() < numberofItems) {
            this.displayBubbleNotification("Missing items!", "Tags: " + tags + "\r\nSome items look to be missing!\r\nFound only " + ret.size() + "/" + numberofItemsStr + " items");
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlTagsPostsAPI(final Account account, final CryptedLink param, final String tags, final String language) throws Exception {
        if (tags == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final SankakucomplexComConfig cfg = PluginJsonConfig.get(SankakucomplexComConfig.class);
        final int maxPage = cfg.getPostTagCrawlerMaxPageLimit();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(tags);
        final String tagsUrlEncoded = URLEncode.encodeURIComponent(tags);
        final int maxItemsPerPage = 40;
        final UrlQuery query = new UrlQuery();
        if (language != null) {
            query.appendEncoded("lang", language);
        }
        query.appendEncoded("limit", Integer.toString(maxItemsPerPage));
        query.appendEncoded("hide_posts_in_books", "in-larger-tags");
        query.append("tags", tagsUrlEncoded, false);
        int page = 1;
        int position = 1;
        pagination: do {
            br.getPage(API_BASE + "/posts/keyset?" + query.toString());
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Map<String, Object> meta = (Map<String, Object>) entries.get("meta");
            final String nextPageHash = (String) meta.get("next");
            final List<Map<String, Object>> data = (List<Map<String, Object>>) entries.get("data");
            for (final Map<String, Object> post : data) {
                final String postID = post.get("id").toString();
                final DownloadLink link = this.createDownloadlink(generateSinglePostURL(postID, language, tagsUrlEncoded));
                link.setProperty(SankakucomplexCom.PROPERTY_POSITION_NUMBER, position);
                SankakucomplexCom.parseFileInfoAndSetFilenameAPI(this, link, post);
                if (cfg.isCrawlerFastLinkcheckEnabled()) {
                    link.setAvailable(true);
                }
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
                position++;
            }
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break pagination;
            } else if (page == maxPage) {
                logger.info("Stopping because: Reached user defined max page limit of " + maxPage);
                this.displayBubbleNotification("Tags: " + tags, "Stopping early because: Reached max user defined page: " + maxPage + "\r\nYou can adjust this limit in the plugin settings.");
                break pagination;
            } else if (StringUtils.isEmpty(nextPageHash)) {
                logger.info("Stopping because: Reached end(?)");
                break pagination;
            } else if (data.size() < maxItemsPerPage) {
                logger.info("Stopping because: Current page contains less items than " + maxItemsPerPage);
                break pagination;
            } else {
                /* Continue to next page */
                page++;
                query.addAndReplace("next", Encoding.urlEncode(nextPageHash));
                continue pagination;
            }
        } while (true);
        if (ret.isEmpty()) {
            logger.info("Looks like users' given tag does not lead to any search results");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return ret;
    }

    /** Crawls books via tag. Typically used to crawl all books of a user. */
    private ArrayList<DownloadLink> crawlTagsBooksAPI(final CryptedLink param) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final SankakucomplexComConfig cfg = PluginJsonConfig.get(SankakucomplexComConfig.class);
        final int maxPage = cfg.getBookTagCrawlerMaxPageLimit();
        if (maxPage == 0) {
            logger.info("Stopping because: User disabled books tag crawler");
            return ret;
        }
        final Regex urlinfo = new Regex(param.getCryptedUrl(), TYPE_TAGS_BOOKS);
        final String languageFromURL = urlinfo.getMatch(1);
        String tags = urlinfo.getMatch(2);
        if (languageFromURL == null || tags == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        tags = URLEncode.decodeURIComponent(tags.replace("+", " "));
        final int maxItemsPerPage = 20;
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("lang", languageFromURL);
        query.appendEncoded("limit", Integer.toString(maxItemsPerPage));
        query.appendEncoded("includes[]", "series");
        query.add("tags", URLEncode.encodeURIComponent(tags));
        query.appendEncoded("pool_type", "0");
        int page = 1;
        pagination: do {
            br.getPage(API_BASE + "/pools/keyset?" + query.toString());
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Map<String, Object> meta = (Map<String, Object>) entries.get("meta");
            final String nextPageHash = (String) meta.get("next");
            final List<Map<String, Object>> data = (List<Map<String, Object>>) entries.get("data");
            if (data.isEmpty()) {
                if (ret.isEmpty()) {
                    logger.info("Looks like users' given tag does not lead to any search results");
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else {
                    logger.info("Stopping because: Current page is empty");
                    break pagination;
                }
            }
            for (final Map<String, Object> book : data) {
                final DownloadLink link = this.createDownloadlink("https://beta.sankakucomplex.com/" + languageFromURL + "/books/" + book.get("id") + "?tags=" + tags);
                ret.add(link);
                distribute(link);
            }
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break pagination;
            } else if (page == maxPage) {
                logger.info("Stopping because: Reached user defined max page limit of " + maxPage);
                this.displayBubbleNotification("Tags books: " + tags, "Stopping early because: Reached max user defined page: " + maxPage + "\r\nYou can adjust this limit in the plugin settings.");
                break pagination;
            } else if (StringUtils.isEmpty(nextPageHash)) {
                logger.info("Stopping because: Reached end(?)");
                break pagination;
            } else if (data.size() < maxItemsPerPage) {
                logger.info("Stopping because: Current page contains less items than " + maxItemsPerPage);
                break pagination;
            } else {
                /* Continue to next page */
                page++;
                query.addAndReplace("next", Encoding.urlEncode(nextPageHash));
                continue pagination;
            }
        } while (true);
        return ret;
    }

    /** Crawls all pages of a book. */
    private ArrayList<DownloadLink> crawlBook(final CryptedLink param) throws Exception {
        final Regex urlinfo = new Regex(param.getCryptedUrl(), TYPE_BOOK);
        String language = urlinfo.getMatch(1);
        final String bookID = urlinfo.getMatch(2);
        if (bookID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (language == null) {
            /* Fallback */
            language = "en";
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage(API_BASE + "/pools/" + bookID + "?lang=" + language + "&includes[]=series&exceptStatuses[]=deleted");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> author = (Map<String, Object>) entries.get("author");
        String bookTitle = (String) entries.get("name_en");
        if (StringUtils.isEmpty(bookTitle)) {
            bookTitle = (String) entries.get("name_ja");
        }
        final SankakucomplexComConfig cfg = PluginJsonConfig.get(SankakucomplexComConfig.class);
        final List<Map<String, Object>> posts = (List<Map<String, Object>>) entries.get("posts");
        int page = 0;
        for (final Map<String, Object> post : posts) {
            final DownloadLink link = this.createDownloadlink("https://beta.sankakucomplex.com/" + language + "/post/show/" + post.get("id") + "?tags=pool%3A" + bookID + "&page=" + page);
            link.setProperty(SankakucomplexCom.PROPERTY_BOOK_TITLE, bookTitle);
            link.setProperty(SankakucomplexCom.PROPERTY_PAGE_NUMBER, page);
            link.setProperty(SankakucomplexCom.PROPERTY_PAGE_NUMBER_MAX, posts.size() - 1);
            SankakucomplexCom.parseFileInfoAndSetFilenameAPI(this, link, post);
            if (cfg.isCrawlerFastLinkcheckEnabled()) {
                link.setAvailable(true);
            }
            ret.add(link);
            page++;
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(author.get("name") + " - " + bookTitle);
        fp.addLinks(ret);
        return ret;
    }

    private static String generateSinglePostURL(final String postID, String language, final String tagsUrlEncoded) {
        if (language == null) {
            language = "en";
        }
        return "https://beta.sankakucomplex.com/" + language + "/post/show/" + postID + "?tags=" + tagsUrlEncoded;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }
}
