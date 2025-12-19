//jDownloader - Downloadmanager
//Copyright (C) 2013  JD-Team support@jdownloader.org
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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.config.TumblrComConfig;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.hoster.TumblrCom;
import jd.utils.JDUtilities;

@DecrypterPlugin(revision = "$Revision: 52014 $", interfaceVersion = 3, names = { "tumblr.com" }, urls = { "https?://(?![a-z0-9]+\\.media\\.tumblr\\.com/.+)[\\w\\.-]+tumblr\\.com/.*" })
public class TumblrComDecrypter extends PluginForDecrypt {
    public TumblrComDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final String  TYPE_INVALID                        = "https?://(?:(?:api|platform|embed|assets)\\.)tumblr\\.com/.+";
    private static final Pattern TYPE_POST                           = Pattern.compile("https?://([\\w-]+)\\.[^/]+/post/(\\d+)(/([a-z0-9-]+))?", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_POST_2                         = Pattern.compile("https?://[^/]+/blog/view/([\\w-]+)/(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_POST_3                         = Pattern.compile("https?://[^/]+/([\\w-]+)/(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_IMAGE                          = Pattern.compile(".+tumblr\\.com/image/\\d+", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_USER_OLD_1                     = Pattern.compile("https?://(?!www\\.)([^/]+)\\.tumblr\\.com/.*", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_USER_OLD_2                     = Pattern.compile("https?://(?:www\\.)?tumblr\\.com/blog/view/([^/]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_USER_NEW                       = Pattern.compile("https?://(?:www\\.)?tumblr\\.com/([\\w-]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_USER_OWN_LIKES                 = Pattern.compile("https?://[^/]+/likes$", Pattern.CASE_INSENSITIVE);
    private static final String  PROPERTY_TAGS                       = "tags";
    private static final String  PROPERTY_DATE                       = "date";
    private static String        anonymousApikey                     = "";
    private static long          anonymousApikeyLastRefreshTimestamp = 0;
    private TumblrComConfig      cfg                                 = null;

    private boolean isBlog(final String url) {
        return new Regex(url, TYPE_USER_NEW).patternFind() || new Regex(url, TYPE_USER_OLD_2).patternFind() || new Regex(url, TYPE_USER_OLD_1).patternFind();
    }

    private boolean isSinglePost(final String url) {
        return new Regex(url, TYPE_POST).patternFind() || new Regex(url, TYPE_POST_2).patternFind() || new Regex(url, TYPE_POST_3).patternFind();
    }

    private boolean isAccountRequired(final String url) {
        return new Regex(url, TYPE_USER_OWN_LIKES).patternFind();
    }

    @SuppressWarnings("deprecation")
    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        if (contenturl.matches(TYPE_INVALID)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String blog = null;
        String puid = null;
        final Regex postregex1 = new Regex(param.getCryptedUrl(), TYPE_POST);
        final Regex postregex2 = new Regex(param.getCryptedUrl(), TYPE_POST_2);
        final Regex postregex3 = new Regex(param.getCryptedUrl(), TYPE_POST_3);
        if (postregex1.patternFind()) {
            blog = postregex1.getMatch(0);
            puid = postregex1.getMatch(1);
        } else if (postregex2.patternFind()) {
            blog = postregex2.getMatch(0);
            puid = postregex2.getMatch(1);
        } else if (postregex3.patternFind()) {
            blog = postregex3.getMatch(0);
            puid = postregex3.getMatch(1);
        } else {
            blog = getUsername(param.getCryptedUrl());
        }
        final boolean isSinglePost = blog != null && puid != null;
        final boolean isBlog = isBlog(contenturl);
        this.cfg = PluginJsonConfig.get(TumblrComConfig.class);
        final Account account = AccountController.getInstance().getValidAccount(JDUtilities.getPluginForHost(getHost()));
        if (account == null && isAccountRequired(contenturl)) {
            throw new AccountRequiredException();
        }
        if (account != null) {
            /* Login whenever possible to be able to download account-only-stuff */
            final TumblrCom plg = (TumblrCom) this.getNewPluginForHostInstance(getHost());
            plg.login(account, false);
            if (isSinglePost) {
                return crawlPostAPI(param, blog, puid);
            } else if (new Regex(contenturl, TYPE_USER_OWN_LIKES).patternFind()) {
                return crawlUsersOwnLikes(param);
            } else if (isBlog) {
                return crawlUserAPI(param, blog);
            } else {
                logger.warning("Unsupported URL");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } else {
            br.setFollowRedirects(true);
            /* 2021-04-13: We need this cookie! Seems like any random boolean value is fine. */
            br.setCookie(getHost(), "euconsent-v2", "true");
            if (isSinglePost) {
                /* 2025-12-17: API did not work here -> Use website */
                // this.getAndSetAnonymousApikey(this.br);
                // return crawlPostAPI(param, blog, puid);
                return this.crawlSinglePostWebsite(blog, puid);
            } else if (isBlog) {
                /* Get anonymous API key from website, then use API to crawl blog/user. */
                this.getAndSetAnonymousApikey(this.br);
                return crawlUserAPI(param, blog);
            } else {
                logger.warning("Unsupported URL");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
    }

    private void getAndSetAnonymousApikey(final Browser brc) throws IOException, PluginException {
        synchronized (anonymousApikey) {
            if (anonymousApikey.isEmpty() || System.currentTimeMillis() - anonymousApikeyLastRefreshTimestamp > 60 * 60 * 1000) {
                logger.info("Obtaining apikey from website");
                /* 2021-04-13: Not all sub-pages contain the apikey -> Use mainpage */
                // br.getPage(param.getCryptedUrl());
                brc.getPage("https://www." + getHost() + "/");
                final String apikey = PluginJSonUtils.getJson(brc, "API_TOKEN");
                if (StringUtils.isEmpty(apikey)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                anonymousApikey = apikey;
                anonymousApikeyLastRefreshTimestamp = System.currentTimeMillis();
            }
        }
        brc.getHeaders().put("Authorization", "Bearer " + anonymousApikey);
    }

    private String getUsername(final String source) {
        String url_username = new Regex(source, TYPE_USER_OLD_2).getMatch(0);
        if (url_username != null) {
            return url_username;
        }
        url_username = new Regex(source, TYPE_USER_NEW).getMatch(0);
        if (url_username != null) {
            return url_username;
        }
        url_username = new Regex(source, TYPE_USER_OLD_1).getMatch(0);
        return url_username;
    }

    /** https://www.tumblr.com/docs/en/api/v2#postspost-id---fetching-a-post-neue-post-format */
    private ArrayList<DownloadLink> crawlPostAPI(final CryptedLink param, final String blog, final String puid) throws Exception {
        if (param == null || blog == null || puid == null) {
            throw new IllegalArgumentException();
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage(TumblrCom.API_BASE + "/blog/" + blog + "/posts/" + puid);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        entries = (Map<String, Object>) entries.get("response");
        this.crawlSinglePostJsonAPI(ret, entries, JsonSchemeType.API);
        return ret;
    }

    /** Wrapper */
    private void crawlMultiplePostsArrayJsonAPI(final ArrayList<DownloadLink> ret, final List<Map<String, Object>> posts) {
        int numberofSkippedItems = 0;
        for (final Map<String, Object> post : posts) {
            final String object_type = post.get("object_type").toString();
            if (!object_type.equalsIgnoreCase("post")) {
                numberofSkippedItems++;
                continue;
            }
            this.crawlSinglePostJsonAPI(ret, post, JsonSchemeType.API);
        }
        logger.info("Number of skipped non-post items (e.g. ads): " + numberofSkippedItems);
    }

    /** Wrapper */
    private void crawlMultiplePostsArrayJsonWebsite(final ArrayList<DownloadLink> ret, final List<Map<String, Object>> posts) {
        int numberofSkippedItems = 0;
        for (final Map<String, Object> post : posts) {
            final String object_type = post.get("objectType").toString();
            if (!object_type.equalsIgnoreCase("post")) {
                numberofSkippedItems++;
                continue;
            }
            this.crawlSinglePostJsonAPI(ret, post, JsonSchemeType.WEBSITE);
        }
        logger.info("Number of skipped non-post items (e.g. ads): " + numberofSkippedItems);
    }

    private void crawlSinglePostJsonAPI(final ArrayList<DownloadLink> ret, final Map<String, Object> entries, final JsonSchemeType scheme) {
        // final String postURL = (String) entries.get("post_url");
        final String blogName;
        if (scheme == JsonSchemeType.WEBSITE) {
            blogName = entries.get("blogName").toString();
        } else {
            blogName = entries.get("blog_name").toString();
        }
        /* Slug can be empty */
        final String slug = (String) entries.get("slug");
        String fpName = blogName;
        if (!StringUtils.isEmpty(slug)) {
            fpName += " - " + slug.replace("-", " ").trim();
        }
        String dateFormatted = null;
        final Object timestampO = entries.get("timestamp");
        if (timestampO != null && timestampO instanceof Number) {
            final long timestamp = ((Number) timestampO).longValue();
            dateFormatted = new SimpleDateFormat("yyyy-MM-dd", Locale.ENGLISH).format(new Date(timestamp * 1000));
        } else {
            logger.warning("Timestamp missing for: " + fpName + "|" + timestampO);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(fpName);
        final List<Object> repostList = (List<Object>) entries.get("trail");
        final List<Object> contentArrays = new ArrayList<Object>();
        contentArrays.add(entries.get("content"));
        for (final Object repostO : repostList) {
            final Map<String, Object> contentMap = (Map<String, Object>) repostO;
            final Object contentO = contentMap.get("content");
            if (contentO != null) {
                contentArrays.add(contentO);
            }
        }
        for (final Object contentArrayO : contentArrays) {
            final List<Object> ressourcelist = (List<Object>) contentArrayO;
            for (final Object contentO : ressourcelist) {
                final Map<String, Object> post = (Map<String, Object>) contentO;
                /* Possible types: image, text, link, video */
                final String type = post.get("type").toString();
                if (!type.matches("image|video")) {
                    /* Skip unsupported content e.g. "text" */
                    logger.info("Skipping unsupported type: " + type);
                    continue;
                }
                /* E.g. "tumblr"(or null!) = selfhosted, "instagram" or others = extern content/embedded" */
                final String provider = (String) post.get("provider");
                final DownloadLink dl;
                if (type.equals("video")) {
                    /* Videos only have 1 version available */
                    String url;
                    if (post.containsKey("url")) {
                        url = (String) post.get("url");
                    } else {
                        url = (String) JavaScriptEngineFactory.walkJson(post, "media/url");
                    }
                    if (StringUtils.isEmpty(url)) {
                        logger.warning("Bad video object");
                        continue;
                    }
                    url = convertDirectVideoUrltoHD(url);
                    dl = this.createDownloadlink(url);
                } else {
                    final List<Map<String, Object>> versions = (List<Map<String, Object>>) post.get("media");
                    if (versions.isEmpty()) {
                        /* This should never happen */
                        logger.warning("Found empty media versions array");
                        continue;
                    }
                    /*
                     * Look for best/original version (Array is usually sorted by quality, first is best).
                     */
                    final Map<String, Object> version = versions.get(0);
                    // for (final Object versionO : versionsO) {
                    // final Map<String, Object> versionInfoTmp = (Map<String, Object>) versionO;
                    // if (versionInfoTmp.containsKey("has_original_dimensions")) {
                    // versionInfo = versionInfoTmp;
                    // }
                    // }
                    // if (versionInfo == null) {
                    // /* Fallback: Use first version */
                    // versionInfo = (Map<String, Object>) versionsO.get(0);
                    // }
                    String url = (String) version.get("url");
                    if (url.endsWith(".gifv") && cfg.isPreferMp4OverGifv()) {
                        /*
                         * 2022-02-16: All gifv content should be available as mp4 files too --> Download mp4 instead of gifv files if
                         * wished by user.
                         */
                        url = url.substring(0, url.lastIndexOf(".")) + ".mp4";
                    }
                    dl = this.createDownloadlink(url);
                    if (StringUtils.endsWithCaseInsensitive(url, ".pnj")) {
                        /*
                         * 2025-12-18: .png image with fake .pnj ending -> Set final filename here to fix this in GUI for user. Looks to be
                         * some kind of hotlinking protection.
                         */
                        final String mimeType = version.get("type").toString();
                        String realExt = this.getExtensionFromMimeType(mimeType);
                        if (realExt != null) {
                            if (!realExt.startsWith(".")) {
                                realExt = "." + realExt;
                            }
                            final String filenameFromURL = extractFileNameFromURL(url);
                            final String filenameFixed = filenameFromURL.replaceFirst("(?i)\\.pnj$", realExt);
                            dl.setName(filenameFixed);
                        } else {
                            logger.warning("Cannot fix file extension for item: " + url);
                        }
                    }
                    /* 2021-04-09: url can contain up to 2 MD5 hashes but neither of those is the actual file-hash! */
                    // final String md5 = new Regex(url, "([a-f0-9]{32})\\.\\w+$").getMatch(0);
                    // if (md5 != null) {
                    // dl.setMD5Hash(md5);
                    // }
                }
                /* Allow external content to go back into another crawler (e.g. Instagram URLs). */
                if (provider == null || provider.equalsIgnoreCase("tumblr")) {
                    dl.setAvailable(true);
                }
                // if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && !StringUtils.isEmpty(postURL)) {
                // dl.setContentUrl(postURL);
                // }
                if (dateFormatted != null) {
                    dl.setProperty(PROPERTY_DATE, dateFormatted);
                }
                dl._setFilePackage(fp);
                ret.add(dl);
                distribute(dl);
            }
        }
    }

    private ArrayList<DownloadLink> crawlSinglePostWebsite(final String blog, final String puid) throws PluginException, IOException {
        br.getPage("https://www." + getHost() + "/" + blog + "/" + puid);
        this.checkErrorsWebsite(br);
        /* Get json which contains the same object-types returned by their official API. */
        final String json = br.getRegex("<script type=\"application/json\" id=\"___INITIAL_STATE___\">(.*?)</script>").getMatch(0);
        final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
        final List<Map<String, Object>> posts = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(entries, "PeeprRoute/initialTimeline/objects");
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        this.crawlMultiplePostsArrayJsonWebsite(ret, posts);
        return ret;
    }

    /** https://www.tumblr.com/docs/en/api/v2#userlikes--retrieve-a-users-likes */
    private ArrayList<DownloadLink> crawlUsersOwnLikes(final CryptedLink param) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        /* TODO: Implement pagination */
        br.getPage(TumblrCom.API_BASE + "/user/likes");
        Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        entries = (Map<String, Object>) entries.get("response");
        final int likedCount = ((Number) entries.get("liked_count")).intValue();
        if (likedCount <= 0) {
            logger.info("Currently loggedIN user has no likes at all");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        logger.info("Crawling " + likedCount + " liked posts...");
        final List<Map<String, Object>> likedPosts = (List<Map<String, Object>>) entries.get("liked_posts");
        this.crawlMultiplePostsArrayJsonAPI(ret, likedPosts);
        return ret;
    }

    /**
     * Crawls all posts of a blog via API: </br>
     * https://www.tumblr.com/docs/en/api/v2#posts--retrieve-published-posts
     */
    private ArrayList<DownloadLink> crawlUserAPI(final CryptedLink param, final String username) throws Exception {
        if (param == null || username == null) {
            throw new IllegalArgumentException();
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        // br.getPage(TumblrCom.API_BASE + "/blog/" + Encoding.urlEncode(username) + ".tumblr.com/info");
        br.getPage(TumblrCom.API_BASE + "/blog/" + Encoding.urlEncode(username) + ".tumblr.com/posts");
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> blog = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "response/blog");
        final int postsCount = ((Number) blog.get("posts")).intValue();
        if (postsCount == 0) {
            logger.info("This blog doesn't contain any posts");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        int pageIndex = 0;
        int crawledPosts = 0;
        pagination: do {
            final List<Map<String, Object>> posts = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(entries, "response/posts");
            final ArrayList<DownloadLink> nextPosts = new ArrayList<DownloadLink>();
            this.crawlMultiplePostsArrayJsonAPI(nextPosts, posts);
            ret.addAll(nextPosts);
            final String nextPageURL = (String) JavaScriptEngineFactory.walkJson(entries, "response/_links/next/href");
            crawledPosts += posts.size();
            logger.info("Crawled page: " + (pageIndex + 1) + " | Crawled posts: " + crawledPosts + " / " + postsCount);
            if (this.isAbort()) {
                break pagination;
            } else if (StringUtils.isEmpty(nextPageURL)) {
                logger.info("Stopping because: nextURL is not given -> Probably reached end");
                break pagination;
            } else if (crawledPosts >= postsCount) {
                logger.info("Stopping because: Found all items");
                break pagination;
            }
            /* Continue to next page */
            pageIndex++;
            br.getPage(TumblrCom.API_BASE_WITHOUT_VERSION + nextPageURL);
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } while (true);
        return ret;
    }

    @Deprecated
    private boolean handlePassword(final CryptedLink param) throws DecrypterException, IOException {
        if (!this.isPasswordRequired(br)) {
            return false;
        }
        logger.info("Blog password needed");
        // final String password_required_url;
        // if (this.br.getRedirectLocation() != null) {
        // password_required_url = this.br.getRedirectLocation();
        // } else {
        // password_required_url = this.br.getURL();
        // }
        // final String blog_user = new Regex(password_required_url, "/blog_auth/(.+)").getMatch(0);
        // if (blog_user != null) {
        // this.br = prepBR(new Browser());
        // this.br.setFollowRedirects(true);
        // this.br.getPage("https://www.tumblr.com/blog_auth/" + blog_user);
        // } else {
        // this.br.setFollowRedirects(true);
        // }
        boolean success = false;
        String passCode = null;
        for (int i = 0; i <= 2; i++) {
            if (param.getDecrypterPassword() != null && i == 0) {
                /* Try saved/given password if possible. */
                passCode = param.getDecrypterPassword();
            } else {
                passCode = getUserInput("Password?", param);
            }
            Form form = br.getFormbyKey("auth");
            if (form == null) {
                form = br.getFormbyKey("password");
            }
            form.put("password", Encoding.urlEncode(passCode));
            br.submitForm(form);
            form = br.getFormbyKey("auth");
            if (form != null) {
                form.put("password", Encoding.urlEncode(passCode));
                br.submitForm(form);
            }
            if (!this.isPasswordRequired(br)) {
                success = true;
                break;
            }
            passCode = null;
            continue;
        }
        if (!success) {
            throw new DecrypterException(DecrypterException.PASSWORD);
        }
        param.setDecrypterPassword(passCode);
        this.br.setFollowRedirects(false);
        return true;
    }

    private boolean isPasswordRequired(final Browser br) {
        final String urlpart_passwordneeded = "/blog_auth";
        if (StringUtils.containsIgnoreCase(br.getRedirectLocation(), urlpart_passwordneeded)) {
            return true;
        } else if (StringUtils.containsIgnoreCase(br.getURL(), urlpart_passwordneeded)) {
            return true;
        }
        return false;
    }

    private void checkErrorsWebsite(final Browser br) throws PluginException {
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getURL().contains("/login_required/")) {
            throw new AccountRequiredException();
        }
    }

    private String convertDirectVideoUrltoHD(final String source) {
        if (source == null) {
            return null;
        }
        if (source.matches("https?://vt\\.tumblr\\.com/.+")) {
            /* We already have an HD url */
            return source;
        } else {
            final String hd_final_url_part = new Regex(source, "/(tumblr_[^/]+)/\\d+$").getMatch(0);
            if (hd_final_url_part != null) {
                /* Yey we have an HD url ... */
                return "https://vt.tumblr.com/" + hd_final_url_part + ".mp4";
            } else {
                /* We can't create an HD url. */
                return source;
            }
        }
    }

    public static enum JsonSchemeType {
        API,
        WEBSITE;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}