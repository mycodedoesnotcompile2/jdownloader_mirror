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
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Files;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.config.InstagramConfig;
import org.jdownloader.plugins.components.config.InstagramConfig.ActionOnRateLimitReached;
import org.jdownloader.plugins.components.config.InstagramConfig.FilenameType;
import org.jdownloader.plugins.components.config.InstagramConfig.SinglePostPackagenameSchemeType;
import org.jdownloader.plugins.components.config.InstagramConfig.StoriesHighlightsPackagenameSchemeType;
import org.jdownloader.plugins.components.config.InstagramConfig.StoryPackagenameSchemeType;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
import jd.http.requests.GetRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
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
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.hoster.InstaGramCom;

@DecrypterPlugin(revision = "$Revision: 53833 $", interfaceVersion = 4, names = {}, urls = {})
public class InstaGramComDecrypter extends PluginForDecrypt {
    public InstaGramComDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.addAllowedResponseCodes(502);
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "instagram.com" });
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
            String regex = "https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(";
            regex += TYPE_GALLERY.pattern();
            regex += "|";
            regex += PATTERN_STORY_CURRENT.pattern();
            regex += "|";
            regex += PATTERN_STORY_SPECIFIC.pattern();
            regex += "|";
            regex += TYPE_LEGACY_HASHTAG.pattern();
            regex += "|";
            regex += TYPE_SEARCH_KEYWORD.pattern();
            regex += "|";
            regex += TYPE_PROFILE_SAVED_OBJECTS.pattern();
            regex += "|";
            regex += TYPE_PROFILE_TAGGED.pattern();
            regex += "|";
            regex += TYPE_PROFILE_REELS.pattern();
            regex += "|";
            regex += TYPE_PROFILE.pattern();
            regex += ")";
            ret.add(regex);
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY };
    }

    @Override
    public void init() {
        setRequestIntervalLimitGlobal();
    }

    public static void setRequestIntervalLimitGlobal() {
        final int limit = PluginJsonConfig.get(InstagramConfig.class).getGlobalRequestIntervalLimitMilliseconds();
        if (limit > 0) {
            Browser.setRequestIntervalLimitGlobal("instagram.com", limit);
            Browser.setRequestIntervalLimitGlobal("cdninstagram.com", limit);
        }
    }

    private static final Pattern       TYPE_GALLERY               = Pattern.compile("/(([^/]+)/)?(p|tv|reels?)/([A-Za-z0-9_-]+)/?", Pattern.CASE_INSENSITIVE);
    private static final Pattern       PATTERN_STORY_CURRENT      = Pattern.compile("/stories/([^/]+)/?$", Pattern.CASE_INSENSITIVE);
    private static final Pattern       PATTERN_STORY_SPECIFIC     = Pattern.compile("/stories/([^/]+)/(\\d+)/?", Pattern.CASE_INSENSITIVE);
    // private static final Pattern TYPE_STORY_HIGHLIGHTS = Pattern.compile("/stories/highlights/(\\d+)/?", Pattern.CASE_INSENSITIVE);
    private static final Pattern       TYPE_LEGACY_HASHTAG        = Pattern.compile("/explore/tags/([^/]+)/?$", Pattern.CASE_INSENSITIVE);
    private static final Pattern       TYPE_SEARCH_KEYWORD        = Pattern.compile("/explore/search/keyword/\\?q=([^&]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern       TYPE_PROFILE_SAVED_OBJECTS = Pattern.compile("/([^/]+)/saved.*", Pattern.CASE_INSENSITIVE);
    private static final Pattern       TYPE_PROFILE_TAGGED        = Pattern.compile("/([^/]+)/tagged/?$", Pattern.CASE_INSENSITIVE);
    private static final Pattern       TYPE_PROFILE_REELS         = Pattern.compile("/([^/]+)/reels/?$", Pattern.CASE_INSENSITIVE);
    private static final Pattern       TYPE_PROFILE               = Pattern.compile("/([^/]+)(?:/.*)?", Pattern.CASE_INSENSITIVE);
    /**
     * Shared across all plugin instances. Always access via getLazyCache/putLazyCache. <br>
     * LRU cache (access order): When the max size is exceeded, only the least recently used entry is removed.
     */
    private static Map<String, String> LAZY_CACHE                 = new LinkedHashMap<String, String>(16, 0.75f, true) {
                                                                      protected boolean removeEldestEntry(Map.Entry<String, String> eldest) {
                                                                          return size() > 500;
                                                                      };
                                                                  };

    private static String getLazyCache(final String key) {
        synchronized (LAZY_CACHE) {
            return LAZY_CACHE.get(key);
        }
    }

    private static void putLazyCache(final String key, final String value) {
        synchronized (LAZY_CACHE) {
            LAZY_CACHE.put(key, value);
        }
    }

    /**
     * For links matching pattern {@link #TYPE_LEGACY_HASHTAG} --> This will be set on created DownloadLink objects as a (packagizer-)
     * property.
     */
    private static Map<String, String> ID_TO_USERNAME             = new LinkedHashMap<String, String>() {
                                                                      protected boolean removeEldestEntry(Map.Entry<String, String> eldest) {
                                                                          return size() > 100;
                                                                      };
                                                                  };

    @SuppressWarnings({ "deprecation" })
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final AtomicBoolean loggedIN = new AtomicBoolean(false);
        final Account account = AccountController.getInstance().getValidAccount(getHost());
        final String contenturl = param.getCryptedUrl();
        if (this.requiresLogin(contenturl) && !loggedIN.get() && account == null) {
            /* E.g. saved users own objects can only be crawled when he's logged in ;) */
            throw new AccountRequiredException();
        }
        InstaGramCom.prepBRWebsite(this.br);
        br.addAllowedResponseCodes(new int[] { 502 });
        try {
            final String galleryID;
            final String hashtag;
            String username = null;
            final String searchTerm;
            final Regex regex_type_story;
            final UrlQuery query = UrlQuery.parse(contenturl);
            final String contentpath = new URL(contenturl).getPath();
            if (new Regex(contentpath, TYPE_PROFILE_SAVED_OBJECTS).patternFind()) {
                return this.crawlUserSavedObjectsFeedAltAPI(param, account, loggedIN);
            } else if ((galleryID = new Regex(contentpath, TYPE_GALLERY).getMatch(3)) != null) {
                /* Crawl single images & galleries */
                return crawlGallery(param, account, loggedIN, galleryID);
            } else if ((hashtag = new Regex(contentpath, TYPE_LEGACY_HASHTAG).getMatch(0)) != null) {
                return crawlHashtag(param, account, loggedIN, hashtag);
            } else if ((regex_type_story = new Regex(contentpath, PATTERN_STORY_SPECIFIC)).patternFind()) {
                return this.crawlStory(param, account, loggedIN, true, regex_type_story.getMatch(0), regex_type_story.getMatch(1));
            } else if ((username = new Regex(contentpath, PATTERN_STORY_CURRENT).getMatch(0)) != null) {
                return this.crawlStory(param, account, loggedIN, username, true);
            } else if ((username = new Regex(contentpath, TYPE_PROFILE_REELS).getMatch(0)) != null) {
                return this.crawlUserReels(param, account, loggedIN, username);
            } else if ((username = new Regex(contentpath, TYPE_PROFILE_TAGGED).getMatch(0)) != null) {
                return this.crawlUserTagged(param, account, loggedIN, username);
            } else if ((searchTerm = query.get("q")) != null) {
                return this.crawlSearchWithLimitErrorhandling(param, account, loggedIN, searchTerm);
            } else if ((username = new Regex(contentpath, TYPE_PROFILE).getMatch(0)) != null) {
                return this.crawlUser(param, account, loggedIN, username);
            } else {
                /* Unsupported URL */
                /**
                 * Special case: We cannot easily filter all unwanted links via plugin pattern thus users can add garbage links for example:
                 * https://www.instagram.com/?hl=ar <br>
                 * --> Silently ignore such links!
                 */
                logger.info("User has added unsupported url: " + contenturl);
                return new ArrayList<DownloadLink>();
            }
        } catch (final AccountUnavailableException e) {
            if (account != null) {
                handleAccountException(account, e);
            }
            throw e;
        } catch (final AccountInvalidException e) {
            if (account != null) {
                handleAccountException(account, e);
            }
            throw e;
        }
    }

    /** Tries different json paths and returns the first result. */
    private Object get(Map<String, Object> entries, final String... paths) {
        for (String path : paths) {
            final Object ret = JavaScriptEngineFactory.walkJson(entries, path);
            if (ret != null) {
                logger.info("Found:" + path);
                return ret;
            } else {
                logger.info("Not found:" + path);
            }
        }
        return null;
    }

    private void getPage(CryptedLink link, final Browser br, final Request sourceRequest) throws Exception {
        int retry = 0;
        final int maxtries = 30;
        long totalWaittime = 0;
        Request request = null;
        boolean success = false;
        while (retry < maxtries && !isAbort()) {
            retry++;
            request = sourceRequest.cloneRequest();
            if (retry > 1) {
                logger.info(String.format(Locale.ROOT, "Trying to get around rate limit %d / %d", retry, maxtries));
                /* 2020-01-21: Changing User-Agent or Cookies will not help us to get around this limit earlier! */
                // br.clearCookies(br.getHost());
                // br.getHeaders().put("User-Agent", "iPad");
            }
            br.getPage(request);
            final int responsecode = br.getHttpConnection().getResponseCode();
            if (responsecode == 502) {
                final int waittime = 20000 + 15000 * retry;
                totalWaittime += waittime;
                logger.info(String.format(Locale.ROOT, "Waiting %d seconds on error 502 until retry", waittime / 1000));
                sleep(waittime, link);
            } else if (responsecode == 403 || responsecode == 429) {
                if (PluginJsonConfig.get(InstagramConfig.class).getActionOnRateLimitReached() == ActionOnRateLimitReached.ABORT) {
                    logger.info("Rate limit has been reached --> Aborting");
                    throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT, "RATE_LIMIT_REACHED_" + br._getURL().getPath(), "Rate limit has been reached and user prefers abort in this case.");
                } else {
                    final int waittime = 20000 + 15000 * retry;
                    totalWaittime += waittime;
                    logger.info(String.format(Locale.ROOT, "Waiting %d seconds on error 403/429 until retry", waittime / 1000));
                    sleep(waittime, link);
                }
            } else {
                success = true;
                break;
            }
        }
        if (!success) {
            if (br.getHttpConnection().getResponseCode() == 502) {
                throw br.new BrowserException("ResponseCode: 502", br.getRequest(), null);
            } else if (br.getHttpConnection().getResponseCode() == 403 || br.getHttpConnection().getResponseCode() == 429) {
                throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT, "RATE_LIMIT_REACHED_EXHAUSTED_RETRY_COUNT_" + br._getURL().getPath(), "Rate limit has been reached and crawler failed to avoid it. Try again later.");
            } else {
                /* Developer mistake */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } else if (retry > 1) {
            logger.info("Total time waited to get around rate limit: " + TimeFormatter.formatMilliSeconds(totalWaittime, 0));
        }
    }

    private String find_ig_app_id(final Browser br, final boolean allowStaticFallback) {
        final String cachedAppID = getLazyCache("ig_app_id");
        if (cachedAppID != null) {
            return cachedAppID;
        }
        String igAppID = null;
        if (br != null) {
            igAppID = regex_app_id(br);
            if (igAppID != null) {
                putLazyCache("ig_app_id", igAppID);
            }
        }
        if (igAppID == null && allowStaticFallback) {
            /**
             * Static fallback. If this id is wrong, requests done using it will get this response: <br>
             * {"message":"useragent mismatch","status":"fail"}
             */
            igAppID = "936619743392459";
        }
        return igAppID;
    }

    private String regex_app_id(final Browser br) {
        return br.getRegex("\"X-IG-App-ID\":\"(\\d+)").getMatch(0);
    }

    /** Do we have to be logged in to crawl this URL? */
    private boolean requiresLogin(final String url) {
        if (new Regex(url, TYPE_PROFILE_SAVED_OBJECTS).patternFind()) {
            return true;
        } else if (new Regex(url, PATTERN_STORY_SPECIFIC).patternFind()) {
            return true;
        } else if (new Regex(url, TYPE_PROFILE_TAGGED).patternFind()) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Returns userID for given username. </br>
     * Uses website to find userID. </br>
     * Throws Exception if it is unable to find userID in HTML code --> Profile is most likely offline then!
     */
    private String findUserID(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String username) throws Exception {
        if (StringUtils.isEmpty(username)) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* First check our cache -> saves time */
        String cachedUserID = getCachedUserID(username);
        if (cachedUserID != null) {
            /* Return cached userID */
            return cachedUserID;
        }
        final boolean allowUseSearchToFindUserID = true;
        String userID = null;
        final String userProfileURL = this.generateURLProfile(username);
        findUserIdViaWebsite: if (userID == null) {
            /**
             * 2026-10-30: The public profile page embeds the result of its GraphQL queries (<code>xig_user_by_username</code>) even when the
             * API refuses anonymous requests (same approach as instaloader). This is the most reliable way to find the userID.
             */
            logger.info("Trying to find user-id via website");
            getPageAutoLogin(account, loggedIN, userProfileURL, param, br, userProfileURL);
            final Map<String, Object> profile = getEmbeddedProfileData(br);
            final Object pk = profile.get("pk");
            if (pk != null) {
                userID = pk.toString();
                final String profile_pic_url = (String) profile.get("profile_pic_url");
                if (profile_pic_url != null) {
                    putLazyCache(userID + "_profile_pic_url", profile_pic_url);
                }
            } else {
                /* Legacy fallbacks */
                userID = br.getRegex("\"owner\":\\s*\\{\"id\":\\s*\"(\\d+)\"\\}").getMatch(0);
                if (StringUtils.isEmpty(userID)) {
                    userID = br.getRegex("\"page_id\"\\s*:\\s*\"profilePage_(\\d+)").getMatch(0);
                }
            }
            if (StringUtils.isEmpty(userID)) {
                logger.info("Failed to find user-id via website method");
            }
        }
        findUserIdViaTopSearch: if (userID == null && allowUseSearchToFindUserID) {
            /**
             * 2025-02-24: For some profiles, this API did not return the expected results or at least not on "pagination page 1". <br>
             * For this reason, other methods should be preferred over this one from now on.
             */
            /* 2022-05-11: New method: Faster and json only */
            logger.info("Trying to find user-id via website search function");
            Request req = br.createGetRequest("https://www." + this.getHost() + "/web/search/topsearch/?context=blended&query=" + username + "&include_reel=true");
            /* None of these headers are mandatory atm. */
            req.getHeaders().put("Referer", userProfileURL);
            req.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            getPageAutoLogin(account, loggedIN, req.getUrl(), param, br, req);
            Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if (Boolean.TRUE.equals(entries.get("require_login")) || br.getHttpConnection().getResponseCode() == 401) {
                /*
                 * 2022-10-07 E.g.
                 * {"message":"Bitte warte einige Minuten und versuche es dann noch einmal.","require_login":true,"status":"fail"}
                 */
                /* 2023-11-08: Also possible: response 401 with {"message":"Server Error","status":"fail"} */
                if (loggedIN.get()) {
                    /* Already logged in -> This should never happen */
                    logger.warning("Login required but we're already logged in -> Possible problem with account/session");
                    throw new AccountRequiredException();
                } else if (account == null) {
                    /* Account required but not available */
                    logger.info("Failed to obtain user-id via topsearch method because: Account required");
                    break findUserIdViaTopSearch;
                }
                sleep(2000, param);
                logger.info("Logging in because: " + entries.get("message"));
                /* We're logged in now -> Perform request again. */
                req = req.cloneRequest();
                getPageAutoLogin(account, loggedIN, true, req.getUrl(), param, br, req);
                entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            }
            if ("fail".equals(entries.get("status"))) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final List<Map<String, Object>> users = (List<Map<String, Object>>) entries.get("users");
            for (final Map<String, Object> entry : users) {
                final Map<String, Object> user = (Map<String, Object>) entry.get("user");
                if (StringUtils.equalsIgnoreCase(username, StringUtils.valueOfOrNull(user.get("username")))) {
                    userID = user.get("pk").toString();
                    break;
                }
            }
            if (StringUtils.isEmpty(userID)) {
                /* Most likely that profile doesn't exist */
                logger.warning("Failed to find user-id via search -> Invalid profile?");
            }
        }
        /*
         * 2022-06-23: Alternative way: https://i.instagram.com/api/v1/users/web_profile_info/?username=<username> --> Requires special
         * User-Agent!
         */
        if (StringUtils.isEmpty(userID)) {
            /* Most likely that profile doesn't exist */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Profile not found");
        }
        /* Add to cache for later usage */
        addCachedUserID(userID, username);
        return userID;
    }

    private void addCachedUserID(final String userID, final String username) {
        synchronized (ID_TO_USERNAME) {
            /* Log only if item is not in map yet to prevent log spam */
            if (!ID_TO_USERNAME.containsKey(userID)) {
                logger.info("Adding userID to cache: user: " + username + " -> userID: " + userID);
            }
            ID_TO_USERNAME.put(userID, username);
        }
    }

    /** Get cached userID by username */
    private String getCachedUserID(final String username) {
        if (username == null) {
            return null;
        }
        synchronized (ID_TO_USERNAME) {
            final Iterator<Entry<String, String>> iterator = ID_TO_USERNAME.entrySet().iterator();
            while (iterator.hasNext()) {
                final Entry<String, String> entry = iterator.next();
                if (entry.getValue().equals(username)) {
                    return entry.getKey();
                }
            }
        }
        return null;
    }

    /**
     * Returns the "data" object of every GraphQL query result that Instagram embeds into its HTML pages (<code>__bbox/result/data</code>
     * inside of <code>&lt;script type="application/json"&gt;</code> tags) in page order.
     */
    private List<Map<String, Object>> getEmbeddedQueryData(final Browser br) {
        final List<Map<String, Object>> ret = new ArrayList<Map<String, Object>>();
        final String[] jsons = br.getRegex("<script type=\"application/json\"[^>]*>(.*?)</script>").getColumn(0);
        if (jsons == null) {
            return ret;
        }
        for (final String json : jsons) {
            try {
                collectEmbeddedQueryData(restoreFromString(json, TypeRef.MAP), ret);
            } catch (final Exception ignore) {
                /* Not every script tag contains valid json */
            }
        }
        return ret;
    }

    private void collectEmbeddedQueryData(final Object o, final List<Map<String, Object>> results) {
        if (o instanceof Map) {
            final Map<String, Object> map = (Map<String, Object>) o;
            final Object bbox = map.get("__bbox");
            if (bbox != null) {
                final Object data = JavaScriptEngineFactory.walkJson(bbox, "result/data");
                if (data instanceof Map) {
                    results.add((Map<String, Object>) data);
                    return;
                }
            }
            for (final Object value : map.values()) {
                if (value instanceof Map || value instanceof List) {
                    collectEmbeddedQueryData(value, results);
                }
            }
        } else if (o instanceof List) {
            for (final Object value : (List<Object>) o) {
                if (value instanceof Map || value instanceof List) {
                    collectEmbeddedQueryData(value, results);
                }
            }
        }
    }

    /**
     * Returns all information about a profile that is embedded in the (logged out) profile page (<code>xig_user_by_username</code>) as one
     * merged map. Contains e.g. pk, username, profile_pic_url, is_private and polaris_ordered_timeline_connection (first posts). Returns
     * an empty map if nothing was found.
     */
    private Map<String, Object> getEmbeddedProfileData(final Browser br) {
        final Map<String, Object> ret = new LinkedHashMap<String, Object>();
        for (final Map<String, Object> data : getEmbeddedQueryData(br)) {
            final Object user = data.get("xig_user_by_username");
            if (user instanceof Map) {
                ret.putAll((Map<String, Object>) user);
            }
        }
        return ret;
    }

    /**
     * Performs a doc_id based GraphQL query via the "api/graphql" endpoint, like the (logged out) web app and instaloader do. Needs the
     * "lsd" token which can be found in every HTML page of Instagram.
     */
    private Map<String, Object> graphqlDocIdRequest(final Browser br, final String docId, final Map<String, Object> variables, final String referer) throws Exception {
        final String lsdRegex = "\"LSD\",\\[\\],\\{[^\\{\\}]*\"token\":\"([^\"]+)\"";
        String lsd = null;
        if (br.getRequest() != null) {
            lsd = br.getRegex(lsdRegex).getMatch(0);
        }
        if (lsd == null) {
            final Browser brStart = br.cloneBrowser();
            brStart.getPage("https://www." + getHost() + "/");
            lsd = brStart.getRegex(lsdRegex).getMatch(0);
            if (lsd == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find lsd token");
            }
        }
        final Browser brc = br.cloneBrowser();
        brc.setRequest(null);
        brc.getHeaders().put("Accept", "*/*");
        brc.getHeaders().put("Origin", "https://www." + getHost());
        brc.getHeaders().put("X-FB-LSD", lsd);
        brc.getHeaders().put("X-IG-App-ID", this.find_ig_app_id(br, true));
        brc.getHeaders().put("Sec-Fetch-Dest", "empty");
        brc.getHeaders().put("Sec-Fetch-Mode", "cors");
        brc.getHeaders().put("Sec-Fetch-Site", "same-origin");
        if (referer != null) {
            brc.getHeaders().put("Referer", referer);
        }
        final String csrftoken = br.getCookie(getHost(), "csrftoken");
        if (csrftoken != null) {
            brc.getHeaders().put("X-CSRFToken", csrftoken);
        }
        final String body = "variables=" + URLEncode.encodeURIComponent(JSonStorage.serializeToJson(variables)) + "&doc_id=" + docId + "&server_timestamps=true&lsd=" + URLEncode.encodeURIComponent(lsd);
        brc.postPage("https://www." + getHost() + "/api/graphql", body);
        final int responsecode = brc.getHttpConnection().getResponseCode();
        if (responsecode == 429 || responsecode == 403) {
            throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT, "RATE_LIMIT_REACHED_GRAPHQL_" + docId, "Rate limit has been reached. Try again later.");
        } else if (responsecode == 401) {
            throw new AccountRequiredException();
        }
        return restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
    }

    private void getPageAutoLogin(final Account account, final AtomicBoolean loginState, final String urlCheck, final CryptedLink param, final Browser br, final String requestURL) throws Exception {
        getPageAutoLogin(account, loginState, urlCheck, param, br, br.createGetRequest(requestURL));
    }

    private void getPageAutoLogin(final Account account, final AtomicBoolean loginState, final String urlCheck, final CryptedLink param, final Browser br, final Request request) throws Exception {
        getPageAutoLogin(account, loginState, null, urlCheck, param, br, request);
    }

    /**
     * Automatically performs given request and logs in if needed and possible. </br>
     *
     * @param urlCheck
     *            Ensures that we are on this URL after request has been performed.
     */
    private void getPageAutoLogin(final Account account, final AtomicBoolean loginState, Boolean forceLogin, final String urlCheck, final CryptedLink param, final Browser br, final Request request) throws Exception {
        try {
            if (PluginJsonConfig.get(InstagramConfig.class).isEnforceLoginIfAccountIsAvailable() && account != null && !loginState.get()) {
                logger.info("Performing forced login according to user plugin setting");
                this.loginOrFail(account, loginState);
            } else if (Boolean.TRUE.equals(forceLogin) && !loginState.get()) {
                logger.info("Performing forced login");
                this.loginOrFail(account, loginState);
            }
            getPage(param, br, request);
            if (br.getRequest().getHtmlCode().startsWith("<")) {
                final String ig_app_id = this.regex_app_id(br);
                if (ig_app_id != null) {
                    putLazyCache("ig_app_id", ig_app_id);
                }
                final String username = new Regex(br._getURL().getPath(), TYPE_PROFILE).getMatch(0);
                if (username != null) {
                    /* We look to be on a username page at this moment -> Look for userID of that username */
                    final String userID = br.getRegex("profilePage_(\\d+)").getMatch(0);
                    if (userID != null) {
                        this.addCachedUserID(userID, username);
                    }
                }
            }
            AccountRequiredException accountRequired = null;
            try {
                InstaGramCom.checkErrors(this, br);
            } catch (final AccountRequiredException e) {
                if (account == null) {
                    // fail fast
                    throw e;
                } else if (loginState.get()) {
                    // already logged in?
                    throw e;
                } else {
                    accountRequired = e;
                }
            }
            if (accountRequired != null || (urlCheck != null && !br.getURL().contains(urlCheck))) {
                /*
                 * E.g. private gallery/account and we're not logged in or we're not logged in with an account with the required permissions
                 * -> Redirect to main page or URL of the profile which uploaded the gallery.
                 */
                if (account == null) {
                    // fail fast
                    throw new AccountRequiredException();
                } else if (loginState.get()) {
                    // already logged in?
                    logger.info("Login required but user is already logged in?!");
                    throw new AccountRequiredException();
                } else {
                    loginOrFail(account, loginState);
                    getPage(param, br, request);
                    try {
                        InstaGramCom.checkErrors(this, br);
                        if (urlCheck != null && !br.getURL().contains(urlCheck)) {
                            throw new AccountRequiredException();
                        }
                    } catch (final AccountRequiredException e) {
                        logger.exception("Logged in but URL still isn't accessible", e);
                        throw e;
                    }
                }
            }
        } catch (Exception e) {
            logger.log(e);
            throw e;
        }
    }

    /**
     * Gallery == post. Can contain single or multiple media items (image/video). </br>
     * If multiple media is present, insividual pictures cannot be linked individually.
     */
    private ArrayList<DownloadLink> crawlGallery(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String galleryID) throws Exception {
        if (StringUtils.isEmpty(galleryID)) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        /**
         * TODO: This is not a TODO, just an information: 2022-06-22: Example get json right away: Add "?__a=1&__d=dis" to URL e.g.: </br>
         * https://www.instagram.com/p/<postID>/?__a=1&__d=dis
         */
        getPageAutoLogin(account, loggedIN, galleryID, param, br, param.getCryptedUrl());
        final List<Map<String, Object>> postsFromWebsite = new ArrayList<Map<String, Object>>(0);
        findGalleryJsonSource: try {
            /* Logged out post pages do not embed any post data anymore -> See GraphQL fallback below. */
            if (loggedIN.get()) {
                final String[] bboxJsons = br.getRegex("data-sjs>(\\{\"require\".*?\\})</script>").getColumn(0);
                if (bboxJsons != null && bboxJsons.length > 0) {
                    for (final String bboxJson : bboxJsons) {
                        final Map<String, Object> bboxJsonParsed = restoreFromString(bboxJson, TypeRef.MAP);
                        final Map<String, Object> result = (Map<String, Object>) findPostsListRecursive(bboxJsonParsed);
                        if (result != null) {
                            final List<Map<String, Object>> postitems = (List<Map<String, Object>>) result.get("items");
                            for (final Map<String, Object> postitem : postitems) {
                                postsFromWebsite.add(postitem);
                            }
                            break findGalleryJsonSource;
                        }
                    }
                }
            }
        } catch (final Throwable e) {
            logger.log(e);
            logger.warning("Website mode failed with exception");
            /* Allow fallback to API down below. */
        }
        if (postsFromWebsite.size() == 0 && !loggedIN.get()) {
            /* 2026-10-30: Logged out post pages do not embed the post data anymore -> Use GraphQL (same as instaloader). */
            try {
                final Map<String, Object> vars = new LinkedHashMap<String, Object>();
                vars.put("shortcode", galleryID);
                vars.put("__relay_internal__pv__PolarisAIGMMediaWebLabelEnabledrelayprovider", false);
                final Map<String, Object> response = this.graphqlDocIdRequest(br, "27128499623469141", vars, null);
                final List<Map<String, Object>> items = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(response, "data/xdt_api__v1__media__shortcode__web_info/items");
                if (items != null) {
                    postsFromWebsite.addAll(items);
                }
            } catch (final InterruptedException e) {
                throw e;
            } catch (final DecrypterRetryException e) {
                throw e;
            } catch (final Exception e) {
                logger.log(e);
                logger.warning("GraphQL post lookup failed");
            }
        }
        if (postsFromWebsite.size() > 0) {
            final InstagramMetadata metadata = new InstagramMetadata();
            if (param.getDownloadLink() != null) {
                /*
                 * Check for previously set properties e.g. user crawls hashtag URL --> Some URLs can go go back into crawler --> We want to
                 * keep the hashtag value in order to use it inside filenames/packagenames and as a packagizer property.
                 */
                final DownloadLink link = param.getDownloadLink();
                final String hashtag = link.getStringProperty(InstaGramCom.PROPERTY_hashtag);
                if (hashtag != null) {
                    metadata.setHashtag(hashtag);
                }
                final String forcedPackagename = link.getStringProperty(InstaGramCom.PROPERTY_forced_packagename);
                if (forcedPackagename != null) {
                    metadata.setPackageName(forcedPackagename);
                }
            }
            final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
            for (final Map<String, Object> post : postsFromWebsite) {
                ret.addAll(crawlPost(param, metadata, post));
            }
            return ret;
        } else {
            /* API mode. Required when we're logged in. */
            logger.info("Auto fallback to API crawler");
            if (account == null) {
                throw new AccountRequiredException();
            }
            String internalMediaID = null;
            do {
                internalMediaID = br.getRegex("property=\"al:ios:url\" content=\"instagram://media\\?id=(\\d+)").getMatch(0);
                if (internalMediaID == null) {
                    internalMediaID = br.getRegex("\"media_id\"\\s*:\\s*\"(\\d+)").getMatch(0);
                }
                if (internalMediaID != null || loggedIN.get()) {
                    break;
                } else {
                    logger.info("Failed to find internalMediaID in logged-out state --> Retry with account");
                    this.loginOrFail(account, loggedIN);
                    br.getPage(param.getCryptedUrl());
                    InstaGramCom.checkErrors(this, br);
                    continue;
                }
            } while (true);
            if (internalMediaID == null) {
                logger.info("Content is either offline or given account is lacking permissions to view it.");
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            return this.crawlGalleryAltAPI(param, account, loggedIN, internalMediaID);
        }
    }

    /* Finds object containing single Instagram posts inside given parsed website-json object. */
    private Object findPostsListRecursive(final Object o) {
        if (o instanceof Map) {
            final Map<String, Object> entrymap = (Map<String, Object>) o;
            final Object targetO = entrymap.get("xdt_api__v1__media__shortcode__web_info");
            if (targetO != null) {
                return targetO;
            } else {
                for (final Map.Entry<String, Object> entry : entrymap.entrySet()) {
                    // final String key = entry.getKey();
                    final Object value = entry.getValue();
                    if (value instanceof List || value instanceof Map) {
                        final Object ret = findPostsListRecursive(value);
                        if (ret != null) {
                            return ret;
                        }
                    }
                }
                return null;
            }
        } else if (o instanceof List) {
            final List<Object> array = (List) o;
            for (final Object arrayo : array) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final Object res = findPostsListRecursive(arrayo);
                    if (res != null) {
                        return res;
                    }
                }
            }
            return null;
        } else {
            return null;
        }
    }

    private ArrayList<DownloadLink> crawlGalleryAltAPI(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String internalMediaID) throws Exception {
        if (internalMediaID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        loginOrFail(account, loggedIN);
        InstaGramCom.prepBRAltAPI(this.br);
        InstaGramCom.getPageAltAPI(account, this.br, InstaGramCom.ALT_API_BASE + "/media/" + internalMediaID + "/info/");
        InstaGramCom.checkErrorsAltAPI(account, br);
        final Map<String, Object> entries = restoreFromString(br.toString(), TypeRef.MAP);
        final List<Map<String, Object>> items = (List<Map<String, Object>>) entries.get("items");
        final Map<String, Object> firstItem = items.get(0);
        final Map<String, Object> user = (Map<String, Object>) firstItem.get("user");
        final InstagramMetadata metadata = new InstagramMetadata(user.get("username").toString());
        metadata.setMainContentID(firstItem.get("code").toString());
        metadata.setDate(new Date(((Number) firstItem.get("taken_at")).longValue() * 1000));
        // for (final Map<String, Object> item : items) {
        // }
        return this.crawlPostListAltAPI(param, items, metadata);
    }

    /** Crawls all items of a user or all items where one user was tagged */
    private ArrayList<DownloadLink> crawlUser(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String username) throws UnsupportedEncodingException, Exception {
        if (username == null) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final InstagramConfig cfg = PluginJsonConfig.get(InstagramConfig.class);
        int userSelectedCrawlTypes = 0;
        final boolean crawlProfilePosts = cfg.getProfileCrawlerMaxItemsLimit() != 0;
        final boolean crawlProfilePicture = cfg.isProfileCrawlerCrawlProfilePicture();
        final boolean crawlStory = cfg.isProfileCrawlerCrawlStory();
        final boolean crawlStoryHighlights = cfg.isProfileCrawlerCrawlStoryHighlights();
        if (crawlStory) {
            userSelectedCrawlTypes++;
        }
        if (crawlStoryHighlights) {
            userSelectedCrawlTypes++;
        }
        if (crawlProfilePosts) {
            userSelectedCrawlTypes++;
        }
        if (crawlProfilePicture) {
            userSelectedCrawlTypes++;
        }
        if (userSelectedCrawlTypes == 0) {
            logger.info("User has disabled all profile crawler functionality");
            return ret;
        }
        logger.info("crawlUser=crawlStory:" + crawlStory + "|crawlStoryHighlights:" + crawlStoryHighlights + "|crawlProfilePosts:" + crawlProfilePosts + "|crawlProfilePicture:" + crawlProfilePicture);
        /* Now do the actual crawling. */
        int elementsToCrawl = 0;
        if (crawlStory) {
            elementsToCrawl++;
        }
        if (crawlStoryHighlights) {
            elementsToCrawl++;
        }
        if (crawlProfilePicture || crawlProfilePosts) {
            elementsToCrawl++;
        }
        int elementIndex = 0;
        if (crawlStory) {
            elementIndex++;
            try {
                if (userSelectedCrawlTypes == 1) {
                    ret.addAll(this.crawlStory(param, account, loggedIN, username, true));
                } else {
                    ret.addAll(this.crawlStory(param, account, loggedIN, username, false));
                }
            } catch (final InterruptedException e) {
                throw e;
            } catch (final Exception e) {
                if (elementIndex == elementsToCrawl) {
                    throw e;
                }
                logger.log(e);
                logger.info("Crawling story failed but continuing because other elements are still to be crawled: " + username);
            }
        }
        if (crawlStoryHighlights) {
            elementIndex++;
            try {
                if (userSelectedCrawlTypes == 1) {
                    ret.addAll(this.crawlAllHighlightStories(username, account, loggedIN, true));
                } else {
                    ret.addAll(this.crawlAllHighlightStories(username, account, loggedIN, false));
                }
            } catch (final InterruptedException e) {
                throw e;
            } catch (final Exception e) {
                if (elementIndex == elementsToCrawl) {
                    throw e;
                }
                logger.log(e);
                logger.info("Crawling story highlights failed but continuing because other elements are still to be crawled: " + username);
            }
        }
        if (crawlProfilePicture || crawlProfilePosts) {
            elementIndex++;
            try {
                if (loggedIN.get() || account != null) {
                    final String userID = findUserID(param, account, loggedIN, username);
                    ret.addAll(this.crawlUserAltAPI(param, account, loggedIN, username, userID, crawlProfilePicture, crawlProfilePosts));
                } else {
                    ret.addAll(this.crawlUserWebsite(param, username, account, loggedIN, crawlProfilePicture, crawlProfilePosts));
                }
            } catch (final InterruptedException e) {
                throw e;
            } catch (final Exception e) {
                if (elementIndex == elementsToCrawl) {
                    throw e;
                }
                logger.log(e);
                logger.info("Crawling profile posts/picture failed but continuing because other elements are still to be crawled: " + username);
            }
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlUserReels(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String username) throws UnsupportedEncodingException, Exception {
        if (StringUtils.isEmpty(username)) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        logger.info("Crawling all reels of user: " + username);
        final String userID = this.findUserID(param, account, loggedIN, username);
        final UrlQuery query = new UrlQuery();
        query.add("target_user_id", userID);
        /* 12 = default pagination value of website */
        query.add("page_size", Integer.toString(PluginJsonConfig.get(InstagramConfig.class).getProfileCrawlerReelsPaginationMaxItemsPerPage()));
        query.add("include_feed_video", "true");
        String max_id = null;
        final InstagramMetadata metadata = new InstagramMetadata();
        metadata.setPackageName(username + " - reels");
        InstaGramCom.prepBRAltAPI(this.br);
        /* Reels are part of the profile so the profile crawler limit applies. */
        final int maxItemsLimit = PluginJsonConfig.get(InstagramConfig.class).getProfileCrawlerMaxItemsLimit();
        int numberofCrawledReels = 0;
        int page = 1;
        do {
            InstaGramCom.postPageAltAPI(account, this.br, InstaGramCom.ALT_API_BASE + "/clips/user/", query);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Map<String, Object> paging_info = (Map<String, Object>) entries.get("paging_info");
            final List<Map<String, Object>> mediaItems = (List<Map<String, Object>>) entries.get("items");
            if (mediaItems.size() == 0) {
                logger.info("Stopping because: empty?");
                if (page == 1) {
                    /* Only add this on the first page: Later pages can be empty even though we already found reels. */
                    ret.add(this.createOfflinelink(param.getCryptedUrl(), "PROFILE_CONTAINS_NO_REELS_" + username, "The following profile doesn't contain any reels: " + username));
                }
                break;
            }
            ret.addAll(this.crawlPostListAltAPI(param, mediaItems, metadata));
            numberofCrawledReels += mediaItems.size();
            max_id = (String) paging_info.get("max_id");
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size() + " | Next max_id: " + max_id);
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            } else if (maxItemsLimit > 0 && numberofCrawledReels >= maxItemsLimit) {
                logger.info("Stopping because: Reached user defined max items limit of " + maxItemsLimit);
                break;
            } else if (((Boolean) paging_info.get("more_available")).booleanValue() == false) {
                logger.info("Stopping because: more_available == false");
                break;
            } else if (StringUtils.isEmpty(max_id)) {
                /* Double fail-safe */
                logger.info("Stopping because: Reached last page");
                break;
            }
            /* Continue to next page */
            page++;
            query.addAndReplace("max_id", Encoding.urlEncode(max_id));
        } while (true);
        return ret;
    }

    private ArrayList<DownloadLink> crawlUserTagged(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String username) throws UnsupportedEncodingException, Exception {
        if (PluginJsonConfig.get(InstagramConfig.class).getProfileTaggedCrawledMaxItemsLimit() == 0) {
            logger.info("User has disabled all profile crawler functionality");
            return new ArrayList<DownloadLink>();
        }
        return this.crawlProfileTaggedAltAPI(param, account, loggedIN, username);
    }

    /**
     * Crawls profile picture and posts of a user without being logged in. <br>
     * 2026-10-30: Reworked: The profile page embeds the first page of posts (only shortcodes and thumbnails) and further pages are
     * obtained via the "api/graphql" endpoint (doc_id based) just like the logged-out web app and instaloader do. The actual media of each
     * post is crawled by sending the post-URL back into this crawler (see {@link #crawlGallery}).
     */
    private ArrayList<DownloadLink> crawlUserWebsite(final CryptedLink param, final String username, final Account account, final AtomicBoolean loggedIN, final boolean crawlProfilePicture, final boolean crawlPosts) throws UnsupportedEncodingException, Exception {
        if (username == null) {
            /* Developer mistake! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (!crawlProfilePicture && !crawlPosts) {
            /* Developer mistake */
            throw new IllegalArgumentException("crawlProfilePicture and crawlPosts cannot both be false");
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String userProfileURL = this.generateURLProfile(username);
        getPageAutoLogin(account, loggedIN, null, param, br, userProfileURL);
        if (br.getURL().contains("/accounts/login")) {
            throw new AccountRequiredException();
        }
        final Map<String, Object> profile = getEmbeddedProfileData(br);
        if (profile.isEmpty()) {
            if (br.getHttpConnection().getResponseCode() == 404 || br.containsHTML("Sorry, this page isn't available")) {
                ret.add(getDummyDownloadlinkProfileOffline(username));
                return ret;
            }
            logger.warning("Failed to find embedded profile data -> Website layout changed or login required");
            throw new AccountRequiredException();
        }
        final String userID = profile.get("pk").toString();
        this.addCachedUserID(userID, username);
        final Boolean isPrivate = (Boolean) profile.get("is_private");
        final Map<String, Object> postsConnection = (Map<String, Object>) profile.get("polaris_ordered_timeline_connection");
        if (Boolean.TRUE.equals(isPrivate) && postsConnection == null) {
            /* Private profile: Only visible with an account that follows this profile. */
            if (account == null) {
                throw new AccountRequiredException();
            }
            return this.crawlUserAltAPI(param, account, loggedIN, username, userID, crawlProfilePicture, crawlPosts);
        }
        final InstagramMetadata metadata = new InstagramMetadata(username);
        metadata.setPackageName(username);
        if (crawlProfilePicture) {
            /* This is a low resolution (150x150) picture but the HD version is not available without being logged in. */
            final String profilePictureURL = profile.get("profile_pic_url").toString();
            final DownloadLink profilePic = this.createDownloadlink(profilePictureURL);
            final String ext = Plugin.getFileNameExtensionFromString(profilePictureURL);
            profilePic.setFinalFileName(username + ext);
            profilePic.setAvailable(true);
            profilePic._setFilePackage(metadata.getFilePackage());
            ret.add(profilePic);
            distribute(profilePic);
        }
        if (crawlPosts) {
            if (postsConnection == null) {
                logger.warning("Failed to find posts in embedded profile data");
                ret.add(getDummyDownloadlinkProfileEmpty(username));
                return ret;
            }
            final long maxItemsLimit = PluginJsonConfig.get(InstagramConfig.class).getProfileCrawlerMaxItemsLimit();
            Map<String, Object> connection = postsConnection;
            int page = 1;
            int numberofCrawledPosts = 0;
            pagination: do {
                final List<Map<String, Object>> edges = (List<Map<String, Object>>) connection.get("edges");
                if (edges == null || edges.size() == 0) {
                    if (page == 1) {
                        ret.add(getDummyDownloadlinkProfileEmpty(username));
                    }
                    logger.info("Stopping because: Found no new links on page " + page);
                    break pagination;
                }
                for (final Map<String, Object> edge : edges) {
                    final Map<String, Object> node = (Map<String, Object>) edge.get("node");
                    final DownloadLink postLink = this.createDownloadlink(this.generateURLPost(node.get("code").toString()));
                    ret.add(postLink);
                    distribute(postLink);
                    numberofCrawledPosts++;
                    if (maxItemsLimit > 0 && numberofCrawledPosts >= maxItemsLimit) {
                        logger.info("Stopping because: User defined max items limit has been reached");
                        break pagination;
                    }
                }
                final Map<String, Object> page_info = (Map<String, Object>) connection.get("page_info");
                final String nextid = (String) page_info.get("end_cursor");
                logger.info("Crawled page: " + page + " | Crawled posts: " + numberofCrawledPosts + " | Next cursor: " + nextid);
                if (!Boolean.TRUE.equals(page_info.get("has_next_page")) || StringUtils.isEmpty(nextid)) {
                    logger.info("Stopping because: Reached last page");
                    break pagination;
                } else if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    break pagination;
                }
                /* Continue to next page */
                page++;
                final Map<String, Object> vars = new LinkedHashMap<String, Object>();
                vars.put("id", userID);
                vars.put("after", nextid);
                vars.put("first", 12);
                vars.put("__relay_internal__pv__PolarisSEOIsCrawlerrelayprovider", false);
                final Map<String, Object> response = this.graphqlDocIdRequest(br, "28904005159232689", vars, userProfileURL);
                connection = (Map<String, Object>) JavaScriptEngineFactory.walkJson(response, "data/node/polaris_ordered_timeline_connection");
                if (connection == null) {
                    /* Instagram only allows anonymous users to see a limited number of items. */
                    logger.info("Stopping because: Response did not contain any further posts -> Possible anonymous limit");
                    break pagination;
                }
            } while (true);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlUserAltAPI(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String username, final String userID, final boolean crawlProfilePicture, final boolean crawlPosts) throws UnsupportedEncodingException, Exception {
        if (username == null || userID == null) {
            /* Most likely developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Login is mandatory! */
        loginOrFail(account, loggedIN);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (!crawlProfilePicture && !crawlPosts) {
            /* Developer mistake */
            throw new IllegalArgumentException("crawlProfilePicture and crawlPosts cannot both be false");
        }
        final InstagramMetadata metadata = new InstagramMetadata(username);
        metadata.setPackageName(username);
        InstaGramCom.prepBRAltAPI(this.br);
        Number total_media_items = null;
        boolean hasCrawledProfilePicture = false;
        if (crawlProfilePicture) {
            /* First try to get a previously cached profile image url - HD preferred, then low-res. */
            String cachedProfilePicURL = getLazyCache(userID + "_profile_pic_url_hd");
            if (cachedProfilePicURL == null) {
                cachedProfilePicURL = getLazyCache(userID + "_profile_pic_url");
            }
            if (cachedProfilePicURL != null) {
                logger.info("Using cached profile image url");
                final DownloadLink profilePic = this.createDownloadlink(cachedProfilePicURL);
                final String ext = Plugin.getFileNameExtensionFromString(cachedProfilePicURL);
                profilePic.setFinalFileName(username + ext);
                profilePic.setAvailable(true);
                profilePic._setFilePackage(metadata.getFilePackage());
                ret.add(profilePic);
                distribute(profilePic);
                hasCrawledProfilePicture = true;
                if (!crawlPosts) {
                    logger.info("Stopping because: user wants to crawl profile image only (from cache)");
                    return ret;
                }
            }
        }
        if (crawlProfilePicture && !hasCrawledProfilePicture) {
            /* Crawl HD profile image, thx to: https://github.com/Ademking/profile-picture-viewer/blob/master/instagram.js#L43 */
            try {
                /**
                 * 2026-05-21: This request looks to be broken or server side deprecated now. <br>
                 * Returns this for me: {"user":{},"status":"ok"}
                 */
                logger.info("Crawling HD profile image");
                InstaGramCom.getPageAltAPI(account, this.br, InstaGramCom.ALT_API_BASE + "/users/" + userID + "/info/");
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final Map<String, Object> user = (Map<String, Object>) entries.get("user");
                total_media_items = ((Number) user.get("media_count")).intValue();
                final Map<String, Object> hd_profile_pic_url_info = (Map<String, Object>) user.get("hd_profile_pic_url_info");
                final String profile_pic_url_hd = hd_profile_pic_url_info.get("url").toString();
                putLazyCache(userID + "_profile_pic_url_hd", profile_pic_url_hd);
                final DownloadLink profilePic = this.createDownloadlink(profile_pic_url_hd);
                final String ext = Plugin.getFileNameExtensionFromString(profile_pic_url_hd);
                profilePic.setFinalFileName(username + ext);
                profilePic.setAvailable(true);
                profilePic._setFilePackage(metadata.getFilePackage());
                ret.add(profilePic);
                distribute(profilePic);
                hasCrawledProfilePicture = true;
                if (!crawlPosts) {
                    logger.info("Stopping because: user wants to crawl profile image only");
                    return ret;
                } else if (total_media_items != null && total_media_items.intValue() == 0) {
                    /* User doesn't have anything else we can crawl */
                    logger.info("Stopping because: media_count==0 | User has zero posts for us to crawl");
                    return ret;
                }
            } catch (final Exception e) {
                logger.log(e);
                logger.warning("Failed to crawl HD profile image -> Will try fallback");
            }
        }
        String nextid = null;
        int page = 1;
        int numberofCrawledPosts = 0;
        final int maxItemsLimit = PluginJsonConfig.get(InstagramConfig.class).getProfileCrawlerMaxItemsLimit();
        final String profilePostsFeedBaseURL = InstaGramCom.ALT_API_BASE + "/feed/user/" + userID + "/";
        pagination: do {
            if (page == 1) {
                InstaGramCom.getPageAltAPI(account, this.br, profilePostsFeedBaseURL);
            } else {
                // br.getPage(hashtagBaseURL + "?after=" + nextid);
                InstaGramCom.getPageAltAPI(account, this.br, profilePostsFeedBaseURL + "?max_id=" + nextid);
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if (crawlProfilePicture && !hasCrawledProfilePicture) {
                /* Alternative/Old way -> This will return a low resolution 150x150 profile image */
                try {
                    final Map<String, Object> user = (Map<String, Object>) entries.get("user");
                    final String profilePictureURL = user.get("profile_pic_url").toString();
                    putLazyCache(userID + "_profile_pic_url", profilePictureURL);
                    final DownloadLink profilePic = this.createDownloadlink(profilePictureURL);
                    final String ext = Plugin.getFileNameExtensionFromString(profilePictureURL);
                    profilePic.setFinalFileName(username + ext);
                    profilePic.setAvailable(true);
                    profilePic._setFilePackage(metadata.getFilePackage());
                    ret.add(profilePic);
                    distribute(profilePic);
                    hasCrawledProfilePicture = true;
                    if (!crawlPosts) {
                        logger.info("Stopping because: do not crawl posts (profile image only)");
                        return ret;
                    }
                } catch (final Exception e) {
                    logger.log(e);
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to crawl profile picture (both HD and fallback failed)", e);
                }
            }
            final int numberofItemsOnCurrentPage = (int) JavaScriptEngineFactory.toLong(entries.get("num_results"), 0);
            if (numberofItemsOnCurrentPage == 0) {
                /* Rare case */
                if (page == 1) {
                    /* Looks like profile doesn't contain any posts. */
                    ret.add(this.getDummyDownloadlinkProfileEmpty(username));
                } else {
                    logger.info("Stopping because: 0 items available on current page");
                }
                return ret;
            }
            nextid = (String) entries.get("next_max_id");
            final boolean more_available = ((Boolean) entries.get("more_available"));
            final List<Map<String, Object>> mediaItems = (List<Map<String, Object>>) entries.get("items");
            if (mediaItems == null || mediaItems.size() == 0) {
                logger.info("Stopping because: Found no new links on page " + page);
                break;
            }
            ret.addAll(this.crawlPostListAltAPI(param, mediaItems, metadata));
            numberofCrawledPosts += numberofItemsOnCurrentPage;
            logger.info("Crawled page: " + page + " | Crawled posts so far: " + numberofCrawledPosts + " | Total number of media items: " + total_media_items);
            if (!more_available) {
                logger.info("Stopping because: more_available == false");
                break pagination;
            } else if (StringUtils.isEmpty(nextid)) {
                logger.info("Stopping because: no nextid available");
                break pagination;
            } else if (maxItemsLimit > 0 && numberofCrawledPosts >= maxItemsLimit) {
                logger.info("Stopping because: reached user defined max items limit of " + maxItemsLimit);
                break pagination;
            }
            /* Continue to next page */
            page++;
        } while (!this.isAbort());
        return ret;
    }

    private void loginOrFail(final Account account, final AtomicBoolean loggedIN) throws Exception {
        try {
            if (account == null) {
                throw new AccountRequiredException();
            }
            if (!loggedIN.get()) {
                final PluginForHost plg = getNewPluginForHostInstance(getHost());
                ((jd.plugins.hoster.InstaGramCom) plg).login(account, false);
                loggedIN.set(true);
            }
        } catch (Exception e) {
            logger.log(e);
            throw e;
        }
    }

    private String getTypeName(final int media_type) {
        switch (media_type) {
        case 1:
            return "GraphImage";
        case 2:
            return "GraphVideo";
        case 8:
            return "GraphSidecar";
        default:
            return null;
        }
    }

    @SuppressWarnings({ "unchecked" })
    private ArrayList<DownloadLink> crawlPost(final CryptedLink param, InstagramMetadata metadata, final Map<String, Object> post) throws Exception {
        long date_timestamp = JavaScriptEngineFactory.toLong(post.get("date"), 0);
        if (date_timestamp == 0) {
            date_timestamp = JavaScriptEngineFactory.toLong(post.get("taken_at_timestamp"), 0);
            if (date_timestamp == 0) {
                // api
                date_timestamp = JavaScriptEngineFactory.toLong(post.get("taken_at"), 0);
            }
        }
        final InstagramConfig cfg = PluginJsonConfig.get(InstagramConfig.class);
        // is this id? // final String linkid_main = (String) entries.get("id");
        String typename = (String) post.get("__typename");
        final Number mediaType = (Number) post.get("media_type");// api
        if (typename == null && mediaType != null) {
            typename = getTypeName(mediaType.intValue());
        }
        if (typename == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unknown media type");
        }
        String mainContentID = (String) post.get("code");
        if (mainContentID == null) {
            /* 2022-03-30: E.g. required when crawling posts in context of user-profile. */
            mainContentID = (String) post.get("shortcode");
        }
        if (StringUtils.isEmpty(mainContentID)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find postID");
        }
        boolean ownerHasPrivateProfile = false;
        if (metadata == null) {
            metadata = new InstagramMetadata();
        }
        metadata.setMainContentID(mainContentID);
        metadata.setDate(new Date(date_timestamp * 1000));
        /* Find username if it is not pre-given. */
        String username = null;
        String userID = null;
        try {
            /* "owner" is given in the website format, "user" is given in the API format. Only one of them is present. */
            final Map<String, Object> ownerInfo = (Map<String, Object>) post.get("owner");
            final Map<String, Object> user = (Map<String, Object>) post.get("user");
            Boolean ownerHasPrivateProfileO = null;
            if (ownerInfo != null) {
                userID = StringUtils.valueOfOrNull(ownerInfo.get("id"));
                username = (String) ownerInfo.get("username");
                ownerHasPrivateProfileO = (Boolean) ownerInfo.get("is_private");
            }
            if (user != null) {
                if (userID == null) {
                    userID = StringUtils.valueOfOrNull(user.get("pk"));
                }
                if (username == null) {
                    username = (String) user.get("username");
                }
                if (ownerHasPrivateProfileO == null) {
                    ownerHasPrivateProfileO = (Boolean) user.get("is_private");
                }
            }
            if (userID == null) {
                /* Should always be given! */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else if (username == null) {
                /* Should always be given! */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            if (ownerHasPrivateProfileO != null) {
                ownerHasPrivateProfile = ownerHasPrivateProfileO.booleanValue();
            }
            /* Cache information for later usage just in case it isn't present in json the next time. */
            addCachedUserID(userID, username);
            if (metadata.getUsername() == null) {
                metadata.setUsername(username);
            }
        } catch (final Throwable ignore) {
            logger.log(ignore);
            logger.warning("Error happened during username lookup");
        }
        final Object captionO = post.get("caption");
        String description = null;
        if (captionO != null) {
            /* E.g. single post */
            if (captionO instanceof String) {
                description = captionO.toString();
            } else if (captionO instanceof Map) {
                // api
                description = (String) JavaScriptEngineFactory.walkJson(captionO, "text");
            }
        }
        if (description == null) {
            /* E.g. single post in context of user-profile */
            try {
                final Map<String, Object> edge_media_to_caption = ((Map<String, Object>) post.get("edge_media_to_caption"));
                final List<Map<String, Object>> edges = (List<Map<String, Object>>) edge_media_to_caption.get("edges");
                if (edges.size() > 0) {
                    final Map<String, Object> node = (Map<String, Object>) edges.get(0).get("node");
                    description = (String) node.get("text");
                }
            } catch (final Throwable ignore) {
            }
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final ArrayList<Map<String, Object>> mediaItems = new ArrayList<Map<String, Object>>();
        if (typename.equalsIgnoreCase("GraphImage")) {
            /* Single image */
            mediaItems.add(post);
        } else if (typename.equalsIgnoreCase("GraphVideo")) {
            /* Single video */
            mediaItems.add(post);
        } else if (typename.equalsIgnoreCase("GraphSidecar")) {
            /* Process multiple media items posted in a single IG post. */
            List<Map<String, Object>> medias = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(post, "edge_sidecar_to_children/edges");
            if (medias == null) {
                // api
                medias = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(post, "carousel_media");
            }
            mediaItems.addAll(medias);
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported media type: " + typename);
        }
        String coauthor_producers_comma_separated = null;
        final List<Map<String, Object>> coauthor_producers = (List<Map<String, Object>>) post.get("coauthor_producers");
        if (coauthor_producers != null) {
            coauthor_producers_comma_separated = "";
            for (final Map<String, Object> coauthor_producer : coauthor_producers) {
                if (coauthor_producers_comma_separated.length() > 0) {
                    coauthor_producers_comma_separated += ",";
                }
                coauthor_producers_comma_separated += coauthor_producer.get("username");
            }
        }
        /* Create FilePackage if packagename is available */
        final FilePackage fp = getFilePackageForGallery(metadata);
        final String postURL = this.generateURLPost(mainContentID);
        final int padLength = StringUtils.getPadLength(mediaItems.size());
        int orderID = 1;
        for (Map<String, Object> media : mediaItems) {
            final Object nodeO = media.get("node");
            if (nodeO != null) {
                /* 2022-03-30: E.g. media as part of user profile */
                media = (Map<String, Object>) nodeO;
            }
            final String internalMediaID = media.get("id").toString();
            final String shortcode = (String) media.get("shortcode");
            final Number thisMediaType = (Number) media.get("media_type");// api
            final boolean isVideo = Boolean.TRUE.equals(media.get("is_video")) || (thisMediaType != null && thisMediaType.intValue() == 2);
            String dllink = null;
            if (isVideo) {
                dllink = (String) media.get("video_url");
            } else {
                /* Find best image-quality */
                final List<Object> ressourcelist = (List<Object>) media.get("display_resources");
                if (ressourcelist != null) {
                    long qualityMax = 0;
                    for (final Object qualityO : ressourcelist) {
                        final Map<String, Object> imageQualityInfo = (Map<String, Object>) qualityO;
                        final long widthTmp = JavaScriptEngineFactory.toLong(imageQualityInfo.get("config_width"), 0);
                        if (widthTmp > qualityMax && imageQualityInfo.containsKey("src")) {
                            qualityMax = widthTmp;
                            dllink = (String) imageQualityInfo.get("src");
                        }
                    }
                }
                if (StringUtils.isEmpty(dllink)) {
                    dllink = (String) media.get("display_src");
                    if (StringUtils.isEmpty(dllink)) {
                        dllink = (String) media.get("display_url");
                        if (StringUtils.isEmpty(dllink)) {
                            dllink = (String) media.get("thumbnail_src");
                        }
                    }
                }
            }
            boolean isFromAltAPI = false;
            if (StringUtils.isEmpty(dllink)) {
                try {
                    dllink = InstaGramCom.getBestQualityURLAltAPI(media);
                    isFromAltAPI = true;
                } catch (final Throwable ignore) {
                }
            }
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find directurl");
            }
            /* 2022-03-30: Shouldn't be required anymore as we crawl story elements separately. */
            final boolean isPartOfStory = Boolean.TRUE.equals(media.get("is_reel_media"));
            final DownloadLink dl = this.createDownloadlink(postURL);
            dl.setAvailable(true);
            dl.setProperty(InstaGramCom.PROPERTY_uploader_id, userID);
            dl.setProperty(InstaGramCom.PROPERTY_main_content_id, mainContentID);
            dl.setProperty(InstaGramCom.PROPERTY_date, metadata.getDateFormatted());
            if (!StringUtils.isEmpty(shortcode)) {
                dl.setProperty(InstaGramCom.PROPERTY_shortcode, shortcode);
            }
            dl.setProperty(InstaGramCom.PROPERTY_DIRECTURL, dllink);
            if (isFromAltAPI) {
                dl.setProperty(InstaGramCom.PROPERTY_has_tried_to_crawl_original_url, true);
            }
            if (!StringUtils.isEmpty(description)) {
                dl.setComment(description);
                dl.setProperty(InstaGramCom.PROPERTY_description, description);
            }
            dl.setProperty(InstaGramCom.PROPERTY_orderid, String.format(Locale.ROOT, "%0" + padLength + "d", orderID));
            dl.setProperty(InstaGramCom.PROPERTY_orderid_raw, orderID);
            dl.setProperty(InstaGramCom.PROPERTY_orderid_max_raw, mediaItems.size());
            dl.setProperty(InstaGramCom.PROPERTY_internal_media_id, internalMediaID);
            if (!StringUtils.isEmpty(metadata.getUsername())) {
                /* Packagizer Property */
                dl.setProperty(InstaGramCom.PROPERTY_uploader, metadata.getUsername());
            }
            if (coauthor_producers_comma_separated != null) {
                dl.setProperty(InstaGramCom.PROPERTY_coauthor_producers_comma_separated, coauthor_producers_comma_separated);
            }
            if (isPartOfStory) {
                dl.setProperty(InstaGramCom.PROPERTY_is_part_of_story, true);
                /* There are no individual public URLs to directly link elements of a story --> Use URL user had added. */
                dl.setContentUrl(param.getCryptedUrl());
            } else {
                dl.setContentUrl(postURL);
            }
            if (ownerHasPrivateProfile) {
                dl.setProperty(InstaGramCom.PROPERTY_is_private, true);
            }
            if (isVideo) {
                dl.setProperty(InstaGramCom.PROPERTY_type, "video");
            } else {
                dl.setProperty(InstaGramCom.PROPERTY_type, "photo");
            }
            final String filename = getFilename(this, dl);
            dl.setFinalFileName(filename);
            dl._setFilePackage(fp);
            ret.add(dl);
            distribute(dl);
            orderID++;
        }
        if (cfg.isPostCrawlerAddPostDescriptionAsTextfile() && !StringUtils.isEmpty(description)) {
            /* Download picture-description as .txt file */
            ret.add(getTextDownloadlink(ret.get(0)));
        }
        return ret;
    }

    private FilePackage getFilePackageForGallery(final InstagramMetadata metadata) {
        FilePackage fp = metadata.getFilePackage();
        if (fp == null) {
            /* Fallback + handling for single posts */
            final InstagramConfig cfg = PluginJsonConfig.get(InstagramConfig.class);
            fp = FilePackage.getInstance();
            if (cfg.getPostCrawlerPackagenameSchemeType() == SinglePostPackagenameSchemeType.UPLOADER && metadata.getUsername() != null) {
                fp.setName(metadata.getUsername());
            } else if (cfg.getPostCrawlerPackagenameSchemeType() == SinglePostPackagenameSchemeType.UPLOADER_MAIN_CONTENT_ID && metadata.getUsername() != null) {
                fp.setName(metadata.getUsername() + " - " + metadata.getMainContentID());
            } else if (cfg.getPostCrawlerPackagenameSchemeType() == SinglePostPackagenameSchemeType.CUSTOM && !StringUtils.isEmpty(cfg.getPostCrawlerPackagenameScheme())) {
                /* Use User defined package names */
                String customPackageName = cfg.getPostCrawlerPackagenameScheme().replace("*date*", metadata.getDateFormatted());
                final String usernameForReplacer;
                if (metadata.getUsername() != null) {
                    usernameForReplacer = metadata.getUsername();
                } else {
                    usernameForReplacer = "-";
                }
                if (metadata.getUsername() != null) {
                    customPackageName = customPackageName.replace("*uploader*", usernameForReplacer);
                }
                customPackageName = customPackageName.replace("*main_content_id*", metadata.getMainContentID());
                fp.setName(customPackageName);
            } else {
                /* Fallback */
                fp.setName(metadata.getMainContentID());
            }
        }
        return fp;
    }

    /** Crawls json objects of stories/stories-highlights */
    private ArrayList<DownloadLink> crawlPostAltAPI(final CryptedLink param, final InstagramMetadata metadata, Map<String, Object> item) throws Exception {
        String mainMediaID = StringUtils.valueOfOrNull(item.get("id"));
        if (mainMediaID == null) {
            if (item.containsKey("media") && item.size() == 1) {
                item = (Map<String, Object>) item.get("media");
                mainMediaID = StringUtils.valueOfOrNull(item.get("id"));
            }
            if (mainMediaID == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        final Number deleted_reason = (Number) item.get("deleted_reason");
        if (deleted_reason != null && deleted_reason.intValue() != 0) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> results = new ArrayList<DownloadLink>();
        final Map<String, Object> user = (Map<String, Object>) item.get("user");
        final String userID = user.get("id").toString();
        /*
         * First try to get username from provided metadata. This is important because a post can also be a repost from someone elses post
         * thus posted by another user than the profile we might be crawling at this moment.
         */
        String username = metadata.getUsername();
        if (username == null) {
            username = user.get("username").toString();
        }
        /* Now that we already have this data, we can also cache it which will save requests later on. */
        this.addCachedUserID(userID, username);
        final Object reel_typeO = item.get("reel_type");
        final InstagramConfig cfg = PluginJsonConfig.get(InstagramConfig.class);
        if (reel_typeO != null) {
            /* Story */
            final String reel_type = reel_typeO.toString();
            final List<Map<String, Object>> mediaItems = (List<Map<String, Object>>) item.get("items");
            final long created_at;
            final String packagenameScheme;
            String title = "";
            /* Different packagenames for different types of stories and based on user plugin settings. */
            if (reel_type.equals("user_reel")) {
                /* Story */
                created_at = ((Number) item.get("latest_reel_media")).longValue();
                final StoryPackagenameSchemeType schemeType = cfg.getStoryPackagenameSchemeType();
                final String customPackagenameScheme = cfg.getStoryPackagenameScheme();
                if (schemeType == StoryPackagenameSchemeType.DEFAULT_1 || StringUtils.isEmpty(customPackagenameScheme)) {
                    packagenameScheme = "story - *uploader*";
                } else {
                    packagenameScheme = customPackagenameScheme;
                }
                /* No title available -> Use "story" as fallback */
                metadata.setStoryTitle("story");
            } else if (reel_type.equals("highlight_reel")) {
                /* Story highlight */
                created_at = ((Number) item.get("created_at")).longValue();
                title = item.get("title").toString();
                final StoriesHighlightsPackagenameSchemeType schemeType = cfg.getStoriesHighlightsPackagenameSchemeType();
                final String customPackagenameScheme = cfg.getStoriesHighlightsPackagenameScheme();
                if (schemeType == StoriesHighlightsPackagenameSchemeType.DEFAULT_1 || StringUtils.isEmpty(customPackagenameScheme)) {
                    packagenameScheme = "story highlights - *uploader* - *title*";
                } else {
                    packagenameScheme = customPackagenameScheme;
                }
                metadata.setStoryTitle(title);
            } else {
                /* Unknown/Unsupported reel_type */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /* Build packagename */
            String packagename = packagenameScheme.replace("*date*", new SimpleDateFormat("yyyy-MM-dd").format(new Date(created_at * 1000)));
            packagename = packagename.replace("*uploader*", username);
            packagename = packagename.replace("*title*", title);
            /* Set packagename */
            final FilePackage fp = getFilePackage();
            fp.setName(packagename);
            final int padLength = StringUtils.getPadLength(mediaItems.size());
            int orderID = 1;
            for (final Map<String, Object> media : mediaItems) {
                final String mediaID = media.get("id").toString();
                final String contentURL;
                if (reel_type.equals("user_reel")) {
                    /* Story: Individual images can be linked individually */
                    contentURL = "https://www." + this.getHost() + "/stories/" + username + "/" + mediaID.split("_")[0] + "/";
                } else if (reel_type.equals("highlight_reel")) {
                    /* Story highlights: Same URL for all items */
                    contentURL = generateStoryHighlightURL(mainMediaID);
                } else {
                    /* Unknown/Unsupported reel_type */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final long date = ((Number) media.get("taken_at")).longValue();
                final String internalMediaID = media.get("id").toString();
                /* Same ID as for public posts but cannot be used e.g. to direct-link this item for the user to open in browser. */
                final String postIDInternal = (String) media.get("code");
                final Number thisMediaType = (Number) media.get("media_type");
                final String directurl = InstaGramCom.getBestQualityURLAltAPI(media);
                final DownloadLink dl = this.createDownloadlink(this.generateURLPost(postIDInternal));
                dl.setContentUrl(contentURL);
                dl.setAvailable(true);
                dl.setProperty(InstaGramCom.PROPERTY_uploader_id, userID);
                dl.setProperty(InstaGramCom.PROPERTY_main_content_id, postIDInternal);
                final String dateFormatted = new SimpleDateFormat("yyyy-MM-dd").format(new Date(date * 1000));
                dl.setProperty(InstaGramCom.PROPERTY_date, dateFormatted);
                if (!StringUtils.isEmpty(directurl)) {
                    dl.setProperty(InstaGramCom.PROPERTY_DIRECTURL, directurl);
                    dl.setProperty(InstaGramCom.PROPERTY_has_tried_to_crawl_original_url, true);
                }
                dl.setProperty(InstaGramCom.PROPERTY_orderid, String.format(Locale.ROOT, "%0" + padLength + "d", orderID));
                dl.setProperty(InstaGramCom.PROPERTY_orderid_raw, orderID);
                dl.setProperty(InstaGramCom.PROPERTY_orderid_max_raw, mediaItems.size());
                dl.setProperty(InstaGramCom.PROPERTY_internal_media_id, internalMediaID);
                dl.setProperty(InstaGramCom.PROPERTY_uploader, username);
                dl.setProperty(InstaGramCom.PROPERTY_is_part_of_story, true);
                dl.setProperty(InstaGramCom.PROPERTY_story_title, metadata.getStoryTitle());
                if (thisMediaType.intValue() == 2) {
                    dl.setProperty(InstaGramCom.PROPERTY_type, "video");
                } else {
                    dl.setProperty(InstaGramCom.PROPERTY_type, "photo");
                }
                dl.setFinalFileName(getFilename(this, dl));
                dl._setFilePackage(fp);
                results.add(dl);
                distribute(dl);
                orderID++;
            }
        } else {
            /* Post */
            /* Same ID as for public posts but cannot be used e.g. to direct-link this item for the user to open in browser. */
            final String postID = (String) item.get("code");
            final Map<String, Object> caption = (Map<String, Object>) item.get("caption");
            final String description = caption != null ? (String) caption.get("text") : null;
            final List<Map<String, Object>> mediaItems = new ArrayList<Map<String, Object>>();
            final long date = ((Number) item.get("taken_at")).longValue();
            final List<Map<String, Object>> carousel_media = (List<Map<String, Object>>) item.get("carousel_media");
            if (carousel_media != null) {
                /* Post contains multiple items */
                mediaItems.addAll(carousel_media);
            } else {
                /* Post contains single item */
                mediaItems.add(item);
            }
            final int padLength = StringUtils.getPadLength(mediaItems.size());
            int orderID = 1;
            final FilePackage fp = this.getFilePackageForGallery(metadata);
            for (final Map<String, Object> media : mediaItems) {
                final Number thisMediaType = (Number) media.get("media_type");
                final String dllink = InstaGramCom.getBestQualityURLAltAPI(media);
                final String contentURL = this.generateURLPost(postID);
                final DownloadLink dl = this.createDownloadlink(contentURL);
                dl.setContentUrl(contentURL);
                dl.setAvailable(true);
                dl.setProperty(InstaGramCom.PROPERTY_uploader_id, userID);
                dl.setProperty(InstaGramCom.PROPERTY_main_content_id, postID);
                final String dateFormatted = new SimpleDateFormat("yyyy-MM-dd").format(new Date(date * 1000));
                dl.setProperty(InstaGramCom.PROPERTY_date, dateFormatted);
                if (!StringUtils.isEmpty(dllink)) {
                    dl.setProperty(InstaGramCom.PROPERTY_DIRECTURL, dllink);
                    dl.setProperty(InstaGramCom.PROPERTY_has_tried_to_crawl_original_url, true);
                }
                dl.setProperty(InstaGramCom.PROPERTY_orderid, String.format(Locale.ROOT, "%0" + padLength + "d", orderID));
                dl.setProperty(InstaGramCom.PROPERTY_orderid_raw, orderID);
                dl.setProperty(InstaGramCom.PROPERTY_orderid_max_raw, mediaItems.size());
                dl.setProperty(InstaGramCom.PROPERTY_internal_media_id, media.get("id").toString());
                dl.setProperty(InstaGramCom.PROPERTY_uploader, username);
                if (thisMediaType.intValue() == 2) {
                    dl.setProperty(InstaGramCom.PROPERTY_type, "video");
                } else {
                    dl.setProperty(InstaGramCom.PROPERTY_type, "photo");
                }
                if (!StringUtils.isEmpty(description)) {
                    dl.setProperty(InstaGramCom.PROPERTY_description, description);
                }
                final Object usertags = media.get("usertags");
                if (usertags != null) {
                    dl.setProperty(InstaGramCom.PROPERTY_json_usertags, JSonStorage.serializeToJson(usertags));
                }
                dl.setFinalFileName(getFilename(this, dl));
                if (fp != null) {
                    dl._setFilePackage(fp);
                }
                results.add(dl);
                distribute(dl);
                orderID++;
            }
            if (cfg.isPostCrawlerAddPostDescriptionAsTextfile() && !StringUtils.isEmpty(description)) {
                /* Download picture-description as .txt file */
                results.add(getTextDownloadlink(results.get(0)));
            }
        }
        return results;
    }

    /**
     * Returns DownloadLink object for text download of Instagram posts. Sets filename based on given list of DownloadLink objects
     * previously crawled for current Instagram post.
     */
    private DownloadLink getTextDownloadlink(final DownloadLink videoItem) throws Exception {
        final DownloadLink text = this.createDownloadlink(videoItem.getPluginPatternMatcher() + "_description");
        /* Add all properties from previously added media. */
        text.setProperties(videoItem.getProperties());
        /* Remove properties that are not relevant or even wrong for our text item. */
        text.removeProperty(InstaGramCom.PROPERTY_orderid);
        text.removeProperty(InstaGramCom.PROPERTY_orderid_raw);
        text.removeProperty(InstaGramCom.PROPERTY_orderid_max_raw);
        text.removeProperty(InstaGramCom.PROPERTY_shortcode);
        text.removeProperty(InstaGramCom.PROPERTY_DIRECTURL);
        text.setProperty(InstaGramCom.PROPERTY_type, "text");
        /* Important!! This will be used in LinkID later! */
        final String mainContentID = videoItem.getStringProperty(InstaGramCom.PROPERTY_main_content_id);
        final String description = videoItem.getStringProperty(InstaGramCom.PROPERTY_description);
        text.setProperty(InstaGramCom.PROPERTY_internal_media_id, mainContentID);
        final String filename = getFilename(this, text);
        text.setFinalFileName(filename);
        try {
            text.setDownloadSize(description.getBytes("UTF-8").length);
        } catch (final UnsupportedEncodingException ignore) {
            ignore.printStackTrace();
        }
        if (videoItem.getFilePackage() != null) {
            /* Put this textfile into the same package the other posts will go into. */
            text._setFilePackage(videoItem.getFilePackage());
        }
        text.setAvailable(true);
        text.setComment(description);
        distribute(text);
        return text;
    }

    /** Returns filename for single media items --> Video/image as part of post/story. */
    public static String getFilename(Plugin plugin, final DownloadLink link) throws Exception {
        /* Check for pre-defined filename */
        if (link.hasProperty(InstaGramCom.PROPERTY_filename_from_crawler)) {
            return link.getStringProperty(InstaGramCom.PROPERTY_filename_from_crawler);
        }
        final String storyHighlightTitle = link.getStringProperty(InstaGramCom.PROPERTY_story_title);
        final String dateFormatted = link.getStringProperty(InstaGramCom.PROPERTY_date);
        final String username = link.getStringProperty(InstaGramCom.PROPERTY_uploader);
        final String user_id = link.getStringProperty(InstaGramCom.PROPERTY_uploader_id);
        final int orderid = link.getIntegerProperty(InstaGramCom.PROPERTY_orderid_raw);
        final int orderid_max = link.getIntegerProperty(InstaGramCom.PROPERTY_orderid_max_raw);
        final String orderidFormatted = link.getStringProperty(InstaGramCom.PROPERTY_orderid);
        final String mainContentID = link.getStringProperty(InstaGramCom.PROPERTY_main_content_id);
        final String shortcode = link.getStringProperty(InstaGramCom.PROPERTY_shortcode);
        final String server_filename;
        final String directurl = link.getStringProperty(InstaGramCom.PROPERTY_DIRECTURL);
        if (directurl != null) {
            server_filename = InstaGramCom.getFileNameFromURL(plugin, new URL(directurl));
        } else {
            server_filename = null;
        }
        final String ext;
        if (InstaGramCom.isText(link)) {
            ext = ".txt";
        } else if (InstaGramCom.isVideo(link)) {
            ext = ".mp4";
        } else {
            final String serverFileNameExtension = Files.getExtension(server_filename);
            ext = "." + (serverFileNameExtension != null ? serverFileNameExtension : "jpg");
        }
        final InstagramConfig cfg = PluginJsonConfig.get(InstagramConfig.class);
        final FilenameType ft = cfg.getFilenameType();
        final String filenameScheme = cfg.getFilenameScheme();
        if (ft == FilenameType.SERVER && server_filename != null) {
            /* Server filename is preferred by user && available. */
            return server_filename;
        } else if (ft == FilenameType.CUSTOM && !StringUtils.isEmpty(filenameScheme)) {
            /* Build filename according to user defined filename scheme. */
            // *date*_*uploader* - *main_content_id* *orderid*_of_*orderid_max* - *shortcode*.*ext*
            String filename = filenameScheme.replace("*date*", dateFormatted);
            filename = filename.replace("*uploader*", username != null ? username : "-");
            filename = filename.replace("*uploader_id*", user_id != null ? user_id : "-");
            filename = filename.replace("*title*", storyHighlightTitle != null ? storyHighlightTitle : "-");
            filename = filename.replace("*orderid*", orderidFormatted != null ? orderidFormatted : "-");
            filename = filename.replace("*main_content_id*", mainContentID);
            filename = filename.replace("*orderid_max*", Integer.toString(orderid_max));
            filename = filename.replace("*shortcode*", shortcode != null ? shortcode : "-");
            filename = filename.replace("*ext*", ext);
            return filename;
        } else {
            /* Default/auto filenames */
            String filename;
            if (PluginJsonConfig.get(InstagramConfig.class).isAddDateToFilenames() && dateFormatted != null) {
                filename = dateFormatted + "_";
            } else {
                filename = "";
            }
            /*
             * 2022-01-28: For some items, individual ID is available when user is not logged in but isn't available when user is logged in.
             */
            final boolean individualShortcodeIsAvailable;
            if (!StringUtils.isEmpty(shortcode) && !shortcode.equals(mainContentID)) {
                individualShortcodeIsAvailable = true;
            } else {
                individualShortcodeIsAvailable = false;
            }
            if (link.hasProperty(InstaGramCom.PROPERTY_is_part_of_story)) {
                /* Use slightly different filenames for items that are part of a users' story */
                filename += username;
                if (orderidFormatted != null && PluginJsonConfig.get(InstagramConfig.class).isAddOrderidToFilenames()) {
                    /* By default: Include orderid whenever it is given to prevent duplicate filenames for different files! */
                    filename += " - " + orderidFormatted;
                }
                filename += " - " + mainContentID;
                if (individualShortcodeIsAvailable) {
                    filename += "_" + shortcode;
                }
            } else {
                final String hashtag = link.getStringProperty(InstaGramCom.PROPERTY_hashtag);
                if (!StringUtils.isEmpty(hashtag)) {
                    filename = hashtag + " - ";
                }
                if (!StringUtils.isEmpty(username)) {
                    filename += username + " - ";
                }
                filename += mainContentID;
                final boolean tryToAddOrderIDToFilename;
                if (PluginJsonConfig.get(InstagramConfig.class).isAddOrderidToFilenames()) {
                    tryToAddOrderIDToFilename = true;
                } else if (!individualShortcodeIsAvailable) {
                    /* Enforce this otherwise we might get same filenames for all items! */
                    tryToAddOrderIDToFilename = true;
                } else {
                    tryToAddOrderIDToFilename = false;
                }
                final boolean includeShortcodesInFilename;
                if (tryToAddOrderIDToFilename) {
                    /* Respect user-setting */
                    if (orderidFormatted != null && (orderid > 1 || orderid_max > 1)) {
                        /*
                         * Only add orderID to filename if mediaItem was added as part of multiple items e.g. story/post with more than 1
                         * items.
                         */
                        filename += " - " + orderidFormatted;
                    }
                    includeShortcodesInFilename = PluginJsonConfig.get(InstagramConfig.class).isAddShortcodeToFilenames();
                } else {
                    /* Force-add shortcode to filename in this case otherwise we might not get unique filenames! */
                    includeShortcodesInFilename = true;
                }
                if (individualShortcodeIsAvailable && includeShortcodesInFilename) {
                    filename += "_" + shortcode;
                }
            }
            filename += ext;
            return filename;
        }
    }

    /*************************************************
     * Methods using alternative API below. All of these require the user to be logged in!
     ***************************************************/
    /**
     * Crawls all saved items of currently logged-in account: https://www.instagram.com/username/saved/ </br>
     * Users can save any post: Their own ones or even posts of other users.
     */
    private ArrayList<DownloadLink> crawlUserSavedObjectsFeedAltAPI(final CryptedLink param, final Account account, final AtomicBoolean loggedIN) throws UnsupportedEncodingException, Exception {
        final String usernameOwnerOfSavedItems = new Regex(param.getCryptedUrl(), TYPE_PROFILE_SAVED_OBJECTS).getMatch(0);
        if (usernameOwnerOfSavedItems == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Login is mandatory to be able to crawl this linktype! */
        loginOrFail(account, loggedIN);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        InstaGramCom.prepBRAltAPI(this.br);
        Map<String, Object> entries;
        String nextid = null;
        int page = 1;
        int numberofCrawledPosts = 0;
        final String savedItemsFeedBaseURL = InstaGramCom.ALT_API_BASE + "/feed/saved/";
        final InstagramMetadata metadata = new InstagramMetadata();
        metadata.setPackageName("saved - " + usernameOwnerOfSavedItems);
        do {
            logger.info("Crawling page: " + page);
            if (page == 1) {
                /**
                 * This may return a varying amount of items e.g. 84 items on the first request, 9 after the next - this is decided
                 * serverside!
                 */
                InstaGramCom.getPageAltAPI(account, this.br, savedItemsFeedBaseURL);
            } else {
                InstaGramCom.getPageAltAPI(account, this.br, savedItemsFeedBaseURL + "?max_id=" + Encoding.urlEncode(nextid));
            }
            entries = restoreFromString(br.toString(), TypeRef.MAP);
            final int numberofitemsOnThisPage = (int) JavaScriptEngineFactory.toLong(entries.get("num_results"), 0);
            if (numberofitemsOnThisPage == 0) {
                if (page == 1) {
                    ret.add(getDummyDownloadlinkProfileHasNoSavedPosts(usernameOwnerOfSavedItems));
                    return ret;
                } else {
                    logger.info("Stopping because: 0 items available ...");
                    break;
                }
            }
            nextid = (String) entries.get("next_max_id");
            final boolean more_available = ((Boolean) entries.get("more_available"));
            final List<Map<String, Object>> mediaItems = (List<Map<String, Object>>) entries.get("items");
            if (mediaItems.size() == 0) {
                logger.info("Found no new links on page " + page + " --> Stopping decryption");
                break;
            }
            ret.addAll(this.crawlPostListAltAPI(param, mediaItems, metadata));
            numberofCrawledPosts += numberofitemsOnThisPage;
            logger.info("Crawled page: " + page + " | Crawled posts so far: " + numberofCrawledPosts);
            if (!more_available) {
                logger.info("Stopping because: more_available == false");
                break;
            } else if (StringUtils.isEmpty(nextid)) {
                logger.info("Stopping because: No nextid available");
                break;
            } else {
                page++;
            }
        } while (!this.isAbort());
        return ret;
    }

    private ArrayList<DownloadLink> crawlHashtag(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, String hashtag) throws UnsupportedEncodingException, Exception {
        final InstagramConfig cfg = PluginJsonConfig.get(InstagramConfig.class);
        if (cfg.getHashtagCrawlerMaxItemsLimit() == 0) {
            logger.info("Stopping because: User has disabled hashtag crawling");
            return new ArrayList<DownloadLink>();
        }
        if (StringUtils.isEmpty(hashtag)) {
            throw new IllegalArgumentException();
        }
        if (!hashtag.startsWith("#")) {
            hashtag = "#" + hashtag;
        }
        return this.crawlSearchWebsite(param, account, loggedIN, hashtag);
    }

    private ArrayList<DownloadLink> crawlSearchWithLimitErrorhandling(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String searchTerm) throws UnsupportedEncodingException, Exception {
        final long searchMaxItemsLimit = PluginJsonConfig.get(InstagramConfig.class).getSearchCrawlerMaxItemsLimit();
        if (searchMaxItemsLimit == 0) {
            logger.info("User has disabled search term crawler");
            final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
            return ret;
        }
        return crawlSearchWebsite(param, account, loggedIN, searchTerm);
    }

    /**
     * Crawls all items found when searching for a specific term in the Instagram search bar.
     */
    private ArrayList<DownloadLink> crawlSearchWebsite(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String searchTerm) throws UnsupportedEncodingException, Exception {
        if (StringUtils.isEmpty(searchTerm)) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        /* Login is mandatory! */
        loginOrFail(account, loggedIN);
        getPageAutoLogin(account, loggedIN, null, param, br, param.getCryptedUrl());
        final String igAppID = this.find_ig_app_id(br, true);
        if (igAppID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final boolean isHashtag = searchTerm.startsWith("#");
        final InstagramMetadata metadata = new InstagramMetadata();
        if (isHashtag) {
            metadata.setHashtag(searchTerm);
            metadata.setPackageName("hashtag - " + metadata.getHashtag());
        } else {
            metadata.setPackageName("search - " + searchTerm);
        }
        int page = 1;
        final long hashtagMaxItemsLimit = PluginJsonConfig.get(InstagramConfig.class).getHashtagCrawlerMaxItemsLimit();
        final long searchMaxItemsLimit = PluginJsonConfig.get(InstagramConfig.class).getSearchCrawlerMaxItemsLimit();
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("enable_metadata", "true");
        query.append("query", searchTerm, false);// already encoded, taken directly from the URL
        final HashSet<String> dupes = new HashSet<String>();
        int numberofCrawledMedia = 0;
        do {
            final GetRequest req = br.createGetRequest("https://www.instagram.com/api/v1/fbsearch/web/top_serp/?" + query.toString());
            req.getHeaders().put("Origin", "https://www." + this.getHost());
            req.getHeaders().put("Referer", param.getCryptedUrl());
            /* Important header! When this is missing we will get a "useragent mismatch" error. */
            req.getHeaders().put("x-ig-app-id", igAppID);
            br.getPage(req);
            br.setCurrentURL("https://www." + this.getHost());
            InstaGramCom.checkErrors(this, br);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Map<String, Object> media_grid = (Map<String, Object>) entries.get("media_grid");
            if (media_grid == null) {
                /* Empty search e.g. https://www.instagram.com/explore/tags/dg454568z645jztijutijuzt */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final List<Map<String, Object>> sections = (List<Map<String, Object>>) media_grid.get("sections");
            final String next_max_id = (String) media_grid.get("next_max_id");
            final String rank_token = (String) media_grid.get("rank_token");
            int numberofNewItemsThisPage = 0;
            boolean reachedHashtagCrawlLimit = false;
            boolean reachedSearchCrawlLimit = false;
            sectionLoop: for (final Map<String, Object> section : sections) {
                final List<Map<String, Object>> medias = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(section, "layout_content/medias");
                if (medias == null) {
                    /* Skip items we cannot handle */
                    continue;
                }
                mediaLoop: for (final Map<String, Object> media : medias) {
                    final Map<String, Object> media_inner = (Map<String, Object>) media.get("media");
                    final String mediaID = media_inner.get("id").toString();
                    if (!dupes.add(mediaID)) {
                        logger.info("Skipping dupe: " + mediaID);
                        continue;
                    }
                    ret.addAll(this.crawlPost(param, metadata, media_inner));
                    numberofNewItemsThisPage++;
                    numberofCrawledMedia++;
                    if (isHashtag) {
                        if (hashtagMaxItemsLimit > 0 && numberofCrawledMedia >= hashtagMaxItemsLimit) {
                            reachedHashtagCrawlLimit = true;
                            break sectionLoop;
                        }
                    } else {
                        if (searchMaxItemsLimit > 0 && numberofCrawledMedia >= searchMaxItemsLimit) {
                            reachedSearchCrawlLimit = true;
                            break sectionLoop;
                        }
                    }
                }
            }
            logger.info("Crawled search page " + page + " | New items this page: " + numberofNewItemsThisPage + " | Number of items crawled so far: " + dupes.size() + "/?" + " | Items crawled so far: " + ret.size());
            if (!Boolean.TRUE.equals(media_grid.get("has_more"))) {
                logger.info("Stopping because: Reached end");
                break;
            } else if (StringUtils.isEmpty(next_max_id)) {
                logger.info("Stopping because: Failed to find next_max_id");
                break;
            } else if (StringUtils.isEmpty(rank_token)) {
                logger.info("Stopping because: Failed to find rank_token");
                break;
            } else if (numberofNewItemsThisPage == 0) {
                logger.info("Stopping because: Failed to find any new items on current page");
                break;
            } else if (reachedHashtagCrawlLimit) {
                logger.info("Stopping because: Reached user defined hashtag max items limit: " + hashtagMaxItemsLimit);
                break;
            } else if (reachedSearchCrawlLimit) {
                logger.info("Stopping because: Reached user defined search max items limit: " + searchMaxItemsLimit);
                break;
            }
            /* Continue to next page */
            query.addAndReplace("next_max_id", Encoding.urlEncode(next_max_id));
            query.addAndReplace("rank_token", Encoding.urlEncode(rank_token));
            page++;
        } while (!this.isAbort());
        return ret;
    }

    private ArrayList<DownloadLink> crawlProfileTaggedAltAPI(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String username) throws UnsupportedEncodingException, Exception {
        if (StringUtils.isEmpty(username)) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        /* Login is mandatory! */
        loginOrFail(account, loggedIN);
        final String userID = this.findUserID(param, account, loggedIN, username);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        InstaGramCom.prepBRAltAPI(this.br);
        int totalNumberofPosts = -1;
        final InstagramMetadata metadata = new InstagramMetadata();
        metadata.setPackageName(username + " - tagged");
        String nextid = null;
        int page = 1;
        int numberofCrawledPostsTotal = 0;
        final String baseURL = InstaGramCom.ALT_API_BASE + "/usertags/" + userID + "/feed/";
        final UrlQuery query = new UrlQuery();
        query.add("count", "12");
        final int maxItemsLimit = PluginJsonConfig.get(InstagramConfig.class).getProfileTaggedCrawledMaxItemsLimit();
        do {
            if (page > 1) {
                query.addAndReplace("max_id", Encoding.urlEncode(nextid));
            }
            InstaGramCom.getPageAltAPI(account, this.br, baseURL + "?" + query.toString());
            final Map<String, Object> entries = restoreFromString(br.toString(), TypeRef.MAP);
            final List<Map<String, Object>> resource_data_list = (List<Map<String, Object>>) entries.get("items");
            final int numberofitemsOnThisPage = resource_data_list.size();
            if (page == 1) {
                totalNumberofPosts = ((Number) entries.get("total_count")).intValue();
                /* Do not(!) Check against 'total_count' here as this can be 12 on first page even though 0 items are available! */
                if (numberofitemsOnThisPage == 0) {
                    ret.add(this.createOfflinelink(param.getCryptedUrl(), "USER_IS_NOT_TAGGED_ANYWHERE_" + username, "The following user is not tagged anywhere: " + username));
                    return ret;
                }
            }
            if (numberofitemsOnThisPage == 0) {
                /* Rare case */
                logger.info("Stopping because: 0 items available ...");
                return ret;
            }
            /* Can be missing on the last page */
            nextid = (String) entries.get("next_max_id");
            final boolean more_available = ((Boolean) entries.get("more_available"));
            if (resource_data_list.size() == 0) {
                /* Should never happen(?) */
                logger.info("Stopping because: Found no new links on page " + page + " --> Stopping decryption");
                break;
            }
            List<Map<String, Object>> itemsToCrawl = resource_data_list;
            if (maxItemsLimit > 0 && numberofCrawledPostsTotal + numberofitemsOnThisPage > maxItemsLimit) {
                /* Do not exceed the user defined limit */
                itemsToCrawl = resource_data_list.subList(0, maxItemsLimit - numberofCrawledPostsTotal);
            }
            ret.addAll(this.crawlPostListAltAPI(param, itemsToCrawl, metadata));
            numberofCrawledPostsTotal += itemsToCrawl.size();
            logger.info("Crawled page: " + page + " | Crawled posts so far: " + numberofCrawledPostsTotal + "/" + totalNumberofPosts);
            if (!more_available) {
                logger.info("Stopping because: More_available == false");
                break;
            } else if (numberofCrawledPostsTotal >= totalNumberofPosts) {
                logger.info("Stopping because: Found number of items is higher or equal to expected number of items");
                break;
            } else if (StringUtils.isEmpty(nextid)) {
                logger.info("Stopping because: No nextid available");
                break;
            } else if (maxItemsLimit > 0 && numberofCrawledPostsTotal >= maxItemsLimit) {
                logger.info("Stopping because: Reached user defined max items limit of " + maxItemsLimit);
                break;
            }
            /* Continue to next page */
            page++;
        } while (!this.isAbort());
        return ret;
    }

    /** Crawls: https://www.instagram.com/stories/highlights/<numbers>/ */
    private ArrayList<DownloadLink> crawlStoryHighlight(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String storyHighlightID) throws UnsupportedEncodingException, Exception {
        if (StringUtils.isEmpty(storyHighlightID)) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        /* Login is required to crawl such elements! */
        this.loginOrFail(account, loggedIN);
        InstaGramCom.prepBRAltAPI(this.br);
        InstaGramCom.getPageAltAPI(account, this.br, InstaGramCom.ALT_API_BASE + "/feed/reels_media/?reel_ids=highlight%3A" + storyHighlightID);
        final Map<String, Object> entries = restoreFromString(br.toString(), TypeRef.MAP);
        final InstagramMetadata metadata = new InstagramMetadata();
        final Map<String, Object> reel = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "reels/highlight:" + storyHighlightID);
        return this.crawlPostAltAPI(param, metadata, reel);
    }

    private ArrayList<DownloadLink> crawlStory(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final boolean addDummyItemOnNoItemsFound, final String username, final String storyID) throws UnsupportedEncodingException, Exception {
        if (StringUtils.isEmpty(username)) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        } else if (StringUtils.isEmpty(storyID)) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        if (username.equalsIgnoreCase("highlights")) {
            return this.crawlStoryHighlight(param, account, loggedIN, storyID);
        } else {
            return crawlStory(param, account, loggedIN, username, addDummyItemOnNoItemsFound);
        }
    }

    /**
     * Crawls current story of given username. </br>
     *
     * @param handleErrors
     *            true = throw exception on error e.g. if no account is given and add dummy item if user has no story. </br>
     *            false = return empty array on error
     */
    private ArrayList<DownloadLink> crawlStory(final CryptedLink param, final Account account, final AtomicBoolean loggedIN, final String username, final boolean handleErrors) throws UnsupportedEncodingException, Exception {
        if (StringUtils.isEmpty(username)) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        } else if (account == null && !handleErrors) {
            return new ArrayList<DownloadLink>();
        }
        /* We need to be loggedIN to be able to see stories of users! */
        this.loginOrFail(account, loggedIN);
        final InstagramMetadata metadata = new InstagramMetadata(username);
        final String userID = findUserID(param, account, loggedIN, username);
        if (StringUtils.isEmpty(userID)) {
            /* Most likely that profile doesn't exist */
            final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
            ret.add(getDummyDownloadlinkProfileOffline(username));
            return ret;
        }
        InstaGramCom.prepBRAltAPI(this.br);
        /* 2026-10-30: Same endpoint as used by instaloader. Old endpoint: /feed/user/<userID>/reel_media/ */
        InstaGramCom.getPageAltAPI(account, this.br, InstaGramCom.ALT_API_BASE + "/feed/reels_media/?reel_ids=" + userID);
        final Map<String, Object> entries = restoreFromString(br.toString(), TypeRef.MAP);
        /* reels map is empty or does not contain this user if the user has no story or if the profile is private and not followed. */
        final Map<String, Object> reel_media = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "reels/" + userID);
        final List<Map<String, Object>> storyItems = reel_media != null ? (List<Map<String, Object>>) reel_media.get("items") : null;
        if (storyItems == null || storyItems.size() == 0) {
            final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
            if (handleErrors) {
                final DownloadLink dummy = this.createOfflinelink(param.getCryptedUrl(), "PROFILE_HAS_NO_STORY_" + username, "This profile currently doesn't have a story or is private and not followed: " + username);
                ret.add(dummy);
            }
            return ret;
        }
        return this.crawlPostAltAPI(param, metadata, reel_media);
    }

    /**
     * Crawls all highlight stories of given username. </br>
     *
     * @param handleErrors
     *            true = throw exception on error e.g. if no account is given and add dummy item if user has no story. </br>
     *            false = return empty array on error
     */
    private ArrayList<DownloadLink> crawlAllHighlightStories(final String username, final Account account, final AtomicBoolean loggedIN, final boolean handleErrors) throws UnsupportedEncodingException, Exception {
        if (username == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (account == null && !handleErrors) {
            return new ArrayList<DownloadLink>();
        }
        /* Login is required to crawl such elements! */
        this.loginOrFail(account, loggedIN);
        final String userID = this.findUserID(null, account, loggedIN, username);
        if (userID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        InstaGramCom.prepBRAltAPI(this.br);
        InstaGramCom.getPageAltAPI(account, this.br, InstaGramCom.ALT_API_BASE + "/highlights/" + userID + "/highlights_tray/");
        final Map<String, Object> entries = restoreFromString(br.toString(), TypeRef.MAP);
        final List<Map<String, Object>> stories = (List<Map<String, Object>>) entries.get("tray");
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        for (final Map<String, Object> story : stories) {
            final String storyID = story.get("id").toString();
            /* These ones will go back into our crawler to find the individual media elements. */
            final String url = this.generateStoryHighlightURL(storyID);
            ret.add(this.createDownloadlink(url));
        }
        if (ret.isEmpty()) {
            logger.info("User has no highlight stories: " + username);
        }
        if (ret.isEmpty() && handleErrors) {
            final DownloadLink dummy = this.createOfflinelink(generateURLProfile(username), "PROFILE_HAS_NO_STORY_HIGHLIGHTS_" + username, "This profile currently doesn't have any story highlights: " + username);
            ret.add(dummy);
        }
        return ret;
    }

    private DownloadLink getDummyDownloadlinkProfileEmpty(final String username) {
        return this.createOfflinelink(generateURLProfile(username), "PROFILE_EMPTY_" + username, "The following profile doesn't contain any posts: " + username);
    }

    private DownloadLink getDummyDownloadlinkProfileOffline(final String username) {
        return this.createOfflinelink(generateURLProfile(username), "PROFILE_NOT_FOUND_" + username, "The following profile doesn't exist: " + username);
    }

    private DownloadLink getDummyDownloadlinkProfilePrivate(final String username) {
        return this.createOfflinelink(generateURLProfile(username), "PRIVATE_PROFILE_PERMISSIONS_MISSING_" + username, "The following profile is private and you do not have the required permissions to access it: " + username);
    }

    private DownloadLink getDummyDownloadlinkProfileHasNoSavedPosts(final String username) {
        return this.createOfflinelink(generateURLProfileSavedPosts(username), "PROFILE_CONTAINS_NO_SAVED_POSTS_" + username, "The following profile doesn't contain any saved posts: " + username);
    }

    /** Crawls list of post/story items obtained via alt API. */
    private ArrayList<DownloadLink> crawlPostListAltAPI(final CryptedLink param, final List<Map<String, Object>> mediaItems, final InstagramMetadata metadata) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        for (final Map<String, Object> mediaItem : mediaItems) {
            ret.addAll(this.crawlPostAltAPI(param, metadata, mediaItem));
        }
        return ret;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* Allow faster processing in debug mode. */
            return 2;
        } else {
            /* 2020-01-21: Set to 1 to avoid download issues and try not to perform too many requests at the same time. */
            return 1;
        }
    }

    protected class InstagramMetadata {
        private String description   = null;
        private String hashtag       = null;
        private String mainContentID = null;
        private String packageName   = null;
        private String storyTitle    = null;
        private String username      = null;
        private Date   date          = null;

        public InstagramMetadata() {
        }

        public InstagramMetadata(final String username) {
            this.username = username;
        }

        public Object clone() throws CloneNotSupportedException {
            return super.clone();
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }

        public Date getDate() {
            return date;
        }

        public void setDate(Date date) {
            this.date = date;
        }

        public String getDateFormatted() {
            if (this.date == null) {
                return null;
            }
            return new SimpleDateFormat("yyyy-MM-dd").format(this.date);
        }

        public String getHashtag() {
            return hashtag;
        }

        public void setHashtag(String hashtag) {
            this.hashtag = hashtag;
        }

        public String getMainContentID() {
            return mainContentID;
        }

        public void setMainContentID(String mainContentID) {
            this.mainContentID = mainContentID;
        }

        public String getPackageName() {
            return packageName;
        }

        public void setPackageName(String packageName) {
            this.packageName = packageName;
        }

        public String getStoryTitle() {
            return storyTitle;
        }

        public void setStoryTitle(String title) {
            this.storyTitle = title;
        }

        public String getUsername() {
            return username;
        }

        public void setUsername(String username) {
            this.username = username;
        }

        public FilePackage getFilePackage() {
            if (this.getPackageName() == null) {
                return null;
            }
            final FilePackage fp = InstaGramComDecrypter.getFilePackage();
            fp.setName(this.getPackageName());
            final String cachedUserID = getCachedUserID(getUsername());
            // use setPackageKey because different user may result in same package name after name cleanup
            if (cachedUserID != null) {
                fp.setPackageKey(cachedUserID);
            } else {
                fp.setPackageKey(this.getPackageName());
            }
            return fp;
        }
    }

    public static FilePackage getFilePackage() {
        final FilePackage fp = FilePackage.getInstance();
        fp.setAllowMerge(true);
        fp.setCleanupPackageName(false);
        return fp;
    }

    private String generateStoryHighlightURL(final String storyID) {
        final String highlightID = storyID.split(":")[1];
        return "https://www." + this.getHost() + "/stories/highlights/" + highlightID + "/";
    }

    private String generateURLPost(final String postID) {
        return "https://www." + this.getHost() + "/p/" + postID + "/";
    }

    private String generateURLProfile(final String username) {
        return "https://www." + this.getHost() + "/" + username + "/";
    }

    private String generateURLProfileSavedPosts(final String username) {
        return "https://www." + this.getHost() + "/" + username + "/saved/";
    }
}
