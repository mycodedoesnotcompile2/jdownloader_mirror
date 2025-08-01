//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;
import java.net.URL;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.config.SubConfiguration;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.decrypter.XHamsterGallery;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.downloader.hls.M3U8Playlist;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@HostPlugin(revision = "$Revision: 51294 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { XHamsterGallery.class })
public class XHamsterCom extends PluginForHost {
    public XHamsterCom(PluginWrapper wrapper) {
        super(wrapper);
        /* Actually only free accounts are supported */
        this.enablePremium("https://" + domain_premium + "/join");
        setConfigElements();
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        prepBr(this, br);
        return br;
    }

    public static void prepBr(final Plugin plugin, final Browser br) {
        /* Set some cookies on all supporte free domains. */
        for (final String[] domains : getPluginDomains()) {
            for (final String domain : domains) {
                br.setCookie(domain, "lang", "en");
                br.setCookie(domain, "playerVer", "old");
                br.setCookie(domain, "video_titles_translation", "0");
            }
        }
        for (final String domain : new String[] { "xhamsterpremium.com", "faphouse.com" }) {
            br.setCookie(domain, "locale", "en");
            br.setCookie(domain, "translate-video-titles", "0");
        }
        /**
         * 2022-07-22: Workaround for possible serverside bug: </br> In some countries, xhamster seems to redirect users to xhamster2.com.
         * </br> If those users send an Accept-Language header of "de,en-gb;q=0.7,en;q=0.3" they can get stuck in a redirect-loop between
         * deu.xhamster3.com and deu.xhamster3.com. </br> See initial report: https://board.jdownloader.org/showthread.php?t=91170
         */
        final String acceptLanguage = "en-gb;q=0.7,en;q=0.3";
        br.setAcceptLanguage(acceptLanguage);
        br.getHeaders().put("Accept-Language", acceptLanguage);
        /* 2023-08-29: They've started to block older user agents. */
        br.getHeaders().put("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36");
        br.setAllowedResponseCodes(new int[] { 400, 410, 423, 452 });
        br.setFollowRedirects(true);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    /** Make sure this is the same in classes XHamsterCom and XHamsterGallery! */
    private static List<String[]> getPluginDomains() {
        return XHamsterGallery.getPluginDomains();
    }

    public static String[] getDeadDomains() {
        /* Add dead domains here so plugin can correct domain in added URL if it is a dead domain. */
        return new String[] { "gold.xhamsterpremium.com", "xhamsterpremium.com", "airportxh.life" };
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
            /* Videos current pattern */
            String pattern = "https?://(?:[a-z0-9\\-]+\\.)?" + buildHostsPatternPart(domains) + "/(?:videos|moments)/[a-z0-9\\-_]+-[A-Za-z0-9]+";
            /* E.g. xhamster.tv */
            pattern += "|https?://(?:[a-z0-9\\-]+\\.)?" + buildHostsPatternPart(domains) + "/video/[a-z0-9\\-]+";
            /* Embed pattern: 2020-05-08: /embed/123 = current pattern, x?embed.php = old one */
            pattern += "|https?://(?:[a-z0-9\\-]+\\.)?" + buildHostsPatternPart(domains) + "/(embed/[A-Za-z0-9]+|x?embed\\.php\\?video=[A-Za-z0-9]+)";
            /* Movies old pattern --> Redirects to TYPE_VIDEOS_2 (or TYPE_VIDEOS_3) */
            pattern += "|https?://(?:[a-z0-9\\-]+\\.)?" + buildHostsPatternPart(domains) + "/movies/[0-9]+/[^/]+\\.html";
            /* Premium pattern */
            pattern += "|https?://(?:gold\\.xhamsterpremium\\.com|faphouse\\.com)/([a-z]{2}/)?videos/([A-Za-z0-9\\-]+)";
            ret.add(pattern);
        }
        return ret.toArray(new String[0]);
    }

    public static String buildHostsPatternPart(String[] domains) {
        final StringBuilder pattern = new StringBuilder();
        pattern.append("(?:");
        for (int i = 0; i < domains.length; i++) {
            final String domain = domains[i];
            if (i > 0) {
                pattern.append("|");
            }
            if ("xhamster.com".equals(domain)) {
                pattern.append("xhamster\\d*\\.(?:com|xxx|desi|one)");
            } else {
                pattern.append(Pattern.quote(domain));
            }
        }
        pattern.append(")");
        return pattern.toString();
    }

    /* Porn_plugin */
    private final String          SETTING_SELECTED_VIDEO_FORMAT                             = "SELECTED_VIDEO_FORMAT_2";
    private final int             default_SETTING_SELECTED_VIDEO_FORMAT                     = 0;
    private final String          SETTING_FILENAME_ID                                       = "Filename_id";
    private final boolean         default_SETTING_FILENAME_ID                               = true;
    /* The list of qualities/formats displayed to the user */
    private static final String[] FORMATS                                                   = new String[] { "Best available", "144p", "240p", "360p", "480p", "720p", "960p", "1080p", "1440p", "2160p" };
    public static final String    domain_premium                                            = "faphouse.com";
    public static final String    api_base_premium                                          = "https://faphouse.com/api";
    private static final String   TYPE_MOVIES                                               = "(?i)^https?://[^/]+/movies/(\\d+)/([^/]+)\\.html$";
    private static final String   TYPE_VIDEOS                                               = "(?i)^https?://[^/]+/(?:[a-z]{2}/)?videos?/([A-Za-z0-9\\-]+)$";
    private static final String   TYPE_VIDEOS_2                                             = "(?i)^https?://[^/]+/videos/([a-z0-9\\-_]+)-(\\d+)$";
    private static final String   TYPE_VIDEOS_3                                             = "(?i)^https?://[^/]+/videos/([a-z0-9\\-_]+)-([A-Za-z0-9]+)$";
    private static final String   TYPE_MOMENTS                                              = "(?i)^https?://[^/]+/moments/([a-z0-9\\-_]+)-([A-Za-z0-9]+)$";
    private final String          PROPERTY_USERNAME                                         = "username";
    private final String          PROPERTY_DATE                                             = "date";
    private final String          PROPERTY_TAGS                                             = "tags";
    private final static String   PROPERTY_VIDEOID                                          = "videoid";
    private final String          PROPERTY_ACCOUNT_LAST_USED_FREE_DOMAIN                    = "last_used_free_domain";
    private final String          PROPERTY_ACCOUNT_PREMIUM_LOGIN_URL                        = "premium_login_url";
    /*
     * Timestamp of when this account was a premium only account last time which means login via xhamster.com was not possible while login
     * via faphouse.com revealed that this account was a valid premium account.
     */
    private final String          PROPERTY_ACCOUNT_TIMESTAMP_LAST_TIME_PREMIUM_ONLY_ACCOUNT = "timestamp_last_time_premium_only_account";
    private static final String   COOKIE_KEY_PREMIUM                                        = "premium";

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_COMBOBOX_INDEX, getPluginConfig(), SETTING_SELECTED_VIDEO_FORMAT, FORMATS, "Preferred format").setDefaultValue(default_SETTING_SELECTED_VIDEO_FORMAT));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), SETTING_FILENAME_ID, "Only for videos: Change file name to 'filename_VideoID.ext' e.g. 'test_48604.mp4' ?").setDefaultValue(default_SETTING_FILENAME_ID));
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/info/terms";
    }

    public static final String  TYPE_MOBILE    = "(?i).+m\\.xhamster\\.+";
    public static final String  TYPE_EMBED     = "(?i)^https?://[^/]+/(?:x?embed\\.php\\?video=|embed/)([A-Za-z0-9\\-]+)";
    private static final String TYPE_PREMIUM   = "(?i).+(xhamsterpremium\\.com|faphouse\\.com).+";
    private static final String NORESUME       = "NORESUME";
    private final String        recaptchav2    = "<div class=\"text\">\\s*In order to watch this video please prove you are a human";
    private String              dllink         = null;
    private String              vq             = null;
    public static final String  DOMAIN_CURRENT = "xhamster.com";

    public static String getCorrectedURL(String url) {
        /*
         * Remove language-subdomain to enforce original/English language else xhamster may auto-translate video-titles based on that
         * subdomain. ge(germany), fra(france)...
         */
        url = url.replaceFirst("://(www\\.)?([a-z]{2,3}\\.)?", "://");
        final String domainFromURL = Browser.getHost(url, true);
        String newDomain = domainFromURL;
        for (final String deadDomain : getDeadDomains()) {
            if (StringUtils.equalsIgnoreCase(domainFromURL, deadDomain)) {
                newDomain = DOMAIN_CURRENT;
                break;
            }
        }
        if (!StringUtils.equals(domainFromURL, newDomain)) {
            if (url.matches(TYPE_MOBILE) || url.matches(TYPE_EMBED)) {
                url = "https://" + newDomain + "/videos/" + new Regex(url, TYPE_EMBED).getMatch(0);
            } else {
                /* Change domain in URL */
                url = url.replaceFirst(Pattern.quote(domainFromURL), newDomain);
            }
        }
        return url;
    }

    /** Returns true if the full content behind the given URL can only be viewed with a paid account. */
    private boolean isPremiumURL(final String url) {
        if (url == null) {
            return false;
        } else if (url.matches(TYPE_PREMIUM)) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return this.getHost() + "://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    private static String getFID(final DownloadLink link) {
        if (link.getPluginPatternMatcher() == null) {
            return null;
        } else {
            final String videoid = link.getStringProperty(PROPERTY_VIDEOID);
            if (videoid != null) {
                return videoid;
            } else {
                return getFID(link.getPluginPatternMatcher());
            }
        }
    }

    private static String getFID(final String url) {
        if (url == null) {
            return null;
        } else {
            if (url.matches(TYPE_EMBED)) {
                return new Regex(url, TYPE_EMBED).getMatch(0);
            } else if (url.matches(TYPE_MOBILE)) {
                return new Regex(url, "https?://[^/]+/[^/]+/[^/]*?([a-z0-9]+)(/|$|\\?)").getMatch(0);
            } else if (url.matches(TYPE_MOVIES)) {
                return new Regex(url, TYPE_MOVIES).getMatch(0);
            } else if (url.matches(TYPE_MOMENTS)) {
                return new Regex(url, TYPE_MOMENTS).getMatch(1);
            } else if (url.matches(TYPE_VIDEOS_3)) {
                // first we check title-FID
                return new Regex(url, TYPE_VIDEOS_3).getMatch(1);
            } else if (url.matches(TYPE_VIDEOS_2)) {
                // then we check next title-NUMBER FID
                return new Regex(url, TYPE_VIDEOS_2).getMatch(1);
            } else if (url.matches(TYPE_VIDEOS)) {
                // then we check last FID
                return new Regex(url, TYPE_VIDEOS).getMatch(0);
            } else {
                /* This should never happen */
                return null;
            }
        }
    }

    private static String getUrlTitle(final String url) {
        // order is important, see getFID
        if (url.matches(TYPE_MOMENTS)) {
            return new Regex(url, TYPE_MOMENTS).getMatch(0);
        } else if (url.matches(TYPE_VIDEOS_3)) {
            return new Regex(url, TYPE_VIDEOS_3).getMatch(0);
        } else if (url.matches(TYPE_VIDEOS_2)) {
            return new Regex(url, TYPE_VIDEOS_2).getMatch(0);
        } else if (url.matches(TYPE_MOVIES)) {
            return new Regex(url, TYPE_MOVIES).getMatch(1);
        } else {
            /* All other linktypes do not contain any title hint --> Return fid */
            return null;
        }
    }

    private String getFallbackFileTitle(final String url) {
        if (url == null) {
            return null;
        }
        final String urlTitle = getUrlTitle(url);
        if (urlTitle != null) {
            return urlTitle;
        } else {
            return getFID(getDownloadLink());
        }
    }

    @Override
    public boolean isProxyRotationEnabledForLinkChecker() {
        return false;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        Account premiumAccount = null;
        final ArrayList<Account> accounts = AccountController.getInstance().getValidAccounts(this.getHost());
        if (accounts != null && accounts.size() > 0) {
            for (final Account acc : accounts) {
                if (isPremiumAccount(acc)) {
                    premiumAccount = acc;
                    break;
                }
            }
            if (premiumAccount != null) {
                return requestFileInformation(link, premiumAccount);
            } else {
                /* No premium account available -> Use first in list */
                return requestFileInformation(link, accounts.get(0));
            }
        } else {
            /* No account */
            return requestFileInformation(link, null);
        }
    }

    private String getTitle(final DownloadLink link, final Browser br) throws Exception {
        String title = br.getRegex("\"videoEntity\"\\s*:\\s*\\{[^\\}\\{]*\"title\"\\s*:\\s*\"(.*?)\"\\s*,").getMatch(0);
        if (title != null) {
            // JSON String to String, no further post processing required
            title = (String) JavaScriptEngineFactory.jsonToJavaObject("\"" + title + "\"");
        }
        final String titleLocalized = br.getRegex("\"videoEntity\"\\s*:\\s*\\{[^\\}\\{]*\"titleLocalized\"\\s*:\\s*(\\{.*?\\})\\s*,").getMatch(0);
        if (title == null && titleLocalized != null) {
            // JSON, no further post processing required
            final Map<String, Object> map = restoreFromString(titleLocalized, TypeRef.MAP);
            title = (String) map.get("value");
        }
        if (title == null) {
            /* Premium content */
            title = br.getRegex("class=\"video__title\">\\s*([^<]+)\\s*</h1>").getMatch(0);
            title = URLEncode.decodeURIComponent(Encoding.htmlOnlyDecode(title));
        }
        if (title == null) {
            title = br.getRegex("<meta property\\s*=\\s*\"og:title\"\\s*content\\s*=\\s*\"(.*?)\"\\s*>").getMatch(0);
            title = URLEncode.decodeURIComponent(Encoding.htmlOnlyDecode(title));
        }
        if (title == null) {
            title = br.getRegex("<title[^>]*>([^<>\"]*?)\\s*(-|\\|)\\s*xHamster[^<]*</title>").getMatch(0);
            title = URLEncode.decodeURIComponent(Encoding.htmlOnlyDecode(title));
        }
        if (title == null) {
            /* Fallback to URL filename - first try to get nice name from URL. */
            title = new Regex(br.getURL(), "/(?:videos|movies|moments)/(.+)(?:$|\\?)").getMatch(0);
            if (title != null) {
                final String fid = getFID(link);
                title = title.replaceFirst("-?" + Pattern.quote(fid), "");
            }
            title = URLEncode.decodeURIComponent(title);
        }
        if (StringUtils.isEmpty(title)) {
            title = getFID(link);
        }
        return title;
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        vq = null;
        dllink = null;
        final boolean isDownload = this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD;
        final String contentURL = getCorrectedURL(link.getPluginPatternMatcher());
        final String extDefault = ".mp4";
        if (!link.isNameSet()) {
            link.setName(getFallbackFileTitle(contentURL) + extDefault);
        }

        if (account != null) {
            login(account, contentURL, true);
        } else {
            br.getPage(contentURL);
        }
        if (StringUtils.containsIgnoreCase(br.getURL(), "/site/error")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Check for self-embed */
        String selfEmbeddedURL = br.getRegex("(?i)<iframe[^>]*src\\s*=\\s*\"(https?://xh\\.video/(?:[A-Za-z])/" + getFID(link) + ")\"[^>]*></iframe>").getMatch(0);
        if (selfEmbeddedURL == null) {
            selfEmbeddedURL = br.getRegex("(?i)<iframe[^>]*src\\s*=\\s*\"(https?://xh\\.video/(?:[A-Za-z])/[^\"]+)\"[^>]*></iframe>").getMatch(0);
        }
        if (selfEmbeddedURL != null) {
            /* 2022-09-12: Some special domains like xhamster.one / xhamster.tv show a different page and self-embeds */
            logger.info("Found self-embed: " + selfEmbeddedURL);
            br.getPage(selfEmbeddedURL);
            /* Now this may have sent us to an embed URL --> Fix that */
            this.embedToNormalHandling(br, link);
        } else if (br.getURL().matches(TYPE_EMBED)) {
            this.embedToNormalHandling(br, link);
        }
        final int responsecode = br.getRequest().getHttpConnection().getResponseCode();
        if (responsecode == 423) {
            if (isVideoOnlyForFriends(br)) {
                return AvailableStatus.TRUE;
            } else if (br.containsHTML("<title>\\s*Page was deleted\\s*</title>")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (isPasswordProtected(br)) {
                return AvailableStatus.TRUE;
            } else {
                String exactErrorMessage = br.getRegex("class=\"item-status not-found\">\\s*<i class=\"xh-icon smile-sad cobalt\"></i>\\s*<div class=\"status-text\">([^<>]+)</div>").getMatch(0);
                if (exactErrorMessage == null) {
                    /* 2021-07-27 */
                    exactErrorMessage = br.getRegex("class=\"error-title\"[^>]*>([^<>\"]+)<").getMatch(0);
                }
                if (exactErrorMessage != null) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 423: " + exactErrorMessage, 60 * 60 * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 423", 60 * 60 * 1000l);
                }
            }
        } else if (responsecode == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (responsecode == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (responsecode == 451) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Error 451 Unavailable For Legal Reasons");
        } else if (responsecode == 452) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Set some Packagizer properties */
        String username = br.getRegex("class=\"entity-author-container__name\"[^>]*href=\"https?://[^/]+/users/([^<>\"]+)\"").getMatch(0);
        String datePublished = br.getRegex("\"datePublished\":\"(\\d{4}-\\d{2}-\\d{2})\"").getMatch(0);
        String filename = null;
        if (this.isPremiumURL(contentURL)) {
            String title = getTitle(link, br);
            if (this.isPremiumAccount(account)) {
                /* Premium users can download the full videos in different qualities. */
                if (isDownload) {
                    dllink = getDllinkPremium(link, true);
                } else {
                    final String filesizeStr = getDllinkPremium(link, false);
                    if (filesizeStr != null) {
                        link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                    }
                }
            } else {
                /* Free / Free-Account users can only download trailers. */
                dllink = br.getRegex("id=\"video-trailer\"[^<]*data-fallback=\"(http[^<>\"]+)\"").getMatch(0); // Progressive
                if (dllink == null) {
                    dllink = br.getRegex("id=\"video-trailer\"[^<]*src=\"(http[^<>\"]+)\"").getMatch(0); // HLS
                }
            }
            if (title != null) {
                filename = title + extDefault;
                link.setFinalFileName(filename);
            }
            datePublished = br.getRegex("class=\"video-publish-date\"[^>]*>[^0-9]*([0-9\\.]+)\\s*</span>").getMatch(0);
            username = br.getRegex("class=\"video-info-details__row\">\\s*<a href=\"/(?:[a-z]{2}/)?studios/([\\w-]+)\"").getMatch(0);
        } else {
            /* Free content */
            // embeded correction --> Usually not needed
            if (contentURL.matches("(?i).+/xembed\\.php.*")) {
                logger.info("Trying to change embed URL --> Real URL");
                String realpage = br.getRegex("(?i)main_url=(https?[^\\&]+)").getMatch(0);
                if (realpage != null && !StringUtils.equals(realpage, contentURL)) {
                    logger.info("Successfully changed: " + contentURL + " ----> " + realpage);
                    link.setUrlDownload(Encoding.htmlDecode(realpage));
                    br.getPage(realpage);
                } else {
                    logger.info("Failed to change embed URL --> Real URL");
                }
            }
            // recaptchav2 here, don't trigger captcha until download....
            if (br.containsHTML(recaptchav2)) {
                if (isDownload) {
                    final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                    final Browser captcha = br.cloneBrowser();
                    captcha.getHeaders().put("Accept", "*/*");
                    captcha.getHeaders().put("X-Requested-With", "XMLHttpRequest");
                    captcha.getPage("/captcha?g-recaptcha-response=" + recaptchaV2Response);
                    br.getPage(br.getURL());
                } else {
                    /* Do not ask user to solve captcha during availablecheck, only during download! */
                    return AvailableStatus.UNCHECKABLE;
                }
            }
            if (br.containsHTML("(403 Forbidden|>\\s*This video was deleted\\s*<)")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            try {
                dllink = this.getDllink(br);
            } catch (Exception e) {
                if (PluginEnvironment.DOWNLOAD.equals(getPluginEnvironment())) {
                    throw e;
                } else {
                    getLogger().log(e);
                }
            }
            final String fid = getFID(link);
            String title = getTitle(link, br);
            final String ext;
            if (StringUtils.containsIgnoreCase(dllink, ".m3u8")) {
                ext = extDefault;
            } else if (!StringUtils.isEmpty(dllink)) {
                ext = getFileNameExtensionFromString(dllink, extDefault);
            } else {
                ext = extDefault;
            }
            if (title != null) {
                // title = Encoding.htmlDecode(Encoding.unicodeDecode(title));
                if (getPluginConfig().getBooleanProperty(SETTING_FILENAME_ID, default_SETTING_FILENAME_ID)) {
                    filename = title + "_" + fid;
                } else {
                    filename = fid + "_" + title;
                }
                if (vq != null) {
                    filename = Encoding.htmlDecode(filename.trim() + "_" + vq).trim();
                } else {
                    filename = Encoding.htmlDecode(filename).trim();
                }
                filename += ext;
                link.setFinalFileName(filename);
            }
        }
        if (username != null) {
            link.setProperty(PROPERTY_USERNAME, Encoding.htmlDecode(username).trim());
        } else {
            logger.warning("Failed to find tag: " + PROPERTY_USERNAME);
        }
        final String[] tagsList = br.getRegex("<a class=\"categories-container__item\"[^>]*href=\"https?://[^/]+/tags/([^\"]+)\"").getColumn(0);
        if (tagsList.length > 0) {
            final StringBuilder sb = new StringBuilder();
            for (String tag : tagsList) {
                tag = Encoding.htmlDecode(tag).trim();
                if (StringUtils.isNotEmpty(tag)) {
                    if (sb.length() > 0) {
                        sb.append(",");
                    }
                    sb.append(tag);
                }
            }
            if (sb.length() > 0) {
                link.setProperty(PROPERTY_TAGS, sb.toString());
            }
        }
        if (datePublished != null) {
            link.setProperty(PROPERTY_DATE, datePublished);
        } else {
            logger.warning("Failed to find tag: " + PROPERTY_DATE);
        }
        /* 2020-01-31: Do not check filesize if we're currently in download mode as directurl may expire then. */
        if (!StringUtils.isEmpty(dllink) && !link.isSizeSet() && !StringUtils.containsIgnoreCase(dllink, ".m3u8") && !isDownload) {
            final Browser brc = br.cloneBrowser();
            this.basicLinkCheck(brc, br.createHeadRequest(this.dllink), link, filename, extDefault);
        }
        return AvailableStatus.TRUE;
    }

    private boolean isPremiumAccount(final Account account) {
        if (account == null || account.getType() == null) {
            return false;
        } else if (account.getType().equals(AccountType.PREMIUM) || account.getType().equals(AccountType.LIFETIME)) {
            return true;
        } else {
            return false;
        }
    }

    /** Returns true if this account can only be used to login in faphouse.com and not for xhamster.com. */
    private boolean isPremiumOnlyAccount(final Account account) {
        return account.hasProperty(PROPERTY_ACCOUNT_TIMESTAMP_LAST_TIME_PREMIUM_ONLY_ACCOUNT);
    }

    @Override
    protected void handleConnectionErrors(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Video broken?");
        }
    }

    /** Looks for normal video URL in html code and accesses it if necessary. */
    private void embedToNormalHandling(final Browser br, final DownloadLink link) throws IOException {
        final String nonEmbedURL = findNonEmbedURL(br);
        if (nonEmbedURL == null) {
            logger.warning("Failed to find nonEmbedURL -> Content offline?");
            return;
        } else if (!StringUtils.equalsIgnoreCase(br.getURL(), nonEmbedURL)) {
            logger.info("Found non-embed URL: Old: " + br.getURL() + " | New: " + nonEmbedURL);
            br.getPage(nonEmbedURL);
            final String realVideoID = getFID(nonEmbedURL);
            if (realVideoID != null) {
                link.setProperty(PROPERTY_VIDEOID, realVideoID);
            }
        }
    }

    /**
     * Designed to find "real" URL inside html of embed video.
     */
    private String findNonEmbedURL(final Browser br) {
        String url = br.getRegex("class=\"xh-helper-hidden xplayer-fallback-image\" href=\"(https?://[^/]+/videos/[\\w\\-]+)").getMatch(0);
        if (url == null) {
            url = PluginJSonUtils.getJson(br, "video_url");
        }
        if (StringUtils.isEmpty(url)) {
            return null;
        }
        final String videoidFromCurrentURL = getFID(br.getURL());
        /* Check if this is the correct URL. */
        if (videoidFromCurrentURL == null || url.contains(videoidFromCurrentURL)) {
            return url;
        } else {
            return null;
        }
    }

    /**
     * @returns: Not null = video is only available for friends of user XXX.
     */
    private String isVideoOnlyForFriendsOf(final Browser br) {
        String friendsname = br.getRegex(">([^<>\"]*?)</a>\\'s friends only\\s*</div>").getMatch(0);
        if (StringUtils.isEmpty(friendsname)) {
            /* 2019-06-05 */
            friendsname = br.getRegex("This video is visible to\\s*<br>\\s*friends of\\s*<a href=\"[^\"]+\">([^<>\"]+)</a> only").getMatch(0);
        }
        if (friendsname != null) {
            return Encoding.htmlDecode(friendsname).trim();
        } else {
            return null;
        }
    }

    private boolean isVideoOnlyForFriends(final Browser br) {
        if (br.getHttpConnection().getResponseCode() == 423 && br.containsHTML(">\\s*This (gallery|video) is visible (for|to)\\s*<")) {
            return true;
        } else {
            return false;
        }
    }

    private boolean isPasswordProtected(final Browser br) {
        return br.containsHTML("class\\s*=\\s*\"video\\-password\\-block\"");
    }

    private boolean isPaidContent(final Browser br) {
        if (br.containsHTML("class\\s*=\\s*\"buy_tips\"|<tipt>\\s*This video is paid\\s*</tipt>")) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Returns best filesize if isDownload == false, returns best downloadurl if isDownload == true.
     *
     * @throws Exception
     */
    private String getDllinkPremium(final DownloadLink link, final boolean isDownload) throws Exception {
        final boolean allowOldWay = false;
        final int userPreferredQualityHeight = getPreferredQualityHeight();
        if (allowOldWay) {
            final String[] htmls = br.getRegex("(<a[^<>]*class\\s*=\\s*\"list__item[^\"]*\".*?</a>)").getColumn(0);
            if (htmls != null && htmls.length > 0) {
                int foundHighestQualityHeight = -1;
                int foundUserPreferredHeight = -1;
                String internalVideoID = null;
                String filesizeHighestStr = null;
                String filesizeUserPreferredStr = null;
                for (final String html : htmls) {
                    final String qualityIdentifierStr = new Regex(html, "(\\d+)p").getMatch(0);
                    final String qualityFilesizeStr = new Regex(html, "\\((\\d+ (MB|GB))\\)").getMatch(0);
                    if (qualityIdentifierStr == null || qualityFilesizeStr == null) {
                        /* Skip invalid items */
                        continue;
                    }
                    if (internalVideoID == null) {
                        /* This id is the same for every quality. */
                        internalVideoID = new Regex(html, "data\\-el\\-item\\-id\\s*=\\s*\"(\\d+)\"").getMatch(0);
                    }
                    final int heightTmp = Integer.parseInt(qualityIdentifierStr);
                    if (heightTmp == userPreferredQualityHeight) {
                        foundUserPreferredHeight = heightTmp;
                        filesizeUserPreferredStr = qualityFilesizeStr;
                        break;
                    }
                    if (heightTmp > foundHighestQualityHeight || foundHighestQualityHeight == -1) {
                        foundHighestQualityHeight = heightTmp;
                        filesizeHighestStr = qualityFilesizeStr;
                    }
                }
                final int chosenQualityHeight;
                final String chosenQualityFilesizeStr;
                if (filesizeUserPreferredStr != null) {
                    /* Found user preferred quality */
                    chosenQualityFilesizeStr = filesizeUserPreferredStr;
                    chosenQualityHeight = foundUserPreferredHeight;
                } else {
                    /* Highest quality */
                    chosenQualityFilesizeStr = filesizeHighestStr;
                    chosenQualityHeight = foundHighestQualityHeight;
                }
                if (!isDownload) {
                    /* Return filesize as string */
                    return chosenQualityFilesizeStr;
                }
            }
        }
        if (!isDownload) {
            /* Do not perform http request now to speed up linkcheck */
            return null;
        }
        final String streamHlsMaster = br.getRegex("data-el-hls-url=\"(https?://[^\"]+)").getMatch(0);
        if (br.containsHTML(">\\s*You've reached the download limit for this month")) {
            if (streamHlsMaster != null) {
                logger.info("Download limit reached -> Fallback to HLS stream download");
                return streamHlsMaster;
            }
            throw new AccountUnavailableException("You've reached the download limit for this month", 10 * 60 * 1000);
        }
        final String internalVideoID = br.getRegex("data-el-item-id\\s*=\\s*\"(\\d+)\"").getMatch(0);
        if (internalVideoID == null) {
            if (streamHlsMaster != null) {
                logger.info("Failed to find internal videoID -> Fallback to HLS stream download");
                return streamHlsMaster;
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(String.format(api_base_premium + "/videos/%s/original-video-config", internalVideoID));
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> errors = (Map<String, Object>) entries.get("errors");
        if (errors != null) {
            // final Object _globalO = errors.get("_global");
            // if(_globalO != null) {
            // final List<String> globalErrors = (List<String>)_globalO;
            // }
            throw new PluginException(LinkStatus.ERROR_FATAL, "Official download not possible");
        }
        /* E.g. {"errors":{"_global":["NOT_PURCHASED"]},"userId":1234567,"hasGoldSubscription":true,"username":"username"} */
        final String username = (String) entries.get("username");
        if (username != null) {
            /* Set Packagizer property */
            link.setProperty(PROPERTY_USERNAME, username);
        }
        /* If this is empty, it is an Array instead of a map (wtf) */
        final Object downloadFormatsO = entries.get("downloadFormats");
        Map<String, Object> downloadFormats = null;
        if (downloadFormatsO instanceof Map) {
            downloadFormats = (Map<String, Object>) downloadFormatsO;
        }
        final Map<String, Object> streamFormats = (Map<String, Object>) entries.get("streamFormats");
        if ((downloadFormats == null || downloadFormats.isEmpty()) && (streamFormats == null || streamFormats.isEmpty())) {
            logger.warning("Zero downloadable formats available");
            return null;
        }
        final Map<String, Object> downloadMap;
        if (downloadFormats != null && downloadFormats.size() > 0) {
            downloadMap = downloadFormats;
        } else {
            downloadMap = streamFormats;
        }
        final String preferredFormat = (String) downloadMap.get(Integer.toString(userPreferredQualityHeight));
        if (preferredFormat != null) {
            return preferredFormat;
        }
        final int originalHeight = ((Number) entries.get("originalHeight")).intValue();
        final String originalFormat = (String) downloadMap.get(Integer.toString(originalHeight));
        if (originalFormat != null) {
            return originalFormat;
        }
        /* Return best */
        int heightMax = -1;
        String dllinkMax = null;
        for (final Entry<String, Object> entry : downloadMap.entrySet()) {
            final int thisHeight = Integer.parseInt(entry.getKey());
            if (dllinkMax == null || thisHeight > heightMax) {
                heightMax = thisHeight;
                dllinkMax = entry.getValue().toString();
            }
        }
        return dllinkMax;
    }

    private static Object CONFIG_MIGRATION_LOCK = new Object();

    @Override
    public SubConfiguration getPluginConfig() {
        synchronized (CONFIG_MIGRATION_LOCK) {
            final SubConfiguration ret = super.getPluginConfig();
            if (!ret.hasProperty("SELECTED_VIDEO_FORMAT")) {
                return ret;
            }
            // migrate old stored index, move +1 for >0
            final int oldValue = ret.getIntegerProperty("SELECTED_VIDEO_FORMAT");
            ret.removeProperty("SELECTED_VIDEO_FORMAT");
            if (oldValue > 0) {
                ret.setProperty(SETTING_SELECTED_VIDEO_FORMAT, oldValue + 1);
            }
            return ret;
        }
    }

    private int getPreferredQualityHeight() {
        final int selected_format = getPluginConfig().getIntegerProperty(SETTING_SELECTED_VIDEO_FORMAT, default_SETTING_SELECTED_VIDEO_FORMAT);
        switch (selected_format) {
        case 9:
            return 2160;
        case 8:
            return 1440;
        case 7:
            return 1080;
        case 6:
            return 960;
        case 5:
            return 720;
        case 4:
            return 480;
        case 3:
            return 360;
        case 2:
            return 240;
        case 1:
            return 144;
        default:
        case 0:
            return -1;
        }
    }

    /**
     * NOTE: They also have .mp4 version of the videos in the html code -> For mobile devices Those are a bit smaller in size
     */
    @SuppressWarnings("deprecation")
    public String getDllink(final Browser br) throws IOException, PluginException {
        final SubConfiguration cfg = getPluginConfig();
        final int selected_format = cfg.getIntegerProperty(SETTING_SELECTED_VIDEO_FORMAT, default_SETTING_SELECTED_VIDEO_FORMAT);
        return getDllink(br, selected_format);
    }

    public String getDllink(final Browser br, int selected_format) throws IOException, PluginException {
        final SubConfiguration cfg = getPluginConfig();
        Integer selectedQualityHeight = null;
        final List<String> qualities = new ArrayList<String>();
        /* TODO: selected quality not available -> it would be better to choose next best instead of best */
        switch (selected_format) {
        /* Fallthrough to automatically choose the next best quality */
        default:
        case 0:// best
            selectedQualityHeight = selectedQualityHeight != null ? selectedQualityHeight : -1;
        case 9:
            qualities.add("2160p");
            selectedQualityHeight = selectedQualityHeight != null ? selectedQualityHeight : 2160;
        case 8:
            qualities.add("1440p");
            selectedQualityHeight = selectedQualityHeight != null ? selectedQualityHeight : 1440;
        case 7:
            qualities.add("1080p");
            selectedQualityHeight = selectedQualityHeight != null ? selectedQualityHeight : 1080;
        case 6:
            qualities.add("960p");
            selectedQualityHeight = selectedQualityHeight != null ? selectedQualityHeight : 960;
        case 5:
            qualities.add("720p");
            selectedQualityHeight = selectedQualityHeight != null ? selectedQualityHeight : 720;
        case 4:
            qualities.add("480p");
            selectedQualityHeight = selectedQualityHeight != null ? selectedQualityHeight : 480;
        case 3:
            qualities.add("360p");
            selectedQualityHeight = selectedQualityHeight != null ? selectedQualityHeight : 360;
        case 2:
            qualities.add("240p");
            selectedQualityHeight = selectedQualityHeight != null ? selectedQualityHeight : 240;
        case 1:
            qualities.add("144p");
            selectedQualityHeight = selectedQualityHeight != null ? selectedQualityHeight : 144;
        }
        int chosenQualityHeight = -1;
        String chosenQualityDownloadurl = null;
        final Map<Integer, Number> videoHeightToFilesize = new HashMap<Integer, Number>();
        Map<String, Object> hlsMap = null;
        try {
            final String jsonStr = br.getRegex(">\\s*window\\.initials\\s*=\\s*(\\{.*?\\})\\s*;\\s*</").getMatch(0);
            final Map<String, Object> json = restoreFromString(jsonStr, TypeRef.MAP);
            // TODO: Maybe save subtitle information as plugin property
            // final List<Map<String, Object>> subtitles = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(json,
            // "xplayerPluginSettings/subtitles/tracks");
            // if (subtitles != null) {
            // for (final Map<String, Object> subtitle : subtitles) {
            // }
            // }
            String bestProgressiveQualityDownloadurl = null;
            int maxQualityHeight = 0;
            final Map<String, Object> videoModel = (Map<String, Object>) json.get("videoModel");
            if (videoModel != null && videoModel.containsKey("sources")) {
                final Set<Integer> availableQualities = new HashSet<Integer>();
                /** 2025-07-03: Sometimes max progressive quality for map down below is 720p thus this is a better source. */
                final Map<String, Object> sources = (Map<String, Object>) videoModel.get("sources");
                final Map<String, Object> sources_mp4 = sources == null ? null : (Map<String, Object>) sources.get("mp4");
                final Map<String, Object> sources_download = sources == null ? null : (Map<String, Object>) sources.get("download");
                if (sources_download != null) {
                    /* Collect file size values for later usage */
                    final Iterator<Entry<String, Object>> iterator = sources_download.entrySet().iterator();
                    while (iterator.hasNext()) {
                        final Entry<String, Object> entry = iterator.next();
                        final String qualityStr = entry.getKey();
                        final int qualityHeight = Integer.parseInt(qualityStr.replaceFirst("(?i)p", ""));
                        final Map<String, Object> downloadinfo = (Map<String, Object>) entry.getValue();
                        videoHeightToFilesize.put(qualityHeight, (Number) downloadinfo.get("size"));
                    }
                }
                final Iterator<Entry<String, Object>> iterator = sources_mp4.entrySet().iterator();
                while (iterator.hasNext()) {
                    final Entry<String, Object> entry = iterator.next();
                    final String qualityStr = entry.getKey();
                    final int qualityHeight = Integer.parseInt(qualityStr.replaceFirst("(?i)p", ""));
                    final String url = entry.getValue().toString();
                    if (bestProgressiveQualityDownloadurl == null || qualityHeight > maxQualityHeight) {
                        bestProgressiveQualityDownloadurl = url;
                        maxQualityHeight = qualityHeight;
                    }
                    if (qualityHeight == selectedQualityHeight) {
                        logger.info("Found preferred quality(url):" + qualityStr + "->" + url);
                        chosenQualityHeight = selectedQualityHeight;
                        chosenQualityDownloadurl = url;
                        return url;
                    }
                }
                if (selectedQualityHeight != -1) {
                    logger.info("Did not find preferred quality(videoModel):" + selectedQualityHeight + "|available:" + availableQualities);
                }
            }
            if (chosenQualityDownloadurl == null && bestProgressiveQualityDownloadurl == null) {
                /* 2025-07-03: There is also "xplayerSettings2" which looks to be the same as "xplayerSettings". */
                List<Map<String, Object>> video_sources = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(json, "xplayerSettings/sources/standard/mp4");
                if (video_sources == null) {
                    /* 2023-07-31: VR */
                    video_sources = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(json, "xplayerSettings/sources/standard/h264");
                }
                if (video_sources != null) {
                    final Set<Integer> availableQualities = new HashSet<Integer>();
                    maxQualityHeight = 0;
                    for (final Map<String, Object> source : video_sources) {
                        final String qualityStr = (String) source.get("quality");
                        String url = (String) source.get("url");
                        if (StringUtils.containsIgnoreCase(url, ".m3u8")) {
                            /* HLS */
                            if (hlsMap == null) {
                                hlsMap = source;
                            }
                        } else {
                            /* Progressive */
                            final int qualityHeight = Integer.parseInt(qualityStr.replaceFirst("(?i)p", ""));
                            availableQualities.add(qualityHeight);
                            String fallback = (String) source.get("fallback");
                            /* We found the quality we were looking for. */
                            url = br.getURL(url).toExternalForm();
                            fallback = !StringUtils.isEmpty(fallback) ? br.getURL(fallback).toExternalForm() : null;
                            if (bestProgressiveQualityDownloadurl == null || qualityHeight > maxQualityHeight) {
                                bestProgressiveQualityDownloadurl = url;
                                maxQualityHeight = qualityHeight;
                            }
                            if (qualityHeight != selectedQualityHeight) {
                                continue;
                            }
                            if (verifyURL(url)) {
                                logger.info("Found preferred quality(url):" + qualityStr + "->" + url);
                                chosenQualityHeight = selectedQualityHeight;
                                chosenQualityDownloadurl = url;
                                break;
                            } else if (fallback != null && verifyURL(fallback)) {
                                logger.info("Found preferred quality(fallback):" + qualityStr + "->" + fallback);
                                chosenQualityHeight = selectedQualityHeight;
                                chosenQualityDownloadurl = fallback;
                                break;
                            } else {
                                logger.info("Sources(failed):" + qualityStr);
                                continue;
                            }
                        }
                    }
                    if (selectedQualityHeight != -1) {
                        logger.info("Did not find preferred quality(sources):" + selectedQualityHeight + "|available:" + availableQualities);
                    }
                } else {
                    logger.warning("Could not find any video sources in json");
                }
            }
            if (chosenQualityDownloadurl == null && bestProgressiveQualityDownloadurl != null) {
                if (selectedQualityHeight == -1) {
                    logger.info("Returning best progressive quality: " + maxQualityHeight + "p: " + bestProgressiveQualityDownloadurl);
                    chosenQualityHeight = maxQualityHeight;
                    chosenQualityDownloadurl = bestProgressiveQualityDownloadurl;
                } else {
                    final String ret = getDllink(br, selected_format + 1);
                    if (ret != null) {
                        return ret;
                    }
                }
            }
            if (chosenQualityDownloadurl != null) {
                logger.info("Returning progressive quality " + chosenQualityHeight + "p");
                final Number filesize = videoHeightToFilesize.get(chosenQualityHeight);
                if (filesize != null) {
                    logger.info("Setting filesize obtained from list of download filesizes -> " + filesize);
                    this.getDownloadLink().setDownloadSize(filesize.longValue());
                }
                return chosenQualityDownloadurl;
            }
        } catch (final JSonMapperException e) {
            logger.log(e);
        }
        if (hlsMap != null) {
            /* 2021-02-01 */
            logger.info("Try fallback to HLS download -> " + hlsMap);
            return (String) hlsMap.get("url");
        }
        final String newPlayer = Encoding.htmlDecode(br.getRegex("videoUrls\":\"(\\{.*?\\]\\})").getMatch(0));
        if (newPlayer != null) {
            // new player
            final Map<String, Object> map = restoreFromString(restoreFromString("\"" + newPlayer + "\"", TypeRef.STRING), TypeRef.MAP);
            if (map != null) {
                for (final String quality : qualities) {
                    final Object list = map.get(quality);
                    if (list != null && list instanceof List) {
                        final List<String> urls = (List<String>) list;
                        if (urls.size() > 0) {
                            vq = quality;
                            logger.info("videoUrls:" + quality + "->" + quality);
                            return urls.get(0);
                        }
                    }
                }
            }
        }
        for (final String quality : qualities) {
            // old player
            final String urls[] = br.getRegex(quality + "\"\\s*:\\s*(\"https?:[^\"]+\")").getColumn(0);
            if (urls != null && urls.length > 0) {
                for (String url : urls) {
                    url = restoreFromString(url, TypeRef.STRING);
                    if (StringUtils.containsIgnoreCase(url, ".mp4")) {
                        final boolean verified = verifyURL(url);
                        if (verified) {
                            vq = quality;
                            logger.info("oldPlayer:" + quality + "->" + quality);
                            return url;
                        }
                    }
                }
            }
        }
        for (final String quality : qualities) {
            // 3d videos
            final String urls[] = br.getRegex(quality + "\"\\s*,\\s*\"url\"\\s*:\\s*(\"https?:[^\"]+\")").getColumn(0);
            if (urls != null && urls.length > 0) {
                String best = null;
                for (String url : urls) {
                    url = restoreFromString(url, TypeRef.STRING);
                    if (best == null || StringUtils.containsIgnoreCase(url, ".mp4")) {
                        best = url;
                    }
                }
                if (best != null) {
                    vq = quality;
                    logger.info("old3D" + quality + "->" + quality);
                    return best;
                }
            }
        }
        // is the rest still in use/required?
        String ret = null;
        logger.info("Video quality selection failed.");
        int urlmodeint = 0;
        final String urlmode = br.getRegex("url_mode=(\\d+)").getMatch(0);
        if (urlmode != null) {
            urlmodeint = Integer.parseInt(urlmode);
        }
        if (urlmodeint == 1) {
            /* Example-ID: 1815274, 1980180 */
            final Regex secondway = br.getRegex("\\&srv=(https?[A-Za-z0-9%\\.]+\\.xhcdn\\.com)\\&file=([^<>\"]*?)\\&");
            String server = br.getRegex("\\'srv\\'\\s*:\\s*\\'(.*?)\\'").getMatch(0);
            if (server == null) {
                server = secondway.getMatch(0);
            }
            String file = br.getRegex("\\'file\\'\\s*:\\s*\\'(.*?)\\'").getMatch(0);
            if (file == null) {
                file = secondway.getMatch(1);
            }
            if (server == null || file == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            if (file.startsWith("http")) {
                // Examplelink (ID): 968106
                ret = file;
            } else {
                // Examplelink (ID): 986043
                ret = server + "/key=" + file;
            }
            logger.info("urlmode:" + urlmodeint + "->" + ret);
        } else {
            /* E.g. url_mode == 3 */
            /* Example-ID: 685813 */
            String flashvars = br.getRegex("flashvars\\s*:\\s*\"([^<>\"]*?)\"").getMatch(0);
            ret = br.getRegex("\"(https?://\\d+\\.xhcdn\\.com/key=[^<>\"]*?)\" class=\"mp4Thumb\"").getMatch(0);
            if (ret == null) {
                ret = br.getRegex("\"(https?://\\d+\\.xhcdn\\.com/key=[^<>\"]*?)\"").getMatch(0);
            }
            if (ret == null) {
                ret = br.getRegex("\"(https?://\\d+\\.xhcdn\\.com/key=[^<>\"]*?)\"").getMatch(0);
            }
            if (ret == null) {
                ret = br.getRegex("flashvars.*?file=(https?%3.*?)&").getMatch(0);
            }
            if (ret == null && flashvars != null) {
                /* E.g. 4753816 */
                flashvars = Encoding.htmlDecode(flashvars);
                flashvars = flashvars.replace("\\", "");
                final String[] qualities2 = { "1080p", "720p", "480p", "360p", "240p" };
                for (final String quality : qualities2) {
                    ret = new Regex(flashvars, "\"" + quality + "\"\\s*:\\s*\\[\"(https?[^<>\"]*?)\"\\]").getMatch(0);
                    if (ret != null) {
                        logger.info("urlmode:" + urlmodeint + "|quality:" + quality + "->" + ret);
                        break;
                    }
                }
            }
        }
        if (ret == null) {
            // urlmode fails, eg: 1099006
            ret = br.getRegex("video\\s*:\\s*\\{[^\\}]+file\\s*:\\s*('|\")(.*?)\\1").getMatch(1);
            if (ret == null) {
                ret = PluginJSonUtils.getJson(br, "fallback");
                if (!StringUtils.isEmpty(ret)) {
                    ret = ret.replace("\\", "");
                    logger.info("urlmode(fallback):" + urlmodeint + "->" + ret);
                }
            }
        }
        if (ret != null) {
            if (ret.contains("&amp;")) {
                ret = Encoding.htmlDecode(ret);
            }
            return ret;
        } else {
            return null;
        }
    }

    public boolean verifyURL(String url) throws IOException, PluginException {
        URLConnectionAdapter con = null;
        final Browser br2 = br.cloneBrowser();
        try {
            con = br2.openHeadConnection(url);
            if (!looksLikeDownloadableContent(con)) {
                br2.followConnection(true);
                throw new IOException();
            }
            return true;
        } catch (final IOException e) {
            logger.log(e);
            return false;
        } finally {
            try {
                con.disconnect();
            } catch (final Exception e) {
            }
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownload(link, null);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload(link, account);
    }

    @SuppressWarnings("deprecation")
    public void handleDownload(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link, account);
        final String contentURL = getCorrectedURL(link.getPluginPatternMatcher());
        final boolean isPremiumURL = this.isPremiumURL(contentURL);
        if (StringUtils.isEmpty(dllink) && !isPremiumURL) {
            // Access the page again to get a new direct link because by checking the availability the first linkisn't valid anymore
            if (isPasswordProtected(br)) {
                final boolean passwordHandlingBroken = true;
                if (passwordHandlingBroken) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Password-protected handling broken svn.jdownloader.org/issues/88690");
                }
                String passCode = link.getDownloadPassword();
                if (passCode == null) {
                    passCode = getUserInput("Password?", link);
                }
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    /* New way */
                    final String videoID = getFID(link);
                    if (videoID == null) {
                        /* This should never happen */
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    final Browser brc = br.cloneBrowser();
                    /* 2020-09-03: Browser sends crypted password but uncrypted password seems to work fine too */
                    final String json = String.format("[{\"name\":\"entityUnlockModelSync\",\"requestData\":{\"model\":{\"id\":null,\"$id\":\"c280e6b4-d696-479c-bb7d-eb0627d36fb1\",\"modelName\":\"entityUnlockModel\",\"itemState\":\"changed\",\"password\":\"%s\",\"entityModel\":\"videoModel\",\"entityID\":%s}}}]", passCode, videoID);
                    brc.getHeaders().put("x-requested-with", "XMLHttpRequest");
                    brc.getHeaders().put("content-type", "text/plain");
                    brc.getHeaders().put("accept", "*/*");
                    brc.postPageRaw("/x-api", json);
                    /*
                     * 2020-09-03: E.g. wrong password:
                     * [{"name":"entityUnlockModelSync","extras":{"result":false,"error":{"password":"Falsches Passwort"}},"responseData":{
                     * "$id":"c280e6b4-d696-479c-bb7d-eb0627d36fb1"}}]
                     */
                    if (brc.containsHTML("\"password\"")) {
                        link.setDownloadPassword(null);
                        throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
                    }
                    link.setDownloadPassword(passCode);
                    /*
                     * 2020-09-03: WTF:
                     * [{"name":"entityUnlockModelSync","extras":{"result":false,"showCaptcha":true,"code":"403 Forbidden"},"responseData":{
                     * "$id":"c280e6b4-d696-479c-bb7d-eb0627d36fb1"}}]
                     */
                } else {
                    /* Old way */
                    br.postPage(br.getURL(), "password=" + Encoding.urlEncode(passCode));
                    if (isPasswordProtected(br)) {
                        link.setDownloadPassword(null);
                        throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
                    }
                    link.setDownloadPassword(passCode);
                }
            } else {
                dllink = getDllink(br);
            }
        }
        if (StringUtils.isEmpty(dllink)) {
            final String onlyforFriendsWithThisName = isVideoOnlyForFriendsOf(br);
            if (onlyforFriendsWithThisName != null) {
                throw new AccountRequiredException("You need to be friends with " + onlyforFriendsWithThisName + " to access this contents");
            } else if (this.isVideoOnlyForFriends(br)) {
                throw new AccountRequiredException("You need to be friends with the uploader to access this content");
            } else if (isPaidContent(br)) {
                throw new AccountRequiredException("Paid content");
            } else if (isPremiumURL) {
                throw new AccountRequiredException("Paid content & trailer download failed");
            } else if (br.containsHTML("\"ageVerificationNeeded\"\\s*:\\s*true")) {
                throw new AccountRequiredException("Age verification needed");
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        if (StringUtils.containsIgnoreCase(dllink, ".m3u8")) {
            /* 2021-02-01: HLS download */
            br.getPage(this.dllink);
            final String m3u8URL;
            final List<HlsContainer> hlsContainers = HlsContainer.getHlsQualities(this.br);
            if (hlsContainers.size() == 0) {
                final List<M3U8Playlist> m3u8 = M3U8Playlist.parseM3U8(this.br);
                if (m3u8.size() == 0) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                m3u8URL = dllink;
            } else {
                HlsContainer chosenQuality = null;
                final int preferredVideoQualityHeight = getPreferredQualityHeight();
                for (final HlsContainer container : hlsContainers) {
                    if (container.getHeight() == preferredVideoQualityHeight) {
                        logger.info("Found preferred quality: " + preferredVideoQualityHeight);
                        chosenQuality = container;
                        break;
                    }
                }
                if (chosenQuality == null) {
                    /* Best quality */
                    chosenQuality = HlsContainer.findBestVideoByBandwidth(hlsContainers);
                }
                m3u8URL = chosenQuality.getStreamURL();
            }
            if (m3u8URL == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, m3u8URL);
            dl.startDownload();
        } else {
            boolean resume = true;
            if (link.getBooleanProperty(NORESUME, false)) {
                resume = false;
            }
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, this.dllink, resume, 0);
            if (!looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getResponseCode() == 416) {
                    logger.info("Response code 416 --> Handling it");
                    if (link.getBooleanProperty(NORESUME, false)) {
                        link.setProperty(NORESUME, Boolean.valueOf(false));
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 416", 30 * 60 * 1000l);
                    }
                    link.setProperty(NORESUME, Boolean.valueOf(true));
                    link.setChunksProgress(null);
                    throw new PluginException(LinkStatus.ERROR_RETRY, "Server error 416");
                } else {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown error");
                }
            }
            dl.startDownload();
        }
    }

    public void login(final Account account, final String checkURL, final boolean force) throws Exception {
        synchronized (account) {
            // used in finally to restore browser redirect status.
            br.setCookiesExclusive(true);
            /**
             * 2020-01-31: They got their free page xhamster.com and paid faphouse.com. <br>
             * This plugin will always try to login into both. <br>
             * Free users can also login via their premium page but they just cannot watch anything or only trailers. <br>
             * 2025-06-13: Users can now also create sole faphouse.com accounts whose login credentials will not work for xhamster.com! A
             * user can also have a sole faphouse.com account with working login credentials but no premium subscription.
             */
            boolean forceLogincheckFree = force;
            boolean forceLogincheckPremium = force;
            if (checkURL != null && force) {
                /* Custom check-URL given -> Only check free or premium login depending on the link -> Speeds things up. */
                if (this.isPremiumURL(checkURL)) {
                    forceLogincheckFree = false;
                    // forceLogincheckPremium = true;
                } else {
                    // forceLogincheckFree = true;
                    forceLogincheckPremium = false;
                }
            }
            boolean freeLoginSuccess = false;
            if (this.isPremiumOnlyAccount(account)) {
                logger.info("Skipping free login because this account is known to be premium only (= " + domain_premium + " only)");
                freeLoginSuccess = false;
            } else {
                try {
                    this.loginFree(br, account, checkURL, forceLogincheckFree);
                    freeLoginSuccess = true;
                } catch (final AccountInvalidException aie) {
                }
            }
            this.loginPremium(br, account, checkURL, forceLogincheckPremium, freeLoginSuccess);
            if (!freeLoginSuccess) {
                account.setProperty(PROPERTY_ACCOUNT_TIMESTAMP_LAST_TIME_PREMIUM_ONLY_ACCOUNT, System.currentTimeMillis());
            } else {
                account.removeProperty(PROPERTY_ACCOUNT_TIMESTAMP_LAST_TIME_PREMIUM_ONLY_ACCOUNT);
            }
            /* Fallback: Double-check to make sure that our target-URL has been accessed. */
            if (checkURL != null) {
                final URL customCheckURL = new URL(checkURL);
                if (br.getURL() == null || !br.getURL().endsWith(customCheckURL.getPath())) {
                    br.getPage(checkURL);
                }
            }
        }
    }

    private void loginFree(final Browser br, final Account account, final String customCheckURL, final boolean validateCookies) throws IOException, PluginException, InterruptedException {
        String freeDomain = account.getStringProperty(PROPERTY_ACCOUNT_LAST_USED_FREE_DOMAIN);
        if (freeDomain == null) {
            logger.info("Determining current free domain");
            if (customCheckURL != null && !this.isPremiumURL(customCheckURL)) {
                br.getPage(customCheckURL);
            } else {
                br.getPage("https://" + this.getHost() + "/");
            }
            freeDomain = br.getHost();
            logger.info("Current free domain is: " + freeDomain);
            account.setProperty(PROPERTY_ACCOUNT_LAST_USED_FREE_DOMAIN, freeDomain);
        }
        final Cookies cookies = account.loadCookies("");
        if (cookies != null) {
            logger.info("Trying free cookie login");
            br.setCookies(freeDomain, cookies, true);
            if (!validateCookies) {
                /* Do not check cookies */
                return;
            }
            if (checkLoginFree(br, account, customCheckURL)) {
                logger.info("Free cookie login successful");
                /* Save new cookie timestamp */
                account.saveCookies(br.getCookies(br.getHost()), "");
                account.setProperty(PROPERTY_ACCOUNT_LAST_USED_FREE_DOMAIN, br.getHost());
                return;
            } else {
                /* Try full login */
                logger.info("Free cookie login failed");
                br.clearCookies(null);
                prepBr(this, br);
            }
        }
        logger.info("Performing full login");
        /* Only access URL if it hasn't been accessed before */
        br.getPage("https://" + freeDomain + "/");
        final String urlBeforeLogin = br.getURL();
        String siteKeyV3 = PluginJSonUtils.getJson(br, "recaptchaKeyV3");
        if (StringUtils.isEmpty(siteKeyV3)) {
            /* 2023-09-11 */
            siteKeyV3 = PluginJSonUtils.getJson(br, "recaptchaEnterpriseKeyV3");
        }
        final String siteKey = PluginJSonUtils.getJson(br, "recaptchaKey");
        final String id = createID();
        final String requestdataFormat = "[{\"name\":\"authorizedUserModelSync\",\"requestData\":{\"model\":{\"id\":null,\"$id\":\"%s\",\"modelName\":\"authorizedUserModel\",\"itemState\":\"unchanged\"},\"trusted\":true,\"username\":\"%s\",\"password\":\"%s\",\"remember\":1,\"redirectURL\":null,\"captcha\":\"\",\"g-recaptcha-response\":\"%s\"}}]";
        final String requestdataFormatCaptcha = "[{\"name\":\"authorizedUserModelSync\",\"requestData\":{\"model\":{\"id\":null,\"$id\":\"%s\",\"modelName\":\"authorizedUserModel\",\"itemState\":\"unchanged\"},\"username\":\"%s\",\"password\":\"%s\",\"remember\":1,\"redirectURL\":null,\"captcha\":\"\",\"trusted\":true,\"g-recaptcha-response\":\"%s\"}}]";
        String requestData = String.format(requestdataFormat, id, account.getUser(), account.getPass(), "");
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        brc.postPageRaw("/x-api", requestData);
        if (brc.containsHTML("showCaptcha\":true")) {
            logger.info("Captcha required");
            final String recaptchaV2Response;
            if (!StringUtils.isEmpty(siteKeyV3)) {
                /* 2020-03-17 */
                recaptchaV2Response = getCaptchaHelperHostPluginRecaptchaV2Invisible(this, brc, siteKeyV3).getToken();
            } else {
                /* Old */
                recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, brc, siteKey).getToken();
            }
            requestData = String.format(requestdataFormatCaptcha, id, account.getUser(), account.getPass(), recaptchaV2Response);
            /* TODO: Fix this */
            brc.postPageRaw("/x-api", requestData);
        }
        /* First login or not a premium account? One more step required! */
        if (this.getMagicPremiumLoginURL(account) == null) {
            if (!this.checkLoginFree(br, account, urlBeforeLogin)) {
                /* This should never happen! */
                throw new AccountInvalidException();
            }
        }
        account.saveCookies(brc.getCookies(br.getHost()), "");
    }

    private boolean checkLoginFree(final Browser br, final Account account, final String customCheckURL) throws IOException {
        String freeDomain = account.getStringProperty(PROPERTY_ACCOUNT_LAST_USED_FREE_DOMAIN);
        if (freeDomain == null) {
            freeDomain = br.getHost();
        }
        if (customCheckURL == null || isPremiumURL(customCheckURL)) {
            br.getPage("https://" + freeDomain + "/");
        } else {
            final String customCheckURLWithCorrectDomain = customCheckURL.replaceFirst(Browser.getHost(customCheckURL, true), freeDomain);
            br.getPage(customCheckURLWithCorrectDomain);
        }
        account.setProperty(PROPERTY_ACCOUNT_LAST_USED_FREE_DOMAIN, br.getHost());
        if (!isLoggedInHTMLFree(br)) {
            logger.info("Free cookie login failed");
            return false;
        }
        logger.info("Free cookie login successful");
        /* Save new cookie timestamp */
        account.saveCookies(br.getCookies(br.getHost()), "");
        String premiumLoginLink = br.getRegex("\"(https?://[^/]+/faphouse/out\\?xhMedium=[^\"]+)\"").getMatch(0);
        if (premiumLoginLink == null) {
            /* 2024-03-25 */
            premiumLoginLink = br.getRegex("\"(https?://[^/]+/fh/out\\?url=[^\"]+)\"").getMatch(0);
        }
        if (premiumLoginLink != null) {
            /* Premium login is possible. This does not mean that this is a premium account!! */
            account.setProperty(PROPERTY_ACCOUNT_PREMIUM_LOGIN_URL, premiumLoginLink);
        } else {
            account.removeProperty(PROPERTY_ACCOUNT_PREMIUM_LOGIN_URL);
            account.clearCookies(COOKIE_KEY_PREMIUM);
        }
        return true;
    }

    private boolean isLoggedInHTMLFree(final Browser br) {
        return br.containsHTML("class\\s*=\\s*\"profile-link-info-name\"");
    }

    /** Use this for faphouse.com */
    private boolean isLoggedinHTMLPremium(final Browser br, final Account account) {
        final String subscriptionStatus = PluginJSonUtils.getJson(br, "userHasSubscription");
        final String currentUserId = PluginJSonUtils.getJson(br, "currentUserId");
        if (subscriptionStatus == null || subscriptionStatus.equals("null") && (currentUserId == null || currentUserId.equals("null"))) {
            /* We are not logged in */
            return false;
        }
        /* We are logged in -> Determine premium status. Return false if this is not a premium account. */
        /**
         * Developer: It is crucial to set the account type here in order for the handling afterwards to work correct. <br>
         * DO NOT TOUCH THIS!
         */
        if ("true".equals(subscriptionStatus)) {
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
        }
        return true;
    }

    private void loginPremium(final Browser br, final Account account, final String customCheckURL, final boolean validateCookies, final boolean xhamsterFreeLoginSuccess) throws IOException, PluginException, InterruptedException {
        final Cookies premiumCookies = account.loadCookies(COOKIE_KEY_PREMIUM);
        if (premiumCookies != null) {
            br.setCookies(domain_premium, premiumCookies);
            if (!validateCookies) {
                return;
            }
            if (this.checkLoginPremium(br, account, customCheckURL)) {
                logger.info("Premium cookie login successful");
                account.saveCookies(br.getCookies(domain_premium), COOKIE_KEY_PREMIUM);
                return;
            } else {
                logger.info("Premium cookie login failed");
                br.clearCookies(domain_premium);
                prepBr(this, br);
            }
        }
        final String magicPremiumLoginURL = getMagicPremiumLoginURL(account);
        if (magicPremiumLoginURL != null) {
            /* Magic link from xhamster.com which will redirect to faphouse.com and should grant us premium login cookies. */
            logger.info("Attempting premium login via magic link");
            br.getPage(magicPremiumLoginURL);
            final String redirecturl = br.getRegex("http-equiv=\"refresh\" content=\"\\d+; url=(https?://[^\"]+)").getMatch(0);
            if (redirecturl != null) {
                br.getPage(redirecturl);
            } else {
                logger.warning("Failed to find redirect to premium domain -> Possible login failure");
            }
            if (isLoggedinHTMLPremium(br, account)) {
                logger.info("Premium login via magic link was successful");
                account.saveCookies(br.getCookies(domain_premium), COOKIE_KEY_PREMIUM);
                return;
            }
            logger.info("Simple login via magic link failed --> Attempting advanced magic link login");
            extendedMagicLinkLogin: if (true) {
                final String callback_url_urlencoded = br.getRegex("callback_url=(http[^\"]+)").getMatch(0);
                if (callback_url_urlencoded == null) {
                    logger.warning("Magic link login: Failed to find callback_url");
                    break extendedMagicLinkLogin;
                }
                final String callback_url_urldecoded = Encoding.htmlDecode(callback_url_urlencoded);
                final String callback_url_raw = UrlQuery.parse(callback_url_urldecoded).get("url");
                if (callback_url_raw == null) {
                    logger.warning("Magic link login: Failed to find callback_url_raw");
                    break extendedMagicLinkLogin;
                }
                final String callback_url = callback_url_raw + "&authType=2";
                br.postPageRaw("/api/auth/xh-one-tap-frame", "{\"isSubscribedToUpdates\":0,\"callbackUrl\":\"https://" + domain_premium + "/auth/one-tap?url=" + callback_url + "\"}");
                final Map<String, Object> entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final String status = (String) entries.get("status");
                if (!StringUtils.equalsIgnoreCase(status, "success")) {
                    logger.warning("Magic link login: Failed to login");
                    break extendedMagicLinkLogin;
                }
                final Map<String, Object> data = (Map<String, Object>) entries.get("data");
                final String redirectUrl = data.get("redirectUrl").toString();
                br.getPage(redirectUrl);
                if (isLoggedinHTMLPremium(br, account)) {
                    logger.info("Extended premium login via magic link was successful");
                    account.saveCookies(br.getCookies(domain_premium), COOKIE_KEY_PREMIUM);
                    return;
                }
            }
        }
        logger.info("Performing full login");
        br.clearCookies(br.getHost());
        prepBr(this, br);
        logger.info("Performing full premium login");
        String recaptchaV2Response = "";
        br.getPage("https://" + domain_premium + "/");
        final Browser brc = br.cloneBrowser();
        /* 2025-06-13: No login captcha required */
        final boolean askForCaptcha = false;
        if (askForCaptcha) {
            String rcKey = br.getRegex("data-site-key=\"([^\"]+)\"").getMatch(0);
            if (rcKey == null) {
                /* Fallback: reCaptchaKey timestamp: 2020-08-04 */
                rcKey = "6LfoawAVAAAAADDXDc7xDBOkr1FQqdfUrEH5Z7up";
                logger.info("Using fallback value for reCaptchaKey: " + rcKey);
            }
            recaptchaV2Response = getCaptchaHelperHostPluginRecaptchaV2Invisible(this, brc, rcKey).getToken();
        }
        final String csrftoken = br.getRegex("data-name=\"csrf-token\" content=\"([^<>\"]+)\"").getMatch(0);
        if (csrftoken != null) {
            brc.getHeaders().put("x-csrf-token", csrftoken);
        } else {
            logger.warning("Failed to find csrftoken --> Premium login might fail because of this");
        }
        /*
         * trackingParamsBag: Use default value which when base64Decoded resolves to:
         * {"promo_id":"","video_id":null,"studio_id":null,"producer_id":null,"orientation":"straight","ml_page":"main_page",
         * "ml_page_value_id":null,"ml_page_value":null,"ml_page_number":null}
         */
        brc.postPageRaw("/api/auth/signin", String.format("{\"login\":\"%s\",\"password\":\"%s\",\"rememberMe\":\"1\",\"recaptcha\":\"%s\",\"trackingParamsBag\":\"eyJwcm9tb19pZCI6IiIsInZpZGVvX2lkIjpudWxsLCJzdHVkaW9faWQiOm51bGwsInByb2R1Y2VyX2lkIjpudWxsLCJvcmllbnRhdGlvbiI6InN0cmFpZ2h0IiwibWxfcGFnZSI6Im1haW5fcGFnZSIsIm1sX3BhZ2VfdmFsdWVfaWQiOm51bGwsIm1sX3BhZ2VfdmFsdWUiOm51bGwsIm1sX3BhZ2VfbnVtYmVyIjpudWxsfQ==\"}", PluginJSonUtils.escape(account.getUser()), PluginJSonUtils.escape(account.getPass()), recaptchaV2Response));
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        /* e.g. error response: { "errors": { "_global": [ "Invalid credentials" ] }, "userId": null, "hasGoldSubscription": false} */
        final Object errorsO = entries.get("errors");
        if (errorsO != null) {
            throw new AccountInvalidException();
        }
        if (!Boolean.TRUE.equals(entries.get("success"))) {
            throw new AccountInvalidException();
        }
        logger.info("Login successful");
        account.saveCookies(brc.getCookies(domain_premium), COOKIE_KEY_PREMIUM);
        if (Boolean.TRUE.equals(entries.get("hasGoldSubscription"))) {
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
            if (!xhamsterFreeLoginSuccess) {
                throw new AccountInvalidException("Account cannot be used for xhamster.com login and at the same time is not a " + domain_premium + " premium account which means there is no point in using it with JDownloader.");
            }
        }
    }

    /** Typically returns https://xy.xhamster.com/faphouse/out?xhMedium=button */
    private String getMagicPremiumLoginURL(final Account account) {
        return account.getStringProperty(PROPERTY_ACCOUNT_PREMIUM_LOGIN_URL);
    }

    /** Checks premium login status and sets AccountInfo */
    private boolean checkLoginPremium(final Browser br, final Account account, String checkURL) throws IOException {
        if (checkURL == null || !this.isPremiumURL(checkURL)) {
            checkURL = "https://" + domain_premium + "/en";
        }
        /* Check via html */
        br.getPage(checkURL);
        return this.isLoggedinHTMLPremium(br, account);
    }

    protected CaptchaHelperHostPluginRecaptchaV2 getCaptchaHelperHostPluginRecaptchaV2Invisible(PluginForHost plugin, Browser br, final String key) throws PluginException {
        return new CaptchaHelperHostPluginRecaptchaV2(this, br, key) {
            @Override
            public org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2.TYPE getType() {
                return TYPE.INVISIBLE;
            }
        };
    }

    private String createID() {
        StringBuffer result = new StringBuffer();
        byte bytes[] = new byte[16];
        SecureRandom random = new SecureRandom();
        random.nextBytes(bytes);
        if (bytes[6] == 15) {
            bytes[6] |= 64;
        }
        if (bytes[8] == 63) {
            bytes[8] |= 128;
        }
        for (int i = 0; i < bytes.length; i++) {
            result.append(String.format("%02x", bytes[i] & 0xFF));
            if (i == 3 || i == 5 || i == 7 || i == 9) {
                result.append("-");
            }
        }
        return result.toString();
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, null, true);
        final AccountInfo ai = new AccountInfo();
        if (AccountType.PREMIUM.equals(account.getType()) || AccountType.LIFETIME.equals(account.getType())) {
            /* Check vja ajax request -> json */
            logger.info("Fetching detailed premium account information");
            br.getPage(api_base_premium + "/subscription/get");
            /**
             * Returns "null" if cookies are valid but this is not a premium account. </br> Redirects to mainpage if cookies are invalid.
             * </br> Return json if cookies are valid. </br> Can also return json along with http responsecode 400 for valid cookies but
             * user is non-premium.
             */
            ai.setUnlimitedTraffic();
            /* Premium domain cookies are valid and we can expect json */
            /*
             * E.g. error 400 for free users:
             * {"errors":{"_global":["Payment system temporary unavailable. Please try later."]},"userId":1234567}
             */
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            long expireTimestamp = 0;
            final String expireStr = (String) entries.get("expiredAt");
            final Boolean isLifetime = (Boolean) entries.get("isLifetime");
            final Boolean isTrial = (Boolean) entries.get("isTrial");
            final Boolean hasGoldSubscription = (Boolean) entries.get("hasGoldSubscription");
            final Boolean isRebillEnabled = (Boolean) entries.get("isRebillEnabled");
            if (!StringUtils.isEmpty(expireStr)) {
                if (expireStr.matches("^\\d{1,2} [A-Za-z]+ \\d{4}$")) {
                    /* 2024-03-25 */
                    expireTimestamp = TimeFormatter.getMilliSeconds(expireStr, "dd MMM yyyy", Locale.ENGLISH);
                } else {
                    expireTimestamp = TimeFormatter.getMilliSeconds(expireStr, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
                }
            }
            if (Boolean.TRUE.equals(entries.get("isLifetime")) || Boolean.TRUE.equals(hasGoldSubscription) || Boolean.TRUE.equals(isTrial) || expireTimestamp > System.currentTimeMillis()) {
                if (isLifetime) {
                    account.setType(AccountType.LIFETIME);
                } else {
                    account.setType(AccountType.PREMIUM);
                }
                String accountStatusText;
                if (Boolean.TRUE.equals(isTrial)) {
                    /* Trial account */
                    accountStatusText = "Trial Account";
                } else {
                    accountStatusText = account.getType().getLabel();
                }
                if (Boolean.TRUE.equals(isRebillEnabled)) {
                    accountStatusText += " | Rebill: Yes";
                } else {
                    accountStatusText += " | Rebill: No";
                }
                if (expireTimestamp > System.currentTimeMillis()) {
                    ai.setValidUntil(expireTimestamp, br);
                }
                br.getPage(api_base_premium + "/user/download-limits");
                final Map<String, Object> limitresponse = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                /* E.g. {"isLimitReached":false,"monthlyDownloads":{"limit":300,"available":299}} */
                final Map<String, Object> monthlyDownloads = (Map<String, Object>) limitresponse.get("monthlyDownloads");
                accountStatusText += " | Monthly DLs left: " + monthlyDownloads.get("available") + "/" + monthlyDownloads.get("limit");
                ai.setStatus(accountStatusText);
            } else {
                /* Expired premium or free account */
                account.setType(AccountType.FREE);
            }
        } else {
            account.setType(AccountType.FREE);
        }
        return ai;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }

    @Override
    public void resetPluginGlobals() {
    }
}