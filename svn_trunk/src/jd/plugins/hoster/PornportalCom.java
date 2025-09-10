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
package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.config.PornportalComConfig;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.host.PluginFinder;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.linkcrawler.LinkCrawlerDeepInspector;
import jd.http.Browser;
import jd.http.Cookie;
import jd.http.Cookies;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.http.requests.PostRequest;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.SiteType.SiteTemplate;
import jd.plugins.decrypter.PornportalComCrawler;

@HostPlugin(revision = "$Revision: 51468 $", interfaceVersion = 2, names = {}, urls = {})
public class PornportalCom extends PluginForHost {
    public PornportalCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost());
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(new int[] { 400 });
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX, LazyPlugin.FEATURE.IMAGE_GALLERY, LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/legal/tos";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "pornportal.com" });
        /* 2024-06-26: babes.com is now spicevids.com */
        ret.add(new String[] { "spicevids.com", "babes.com", "blackisbetter.com" });
        ret.add(new String[] { "biempire.com" });
        ret.add(new String[] { "brazzers.com", "brazzer.com" });
        ret.add(new String[] { "digitalplayground.com" });
        ret.add(new String[] { "erito.com", "eritos.com" });
        ret.add(new String[] { "fakehub.com", "femalefaketaxi.com", "fakedrivingschool.com", "fakehostel.com" });
        ret.add(new String[] { "hentaipros.com" });
        ret.add(new String[] { "lilhumpers.com" });
        ret.add(new String[] { "milehighmedia.com", "sweetheartvideo.com", "realityjunkies.com" });
        ret.add(new String[] { "metrohd.com", "familyhookups.com", "kinkyspa.com" });
        ret.add(new String[] { "mofos.com", "publicpickups.com", "iknowthatgirl.com", "dontbreakme.com" });
        ret.add(new String[] { "propertysex.com" });
        ret.add(new String[] { "realitykings.com", "gfleaks.com", "inthevip.com", "mikesapartment.com", "8thstreetlatinas.com", "bignaturals.com", "cumfiesta.com", "happytugs.com", "milfhunter.com", "momsbangteens.com", "momslickteens.com", "moneytalks.com", "roundandbrown.com", "sneakysex.com", "teenslovehugecocks.com", "welivetogether.com", "blackgfs.com", "daredorm.com" });
        ret.add(new String[] { "sexyhub.com", "fitnessrooms.com" });
        ret.add(new String[] { "spankwirepremium.com" });
        ret.add(new String[] { "squirted.com" });
        ret.add(new String[] { "transangels.com" });
        ret.add(new String[] { "transsensual.com" });
        ret.add(new String[] { "trueamateurs.com" });
        ret.add(new String[] { "twistys.com", "teenpinkvideos.com" });
        ret.add(new String[] { "whynotbi.com" });
        ret.add(new String[] { "bangbros.com", "bangbrothers.com", "bangbrothers.net" });
        ret.add(new String[] { "seancody.com" });
        ret.add(new String[] { "dancingbear.com" });
        ret.add(new String[] { "adultmobile.com" });
        ret.add(new String[] { "mygf.com" });
        ret.add(new String[] { "letsdoeit.com" });
        return ret;
    }

    /** Returns content of getPluginDomains as single dimensional Array. */
    public static ArrayList<String> getAllSupportedPluginDomainsFlat() {
        ArrayList<String> allDomains = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            for (final String singleDomain : domains) {
                allDomains.add(singleDomain);
            }
        }
        return allDomains;
    }

    /** Contains domains which will be ignored in list of supported hosts in account view / "multihoster support". */
    public static ArrayList<String> getAllBlacklistedDomains() {
        final ArrayList<String> allDomains = new ArrayList<String>();
        allDomains.add("pornportal.com");
        return allDomains;
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
            /* No regex needed - all items are added via crawler plugin. */
            ret.add("");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String videoid = link.getStringProperty(PROPERTY_VIDEO_ID);
        final String videoquality = link.getStringProperty(PROPERTY_VIDEO_QUALITY);
        final String videoStreamType = link.getStringProperty(PROPERTY_VIDEO_STREAM_TYPE);
        final String gallery_type = this.getGalleryType(link);
        final String galleryid = link.getStringProperty(PROPERTY_GALLERY_ID);
        final int galleryImagePosition = link.getIntegerProperty(PROPERTY_GALLERY_IMAGE_POSITION, -1);
        final int galleryPosition = link.getIntegerProperty(PROPERTY_GALLERY_POSITION, -1);
        if (videoid != null && videoquality != null) {
            return this.getHost() + "://video" + videoid + "/" + videoquality + "/" + videoStreamType;
        } else if (galleryid != null && galleryPosition != -1 && galleryImagePosition != -1) {
            return this.getHost() + "://photo" + gallery_type + galleryid + "/" + galleryPosition + "/" + galleryImagePosition;
        } else {
            return super.getLinkID(link);
        }
    }

    /*
     * Debug function: Can be used to quickly find the currently used pornportal version of all supported websites and compare against
     * previously set expected version value.
     */
    public static void checkUsedVersions(final Plugin plg) {
        final String target_version = "4.35.2";
        plg.getLogger().info("Target version: " + target_version);
        final Browser br = plg.createNewBrowserInstance();
        final String[] supportedSites = getAnnotationNames();
        for (final String host : supportedSites) {
            try {
                getPage(br, getPornportalMainURL(host));
                final String usedVersion = PluginJSonUtils.getJson(br, "appVersion");
                plg.getLogger().info("***********************************");
                plg.getLogger().info("Site: " + host);
                if (StringUtils.isEmpty(usedVersion)) {
                    plg.getLogger().info("Used version: Unknown");
                } else {
                    plg.getLogger().info("Used version: " + usedVersion);
                    if (usedVersion.equals(target_version)) {
                        plg.getLogger().info("Expected version: OK");
                    } else {
                        plg.getLogger().info("Expected version: NOK");
                    }
                }
            } catch (final Throwable e) {
                plg.getLogger().info("!BROWSER ERROR!");
            }
        }
        plg.getLogger().info("***********************************");
    }

    /* Tries to find new websites based on current account! This only works if you are logged in an account! */
    public static void findNewPossiblySupportedSites(final Plugin plg, final Browser br) {
        try {
            final String sid = br.getCookie("ppp.contentdef.com", "ppp_session");
            if (sid == null) {
                plg.getLogger().warning("Failed to find sid");
                return;
            }
            getPage(br, "https://ppp.contentdef.com/thirdparty?sid=" + sid + "&_=" + System.currentTimeMillis());
            Map<String, Object> entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final List<Object> domaininfos = (List<Object>) entries.get("notificationNetworks");
            final PluginFinder finder = new PluginFinder();
            for (final Object domaininfo : domaininfos) {
                entries = (Map<String, Object>) domaininfo;
                final String name = (String) entries.get("name");
                final String domain = (String) entries.get("domain");
                if (StringUtils.isEmpty(name) || StringUtils.isEmpty(domain)) {
                    /* Skip invalid items */
                    continue;
                }
                final String plugin_host = finder.assignHost(domain);
                if (plugin_host == null) {
                    plg.getLogger().info("Found new host: " + plugin_host);
                }
            }
        } catch (final Throwable e) {
            plg.getLogger().log(e);
            plg.getLogger().info("Failure due to Exception");
        }
    }

    public static String getPornportalMainURL(final String host) {
        if (host == null) {
            return null;
        }
        /*
         * TODO: Move away from static method to e.g. support sites like: https://bbw-channel.pornportal.com/,
         * https://ebony-channel.pornportal.com/, https://latina-channel.pornportal.com/, https://cosplay-channel.pornportal.com/login,
         * https://stepfamily-channel.pornportal.com, https://3dxstar-channel.pornportal.com, https://realitygang-channel.pornportal.com,
         * https://lesbian-channel.pornportal.com, https://anal-channel.pornportal.com, https://milf-channel.pornportal.com,
         * https://teen-channel.pornportal.com/login
         */
        return "https://site-ma." + host;
    }

    public static String getAPIBase() {
        return "https://site-api.project1service.com/v1";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final String dllink = link.getStringProperty(PROPERTY_directurl);
        if (isHLS(dllink)) {
            return false;
        } else {
            return true;
        }
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    public static final String PROPERTY_directurl                  = "directurl";
    public static final String PROPERTY_VIDEO_ID                   = "videoid";
    public static final String PROPERTY_VIDEO_QUALITY              = "quality";
    public static final String PROPERTY_VIDEO_STREAM_TYPE          = "stream_type";
    public static final String PROPERTY_GALLERY_TYPE               = "gallerytype";
    public static final String PROPERTY_GALLERY_ID                 = "galleryid";
    public static final String PROPERTY_GALLERY_POSITION           = "gallery_position";
    public static final String PROPERTY_GALLERY_IMAGE_POSITION     = "gallery_image_position";
    public static final String PROPERTY_GALLERY_DIRECTORY          = "gallery_directory";
    public static final String PROPERTY_GALLERY_SIZE               = "gallery_size";
    public static final String PROPERTY_ACTORS_COMMA_SEPARATED     = "actors_comma_separated";
    public static Object       KEYLOCK                             = new Object();
    public static final String GALLERY_TYPE_GALLERY                = "gallery";
    public static final String GALLERY_TYPE_THUMBNAIL_SLASH_POSTER = "thumbnail_slash_poster";

    public static Request getPage(final Browser br, final Request request) throws Exception {
        br.getPage(request);
        String RNKEY = evalKEY(br);
        if (RNKEY == null) {
            return br.getRequest();
        }
        int maxLoops = 8;// up to 3 loops in tests
        synchronized (KEYLOCK) {
            while (true) {
                if (RNKEY == null) {
                    return br.getRequest();
                } else if (--maxLoops > 0) {
                    br.setCookie(br.getHost(), "KEY", RNKEY);
                    Thread.sleep(1000 + ((8 - maxLoops) * 500));
                    br.getPage(request.cloneRequest());
                    RNKEY = evalKEY(br);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
    }

    public static Request getPage(final Browser br, final String url) throws Exception {
        return getPage(br, br.createGetRequest(url));
    }

    /* Similar in: PornHubCom, PornportalCom */
    public final static String evalKEY(Browser br) throws ScriptException {
        if (br.containsHTML("document\\.cookie=\"KEY") && br.containsHTML("leastFactor")) {
            ScriptEngineManager mgr = JavaScriptEngineFactory.getScriptEngineManager(null);
            ScriptEngine engine = mgr.getEngineByName("JavaScript");
            String js = br.toString();
            js = new Regex(js, "<script[^>]*>(?:<!--)?(.*?)(?://-->)?</script>").getMatch(0);
            js = js.replace("document.cookie=", "return ");
            js = js.replaceAll("(/\\*.*?\\*/)", "");
            engine.eval(js + " var ret=go();");
            final String answer = engine.get("ret").toString();
            final String keyStr = new Regex(answer, "KEY=(.+)").getMatch(0);
            return keyStr.split(";")[0];
        } else {
            return null;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, null, false);
    }

    private String getGalleryType(final DownloadLink link) {
        final String galleryID = link.getStringProperty(PROPERTY_GALLERY_ID);
        if (galleryID == null) {
            /* Item is not part of an image gallery */
            return null;
        }
        /*
         * Use GALLERY_TYPE_GALLERY as fallback since in older plugin versions we only had one type: Image gallery. Thumbnails/Posters were
         * added later.
         */
        return link.getStringProperty(PROPERTY_GALLERY_TYPE, GALLERY_TYPE_GALLERY);
    }

    private String getContentID(final DownloadLink link) {
        final String videoid = link.getStringProperty(PROPERTY_VIDEO_ID);
        final String galleryid = link.getStringProperty(PROPERTY_GALLERY_ID);
        if (videoid != null) {
            return videoid;
        } else if (galleryid != null) {
            return galleryid;
        } else {
            return null;
        }
    }

    private boolean isVideo(final DownloadLink link) {
        final String videoid = link.getStringProperty(PROPERTY_VIDEO_ID);
        if (videoid != null) {
            return true;
        } else {
            return false;
        }
    }

    private boolean isImage(final DownloadLink link) {
        final String galleryid = link.getStringProperty(PROPERTY_GALLERY_ID);
        if (galleryid != null) {
            return true;
        } else {
            return false;
        }
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        this.setBrowserExclusive();
        final String dllink = link.getStringProperty(PROPERTY_directurl);
        if (dllink == null) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (account != null) {
            this.login(this.br, account, link.getHost(), false);
        }
        URLConnectionAdapter con = null;
        String newDirecturl = null;
        try {
            boolean refreshDirecturlNeeded = false;
            /*
             * 2024-08-05: HLS URLs may return response 200 on HEAD-request so we do not only rely on checking URLs here but we also
             * evaluate the "valid to" parameter stored inside the URL.
             */
            final UrlQuery query = UrlQuery.parse(dllink);
            String expireTimestampStr = query.get("validto");
            if (expireTimestampStr == null) {
                expireTimestampStr = new Regex(dllink, "st=\\d+~exp=(\\d+)").getMatch(0);// expire on m3u8 playlists
            }
            if (expireTimestampStr != null && expireTimestampStr.matches("\\d+") && Long.parseLong(expireTimestampStr) * 1000l < System.currentTimeMillis()) {
                logger.info("Directurl is expired according to timestamp from URL-parameter");
                refreshDirecturlNeeded = true;
            }
            if (!refreshDirecturlNeeded) {
                /* Check URL to see if a refresh is needed. */
                con = br.openHeadConnection(dllink);
                /**
                 * 403 = Generic expired </br>
                 * 472 = Video-directurl expired</br>
                 * 474 = Image directurl expired and/or any directurl is not expired but used with the wrong IP -> New one needs to be
                 * obtained.
                 */
                if (con.getResponseCode() == 403 || con.getResponseCode() == 472 || con.getResponseCode() == 474) {
                    br.followConnection(true);
                    refreshDirecturlNeeded = true;
                } else {
                    refreshDirecturlNeeded = false;
                }
            }
            if (refreshDirecturlNeeded) {
                logger.info("Directurl needs to be refreshed");
                if (!isDownload) {
                    /* Only refresh directurls in download mode - account will only be available in download mode anyways! */
                    return AvailableStatus.UNCHECKABLE;
                } else if (account == null) {
                    /* We need an account! */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Account required to refresh expired directurl", 5 * 60 * 1000l);
                }
                logger.info("Trying to refresh directurl");
                final String contentID = getContentID(link);
                if (contentID == null) {
                    /* This should never happen */
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                // this.login(this.br, account, link.getHost(), false);
                final PornportalComCrawler crawler = (PornportalComCrawler) this.getNewPluginForDecryptInstance(this.getHost());
                final ArrayList<DownloadLink> results = crawler.crawlContentAPI(this, contentID, account, null);
                final String targetLinkid = this.getLinkID(link);
                DownloadLink result = null;
                for (final DownloadLink item : results) {
                    if (StringUtils.equals(this.getLinkID(item), targetLinkid)) {
                        result = item;
                        break;
                    }
                }
                if (result == null) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to refresh expired directurl --> Content offline or session expired?");
                }
                newDirecturl = result.getStringProperty(PROPERTY_directurl);
                if (StringUtils.isEmpty(newDirecturl)) {
                    /* This should never happen */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                logger.info("Successfully found new directurl");
                con = br.openHeadConnection(newDirecturl);
            }
            final boolean looksLikeHLSContent = con.getResponseCode() == 200 && LinkCrawlerDeepInspector.looksLikeMpegURL(con);
            if (con.getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (!looksLikeDownloadableContent(con) && !looksLikeHLSContent) {
                br.followConnection(true);
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Directurl did not lead to downloadable content");
            }
            if (con.getCompleteContentLength() > 0 && !looksLikeHLSContent) {
                if (con.isContentDecoded()) {
                    link.setDownloadSize(con.getCompleteContentLength());
                } else {
                    link.setDownloadSize(con.getCompleteContentLength());
                }
            }
            /*
             * 2020-04-08: Final filename is supposed to be set in crawler. Their internal filenames are always the same e.g.
             * "scene_320p.mp4".
             */
            // link.setFinalFileName(Encoding.htmlDecode(getFileNameFromHeader(con)));
            if (newDirecturl != null) {
                /* Only set new directurl if it is working. Keep old one until then! */
                logger.info("Successfully checked new directurl and set property");
                link.setProperty(PROPERTY_directurl, newDirecturl);
            }
        } finally {
            try {
                con.disconnect();
            } catch (final Throwable ignore) {
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        this.handleDownload(link, null);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        logger.info("Downloading in multihoster mode");
        this.handlePremium(link, account);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    /* Account properties */
    private static final String PROPERTY_authorization                     = "authorization";
    private static final String PROPERTY_jwt                               = "jwt";
    private static final String PROPERTY_timestamp_website_cookies_updated = "timestamp_website_cookies_updated";
    public static final String  PROPERTY_cookiename_authCookie             = "auth_cookie";
    public static final String  PROPERTY_cookiename_instanceCookie         = "instanceCookie";
    public static final String  PROPERTY_url_external_login                = "url_external_login";
    /* Plugin properties */
    public static final String  PROPERTY_plugin_jwt                        = "jwt";
    public static final String  PROPERTY_plugin_jwt_create_timestamp       = "jwt_create_timestamp";

    public void login(final Browser brlogin, final Account account, final String target_domain, final boolean checkCookies) throws Exception {
        synchronized (account) {
            final boolean isExternalPortalLogin;
            if (!target_domain.equalsIgnoreCase(account.getHoster())) {
                // TODO: Remove this
                isExternalPortalLogin = true;
            } else {
                isExternalPortalLogin = false;
            }
            if (isExternalPortalLogin) {
                /* Login via "Jump-URL" into other portal */
                logger.info("External portal login: " + target_domain);
            } else {
                /* Login to main portal */
                logger.info("Internal portal login: " + target_domain);
            }
            brlogin.setCookiesExclusive(true);
            // checkUsedVersions(this);
            final Cookies cookies = account.loadCookies(target_domain);
            final Cookies userCookies = account.loadUserCookies();
            String jwt = null;
            String login_cookie_key = getDefaultCookieNameLogin();
            String login_cookie = null;
            if (cookies != null || userCookies != null) {
                /*
                 * Try to avoid login captcha at all cost!
                 */
                final Cookies targetCookies;
                if (userCookies != null) {
                    final Cookie logincookie_cookie = userCookies.get(login_cookie_key);
                    if (logincookie_cookie == null || StringUtils.isEmpty(logincookie_cookie.getValue())) {
                    }
                    login_cookie = logincookie_cookie.getValue();
                    /* Store for later usage */
                    setPropertyAccount(account, target_domain, PROPERTY_authorization, login_cookie);
                    targetCookies = userCookies;
                } else {
                    targetCookies = cookies;
                }
                brlogin.setCookies(target_domain, targetCookies);
                brlogin.setCookies(targetCookies);
                jwt = this.getStringPropertyAccount(account, target_domain, PROPERTY_jwt, null);
                if (jwt == null) {
                    /* Especially needed for user cookie login */
                    logger.info("Obtaining JWT value for the first time");
                    this.getFreshJWT(brlogin, account, targetCookies, true);
                }
                setStoredAPIAuthHeaderAccount(brlogin, account, target_domain);
                if (!checkCookies) {
                    /* Trust cookies without check */
                    logger.info("Trust cookies without check");
                    return;
                }
                logger.info("Checking cookies");
                getPage(brlogin, getAPIBase() + "/self");
                if (brlogin.getHttpConnection().getResponseCode() == 200) {
                    logger.info("Cookie login successful");
                    final long timestamp_headers_updated = this.getLongPropertyAccount(account, target_domain, PROPERTY_timestamp_website_cookies_updated, 0);
                    /* Update website cookies sometimes although we really use the Website-API for most of all requests. */
                    if (System.currentTimeMillis() - timestamp_headers_updated >= 5 * 60 * 1000l) {
                        logger.info("Updating website cookies and JWT value");
                        this.getFreshJWT(brlogin, account, targetCookies, false);
                    }
                    if (userCookies == null) {
                        /* Only store cookies on account item if they were not supplied by user. */
                        account.saveCookies(brlogin.getCookies(target_domain), target_domain);
                    }
                    return;
                }
                logger.info("Cookie login failed");
                if (userCookies != null) {
                    /* Dead end */
                    logger.info("User Cookie login failed");
                    if (account.hasEverBeenValid()) {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                    } else {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                    }
                }
                /* Important: Especially old "Authorization" headers can cause trouble! */
                brlogin.clearCookies(null);
                account.clearCookies("");
            }
            logger.info("Performing full login");
            brlogin.setCookiesExclusive(true);
            Map<String, Object> entries;
            if (isExternalPortalLogin) {
                handleExternalLoginStep(brlogin, account, target_domain);
                /* Now we should finally land on '/postlogin' --> This would mean SUCCESS! */
                if (!brlogin.getURL().contains("/postlogin")) {
                    logger.warning("Possible external login failure: Expected location '/postlogin' but got this instead: " + br.getURL());
                }
                /* Further checks will decide whether we're loggedIN or not */
                entries = getJsonJuanEawInstance(brlogin);
                login_cookie_key = PluginJSonUtils.getJson(brlogin, "authCookie");
            } else {
                getPage(brlogin, getPornportalMainURL(target_domain) + "/login");
                entries = getJsonJuanEawInstance(brlogin);
                final String authApiUrl = PluginJSonUtils.getJson(brlogin, "authApiUrl");
                login_cookie_key = PluginJSonUtils.getJson(brlogin, "authCookie");
                if (login_cookie_key == null) {
                    login_cookie_key = getDefaultCookieNameLogin();
                }
                if (StringUtils.isEmpty(authApiUrl)) {
                    logger.warning("Failed to find api_base");
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                } else if (!prepareBrAPI(this, brlogin, account, entries)) {
                    logger.warning("Failed to prepare API headers");
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final Map<String, Object> domainmap = (Map<String, Object>) entries.get("domain");
                final String hostname = domainmap.get("hostname").toString(); // E.g. site-ma.fakehub.com
                final String recaptchaSiteKey = (String) domainmap.get("siteKey");
                final String recaptchaInvisibleSiteKey = (String) domainmap.get("siteKeyV3");
                final boolean allowNormalReCaptchaV2 = false;
                final boolean allowInvisibleReCaptchaV2 = true; // 2023-11-02
                /* Prepare POST-data */
                final Map<String, Object> logindata = new HashMap<String, Object>();
                logindata.put("hostname", hostname);
                logindata.put("username", account.getUser());
                logindata.put("password", account.getPass());
                /* 2023-11-02: Not needed anymore TODO: Remove commented-out-code down below */
                // logindata.put("failureUrl", "https://" + hostname + "/access/failure");
                // logindata.put("successUrl", "https://" + hostname + "/access/success");
                /* 2020-04-03: So far, all pornportal websites required a captcha on login. */
                String recaptchaV2Response = null;
                if (!StringUtils.isEmpty(recaptchaSiteKey) && allowNormalReCaptchaV2) {
                    final CaptchaHelperHostPluginRecaptchaV2 captcha = new CaptchaHelperHostPluginRecaptchaV2(this, brlogin, recaptchaSiteKey);
                    recaptchaV2Response = captcha.getToken();
                    logindata.put("googleReCaptchaResponse", recaptchaV2Response);
                } else if (!StringUtils.isEmpty(recaptchaInvisibleSiteKey) && allowInvisibleReCaptchaV2) {
                    final CaptchaHelperHostPluginRecaptchaV2 captcha = new CaptchaHelperHostPluginRecaptchaV2(this, brlogin, recaptchaInvisibleSiteKey) {
                        @Override
                        public TYPE getType() {
                            return TYPE.INVISIBLE;
                        }

                        @Override
                        protected boolean isEnterprise() {
                            /* 2023-01-11: This is mandatory! Without this, this reCaptcha may fail for some users. */
                            return true;
                        }
                    };
                    recaptchaV2Response = captcha.getToken();
                    logindata.put("googleReCaptchaResponse", recaptchaV2Response);
                    logindata.put("googleReCaptchaVersion", "v3");
                }
                final PostRequest postRequest = brlogin.createPostRequest(authApiUrl + "/v1/authenticate", JSonStorage.serializeToJson(logindata));
                getPage(brlogin, postRequest);
                if (brlogin.getHttpConnection().getResponseCode() == 401) {
                    throw new AccountInvalidException();
                } else if (brlogin.getHttpConnection().getResponseCode() == 403) {
                    throw new AccountInvalidException();
                }
                final Object resp = restoreFromString(brlogin.getRequest().getHtmlCode(), TypeRef.OBJECT);
                if (resp instanceof List) {
                    /* Assume we got an error e.g. [{"code":3004,"message":"Invalid Captcha."}] */
                    final List<Map<String, Object>> errorlist = (List<Map<String, Object>>) resp;
                    final Map<String, Object> error0 = errorlist.get(0);
                    throw new AccountInvalidException("Code " + error0.get("code") + " | " + error0.get("message"));
                }
                final Map<String, Object> authinfo = (Map<String, Object>) resp;
                final String accessToken = (String) authinfo.get("access_token");
                if (StringUtils.isEmpty(accessToken)) {
                    throw new AccountInvalidException();
                }
                brlogin.setCookie(hostname, login_cookie_key, accessToken);
                final String authenticationUrl = "https://" + hostname + "/postlogin";
                /* Now continue without API */
                getPage(brlogin, authenticationUrl);
                final Form continueform = brlogin.getFormbyKey("response");
                if (continueform != null) {
                    /*
                     * Redirect from API to main website --> Grants us authorization cookie which can then again be used to authorize API
                     * requests
                     */
                    logger.info("Found continueform");
                    brlogin.submitForm(continueform);
                } else {
                    logger.warning("Failed to find continueform");
                }
            }
            /* Now we should e.g. be here: '/postlogin' */
            if (login_cookie_key == null) {
                login_cookie_key = getDefaultCookieNameLogin();
            }
            /*
             * 2020-04-18: This cookie is valid for (max.) 24 hours.
             */
            login_cookie = getLoginCookie(brlogin, login_cookie_key);
            jwt = PluginJSonUtils.getJson(brlogin, "jwt");
            if (login_cookie == null) {
                logger.info("Login failure after API login");
                loginFailure(isExternalPortalLogin);
            } else if (StringUtils.isEmpty(jwt)) {
                logger.info("Login failure after API login");
                loginFailure(isExternalPortalLogin);
            }
            logger.info("Looks like successful login");
            setPropertyAccount(account, target_domain, PROPERTY_authorization, login_cookie);
            setPropertyAccount(account, target_domain, PROPERTY_jwt, jwt);
            setPropertyAccount(account, target_domain, PROPERTY_timestamp_website_cookies_updated, System.currentTimeMillis());
            account.saveCookies(brlogin.getCookies(brlogin.getHost()), target_domain);
            setStoredAPIAuthHeaderAccount(brlogin, account, target_domain);
        }
    }

    private void getFreshJWT(final Browser sourceBrowser, final Account account, final Cookies cookies, final boolean exceptionOnFailure) throws Exception {
        /* Access main page without authorization headers but with cookies */
        final Browser brc = this.createNewBrowserInstance();
        final String target_domain = account.getHoster();
        brc.setCookies(target_domain, cookies);
        getPage(brc, getPornportalMainURL(account.getHoster()));
        /* Attention: This is very unsafe without using json parser! */
        String jwt = PluginJSonUtils.getJson(brc, "jwt");
        if (jwt != null) {
            logger.info("Storing new jwt value: " + jwt);
            this.setPropertyAccount(account, target_domain, PROPERTY_jwt, jwt);
            sourceBrowser.setCookie(getPornportalMainURL(account.getHoster()), this.getStringPropertyAccount(account, target_domain, PROPERTY_cookiename_instanceCookie, getDefaultCookieNameInstance()), jwt);
            this.setPropertyAccount(account, target_domain, PROPERTY_timestamp_website_cookies_updated, System.currentTimeMillis());
        } else if (exceptionOnFailure) {
            throw new AccountInvalidException("Failed to refresh JWT");
        } else {
            logger.warning("Failed to find jwt");
        }
    }

    private void handleExternalLoginStep(final Browser brlogin, final Account account, final String target_domain) throws Exception {
        String autologinURL = this.getStringPropertyAccount(account, target_domain, PROPERTY_url_external_login, null);
        if (autologinURL == null) {
            logger.warning("Property autologinURL is null");
            return;
        }
        if (autologinURL.startsWith("/")) {
            autologinURL = "https://ppp.contentdef.com/" + autologinURL;
        }
        getPage(brlogin, autologinURL);
        final String redirectURL = brlogin.getRegex("window\\.top\\.location\\s*=\\s*\\'(https?://[^<>\"\\']+)").getMatch(0);
        if (redirectURL == null) {
            logger.warning("Failed to find external login redirectURL");
            return;
        }
        /* This is already the final step for non-pornportal sites */
        getPage(brlogin, redirectURL);
        /* This is usually the final step for pornportal sites */
        final Form probillerForm = brlogin.getFormbyActionRegex(".+access/success.*?");
        if (probillerForm != null) {
            logger.info("Found proBiller Form");
            brlogin.submitForm(probillerForm);
        }
    }

    private void loginFailure(final boolean isExternalLogin) throws PluginException {
        if (isExternalLogin) {
            /*
             * Never throw exceptions which affect accounts in here as we do not e.g. want to (temp.) disable a erito.com account just
             * because external login for fakehub.com fails here!
             */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "External portal login failed");
        } else {
            throw new AccountInvalidException();
        }
    }

    private void setPropertyAccount(final Account account, final String target_domain, final String key, final Object value) {
        account.setProperty(key + "_" + target_domain, value);
    }

    private String getStringPropertyAccount(final Account account, final String target_domain, final String key, final String fallback) {
        return account.getStringProperty(key + "_" + target_domain, fallback);
    }

    private long getLongPropertyAccount(final Account account, final String target_domain, final String key, final long fallback) {
        return account.getLongProperty(key + "_" + target_domain, fallback);
    }

    private String getLoginCookie(final Browser br, String login_cookie_key) {
        if (login_cookie_key == null) {
            login_cookie_key = getDefaultCookieNameLogin();
        }
        return br.getCookie(br.getHost(), login_cookie_key, Cookies.NOTDELETEDPATTERN);
    }

    public static boolean prepareBrAPI(final Plugin plg, final Browser br, final Account acc) throws PluginException {
        final Map<String, Object> entries = getJsonJuanEawInstance(br);
        return prepareBrAPI(plg, br, acc, entries);
    }

    /** Sets required API headers based on data given in json. */
    public static boolean prepareBrAPI(final Plugin plg, final Browser br, final Account acc, Map<String, Object> entries) throws PluginException {
        final String plugin_host = plg.getHost();
        final String hostname;
        String ip = null;
        if (entries != null) {
            entries = (Map<String, Object>) entries.get("domain");
            /* E.g. site-ma.fakehub.com */
            hostname = (String) entries.get("hostname");
            ip = PluginJSonUtils.getJson(br, "ip");
        } else {
            hostname = getPornportalMainURL(plugin_host);
        }
        boolean isNewJWT = false;
        String jwt = null;
        if (acc == null) {
            /* Try to re-use old token */
            jwt = setAndGetStoredAPIAuthHeaderPlugin(br, plg);
        }
        if (jwt == null) {
            if (entries == null) {
                /* E.g. attempt to restore old tokens without having json/html with new data available. */
                return false;
            }
            jwt = PluginJSonUtils.getJson(br, "jwt");
            isNewJWT = true;
        }
        String cookie_name_instance = PluginJSonUtils.getJson(br, "instanceCookie");
        if (cookie_name_instance == null) {
            cookie_name_instance = getDefaultCookieNameInstance();
        } else if (acc != null) {
            acc.setProperty(PROPERTY_cookiename_instanceCookie, cookie_name_instance);
        }
        if (StringUtils.isEmpty(jwt) || StringUtils.isEmpty(hostname)) {
            plg.getLogger().warning("Failed to find api base data");
            return false;
        }
        br.setCookie(hostname, cookie_name_instance, jwt);
        br.getHeaders().put("Content-Type", "application/json");
        br.getHeaders().put("Referer", "https://" + hostname + "/login");
        br.getHeaders().put("sec-fetch-dest", "empty");
        br.getHeaders().put("sec-fetch-mode", "cors");
        br.getHeaders().put("sec-fetch-site", "cross-site");
        br.getHeaders().put("Origin", "https://" + hostname);
        br.getHeaders().put("Instance", jwt);
        if (!StringUtils.isEmpty(ip)) {
            br.getHeaders().put("x-forwarded-for", ip);
        }
        if (acc == null && isNewJWT) {
            plg.getPluginConfig().setProperty(PROPERTY_plugin_jwt, jwt);
            plg.getPluginConfig().setProperty(PROPERTY_plugin_jwt_create_timestamp, System.currentTimeMillis());
        }
        return true;
    }

    public static Map<String, Object> getJsonJuanEawInstance(final Browser br) {
        final String json = br.getRegex("window\\.__JUAN\\.rawInstance = (\\{.*?\\});\n").getMatch(0);
        return JSonStorage.restoreFromString(json, TypeRef.MAP);
    }

    public static Map<String, Object> getJsonJuanInitialState(final Browser br) {
        final String json = br.getRegex("window\\.__JUAN\\.initialState = (\\{.*?\\});\\s+").getMatch(0);
        return JSonStorage.restoreFromString(json, TypeRef.MAP);
    }

    private static final String getDefaultCookieNameLogin() {
        /* 2020-04-03 */
        return "access_token_ma";
    }

    public static final String getDefaultCookieNameInstance() {
        /* 2020-04-03 */
        return "instance_token";
    }

    /* Sets headers required to do API requests. */
    private boolean setStoredAPIAuthHeaderAccount(final Browser br, final Account account, final String target_domain) {
        final String jwt = this.getStringPropertyAccount(account, target_domain, PROPERTY_jwt, null);
        final String authorization = this.getStringPropertyAccount(account, target_domain, PROPERTY_authorization, null);
        if (jwt == null || authorization == null) {
            /* This should never happen */
            return false;
        }
        br.getHeaders().put("Instance", jwt);
        br.getHeaders().put("Authorization", authorization);
        return true;
    }

    public static String setAndGetStoredAPIAuthHeaderPlugin(final Browser br, final Plugin plg) {
        if (plg == null) {
            return null;
        }
        final String jwt = plg.getPluginConfig().getStringProperty(PROPERTY_plugin_jwt);
        final long timestamp_jwt_created = plg.getPluginConfig().getLongProperty(PROPERTY_plugin_jwt_create_timestamp, 0);
        final long jwt_age = System.currentTimeMillis() - timestamp_jwt_created;
        final long max_jwt_age_minutes = 5;
        if (jwt == null) {
            return null;
        } else if (jwt_age > max_jwt_age_minutes * 60 * 1000) {
            plg.getLogger().info("jwt is older than " + max_jwt_age_minutes + " minutes --> New jwt required");
            return null;
        }
        br.getHeaders().put("Instance", jwt);
        return jwt;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        synchronized (account) {
            login(br, account, this.getHost(), true);
            final AccountInfo ai = new AccountInfo();
            account.setConcurrentUsePossible(true);
            ai.setUnlimitedTraffic();
            if (br.getURL() == null || !br.getURL().contains("/v1/self")) {
                getPage(br, getAPIBase() + "/self");
            }
            final Map<String, Object> user = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String username = (String) user.get("username");
            final String joinDate = (String) user.get("joinDate");
            final String defaultDateFormat = "yyyy'-'MM'-'dd'T'HH':'mm':'ss";
            if (!StringUtils.isEmpty(joinDate)) {
                ai.setCreateTime(TimeFormatter.getMilliSeconds(joinDate, defaultDateFormat, Locale.ENGLISH));
            }
            if (Boolean.TRUE.equals(user.get("isBanned"))) {
                /*
                 * 2021-11-08: This may randomly be "true" (also via website) although the account is definitely not banned! Tested with a
                 * brazzers.com account. --> Ignore this for now!
                 */
                final boolean trustBannedFlag = false;
                if (trustBannedFlag) {
                    throw new AccountInvalidException("Account banned for reason: " + user.get("banReason"));
                } else {
                    logger.info("Account might be banned??");
                }
            }
            final Boolean isExpired = (Boolean) user.get("isExpired");
            final Boolean isTrial = (Boolean) user.get("isTrial");
            final Boolean isCanceled = (Boolean) user.get("isCanceled");
            final Boolean hasAccess = (Boolean) user.get("hasAccess");
            final Number initialAmount = (Number) user.get("initialAmount");
            long expireTimestamp = -1;
            final String expiryDate = (String) user.get("expiryDate");
            if (!StringUtils.isEmpty(expiryDate)) {
                expireTimestamp = TimeFormatter.getMilliSeconds(expiryDate, defaultDateFormat, Locale.ENGLISH);
            }
            final ArrayList<String> packageFeatures = new ArrayList<String>();
            if (Boolean.TRUE.equals(isExpired)) {
                account.setType(AccountType.FREE);
                /* Free accounts can be used to download trailers */
                packageFeatures.add("expired premium");
            } else if (Boolean.TRUE.equals(isTrial)) {
                /* Free trial -> Free Account with premium capability */
                account.setType(AccountType.PREMIUM);
                packageFeatures.add("Trial");
            } else if (expireTimestamp > System.currentTimeMillis()) {
                /* Premium user with running contract */
                ai.setValidUntil(expireTimestamp, br);
                account.setType(AccountType.PREMIUM);
            } else if (Boolean.TRUE.equals(hasAccess)) {
                /* Premium account without expire-date */
                account.setType(AccountType.PREMIUM);
            } else if (initialAmount == null || initialAmount.longValue() <= 0) {
                /* Free user who never (?) had a premium subscription. */
                account.setType(AccountType.FREE);
            } else {
                /* Assume that we got a premium account */
                account.setType(AccountType.PREMIUM);
            }
            if (isCanceled != null && !Boolean.TRUE.equals(isExpired)) {
                if (Boolean.TRUE.equals(isCanceled)) {
                    packageFeatures.add("subscription cancelled");
                } else {
                    packageFeatures.add("subscription running");
                }
            }
            /**
             * Try to find alternative bundle-expire-date inside users' additional purchased "bundles". </br>
             * Each bundle can have different expire-dates and separate pricing.
             */
            final Map<String, Object> marketplaceProfile = (Map<String, Object>) user.get("marketplaceProfile");
            long highestExpireTimestamp = -1;
            String titleOfBundleWithHighestExpireDate = null;
            int numberOfActiveBundles = 0;
            if (marketplaceProfile != null) {
                final List<Map<String, Object>> bundles = (List<Map<String, Object>>) marketplaceProfile.get("bundles");
                if (bundles != null) {
                    for (final Map<String, Object> bundle : bundles) {
                        if (!(Boolean) bundle.get("isActive")) {
                            continue;
                        }
                        numberOfActiveBundles++;
                        final String expireDateStrTmp = (String) bundle.get("expirationDate");
                        if (expireDateStrTmp == null) {
                            /* Active bundle without expire-date, possible one with field "isUnlimited":true */
                            continue;
                        }
                        final long expireTimestampTmp = TimeFormatter.getMilliSeconds(expireDateStrTmp, defaultDateFormat, Locale.ENGLISH);
                        if (expireTimestampTmp < highestExpireTimestamp) {
                            continue;
                        }
                        highestExpireTimestamp = expireTimestampTmp;
                        titleOfBundleWithHighestExpireDate = (String) bundle.get("title");
                    }
                }
            }
            logger.info("Number of active bundles: " + numberOfActiveBundles);
            packageFeatures.add("Active bundles: " + numberOfActiveBundles);
            String packageFeaturesCommaSeparated = "";
            for (final String packageFeature : packageFeatures) {
                if (packageFeaturesCommaSeparated.length() > 0) {
                    packageFeaturesCommaSeparated += ", ";
                }
                packageFeaturesCommaSeparated += packageFeature;
            }
            ai.setStatus(account.getType().getLabel() + " (" + packageFeaturesCommaSeparated + ")");
            if (account.getType() == AccountType.PREMIUM) {
                if (highestExpireTimestamp < System.currentTimeMillis()) {
                    logger.info("Failed to find alternative expiredate");
                } else {
                    logger.info("Successfully found alternative expiredate");
                    ai.setValidUntil(highestExpireTimestamp, br);
                    if (!StringUtils.isEmpty(titleOfBundleWithHighestExpireDate)) {
                        ai.setStatus(ai.getStatus() + " [" + titleOfBundleWithHighestExpireDate + "]");
                    }
                }
            }
            if (account.getType() == AccountType.FREE && numberOfActiveBundles == 0) {
                /* Free account without any active bundles -> Cannot access more content than accessing website without account. */
                ai.setExpired(true);
            }
            if (account.loadUserCookies() != null && !StringUtils.isEmpty(username)) {
                /*
                 * When cookie login is used, user could enter anything into username field but we want to make sure that we have an unique
                 * username value.
                 */
                account.setUser(username);
            }
            return ai;
        }
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload(link, account);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link, account, true);
        final String dllink = link.getStringProperty(PROPERTY_directurl);
        if (StringUtils.isEmpty(dllink)) {
            /* This should never happen! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (isHLS(dllink)) {
            final HlsContainer hlsbest = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(br.cloneBrowser(), dllink));
            if (hlsbest == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, hlsbest.getStreamURL());
            dl.startDownload();
        } else {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            handleConnectionErrors(br, dl.getConnection());
            dl.startDownload();
        }
    }

    private static boolean isHLS(final String url) {
        if (StringUtils.containsIgnoreCase(url, "master.m3u8")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public Class<? extends PornportalComConfig> getConfigInterface() {
        return PornportalComConfig.class;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        /**
         * 2024-08-27: They've added rate-limiting for HLS streams. Allowing only max 1 downloads is my simple attempt for this moment.
         * </br>
         * See also: https://board.jdownloader.org/showthread.php?t=96341
         */
        return 1;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.PornPortal;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}