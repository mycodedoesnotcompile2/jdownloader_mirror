package org.jdownloader.plugins.components;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.SocketException;
import java.net.URL;
import java.net.URLDecoder;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Currency;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import jd.PluginWrapper;
import jd.config.SubConfiguration;
import jd.controlling.AccountController;
import jd.controlling.linkcrawler.LinkCrawlerDeepInspector;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.parser.html.HTMLParser;
import jd.parser.html.InputField;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadConnectionVerifier;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.SiteType.SiteTemplate;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.AbstractCloudflareTurnstileCaptcha;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;
import org.jdownloader.captcha.v2.challenge.hcaptcha.CaptchaHelperHostPluginHCaptcha;
import org.jdownloader.captcha.v2.challenge.keycaptcha.KeyCaptcha;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.config.XFSConfig;
import org.jdownloader.plugins.components.config.XFSConfigVideo;
import org.jdownloader.plugins.components.config.XFSConfigVideo.DownloadMode;
import org.jdownloader.plugins.components.config.XFSConfigVideo.PreferredDownloadQuality;
import org.jdownloader.plugins.components.config.XFSConfigVideo.PreferredStreamQuality;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;
import org.mozilla.javascript.EcmaError;

@HostPlugin(revision = "$Revision: 51270 $", interfaceVersion = 2, names = {}, urls = {})
public abstract class XFileSharingProBasic extends antiDDoSForHost implements DownloadConnectionVerifier {
    public XFileSharingProBasic(PluginWrapper wrapper) {
        super(wrapper);
        // this.enablePremium(super.getPurchasePremiumURL());
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    // public static List<String[]> getPluginDomains() {
    // final List<String[]> ret = new ArrayList<String[]>();
    // // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
    // ret.add(new String[] { "example.com" });
    // return ret;
    // }
    //
    // public static String[] getAnnotationNames() {
    // return buildAnnotationNames(getPluginDomains());
    // }
    //
    // @Override
    // public String[] siteSupportedNames() {
    // return buildSupportedNames(getPluginDomains());
    // }
    //
    // public static String[] getAnnotationUrls() {
    // return XFileSharingProBasic.buildAnnotationUrls(getPluginDomains());
    // }
    // @Override
    // public String rewriteHost(final String host) {
    // return this.rewriteHost(getPluginDomains(), host);
    // }
    public static final String getDefaultAnnotationPatternPart() {
        return "/(d/[A-Za-z0-9]+|(d|e)/[a-z0-9]{12}|embed-[a-z0-9]{12}\\.html|[a-z0-9]{12}(/[^/]+(?:\\.html)?)?)";
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        try {
            String urlfilename = new Regex(link.getPluginPatternMatcher(), "(?i).+/(.+)\\.html$").getMatch(0);
            if (urlfilename != null) {
                urlfilename = URLEncode.decodeURIComponent(urlfilename, "UTF-8", true);
                return urlfilename;
            }
        } catch (Exception e) {
        }
        return super.getDefaultFileName(link);
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(?::\\d+)?" + XFileSharingProBasic.getDefaultAnnotationPatternPart());
        }
        return ret.toArray(new String[0]);
    }

    /**
     * Override this and add dead domains so upper handling can auto update added URLs and change domain if it contains a dead domain. This
     * way a lot of "old" URLs will continue to work in JD while they may fail in browser. <br>
     */
    protected List<String> getDeadDomains() {
        return null;
    }

    /* Used variables */
    protected WeakHashMap<Request, String[]>  correctedBrowserRequestMap                                        = new WeakHashMap<Request, String[]>();
    /* Don't touch the following! */
    private static Map<String, AtomicInteger> freeRunning                                                       = new HashMap<String, AtomicInteger>();
    protected static final String             PROPERTY_ACCOUNT_apikey                                           = "apikey";
    private static final String               PROPERTY_PLUGIN_api_domain_with_protocol                          = "apidomain";
    protected static final String             PROPERTY_PLUGIN_REPORT_FILE_AVAILABLECHECK_LAST_FAILURE_TIMESTAMP = "REPORT_FILE_AVAILABLECHECK_LAST_FAILURE_TIMESTAMP";
    protected static final String             PROPERTY_PLUGIN_REPORT_FILE_AVAILABLECHECK_LAST_FAILURE_VERSION   = "REPORT_FILE_AVAILABLECHECK_LAST_FAILURE_VERSION";
    private static final String               PROPERTY_PLUGIN_ALT_AVAILABLECHECK_LAST_FAILURE_TIMESTAMP         = "ALT_AVAILABLECHECK_LAST_FAILURE_TIMESTAMP";
    private static final String               PROPERTY_PLUGIN_ALT_AVAILABLECHECK_LAST_FAILURE_VERSION           = "ALT_AVAILABLECHECK_LAST_FAILURE_VERSION";
    private static final String               PROPERTY_PLUGIN_ALT_AVAILABLECHECK_LAST_WORKING                   = "ALT_AVAILABLECHECK_LAST_WORKING";
    protected static final String             PROPERTY_ACCOUNT_ALLOW_API_DOWNLOAD_ATTEMPT_IN_WEBSITE_MODE       = "allow_api_download_attempt_in_website_mode";
    private String                            videoStreamDownloadurl                                            = null;
    private boolean                           hasCheckedEmbedHandling                                           = false;

    public static enum URL_TYPE {
        EMBED_VIDEO,
        EMBED_VIDEO_2,
        FILE,
        IMAGE,
        NORMAL,
        SHORT,
        OFFICIAL_VIDEO_DOWNLOAD
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        final List<LazyPlugin.FEATURE> ret = new ArrayList<LazyPlugin.FEATURE>();
        if (enableAccountApiOnlyMode()) {
            ret.add(LazyPlugin.FEATURE.API_KEY_LOGIN);
        } else {
            if (requiresCookieLogin()) {
                ret.add(LazyPlugin.FEATURE.COOKIE_LOGIN_ONLY);
            } else {
                ret.add(LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL);
            }
        }
        if (isImagehoster()) {
            ret.add(LazyPlugin.FEATURE.IMAGE_HOST);
        }
        if (isVideohoster(null)) {
            ret.add(LazyPlugin.FEATURE.VIDEO_STREAMING);
        }
        return ret.toArray(new LazyPlugin.FEATURE[0]);
    }

    /**
     * DEV NOTES XfileSharingProBasic Version 4.4.3.8<br />
     * See official changelogs for upcoming XFS changes: https://sibsoft.net/xfilesharing/changelog.html |
     * https://sibsoft.net/xvideosharing/changelog.html <br/>
     * limit-info:<br />
     * captchatype-info: null 4dignum reCaptchaV2, hcaptcha<br />
     */
    @Override
    public String getAGBLink() {
        return this.getMainPage() + "/tos.html";
    }

    public String getPurchasePremiumURL() {
        return this.getMainPage() + "/premium.html";
    }

    /**
     * Returns whether resume is supported or not for current download mode based on account availability and account type. <br />
     * Override this function to set resume settings!
     */
    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (isPremium(account)) {
            /* Premium account */
            return true;
        } else if (isFree(account)) {
            /* Free Account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    protected boolean isPremium(Account account) {
        final AccountType type = account != null ? account.getType() : null;
        return AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type);
    }

    protected boolean isFree(Account account) {
        final AccountType type = account != null ? account.getType() : null;
        return AccountType.FREE.equals(type);
    }

    /**
     * Returns how many max. chunks per file are allowed for current download mode based on account availability and account type. <br />
     * Override this function to set chunks settings!
     */
    public int getMaxChunks(final Account account) {
        if (isPremium(account)) {
            /* Premium account */
            return 0;
        } else if (isFree(account)) {
            /* Free Account */
            return 1;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    protected AtomicInteger getFreeRunning() {
        synchronized (freeRunning) {
            AtomicInteger ret = freeRunning.get(getHost());
            if (ret == null) {
                ret = new AtomicInteger(0);
                freeRunning.put(getHost(), ret);
            }
            return ret;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        final int max = getMaxSimultaneousFreeAnonymousDownloads();
        if (max == -1) {
            return -1;
        } else {
            final int running = getFreeRunning().get();
            final int ret = Math.min(running + 1, max);
            return ret;
        }
    }

    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return 1;
    }

    public int getMaxSimultaneousFreeAccountDownloads() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    /** Returns property to store generated directurl -> Depends on current download mode and account account type. */
    protected String getDownloadModeDirectlinkProperty(final Account account) {
        if (isFree(account)) {
            /* Free Account */
            return "freelink2";
        } else if (isPremium(account)) {
            /* Premium account */
            return "premlink";
        } else {
            /* Free(anonymous) and unknown account type */
            return "freelink";
        }
    }

    protected boolean useHTTPS() {
        return websiteSupportsHTTPS() && userPrefersHTTPS();
    }

    /**
     * @return true: Website supports https and plugin will prefer https. <br />
     *         false: Website does not support https - plugin will avoid https. <br />
     *         default: true
     */
    protected boolean websiteSupportsHTTPS() {
        return true;
    }

    protected boolean userPrefersHTTPS() {
        final Class<? extends XFSConfig> cfgO = this.getConfigInterface();
        if (cfgO != null) {
            return !PluginJsonConfig.get(cfgO).isPreferHTTP();
        } else {
            return true;
        }
    }

    /**
     * Relevant for premium accounts.
     *
     * @return A list of possible 'paymentURLs' which may contain an exact premium account expire-date down to the second. Return null to
     *         disable this feature!
     */
    protected String[] supportsPreciseExpireDate() {
        return new String[] { "/?op=payments", "/upgrade" };
    }

    /**
     * <b> Enabling this leads to at least one additional http-request! </b> <br />
     * Enable this for websites using <a href="https://sibsoft.net/xvideosharing.html">XVideosharing</a>. <br />
     * Demo-Website: <a href="http://xvideosharing.com">xvideosharing.com</a> DO NOT CALL THIS DIRECTLY - ALWAYS USE
     * {@link #internal_isVideohosterEmbed()}!!!<br />
     *
     * @return true: Try to find final downloadlink via '/embed-<fuid>.html' request. <br />
     *         false: Skips this part. <br />
     *         default: false
     */
    protected boolean isVideohosterEmbed() {
        return false;
    }

    /**
     * Checks whether current html code contains embed code for current fuid which would indicate that we have a videohost and it looks like
     * we can access the embed URL to stream/download our video content. <br>
     * </b> Attention! Browser can be null! </b>
     */
    protected boolean isVideohosterEmbedHTML(final Browser br) {
        if (br == null) {
            return false;
        }
        final String fuid = this.getFUIDFromURL(this.getDownloadLink());
        if (br.containsHTML("/embed-" + fuid + "\\.html")) {
            return true;
        } else if (br.containsHTML("/e/" + fuid)) {
            /* A lot of newer XFS templates got such embed URLs. */
            return true;
        } else if (br.containsHTML("This video can be watched as embed only\\s*<")) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Keep in mind: Most videohosters will allow embedding their videos thus a "video filename" should be enforced but they may also
     * sometimes NOT support embedding videos while a "video filename" should still be enforced - then this trigger might be useful! <br>
     * DO NOT CALL THIS FUNCTION DIRECTLY! Use {@link #internal_isVideohoster_enforce_video_filename()} instead!!
     *
     * @return true: Implies that the hoster only allows video-content to be uploaded. Enforces .mp4 extension for all URLs. Also sets
     *         mime-hint via CompiledFiletypeFilter.VideoExtensions.MP4. <br />
     *         false: Website is just a normal filehost and their filenames should contain the fileextension. <br />
     *         default: false
     */
    protected boolean isVideohoster_enforce_video_filename() {
        return false;
    }

    /**
     * Enable this for websites using <a href="https://sibsoft.net/ximagesharing.html">XImagesharing</a>. <br />
     * Demo-Website: <a href="http://ximagesharing.com">ximagesharing.com</a>
     *
     * @return true: Implies that the hoster only allows photo-content to be uploaded. Enabling this will make plugin try to find
     *         picture-downloadlinks. Also sets mime-hint via CompiledFiletypeFilter.ImageExtensions.JPG. <br />
     *         false: Website is just an usual filehost, use given fileextension if possible. <br />
     *         default: false
     */
    protected boolean isImagehoster() {
        return false;
    }

    /** Indicates that this website is hosting video content only. */
    private boolean isVideohoster(final Browser br) {
        return isVideohosterEmbed() || (br != null && isVideohosterEmbedHTML(br));
    }

    /**
     * See also function {@link #getFilesizeViaAvailablecheckAlt()} ! <br />
     * <b> Enabling this will eventually lead to at least one additional website-request! </b> <br/>
     * <b>DO NOT CALL THIS DIRECTLY, USE {@link #internal_supports_availablecheck_alt()} </b>
     *
     * @return true: Implies that website supports {@link #getFilesizeViaAvailablecheckAlt()} call as an alternative source for
     *         filesize-parsing.<br />
     *         false: Implies that website does NOT support {@link #getFilesizeViaAvailablecheckAlt()}. <br />
     *         default: true
     */
    protected boolean supports_availablecheck_alt() {
        return true;
    }

    /**
     * Only works when {@link #getFilesizeViaAvailablecheckAlt(Browser, DownloadLink))} returns true! See
     * {@link #getFilesizeViaAvailablecheckAlt()}!
     *
     * @return true: Implies that website supports {@link #getFilesizeViaAvailablecheckAlt(Browser, DownloadLink))} call without
     *         Form-handling (one call less than usual) as an alternative source for filesize-parsing. <br />
     *         false: Implies that website does NOT support {@link #getFilesizeViaAvailablecheckAlt(Browser, DownloadLink))} without
     *         Form-handling. <br />
     *         default: true
     */
    protected boolean supports_availablecheck_filesize_alt_fast() {
        return true;
    }

    /**
     * See also function getFilesizeViaAvailablecheckAlt!
     *
     * @return true: Website uses old version of getFilesizeViaAvailablecheckAlt. Old will be tried first, then new if it fails. <br />
     *         false: Website uses current version of getFilesizeViaAvailablecheckAlt - it will be used first and if it fails, old call will
     *         be tried. <br />
     *         2019-07-09: Do not override this anymore - this code will auto-detect this situation!<br/>
     *         default: false
     */
    @Deprecated
    protected boolean prefer_availablecheck_filesize_alt_type_old() {
        return false;
    }

    /**
     * See also function {@link #getFnameViaAbuseLink()}!<br />
     * <b> Enabling this will eventually lead to at least one additional website-request! </b> <br/>
     * DO NOT CALL THIS DIRECTLY - ALWAYS USE {@link #internal_supports_availablecheck_filename_abuse()}!!<br />
     *
     * @return true: Implies that website supports {@link #getFnameViaAbuseLink() } call as an alternative source for filename-parsing. <br />
     *         false: Implies that website does NOT support {@link #getFnameViaAbuseLink()}. <br />
     *         default: true
     */
    protected boolean supports_availablecheck_filename_abuse() {
        return true;
    }

    /**
     * @return true: Try to RegEx filesize from normal html code. If this fails due to static texts on a website or even fake information,
     *         all links of a filehost may just get displayed with the same/wrong filesize. <br />
     *         false: Do not RegEx filesize from normal html code. <br>
     *         Plugin will still be able to find filesize if {@link #supports_availablecheck_alt()} or
     *         {@link #supports_availablecheck_alt_fast()} is enabled (=default)! <br />
     *         default: true
     */
    protected boolean supports_availablecheck_filesize_html() {
        return true;
    }

    /**
     * Video hosts may provide the filesize in bytes inside the so called "sharebox". <br>
     * If this returns false, the filesize will not be obtained from said sharebox.
     */
    protected boolean supports_availablecheck_filesize_bytes_from_sharebox() {
        return true;
    }

    /**
     * This is designed to find the filesize during availablecheck for videohosts based on checking their directURLs! Videohosts usually
     * don't display the filesize anywhere! <br />
     * CAUTION: Only set this to true if a filehost: <br />
     * 1. Allows users to embed videos via '/embed-<fuid>.html'. <br />
     * 2. Does not display a filesize anywhere inside html code or other calls where we do not have to do an http request on a directurl. <br />
     * 3. Allows a lot of simultaneous connections. <br />
     * 4. Is FAST - if it is not fast, this will noticably slow down the linkchecking procedure! <br />
     * 5. Allows using a generated direct-URL at least two times.
     *
     * @return true: {@link #requestFileInformation(DownloadLink)} will use '/embed' to do an additional offline-check and find the
     *         filesize. <br />
     *         false: Disable this.<br />
     *         default: false
     */
    protected boolean supports_availablecheck_filesize_via_embedded_video() {
        return false;
    }

    /**
     * A correct setting increases linkcheck-speed as unnecessary redirects will be avoided. <br />
     * Also in some cases, you may get 404 errors or redirects to other websites if this setting is not correct.
     *
     * @return true: Implies that website requires 'www.' in all URLs. <br />
     *         false: Implies that website does NOT require 'www.' in all URLs. <br />
     *         default: false
     */
    protected boolean requiresWWW() {
        return false;
    }

    /**
     * Use HEAD or GET request for checking directurls? <br>
     * Example HEAD request unsupported: 2022-11-25: no example anymore :(
     *
     * @return default: true
     *
     */
    protected boolean supportsHEADRequestForDirecturlCheck() {
        return true;
    }

    /**
     * Implies that a host supports login via 'API Mod'[https://sibsoft.net/xfilesharing/mods/api.html] via one of these APIs:
     * https://xvideosharing.docs.apiary.io/ OR https://xfilesharingpro.docs.apiary.io/ <br />
     * Enabling this will do the following: <br>
     * - Change login process to accept apikey instead of username & password <br>
     * - Use API for single- and mass linkchecking <br>
     * - Enforce API usage on account downloads: Never download via website, does NOT fallback to website! <br>
     * Sadly, it seems like their linkcheck function often only works for self uploaded conent. <br>
     * API docs: https://xvideosharing.docs.apiary.io/#reference/file/file-info/get-info/check-file(s) <br />
     * 2019-08-20: Some XFS websites are supported via another API via play.google.com/store/apps/details?id=com.zeuscloudmanager --> This
     * has nothing to do with the official XFS API! <br>
     * Example: xvideosharing.com, clicknupload.co <br />
     * default: false
     */
    protected boolean enableAccountApiOnlyMode() {
        return false;
    }

    /** If needed, this can be used to enforce cookie login e.g. if an unsupported captcha type is required for login. */
    protected boolean requiresCookieLogin() {
        return false;
    }

    /** Set this to true if added video embed URLs should be checked only as embed URLs and not as file URLs. */
    protected boolean trustAvailablecheckVideoEmbed() {
        return false;
    }

    protected boolean allowAPIDownloadIfApikeyIsAvailable(final DownloadLink link, final Account account) {
        if (account == null) {
            return false;
        }
        /* Allow download via API if API key is available && download via API is allowed. */
        final boolean apikey_is_available = this.getAPIKeyFromAccount(account) != null;
        if (apikey_is_available && account.hasProperty(PROPERTY_ACCOUNT_ALLOW_API_DOWNLOAD_ATTEMPT_IN_WEBSITE_MODE)) {
            return true;
        } else {
            return false;
        }
    }

    protected boolean allowAPIAvailablecheckInPremiumModeIfApikeyIsAvailable(final Account account) {
        final boolean apikey_is_available = this.getAPIKeyFromAccount(account) != null;
        /* Enable this switch to be able to use this in dev mode. Default = off as we do not use the API by default! */
        final boolean allow_api_availablecheck_in_premium_mode = false;
        return DebugMode.TRUE_IN_IDE_ELSE_FALSE && apikey_is_available && allow_api_availablecheck_in_premium_mode;
    }

    /**
     * If enabled, API will be used to import (public) files into users' account and download them from there. <br>
     * This may sometimes be the only way to download via API because until now (2019-10-31) the XFS API can only be used to download files
     * which the user itself uploaded (= files which are in his account). <br>
     * Warning! The imported files may be PUBLIC as well by default! <br>
     * So far this exists for development purposes ONLY!!
     */
    protected boolean requiresAPIGetdllinkCloneWorkaround(final Account account) {
        /* Enable this switch to be able to use this in dev mode. Default = off as we do not use this workaround by default! */
        final boolean allow_dllink_clone_workaround = false;
        return DebugMode.TRUE_IN_IDE_ELSE_FALSE && allow_dllink_clone_workaround;
    }

    /**
     * @return: Skip pre-download waittime or not. See waitTime function below. <br />
     *          default: false <br />
     *          example true: uploadrar.com
     */
    protected boolean preDownloadWaittimeSkippable() {
        return false;
    }

    /**
     * This is especially useful if a website e.g. provides URLs in this style by default:
     * https://website.com/[a-z0-9]{12}/filename.ext.html --> Then we already have the filename which is perfect as the website mass
     * linkchecker will only return online status (and filesize if the XFS website is up-to-date). <br>
     * You should really only use this if the mass-linkchecker returns filesizes!
     *
     * @default false
     */
    protected boolean supportsMassLinkcheckOverWebsite() {
        return false;
    }

    /**
     * Set this to false if a website is using links that look like short URLs but are not short URLs. <br>
     * Example: streamhide.com, ddownload.com, filoz.net, filespayout.com
     */
    protected boolean supportsShortURLs() {
        return true;
    }

    @Override
    public String getLinkID(DownloadLink link) {
        final String fuid = getFUIDFromURL(link);
        if (fuid != null) {
            return getHost() + "://" + fuid;
        } else {
            return super.getLinkID(link);
        }
    }

    protected boolean isEmbedURL(final DownloadLink link) {
        return isEmbedURLType(getURLType(link));
    }

    protected boolean isEmbedURL(final String url) {
        return isEmbedURLType(getURLType(url));
    }

    protected boolean isEmbedURLType(final URL_TYPE type) {
        return URL_TYPE.EMBED_VIDEO.equals(type) || URL_TYPE.EMBED_VIDEO_2.equals(type);
    }

    protected String buildEmbedURLPath(DownloadLink link, final String fuid) {
        return buildURLPath(link, fuid, URL_TYPE.EMBED_VIDEO);
    }

    protected String buildNormalURLPath(DownloadLink link, final String fuid) {
        return buildURLPath(link, fuid, URL_TYPE.NORMAL);
    }

    protected String buildImageURLPath(DownloadLink link, final String fuid) {
        return buildURLPath(link, fuid, URL_TYPE.IMAGE);
    }

    protected String buildNormalFileURLPath(DownloadLink link, final String fuid) {
        return buildURLPath(link, fuid, URL_TYPE.FILE);
    }

    protected String buildShortURLPath(DownloadLink link, final String fuid) {
        return buildURLPath(link, fuid, URL_TYPE.SHORT);
    }

    protected String buildURLPath(final DownloadLink link, final String fuid, final URL_TYPE type) {
        switch (type) {
        case EMBED_VIDEO:
            return "/embed-" + fuid + ".html";
        case EMBED_VIDEO_2:
            return "/e/" + fuid;
        case FILE:
            return "/file/" + fuid;
        case IMAGE:
            return "/" + fuid;
        case NORMAL:
            return "/" + fuid;
        case SHORT:
            return "/d/" + fuid;
        case OFFICIAL_VIDEO_DOWNLOAD:
            return "/d/" + fuid;
        default:
            throw new IllegalArgumentException("Unsupported type:" + type + "|" + fuid);
        }
    }

    /**
     * Tries to return a "normal" URL path e.g. if the original URL is an embed URL this will return a "normal" URL. <br>
     * Example: original: /e/xxxxxxyyyyyy -> Returns /d/xxxxxxyyyyyy
     */
    protected String getNormalizedDownloadURL(final DownloadLink link) {
        final String fuid = this.getFUIDFromURL(link);
        final String base = this.getMainPage(br);
        if (fuid == null) {
            /* Without a fuid we cannot build URLs. */
            return this.getContentURL(link);
        }
        final URL_TYPE urltype = this.getURLType(link);
        if (urltype == URL_TYPE.EMBED_VIDEO) {
            return base + buildURLPath(link, fuid, URL_TYPE.NORMAL);
        } else if (urltype == URL_TYPE.IMAGE) {
            return base + buildURLPath(link, fuid, URL_TYPE.NORMAL);
        } else if (urltype == URL_TYPE.EMBED_VIDEO_2) {
            return base + buildURLPath(link, fuid, URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD);
        } else {
            /* Do not modify URL-type */
            return this.getContentURL(link);
        }
    }

    /**
     * Returns the desired host. Override is required in some cases where given host can contain unwanted subdomains e.g. imagetwist.com.
     */
    protected String getPreferredHost(final DownloadLink link, URL url) {
        if (isImagehoster()) {
            return getHost();
        } else {
            return url.getHost();
        }
    }

    protected boolean allowGetProtocolHttpsAutoHandling(final String url) {
        return true;
    }

    /**
     * Returns URL to content. <br>
     * Uses original domain whenever possible. <br>
     */
    protected String getContentURL(final DownloadLink link) {
        if (link == null) {
            return null;
        }
        final String originalURL = link.getPluginPatternMatcher();
        if (originalURL == null) {
            return null;
        }
        /* link cleanup, prefer https if possible */
        try {
            final URL url = new URL(originalURL);
            final String urlHost = getPreferredHost(link, url);
            final String urlHostWithoutWww = urlHost.replaceFirst("(?i)www\\.", "");
            final String protocol;
            if (this.useHTTPS()) {
                protocol = "https://";
            } else if ("https".equalsIgnoreCase(url.getProtocol()) && allowGetProtocolHttpsAutoHandling(originalURL)) {
                protocol = "https://";
            } else {
                protocol = "http://";
            }
            /* Get full host with subdomain and correct base domain. */
            final String pluginHost = this.getHost();
            final List<String> deadDomains = this.getDeadDomains();
            final String host;
            if (deadDomains != null && (deadDomains.contains(urlHost) || deadDomains.contains(urlHostWithoutWww))) {
                /* Fallback to plugin domain */
                /* e.g. down.xx.com -> down.yy.com, keep subdomain(s) */
                host = urlHost.replaceFirst("(?i)" + Pattern.quote(Browser.getHost(url, false)) + "$", pluginHost);
            } else {
                /* Use preferred host */
                host = urlHost;
            }
            final String hostCorrected = this.appendWWWIfRequired(host);
            return originalURL.replaceFirst("(?i)^https?://[^/]+", protocol + hostCorrected);
        } catch (final MalformedURLException e) {
            /* Return unmodified url. */
            logger.log(e);
            return originalURL;
        }
    }

    @Override
    public String buildExternalDownloadURL(final DownloadLink link, final PluginForHost buildForThisPlugin) {
        return getContentURL(link);
    }

    @Override
    public Browser prepBrowser(final Browser prepBr, final String host) {
        if (!(this.browserPrepped.containsKey(prepBr) && this.browserPrepped.get(prepBr) == Boolean.TRUE)) {
            super.prepBrowser(prepBr, host);
            /* Define custom browser headers and preferred language */
            prepBr.setCookie(getMainPage(), "lang", "english");
            prepBr.setAllowedResponseCodes(new int[] { 500 });
        }
        return prepBr;
    }

    /**
     * Returns https?://host.tld ATTENTION: On override, make sure that current browsers' host still gets preferred over plugin host. <br>
     * If a subdomain is required, do not use this method before making a browser request!!
     */
    @Deprecated
    protected String getMainPage() {
        return getMainPage(this.br);
    }

    protected String getMainPage(final DownloadLink link) {
        final URL url;
        try {
            url = new URL(link.getPluginPatternMatcher());
        } catch (MalformedURLException e) {
            e.printStackTrace();
            return null;
        }
        final String urlHost = this.getPreferredHost(link, url);
        final List<String> deadDomains = this.getDeadDomains();
        final String domainToUse;
        if (deadDomains != null && deadDomains.contains(urlHost)) {
            domainToUse = this.getHost();
        } else {
            domainToUse = urlHost;
        }
        final String protocol;
        if (this.useHTTPS()) {
            protocol = "https://";
        } else if ("https".equalsIgnoreCase(url.getProtocol()) && allowGetProtocolHttpsAutoHandling(url.toExternalForm())) {
            protocol = "https://";
        } else {
            protocol = "http://";
        }
        final String finalDomainToUse = this.appendWWWIfRequired(domainToUse);
        return protocol + finalDomainToUse;
    }

    /** Adds "www." to given host if it doesn't already contain that and if it doesn't contain any other subdomain. */
    protected String appendWWWIfRequired(final String host) {
        if (!requiresWWW() || StringUtils.startsWithCaseInsensitive(host, "www.")) {
            // do not modify host
            return host;
        } else {
            final String hostTld = Browser.getHost(host, false);
            if (!StringUtils.equalsIgnoreCase(host, hostTld)) {
                // keep subdomain
                return host;
            } else {
                // add www.
                return "www." + host;
            }
        }
    }

    protected String getMainPage(final Browser br) {
        final Request request = br != null ? br.getRequest() : null;
        final String host;
        if (request != null) {
            /* Has a browser request been done before? Use this domain as it could e.g. differ from the plugin set main domain. */
            host = request.getURL().getHost();
        } else {
            /* Return current main domain */
            /* 2019-07-25: This may not be correct out of the box e.g. for imgmaze.com */
            host = this.getHost();
        }
        final String protocol;
        if (this.useHTTPS()) {
            protocol = "https://";
        } else if (request != null && "https".equalsIgnoreCase(request.getURL().getProtocol()) && allowGetProtocolHttpsAutoHandling(request.getUrl())) {
            protocol = "https://";
        } else {
            protocol = "http://";
        }
        final String finalHost = this.appendWWWIfRequired(host);
        return protocol + finalHost;
    }

    /**
     * @return true: Link is password protected <br />
     *         false: Link is not password protected
     */
    public boolean isPasswordProtectedHTML(final Browser br, final Form pwForm) {
        final String pattern = "<br>\\s*<b>\\s*Passwor(d|t)\\s*:\\s*</b>\\s*(<input|</div)";
        boolean ret = br.containsHTML(pattern);
        if (ret) {
            /* Double-check in cleaned HTML */
            ret = new Regex(correctBR(br), pattern).patternFind();
            if (!ret) {
                logger.warning("File is password protected according to html but is not password protected according to cleaned HTML!");
            }
        }
        return ret;
    }

    /**
     * Checks premiumonly status based on current Browser-URL.
     *
     * @return true: Link only downloadable for premium users (sometimes also for registered users). <br />
     *         false: Link is downloadable for all users.
     */
    protected boolean isPremiumOnlyURL(final Browser br) {
        final String url = br != null ? br.getURL() : null;
        if (url == null) {
            return false;
        } else if (StringUtils.containsIgnoreCase(url, "/?op=login&redirect=")) {
            return true;
        } else if (url.matches("(?i).*/login\\?redirect=.*")) {
            /* 2023-11-15 e.g. rapidbytez.com, EzvnNet, TerabytezOrg */
            return true;
        } else {
            final String[] supports_precise_expire_date = this.supportsPreciseExpireDate();
            if (supports_precise_expire_date != null) {
                /* 2025-03-31 eg subyshare, redirects to /?op=payments with blocked? VPN */
                for (final String page : supports_precise_expire_date) {
                    if (StringUtils.containsIgnoreCase(url, page)) {
                        return true;
                    }
                }
            }
            return false;
        }
    }

    /** Returns sanitized(!) error text displayed on website if an account is needed to download specific files. */
    protected String getPremiumOnlyErrorMessage(final Browser br) {
        final List<String> texts = new ArrayList<String>();
        texts.add("The file you requested reached max downloads");
        texts.add("This file reached max downloads");
        texts.add("Available Only for Premium Members");
        texts.add("File is available only for Premium users");
        texts.add("Please Buy Premium To download");
        texts.add("This file is not available for free download");
        texts.add("Only Premium user can download this file");
        /* 2019-05-30: Example: xvideosharing.com */
        texts.add("This video is available for Premium Users only");
        texts.add("This file is available for Premium Users only");
        String msg = null;
        for (final String text : texts) {
            msg = br.getRegex(">(\\s*" + Pattern.quote(text) + "[^<]*)").getMatch(0);
            if (msg != null) {
                break;
            }
        }
        if (msg == null) {
            msg = br.getRegex(">((\\s*Sorry\\s*,)?\\s*This file (can|only can|can only) be downloaded by[^<]+)").getMatch(0);
            if (msg == null) {
                /* class="err">You can download files up to 1 Mb only.<br>Upgrade your account to download bigger files.</div> */
                msg = br.getRegex(">\\s*(You can download files up to \\d+ [^<]*)").getMatch(0);
            }
        }
        if (msg != null) {
            msg = Encoding.htmlDecode(msg).trim();
            return msg;
        }
        return null;
    }

    /**
     * Checks premiumonly status via current Browser-HTML AND URL via isPremiumOnlyURL.
     *
     * @return true: Link only downloadable for premium users (sometimes also for registered users). <br />
     *         false: Link is downloadable for all users.
     */
    public boolean isPremiumOnly(final Browser br) {
        if (isPremiumOnlyURL(br)) {
            return true;
        }
        final String msg = getPremiumOnlyErrorMessage(br);
        if (msg != null) {
            return true;
        }
        return false;
    }

    /**
     * @return true: Downloadserver is in maintenance mode - downloads are not possible but linkcheck may be possible. <br />
     *         false: Downloadserver is not in maintenance mode and should be possible.
     */
    protected boolean isServerUnderMaintenance(final Browser br) {
        return br.getHttpConnection().getResponseCode() == 500 || br.containsHTML(">\\s*This server is in maintenance mode");
    }

    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.getHttpConnection().getResponseCode() == 404) {
            return true;
        }
        if (br.containsHTML("No such file")) {
            return true;
        } else if (br.containsHTML(">\\s*File Not Found\\s*<")) {
            return true;
        } else if (br.containsHTML(">\\s*The file was removed by")) {
            return true;
        } else if (br.containsHTML(">\\s*Reason for deletion:?")) {
            return true;
        } else if (br.containsHTML(">\\s*The file expired")) {
            return true;
        } else if (br.containsHTML(">\\s*Sorry, we can't find the page you're looking for")) {
            return true;
        } else if (br.containsHTML(">\\s*File could not be found due to expiration or removal by the file owner")) {
            return true;
        } else if (br.containsHTML(">\\s*The file of the above link no longer exists")) {
            return true;
        } else if (br.containsHTML(">\\s*video you are looking for is not found")) {
            return true;
        } else if (br.containsHTML(">\\s*The file you were looking for doesn't exist")) {
            return true;
        } else if (br.containsHTML(">\\s*File is no longer available as it")) {
            return true;
        } else {
            return false;
        }
    }

    /** Returns true if a special referer is needed to view/watch this item. */
    private boolean isRefererBlocked(final Browser br) {
        return br.containsHTML(">\\s*This video cannot be watched under this domain");
    }

    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        final String apiKey = this.getAPIKey();
        if (enableAccountApiOnlyMode() || (this.looksLikeValidAPIKey(apiKey) && this.supportsAPIMassLinkcheck())) {
            return massLinkcheckerAPI(urls, apiKey);
        } else if (supportsMassLinkcheckOverWebsite()) {
            return this.massLinkcheckerWebsite(urls);
        } else {
            /* No mass linkchecking possible */
            return false;
        }
    }

    @Override
    public boolean enoughTrafficFor(final DownloadLink link, final Account account) throws Exception {
        final String dllink = getStoredDirectUrl(link, account);
        /*
         * E.g. account doesn't have enough traffic left but we still got a stored directurl from a previous downloadstart --> Allow
         * download attempt anyways as we can be quite sure that it will still be valid.
         */
        if (StringUtils.isNotEmpty(dllink)) {
            return true;
        } else {
            return super.enoughTrafficFor(link, account);
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, null);
    }

    protected AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        final String apikey = getAPIKey();
        if (this.supportsAPISingleLinkcheck() && apikey != null) {
            /* API linkcheck */
            return this.requestFileInformationAPI(link, apikey);
        } else {
            /* Website linkcheck */
            return requestFileInformationWebsite(link, null);
        }
    }

    @Override
    protected boolean looksLikeDownloadableContent(final URLConnectionAdapter urlConnection) {
        looksLike: if (!super.looksLikeDownloadableContent(urlConnection)) {
            final long verifiedFileSize = getDownloadLink().getVerifiedFileSize();
            if ((urlConnection.getResponseCode() == 200 || urlConnection.getResponseCode() == 206) && verifiedFileSize > 0 && verifiedFileSize == urlConnection.getCompleteContentLength()) {
                /*
                 * we assume downloadable content due to good response code and complete content length matches our known verified file size
                 */
                break looksLike;
            }
            return false;
        }
        try {
            /* Check if response is plaintext and contains any known error messages. */
            final byte[] probe = urlConnection.peek(32);
            if (probe.length > 0) {
                final String probeContext = new String(probe, "UTF-8");
                final Request clone = urlConnection.getRequest().cloneRequest();
                clone.setHtmlCode(probeContext);
                final Browser br = createNewBrowserInstance();
                br.setRequest(clone);
                try {
                    // TODO: extract the html checks into own method to avoid Browser instance
                    checkServerErrors(br, getDownloadLink(), null);
                } catch (PluginException e) {
                    logger.log(e);
                    return false;
                }
            }
        } catch (IOException e) {
            logger.log(e);
        }
        return true;
    }

    protected boolean probeDirectDownload(final DownloadLink link, final Account account, final Browser br, final Request request, final boolean setFilesize) throws Exception {
        request.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING, "identity");
        final String referer = this.getReferer(link);
        if (referer != null) {
            request.getHeaders().put(HTTPConstants.HEADER_REQUEST_REFERER, referer);
        }
        URLConnectionAdapter con = null;
        try {
            con = openAntiDDoSRequestConnection(br, request);
            if (this.looksLikeDownloadableContent(con)) {
                final long completeContentLength = con.getCompleteContentLength();
                if (setFilesize && completeContentLength > 0) {
                    if (con.isContentDecoded()) {
                        link.setDownloadSize(completeContentLength);
                    } else {
                        link.setVerifiedFileSize(completeContentLength);
                    }
                }
                final String connectionFilename = Plugin.getFileNameFromConnection(con);
                if (!StringUtils.isEmpty(connectionFilename)) {
                    link.setFinalFileName(connectionFilename);
                }
                storeDirecturl(link, account, con.getURL().toExternalForm());
                return true;
            } else {
                br.followConnection();
                runPostRequestTask(br);
                correctBR(br);
                return false;
            }
        } finally {
            if (con != null) {
                con.disconnect();
            }
        }
    }

    /**
     * Handling for older / standard XFS links for examle: <br>
     * https://xfswebsite.tld/[a-z0-9]{12} <br>
     * https://xfswebsite.tld/embed-[a-z0-9]{12}
     */
    public AvailableStatus requestFileInformationWebsiteXFSOld(final DownloadLink link, final Account account) throws Exception {
        final boolean isDownload = PluginEnvironment.DOWNLOAD.equals(getPluginEnvironment());
        final String contentURL = this.getContentURL(link);
        if (probeDirectDownload(link, account, br, br.createGetRequest(this.getContentURL(link)), true)) {
            return AvailableStatus.TRUE;
        }
        if (this.supportsShortURLs()) {
            resolveShortURL(br, link, account);
        }
        final URL_TYPE urltype = this.getURLType(br.getURL());
        // boolean looksLikeOffline = false;
        PluginException embedException = null;
        if (urltype == URL_TYPE.EMBED_VIDEO) {
            try {
                hasCheckedEmbedHandling = true;
                this.requestFileInformationVideoEmbedXFSOld(br, link, account, true);
                if (trustAvailablecheckVideoEmbed()) {
                    return AvailableStatus.TRUE;
                }
            } catch (final PluginException e) {
                embedException = e;
                logger.log(e);
            }
        }
        /* Now check normalized link */
        final String fuid = this.getFUIDFromURL(link);
        if (fuid == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String url;
        if (urltype == URL_TYPE.NORMAL || urltype == URL_TYPE.SHORT) {
            url = this.getContentURL(link);
        } else {
            url = this.getMainPage(br) + this.buildURLPath(link, fuid, URL_TYPE.NORMAL);
        }
        if (!StringUtils.equals(url, contentURL)) {
            /* Check for direct-download if we haven't already done this before. */
            if (probeDirectDownload(link, account, br, br.createGetRequest(url), true)) {
                return AvailableStatus.TRUE;
            }
        }
        if (isOffline(link, this.br)) {
            if (embedException != null) {
                throw embedException;
            } else if (hasCheckedEmbedHandling) {
                logger.info("Item looks to be online according to embed check but offline according to official video download handling");
                return AvailableStatus.TRUE;
            } else {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        }
        final String[] fileInfo = internal_getFileInfoArray();
        final Browser altbr = br.cloneBrowser();
        if (isPremiumOnlyURL(this.br)) {
            /*
             * Hosts whose urls are all premiumonly usually don't display any information about the URL at all - only maybe online/offline.
             * There are 2 alternative ways to get this information anyways!
             */
            logger.info("PREMIUMONLY linkcheck: Trying alternative linkcheck");
            /* Find filename */
            if (this.internal_supports_availablecheck_filename_abuse()) {
                fileInfo[0] = this.getFnameViaAbuseLink(altbr, link);
            }
            /* Find filesize */
            if (this.internal_supports_availablecheck_alt()) {
                getFilesizeViaAvailablecheckAlt(altbr, link);
            }
        } else {
            /* Normal handling */
            scanInfo(fileInfo);
            /**
             * Two possible reasons to use fallback handling to find filename: <br>
             * 1. Filename abbreviated over x chars long (common serverside XFS bug) --> Use getFnameViaAbuseLink as a workaround to find
             * the full-length filename! <br>
             * 2. Missing filename.
             */
            if (!StringUtils.isEmpty(fileInfo[0]) && fileInfo[0].trim().endsWith("&#133;") && this.internal_supports_availablecheck_filename_abuse() && !hasCheckedEmbedHandling) {
                logger.warning("Found filename is crippled by website -> Looking for full length filename");
                final String betterFilename = this.getFnameViaAbuseLink(altbr, link);
                if (betterFilename != null) {
                    logger.info("Found full length filename: " + betterFilename);
                    fileInfo[0] = betterFilename;
                } else {
                    logger.info("Failed to find full length filename");
                }
            } else if (StringUtils.isEmpty(fileInfo[0]) && this.internal_supports_availablecheck_filename_abuse() && !hasCheckedEmbedHandling) {
                /* We failed to find the filename via html --> Try getFnameViaAbuseLink as workaround */
                logger.info("Failed to find any filename, trying to obtain filename via getFnameViaAbuseLink");
                final String betterFilename = this.getFnameViaAbuseLink(altbr, link);
                if (betterFilename != null) {
                    logger.info("Found filename: " + betterFilename);
                    fileInfo[0] = betterFilename;
                } else {
                    logger.info("Failed to find any filename -> Fallback will be used");
                }
            }
            /* Filesize fallback */
            if (StringUtils.isEmpty(fileInfo[1]) && this.internal_supports_availablecheck_alt()) {
                /* Failed to find filesize? Try alternative way! */
                getFilesizeViaAvailablecheckAlt(altbr, link);
            }
        }
        processFileInfo(fileInfo, altbr, link);
        if (!StringUtils.isEmpty(fileInfo[0])) {
            /* Correct- and set filename */
            setFilename(fileInfo[0], link, br);
        } else {
            /*
             * Fallback. Do this again as now we got the html code available so we can e.g. know if this is a video-filehoster or not.
             */
            this.setWeakFilename(link, br);
        }
        /* Set filesize */
        if (!StringUtils.isEmpty(fileInfo[1])) {
            link.setDownloadSize(SizeFormatter.getSize(fileInfo[1]));
        } else if (!link.isSizeSet() && this.internal_isVideohosterEmbed(this.br) && supports_availablecheck_filesize_via_embedded_video() && !isDownload && urltype != URL_TYPE.EMBED_VIDEO) {
            /*
             * Special case for some videohosts to determine the filesize: Last chance to find filesize - do NOT execute this when used has
             * started the download of our current DownloadLink as this could lead to "Too many connections" errors!
             */
            requestFileInformationVideoEmbedXFSOld(br.cloneBrowser(), link, account, true);
        }
        /* Set md5hash - most times there is no md5hash available! */
        if (!StringUtils.isEmpty(fileInfo[2])) {
            link.setMD5Hash(fileInfo[2].trim());
        }
        return AvailableStatus.TRUE;
    }

    /**
     * 2019-05-15: This can check availability via '/embed-[a-z0-9]{12}' URL. <br />
     * Only call this if internal_isVideohosterEmbed returns true. <br>
     *
     * @return final downloadurl
     */
    protected void requestFileInformationVideoEmbedXFSOld(final Browser br, final DownloadLink link, final Account account, final boolean findAndSetFilesize) throws Exception {
        /*
         * Some video sites contain their directurl right on the first page - let's use this as an indicator and assume that the file is
         * online if we find a directurl. This also speeds-up linkchecking! Example: uqload.com
         */
        final String fid = this.getFUIDFromURL(link);
        final URL_TYPE currentBrowserURLType = this.getURLType(br.getURL());
        if (currentBrowserURLType != URL_TYPE.EMBED_VIDEO) {
            /* URL needs to be accessed */
            final URL_TYPE type = this.getURLType(link);
            final String url;
            if (type == URL_TYPE.EMBED_VIDEO) {
                url = this.getContentURL(link);
            } else {
                url = this.getMainPage(br) + this.buildURLPath(link, fid, URL_TYPE.EMBED_VIDEO);
            }
            getPage(br, url);
        }
        /* Special embed video offline check */
        if (br.getRequest().getHtmlCode().equalsIgnoreCase("File was deleted")) {
            /* Should be valid for all XFS hosts e.g. speedvideo.net, uqload.com */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (isOffline(link, br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        this.checkErrors(br, br.getRequest().getHtmlCode(), link, account, false);
        final String dllink = getDllink(link, account, br, getCorrectBR(br));
        boolean isFilesizeSet = false;
        if (!StringUtils.isEmpty(dllink)) {
            this.videoStreamDownloadurl = dllink;
            if (findAndSetFilesize && !dllink.contains(".m3u8")) {
                /* Get- and set filesize from directurl */
                if (checkDirectLinkAndSetFilesize(link, dllink, true) != null) {
                    /* Directurl is valid -> Store it */
                    storeDirecturl(link, account, dllink);
                    isFilesizeSet = true;
                }
            }
        }
        final String[] fileInfo = internal_getFileInfoArray();
        scanInfo(fileInfo);
        processFileInfo(fileInfo, br, link);
        if (!StringUtils.isEmpty(fileInfo[0])) {
            /* Correct- and set filename */
            setFilename(fileInfo[0], link, br);
        } else {
            /* Fallback */
            this.setWeakFilename(link, br);
        }
        /* Set filesize */
        if (!StringUtils.isEmpty(fileInfo[1]) && !isFilesizeSet) {
            link.setDownloadSize(SizeFormatter.getSize(fileInfo[1]));
        }
    }

    protected static final String PROPERTY_REFERER_REQUIRED = "PROPERTY_REFERER_REQUIRED";

    protected String isRefererRequired(DownloadLink link) throws PluginException {
        final Object ret = link.getProperty(PROPERTY_REFERER_REQUIRED);
        if (ret == null) {
            return null;
        } else if (ret instanceof Boolean) {
            return ret.toString();
        } else if ("no".equals(ret) || "bypassEmbed".equals(ret)) {
            return (String) ret;
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported value:" + ret);
        }
    }

    /** For XFS websites with "/e/" embed URLs and "/d/" official download URLs. */
    public AvailableStatus requestFileInformationWebsiteXFSNew(final DownloadLink link, final Account account) throws Exception {
        final boolean isDownload = PluginEnvironment.DOWNLOAD.equals(getPluginEnvironment());
        PluginException embedException = null;
        final URL_TYPE urltype = this.getURLType(link);
        final String isRefererRequired = isRefererRequired(link);
        if (urltype == URL_TYPE.EMBED_VIDEO_2) {
            embed_video: try {
                if ("bypassEmbed".equals(isRefererRequired)) {
                    break embed_video;
                }
                hasCheckedEmbedHandling = true;
                final boolean isChecked;
                if (!isDownload) {
                    this.requestFileInformationVideoEmbedXFSNew(br, link, account, false, true);
                    isChecked = isRefererRequired != null || !isRefererBlocked(br);
                } else {
                    if (isRefererRequired != null) {
                        this.requestFileInformationVideoEmbedXFSNew(br, link, account, true, true);
                        isChecked = true;
                    } else {
                        this.requestFileInformationVideoEmbedXFSNew(br, link, account, false, true);
                        isChecked = !isRefererBlocked(br);
                    }
                }
                if (!link.hasProperty(PROPERTY_REFERER_REQUIRED)) {
                    link.setProperty(PROPERTY_REFERER_REQUIRED, isRefererBlocked(br));
                }
                if (trustAvailablecheckVideoEmbed() && isChecked && isRefererRequired != null) {
                    return AvailableStatus.TRUE;
                }
            } catch (final PluginException e) {
                embedException = e;
                logger.log(e);
            }
        }
        final String fuid = this.getFUIDFromURL(link);
        final String url;
        if (urltype == URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD) {
            url = this.getContentURL(link);
        } else {
            url = this.getMainPage(br) + this.buildURLPath(link, fuid, URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD);
        }
        if (probeDirectDownload(link, account, br, br.createGetRequest(url), true)) {
            return AvailableStatus.TRUE;
        } else if (isOffline(link, this.br)) {
            if (!CompareUtils.equals(isRefererRequired, isRefererRequired(link))) {
                return requestFileInformationWebsiteXFSNew(link, account);
            } else if (embedException != null) {
                throw embedException;
            } else if (hasCheckedEmbedHandling) {
                logger.info("Item looks to be online according to embed check but offline according to official video download handling");
                return AvailableStatus.TRUE;
            } else {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        } else if (isRefererRequired == null) {
            if ("true".equals(isRefererRequired(link))) {
                link.setProperty(PROPERTY_REFERER_REQUIRED, "bypassEmbed");
            } else {
                link.setProperty(PROPERTY_REFERER_REQUIRED, "no");
            }
        }
        // this.checkErrors(br, url, link, account, false);
        final String[] fileInfo = internal_getFileInfoArray();
        scanInfo(fileInfo);
        processFileInfo(fileInfo, br, link);
        if (!StringUtils.isEmpty(fileInfo[0])) {
            /* Correct- and set filename */
            setFilename(fileInfo[0], link, br);
        } else {
            /*
             * Fallback. Do this again as now we got the html code available so we can e.g. know if this is a video-filehoster or not.
             */
            this.setWeakFilename(link, br);
        }
        /* Set filesize */
        if (!StringUtils.isEmpty(fileInfo[1])) {
            link.setDownloadSize(SizeFormatter.getSize(fileInfo[1]));
        }
        return AvailableStatus.TRUE;
    }

    protected void requestFileInformationVideoEmbedXFSNew(final Browser br, final DownloadLink link, final Account account, final boolean isDownload, final boolean findAndSetFilesize) throws Exception {
        /*
         * Some video sites contain their directurl right on the first page - let's use this as an indicator and assume that the file is
         * online if we find a directurl. This also speeds-up linkchecking! Example: uqload.com
         */
        final String fid = this.getFUIDFromURL(link);
        final URL_TYPE type = this.getURLType(link);
        final String url;
        if (type == URL_TYPE.EMBED_VIDEO_2) {
            url = this.getContentURL(link);
        } else {
            url = this.getMainPage(br) + this.buildURLPath(link, fid, URL_TYPE.EMBED_VIDEO_2);
        }
        final String refererSaved = this.getReferer(link);
        if (refererSaved != null) {
            br.getHeaders().put(HTTPConstants.HEADER_REQUEST_REFERER, refererSaved);
        }
        getPage(br, url);
        if (this.isRefererBlocked(br)) {
            if (isDownload) {
                final String referer = getUserInput("Enter referer-URL?", link);
                br.getHeaders().put(HTTPConstants.HEADER_REQUEST_REFERER, referer);
                /* Reload page */
                getPage(br.getURL());
                if (!this.isRefererBlocked(br)) {
                    /* Success */
                    link.setDownloadPassword(referer);
                } else {
                    /* Failure */
                    link.setDownloadPassword(null);
                }
            } else {
                /* Problem happened during availablecheck -> Do not ask user for password/referer. */
                return;
            }
        }
        this.checkErrors(br, url, link, account, false);
        if (isOffline(link, br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String dllink = getDllink(link, account, br, getCorrectBR(br));
        if (!StringUtils.isEmpty(dllink)) {
            this.videoStreamDownloadurl = dllink;
            if (findAndSetFilesize && !dllink.contains(".m3u8")) {
                /* Get- and set filesize from directurl */
                if (checkDirectLinkAndSetFilesize(link, dllink, true) != null) {
                    /* Directurl is valid -> Store it */
                    storeDirecturl(link, account, dllink);
                }
            }
        }
    }

    protected boolean isXFSOld(final URL_TYPE urltype) {
        if (urltype == null || urltype == URL_TYPE.IMAGE || urltype == URL_TYPE.NORMAL || urltype == URL_TYPE.EMBED_VIDEO || urltype == URL_TYPE.SHORT || urltype == URL_TYPE.FILE) {
            /* Old XFS */
            return true;
        } else {
            /* New XFS */
            return false;
        }
    }

    public AvailableStatus requestFileInformationWebsite(final DownloadLink link, final Account account) throws Exception {
        /* Set fallback-filename */
        if (!link.isNameSet()) {
            setWeakFilename(link, null);
        }
        final URL_TYPE urltype = this.getURLType(link);
        if (isXFSOld(urltype)) {
            return requestFileInformationWebsiteXFSOld(link, account);
        } else {
            /* New XFS handling */
            return requestFileInformationWebsiteXFSNew(link, account);
        }
    }

    protected String removeHostNameFromFilename(final String filename) {
        String ret = filename;
        if (StringUtils.isNotEmpty(ret)) {
            final LinkedHashSet<String> hosts = new LinkedHashSet<String>();
            hosts.add(getHost());
            final String[] siteSupportedNames = siteSupportedNames();
            if (siteSupportedNames != null) {
                hosts.addAll(Arrays.asList(siteSupportedNames));
            }
            for (final String host : hosts) {
                final String host_tag = new Regex(ret, Pattern.compile("(_?" + Pattern.quote(host) + ")", Pattern.CASE_INSENSITIVE)).getMatch(0);
                if (host_tag != null) {
                    ret = ret.replace(host_tag, "");
                }
            }
        }
        return ret;
    }

    /**
     * Wrapper. <br>
     * Does some corrections on given name string and sets it as filename on given DownloadLink.
     */
    protected void setFilename(String name, final DownloadLink link, final Browser br) {
        if (StringUtils.isEmpty(name)) {
            return;
        }
        /* Correct- and set filename */
        if (Encoding.isHtmlEntityCoded(name)) {
            name = Encoding.htmlDecode(name);
        }
        /* Remove some html tags - in most cases not necessary! */
        name = name.replaceAll("(</b>|<b>|\\.html)", "").trim();
        if (this.internal_isVideohoster_enforce_video_filename(link, br)) {
            /* For videohosts we often get ugly filenames such as 'some_videotitle.avi.mkv.mp4' --> Correct that! */
            name = this.applyFilenameExtension(name, ".mp4");
        }
        link.setName(name);
    }

    protected void processFileInfo(String[] fileInfo, Browser altbr, DownloadLink link) {
    }

    protected boolean isShortURL(final DownloadLink link) {
        return URL_TYPE.SHORT.equals(getURLType(link));
    }

    protected URL_TYPE getURLType(final DownloadLink link) {
        return link != null ? getURLType(link.getPluginPatternMatcher()) : null;
    }

    private static final Pattern PATTERN_SHORTURL = Pattern.compile("/d/([A-Za-z0-9]+)", Pattern.CASE_INSENSITIVE);

    protected URL_TYPE getURLType(final String url) {
        if (url == null) {
            return null;
        }
        final String shorturlID = this.supportsShortURLs() ? new Regex(url, PATTERN_SHORTURL).getMatch(0) : null;
        if (isImagehoster() && url.matches("(?i)^https?://[^/]+/(?:th|i)/\\d+/([a-z0-9]{12}).*")) {
            return URL_TYPE.IMAGE;
        } else if (shorturlID != null && (shorturlID.length() < 12)) {
            return URL_TYPE.SHORT;
        } else if (url.matches("(?i)^https?://[^/]+/d/([a-z0-9]{12}).*")) {
            return URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD;
        } else if (url.matches("(?i)^https?://[^/]+/([a-z0-9]{12}).*")) {
            return URL_TYPE.NORMAL;
        } else if (url.matches("(?i)^https?://[^/]+/file/([a-z0-9]{12}).*")) {
            return URL_TYPE.FILE;
        } else if (url.matches("(?i)^https?://[A-Za-z0-9\\-\\.:]+/embed-([a-z0-9]{12}).*")) {
            return URL_TYPE.EMBED_VIDEO;
        } else if (url.matches("(?i)^https?://[A-Za-z0-9\\-\\.:]+/e/([a-z0-9]{12}).*")) {
            return URL_TYPE.EMBED_VIDEO_2;
        } else {
            logger.info("Unknown URL_TYPE: " + url);
            return null;
        }
    }

    protected String getFUID(final String url, URL_TYPE type) {
        if (url == null || type == null) {
            return null;
        }
        try {
            final String path = new URL(url).getPath();
            switch (type) {
            case IMAGE:
                if (isImagehoster()) {
                    return new Regex(path, "/(?:th|i)/\\d+/([a-z0-9]{12})").getMatch(0);
                } else {
                    throw new IllegalArgumentException("Unsupported type:" + type + "|" + url);
                }
            case EMBED_VIDEO:
                return new Regex(path, "(?i)/embed-([a-z0-9]{12})").getMatch(0);
            case EMBED_VIDEO_2:
                return new Regex(path, "(?i)/e/([a-z0-9]{12})").getMatch(0);
            case FILE:
                return new Regex(path, "(?i)/file/([a-z0-9]{12})").getMatch(0);
            case SHORT:
                return new Regex(path, PATTERN_SHORTURL).getMatch(0);
            case OFFICIAL_VIDEO_DOWNLOAD:
                return new Regex(path, "(?i)/d/([a-z0-9]{12})").getMatch(0);
            case NORMAL:
                return new Regex(path, "/([a-z0-9]{12})").getMatch(0);
            default:
                throw new IllegalArgumentException("Unsupported type:" + type + "|" + url);
            }
        } catch (MalformedURLException e) {
            logger.log(e);
        }
        return null;
    }

    protected String getFUID(final DownloadLink link, final URL_TYPE type) {
        return link != null ? getFUID(link.getPluginPatternMatcher(), type) : null;
    }

    /**
     * Handles URLs matching TYPE_SHORTURL and ensures that we get one of TYPE_NORMAL (or Exception). <br>
     * There are multiple reasons for us to handle this here instead of using a separate crawler plugin. Do NOT move this handling into a
     * separate crawler plugin!! <br>
     * Examples: cosmobox.org, ddownload.com
     */
    protected void resolveShortURL(final Browser br, final DownloadLink link, final Account account) throws Exception {
        synchronized (link) {
            if (!supportsShortURLs()) {
                return;
            } else if (!isShortURL(link)) {
                /* Do nothing */
                return;
            }
            final String contentURL = this.getContentURL(link);
            /* Short URLs -> We need to find the long FUID! */
            /* Check if the URL we want has already been accessed with given browser instance */
            if (br.getURL() == null || !br.getURL().equals(contentURL)) {
                if (probeDirectDownload(link, account, br, br.createGetRequest(contentURL), true)) {
                    return;
                }
            }
            if (this.isOffline(link, br)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            URL_TYPE type = getURLType(br.getURL());
            final String realFUID;
            if (type != null && !URL_TYPE.SHORT.equals(type)) {
                logger.info("Type of current URL is not a short URL anymore -> Try to extract file_id from URL");
                realFUID = getFUID(br.getURL(), type);
            } else {
                final Form form = br.getFormbyProperty("name", "F1");
                final InputField id = form != null ? form.getInputFieldByName("id") : null;
                realFUID = id != null ? id.getValue() : null;
                type = URL_TYPE.NORMAL;
            }
            if (realFUID == null || !realFUID.matches("[A-Za-z0-9]{12}")) {
                /* Failure */
                /**
                 * The usual XFS errors can happen here in which case we won't be able to find the long FUID. <br>
                 * Even while a limit is reached, such URLs can sometimes be checked via: "/?op=check_files" but we won't do this for now!
                 */
                this.checkErrors(br, br.getRequest().getHtmlCode(), link, account, false);
                /* Assume that this URL is offline */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "realFUID:" + realFUID);
            }
            /* Success! */
            final String urlNew;
            if (URL_TYPE.FILE.equals(type)) {
                urlNew = URLHelper.parseLocation(new URL(this.getMainPage(link)), buildNormalFileURLPath(link, realFUID));
            } else {
                urlNew = URLHelper.parseLocation(new URL(this.getMainPage(link)), buildNormalURLPath(link, realFUID));
            }
            logger.info("resolve URL|old: " + contentURL + "|new:" + urlNew);
            link.setPluginPatternMatcher(urlNew);
        }
    }

    /**
     * Tries to find filename, filesize and md5hash inside html. On Override, make sure to first use your special RegExes e.g.
     * fileInfo[0]="bla", THEN, if needed, call super.scanInfo(fileInfo). <br />
     * fileInfo[0] = filename, fileInfo[1] = filesize, fileInfo[2] = md5hash
     */
    public String[] scanInfo(final String[] fileInfo) {
        return scanInfo(getCorrectBR(br), fileInfo);
    }

    public String[] scanInfo(final String html, final String[] fileInfo) {
        final DownloadLink link = this.getDownloadLink();
        final String urlFUID = this.getFUIDFromURL(link);
        String quotedFUID = null;
        if (urlFUID != null) {
            quotedFUID = Pattern.quote(urlFUID);
        }
        String filename = new Regex(html, "name=\"fname\"[^>]*value=\"([^\"]+)\"").getMatch(0);
        if (StringUtils.isEmpty(filename)) {
            /* 2024-04-23: e.g. oceanoffile.com */
            filename = new Regex(html, "class\\s*=\\s*\"dfilename\"[^>]*>\\s*(?:<div>)?\\s*([^<>\"]*?)</").getMatch(0);
            if (StringUtils.isEmpty(filename)) {
                filename = new Regex(html, "<div[^>]*id\\s*=\\s*\"dfilename\"[^>]*>\\s*(?:<div>)?\\s*([^<>\"]*?)</").getMatch(0);
            }
        }
        boolean preferRoughFilesize = false;
        String filesizeBytesStr = null;
        String filesizeWithUnit = null;
        /* 2020-08-10: E.g. myqloud.org */
        try {
            filesizeWithUnit = getDllinkViaOfficialVideoDownload(this.br.cloneBrowser(), link, null, true);
            if (filesizeWithUnit != null) {
                /* File size from official video download handling can be considered a "safe source" */
                preferRoughFilesize = true;
            }
        } catch (final Throwable e) {
            logger.log(e);
        }
        {
            /* New sharebox handling 2024-04-10 */
            /* Look for textarea with special copy-able URLs which will sometimes contain information about filename/size. */
            final String[] textareas = new Regex(html, "(?i)<textarea[^>]*>(.*?)</textarea>").getColumn(0);
            /* E.g. brupload.net, apkadmin.com, send.cm */
            final Pattern pattern_shareboxWithFilenameAndFilesizeBytes = quotedFUID != null ? Pattern.compile(quotedFUID + "(?:\\.html)?(?:/[^\\]]+)?\\]([^\"]+) - \\s*(\\d+)\\[/URL\\]") : null;
            if (textareas != null && textareas.length > 0) {
                for (final String textarea : textareas) {
                    if (filename == null) {
                        /* Filename from URL */
                        if (quotedFUID != null) {
                            final Regex targetUrlWithFilename = new Regex(textarea, "(?i)^https?://[^/]+/" + quotedFUID + "(?:\\.html)?/" + "([^/#?]+)(\\.html)?$");
                            if (targetUrlWithFilename.patternFind()) {
                                filename = targetUrlWithFilename.getMatch(0);
                            }
                        }
                        if (filename == null) {
                            /* e.g. xxembed.com */
                            filename = new Regex(textarea, "(?i)\\[URL=[^\\]]+\\]\\[IMG\\][^\\[]+\\[/IMG\\](.*?)\\[/URL\\]").getMatch(0);
                            if (filename == null && !supports_availablecheck_filesize_bytes_from_sharebox()) {
                                filename = new Regex(textarea, "\\](.*?)\\[/URL\\]").getMatch(0);
                            }
                            if (filename == null) {
                                /* Video image thumbnail item: Forum code: Link + Thumbnail as URL with title as text. */
                                filename = new Regex(textarea, "(?i)\\[URL=[^\\]]+\\]\\[IMG\\][^\\[]+\\[/IMG\\](.*?)\\[/URL\\]").getMatch(0);
                            }
                        }
                    }
                    if ((filename == null || filesizeBytesStr == null) && pattern_shareboxWithFilenameAndFilesizeBytes != null) {
                        final Regex shareboxWithFilesizeBytesRegex = new Regex(textarea, pattern_shareboxWithFilenameAndFilesizeBytes);
                        if (filename == null) {
                            filename = shareboxWithFilesizeBytesRegex.getMatch(0);
                        }
                        if (filesizeBytesStr == null) {
                            filesizeBytesStr = shareboxWithFilesizeBytesRegex.getMatch(1);
                        }
                    }
                    if (filesizeWithUnit == null) {
                        filesizeWithUnit = new Regex(textarea, "(?i)([\\d\\.]+ (?:B|KB|MB|GB))(\\[/URL\\]|</a>)").getMatch(0);
                    }
                    if (filename != null && filesizeWithUnit != null) {
                        /* We found all needed info -> No need to continue. */
                        break;
                    }
                }
            }
            if ((filename == null || filesizeBytesStr == null) && pattern_shareboxWithFilenameAndFilesizeBytes != null) {
                /* E.g. send.cm */
                final Regex shareboxWithFilesizeBytesRegex = br.getRegex(pattern_shareboxWithFilenameAndFilesizeBytes);
                if (filename == null) {
                    filename = shareboxWithFilesizeBytesRegex.getMatch(0);
                }
                if (filesizeBytesStr == null) {
                    filesizeBytesStr = shareboxWithFilesizeBytesRegex.getMatch(1);
                }
            }
            // if (filename != null) {
            // if (filesizeWithUnit != null) {
            // filename = filename.replaceFirst(Pattern.quote(filesizeWithUnit) + "$", "");
            // }
            // }
            if (!supports_availablecheck_filesize_bytes_from_sharebox() && filesizeBytesStr != null) {
                logger.info("Ignoring this possible filesize_bytes string: " + filesizeBytesStr);
                filesizeBytesStr = null;
            }
        }
        /* Standard traits from base page */
        if (StringUtils.isEmpty(filename)) {
            filename = new Regex(html, "(?i)You have requested.*?https?://(?:www\\.)?[^/]+/" + urlFUID + "/([^<>\"]+)<").getMatch(0);
            if (StringUtils.isEmpty(filename)) {
                filename = new Regex(html, "<h2>\\s*Download File\\s*(?:<(?:span|b)[^>]*>)?\\s*(.+?)\\s*(</(?:span|b|h2)>)").getMatch(0);
                /* traits from download1 page below */
                if (StringUtils.isEmpty(filename)) {
                    filename = new Regex(html, "Filename:?\\s*(<[^>]+>\\s*)+?([^<>\"]+)").getMatch(1);
                }
            }
        }
        if (StringUtils.isEmpty(filename) || StringUtils.isAllEmpty(filesizeWithUnit, filesizeBytesStr)) {
            // eg rarefile.net
            final String downloadFileTable = new Regex(html, "<h\\d+>\\s*Download\\s*File\\s*</h\\d+>\\s*<table[^>]*>(.*?)</table>").getMatch(0);
            if (downloadFileTable != null) {
                if (StringUtils.isEmpty(filename)) {
                    filename = new Regex(downloadFileTable, "<td>\\s*<font[^>]*>\\s*(.*?)\\s*</font>").getMatch(0);
                }
                if (StringUtils.isAllEmpty(filesizeWithUnit, filesizeBytesStr)) {
                    filesizeWithUnit = new Regex(downloadFileTable, ">\\s*Size\\s*:\\s*([0-9\\.]+\\s*(TB|GB|MB|KB|B))").getMatch(0);
                }
            }
        }
        if (StringUtils.isEmpty(filename) || StringUtils.isAllEmpty(filesizeWithUnit, filesizeBytesStr)) {
            // eg dailyuploads.net
            final String downloadFileTable2 = new Regex(html, "(?i)<table[^>]*>.*?<h\\d+[^>]*>\\s*Download\\s*File\\s*</h\\d+>\\s*(.*?)</table>").getMatch(0);
            if (downloadFileTable2 != null) {
                if (StringUtils.isEmpty(filename)) {
                    filename = new Regex(downloadFileTable2, "<td\\s*style\\s*=\\s*\"font[^>]*>\\s*(.*?)\\s*(</|<br)").getMatch(0);
                }
                if (StringUtils.isAllEmpty(filesizeWithUnit, filesizeBytesStr)) {
                    filesizeWithUnit = scanGenericFileSize(downloadFileTable2);
                }
            }
        }
        if (StringUtils.isEmpty(filename)) {
            /*
             * 2019-05-21: E.g. uqload.com, vidoba.net - this method will return a 'cleaner' filename than in other places - their titles
             * will often end with " mp4" which we have to correct later!
             */
            final String sharebox3_videohost = "(?i)\\[URL=https?://[^/]+/" + urlFUID + "[^/<>\\]]*?\\]\\[IMG\\][^<>\"\\[\\]]+\\[/IMG\\]([^<>\"]+)\\[/URL\\]";
            if (StringUtils.isEmpty(filename)) {
                /* 2023-07-28: For new style XFS videohosts when on official video download page "/d/<fuid>" */
                filename = new Regex(html, "(?i)<h4 [^>]*>\\s*Download\\s*([^<]*?)\\s*</h\\d+>").getMatch(0);
            }
            /* Next - RegExes for specified types of websites e.g. imagehosts */
            if (StringUtils.isEmpty(filename) && this.isImagehoster()) {
                filename = regexImagehosterFilename(br);
            }
            /* Next - RegExes for videohosts */
            if (StringUtils.isEmpty(filename)) {
                filename = new Regex(html, sharebox3_videohost).getMatch(0);
                if (StringUtils.isEmpty(filename)) {
                    /* 2017-04-11: Typically for XVideoSharing sites */
                    filename = new Regex(html, Pattern.compile("<title>\\s*Watch(?:ing)?\\s*([^<>\"]+)\\s*</title>", Pattern.CASE_INSENSITIVE)).getMatch(0);
                }
                if (StringUtils.isEmpty(filename) && isImagehoster()) {
                    /* Imagehoster site title */
                    final String websiteName = Browser.getHost(getHost()).replaceAll("(\\..+)$", "");
                    filename = new Regex(html, Pattern.compile("<title>\\s*(.*?\\.(png|jpe?g|gif))\\s*-\\s*(" + Pattern.quote(getHost()) + "|" + Pattern.quote(websiteName) + ")\\s*</title>", Pattern.CASE_INSENSITIVE)).getMatch(0);
                }
            }
            if (internal_isVideohosterEmbed(this.br) && (StringUtils.isEmpty(filename) || StringUtils.equalsIgnoreCase("No title", filename))) {
                /* 2019-10-15: E.g. vidoza.net */
                final String curFileName = br.getRegex("var\\s*curFileName\\s*=\\s*\"(.*?)\"").getMatch(0);
                if (StringUtils.isNotEmpty(curFileName)) {
                    filename = curFileName;
                }
            }
            /*
             * 2019-05-16: Experimental RegEx to find 'safe' filesize traits which can always be checked, regardless of the
             * 'supports_availablecheck_filesize_html' setting:
             */
        }
        if (filesizeBytesStr == null) {
            filesizeBytesStr = new Regex(html, "\\((\\d+\\s*bytes)\\)").getMatch(0);
        }
        if (StringUtils.isAllEmpty(filesizeWithUnit, filesizeBytesStr)) {
            filesizeWithUnit = new Regex(html, "id\\s*=\\s*\"fsize[^\"]*\"\\s*>\\s*([0-9\\.]+\\s*[MBTGK]+)\\s*<").getMatch(0);
            if (StringUtils.isEmpty(filesizeWithUnit)) {
                /* 2019-07-12: Example: Katfile.com */
                filesizeWithUnit = new Regex(html, "class\\s*=\\s*\"statd\"\\s*>\\s*size\\s*</span>\\s*<span>\\s*([0-9\\.]+\\s*[MBTGK]+)\\s*<").getMatch(0);
            }
            if (this.supports_availablecheck_filesize_html() && StringUtils.isEmpty(filesizeWithUnit)) {
                filesizeWithUnit = scanGenericFileSize(html);
            }
        }
        fileInfo[0] = filename;
        if (preferRoughFilesize && filesizeWithUnit != null) {
            fileInfo[1] = filesizeWithUnit;
        } else if (filesizeBytesStr != null) {
            fileInfo[1] = filesizeBytesStr;
        } else {
            fileInfo[1] = filesizeWithUnit;
        }
        /* MD5 is only available in very very rare cases! 2024-07-23: Removed this */
        // fileInfo[2] = new Regex(html, "(?i)<b>\\s*MD5.*?</b>.*?nowrap>\\s*(.*?)\\s*<").getMatch(0);
        return fileInfo;
    }

    protected String scanGenericFileSize(final String html) {
        // sync with YetiShareCore.scanInfo- Generic failover
        String ret = new Regex(html, "(?:>\\s*|\\(\\s*|\"\\s*|\\[\\s*|\\s+)([0-9\\.]+(?:\\s+|\\&nbsp;)?(bytes)(?!ps|/s|\\w|\\s*Storage|\\s*Disk|\\s*Space|\\s*traffic))").getMatch(0);
        if (StringUtils.isEmpty(ret)) {
            ret = new Regex(html, "(?:>\\s*|\\(\\s*|\"\\s*|\\[\\s*|\\s+)([0-9\\.]+(?:\\s+|\\&nbsp;)?(TB|GB|MB|KB)(?!ps|/s|\\w|\\s*Storage|\\s*Disk|\\s*Space|\\s*traffic))").getMatch(0);
        }
        return ret;
    }

    /** Check single URL via mass-linkchecker. Throws PluginException if URL has been detected as offline. */
    public AvailableStatus requestFileInformationWebsiteMassLinkcheckerSingle(final DownloadLink link) throws IOException, PluginException {
        massLinkcheckerWebsite(new DownloadLink[] { link });
        if (!link.isAvailabilityStatusChecked()) {
            return AvailableStatus.UNCHECKED;
        } else if (!link.isAvailable()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            return AvailableStatus.TRUE;
        }
    }

    /**
     * Use this to Override 'checkLinks(final DownloadLink[])' in supported plugins. <br />
     * Used by getFilesizeViaAvailablecheckAlt <br />
     * <b>Use this only if:</b> <br />
     * - You have verified that the filehost has a mass-linkchecker and it is working fine with this code. <br />
     * - The contentURLs contain a filename as a fallback e.g. https://host.tld/<fuid>/someFilename.png.html <br>
     * - If used for single URLs inside 'normal linkcheck' (e.g. inside requestFileInformation), call with setWeakFilename = false <br/>
     * - If the normal way via website is blocked somehow e.g. 'site-verification' captcha <br>
     * <b>- If used to check multiple URLs (mass-linkchecking feature), call with setWeakFilename = true!! </b>
     */
    public boolean massLinkcheckerWebsite(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        boolean linkcheckerHasFailed = false;
        String checkTypeCurrent = null;
        /* Checks linkchecking via: examplehost.com/?op=checkfiles AND examplehost.com/?op=check_files */
        final String checkTypeOld = "checkfiles";
        final String checkTypeNew = "check_files";
        final SubConfiguration cfg = this.getPluginConfig();
        final String checkType_last_used_and_working = cfg.getStringProperty(PROPERTY_PLUGIN_ALT_AVAILABLECHECK_LAST_WORKING, null);
        String checkURL = null;
        int linkcheckTypeTryCount = 0;
        try {
            final Browser br = createNewBrowserInstance();
            this.prepBrowser(br, getMainPage());
            br.setCookiesExclusive(true);
            final StringBuilder sb = new StringBuilder();
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            Form checkForm = null;
            while (true) {
                links.clear();
                while (true) {
                    /* We test max 50 links at once. 2020-05-28: Checked to up to 100 but let's use max. 50. */
                    if (index == urls.length || links.size() == 50) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                sb.delete(0, sb.capacity());
                for (final DownloadLink link : links) {
                    try {
                        this.resolveShortURL(br.cloneBrowser(), link, null);
                    } catch (final PluginException e) {
                        logger.log(e);
                        if (e.getLinkStatus() == LinkStatus.ERROR_FILE_NOT_FOUND) {
                            link.setAvailable(false);
                        } else {
                            link.setAvailable(true);
                        }
                        if (!link.isNameSet()) {
                            setWeakFilename(link, null);
                        }
                        /*
                         * We cannot check shortLinks via mass-linkchecker so if we're unable to convert them to TYPE_NORMAL we basically
                         * already checked them here. Also we have to avoid sending wrong fileIDs to the API otherwise linkcheck WILL fail!
                         */
                        continue;
                    }
                    sb.append(URLEncode.encodeURIComponent(this.getNormalizedDownloadURL(link)));
                    sb.append("%0A");
                }
                {
                    /* Check if the mass-linkchecker works and which check we have to use */
                    while (linkcheckTypeTryCount <= 1) {
                        if (checkTypeCurrent != null) {
                            /* No matter which checkType we tried first - it failed and we need to try the other one! */
                            if (checkTypeCurrent.equals(checkTypeNew)) {
                                checkTypeCurrent = checkTypeOld;
                            } else {
                                checkTypeCurrent = checkTypeNew;
                            }
                        } else if (this.prefer_availablecheck_filesize_alt_type_old()) {
                            /* Old checkType forced? */
                            checkTypeCurrent = checkTypeOld;
                        } else if (checkType_last_used_and_working != null) {
                            /* Try to re-use last working method */
                            checkTypeCurrent = checkType_last_used_and_working;
                        } else {
                            /* First launch */
                            checkTypeCurrent = checkTypeNew;
                        }
                        /*
                         * Sending the Form without a previous request might e.g. fail if the website requires "www." but
                         * supports_availablecheck_filesize_alt_fast returns false.
                         */
                        if (br.getURL() != null) {
                            checkURL = "/?op=" + checkTypeCurrent;
                        } else {
                            checkURL = getMainPage() + "/?op=" + checkTypeCurrent;
                        }
                        /* Get- and prepare Form */
                        if (this.supports_availablecheck_filesize_alt_fast()) {
                            /* Quick way - we do not access the page before and do not need to parse the Form. */
                            checkForm = new Form();
                            checkForm.setMethod(MethodType.POST);
                            checkForm.setAction(checkURL);
                            checkForm.put("op", checkTypeCurrent);
                            checkForm.put("process", "Check+URLs");
                        } else {
                            /* Try to get the Form IF NEEDED as it can contain tokens which would otherwise be missing. */
                            getPage(br, checkURL);
                            checkForm = br.getFormByInputFieldKeyValue("op", checkTypeCurrent);
                            if (checkForm == null) {
                                logger.info("Failed to find Form for checkType: " + checkTypeCurrent);
                                linkcheckTypeTryCount++;
                                continue;
                            }
                        }
                        checkForm.put("list", sb.toString());
                        this.submitForm(br, checkForm);
                        /*
                         * Some hosts will not display any errorpage but also we will not be able to find any of our checked file-IDs inside
                         * the html --> Use this to find out about non-working linkchecking method!
                         */
                        final String example_fuid = this.getFUIDFromURL(links.get(0));
                        if (br.getHttpConnection().getResponseCode() == 404 || !br.getURL().contains(checkTypeCurrent) || !br.containsHTML(example_fuid)) {
                            /*
                             * This method of linkcheck is not supported - increase the counter by one to find out if ANY method worked in
                             * the end.
                             */
                            logger.info("Failed to find check_files Status via checkType: " + checkTypeCurrent);
                            linkcheckTypeTryCount++;
                            continue;
                        } else {
                            break;
                        }
                    }
                }
                for (final DownloadLink link : links) {
                    if (massLinkcheckerParseFileInfo(br, link) == AvailableStatus.UNCHECKED) {
                        logger.warning("Failed to find any information for current DownloadLink --> Possible mass-linkchecker failure");
                        linkcheckerHasFailed = true;
                        continue;
                    }
                    if (!link.isNameSet()) {
                        /*
                         * Fallback! We cannot get 'good' filenames via this call so we have to rely on our fallback-filenames (fuid or
                         * filename inside URL)!
                         */
                        setWeakFilename(link, null);
                    }
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            logger.log(e);
            return false;
        } finally {
            if (linkcheckerHasFailed) {
                logger.info("Seems like checkfiles availablecheck is not supported by this host");
                cfg.setProperty(PROPERTY_PLUGIN_ALT_AVAILABLECHECK_LAST_FAILURE_TIMESTAMP, System.currentTimeMillis());
                cfg.setProperty(PROPERTY_PLUGIN_ALT_AVAILABLECHECK_LAST_FAILURE_VERSION, getPluginVersionHash());
            } else {
                cfg.setProperty(PROPERTY_PLUGIN_ALT_AVAILABLECHECK_LAST_WORKING, checkTypeCurrent);
            }
        }
        if (linkcheckerHasFailed) {
            return false;
        } else {
            return true;
        }
    }

    /** Parses and sets file info returned after doing a mass-linkchecking request to a an XFS website. */
    protected AvailableStatus massLinkcheckerParseFileInfo(final Browser br, final DownloadLink link) {
        final String fuid = this.getFUIDFromURL(link);
        boolean isNewLinkchecker = true;
        String html_for_fuid = br.getRegex("<tr>((?!</?tr>).)*?" + fuid + "((?!</?tr>).)*?</tr>").getMatch(-1);
        if (html_for_fuid == null) {
            /*
             * 2019-07-10: E.g. for old linkcheckers which only return online/offline status in a single line and not as a html table.
             */
            html_for_fuid = br.getRegex("<font color=\\'(?:green|red)\\'>[^>]*?" + fuid + "[^>]*?</font>").getMatch(-1);
            isNewLinkchecker = false;
        }
        if (html_for_fuid == null) {
            return AvailableStatus.UNCHECKED;
        }
        final boolean isOffline;
        if (isNewLinkchecker) {
            isOffline = new Regex(html_for_fuid, "Not found").patternFind();
        } else {
            isOffline = new Regex(html_for_fuid, "<font color='red").patternFind();
        }
        if (isOffline) {
            link.setAvailable(false);
            return AvailableStatus.FALSE;
        } else {
            /* We know that the file is online - let's try to find the filesize ... */
            link.setAvailable(true);
            try {
                final String[] tabla_data = new Regex(html_for_fuid, "<td>?(.*?)</td>").getColumn(0);
                final String size = tabla_data.length >= 2 ? tabla_data[2] : null;
                if (size != null) {
                    /*
                     * Filesize should definitly be given - but at this stage we are quite sure that the file is online so let's not throw a
                     * fatal error if the filesize cannot be found.
                     */
                    link.setDownloadSize(SizeFormatter.getSize(size));
                }
            } catch (final Throwable ignore) {
                logger.log(ignore);
            }
            return AvailableStatus.TRUE;
        }
    }

    /**
     * Try to find filename via '/?op=report_file&id=<fuid>'. Only call this function if internal_supports_availablecheck_filename_abuse()
     * returns true!<br />
     * E.g. needed if officially only logged in users can see filename or filename is missing in html code for whatever reason.<br />
     * Often needed for <b><u>IMAGEHOSTER</u> ' s</b>.<br />
     * Important: Only call this if <b><u>SUPPORTS_AVAILABLECHECK_ABUSE</u></b> is <b>true</b> (meaning omly try this if website supports
     * it)!<br />
     *
     * @throws Exception
     */
    protected String getFnameViaAbuseLink(final Browser br, final DownloadLink link) throws Exception {
        getPage(br, getMainPage() + "/?op=report_file&id=" + this.getFUIDFromURL(link), false);
        /*
         * 2019-07-10: ONLY "No such file" as response might always be wrong and should be treated as a failure! Example: xvideosharing.com
         */
        if (br.containsHTML(">\\s*No such file\\s*<")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String filename = regexFilenameAbuse(br);
        if (filename != null) {
            logger.info("Successfully found filename via report_file: " + filename);
            return filename;
        } else {
            logger.info("Failed to find filename via report_file");
            final boolean fnameViaAbuseUnsupported = br.getHttpConnection().getResponseCode() == 404 || br.getHttpConnection().getResponseCode() == 500 || !StringUtils.containsIgnoreCase(br.getURL(), "report_file") || br.getRequest().getHtmlCode().trim().equalsIgnoreCase("No such file");
            if (fnameViaAbuseUnsupported) {
                logger.info("Seems like report_file availablecheck seems not to be supported by this host");
                final SubConfiguration config = this.getPluginConfig();
                config.setProperty(PROPERTY_PLUGIN_REPORT_FILE_AVAILABLECHECK_LAST_FAILURE_TIMESTAMP, System.currentTimeMillis());
                config.setProperty(PROPERTY_PLUGIN_REPORT_FILE_AVAILABLECHECK_LAST_FAILURE_VERSION, getPluginVersionHash());
            }
            return null;
        }
    }

    /** Part of {@link #getFnameViaAbuseLink() getFnameViaAbuseLink} */
    public String regexFilenameAbuse(final Browser br) {
        String filename = null;
        final String filename_src = br.getRegex("(?i)<b>Filename\\s*:?\\s*<[^\n]+</td>").getMatch(-1);
        if (filename_src != null) {
            filename = new Regex(filename_src, ">([^>]+)</td>$").getMatch(0);
        }
        if (filename == null) {
            /* 2021-05-12: New XFS style e.g. userupload.net */
            filename = br.getRegex("(?i)<label>\\s*Filename\\s*</label>\\s*<input[^>]*class=\"form-control form-control-plaintext\"[^>]*value=\"([^\"]+)\"").getMatch(0);
        }
        return filename;
    }

    /** Only use this if it is made sure that the host we're working with is an imagehoster ("ximagesharing")!! */
    public String regexImagehosterFilename(final Browser br) {
        return br.getRegex("class=\"pic\"[^>]*alt=\"([^<>\"]*?)\"").getMatch(0);
    }

    /**
     * Get filesize via massLinkchecker/alternative availablecheck.<br />
     * Wrapper for requestFileInformationWebsiteMassLinkcheckerSingle which contains a bit of extra log output <br>
     * Often used as fallback if e.g. only logged-in users can see filesize or filesize is not given in html code for whatever reason.<br />
     * Often needed for <b><u>IMAGEHOSTER</u>S</b>.<br />
     * Important: Only call this if <b><u>supports_availablecheck_alt</u></b> is <b>true</b> (meaning omly try this if website supports it)!<br />
     * Some older XFS versions AND videohosts have versions of this linkchecker which only return online/offline and NO FILESIZE!<br>
     * In case there is no filesize given, offline status will still be recognized! <br/>
     *
     * @return isOnline
     * @throws IOException
     */
    protected boolean getFilesizeViaAvailablecheckAlt(final Browser br, final DownloadLink link) throws PluginException, IOException {
        logger.info("Trying getFilesizeViaAvailablecheckAlt");
        requestFileInformationWebsiteMassLinkcheckerSingle(link);
        if (link.isAvailabilityStatusChecked()) {
            logger.info("Successfully checked URL via website massLinkcheck | filesize: " + link.getView().getBytesTotal());
            return true;
        } else {
            logger.info("Failed to find filesize via website massLinkcheck");
            return false;
        }
    }

    protected String getEmbedDllink(final Browser br, final String embedURL, final DownloadLink link, final Account account) throws Exception, PluginException {
        final Browser brc = br.cloneBrowser();
        getPage(brc, embedURL);
        return this.getDllink(link, account, brc, brc.getRequest().getHtmlCode());
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        resolveShortURL(br.cloneBrowser(), link, null);
        doFree(link, null);
    }

    /** Handles pre-download forms & captcha for free (anonymous) + FREE ACCOUNT modes. */
    public void doFree(final DownloadLink link, final Account account) throws Exception, PluginException {
        /* First bring up saved final links */
        if (this.attemptStoredDownloadurlDownload(link, account)) {
            return;
        }
        requestFileInformationWebsite(link, account);
        if (!hasCheckedEmbedHandling && videoStreamDownloadurl == null) {
            final String embedURL = this.findEmbedURL(br, link, account);
            if (embedURL != null) {
                logger.info("Looks like this is a videohost which is using self-embedding");
                hasCheckedEmbedHandling = true;
                videoStreamDownloadurl = getEmbedDllink(br, embedURL, link, account);
            }
        }
        String dllink = null;
        String officialDownloadURL = null;
        final DownloadMode mode = this.getPreferredDownloadModeFromConfig();
        final boolean preferOfficialVideoDownload;
        if (mode == DownloadMode.ORIGINAL && Boolean.TRUE.equals(requiresCaptchaForOfficialVideoDownload())) {
            preferOfficialVideoDownload = true;
        } else if (mode == DownloadMode.AUTO && Boolean.FALSE.equals(requiresCaptchaForOfficialVideoDownload())) {
            preferOfficialVideoDownload = true;
        } else {
            preferOfficialVideoDownload = false;
        }
        if (this.videoStreamDownloadurl != null && !preferOfficialVideoDownload) {
            logger.info("Do not enter download1 loop because: Found stream downloadurl");
            dllink = this.videoStreamDownloadurl;
        } else {
            int download1counter = 0;
            final int download1max = 1;
            download1: do {
                logger.info(String.format("Handling download1 loop %d / %d", download1counter + 1, download1max + 1));
                dllink = getDllink(link, account, br, getCorrectBR(br));
                if (!StringUtils.isEmpty(dllink) && !preferOfficialVideoDownload) {
                    logger.info("Stepping out of download1 loop because: Found directurl");
                    break download1;
                }
                officialDownloadURL = getDllinkViaOfficialVideoDownload(this.br.cloneBrowser(), link, account, false);
                if (!StringUtils.isEmpty(officialDownloadURL)) {
                    logger.info("Stepping out of download1 loop because: User wants original download && we found original download");
                    break download1;
                }
                /* Extra handling for imagehosts */
                if (StringUtils.isEmpty(dllink) && this.isImagehoster()) {
                    checkErrors(br, getCorrectBR(br), link, account, false);
                    Form imghost_next_form = findImageForm(this.br);
                    if (imghost_next_form != null) {
                        int counter = -1;
                        final int countermax = 3;
                        do {
                            counter++;
                            logger.info(String.format("imghost_next_form loop %d / %d", counter + 1, countermax));
                            // this.handleCaptcha(link, imghost_next_form);
                            submitForm(imghost_next_form);
                            checkErrors(br, getCorrectBR(br), link, account, false);
                            dllink = getDllink(link, account, br, getCorrectBR(br));
                            /* For imagehosts, filenames are often not given until we can actually see/download the image. */
                            final String imageFilename = regexImagehosterFilename(br);
                            if (imageFilename != null) {
                                link.setName(Encoding.htmlOnlyDecode(imageFilename));
                            }
                            if (this.isAbort()) {
                                throw new InterruptedException();
                            } else if (!StringUtils.isEmpty(dllink)) {
                                logger.info("Found image directurl: " + dllink);
                                break download1;
                            } else if (counter >= countermax) {
                                logger.warning("Imagehost handling exceeded max tries");
                                break;
                            } else {
                                /* Continue to next try */
                                imghost_next_form = findImageForm(this.br);
                                if (imghost_next_form == null) {
                                    logger.warning("Failed to find next imghost_next_form and no directurl present -> Stepping out of imagehost handling");
                                    break;
                                }
                                continue;
                            }
                        } while (true);
                    }
                }
                if (!StringUtils.isEmpty(dllink) || !StringUtils.isEmpty(officialDownloadURL)) {
                    logger.info("Stepping out of download1 loop because: Found directurl");
                    break download1;
                } else if (download1counter > 0) {
                    break download1;
                }
                /* Check for errors and download1 Form. Only execute this part once! */
                /*
                 * Check errors here because if we don't and a link is premiumonly, download1 Form will be present, plugin will send it and
                 * most likely end up with error "Fatal countdown error (countdown skipped)"
                 */
                checkErrors(br, getCorrectBR(br), link, account, false);
                final Form download1 = findFormDownload1Free(br);
                if (download1 == null) {
                    logger.info("Failed to find download1 Form");
                    break download1;
                }
                logger.info("Found download1 Form");
                /* Wait before sending download1 form is not so common. Example where it is needed: fastream.to */
                waitTime(link, Time.systemIndependentCurrentJVMTimeMillis());
                submitForm(download1);
                checkErrors(br, getCorrectBR(br), link, account, false);
                download1counter++;
            } while (download1counter <= download1max && StringUtils.isEmpty(dllink));
        }
        if (!StringUtils.isEmpty(dllink) && !preferOfficialVideoDownload) {
            logger.info("Do not enter download2 loop because: Found stream downloadurl");
        } else {
            download2: if ((StringUtils.isEmpty(dllink) && StringUtils.isEmpty(officialDownloadURL)) || (StringUtils.isEmpty(officialDownloadURL) && preferOfficialVideoDownload)) {
                /* Go further if needed. */
                logger.info("Jumping into download2 handling");
                Form download2 = findFormDownload2Free(br);
                if (download2 == null) {
                    logger.warning("Failed to find download2 Form");
                    break download2;
                }
                logger.info("Found download2 Form");
                checkErrors(br, getCorrectBR(br), link, account, false);
                /* Define how many forms deep do we want to try? */
                final int download2start = 0;
                final int download2max = 2;
                for (int download2counter = download2start; download2counter <= download2max; download2counter++) {
                    logger.info(String.format("Download2 loop %d / %d", download2counter + 1, download2max + 1));
                    final long timeBefore = Time.systemIndependentCurrentJVMTimeMillis();
                    handlePassword(download2, link);
                    handleCaptcha(link, br, download2);
                    waitTime(link, timeBefore);
                    if (this.tryDownload(br, link, account, br.createFormRequest(download2), DOWNLOAD_ATTEMPT_FLAGS.CONNECT_OR_EXCEPTION)) {
                        return;
                    }
                    logger.info("Submitted Form download2");
                    checkErrors(br, getCorrectBR(br), link, account, true);
                    if (StringUtils.isEmpty(dllink)) {
                        dllink = getDllink(link, account, br, getCorrectBR(br));
                        if (!StringUtils.isEmpty(dllink) && !preferOfficialVideoDownload) {
                            break download2;
                        }
                    }
                    if (StringUtils.isEmpty(officialDownloadURL)) {
                        officialDownloadURL = getDllinkViaOfficialVideoDownload(this.br.cloneBrowser(), link, account, false);
                    }
                    if (!StringUtils.isEmpty(officialDownloadURL) || !StringUtils.isEmpty(dllink)) {
                        /* Success */
                        validateLastChallengeResponse();
                        logger.info("Stepping out of download2 loop because: Found downloadlink");
                        break;
                    } else if ((download2 = findFormDownload2Free(br)) == null) {
                        /* Failure */
                        logger.info("Stepping out of download2 loop because: download2 form is null");
                        break;
                    } else {
                        /* Continue to next round / next pre-download page */
                        invalidateLastChallengeResponse();
                        continue;
                    }
                }
            }
        }
        if (StringUtils.isEmpty(officialDownloadURL) && StringUtils.isEmpty(dllink)) {
            logger.warning("Failed to find final downloadurl");
            checkErrors(br, getCorrectBR(br), link, account, false);
            checkServerErrors(br, link, account);
            checkErrorsLastResort(br, link, account);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        handleDownload(link, account, officialDownloadURL, dllink);
    }

    /** Returns relative URL to embed content if available in current browsers' html code. */
    protected String findEmbedURL(final Browser br, final DownloadLink link, final Account Account) {
        final String fid = this.getFUIDFromURL(link);
        final URL_TYPE urltype = this.getURLType(link);
        if (fid == null || urltype == null) {
            return null;
        }
        final String expectedurl;
        if (this.isXFSOld(urltype)) {
            expectedurl = this.buildURLPath(link, fid, URL_TYPE.EMBED_VIDEO);
        } else {
            expectedurl = this.buildURLPath(link, fid, URL_TYPE.EMBED_VIDEO_2);
        }
        if (br.containsHTML(Pattern.quote(expectedurl))) {
            return expectedurl;
        } else {
            return null;
        }
    }

    /**
     * Checks if official video download is possible and returns final downloadurl if possible. <br>
     * This should NOT throw any Exceptions!
     *
     * @param returnFilesize
     *            true = Only return filesize of selected quality. Use this in availablecheck. <br>
     *            false = return final downloadurl of selected quality. Use this in download mode.
     */
    protected String getDllinkViaOfficialVideoDownload(final Browser br, final DownloadLink link, final Account account, final boolean returnFilesize) throws Exception {
        if (returnFilesize) {
            logger.info("[FilesizeMode] Trying to find official video downloads");
        } else {
            logger.info("[DownloadMode] Trying to find official video downloads");
        }
        String dllink = null;
        /* Info in table. E.g. xvideosharing.com, watchvideo.us */
        String[] videoQualityHTMLs = br.getRegex("<tr><td>[^\r\t\n]+download_video\\(.*?</td></tr>").getColumn(-1);
        if (videoQualityHTMLs == null || videoQualityHTMLs.length == 0) {
            /* Match on line - safe attempt but this may not include filesize! */
            videoQualityHTMLs = br.getRegex("download_video\\([^\r\t\n]+").getColumn(-1);
        }
        if (videoQualityHTMLs == null || videoQualityHTMLs.length == 0) {
            /* Try new handling */
            return getDllinkViaOfficialVideoDownloadNew(br, link, account, returnFilesize);
        }
        /*
         * Internal quality identifiers highest to lowest (inside 'download_video' String): o = original, h = high, n = normal, l=low
         */
        final Map<String, Integer> qualityMap = new HashMap<String, Integer>();
        qualityMap.put("l", 20); // low
        qualityMap.put("n", 40); // normal
        qualityMap.put("h", 60); // high
        qualityMap.put("o", 80); // original
        qualityMap.put("x", 100); // download/original
        // Parse all HTML qualities once and store in list
        List<Map<String, Object>> parsedQualities = new ArrayList<Map<String, Object>>();
        for (int currentQualityIndex = 0; currentQualityIndex < videoQualityHTMLs.length; currentQualityIndex++) {
            final String videoQualityHTML = videoQualityHTMLs[currentQualityIndex];
            final String filesizeStrTmp = scanGenericFileSize(videoQualityHTML);
            final Regex videoinfo = new Regex(videoQualityHTML, "download_video\\('([a-z0-9]+)','([^<>\"\\']*)','([^<>\"\\']*)'");
            final String videoQualityStr = videoinfo.getMatch(1);
            final String videoHashTmp = videoinfo.getMatch(2);
            if (StringUtils.isEmpty(videoQualityStr) || StringUtils.isEmpty(videoHashTmp)) {
                logger.warning("Found unidentifyable video quality");
                continue;
            } else if (!qualityMap.containsKey(videoQualityStr)) {
                logger.info("Skipping unknown quality: " + videoQualityStr);
                continue;
            }
            final int internalQualityValue = qualityMap.get(videoQualityStr);
            Map<String, Object> qualityData = new HashMap<String, Object>();
            qualityData.put("qualityStr", videoQualityStr);
            qualityData.put("videoHash", videoHashTmp);
            qualityData.put("filesizeStr", filesizeStrTmp);
            qualityData.put("targetHTML", videoQualityHTML);
            qualityData.put("qualityIndex", currentQualityIndex);
            qualityData.put("internalQualityValue", internalQualityValue);
            parsedQualities.add(qualityData);
        }
        if (parsedQualities.isEmpty()) {
            logger.warning("No valid video qualities found");
            return null;
        }
        String filesizeStr = null;
        String chosenVideoQualityStr = null;
        String videoHash = null;
        String targetHTML = null;
        final String userSelectedQualityStr = getPreferredDownloadQualityStr();
        boolean foundUserSelectedQuality = false;
        if (userSelectedQualityStr == null) {
            logger.info("Trying to find highest quality for official video download");
        } else {
            logger.info(String.format("Trying to find user selected quality %s for official video download", userSelectedQualityStr));
        }
        int selectedQualityIndex = 0;
        Map<String, Object> selectedQuality = null;
        // First try to find exact match for user selected quality
        if (userSelectedQualityStr != null) {
            for (Map<String, Object> quality : parsedQualities) {
                String qualityStr = (String) quality.get("qualityStr");
                if (qualityStr.equalsIgnoreCase(userSelectedQualityStr)) {
                    logger.info("Found user selected quality: " + userSelectedQualityStr);
                    foundUserSelectedQuality = true;
                    selectedQuality = quality;
                    break;
                }
            }
            // If user selected quality not found, find next best
            if (!foundUserSelectedQuality) {
                logger.info("Failed to find user selected quality --> Finding next best quality");
                final int userSelectedQualityValue = qualityMap.get(userSelectedQualityStr.toLowerCase());
                int nextBestQualityValue = Integer.MAX_VALUE;
                for (Map<String, Object> quality : parsedQualities) {
                    int internalQualityValue = (Integer) quality.get("internalQualityValue");
                    // Find the smallest quality value that is still higher than user selected
                    if (internalQualityValue > userSelectedQualityValue && internalQualityValue < nextBestQualityValue) {
                        nextBestQualityValue = internalQualityValue;
                        selectedQuality = quality;
                    }
                }
                if (selectedQuality != null) {
                    logger.info("Selected next best quality: " + selectedQuality.get("qualityStr") + " (higher than target " + userSelectedQualityStr + ")");
                }
            }
        }
        // If no user selection or next best found, find highest quality
        if (selectedQuality == null) {
            int maxInternalQualityValue = 0;
            for (Map<String, Object> quality : parsedQualities) {
                int internalQualityValue = (Integer) quality.get("internalQualityValue");
                if (internalQualityValue > maxInternalQualityValue) {
                    maxInternalQualityValue = internalQualityValue;
                    selectedQuality = quality;
                }
            }
            if (userSelectedQualityStr != null && !foundUserSelectedQuality) {
                logger.info("No higher quality found, returning highest available: " + selectedQuality.get("qualityStr"));
            }
        }
        // Extract final values from selected quality
        if (selectedQuality != null) {
            chosenVideoQualityStr = (String) selectedQuality.get("qualityStr");
            videoHash = (String) selectedQuality.get("videoHash");
            filesizeStr = (String) selectedQuality.get("filesizeStr");
            targetHTML = (String) selectedQuality.get("targetHTML");
            selectedQualityIndex = (Integer) selectedQuality.get("qualityIndex");
        }
        if (targetHTML == null || chosenVideoQualityStr == null || videoHash == null) {
            /* This should never happen */
            logger.info(String.format("Failed to find officially downloadable video quality although there are %d qualities available", videoQualityHTMLs.length));
            return null;
        }
        if (filesizeStr == null) {
            /*
             * Last chance attempt to find filesize for selected quality. Only allow units "MB" and "GB" as most filesizes will have one of
             * these units.
             */
            final String[] filesizeCandidates = br.getRegex("(\\d+(?:\\.\\d{1,2})? *(MB|GB))").getColumn(0);
            /* Are there as many filesizes available as there are video qualities --> Chose correct filesize by index */
            if (filesizeCandidates.length == videoQualityHTMLs.length) {
                filesizeStr = filesizeCandidates[selectedQualityIndex];
            }
        }
        if (foundUserSelectedQuality) {
            logger.info("Found user selected quality: " + userSelectedQualityStr);
        } else {
            logger.info("Picked BEST quality: " + chosenVideoQualityStr);
        }
        if (filesizeStr == null) {
            /* No dramatic failure */
            logger.info("Failed to find filesize");
        } else {
            logger.info("Found filesize of official video download: " + filesizeStr);
        }
        if (returnFilesize) {
            /* E.g. in availablecheck */
            return filesizeStr;
        }
        /* 2019-08-29: Wait time here is possible but a rare case e.g. deltabit.co */
        this.waitTime(link, Time.systemIndependentCurrentJVMTimeMillis());
        logger.info("Waiting extra wait seconds: " + getDllinkViaOfficialVideoDownloadExtraWaittimeSeconds());
        /**
         * 2020-05-22: Workaround attempt for unnerving class="err">Security error< which can sometimes appear if you're too fast in this
         * handling. <br>
         * This issue may have solved in newer XFS versions so we might be able to remove this extra wait in the long run.
         */
        this.sleep(getDllinkViaOfficialVideoDownloadExtraWaittimeSeconds() * 1000l, link);
        getPage(br, "/dl?op=download_orig&id=" + this.getFUIDFromURL(link) + "&mode=" + chosenVideoQualityStr + "&hash=" + videoHash);
        /* 2019-08-29: This Form may sometimes be given e.g. deltabit.co */
        final Form download1 = br.getFormByInputFieldKeyValue("op", "download1");
        if (download1 != null) {
            this.submitForm(br, download1);
            this.checkErrors(br, br.getRequest().getHtmlCode(), link, account, false);
        }
        /*
         * 2019-10-04: TODO: Unsure whether we should use the general 'getDllink' method here as it contains a lot of RegExes (e.g. for
         * streaming URLs) which are completely useless here.
         */
        dllink = this.getDllink(link, account, br, br.getRequest().getHtmlCode());
        if (StringUtils.isEmpty(dllink)) {
            logger.warning("Failed to find dllink via official video download");
            return null;
        } else {
            logger.info("Successfully found dllink via official video download:" + dllink);
            return dllink;
        }
    }

    /** 2023-07-25:For new style XFS websites with links like /d/[a-z0-9]{12} e.g. streamhide.com */
    protected String getDllinkViaOfficialVideoDownloadNew(final Browser br, final DownloadLink link, final Account account, final boolean returnFilesize) throws Exception {
        if (returnFilesize) {
            logger.info("[FilesizeMode] Trying to find official video downloads");
        } else {
            logger.info("[DownloadMode] Trying to find official video downloads");
        }
        final String[] videourls = br.getRegex("(/d/[a-z0-9]{12}_[a-z]{1})").getColumn(0);
        final String[][] videoresolutionsAndFilesizes = br.getRegex(">\\s*(\\d+x\\d+), (\\d+(\\.\\d{1,2})?,? [A-Za-z]{1,5})").getMatches();
        if (videourls == null || videourls.length == 0) {
            logger.info("Failed to find any official video downloads");
            return null;
        }
        // Parse all video info once and store in list of maps
        List<Map<String, Object>> parsedVideos = new ArrayList<Map<String, Object>>();
        for (int i = 0; i < videourls.length; i++) {
            final String videoURL = videourls[i];
            String resolution = null;
            String filesizeStr = null;
            if (videoresolutionsAndFilesizes != null && videoresolutionsAndFilesizes.length == videourls.length) {
                final String[] thisVideoResolutionAndFilesize = videoresolutionsAndFilesizes[i];
                resolution = thisVideoResolutionAndFilesize[0];
                filesizeStr = thisVideoResolutionAndFilesize[1];
            }
            final String videoQualityStr = new Regex(videoURL, "_([a-z]{1})$").getMatch(0);
            if (StringUtils.isEmpty(videoQualityStr)) {
                logger.warning("Found unidentifyable video quality");
                continue;
            }
            Map<String, Object> videoData = new HashMap<String, Object>();
            videoData.put("videoURL", videoURL);
            videoData.put("resolution", resolution);
            videoData.put("filesizeStr", filesizeStr);
            videoData.put("videoQualityStr", videoQualityStr);
            parsedVideos.add(videoData);
        }
        if (parsedVideos.isEmpty()) {
            logger.info("No valid video qualities found");
            return null;
        }
        /*
         * Internal quality identifiers highest to lowest (inside 'download_video' String): o = original, h = high, n = normal, l=low
         */
        final Map<String, Integer> qualityMap = new HashMap<String, Integer>();
        qualityMap.put("l", 20); // low
        qualityMap.put("n", 40); // normal
        qualityMap.put("h", 60); // high
        qualityMap.put("o", 80); // original
        qualityMap.put("x", 100); // download
        // Filter out unknown qualities and add internal quality values
        List<Map<String, Object>> validVideos = new ArrayList<Map<String, Object>>();
        for (Map<String, Object> videoData : parsedVideos) {
            String videoQualityStr = (String) videoData.get("videoQualityStr");
            if (!qualityMap.containsKey(videoQualityStr)) {
                logger.info("Skipping unknown quality: " + videoQualityStr);
                continue;
            }
            videoData.put("internalQualityValue", qualityMap.get(videoQualityStr));
            validVideos.add(videoData);
        }
        if (validVideos.isEmpty()) {
            logger.warning("No valid video qualities found after filtering");
            return null;
        }
        final String userSelectedQualityValue = getPreferredDownloadQualityStr();
        if (userSelectedQualityValue == null) {
            logger.info("Trying to find highest quality for official video download");
        } else {
            logger.info(String.format("Trying to find user selected quality %s for official video download", userSelectedQualityValue));
        }
        Map<String, Object> selectedVideo = null;
        boolean foundUserSelectedQuality = false;
        // First try to find exact match for user selected quality
        if (userSelectedQualityValue != null) {
            for (Map<String, Object> videoData : validVideos) {
                String videoQualityStr = (String) videoData.get("videoQualityStr");
                if (videoQualityStr.equalsIgnoreCase(userSelectedQualityValue)) {
                    logger.info("Found user selected quality: " + userSelectedQualityValue);
                    foundUserSelectedQuality = true;
                    selectedVideo = videoData;
                    break;
                }
            }
            // If user selected quality not found, find next best
            if (!foundUserSelectedQuality) {
                logger.info("Failed to find user selected quality --> Finding next best quality");
                final int userSelectedQualityValueInt = qualityMap.get(userSelectedQualityValue.toLowerCase());
                int nextBestQualityValue = Integer.MAX_VALUE;
                for (Map<String, Object> videoData : validVideos) {
                    int internalQualityValue = (Integer) videoData.get("internalQualityValue");
                    // Find the smallest quality value that is still higher than user selected
                    if (internalQualityValue > userSelectedQualityValueInt && internalQualityValue < nextBestQualityValue) {
                        nextBestQualityValue = internalQualityValue;
                        selectedVideo = videoData;
                    }
                }
                if (selectedVideo != null) {
                    logger.info("Selected next best quality: " + selectedVideo.get("videoQualityStr") + " (higher than target " + userSelectedQualityValue + ")");
                }
            }
        }
        // If no user selection or next best found, find highest quality
        if (selectedVideo == null) {
            int maxInternalQualityValue = 0;
            for (Map<String, Object> videoData : validVideos) {
                int internalQualityValue = (Integer) videoData.get("internalQualityValue");
                if (internalQualityValue > maxInternalQualityValue) {
                    maxInternalQualityValue = internalQualityValue;
                    selectedVideo = videoData;
                }
            }
            if (userSelectedQualityValue != null && !foundUserSelectedQuality) {
                logger.info("No higher quality found, returning highest available: " + selectedVideo.get("videoQualityStr"));
            } else {
                logger.info("Returning BEST quality according to user preference");
            }
        }
        if (selectedVideo == null) {
            logger.warning("Video selection handling failed");
            return null;
        }
        // Extract final values
        final String filesizeStrChosen = (String) selectedVideo.get("filesizeStr");
        final String continueURL = (String) selectedVideo.get("videoURL");
        if (foundUserSelectedQuality) {
            logger.info("Returning user selected quality: " + userSelectedQualityValue);
        } else if (userSelectedQualityValue != null) {
            logger.info("Returning next best or highest quality as fallback");
        } else {
            logger.info("Returning BEST quality according to user preference");
        }
        if (returnFilesize) {
            /* E.g. in availablecheck */
            return filesizeStrChosen;
        }
        getPage(br, continueURL);
        checkErrors(br, continueURL, link, account, false);
        final Form download1 = br.getFormByInputFieldKeyValue("op", "download_orig");
        if (download1 != null) {
            final long timeBefore = Time.systemIndependentCurrentJVMTimeMillis();
            handleCaptcha(link, br, download1);
            final int extraWaitSeconds = getDllinkViaOfficialVideoDownloadExtraWaittimeSeconds();
            if (extraWaitSeconds > 0) {
                logger.info("Waiting extra wait seconds: " + extraWaitSeconds);
                this.waitTime(link, timeBefore, extraWaitSeconds);
            }
            submitForm(br, download1);
            checkErrors(br, br.getRequest().getHtmlCode(), link, account, false);
        }
        final String dllink = this.getDllink(link, account, br, br.getRequest().getHtmlCode());
        if (StringUtils.isEmpty(dllink)) {
            logger.warning("Failed to find dllink via official video download");
            return null;
        }
        logger.info("Successfully found dllink via official video download");
        return dllink;
    }

    protected int getDllinkViaOfficialVideoDownloadExtraWaittimeSeconds() {
        return 5;
    }

    /**
     * @return User selected video download quality for official video download. <br>
     *         h = high <br>
     *         n = normal <br>
     *         l = low <br>
     *         null = No selection/Grab BEST available
     */
    protected String getPreferredDownloadQualityStr() {
        final Class<? extends XFSConfigVideo> cfgO = getVideoConfigInterface();
        if (cfgO == null) {
            return null;
        }
        final XFSConfigVideo cfg = PluginJsonConfig.get(cfgO);
        final PreferredDownloadQuality quality = cfg.getPreferredDownloadQuality();
        switch (quality) {
        case HIGH:
            return "h";
        case NORMAL:
            return "n";
        case LOW:
            return "l";
        case BEST:
        default:
            return null;
        }
    }

    protected void setCaptchaResponse(final Browser br, CaptchaHosterHelperInterface captchaHosterHelper, final Form form, final String response) {
        if (captchaHosterHelper instanceof CaptchaHelperHostPluginHCaptcha) {
            form.put("h-captcha-response", Encoding.urlEncode(response));
            if (containsRecaptchaV2Class(br)) {
                /*
                 * E.g. novafile.com, filefox.cc - some use this as legacy handling, some will even send both, h-captcha-response AND
                 * g-recaptcha-response
                 */
                form.put("g-recaptcha-response", Encoding.urlEncode(response));
            }
        } else {
            form.put("g-recaptcha-response", Encoding.urlEncode(response));
        }
    }

    /**
     * Admins may sometimes setup waittimes that are higher than the interactive captcha timeout so lets say they set up 180 seconds of
     * pre-download-waittime --> User solves captcha immediately --> Captcha-solution times out after 120 seconds --> User has to re-enter
     * it in browser (and it would fail in JD)! <br>
     * If admins set it up in a way that users can solve the captcha via the waittime counts down, this failure may even happen via browser! <br>
     * This is basically a workaround which avoids running into said timeout: Make sure that we wait less than 120 seconds after the user
     * has solved the captcha by waiting some of this time in beforehand.
     */
    protected void waitBeforeInteractiveCaptcha(final DownloadLink link, final int captchaTimeoutMillis) throws PluginException {
        final String waitStr;
        if (this.preDownloadWaittimeSkippable()) {
            return;
        } else if ((waitStr = regexWaittime(br)) == null) {
            return;
        } else if (!waitStr.matches("\\d+")) {
            return;
        }
        final int preDownloadWaittimeMillis = Integer.parseInt(waitStr) * 1000;
        if (preDownloadWaittimeMillis > captchaTimeoutMillis) {
            final int prePrePreDownloadWait = preDownloadWaittimeMillis - captchaTimeoutMillis;
            logger.info("Waittime is higher than interactive captcha timeout --> Waiting a part of it before solving captcha to avoid captcha-token-timeout");
            logger.info("Pre-pre download waittime seconds: " + (prePrePreDownloadWait / 1000));
            this.sleep(prePrePreDownloadWait, link);
        }
    }

    protected boolean handleCloudflareTurnstileCaptcha(final DownloadLink link, Browser br, final Form captchaForm) throws Exception {
        final CaptchaHelperHostPluginCloudflareTurnstile ts = new CaptchaHelperHostPluginCloudflareTurnstile(this, br);
        /**
         * This contains a workaround for a widespread design-flaw when using an interactive captcha and a long wait-time in browser: <br>
         * We need to split up the total wait time in such a case otherwise our solution token will expire before we get the chance to send
         * it!
         */
        logger.info("Detected captcha method \"CloudflareTurnstileCaptcha\" for this host");
        this.waitBeforeInteractiveCaptcha(link, ts.getSolutionTimeout());
        final String cfTurnstileResponse = ts.getToken();
        captchaForm.put("cf-turnstile-response", Encoding.urlEncode(cfTurnstileResponse));
        /* For cheap copy & paste implementations which some websites are doing. */
        captchaForm.put("g-recaptcha-response", Encoding.urlEncode(cfTurnstileResponse));
        return true;
    }

    protected boolean handleHCaptcha(final DownloadLink link, Browser br, final Form captchaForm) throws Exception {
        final CaptchaHelperHostPluginHCaptcha hCaptcha = getCaptchaHelperHostPluginHCaptcha(this, br);
        /**
         * This contains a workaround for a widespread design-flaw when using hcaptcha and a long wait-time in browser: <br>
         * We need to split up the total waittime in such a case otherwise our solution token will expire before we get the chance to send
         * it!
         */
        logger.info("Detected captcha method \"hcaptcha\" type '" + hCaptcha.getType() + "' for this host");
        this.waitBeforeInteractiveCaptcha(link, hCaptcha.getSolutionTimeout());
        final String response = hCaptcha.getToken();
        setCaptchaResponse(br, hCaptcha, captchaForm, response);
        return true;
    }

    protected boolean handleRecaptchaV2(final DownloadLink link, Browser br, final Form captchaForm) throws Exception {
        /*
         * This contains a workaround for a widespread design-flaw when using reCaptchaV2 and a long wait-time in browser: We need to split
         * up the wait in such a case.
         */
        final CaptchaHelperHostPluginRecaptchaV2 rc2 = getCaptchaHelperHostPluginRecaptchaV2(this, br);
        logger.info("Detected captcha method \"RecaptchaV2\" normal-type '" + rc2.getType() + "' for this host");
        this.waitBeforeInteractiveCaptcha(link, rc2.getSolutionTimeout());
        final String recaptchaV2Response = rc2.getToken();
        setCaptchaResponse(br, rc2, captchaForm, recaptchaV2Response);
        return true;
    }

    protected CaptchaHelperHostPluginHCaptcha getCaptchaHelperHostPluginHCaptcha(PluginForHost plugin, Browser br) throws PluginException {
        return new CaptchaHelperHostPluginHCaptcha(this, br);
    }

    protected CaptchaHelperHostPluginRecaptchaV2 getCaptchaHelperHostPluginRecaptchaV2(PluginForHost plugin, Browser br) throws PluginException {
        return new CaptchaHelperHostPluginRecaptchaV2(this, br);
    }

    /** Handles all kinds of captchas, also login-captcha - fills captcha answer into given captchaForm. */
    public void handleCaptcha(final DownloadLink link, Browser br, final Form captchaForm) throws Exception {
        /* Captcha START */
        final String reCaptchaKey = captchaForm.getRegex("data-sitekey=\"([^\"]+)").getMatch(0);
        if (new Regex(getCorrectBR(br), Pattern.compile("\\$\\.post\\(\\s*\"/ddl\"", Pattern.CASE_INSENSITIVE)).patternFind()) {
            /* 2019-06-06: Rare case */
            final String captchaResponse;
            final CaptchaHosterHelperInterface captchaHelper;
            if (containsHCaptcha(getCorrectBR(br))) {
                final CaptchaHelperHostPluginHCaptcha hCaptcha = getCaptchaHelperHostPluginHCaptcha(this, br);
                logger.info("Detected captcha method \"hCaptcha\" type '" + hCaptcha.getType() + "' for this host");
                captchaHelper = hCaptcha;
                this.waitBeforeInteractiveCaptcha(link, hCaptcha.getSolutionTimeout());
                captchaResponse = hCaptcha.getToken();
            } else {
                /* Assume reCaptchaV2 is required */
                final CaptchaHelperHostPluginRecaptchaV2 rc2 = getCaptchaHelperHostPluginRecaptchaV2(this, br);
                logger.info("Detected captcha method \"RecaptchaV2\" type '" + rc2.getType() + "' for this host");
                captchaHelper = rc2;
                this.waitBeforeInteractiveCaptcha(link, rc2.getSolutionTimeout());
                captchaResponse = captchaHelper.getToken();
            }
            /*
             * 2017-12-07: New - solve- and check reCaptchaV2 here via ajax call, then wait- and submit the main downloadform. This might as
             * well be a workaround by the XFS developers to avoid expiring reCaptchaV2 challenges. Example: filefox.cc
             */
            /* 2017-12-07: New - this case can only happen during download and cannot be part of the login process! */
            /* Do not put the result in the given Form as the check itself is handled via Ajax right here! */
            captchaForm.put("g-recaptcha-response", "");
            final Form ajaxCaptchaForm = new Form();
            ajaxCaptchaForm.setMethod(MethodType.POST);
            ajaxCaptchaForm.setAction("/ddl");
            final InputField inputField_Rand = captchaForm.getInputFieldByName("rand");
            final String file_id = PluginJSonUtils.getJson(br, "file_id");
            if (inputField_Rand != null) {
                /* This is usually given */
                ajaxCaptchaForm.put("rand", inputField_Rand.getValue());
            }
            if (!StringUtils.isEmpty(file_id)) {
                /* This is usually given */
                ajaxCaptchaForm.put("file_id", file_id);
            }
            ajaxCaptchaForm.put("op", "captcha1");
            final Browser brc = br.cloneBrowser();
            setCaptchaResponse(brc, captchaHelper, ajaxCaptchaForm, captchaResponse);
            /* User existing Browser object as we get a cookie which is required later. */
            brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            this.submitForm(brc, ajaxCaptchaForm);
            if (brc.getRequest().getHtmlCode().equalsIgnoreCase("ERROR: Wrong captcha")) {
                /* 2019-12-14: Happens but should never happen ... */
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            } else if (!brc.getRequest().getHtmlCode().equalsIgnoreCase("OK")) {
                this.logger.warning("Fatal " + captchaHelper + " ajax handling failure");
                checkErrorsLastResort(brc, link, null);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } else if (AbstractCloudflareTurnstileCaptcha.containsCloudflareTurnstileClass(br)) {
            if (handleCloudflareTurnstileCaptcha(link, br, captchaForm)) {
            }
        } else if (containsHCaptcha(getCorrectBR(br))) {
            if (handleHCaptcha(link, br, captchaForm)) {
            }
        } else if (containsRecaptchaV2Class(getCorrectBR(br))) {
            if (handleRecaptchaV2(link, br, captchaForm)) {
            }
        } else if (reCaptchaKey != null) {
            /* 2025-07-15: Detected reCaptcha within form e.g. dropload.io when called via getDllinkViaOfficialVideoDownloadNew */
            if (handleRecaptchaV2(link, br, captchaForm)) {
            }
        } else {
            if (containsPlainTextCaptcha(getCorrectBR(br))) {
                logger.info("Detected captcha method \"plaintext captchas\" for this host");
                /* Captcha method by ManiacMansion */
                String[][] letters = new Regex(br, "<span style=.position:absolute;padding-left:(\\d+)px;padding-top:\\d+px;.>(&#\\d+;)</span>").getMatches();
                if (letters == null || letters.length == 0) {
                    /* Try again, this time look in non-cleaned-up html as correctBR() could have removed this part! */
                    letters = new Regex(br.getRequest().getHtmlCode(), "<span style=.position:absolute;padding-left:(\\d+)px;padding-top:\\d+px;.>(&#\\d+;)</span>").getMatches();
                    if (letters == null || letters.length == 0) {
                        logger.warning("plaintext captchahandling broken!");
                        checkErrorsLastResort(br, link, null);
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
                final SortedMap<Integer, String> capMap = new TreeMap<Integer, String>();
                for (String[] letter : letters) {
                    capMap.put(Integer.parseInt(letter[0]), Encoding.htmlDecode(letter[1]));
                }
                final StringBuilder code = new StringBuilder();
                for (String value : capMap.values()) {
                    code.append(value);
                }
                captchaForm.put("code", code.toString());
                logger.info("Put captchacode " + code.toString() + " obtained by captcha metod \"plaintext captchas\" in captchaForm");
            } else if (StringUtils.containsIgnoreCase(getCorrectBR(br), "/captchas/")) {
                logger.info("Detected captcha method \"Standard captcha\" for this host");
                final String[] sitelinks = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), "");
                if (sitelinks == null || sitelinks.length == 0) {
                    logger.warning("Standard captcha captchahandling broken!");
                    checkErrorsLastResort(br, link, null);
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                String captchaurl = null;
                for (final String linkTmp : sitelinks) {
                    if (StringUtils.containsIgnoreCase(linkTmp, "/captchas/")) {
                        captchaurl = linkTmp;
                        break;
                    }
                }
                if (StringUtils.isEmpty(captchaurl)) {
                    /* Fallback e.g. for relative URLs (e.g. subyshare.com [bad example, needs special handling anways!]) */
                    captchaurl = new Regex(getCorrectBR(br), "(/captchas/[a-z0-9]+\\.jpe?g)", Pattern.CASE_INSENSITIVE).getMatch(0);
                }
                if (captchaurl == null) {
                    logger.warning("Standard captcha captchahandling broken2!");
                    checkErrorsLastResort(br, link, null);
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String code = getCaptchaCode("xfilesharingprobasic", captchaurl, link);
                captchaForm.put("code", code);
                logger.info("Put captchacode " + code + " obtained by captcha metod \"Standard captcha\" in the form.");
            } else if (new Regex(getCorrectBR(br), "(api\\.recaptcha\\.net|google\\.com/recaptcha/api/)").patternFind()) {
                logger.info("Detected captcha method \"reCaptchaV1\" for this host");
                throw new PluginException(LinkStatus.ERROR_FATAL, "Website uses reCaptchaV1 which has been shut down by Google. Contact website owner!");
            } else if (br.containsHTML("id=\"capcode\" name= \"capcode\"")) {
                logger.info("Detected captcha method \"keycaptcha\"");
                String result = handleCaptchaChallenge(getDownloadLink(), new KeyCaptcha(this, br, getDownloadLink()).createChallenge(this));
                if (result == null) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
                if ("CANCEL".equals(result)) {
                    throw new PluginException(LinkStatus.ERROR_FATAL);
                }
                captchaForm.put("capcode", result);
            } else if (AbstractCloudflareTurnstileCaptcha.containsCloudflareTurnstileClass(br)) {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Unsupported captcha type 'Cloudflare Turnstile'");
            } else {
                /* No captcha */
            }
            /* Captcha END */
        }
    }

    protected boolean containsPlainTextCaptcha(final String correctBR) {
        return StringUtils.containsIgnoreCase(correctBR, ";background:#ccc;text-align");
    }

    /** Tries to find 1st download Form for free(and Free-Account) download. */
    public Form findFormDownload1Free(final Browser br) throws Exception {
        if (br == null) {
            return null;
        }
        final Form ret = br.getFormByInputFieldKeyValue("op", "download1");
        if (ret == null) {
            return null;
        }
        ret.remove("method_premium");
        /* Fix/Add "method_free" value if necessary. */
        final String method_free_key = "method_free";
        if (!ret.hasInputFieldByName(method_free_key) || ret.getInputFieldByName(method_free_key).getValue() == null) {
            String method_free_value = ret.getRegex("\"" + method_free_key + "\" value=\"([^<>\"]+)\"").getMatch(0);
            if (method_free_value == null || method_free_value.equals("")) {
                method_free_value = "Free Download";
            }
            ret.put(method_free_key, Encoding.urlEncode(method_free_value));
        }
        return ret;
    }

    /** Tries to find 2nd download Form for free(and Free-Account) download. */
    protected Form findFormDownload2Free(final Browser br) {
        Form ret = null;
        /* First try to find Form for video hosts with multiple qualities. */
        final Form[] forms = br.getForms();
        for (final Form form : forms) {
            final InputField op_field = form.getInputFieldByName("op");
            /* E.g. name="op" value="download_orig" */
            if (form.containsHTML("method_") && op_field != null && op_field.getValue().contains("download")) {
                ret = form;
                break;
            }
        }
        /* Nothing found? Fallback to simpler handling - this is more likely to pickup a wrong Form! */
        if (ret == null) {
            ret = br.getFormbyProperty("name", "F1");
            if (ret == null) {
                ret = br.getFormByInputFieldKeyValue("op", "download2");
            }
        }
        if (ret == null) {
            return null;
        }
        final InputField adblock_detected = ret.getInputField("adblock_detected");
        if (adblock_detected != null && StringUtils.isEmpty(adblock_detected.getValue())) {
            adblock_detected.setValue("0");
        }
        return ret;
    }

    /**
     * Tries to find download Form for premium download.
     *
     * @throws Exception
     */
    public Form findFormDownload2Premium(final DownloadLink downloadLink, final Account account, final Browser br) throws Exception {
        return br.getFormbyProperty("name", "F1");
    }

    protected String getStoredDirectUrl(final DownloadLink link, final Account account) {
        final String directurlproperty = getDownloadModeDirectlinkProperty(account);
        final String ret = link.getStringProperty(directurlproperty);
        return ret;
    }

    /**
     * Check if a stored directlink exists under property 'property' and if so, check if it is still valid (leads to a downloadable content
     * [NOT html]).
     *
     * @throws Exception
     */
    protected final String checkDirectLink(final DownloadLink link, final Account account) throws Exception {
        final String dllink = getStoredDirectUrl(link, account);
        if (dllink == null) {
            return null;
        }
        final String validDirecturl = checkDirectLinkAndSetFilesize(link, dllink, false);
        if (validDirecturl != null) {
            return validDirecturl;
        } else {
            removeStoredDirectUrl(link, account);
            return null;
        }
    }

    /**
     * Checks if a directurl leads to downloadable content and if so, returns true. <br />
     * This will also return true if the serverside connection limit has been reached. <br />
     *
     * @param link
     *            : The DownloadLink
     * @param directurl
     *            : Directurl which should lead to downloadable content
     * @param setFilesize
     *            : true = setVerifiedFileSize filesize if directurl is really downloadable
     * @throws Exception
     */
    protected final String checkDirectLinkAndSetFilesize(final DownloadLink link, final String directurl, final boolean setFilesize) throws Exception {
        if (StringUtils.isEmpty(directurl) || !directurl.startsWith("http")) {
            return null;
        }
        URLConnectionAdapter con = null;
        boolean throwException = false;
        try {
            final Browser br2 = br.cloneBrowser();
            final Request request;
            if (supportsHEADRequestForDirecturlCheck()) {
                request = br2.createHeadRequest(directurl);
            } else {
                request = br2.createGetRequest(directurl);
            }
            request.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING, "identity");
            con = openAntiDDoSRequestConnection(br2, request);
            if (con.getResponseCode() == 503) {
                /* 503 too many connections: URL is valid but we can't use it at this moment. */
                throwException = true;
                exception503ConnectionLimitReached();
                return directurl;
            } else if (looksLikeDownloadableContent(con)) {
                final long completeContentLength = con.getCompleteContentLength();
                if (setFilesize && completeContentLength > 0) {
                    if (con.isContentDecoded()) {
                        link.setDownloadSize(completeContentLength);
                    } else {
                        link.setVerifiedFileSize(completeContentLength);
                    }
                }
                return directurl;
            } else if (LinkCrawlerDeepInspector.looksLikeMpegURL(con)) {
                /* HLS download --> HLS URL is valid */
                return directurl;
            } else {
                /* Failure */
                br2.followConnection(true);
                throw new Exception("no downloadable content?" + con.getResponseCode() + "|" + con.getContentType() + "|" + con.isContentDisposition());
            }
        } catch (final Exception e) {
            /* Failure */
            if (throwException) {
                throw e;
            } else {
                logger.log(e);
                return null;
            }
        } finally {
            if (con != null) {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
        }
    }

    protected final boolean attemptStoredDownloadurlDownload(final DownloadLink link, final Account account) throws Exception {
        final String url = getStoredDirectUrl(link, account);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        logger.info("Attempting to re-use stored directurl: " + url);
        final Browser br = createNewBrowserInstance();
        if (!this.tryDownload(br, link, account, br.createGetRequest(url))) {
            /* Delete stored direct-URL so it will not be re-used next time. */
            removeStoredDirectUrl(link, account);
            return false;
        } else {
            return true;
        }
    }

    protected boolean removeStoredDirectUrl(final DownloadLink link, final Account account) {
        if (true) {
            // do not remove stored direct url until it has been replaced by newer/working one
            return false;
        }
        final String directurlproperty = getDownloadModeDirectlinkProperty(account);
        return link.removeProperty(directurlproperty);
    }

    protected boolean verifyURLFormat(final String url) {
        try {
            if (StringUtils.startsWithCaseInsensitive(url, "http")) {
                URLHelper.verifyURL(new URL(url));
                return true;
            }
        } catch (MalformedURLException ignore) {
        }
        return false;
    }

    /**
     * Returns referer to be used in availablecheck. <br>
     * This is e.g. useful for websites which restrict the embedding of videos to a specific source.
     */
    protected String getReferer(final DownloadLink link) {
        final String downloadPassword = link.getDownloadPassword();
        if (!StringUtils.isEmpty(downloadPassword) && !this.canHandle(downloadPassword) && verifyURLFormat(downloadPassword)) {
            logger.info("Using download password as referer: " + downloadPassword);
            return downloadPassword;
        }
        final String containerURL = link.getContainerUrl();
        if (!StringUtils.isEmpty(containerURL) && !this.canHandle(containerURL) && verifyURLFormat(containerURL)) {
            logger.info("Using containerURL as referer: " + containerURL);
            return containerURL;
        }
        final Class<? extends XFSConfig> cfg = this.getConfigInterface();
        if (cfg != null) {
            final String custom_referer_from_settings = PluginJsonConfig.get(cfg).getCustomReferer();
            if (!StringUtils.isEmpty(custom_referer_from_settings) && !this.canHandle(custom_referer_from_settings) && verifyURLFormat(custom_referer_from_settings)) {
                logger.info("Using custom config as referer: " + custom_referer_from_settings);
                return custom_referer_from_settings;
            }
        }
        /* No Referer at all. */
        return null;
    }

    @Override
    public boolean hasAutoCaptcha() {
        /* Assume we never got auto captcha as most services will use e.g. reCaptchaV2 nowdays. */
        return false;
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        if (isPremium(acc)) {
            /* Premium accounts don't have captchas */
            return false;
        } else if (isImagehoster()) {
            /* Image hosts usually don't have captchas. */
            return false;
        } else {
            /* Normal file hoster: Anonymous downloads & Free account downloads may have captchas */
            return true;
        }
    }

    /**
     * Return true if you know that a captcha will be required for official video download. <br>
     * This can be used so that upper handling can try to avoid captchas if configured this way.
     */
    protected Boolean requiresCaptchaForOfficialVideoDownload() {
        return Boolean.TRUE;
    }

    /** Cleans correctedBrowserRequestMap */
    @Override
    public void clean() {
        try {
            correctBrowserMap.set(null);
            super.clean();
        } finally {
            synchronized (correctedBrowserRequestMap) {
                correctedBrowserRequestMap.clear();
            }
        }
    }

    /** Traits used to cleanup html of our basic browser object and put it into correctedBR. */
    public ArrayList<String> getCleanupHTMLRegexes() {
        final ArrayList<String> regexStuff = new ArrayList<String>();
        // remove custom rules first!!! As html can change because of generic cleanup rules.
        /* generic cleanup */
        // FIXME: these patterns are really BAD as they do not support multiple div/comment tags within the match
        regexStuff.add("<\\!(\\-\\-.*?\\-\\-)>");
        regexStuff.add("(<div[^>]*style\\s*=\\s*\"[^\"]*display: ?none;\"[^>]*>.*?</div>)");
        regexStuff.add("(visibility:hidden>.*?<)");
        return regexStuff;
    }

    protected String replaceCorrectBR(Browser br, String pattern, String target) {
        /* Do not e.g. remove captcha forms from html! */
        if (StringUtils.containsIgnoreCase(pattern, "none") && (containsHCaptcha(target) || containsRecaptchaV2Class(target) || containsPlainTextCaptcha(target))) {
            return null;
        } else {
            return "";
        }
    }

    /** Removes HTML code which could break the plugin and puts it into correctedBR. */
    protected String correctBR(final Browser br) {
        synchronized (correctedBrowserRequestMap) {
            final Request request = br.getRequest();
            String html[] = correctedBrowserRequestMap.get(request);
            if (html == null) {
                html = new String[2];
                try {
                    html[1] = br.getRequest().getHtmlCode();
                } catch (final Throwable e) {
                    logger.log(e);
                    /* E.g. image content */
                    html[1] = "";
                }
                html[0] = html[1];
                final ArrayList<String> regexStuff = getCleanupHTMLRegexes();
                // remove custom rules first!!! As html can change because of generic cleanup rules.
                /* generic cleanup */
                boolean modified = false;
                if (regexStuff != null) {
                    for (final String aRegex : regexStuff) {
                        final String before = html[0];
                        final String after = applyCorrectBR(br, before, aRegex);
                        if (!StringUtils.equals(before, after)) {
                            html[0] = after;
                            modified = true;
                        }
                    }
                }
                if (modified && request != null && request.isRequested()) {
                    correctedBrowserRequestMap.put(request, html);
                } else {
                    correctedBrowserRequestMap.remove(request);
                }
            }
            /**
             * 2024-04-15: New: Replace html inside browser instance. <br>
             * By now this is rarely needed e.g. dailyuploads.net, streamplay.to
             */
            br.getRequest().setHtmlCode(html[0]);
            return html[0];
        }
    }

    protected String applyCorrectBR(Browser br, String correctedBR, String pattern) {
        while (true) {
            final String matches[] = new Regex(correctedBR, pattern).getColumn(0);
            if (matches != null && matches.length > 0) {
                for (String match : matches) {
                    final String replace = replaceCorrectBR(br, pattern, match);
                    if (replace != null) {
                        correctedBR = correctedBR.replace(match, replace);
                    }
                }
                break;
            } else {
                break;
            }
        }
        return correctedBR;
    }

    protected String getCorrectBR(Browser br) {
        synchronized (correctedBrowserRequestMap) {
            final String html[] = correctedBrowserRequestMap.get(br.getRequest());
            if (html != null) {
                return html[0];
            } else {
                return br.getRequest().getHtmlCode();
            }
        }
    }

    /**
     * Function to find the final downloadlink. <br>
     * This will also find video directurls of embedded videos if the player is 'currently visible'.
     */
    protected String getDllink(final DownloadLink link, final Account account, final Browser br, String src) {
        String dllink = null;
        for (final Pattern pattern : getDownloadurlRegexes()) {
            dllink = new Regex(src, pattern).getMatch(0);
            if (dllink != null) {
                break;
            }
        }
        if (StringUtils.isEmpty(dllink)) {
            final String cryptedScripts[] = new Regex(src, "p\\}\\((.*?)\\.split\\('\\|'\\)").getColumn(0);
            if (cryptedScripts != null && cryptedScripts.length != 0) {
                for (String crypted : cryptedScripts) {
                    dllink = decodeDownloadLink(link, account, br, crypted);
                    if (dllink != null) {
                        break;
                    }
                }
            }
        }
        if (StringUtils.isEmpty(dllink)) {
            dllink = getDllinkVideohost(link, account, br, src);
        }
        if (dllink == null && this.isImagehoster()) {
            /* Used for image hosts */
            dllink = getDllinkImagehost(link, account, br, src);
        }
        if (dllink != null) {
            dllink = Encoding.htmlOnlyDecode(dllink);
        }
        return dllink;
    }

    protected String getDllinkImagehost(DownloadLink link, Account account, Browser br, final String src) {
        /*
         * 2019-07-24: This is basically a small workaround because if a file has a "bad filename" the filename inside our URL may just look
         * like it is a thumbnail although it is not. If we find several URLs and all are the same we may still just take one of them
         * although it could be a thumbnail.
         */
        final Map<String, Integer> possibleDllinks = new HashMap<String, Integer>();
        for (final Pattern regex : getImageDownloadurlRegexes()) {
            final String[] dllinksTmp = new Regex(src, regex).getColumn(0);
            for (final String url : dllinksTmp) {
                Integer count = possibleDllinks.get(url);
                if (count == null) {
                    count = 1;
                } else {
                    count++;
                }
                possibleDllinks.put(url, count);
            }
        }
        Entry<String, Integer> best = null;
        for (Entry<String, Integer> entry : possibleDllinks.entrySet()) {
            /* Avoid downloading thumbnails */
            /* 2019-07-24: Improve recognization of thumbnails e.g. https://img67.imagetwist.com/th/123456/[a-z0-9]{12}.jpg */
            if (entry.getKey().matches(".+_t\\.[A-Za-z]{3,4}$") || entry.getKey().matches(".+/th/\\d+.*$")) {
                continue;
            } else if (best == null) {
                best = entry;
            } else if (entry.getValue() > best.getValue()) {
                best = entry;
            }
        }
        if (best == null && possibleDllinks.size() > 0) {
            logger.info("All possible image directurls have been filtered (all thumbnails?)");
        }
        if (best != null) {
            final String dllink = best.getKey();
            return dllink;
        } else {
            return null;
        }
    }

    /**
     * Tries to find stream-URL for videohosts.
     *
     * @param link
     *            TODO
     * @param account
     *            TODO
     * @param br
     *            TODO
     */
    protected String getDllinkVideohost(final DownloadLink link, final Account account, final Browser br, final String src) {
        String dllink = null;
        /* RegExes for videohosts */
        String jssource = new Regex(src, "\"?sources\"?\\s*:\\s*(\\[[^\\]]+\\])").getMatch(0);
        if (StringUtils.isEmpty(jssource)) {
            /* 2019-07-04: Wider attempt - find sources via pattern of their video-URLs. */
            final String[] jssources = new Regex(src, ":\\s*(\\[[^\\]]+\\])").getColumn(0);
            if (jssources != null && jssources.length > 0) {
                for (final String thisjssource : jssources) {
                    if (new Regex(thisjssource, "[a-z0-9]{60}/v\\.mp4").patternFind()) {
                        jssource = thisjssource;
                        break;
                    }
                }
            }
        }
        if (!StringUtils.isEmpty(jssource)) {
            logger.info("Found video json source");
            /*
             * Different services store the values we want under different names. E.g. vidoza.net uses 'res', most providers use 'label'.
             */
            final String[] possibleQualityObjectNames = new String[] { "label", "res" };
            /*
             * Different services store the values we want under different names. E.g. vidoza.net uses 'src', most providers use 'file'.
             */
            final String[] possibleStreamURLObjectNames = new String[] { "file", "src" };
            try {
                /*
                 * Important: Default is -1 so that even if only one quality is available without quality-identifier, it will be used!
                 */
                long quality_picked = -1;
                String dllink_temp = null;
                /*
                 * Important: Do not use "Plugin.restoreFromString" here as the input of this can also be js structure and not only json!!
                 */
                List<Object> ressourcelist = null;
                Map<String, String> references = new HashMap<String, String>();
                while (true) {
                    try {
                        if (references.size() > 0) {
                            final ScriptEngineManager mgr = JavaScriptEngineFactory.getScriptEngineManager(this);
                            final ScriptEngine engine = mgr.getEngineByName("JavaScript");
                            for (Entry<String, String> reference : references.entrySet()) {
                                engine.eval("var " + reference.getKey() + "=" + reference.getValue() + ";");
                            }
                            engine.eval("var response=" + jssource + ";");
                            ressourcelist = (List<Object>) JavaScriptEngineFactory.convertJavaScriptToJava(engine.get("response"));
                        } else {
                            ressourcelist = (List<Object>) JavaScriptEngineFactory.jsonToJavaObject(jssource);
                        }
                        break;
                    } catch (Exception e) {
                        // VidhideCom -> playrecord.biz
                        final EcmaError ee = Exceptions.getInstanceof(e, EcmaError.class);
                        final String undefined = ee == null ? null : new Regex(ee.getMessage(), "ReferenceError\\s*:\\s*\"(.*?)\"\\s*(is not defined|n'est pas défini|未定义)?").getMatch(0);
                        if (undefined == null || references.containsKey(undefined)) {
                            throw e;
                        }
                        final String value = new Regex(src, "var\\s*" + Pattern.quote(undefined) + "\\s*=\\s*(\\{.*?\\})\\s*;").getMatch(0);
                        if (value == null) {
                            throw e;
                        }
                        references.put(undefined, value);
                        getLogger().log(e);
                    }
                }
                final boolean onlyOneQualityAvailable = ressourcelist.size() == 1;
                final int userSelectedQuality = getPreferredStreamQuality();
                if (userSelectedQuality == -1) {
                    logger.info("Looking for BEST video stream");
                } else {
                    logger.info("Looking for user selected video stream quality: " + userSelectedQuality);
                }
                boolean foundUserSelectedQuality = false;
                for (final Object videoo : ressourcelist) {
                    /* Check for single URL without any quality information e.g. uqload.com */
                    if (videoo instanceof String && onlyOneQualityAvailable) {
                        logger.info("Only one quality available --> Returning that");
                        dllink_temp = (String) videoo;
                        if (dllink_temp.startsWith("http")) {
                            dllink = dllink_temp;
                            break;
                        }
                    }
                    final Map<String, Object> entries;
                    if (videoo instanceof Map) {
                        entries = (Map<String, Object>) videoo;
                        for (final String possibleStreamURLObjectName : possibleStreamURLObjectNames) {
                            if (entries.containsKey(possibleStreamURLObjectName)) {
                                dllink_temp = (String) entries.get(possibleStreamURLObjectName);
                                break;
                            }
                        }
                    } else {
                        entries = null;
                    }
                    if (StringUtils.isEmpty(dllink_temp)) {
                        /* No downloadurl found --> Continue */
                        continue;
                    } else if (dllink_temp.contains(".mpd")) {
                        /* 2020-05-20: This plugin cannot yet handle DASH stream downloads */
                        logger.info("Skipping DASH stream: " + dllink_temp);
                        continue;
                    }
                    /* Find quality + downloadurl */
                    long quality_temp = 0;
                    for (final String possibleQualityObjectName : possibleQualityObjectNames) {
                        try {
                            final Object quality_temp_o = entries.get(possibleQualityObjectName);
                            if (quality_temp_o != null && quality_temp_o instanceof Number) {
                                quality_temp = ((Number) quality_temp_o).intValue();
                            } else if (quality_temp_o != null && quality_temp_o instanceof String) {
                                /* E.g. '360p' */
                                final String res = new Regex((String) quality_temp_o, "(\\d+)p?$").getMatch(0);
                                if (res != null) {
                                    quality_temp = (int) Long.parseLong(res);
                                }
                            }
                            if (quality_temp > 0) {
                                break;
                            }
                        } catch (final Throwable e) {
                            /* This should never happen */
                            logger.log(e);
                            logger.info("Failed to find quality via key '" + possibleQualityObjectName + "' for current downloadurl candidate: " + dllink_temp);
                            if (!onlyOneQualityAvailable) {
                                continue;
                            }
                        }
                    }
                    if (StringUtils.isEmpty(dllink_temp)) {
                        continue;
                    } else if (quality_temp == userSelectedQuality) {
                        /* Found user selected quality */
                        logger.info("Found user selected quality: " + userSelectedQuality);
                        foundUserSelectedQuality = true;
                        quality_picked = quality_temp;
                        dllink = dllink_temp;
                        break;
                    } else {
                        /* Look for best quality */
                        if (quality_temp > quality_picked) {
                            quality_picked = quality_temp;
                            dllink = dllink_temp;
                        }
                    }
                }
                if (!StringUtils.isEmpty(dllink)) {
                    logger.info("Quality handling for multiple video stream sources succeeded - picked quality is: " + quality_picked);
                    if (foundUserSelectedQuality) {
                        logger.info("Successfully found user selected quality: " + userSelectedQuality);
                    } else {
                        logger.info("Successfully found BEST quality: " + quality_picked);
                    }
                } else {
                    logger.info("Failed to find any stream downloadurl");
                }
            } catch (final Throwable e) {
                logger.log(e);
                logger.info("BEST handling for multiple video source failed");
            }
        }
        if (StringUtils.isEmpty(dllink)) {
            /* 2019-07-04: Examplehost: vidoza.net */
            /* TODO: Check if we can remove 'regexVideoStreamDownloadURL' or integrate it in this function. */
            dllink = regexVideoStreamDownloadURL(src);
        }
        if (StringUtils.isEmpty(dllink)) {
            final String check = new Regex(src, "file\\s*:\\s*\"(https?[^<>\"]*?\\.(?:mp4|flv))\"").getMatch(0);
            if (StringUtils.isNotEmpty(check) && !StringUtils.containsIgnoreCase(check, "/images/")) {
                // jwplayer("flvplayer").onError(function()...
                dllink = check;
            }
        }
        if (StringUtils.isEmpty(dllink)) {
            /* Official video download */
            /*
             * 2019-05-30: Test - worked for: xvideosharing.com - not exactly required as getDllink will usually already return a result.
             */
            dllink = br.getRegex("href\\s*=\\s*\"(https?://[^\"]+)\"[^>]*>\\s*Direct Download Link").getMatch(0);
        }
        return dllink;
    }

    /** Generic RegEx to find common XFS stream download URLs */
    private final String regexVideoStreamDownloadURL(final String src) {
        String dllink = new Regex(src, Pattern.compile("(https?://[^/]+[^\"]+[a-z0-9]{60}/v\\.mp4)", Pattern.CASE_INSENSITIVE)).getMatch(0);
        if (StringUtils.isEmpty(dllink)) {
            /* Wider attempt */
            dllink = new Regex(src, Pattern.compile("\"(https?://[^/]+/[a-z0-9]{60}/[^\"]+)\"", Pattern.CASE_INSENSITIVE)).getMatch(0);
        }
        return dllink;
    }

    protected Class<? extends XFSConfigVideo> getVideoConfigInterface() {
        final Class<? extends XFSConfig> configInterface = this.getConfigInterface();
        if (configInterface != null && XFSConfigVideo.class.isAssignableFrom(configInterface)) {
            return (Class<? extends XFSConfigVideo>) configInterface;
        } else {
            return null;
        }
    }

    /** Returns user selected stream quality. -1 = BEST/no selection */
    protected final int getPreferredStreamQuality() {
        final Class<? extends XFSConfigVideo> cfgO = getVideoConfigInterface();
        if (cfgO == null) {
            return -1;
        }
        final XFSConfigVideo cfg = PluginJsonConfig.get(cfgO);
        final PreferredStreamQuality quality = cfg.getPreferredStreamQuality();
        switch (quality) {
        case Q2160P:
            return 2160;
        case Q1080P:
            return 1080;
        case Q720P:
            return 720;
        case Q480P:
            return 480;
        case Q360P:
            return 360;
        case BEST:
        default:
            return -1;
        }
    }

    public String decodeDownloadLink(final DownloadLink link, final Account account, final Browser br, final String s) {
        String decoded = null;
        try {
            final Regex params = new Regex(s, "'(.*?[^\\\\])',(\\d+),(\\d+),'(.*?)'");
            String p = params.getMatch(0).replaceAll("\\\\", "");
            int a = Integer.parseInt(params.getMatch(1));
            int c = Integer.parseInt(params.getMatch(2));
            String[] k = params.getMatch(3).split("\\|");
            while (c != 0) {
                c--;
                if (k[c].length() != 0) {
                    p = p.replaceAll("\\b" + Integer.toString(c, a) + "\\b", k[c]);
                }
            }
            decoded = p;
        } catch (Exception e) {
            logger.log(e);
            logger.info("js unpack failed");
        }
        String dllink = null;
        if (decoded != null) {
            dllink = getDllinkVideohost(link, account, br, decoded);
            if (StringUtils.isEmpty(dllink)) {
                /* Open regex is possible because in the unpacked JS there are usually only 1-2 URLs. */
                dllink = new Regex(decoded, "(?:\"|')(https?://[^<>\"']*?\\.(avi|flv|mkv|mp4|m3u8))(?:\"|')").getMatch(0);
            }
        }
        return dllink;
    }

    protected boolean isDllinkFile(final String url) {
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        for (final Pattern pattern : this.getDownloadurlRegexes()) {
            final String urlMatch = new Regex(url, pattern).getMatch(0);
            if (urlMatch != null) {
                return true;
            }
        }
        return false;
    }

    protected final String getDllinkHostPattern() {
        return "[A-Za-z0-9\\-\\.]*";
    }

    /** Returns pre-download-waittime (seconds as String) from inside HTML. */
    protected String regexWaittime(final Browser br) {
        return regexWaittime(br.getRequest().getHtmlCode());
    }

    /** Returns pre-download-waittime (seconds) from inside HTML. */
    protected String regexWaittime(final String html) {
        String waitStr = new Regex(html, "id=(?:\"|\\')countdown_str(?:\"|\\')[^>]*>[^<>]*<span id=[^>]*>\\s*(\\d+)\\s*</span>").getMatch(0);
        if (waitStr == null) {
            waitStr = new Regex(html, "class=\"seconds\"[^>]*>\\s*(\\d+)\\s*</span>").getMatch(0);
            if (waitStr == null) {
                /* More open RegEx */
                waitStr = new Regex(html, "class=\"seconds\"[^>]*>\\s*(\\d+)\\s*<").getMatch(0);
                if (waitStr == null) {
                    /* 2024-10-08: via id e.g. uploadfox.net, file-up.org */
                    waitStr = new Regex(html, "id=\"seconds\"[^>]*>\\s*(\\d+)\\s*<").getMatch(0);
                }
            }
        }
        return waitStr;
    }

    /** Returns list of possible final downloadurl patterns. Match 0 will be used to find downloadurls in html source! */
    protected List<Pattern> getDownloadurlRegexes() {
        final List<Pattern> patterns = new ArrayList<Pattern>();
        /* 2020-04-01: TODO: Maybe add this part to the end: (\\s+|\\s*>|\\s*\\)|\\s*;) (?) */
        /* Allow ' in URL */
        patterns.add(Pattern.compile("\"" + String.format("(https?://(?:\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}|%s)(?::\\d+)?/(?:files|d|cgi\\-bin/dl\\.cgi|dl)/(?:\\d+/)?[a-z0-9]+/[^<>\"/]*)", this.getDllinkHostPattern()) + "\""));
        /* Allow ' in URL but must end on '); */
        patterns.add(Pattern.compile(String.format("(https?://(?:\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}|%s)(?::\\d+)?/(?:files|d|cgi\\-bin/dl\\.cgi|dl)/(?:\\d+/)?[a-z0-9]+/[^<>\"/']*)", this.getDllinkHostPattern()) + "'\\s*\\)\\s*;"));
        /* Don't allow ' in URL */
        patterns.add(Pattern.compile(String.format("(https?://(?:\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}|%s)(?::\\d+)?/(?:files|d|cgi\\-bin/dl\\.cgi|dl)/(?:\\d+/)?[a-z0-9]+/[^<>\"'/]*)", this.getDllinkHostPattern())));
        return patterns;
    }

    /** Returns list of possible final image-host-downloadurl patterns. Match 0 will be used to find downloadurls in html source! */
    protected List<Pattern> getImageDownloadurlRegexes() {
        final List<Pattern> patterns = new ArrayList<Pattern>();
        // Base URL components
        String protocol = "https?://";
        String ipAddress = "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}";
        String hostPattern = String.format("(?:%s|%s)", ipAddress, this.getDllinkHostPattern());
        String port = "(?:\\d+)?";
        String fileNamePattern = "[^<>\"'\\[\\]]+";
        // Optional protocol and host for relative URLs
        String optionalProtocolAndHost = String.format("(?:%s%s%s)?", protocol, hostPattern, port);
        // Pattern 1: /img/{digits}/{filename}
        String imgWithDigits = String.format("(%s/img/\\d+/%s)", optionalProtocolAndHost, fileNamePattern);
        patterns.add(Pattern.compile(imgWithDigits));
        // Pattern 2: /img/{alphanumeric}/{filename}, for example: picbaron.com
        String imgWithAlphaNum = String.format("(%s/img/[a-z0-9]+/%s)", optionalProtocolAndHost, fileNamePattern);
        patterns.add(Pattern.compile(imgWithAlphaNum));
        // Pattern 3: /img/{filename} (direct)
        String imgDirect = String.format("(%s/img/%s)", optionalProtocolAndHost, fileNamePattern);
        patterns.add(Pattern.compile(imgDirect));
        // Pattern 4: /i/{digits}/{filename}
        String iWithDigits = String.format("(%s/i/\\d+/%s)", optionalProtocolAndHost, fileNamePattern);
        patterns.add(Pattern.compile(iWithDigits));
        // Pattern 5: /i/{digits}/{filename} (excluding thumbnails with _t.ext)
        String iWithDigitsNoThumbnails = String.format("(%s/i/\\d+/%s(?!_t\\.[A-Za-z]{3,4}))", optionalProtocolAndHost, fileNamePattern);
        patterns.add(Pattern.compile(iWithDigitsNoThumbnails));
        return patterns;
    }

    /**
     * Prevents more than one free download from starting at a given time. One step prior to dl.startDownload(), it adds a slot to maxFree
     * which allows the next singleton download to start, or at least try.
     *
     * This is needed because xfileshare(website) only throws errors after a final dllink starts transferring or at a given step within pre
     * download sequence. But this template(XfileSharingProBasic) allows multiple slots(when available) to commence the download sequence,
     * this.setstartintival does not resolve this issue. Which results in x(20) captcha events all at once and only allows one download to
     * start. This prevents wasting peoples time and effort on captcha solving and|or wasting captcha trading credits. Users will experience
     * minimal harm to downloading as slots are freed up soon as current download begins.
     *
     * @param num
     *            : (+1|-1)
     */
    protected void controlMaxFreeDownloads(final Account account, final DownloadLink link, final int num) {
        if (account == null) {
            final AtomicInteger freeRunning = getFreeRunning();
            synchronized (freeRunning) {
                final int before = freeRunning.get();
                final int after = before + num;
                freeRunning.set(after);
                logger.info("freeRunning(" + link.getName() + ")|max:" + getMaxSimultanFreeDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
            }
        }
    }

    protected void getPage(final Browser br, String page, final boolean correctBr) throws Exception {
        setCorrectBrowserFlag(br, correctBr);
        getPage(br, page);
    }

    private final ThreadLocal<WeakHashMap<Browser, Boolean>> correctBrowserMap = new ThreadLocal<WeakHashMap<Browser, Boolean>>();

    @Override
    protected void sendRequest(Browser br, Request request) throws Exception {
        super.sendRequest(br, request);
        if (wasCorrectBrowserFlagSet(br)) {
            correctBR(br);
        }
    }

    protected void setCorrectBrowserFlag(final Browser br, boolean flag) {
        WeakHashMap<Browser, Boolean> map = correctBrowserMap.get();
        if (flag && map == null) {
            // see wasCorrectBrowserFlagSet method
            return;
        } else if (map != null) {
            map.put(br, flag);
        } else if (!flag) {
            if (map == null) {
                map = new WeakHashMap<Browser, Boolean>();
                correctBrowserMap.set(map);
            }
            map.put(br, Boolean.FALSE);
        }
    }

    protected boolean wasCorrectBrowserFlagSet(final Browser br) {
        final WeakHashMap<Browser, Boolean> map = correctBrowserMap.get();
        return map == null || Boolean.TRUE.equals(map.remove(br));
    }

    /**
     * Handles pre download (pre-captcha) wait time.
     */
    protected void waitTime(final DownloadLink link, final long timeBefore) throws PluginException {
        /* Ticket Time */
        final String waitStr = regexWaittime(br);
        if (this.preDownloadWaittimeSkippable()) {
            /* Very rare case! */
            logger.info("Skipping pre-download waittime: " + waitStr);
            return;
        } else if (waitStr == null) {
            logger.info("Found no waittime");
            return;
        } else if (!waitStr.matches("\\d+")) {
            logger.info("Got invalid wait time string: " + waitStr);
            return;
        }
        logger.info("Found waittime, parsing waittime: " + waitStr);
        final int waitSeconds = Integer.parseInt(waitStr);
        waitTime(link, timeBefore, waitSeconds);
    }

    protected void waitTime(final DownloadLink link, final long timeBefore, int waitSeconds) throws PluginException {
        if (waitSeconds <= 0) {
            logger.info("Strange: Got 0 wait seconds");
            return;
        }
        /*
         * Check how much time has passed during eventual captcha event before this function has been called and see how much time is left
         * to wait.
         */
        int passedTime = (int) ((Time.systemIndependentCurrentJVMTimeMillis() - timeBefore) / 1000);
        waitSeconds -= passedTime;
        if (passedTime > 0) {
            /* This usually means that the user had to solve a captcha which cuts down the remaining time we have to wait. */
            logger.info("Total passed time during captcha: " + passedTime);
        }
        if (waitSeconds > 0) {
            logger.info("Waiting final waittime: " + waitSeconds);
            sleep(waitSeconds * 1000l, link);
        } else if (waitSeconds < waitSeconds) {
            /* User needed more time to solve the captcha so there is no waittime left :) */
            logger.info("Congratulations: Time to solve captcha was higher than waittime --> No waittime left");
        } else {
            /* No wait time at all */
            logger.info("Found no waittime");
        }
    }

    /**
     * Fix filenames for HLS video downloads. <br>
     * Ignores HLS audio for now.
     */
    protected void fixFilenameHLSDownload(final DownloadLink link) {
        /* Either final filename from previous download attempt or filename found in HTML. */
        final String orgNameWithExt = link.getName();
        if (orgNameWithExt != null) {
            link.setFinalFileName(this.applyFilenameExtension(orgNameWithExt, ".mp4"));
        }
    }

    protected String getFileNameFromConnection(final URLConnectionAdapter connection, final DownloadLink link) {
        String fileName = getFileNameFromDispositionHeader(connection);
        if (StringUtils.isEmpty(fileName)) {
            fileName = Plugin.getFileNameFromURL(connection.getURL());
            fileName = URLEncode.decodeURIComponent(fileName);
            /* Ignore typical meaningless names. */
            if (fileName != null && fileName.matches("(?i)video\\.(mp4|mkv)$")) {
                fileName = null;
            }
        }
        return fileName;
    }

    /**
     * This fixes filenames from all xfs modules: file hoster, audio/video streaming (including transcoded video), or blocked link checking
     * which is based on fuid.
     *
     * @version 0.4
     * @author raztoki
     */
    protected void fixFilename(final URLConnectionAdapter connection, final DownloadLink link) {
        /* TODO: Maybe make use of already given methods to e.g. extract filename without extension from String. */
        /* Server filename with extension */
        String servName = null;
        /* Server filename without extension */
        String servExt = null;
        /* Either final filename from previous download attempt or filename found in HTML. */
        final String orgNameExt = link.getName();
        /* Extension of orgNameExt */
        final String orgExt;
        if (!StringUtils.isEmpty(orgNameExt) && StringUtils.contains(orgNameExt, ".")) {
            orgExt = orgNameExt.substring(orgNameExt.lastIndexOf("."));
        } else {
            orgExt = null;
        }
        /* Previous (e.h. html) filename without extension */
        final String orgName;
        if (!StringUtils.isEmpty(orgExt)) {
            orgName = new Regex(orgNameExt, "^(.+)" + Pattern.quote(orgExt) + "$").getMatch(0);
        } else {
            /* No extension given */
            orgName = orgNameExt;
        }
        String servNameExt = connection != null ? getFileNameFromConnection(connection, link) : null;
        if (!StringUtils.isEmpty(servNameExt) && !StringUtils.contains(servNameExt, ".")) {
            /* Extension according to Content-Type header */
            final String mimeExt = connection != null ? getExtensionFromMimeType(connection) : null;
            if (mimeExt != null) {
                servNameExt = servNameExt + "." + mimeExt;
            }
        }
        if (!StringUtils.isEmpty(servNameExt) && StringUtils.contains(servNameExt, ".")) {
            servExt = servNameExt.substring(servNameExt.lastIndexOf("."));
            servName = new Regex(servNameExt, "(.+)" + Pattern.quote(servExt)).getMatch(0);
        } else {
            /* No extension available */
            servName = servNameExt;
        }
        final String finalFileName;
        if (StringUtils.equalsIgnoreCase(orgName, this.getFUIDFromURL(link))) {
            /* Current filename only consists of fuid --> Prefer full server filename */
            finalFileName = servNameExt;
            logger.info("fixFileName case 1: prefer servNameExt: Use servNameExt");
        } else if (StringUtils.isEmpty(orgExt) && !StringUtils.isEmpty(servExt) && (StringUtils.containsIgnoreCase(servName, orgName) && !StringUtils.equalsIgnoreCase(servName, orgName))) {
            /*
             * When partial match of filename exists. eg cut off by quotation mark miss match, or orgNameExt has been abbreviated by hoster
             * --> Prefer server filename
             */
            finalFileName = servNameExt;
            logger.info("fixFileName case 2: prefer servNameExt: Use servNameExt");
        } else if (!StringUtils.isEmpty(servExt) && !StringUtils.equalsIgnoreCase(orgExt, servExt)) {
            /*
             * Current filename has or has no extension given but server filename has other extension --> Swap extensions, trust the name we
             * have but use extension from server
             */
            finalFileName = orgName + servExt;
            logger.info(String.format("fixFileName case 3: Use orgName + servExt | Old ext: %s | New ext: %s", orgExt, servExt));
        } else {
            finalFileName = orgNameExt;
            logger.info("fixFileName case 4: prefer orgNameExt");
        }
        logger.info("fixFileName: link=" + orgNameExt + "|server=" + servNameExt + "|final=" + finalFileName);
        link.setFinalFileName(finalFileName);
    }

    /** Returns unique id from inside URL - usually with this pattern: [a-z0-9]{12} */
    public String getFUIDFromURL(final DownloadLink link) {
        final URL_TYPE type = getURLType(link);
        return getFUID(link, type);
    }

    /**
     * In some cases, URL may contain filename which can be used as fallback e.g. 'https://host.tld/<fuid>/<filename>(\\.html)?'. <br>
     * Examples without '.html' ending: vipfile.cc, prefiles.com
     */
    public String getFilenameFromURL(final DownloadLink link) {
        try {
            String result = null;
            final String url_name_RegEx = "/[a-z0-9]{12}/(.*?)(?:\\.html|\\?|$)";
            /**
             * It's important that we check the contentURL too as we do alter pluginPatternMatcher in { @link
             * #correctDownloadLink(DownloadLink) }
             */
            final String contentURL = getPluginContentURL(link);
            if (contentURL != null) {
                result = new Regex(new URL(contentURL).getPath(), url_name_RegEx).getMatch(0);
            }
            if (result == null) {
                result = new Regex(new URL(link.getPluginPatternMatcher()).getPath(), url_name_RegEx).getMatch(0);
            }
            return result;
        } catch (MalformedURLException e) {
            logger.log(e);
        }
        return null;
    }

    protected String getFallbackFilename(final DownloadLink link, final Browser br) {
        String filenameURL = this.getFilenameFromURL(link);
        if (filenameURL != null) {
            filenameURL = URLEncode.decodeURIComponent(filenameURL);
            if (getFileNameExtensionFromString(filenameURL, null) == null) {
                if (this.internal_isVideohoster_enforce_video_filename(link, br)) {
                    return filenameURL + ".mp4";
                } else if (isImagehoster()) {
                    return filenameURL + ".jpg";
                }
            }
            return filenameURL;
        } else {
            if (this.internal_isVideohoster_enforce_video_filename(link, br)) {
                return this.getFUIDFromURL(link) + ".mp4";
            } else if (this.isImagehoster()) {
                return this.getFUIDFromURL(link) + ".jpg";
            } else {
                return this.getFUIDFromURL(link);
            }
        }
    }

    protected void handlePassword(final Form pwform, final DownloadLink link) throws PluginException {
        if (isPasswordProtectedHTML(this.br, pwform)) {
            link.setPasswordProtected(true);
            if (pwform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            logger.info("URL is password protected");
            String passCode = link.getDownloadPassword();
            if (passCode == null) {
                passCode = getUserInput("Password?", link);
                if (StringUtils.isEmpty(passCode)) {
                    logger.info("User has entered blank password, exiting handlePassword");
                    link.setDownloadPassword(null);
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Pre-Download Password not provided");
                }
            }
            logger.info("Put password \"" + passCode + "\" entered by user in the DLForm.");
            pwform.put("password", Encoding.urlEncode(passCode));
            link.setDownloadPassword(passCode);
        } else {
            link.setPasswordProtected(false);
        }
    }

    /**
     * Checks for (-& handles) all kinds of errors e.g. wrong captcha, wrong downloadpassword, waittimes and server error-responsecodes such
     * as 403, 404 and 503. <br />
     * checkAll: If enabled, ,this will also check for wrong password, wrong captcha and 'Skipped countdown' errors. <br/>
     */
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account, final boolean checkAll) throws NumberFormatException, PluginException {
        if (checkAll) {
            if (new Regex(html, "(?i)>\\s*Wrong password").patternFind()) {
                if (link.isPasswordProtected()) {
                    final String userEnteredPassword = link.getDownloadPassword();
                    /* handle password has failed in the past, additional try catching / resetting values */
                    logger.warning("Wrong password, the entered password \"" + userEnteredPassword + "\" is wrong, retrying...");
                    link.setDownloadPassword(null);
                    throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
                } else {
                    /*
                     * 2020-03-26: Extremely rare case: Either plugin failure or serverside failure e.g. URL is password protected but
                     * website does never ask for the password e.g. 2020-03-26: ddl.to. We cannot use link.getDownloadPassword() to check
                     * this because users can enter download passwords at any time no matter whether they're required/used or not.
                     */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Got error 'wrong password' but website never prompted for one");
                }
            } else if (new Regex(html, "(?i)>\\s*Wrong captcha").patternFind()) {
                logger.warning("Wrong captcha (or wrong password as well)!");
                if (this.getChallengeRound() >= 1) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                } else {
                    /* This should never happen. Either developer mistake, broken plugin or broken filehost website. */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server says 'wrong captcha' but never prompted for one");
                }
            } else if (new Regex(html, "(?i)>\\s*Skipped countdown\\s*<").patternFind()) {
                /* 2019-08-28: e.g. "<br><b class="err">Skipped countdown</b><br>" */
                throw new PluginException(LinkStatus.ERROR_FATAL, "Fatal countdown error (countdown skipped)");
            }
        }
        /** Wait time reconnect handling */
        final String limitBasedOnNumberofFilesAndTime = new Regex(html, "(?i)>\\s*(You have reached the maximum limit \\d+ files in \\d+ hours)").getMatch(0);
        final String preciseWaittime = new Regex(html, "(?i)((You have reached the download(\\-| )limit|You have to wait)[^<>]+)").getMatch(0);
        if (preciseWaittime != null) {
            /* Reconnect waittime with given (exact) waittime usually either up to the minute or up to the second. */
            final String tmphrs = new Regex(preciseWaittime, "(?i)\\s*(\\d+)\\s*hours?").getMatch(0);
            final String tmpmin = new Regex(preciseWaittime, "(?i)\\s*(\\d+)\\s*minutes?").getMatch(0);
            final String tmpsec = new Regex(preciseWaittime, "(?i)\\s*(\\d+)\\s*seconds?").getMatch(0);
            final String tmpdays = new Regex(preciseWaittime, "(?i)\\s*(\\d+)\\s*days?").getMatch(0);
            int waittime;
            if (tmphrs == null && tmpmin == null && tmpsec == null && tmpdays == null) {
                /* This should not happen! This is an indicator of developer-failure! */
                logger.info("Waittime RegExes seem to be broken - using default waittime");
                waittime = 60 * 60 * 1000;
            } else {
                int minutes = 0, seconds = 0, hours = 0, days = 0;
                if (tmphrs != null) {
                    hours = Integer.parseInt(tmphrs);
                }
                if (tmpmin != null) {
                    minutes = Integer.parseInt(tmpmin);
                }
                if (tmpsec != null) {
                    seconds = Integer.parseInt(tmpsec);
                }
                if (tmpdays != null) {
                    days = Integer.parseInt(tmpdays);
                }
                waittime = ((days * 24 * 3600) + (3600 * hours) + (60 * minutes) + seconds + 1) * 1000;
            }
            logger.info("Detected reconnect waittime (milliseconds): " + waittime);
            /* Not enough wait time to reconnect -> Wait short and retry */
            final String errMsg = "Download limit reached or wait until next download can be started";
            if (account != null) {
                /*
                 * 2020-04-17: Some hosts will have trafficlimit and e.g. only allow one file every X minutes so his errormessage might be
                 * confusing to some users. Now it should cover both cases at the same time.
                 */
                throw new AccountUnavailableException(errMsg, waittime);
            } else {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, errMsg, waittime);
            }
        } else if (limitBasedOnNumberofFilesAndTime != null) {
            /*
             * 2019-05-09: New: Seems like XFS owners can even limit by number of files inside specified timeframe. Example: hotlink.cc; 150
             * files per 24 hours
             */
            /* Typically '>You have reached the maximum limit 150 files in 24 hours' */
            ipBlockedOrAccountLimit(link, account, limitBasedOnNumberofFilesAndTime, 15 * 60 * 1000l);
        } else if (StringUtils.containsIgnoreCase(html, "You're using all download slots for IP")) {
            ipBlockedOrAccountLimit(link, account, "You're using all download slots for IP...", 5 * 60 * 1000l);
        } else if (StringUtils.containsIgnoreCase(html, "Error happened when generating Download Link")) {
            /* Rare issue */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 'Error happened when generating Download Link'", 10 * 60 * 1000l);
        }
        /** Error handling for premiumonly links */
        final String premiumOnlyMessage = getPremiumOnlyErrorMessage(br);
        if (premiumOnlyMessage != null) {
            throw new AccountRequiredException(premiumOnlyMessage);
        } else if (isPremiumOnly(br)) {
            throw new AccountRequiredException();
        } else if (new Regex(html, "(?i)>\\s*Expired download session").patternFind()) {
            /* Rare error */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 'Expired download session'", 10 * 60 * 1000l);
        } else if (isServerUnderMaintenance(br)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server is under maintenance", 30 * 60 * 1000l);
        }
        /* Host-type specific errors */
        /* Videohoster */
        if (new Regex(html, "(?i)>\\s*Video is processing now").patternFind()) {
            /* E.g. '<div id="over_player_msg">Video is processing now. <br>Conversion stage: <span id='enc_pp'>...</span></div>' */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Not (yet) downloadable: Video is still being encoded or broken", 10 * 60 * 1000l);
        } else if (br.containsHTML("(?i)>\\s*Downloads disabled for this file")) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Uploader has disabled downloads for this file");
        } else if (br.containsHTML("(?i)>\\s*Downloads are disabled for your country")) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Downloads are disabled for your country", 60 * 60 * 1000l);
        } else if (br.containsHTML(">\\s*File was locked by administrator")) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "File was locked by administrator");
        }
        /*
         * Errorhandling for accounts that are valid but cannot be used yet because the user has to add his mail to the account via website.
         * E.g. accounts which have been generated via balance/points of uploaders' accounts. This should be a rare case. In this case,
         * every request you do on the website will redirect to /?op=my_account along with an errormessage (sometimes).
         */
        if (account != null && (StringUtils.containsIgnoreCase(br.getURL(), "op=my_account") || StringUtils.containsIgnoreCase(br.getRedirectLocation(), "op=my_account"))) {
            /* Attempt to make this work language-independant: Rely only on URL and NOT html! */
            final String accountErrorMsg;
            if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                accountErrorMsg = String.format("Ergänze deine E-Mail Adresse unter %s/?op=my_account um diesen Account verwenden zu können!", this.getHost());
            } else {
                accountErrorMsg = String.format("Go to %s/?op=my_account and enter your e-mail in order to be able to use this account!", this.getHost());
            }
            throw new AccountUnavailableException(accountErrorMsg, 10 * 60 * 1000l);
        }
        checkResponseCodeErrors(br.getHttpConnection());
    }

    protected void checkErrorsLoginWebsite(final Browser br, final Account account) throws Exception {
    }

    /** Throws appropriate Exception depending on whether or not an account is given. */
    private void ipBlockedOrAccountLimit(final DownloadLink link, final Account account, final String errorMsg, final long waitMillis) throws PluginException {
        if (account != null) {
            throw new AccountUnavailableException(errorMsg, waitMillis);
        } else {
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, errorMsg, waitMillis);
        }
    }

    /** Use this during download handling instead of just throwing PluginException with LinkStatus ERROR_PLUGIN_DEFECT! */
    protected void checkErrorsLastResort(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        logger.info("Last resort errorhandling");
        if (account != null && br.getHttpConnection().getResponseCode() == 200 && br.containsHTML("<html[^>]*>") && !this.isLoggedin(br)) {
            /* TODO: Maybe add a better check e.g. access mainpage and check loggedin state */
            throw new AccountUnavailableException("Session expired?", 5 * 60 * 1000l);
        }
        final long waitMillis = 5 * 60 * 1000l;
        String website_error = br.getRegex("class=\"[^\"]*(?:err|alert-danger)[^\"]*\"[^>]*>([^<]+)<").getMatch(0);
        if (!StringUtils.isEmpty(website_error)) {
            website_error = Encoding.htmlDecode(website_error).trim();
            logger.info("Found website error: " + website_error);
            if (link == null) {
                throw new AccountUnavailableException(website_error, waitMillis);
            } else {
                throw new PluginException(LinkStatus.ERROR_FATAL, website_error);
            }
        }
        /* 2020-06-05 E.g. <div id="over_player_msg">File is awaiting for moderation</div> */
        String website_error_videoplayer = br.getRegex("id=\"over_player_msg\"[^>]*?>([^<>\"]+)<").getMatch(0);
        if (website_error_videoplayer != null) {
            website_error = Encoding.htmlDecode(website_error_videoplayer).trim();
            logger.info("Found website videoplayer error: " + website_error_videoplayer);
            throw new PluginException(LinkStatus.ERROR_FATAL, website_error_videoplayer);
        }
        if (br.getRequest().getHtmlCode().length() == 0) {
            final String errormessage = "Got blank page";
            if (link == null) {
                /* No DownloadLink -> Assume that this error happened during login. */
                throw new AccountUnavailableException(errormessage, waitMillis);
            } else {
                throw new PluginException(LinkStatus.ERROR_FATAL, errormessage);
            }
        } else if (br.getRequest().getHtmlCode().length() <= 100 && !br.containsHTML("<html")) {
            /* Assume that we got a small plaintext error response */
            final String plaintextError = br.getRequest().getHtmlCode().trim();
            if (link == null) {
                /* No DownloadLink -> Assume that this error happened during login. */
                throw new AccountUnavailableException(plaintextError, waitMillis);
            } else {
                throw new PluginException(LinkStatus.ERROR_FATAL, plaintextError);
            }
        }
        logger.warning("Unknown error happened");
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    /** Handles all kinds of error-responsecodes! Same for API and website! */
    public void checkResponseCodeErrors(final URLConnectionAdapter con) throws PluginException {
        if (con == null) {
            return;
        }
        final long responsecode = con.getResponseCode();
        if (responsecode == 403) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
        } else if (responsecode == 404) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
        } else if (responsecode == 416) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 416", 5 * 60 * 1000l);
        } else if (responsecode == 500) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 500", 5 * 60 * 1000l);
        } else if (responsecode == 503) {
            exception503ConnectionLimitReached();
        }
    }

    private void exception503ConnectionLimitReached() throws PluginException {
        throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server error 503 connection limit reached", 15 * 60 * 1000l);
    }

    /**
     * Handles all kinds of errors which can happen if we get the final downloadlink but we get html code instead of the file we want to
     * download.
     */
    public void checkServerErrors(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        final String html = getCorrectBR(br);
        if (new Regex(html, "^(No file|error_nofile|Not Found)$").patternFind()) {
            /* Possibly dead file but it is supposed to be online so let's wait and retry! */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 'No file'", 30 * 60 * 1000l);
        } else if (new Regex(html, "^Wrong IP$").patternFind()) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error: 'Wrong IP'", 2 * 60 * 60 * 1000l);
        } else if (new Regex(html, "^Expired$").patternFind()) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error: 'Expired'", 2 * 60 * 60 * 1000l);
        } else if (new Regex(html, "(^File Not Found$|<h1>404 Not Found</h1>)").patternFind()) {
            /* most likely result of generated link that has expired -raztoki */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 30 * 60 * 1000l);
        }
    }

    protected boolean supports_lifetime_account() {
        return false;
    }

    @Deprecated
    /** TODO: Find out where this is used. Lifetime accounts are usually not used for XFS filehosts. */
    protected boolean is_lifetime_account(final Browser br) {
        return br.getRegex("(?i)>\\s*Premium account expire\\s*</TD><TD><b>Lifetime</b>").matches();
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        if (this.enableAccountApiOnlyMode()) {
            return this.fetchAccountInfoAPI(this.br, account);
        } else {
            return this.fetchAccountInfoWebsite(account);
        }
    }

    protected void fetchAccountInfoWebsiteStorage(final Browser br, final Account account, final AccountInfo ai) throws Exception {
        final String space[] = new Regex(getCorrectBR(br), ">\\s*Used space:\\s*</td>.*?<td.*?b>([0-9\\.]+) ?(KB|MB|GB|TB)?</b>").getRow(0);
        if ((space != null && space.length != 0) && (space[0] != null && space[1] != null)) {
            /* free users it's provided by default */
            ai.setUsedSpace(space[0] + " " + space[1]);
        } else if ((space != null && space.length != 0) && space[0] != null) {
            /* premium users the Mb value isn't provided for some reason... */
            ai.setUsedSpace(space[0] + "Mb");
        }
    }

    protected void fetchAccountInfoWebsiteTraffic(final Browser br, final Account account, final AccountInfo ai) throws Exception {
        /*
         * trafficleft is usually not given via API so we'll have to check for it via website. Also we do not trsut 'unlimited traffic' via
         * API yet.
         */
        String trafficLeftStr = regExTrafficLeft(br);
        /* Example non english: brupload.net */
        final boolean userHasUnlimitedTraffic = trafficLeftStr != null && trafficLeftStr.matches(".*?(nlimited|Ilimitado).*?");
        if (trafficLeftStr != null && !userHasUnlimitedTraffic && !trafficLeftStr.equalsIgnoreCase("Mb")) {
            trafficLeftStr = Encoding.htmlDecode(trafficLeftStr).trim();
            /* Need to set 0 traffic left, as getSize returns positive result, even when negative value supplied. */
            long trafficLeft = 0;
            if (trafficLeftStr.startsWith("-")) {
                /* Negative traffic value = User downloaded more than he is allowed to (rare case) --> No traffic left */
                trafficLeft = 0;
            } else {
                trafficLeft = SizeFormatter.getSize(trafficLeftStr);
            }
            /* 2019-02-19: Users can buy additional traffic packages: Example(s): subyshare.com */
            final String usableBandwidth = br.getRegex("Usable Bandwidth\\s*<span[^>]*>\\s*([0-9\\.]+\\s*[TGMKB]+)\\s*/\\s*[0-9\\.]+\\s*[TGMKB]+\\s*<").getMatch(0);
            if (usableBandwidth != null) {
                trafficLeft = Math.max(trafficLeft, SizeFormatter.getSize(usableBandwidth));
            }
            ai.setTrafficLeft(trafficLeft);
        } else {
            ai.setUnlimitedTraffic();
        }
    }

    protected AccountInfo fetchAccountInfoWebsite(final Account account) throws Exception {
        AccountInfo ai = new AccountInfo();
        loginWebsite(null, account, true);
        /*
         * Only access URL if we haven't accessed it before already. Some sites will redirect to their Account-Info page right after
         * logging-in or our login-function when it is verifying cookies and not performing a full login.
         */
        if (br.getURL() == null || !br.getURL().contains(getRelativeAccountInfoURL())) {
            getPage(this.getMainPage() + getRelativeAccountInfoURL());
        }
        boolean apiSuccess = false;
        obtainAccountInfoFromAPI: try {
            final String apikey = this.findAPIKey(this.br.cloneBrowser());
            if (apikey == null) {
                break obtainAccountInfoFromAPI;
            }
            logger.info("Found apikey --> Trying to get AccountInfo via API: " + apikey);
            /* Save apikey for later usage */
            synchronized (account) {
                account.setProperty(PROPERTY_ACCOUNT_apikey, apikey);
                try {
                    ai = this.fetchAccountInfoAPI(this.br.cloneBrowser(), account);
                    apiSuccess = true;
                } catch (final Throwable e) {
                    e.printStackTrace();
                    logger.warning("Failed to find accountinfo via API even though apikey is given; probably serverside API failure --> Fallback to website handling");
                    /* Do not store invalid API key */
                    account.removeProperty(PROPERTY_ACCOUNT_apikey);
                }
            }
        } catch (final Throwable e) {
            /*
             * 2019-08-16: All kinds of errors may happen when trying to access the API. It is preferable if it works but we cannot rely on
             * it working so we need that website fallback!
             */
            logger.info("Failed to find apikey (with Exception) --> Continuing via website");
            logger.log(e);
        }
        final boolean devDebugTrustAPIInfo = false;
        if (apiSuccess && devDebugTrustAPIInfo && DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* Trust API info */
            logger.info("Successfully found complete AccountInfo via API");
            /* API with trafficleft value is uncommon -> Make sure devs easily take note of this! */
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                /* Devs only */
                String accStatus;
                if (ai.getStatus() != null && !ai.getStatus().startsWith("[API] ")) {
                    accStatus = ai.getStatus();
                } else {
                    accStatus = account.getType().toString();
                }
                ai.setStatus("[API] " + accStatus);
            }
            return ai;
        }
        if (apiSuccess) {
            /* No info from API available */
            logger.info("Found AccountInfo via API but trying to obtain trafficleft value from website as it is usually not given via API");
        }
        fetchAccountInfoWebsiteTraffic(br, account, ai);
        if (apiSuccess) {
            logger.info("Successfully found AccountInfo without trafficleft via API (fetched trafficleft via website)");
            return ai;
        }
        fetchAccountInfoWebsiteStorage(br, account, ai);
        if (supports_lifetime_account() && is_lifetime_account(br)) {
            ai.setValidUntil(-1);
            setAccountLimitsByType(account, AccountType.LIFETIME);
        } else {
            final Long expire_milliseconds = fetchAccountInfoWebsiteExpireDate(br, account, ai);
            if (expire_milliseconds == null) {
                logger.info("Account is a FREE account as no expiredate has been found");
                ai.setValidUntil(-1);
                setAccountLimitsByType(account, AccountType.FREE);
            } else if (expire_milliseconds < 0) {
                logger.info("Premium expired --> Free account");
                ai.setValidUntil(-1);
                setAccountLimitsByType(account, AccountType.FREE);
            } else if (expire_milliseconds == Long.MAX_VALUE) {
                logger.info("Lifetime Premium account");
                ai.setValidUntil(-1);
                setAccountLimitsByType(account, AccountType.LIFETIME);
            } else {
                logger.info("Premium account");
                ai.setValidUntil(expire_milliseconds);
                setAccountLimitsByType(account, AccountType.PREMIUM);
            }
        }
        return ai;
    }

    protected Long fetchAccountInfoWebsiteExpireDate(Browser br, Account account, AccountInfo ai) throws Exception {
        /* 2019-07-11: It is not uncommon for XFS websites to display expire-dates even though the account is not premium anymore! */
        final AtomicBoolean isPreciseTimestampFlag = new AtomicBoolean(false);
        final Long expire_milliseconds_from_expiredate = findExpireTimestamp(account, br, isPreciseTimestampFlag);
        long expire_milliseconds_precise_to_the_second = -1;
        final String[] supports_precise_expire_date = (isPreciseTimestampFlag.get() && expire_milliseconds_from_expiredate != null) ? null : this.supportsPreciseExpireDate();
        if (supports_precise_expire_date != null && supports_precise_expire_date.length > 0) {
            /*
             * A more accurate expire time, down to the second. Usually shown on 'extend premium account' page. Case[0] e.g. 'flashbit.cc',
             * Case [1] e.g. takefile.link, example website which has no precise expiredate at all: anzfile.net
             */
            final List<String> paymentURLs;
            final String last_working_payment_url = this.getPluginConfig().getStringProperty("property_last_working_payment_url", null);
            if (StringUtils.isNotEmpty(last_working_payment_url)) {
                paymentURLs = new ArrayList<String>();
                logger.info("Found stored last_working_payment_url --> Trying this first in an attempt to save http requests: " + last_working_payment_url);
                paymentURLs.add(last_working_payment_url);
                /* Add all remaining URLs, start with the last working one */
                for (final String paymentURL : supports_precise_expire_date) {
                    if (!paymentURLs.contains(paymentURL)) {
                        paymentURLs.add(paymentURL);
                    }
                }
            } else {
                /* Add all possible payment URLs. */
                logger.info("last_working_payment_url is not available --> Going through all possible paymentURLs");
                paymentURLs = Arrays.asList(supports_precise_expire_date);
            }
            /* Go through possible paymentURLs in an attempt to find an exact expiredate if the account is premium. */
            for (final String paymentURL : paymentURLs) {
                if (StringUtils.isEmpty(paymentURL)) {
                    continue;
                } else {
                    try {
                        getPage(paymentURL);
                    } catch (final Throwable e) {
                        logger.log(e);
                        /* Skip failures due to timeout or bad http error-responses */
                        continue;
                    }
                }
                /* Find html snippet which should contain our expiredate. */
                final String expireSecond = findExpireDate(br);
                if (!StringUtils.isEmpty(expireSecond)) {
                    final String tmpYears = new Regex(expireSecond, "(\\d+)\\s+years?").getMatch(0);
                    final String tmpdays = new Regex(expireSecond, "(\\d+)\\s+days?").getMatch(0);
                    final String tmphrs = new Regex(expireSecond, "(\\d+)\\s+hours?").getMatch(0);
                    final String tmpmin = new Regex(expireSecond, "(\\d+)\\s+minutes?").getMatch(0);
                    final String tmpsec = new Regex(expireSecond, "(\\d+)\\s+seconds?").getMatch(0);
                    long years = 0, days = 0, hours = 0, minutes = 0, seconds = 0;
                    if (!StringUtils.isEmpty(tmpYears)) {
                        years = Integer.parseInt(tmpYears);
                    }
                    if (!StringUtils.isEmpty(tmpdays)) {
                        days = Integer.parseInt(tmpdays);
                    }
                    if (!StringUtils.isEmpty(tmphrs)) {
                        hours = Integer.parseInt(tmphrs);
                    }
                    if (!StringUtils.isEmpty(tmpmin)) {
                        minutes = Integer.parseInt(tmpmin);
                    }
                    if (!StringUtils.isEmpty(tmpsec)) {
                        seconds = Integer.parseInt(tmpsec);
                    }
                    expire_milliseconds_precise_to_the_second = ((years * 86400000 * 365) + (days * 86400000) + (hours * 3600000) + (minutes * 60000) + (seconds * 1000));
                }
                if (expire_milliseconds_precise_to_the_second > 0) {
                    /* Later we will decide whether we are going to use this value or not. */
                    logger.info("Successfully found precise expire-date via paymentURL: \"" + paymentURL + "\" : " + expireSecond);
                    this.getPluginConfig().setProperty("property_last_working_payment_url", paymentURL);
                    break;
                } else {
                    logger.info("Failed to find precise expire-date via paymentURL: \"" + paymentURL + "\"");
                }
            }
        }
        final long currentTime = br.getCurrentServerTime(System.currentTimeMillis());
        if (expire_milliseconds_precise_to_the_second > 0) {
            /* Add current time to parsed value */
            expire_milliseconds_precise_to_the_second += currentTime;
        }
        final long expire_milliseconds;
        if (isPreciseTimestampFlag.get() && expire_milliseconds_from_expiredate != null) {
            logger.info("Using precise expire-date");
            expire_milliseconds = expire_milliseconds_from_expiredate.longValue();
        } else if (expire_milliseconds_precise_to_the_second > 0) {
            logger.info("Using precise expire-date");
            expire_milliseconds = expire_milliseconds_precise_to_the_second;
        } else if (expire_milliseconds_from_expiredate != null) {
            logger.info("Using expire-date which is up to 24 hours precise");
            expire_milliseconds = expire_milliseconds_from_expiredate.longValue();
        } else {
            logger.info("Failed to find any useful expire-date at all");
            expire_milliseconds = -1;
        }
        if (expire_milliseconds < 0 || (expire_milliseconds - currentTime) <= 0) {
            /* If the premium account is expired or we cannot find an expire-date we'll simply accept it as a free account. */
            if (expire_milliseconds > 0) {
                return -expire_milliseconds;
            } else {
                return null;
            }
        } else {
            /* Expire date is in the future --> It is a premium account */
            return expire_milliseconds;
        }
    }

    protected Long findExpireTimestamp(final Account account, final Browser br, AtomicBoolean isPreciseTimestampFlag) throws Exception {
        String expireStr = new Regex(getCorrectBR(br), "(\\d{1,2} (January|February|March|April|May|June|July|August|September|October|November|December) \\d{4})").getMatch(0);
        if (expireStr != null) {
            /*
             * 2019-12-17: XFS premium accounts usually don't expire just before the next day. They will end to the same time of the day
             * when they were bought but website only displays it to the day which is why we set it to just before the next day to prevent
             * them from expiring too early in JD. XFS websites with API may provide more precise information on the expiredate (down to the
             * second).
             */
            expireStr += " 23:59:59";
            return TimeFormatter.getMilliSeconds(expireStr, "dd MMMM yyyy HH:mm:ss", Locale.ENGLISH);
        }
        // XFileSharingPro Version 4 has broken tag, <span class="text-body"Premium account expire</span>
        // filesfly.cc -> <td>Premium expiration</td><td>2025-XX-XX XX:XX:XX</td>
        expireStr = new Regex(getCorrectBR(br), "(?:>\\s*|<span[^>]*)Premium\\s*(?:account expire|until|expiration):?\\s*</(?:span|td)>\\s*[^>]*>([\\d]+-[\\w{2}]+-[\\d]+\\s[\\d:]+)</").getMatch(0);
        if (expireStr != null) {
            /**
             * e.g. kenfiles.com
             *
             * <span class="profile-ud-label">Premium account expire:</span> <span class="profile-ud-value">2023-02-07 19:58:15</span>
             */
            final long ret = TimeFormatter.getMilliSeconds(expireStr, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
            if (ret > 0) {
                isPreciseTimestampFlag.set(true);
                return ret;
            }
        }
        return -1l;
    }

    protected String findExpireDate(final Browser br) throws Exception {
        boolean allHTML = false;
        String preciseExpireHTML = new Regex(getCorrectBR(br), "<div[^>]*class=\"[^\"]*accexpire[^\"]*\"[^>]*>.*?</div>").getMatch(-1);
        if (preciseExpireHTML == null) {
            preciseExpireHTML = new Regex(getCorrectBR(br), "<div[^>]*class=((?!</div>).)*expires_in((?!</div>).)*>.*?</div>").getMatch(-1);
        }
        if (preciseExpireHTML == null) {
            allHTML = true;
            preciseExpireHTML = getCorrectBR(br);
        }
        // pattern good enough for all html
        String expireSecond = new Regex(preciseExpireHTML, "(?:Premium(-| )Account expires?(?: in)?|Twoje premium wygaśnie za)\\s*:\\s*(?:</span>)?\\s*(?:</span>)?\\s*(?:<span>)?\\s*([a-zA-Z0-9, ]+)\\s*</").getMatch(-1);
        if (StringUtils.isEmpty(expireSecond)) {
            /* e.g. kenfiles.com */
            expireSecond = new Regex(preciseExpireHTML, Pattern.compile(">\\s*Your premium expires?\\s*:\\s*(\\d+ years?, )?(\\d+ days?, )?(\\d+ hours?, )?(\\d+ minutes?, )?\\d+ seconds\\s*<", Pattern.CASE_INSENSITIVE)).getMatch(-1);
        }
        if (StringUtils.isEmpty(expireSecond)) {
            /**
             * e.g. filejoker.com <div class="col-12">Premium Account expires in 209 days, 11 hours</div>
             */
            expireSecond = new Regex(preciseExpireHTML, Pattern.compile(">\\s*Premium Account expires in\\s*(\\d+ years?,?\\s*)?(\\d+ days?,?\\s*)?(\\d+ hours?,?\\s*)?(\\d+ minutes?,?\\s*)?(\\d+ seconds)?\\s*<", Pattern.CASE_INSENSITIVE)).getMatch(-1);
        }
        if (StringUtils.isEmpty(expireSecond) && !allHTML) {
            /*
             * Last attempt - wider RegEx but we expect the 'second(s)' value to always be present!! Example: file-up.org:
             * "<p style="direction: ltr; display: inline-block;">1 year, 352 days, 22 hours, 36 minutes, 45 seconds</p>"
             */
            expireSecond = new Regex(preciseExpireHTML, Pattern.compile(">\\s*(\\d+ years?, )?(\\d+ days?, )?(\\d+ hours?, )?(\\d+ minutes?, )?\\d+ seconds\\s*<", Pattern.CASE_INSENSITIVE)).getMatch(-1);
        }
        if (StringUtils.isEmpty(expireSecond) && !StringUtils.isEmpty(preciseExpireHTML)) {
            /*
             * 2019-09-07: This html-class may also be given for non-premium accounts e.g. fileup.cc
             */
            logger.info("html contains 'accexpire' class but we failed to find a precise expiredate --> Either we have a free account or failed to find precise expiredate although it is given");
        }
        return expireSecond;
    }

    /**
     * Tries to find apikey on website which, if given, usually camn be found on /?op=my_account Example host which has 'API mod' installed:<br>
     * This will also try to get- and save the API host with protocol in case it differs from the plugins' main host (examples:
     * ddownload.co, vup.to). clicknupload.org <br>
     * apikey will usually be located here: "/?op=my_account"
     */
    protected String findAPIKey(final Browser brc) throws Exception {
        String apikey = regexAPIKey(brc);
        generateAPIKey: if (StringUtils.isEmpty(apikey)) {
            if (!allowToGenerateAPIKeyInWebsiteMode()) {
                /* We are not allowed to even try to generate an API key. */
                break generateAPIKey;
            }
            String generateApikeyUrl = this.regexGenerateAPIKeyURL(brc);
            if (generateApikeyUrl == null) {
                /* We cannot generate an API Key without this URL. */
                break generateAPIKey;
            }
            generateApikeyUrl = Encoding.htmlOnlyDecode(generateApikeyUrl);
            logger.info("Failed to find apikey but host has api-mod enabled --> Trying to generate first apikey for this account via: " + generateApikeyUrl);
            try {
                getPage(brc, generateApikeyUrl);
                apikey = regexAPIKey(brc);
                if (apikey == null) {
                    /*
                     * 2019-10-01: Some hosts will not display an APIKey immediately e.g. vup.to 'New API key generated. Please wait 1-2
                     * minutes while the key is being generated and refresh the page afterwards.'. This should not be an issue for us as the
                     * APIKey will be detected upon next account-check.
                     */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find generated apikey - possible plugin failure");
                } else {
                    logger.info("Successfully found newly generated apikey: " + apikey);
                }
            } catch (final Throwable e) {
                logger.exception("Exception occured during accessing generateApikeyUrl", e);
            }
        }
        if (apikey != null) {
            findAPIHost(brc, apikey);
        }
        return apikey;
    }

    /**
     * Finds API host and sets it as a plugin property. <br>
     * Call this before attempting to use a previously found apikey in website mode! <br>
     * This is needed because some websites are using a different domain or a subdomain for API requests.
     */
    protected void findAPIHost(final Browser brc, final String apikey) {
        if (apikey == null) {
            return;
        }
        logger.info("Found apikey! Trying to find api domain with protocol");
        String url_with_apikey = brc.getRegex("(https?://[^/]+/api/account/info[^<>\"\\']*key=" + apikey + "[^<>\"\\']*)").getMatch(0);
        boolean api_uses_special_domain = false;
        if (url_with_apikey == null) {
            logger.info("Unable to find API domain - assuming it is the same as this plugins' domain");
        } else {
            try {
                url_with_apikey = Encoding.htmlOnlyDecode(url_with_apikey);
                final URL apiurl = new URL(url_with_apikey);
                final String apihost = Browser.getHost(apiurl, true);
                if (!apihost.equalsIgnoreCase(this.getHost())) {
                    logger.info(String.format("API domain is %s while main domain of plugin is %s", apihost, this.getHost()));
                    api_uses_special_domain = true;
                    final String test = apiurl.getProtocol() + "://" + apiurl.getHost() + "/api";
                    this.getPluginConfig().setProperty(PROPERTY_PLUGIN_api_domain_with_protocol, test);
                } else {
                    logger.info("API domain and main domain are the same: " + this.getHost());
                }
            } catch (final Throwable e) {
                logger.exception("Error while trying to find API domain", e);
            }
        }
        if (!api_uses_special_domain) {
            /* Important: Dump old data - maybe apihost was different and is now the same! */
            this.getPluginConfig().removeProperty(PROPERTY_PLUGIN_api_domain_with_protocol);
        }
    }

    protected String regexAPIKey(final Browser br) {
        String ret = br.getRegex("/api/account/info\\?key=([a-z0-9]+)").getMatch(0);
        if (ret == null) {
            // darkibox.com, <input type="text" class="form-control" value="......" readonly onfocus="this.select();">
            final String rets[][] = br.getRegex("<input[^>]*value\\s*=\\s*\"([a-z0-9]{16,})\"[^>]readonly").getMatches();
            if (rets.length == 1 && rets[0].length == 1) {
                ret = rets[0][0];
            }
        }
        return ret;
    }

    protected String regexGenerateAPIKeyURL(final Browser br) {
        return br.getRegex("\"([^\"]*?generate_api_key=1[^\"]*?token=[a-f0-9]{32}[^\"]*?)\"").getMatch(0);
    }

    protected void setAccountLimitsByType(final Account account, final AccountType type) {
        account.setType(type);
        switch (type) {
        case LIFETIME:
        case PREMIUM:
            account.setConcurrentUsePossible(true);
            account.setMaxSimultanDownloads(this.getMaxSimultanPremiumDownloadNum());
            break;
        case FREE:
            account.setConcurrentUsePossible(false);
            account.setMaxSimultanDownloads(getMaxSimultaneousFreeAccountDownloads());
            break;
        case UNKNOWN:
        default:
            account.setConcurrentUsePossible(false);
            account.setMaxSimultanDownloads(1);
            break;
        }
    }

    public Form findLoginform(final Browser br) {
        Form loginform = br.getFormbyProperty("name", "FL");
        if (loginform == null) {
            /* More complicated way to find loginform ... */
            final Form[] allForms = br.getForms();
            for (final Form aForm : allForms) {
                final InputField inputFieldOP = aForm.getInputFieldByName("op");
                if (inputFieldOP != null && "login".equalsIgnoreCase(inputFieldOP.getValue())) {
                    loginform = aForm;
                    break;
                }
            }
        }
        if (loginform == null) {
            return null;
        }
        final InputField redirect = loginform.getInputFieldByName("redirect");
        if (redirect != null && StringUtils.isNotEmpty(redirect.getValue())) {
            try {
                final String value = URLDecoder.decode(redirect.getValue(), "UTF-8");
                if (value != null && canHandle(value)) {
                    /*
                     * Prevent redirect value redirecting to file-download straight away which would lead to an Exception.
                     */
                    logger.info("clear login redirect to download:" + value);
                    redirect.setValue("");
                }
            } catch (Exception e) {
                logger.log(e);
            }
        }
        return loginform;
    }

    /** Returns Form required to click on 'continue to image' for image-hosts. */
    public Form findImageForm(final Browser br) {
        final Form imghost_next_form = br.getFormbyKey("next");
        if (imghost_next_form != null && imghost_next_form.hasInputFieldByName("method_premium")) {
            imghost_next_form.remove("method_premium");
        }
        return imghost_next_form;
    }

    /** Tries to find available traffic-left value inside html code. */
    protected String regExTrafficLeft(final Browser br) {
        /* 2020-30-09: progressbar with tooltip */
        final String src = this.getCorrectBR(br);
        String availabletraffic = new Regex(src, "Traffic available(?:\\s*today)?\\s*[^<>]*:?(?:<[^>]*>)?</TD>\\s*<TD[^>]*>\\s*<div[^>]*title\\s*=\\s*\"\\s*([^<>\"']+)\\s*available").getMatch(0);
        if (StringUtils.isEmpty(availabletraffic)) {
            /* Traffic can also be negative! */
            availabletraffic = new Regex(src, "Traffic available(?:\\s*today)?\\s*[^<>]*:?(?:<[^>]*>)?</TD>\\s*<TD[^>]*>\\s*(?:<b[^>]*>)?\\s*([^<>\"']+)").getMatch(0);
            if (StringUtils.isEmpty(availabletraffic)) {
                /* 2019-02-11: For newer XFS versions */
                availabletraffic = new Regex(src, ">\\s*Traffic available(?:\\s*today)?\\s*</div>\\s*<div class=\"txt\\d+\">\\s*([^<>\"]+)\\s*<").getMatch(0);
            }
            if (StringUtils.isEmpty(availabletraffic)) {
                // wrzucajpliki.pl
                // <span>Traffic available</span><div class="price"><sup>MB</sup>102400</div>
                final String trafficLeft = new Regex(src, ">\\s*Traffic available(?:\\s*today)?\\s*</[^>]*>\\s*<div class=\"(?:txt\\d+|price)\">\\s*(.*?)\\s*</div").getMatch(0);
                final String unit = new Regex(trafficLeft, "<sup>\\s*([TGMKB]+)\\s*</sup>").getMatch(0);
                final String left = new Regex(trafficLeft, "</sup>\\s*([\\-\\s*]*[0-9\\.]+)").getMatch(0);
                if (unit != null && left != null) {
                    availabletraffic = left + " " + unit;
                }
            }
        }
        if (StringUtils.isEmpty(availabletraffic)) {
            /* filejoker.net */
            final String formGroup = new Regex(src, ">\\s*Traffic available(?:\\s*today)?\\s*:?\\s*</[^>]*>(.*?)<div\\s*class\\s*=\\s*\"form-group").getMatch(0);
            String trafficDetails[] = new Regex(formGroup, "title\\s*=\\s*\"\\s*([0-9\\.]+\\s*[TGMB]+\\s*)/\\s*([0-9\\.]+\\s*[TGMB]+\\s*)\"").getRow(0);
            if (trafficDetails != null) {
                /**
                 * kenfiles.com
                 *
                 * >Traffic available today</span><span><a href="https://kenfiles.com/contact" title="671Mb/50000Mb"
                 * data-toggle="tooltip">49329 Mb</a></span>
                 */
                final long used = SizeFormatter.getSize(trafficDetails[0]);
                final long max = SizeFormatter.getSize(trafficDetails[1]);
                if (used > 0 && max > 0) {
                    return (max - used) + "b";
                }
            }
            /**
             * filejoker.net
             *
             * >Traffic Available:</label> <div class="col-12 col-md-8 col-lg"> <div class="progress"> <div
             * class="progress-bar progress-bar-striped bg-success" role="progressbar" style="width:47.95%" aria-valuenow="47.95"
             * aria-valuemin="0" aria-valuemax="100" title="47951 MB available">47.95%</div>
             */
            availabletraffic = new Regex(formGroup, "title\\s*=\\s*\"\\s*([\\-\\s*]*[0-9\\.]+\\s*[TGMB]+\\s*)(?:available)?\"").getMatch(0);
        }
        if (StringUtils.isNotEmpty(availabletraffic)) {
            return availabletraffic;
        } else {
            return null;
        }
    }

    public boolean isLoggedin(final Browser brc) {
        /**
         * Please use valid combinations only! login or email alone without xfss is NOT valid!
         */
        final String mainpage = getMainPage(brc);
        logger.info("Doing login-cookiecheck for: " + mainpage);
        final String cookieXFSS = brc.getCookie(mainpage, "xfss", Cookies.NOTDELETEDPATTERN);
        final String cookieXFSTS = brc.getCookie(mainpage, "xfsts", Cookies.NOTDELETEDPATTERN);
        final boolean login_xfss_CookieOkay = StringUtils.isAllNotEmpty(brc.getCookie(mainpage, "login", Cookies.NOTDELETEDPATTERN), cookieXFSS);
        /* xfsts cookie is mostly used in xvideosharing sites (videohosters) example: vidoza.net */
        final boolean login_xfsts_CookieOkay = StringUtils.isAllNotEmpty(brc.getCookie(mainpage, "login", Cookies.NOTDELETEDPATTERN), cookieXFSTS);
        /* 2019-06-21: Example website which uses rare email cookie: filefox.cc (so far the only known!) */
        final boolean email_xfss_CookieOkay = StringUtils.isAllNotEmpty(brc.getCookie(mainpage, "email", Cookies.NOTDELETEDPATTERN), cookieXFSS);
        final boolean email_xfsts_CookieOkay = StringUtils.isAllNotEmpty(brc.getCookie(mainpage, "email", Cookies.NOTDELETEDPATTERN), cookieXFSTS);
        /* buttons or sites that are only available for logged in users */
        // remove script tags
        // remove comments, eg ddl.to just comment some buttons/links for expired cookies/non logged in
        final String htmlWithoutScriptTagsAndComments;
        if (brc.getRequest() == null || brc.getRequest().getHtmlCode() == null) {
            htmlWithoutScriptTagsAndComments = "";
        } else {
            htmlWithoutScriptTagsAndComments = brc.getRequest().getHtmlCode().replaceAll("(?s)(<script.*?</script>)", "").replaceAll("(?s)(<!--.*?-->)", "");
        }
        final String ahrefPattern = "<a[^<]*href\\s*=\\s*\"[^\"]*";
        /**
         * Test cases <br>
         * op=logout: ddownload.com <br>
         * /(user_)?logout\": ?? <br>
         * logout\\.html: fastclick.to/drop.download <br>
         * /logout/: crockdown.com:
         *
         *
         */
        final boolean logout = new Regex(htmlWithoutScriptTagsAndComments, ahrefPattern + "(&|\\?)op=logout").matches() || new Regex(htmlWithoutScriptTagsAndComments, ahrefPattern + "/(user_)?logout/?\"").matches() || new Regex(htmlWithoutScriptTagsAndComments, ahrefPattern + "/logout\\.html\"").matches();
        final boolean login = new Regex(htmlWithoutScriptTagsAndComments, ahrefPattern + "(&|\\?)op=login").matches() || new Regex(htmlWithoutScriptTagsAndComments, ahrefPattern + "/(user_)?login/?\"").matches() || new Regex(htmlWithoutScriptTagsAndComments, ahrefPattern + "/login\\.html\"").matches();
        // unsafe, not every site does redirect
        final boolean loginURLFailed = brc.getURL().contains("op=") && brc.getURL().contains("op=login");
        /*
         * 2019-11-11: Set myAccountOkay to true if there is currently a redirect which means in this situation we rely on our cookie ONLY.
         * This may be the case if a user has direct downloads enabled. We access downloadurl --> Redirect happens --> We check for login
         */
        final boolean isRedirect = brc.getRedirectLocation() != null;
        final boolean myAccountOkay = new Regex(htmlWithoutScriptTagsAndComments, ahrefPattern + "(&|\\?)op=my_account").matches() || new Regex(htmlWithoutScriptTagsAndComments, ahrefPattern + "/my(-|_)account\"").matches() || new Regex(htmlWithoutScriptTagsAndComments, ahrefPattern + "/account/?\"").matches();
        logger.info("xfss_Cookie:" + cookieXFSS);
        logger.info("xfsts_Cookie:" + cookieXFSTS);
        logger.info("login_xfss_CookieOkay:" + login_xfss_CookieOkay);
        logger.info("login_xfsts_CookieOkay:" + login_xfsts_CookieOkay);
        logger.info("email_xfss_CookieOkay:" + email_xfss_CookieOkay);
        logger.info("email_xfsts_CookieOkay:" + email_xfsts_CookieOkay);
        logger.info("logout_exists:" + logout);
        logger.info("login_exists:" + login);
        logger.info("myaccount_exists:" + myAccountOkay);
        logger.info("redirect:" + isRedirect);
        logger.info("loginURLFailed:" + loginURLFailed);
        boolean ret = (login_xfss_CookieOkay || email_xfss_CookieOkay || login_xfsts_CookieOkay || email_xfsts_CookieOkay) && ((logout || (myAccountOkay && !login) || isRedirect) && !loginURLFailed);
        if (!ret) {
            // hosts that only have xfss/xfsts cookie and no login/email one, for example fastfile.cc
            // maybe login/email cookie not available when 2fa is enabled to protect attack on username/email
            ret = (cookieXFSS != null || cookieXFSTS != null) && (logout || (myAccountOkay && !login)) && !loginURLFailed;
        }
        logger.info("loggedin:" + ret);
        return ret;
    }

    /** Returns the full URL to the page which should contain the loginForm. */
    public String getLoginURL() {
        return getMainPage() + "/login.html";
    }

    /**
     * Returns the relative URL to the page which should contain all account information (account type, expiredate, apikey, remaining
     * traffic).
     */
    protected String getRelativeAccountInfoURL() {
        return "/?op=my_account";
    }

    protected boolean containsInvalidLoginsMessage(final Browser br) {
        return br != null && br.containsHTML("(?i)>\\s*Incorrect (Login|Username) or Password\\s*<");
    }

    protected boolean containsBlockedIPLoginMessage(final Browser br) {
        return br != null && (br.containsHTML("(?i)>\\s*You can't login from this IP") || br.containsHTML("(?i)>\\s*Your IP (is|was) banned\\s*<") || br.containsHTML("(?i)>\\s*Your IP (is|was) banned by administrator\\s*<"));
    }

    protected void fillWebsiteLoginForm(Browser br, Form loginform, Account account) {
        {
            final String user = Encoding.urlEncode(account.getUser());
            InputField userField = null;
            final String userFieldNames[] = new String[] { "login", "email" };
            for (String userFieldName : userFieldNames) {
                userField = loginform.getInputFieldByName(userFieldName);
                if (userField != null) {
                    break;
                }
            }
            if (userField != null) {
                userField.setValue(user);
            } else {
                loginform.put(userFieldNames[0], user);
            }
        }
        {
            final String password = Encoding.urlEncode(account.getPass());
            InputField passwordField = null;
            final String passwordFieldNames[] = new String[] { "password", "pass" };
            for (String passwordFieldName : passwordFieldNames) {
                passwordField = loginform.getInputFieldByName(passwordFieldName);
                if (passwordField != null) {
                    break;
                }
            }
            if (passwordField != null) {
                passwordField.setValue(password);
            } else {
                loginform.put(passwordFieldNames[0], password);
            }
        }
    }

    protected boolean handleLoginWebsite2FA(PluginException e, final DownloadLink link, final Account account, final boolean validateCookies) throws Exception {
        Form twoFAForm = null;
        String fieldKey = null;
        final Form[] forms = br.getForms();
        formLoop: for (final Form form : forms) {
            final List<InputField> fields = form.getInputFields();
            for (final InputField field : fields) {
                if (field.getKey() != null && field.getKey().matches("^code\\d*$")) {
                    fieldKey = field.getKey();
                    twoFAForm = form;
                    break formLoop;
                }
            }
            for (final InputField field : fields) {
                if (field.getKey() != null && field.getKey().matches("^new_ip_token$")) {
                    fieldKey = field.getKey();
                    // send.cm -> SendCm class
                    twoFAForm = form;
                    break formLoop;
                }
            }
        }
        if (twoFAForm == null) {
            /* No 2FA login needed -> Login failed because user has entered invalid credentials. */
            throw e;
        }
        logger.info("2FA code required");
        final String twoFACode = this.getTwoFACode(account, "\\d{6}");
        logger.info("Submitting 2FA code");
        twoFAForm.put(fieldKey, twoFACode);
        this.submitForm(twoFAForm);
        if (!this.br.getURL().contains("?op=my_account")) {
            throw new AccountInvalidException(org.jdownloader.gui.translate._GUI.T.jd_gui_swing_components_AccountDialog_2FA_login_invalid());
        } else {
            final Cookies cookies = br.getCookies(br.getHost());
            account.saveCookies(cookies, "");
            if (verifyCookies(account, cookies)) {
                return loginWebsite(link, account, validateCookies);
            } else {
                throw e;
            }
        }
    }

    /**
     * @param validateCookies
     *            true = Check whether stored cookies are still valid, if not, perform full login <br/>
     *            false = Set stored cookies and trust them if they're not older than 300000l
     *
     */
    public boolean loginWebsite(final DownloadLink link, final Account account, final boolean validateCookies) throws Exception {
        synchronized (account) {
            try {
                /* Load cookies */
                br.setCookiesExclusive(true);
                final Cookies cookies = account.loadCookies("");
                final Cookies userCookies = account.loadUserCookies();
                if (userCookies == null && this.requiresCookieLogin()) {
                    /**
                     * Cookie login required but user did not put cookies into the password field: <br>
                     * Ask user to login via exported browser cookies e.g. xubster.com.
                     */
                    showCookieLoginInfo();
                    throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_required());
                }
                if (userCookies != null) {
                    br.setCookies(getMainPage(), userCookies);
                    if (!validateCookies) {
                        /* Trust cookies without check */
                        return false;
                    }
                    if (!this.verifyCookies(account, userCookies)) {
                        if (account.hasEverBeenValid()) {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                        } else {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                        }
                    }
                    /**
                     * If user enters cookies to login he can enter whatever he wants into the "username" field but we want unique usernames
                     * --> Try to find real username of added account and set it.
                     */
                    String cookiesUsername = br.getCookie(br.getHost(), "login", Cookies.NOTDELETEDPATTERN);
                    if (StringUtils.isEmpty(cookiesUsername)) {
                        cookiesUsername = br.getCookie(br.getHost(), "email", Cookies.NOTDELETEDPATTERN);
                    }
                    if (!StringUtils.isEmpty(cookiesUsername)) {
                        cookiesUsername = Encoding.htmlDecode(cookiesUsername).trim();
                    }
                    /**
                     * During cookie login, user can enter whatever he wants into username field.<br>
                     * Most users will enter their real username but to be sure to have unique usernames we don't trust them and try to get
                     * the real username out of our cookies.
                     */
                    if (StringUtils.isEmpty(cookiesUsername)) {
                        /* Not a major problem but worth logging. */
                        logger.warning("Failed to find username via cookie");
                    } else {
                        logger.info("Found username by cookie: " + cookiesUsername);
                        if (!account.getUser().equals(cookiesUsername)) {
                            logger.info("Setting new username by cookie | New: " + cookiesUsername + " | Old: " + account.getUser());
                            account.setUser(cookiesUsername);
                        }
                    }
                    return true;
                } else if (cookies != null) {
                    br.setCookies(getMainPage(), cookies);
                    if (!validateCookies) {
                        /* Trust cookies without check */
                        return false;
                    }
                    if (this.verifyCookies(account, cookies)) {
                        account.saveCookies(br.getCookies(getMainPage()), "");
                        return true;
                    }
                }
                logger.info("Full login required");
                /*
                 * 2019-08-20: Some hosts (rare case) will fail on the first attempt even with correct logindata and then demand a captcha.
                 * Example: filejoker.net
                 */
                int login_counter = 1;
                final int maxLoginAttempts = 3;
                br.clearCookies(getMainPage());
                // getPage(getMainPage()); //loginForm most likely never included on mainPage and thus we open getLoginURL first
                boolean userSolvedAtLeastOneLoginCaptcha = false;
                do {
                    logger.info("Performing full website login attempt: " + login_counter + "/" + maxLoginAttempts + " | Multiple attempts will only happen if a captcha is required");
                    Form loginForm = findLoginform(this.br);
                    if (loginForm == null) {
                        // some sites (eg filejoker) show login captcha AFTER first login attempt, so only reload getLoginURL(without
                        // captcha) if required
                        getPage(getLoginURL());
                        if (br.getHttpConnection().getResponseCode() == 404) {
                            /* Required for some XFS setups - use as common fallback. */
                            getPage(getMainPage() + "/login");
                        }
                        loginForm = findLoginform(this.br);
                        if (loginForm == null) {
                            logger.warning("Failed to find loginform");
                            /* E.g. 503 error during login */
                            this.checkErrorsLoginWebsite(br, account);
                            checkResponseCodeErrors(br.getHttpConnection());
                            if (containsBlockedIPLoginMessage(br)) {
                                throw new AccountInvalidException("\r\nYou can't login from this IP!\r\n");
                            }
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        }
                    }
                    fillWebsiteLoginForm(br, loginForm, account);
                    /* Handle login-captcha if required */
                    final int captchasBefore = getChallenges().size();
                    handleCaptcha(new DownloadLink(this, "Account", this.getHost(), "https://" + account.getHoster(), true), br, loginForm);
                    final int captchasAfter = getChallenges().size();
                    final boolean captchaRequiredInThisRun = captchasAfter > captchasBefore;
                    if (captchaRequiredInThisRun) {
                        userSolvedAtLeastOneLoginCaptcha = true;
                    } else if (login_counter > 1) {
                        /*
                         * After first login attempt(login_counter >=1), there must either be a login captcha (= initial captcha or first
                         * captcha attempt was wrong) or the logins are wrong.
                         */
                        logger.info("Logins seem to be invalid because no login captcha required on login attempt: " + login_counter);
                        break;
                    }
                    submitForm(loginForm);
                    final boolean captchaRequiredInNextRun = this.containsCaptcha(this.findLoginform(br));
                    if (!captchaRequiredInNextRun) {
                        logger.info("Ending login loop because: No captcha required in next run --> No more attempts needed");
                        break;
                    } else if (userSolvedAtLeastOneLoginCaptcha && (containsInvalidLoginsMessage(br) || containsBlockedIPLoginMessage(br))) {
                        logger.info("Logins seem to be invalid because: There has been a login captcha but server response indicates invalid logins on login attempt: " + login_counter);
                        break;
                    }
                    login_counter++;
                } while (!this.isLoggedin(this.br) && login_counter <= maxLoginAttempts);
                if (!this.isLoggedin(this.br)) {
                    logger.info("Login failed after attempts: " + login_counter);
                    if (getCorrectBR(br).contains("op=resend_activation")) {
                        /* User entered correct logindata but hasn't activated his account yet. */
                        throw new AccountUnavailableException("\r\nYour account has not yet been activated!\r\nActivate it via the URL you received via E-Mail and try again!", 5 * 60 * 1000l);
                    } else if (containsInvalidLoginsMessage(br)) {
                        if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                            throw new AccountInvalidException("\r\nUngültiger Benutzername/Passwort!\r\nDu bist dir sicher, dass dein eingegebener Benutzername und Passwort stimmen? Versuche folgendes:\r\n1. Falls dein Passwort Sonderzeichen enthält, ändere es (entferne diese) und versuche es erneut!\r\n2. Gib deine Zugangsdaten per Hand (ohne kopieren/einfügen) ein.");
                        } else if ("pl".equalsIgnoreCase(System.getProperty("user.language"))) {
                            throw new AccountInvalidException("\r\nNieprawidłowa nazwa użytkownika / hasło!\r\nUpewnij się, że prawidłowo wprowadziłes hasło i nazwę użytkownika. Dodatkowo:\r\n1. Jeśli twoje hasło zawiera znaki specjalne, zmień je (usuń) i spróbuj ponownie!\r\n2. Wprowadź hasło i nazwę użytkownika ręcznie bez użycia opcji Kopiuj i Wklej.");
                        } else {
                            throw new AccountInvalidException("\r\nInvalid username/password!\r\nYou're sure that the username and password you entered are correct? Some hints:\r\n1. If your password contains special characters, change it (remove them) and try again!\r\n2. Type in your username/password by hand without copy & paste.");
                        }
                    } else if (containsBlockedIPLoginMessage(br)) {
                        throw new AccountInvalidException("\r\nYou can't login from this IP!\r\n");
                    } else if (containsCaptcha(this.findLoginform(br))) {
                        if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                            throw new AccountInvalidException("\r\nUngültiges Login captcha!\r\nVersuche es erneut.");
                        } else {
                            throw new AccountInvalidException("\r\nInvalid login captcha answer!\r\nTry again.");
                        }
                    } else {
                        // unknown reason, containsInvalidLoginsMessage did not match
                        throw new AccountInvalidException();
                    }
                }
                account.saveCookies(br.getCookies(getMainPage()), "");
                return true;
            } catch (final PluginException e) {
                if (e.getLinkStatus() == LinkStatus.ERROR_PREMIUM) {
                    account.clearCookies("");
                }
                throw e;
            }
        }
    }

    /** Sets given cookies and checks if we can login with them. */
    protected boolean verifyCookies(final Account account, final Cookies cookies) throws Exception {
        br.setCookies(getMainPage(), cookies);
        br.setCookies(cookies);
        getPage(getMainPage() + getRelativeAccountInfoURL());
        if (isLoggedin(this.br)) {
            logger.info("Cookie login successful");
            return true;
        } else {
            logger.info("Cookie login failed");
            br.clearCookies(br.getHost());
            return false;
        }
    }

    protected boolean containsCaptcha(final Form form) {
        if (form == null) {
            return false;
        } else {
            return containsCaptcha(form.getHtmlCode());
        }
    }

    protected boolean containsCaptcha(final Browser br) {
        return br != null && containsCaptcha(br.getRequest().getHtmlCode());
    }

    protected boolean containsCaptcha(final String str) {
        if (str == null) {
            return false;
        } else if (containsHCaptcha(str) || containsRecaptchaV2Class(str) || containsPlainTextCaptcha(str)) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected void throwFinalConnectionException(Browser br, URLConnectionAdapter con) throws PluginException, IOException {
        /* This code should never be reached */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    /**
     * Flags to control download attempt behavior.
     */
    protected static enum DOWNLOAD_ATTEMPT_FLAGS {
        DOWNLOAD_OR_EXCEPTION,
        CONNECT_OR_EXCEPTION
    }

    /**
     * Attempts download and only contains very rudimentary error handling. <br>
     * Returns false if the download is impossible. <br>
     * Returns true if the download is possible && done.
     */
    protected boolean tryDownload(final Browser br, final DownloadLink link, final Account account, final Request req, final DOWNLOAD_ATTEMPT_FLAGS... flags) throws Exception {
        if (req == null) {
            throw new IllegalArgumentException();
        }
        final List<DOWNLOAD_ATTEMPT_FLAGS> flagList = flags != null ? Arrays.asList(flags) : new ArrayList<DOWNLOAD_ATTEMPT_FLAGS>(0);
        final boolean throwConnectException = flagList.contains(DOWNLOAD_ATTEMPT_FLAGS.DOWNLOAD_OR_EXCEPTION) || flagList.contains(DOWNLOAD_ATTEMPT_FLAGS.CONNECT_OR_EXCEPTION);
        final String url_original = req.getUrl();
        String url = url_original;
        url = fixProtocol(link, account, br, url);
        if (!url.equals(req.getUrl())) {
            logger.info("Final downloadlink was changed to: " + url);
            req.setURL(new URL(url));
        }
        String hlsURL = null;
        if (req.getRequestMethod() == RequestMethod.GET && StringUtils.containsIgnoreCase(url, ".m3u8")) {
            /* HLS download */
            hlsURL = url;
        } else {
            /* encoding can cause problems because indices no longer match real file indices */
            req.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING, "identity");
            /* Looks like http/progressive download -> Open connection and see what we get */
            final boolean resume = this.isResumeable(link, account);
            int maxChunks = getMaxChunks(account);
            if (maxChunks > 1) {
                logger.info("@Developer: fixme! maxChunks may not be fixed positive:" + maxChunks);
                maxChunks = -maxChunks;
            }
            if (!resume) {
                if (maxChunks != 1) {
                    logger.info("@Developer: fixme! no resume allowed but maxChunks is not 1:" + maxChunks);
                    maxChunks = 1;
                }
            }
            /* Open connection */
            try {
                dl = new jd.plugins.BrowserAdapter().openDownload(br, link, req, resume, maxChunks);
            } catch (InterruptedException e) {
                throw e;
            } catch (Exception e) {
                if (!throwConnectException && Exceptions.containsInstanceOf(e, SocketException.class, UnknownHostException.class)) {
                    // only special handling for SocketException and UnknownHostException
                    logger.log(e);
                    return false;
                }
                // other exceptions, eg DiskFull, Skip File...must always be thrown
                throw e;
            }
            if (LinkCrawlerDeepInspector.looksLikeMpegURL(dl.getConnection())) {
                /* Unexpected HLS download */
                hlsURL = dl.getConnection().getURL().toExternalForm();
                try {
                    dl.getConnection().disconnect();
                } catch (final Throwable ignore) {
                }
            } else if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                runPostRequestTask(br);
                this.correctBR(br);
                if (dl.getConnection().getResponseCode() == 503) {
                    exception503ConnectionLimitReached();
                    throwFinalConnectionException(br, dl.getConnection());// ensure thrown exception
                } else if (flagList.contains(DOWNLOAD_ATTEMPT_FLAGS.DOWNLOAD_OR_EXCEPTION)) {
                    handleDownloadErrors(dl.getConnection(), link, account);
                    throwFinalConnectionException(br, dl.getConnection());// ensure thrown exception
                }
                dl.close();
                dl = null;
                return false;
            }
        }
        if (hlsURL != null) {
            final String finalDownloadlink;
            try {
                finalDownloadlink = handleQualitySelectionHLS(this.br.cloneBrowser(), hlsURL);
            } catch (InterruptedException e) {
                throw e;
            } catch (Exception e) {
                if (flagList.contains(DOWNLOAD_ATTEMPT_FLAGS.DOWNLOAD_OR_EXCEPTION)) {
                    throw e;
                } else {
                    logger.log(e);
                    return false;
                }
            }
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, finalDownloadlink);
            try {
                fixFilenameHLSDownload(link);
            } catch (final Exception e) {
                logger.log(e);
            }
        } else {
            /* Progressive download */
            try {
                fixFilename(dl.getConnection(), link);
            } catch (final Exception ignore) {
                logger.log(ignore);
            }
        }
        /* Store URL for next download attempt */
        this.storeDirecturl(link, account, dl.getConnection().getURL().toExternalForm());
        /* add a download slot */
        controlMaxFreeDownloads(account, link, +1);
        try {
            /* start the dl */
            dl.startDownload();
        } finally {
            /* remove download slot */
            controlMaxFreeDownloads(account, link, -1);
        }
        return true;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (this.attemptStoredDownloadurlDownload(link, account)) {
            return;
        }
        resolveShortURL(br.cloneBrowser(), link, account);
        if (this.enableAccountApiOnlyMode()) {
            /* API-only mode */
            handleDownload(link, account, null, this.getDllinkAPI(link, account));
            return;
        }
        /* Website/mixed mode */
        final String contentURL = this.getNormalizedDownloadURL(link);
        tryAPIDownload: if (this.allowAPIDownloadIfApikeyIsAvailable(link, account)) {
            String directlinkFromAPI = null;
            try {
                directlinkFromAPI = this.getDllinkAPI(link, account);
            } catch (final InterruptedException ie) {
                throw ie;
            } catch (final Throwable e) {
                /* Do not throw Exception --> Fallback to website instead */
                logger.log(e);
                logger.warning("Error in API download handling in website mode");
                break tryAPIDownload;
            }
            logger.info("Found directurl via API mode: " + directlinkFromAPI);
            handleDownload(link, account, null, directlinkFromAPI);
            return;
        }
        /* API failed/not supported -> Try website! */
        requestFileInformationWebsite(link, account);
        final boolean verifiedLogin = loginWebsite(link, account, false);
        if (this.tryDownload(br, link, account, br.createGetRequest(contentURL), DOWNLOAD_ATTEMPT_FLAGS.CONNECT_OR_EXCEPTION)) {
            return;
        }
        if (!verifiedLogin && !isLoggedin(this.br)) {
            /* Now we must be logged in -> Try again */
            loginWebsite(link, account, true);
            if (this.tryDownload(br, link, account, br.createGetRequest(contentURL), DOWNLOAD_ATTEMPT_FLAGS.CONNECT_OR_EXCEPTION)) {
                return;
            }
        }
        if (isFree(account)) {
            /* Not a premium account -> Jump into free handling */
            doFree(link, account);
            return;
        }
        /*
         * Check for final downloadurl here because if user/host has direct downloads enabled, PluginPatternMatcher will redirect to our
         * final downloadurl thus isLoggedin might return false although we are loggedin!
         */
        String officialVideoDownloadURL = null;
        officialVideoDownloadURL = getDllinkViaOfficialVideoDownload(this.br.cloneBrowser(), link, account, false);
        String dllink = getDllink(link, account, br, getCorrectBR(br));
        if (StringUtils.isEmpty(dllink) || StringUtils.isEmpty(officialVideoDownloadURL)) {
            final Form dlform = findFormDownload2Premium(link, account, this.br);
            if (dlform != null) {
                handlePassword(dlform, link);
                if (this.tryDownload(br, link, account, br.createFormRequest(dlform), DOWNLOAD_ATTEMPT_FLAGS.CONNECT_OR_EXCEPTION)) {
                    return;
                }
                /* Important: Do not check for errors here! Only check for errors later if no download link was found!! */
                // checkErrors(br, getCorrectBR(br), link, account, true);
                if (StringUtils.isEmpty(officialVideoDownloadURL)) {
                    officialVideoDownloadURL = getDllinkViaOfficialVideoDownload(this.br.cloneBrowser(), link, account, false);
                }
                if (dllink == null) {
                    dllink = getDllink(link, account, br, getCorrectBR(br));
                }
            }
        }
        if (StringUtils.isEmpty(dllink) && StringUtils.isEmpty(officialVideoDownloadURL)) {
            checkErrors(br, getCorrectBR(br), link, account, true);
            logger.warning("Failed to find Form download2");
            checkServerErrors(br, link, account);
            checkErrorsLastResort(br, link, account);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        handleDownload(link, account, officialVideoDownloadURL, dllink);
    }

    /**
     * rewrite/fix wrong protocol from http to https if required
     *
     * @param link
     * @param account
     * @param br
     * @param dllink
     * @return
     * @throws Exception
     */
    protected String fixProtocol(final DownloadLink link, final Account account, final Browser br, final String dllink) throws Exception {
        if (dllink == null) {
            return null;
        }
        final URL url = br.getURL(dllink);
        if (url.getPort() != -1 && StringUtils.equalsIgnoreCase(url.getProtocol(), "http")) {
            try {
                final Browser brc = br.cloneBrowser();
                brc.setAllowedResponseCodes(400);
                brc.getPage(url.getProtocol() + "://" + url.getHost() + ":" + url.getPort() + "/");
                if (brc.getHttpConnection().getResponseCode() == 400 && brc.containsHTML("The plain HTTP request was sent to HTTPS port")) {
                    final String ret = url.toExternalForm().replaceFirst("(?i)^(http://)", "https://");
                    logger.info("fixProtocol downloadlink = " + dllink + "->" + ret);
                    return ret;
                }
            } catch (final IOException e) {
                logger.log(e);
            }
        }
        return dllink;
    }

    protected void handleDownload(final DownloadLink link, final Account account, final String officialVideoDownloadURL, final String directurl) throws Exception {
        final DownloadMode mode = this.getPreferredDownloadModeFromConfig();
        String finalDownloadlink;
        if (!StringUtils.isEmpty(officialVideoDownloadURL) && (mode == null || mode == DownloadMode.ORIGINAL || mode == DownloadMode.AUTO || StringUtils.isEmpty(directurl))) {
            /* Official video download */
            finalDownloadlink = officialVideoDownloadURL;
        } else {
            /* Fallback / File download / Stream download */
            finalDownloadlink = directurl;
        }
        if (Encoding.isHtmlEntityCoded(finalDownloadlink)) {
            finalDownloadlink = Encoding.htmlOnlyDecode(finalDownloadlink);
        }
        this.tryDownload(br, link, account, br.createGetRequest(finalDownloadlink), DOWNLOAD_ATTEMPT_FLAGS.DOWNLOAD_OR_EXCEPTION);
    }

    protected String handleQualitySelectionHLS(final Browser br, final String hlsMaster) throws Exception {
        if (hlsMaster == null) {
            /* This should never happen */
            throw new IllegalArgumentException();
        }
        /* Access URL if it hasn't been accessed already. */
        if (!StringUtils.equals(br.getURL(), hlsMaster)) {
            this.getPage(br, hlsMaster);
        }
        final List<HlsContainer> hlsQualities = HlsContainer.getHlsQualities(br);
        if (hlsQualities == null || hlsQualities.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "HLS stream broken?");
        }
        HlsContainer hlsSelected = null;
        final int userSelectedQuality = getPreferredStreamQuality();
        if (userSelectedQuality == -1) {
            logger.info("Looking for BEST video stream");
            hlsSelected = HlsContainer.findBestVideoByBandwidth(hlsQualities);
        } else {
            logger.info("Looking for user selected video stream quality: " + userSelectedQuality);
            for (final HlsContainer hlsQualityTmp : hlsQualities) {
                final int height = hlsQualityTmp.getHeight();
                if (height == userSelectedQuality) {
                    logger.info("Successfully found selected quality: " + userSelectedQuality);
                    hlsSelected = hlsQualityTmp;
                    break;
                }
            }
            if (hlsSelected == null) {
                logger.info("Failed to find user selected quality --> Finding next best quality");
                hlsSelected = HlsContainer.findBestTargetHeight(hlsQualities, userSelectedQuality);
            }
        }
        logger.info(String.format("Picked stream quality = %sp", hlsSelected.getHeight()));
        return hlsSelected.getDownloadurl();
    }

    /** Stores final downloadurl on current DownloadLink object */
    protected void storeDirecturl(final DownloadLink link, final Account account, final String directurl) {
        final String directlinkproperty = getDownloadModeDirectlinkProperty(account);
        link.setProperty(directlinkproperty, directurl);
    }

    /** Handles errors right before starting the download. */
    protected void handleDownloadErrors(final URLConnectionAdapter con, final DownloadLink link, final Account account) throws Exception {
        if (!looksLikeDownloadableContent(con)) {
            logger.warning("The final dllink seems not to be a file!");
            br.followConnection(true);
            correctBR(br);
            checkResponseCodeErrors(con);
            checkServerErrors(br, link, account);
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Final downloadlink did not lead to downloadable content");
        } else {
            try {
                checkResponseCodeErrors(con);
            } catch (final PluginException e) {
                try {
                    br.followConnection(true);
                } catch (IOException ioe) {
                    throw Exceptions.addSuppressed(e, ioe);
                }
                throw e;
            }
        }
    }

    /* *************************** PUT API RELATED METHODS HERE *************************** */
    protected String getAPIBase() {
        final String custom_apidomain = this.getPluginConfig().getStringProperty(PROPERTY_PLUGIN_api_domain_with_protocol);
        if (custom_apidomain != null) {
            return custom_apidomain;
        } else {
            return getMainPage() + "/api";
        }
    }

    /** If enabled this plugin will attempt to generate an API key if no API key was found and it looks like the website supports that. */
    protected boolean allowToGenerateAPIKeyInWebsiteMode() {
        return true;
    }

    /**
     * Generates final downloadurl via API if API usage is allowed and apikey is available. Only execute this if you know that the currently
     * used host supports this! <br>
     * Only execute this if an apikey is given! <br>
     * Only execute this if you know that a particular host has enabled this API call! <br>
     * Important: For some hosts, this API call will only be available for premium accounts, not for free accounts!
     */
    /*
     * TODO: check/add support for URL_TYPE.FILE
     */
    protected String getDllinkAPI(final DownloadLink link, final Account account) throws Exception {
        /* Linkcheck is not required here - download API will return appropriate offline status if file is not available anymore. */
        // requestFileInformationAPI(link, account);
        logger.info("Trying to get dllink via API");
        final String apikey = getAPIKeyFromAccount(account);
        if (StringUtils.isEmpty(apikey)) {
            /* This should never happen */
            throw new IllegalArgumentException("apikey is null");
        }
        final String fileid_to_download;
        if (requiresAPIGetdllinkCloneWorkaround(account)) {
            logger.info("Trying to download file via clone workaround");
            getPage(this.getAPIBase() + "/file/clone?key=" + apikey + "&file_code=" + this.getFUIDFromURL(link));
            this.checkErrorsAPI(this.br, link, account);
            fileid_to_download = PluginJSonUtils.getJson(br, "filecode");
            if (StringUtils.isEmpty(fileid_to_download)) {
                logger.warning("Failed to find new fileid in clone handling");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } else {
            logger.info("Trying to download file via api without workaround");
            fileid_to_download = this.getFUIDFromURL(link);
        }
        /*
         * Users can also chose a preferred quality via '&q=h' but we prefer to receive all and then chose to easily have a fallback in case
         * the quality selected by our user is not available.
         */
        /* Documentation videohost: https://xfilesharingpro.docs.apiary.io/#reference/file/file-clone/get-direct-link */
        /*
         * Documentation filehost:
         * https://xvideosharing.docs.apiary.io/#reference/file/file-direct-link/get-links-to-all-available-qualities
         */
        getPage(this.getAPIBase() + "/file/direct_link?key=" + apikey + "&file_code=" + fileid_to_download);
        final Map<String, Object> entries = this.checkErrorsAPI(this.br, link, account);
        final Map<String, Object> result = (Map<String, Object>) entries.get("result");
        /**
         * TODO: Add quality selection. 2020-05-20: Did not add selection yet because so far this API call has NEVER worked for ANY
         * filehost&videohost!
         */
        /* For video hosts: Pick the best quality */
        String dllink = null;
        final String[] qualities = new String[] { "o", "h", "n", "l" };
        for (final String quality : qualities) {
            final Map<String, Object> quality_tmp = (Map<String, Object>) result.get(quality);
            if (quality_tmp != null) {
                dllink = (String) quality_tmp.get("url");
                if (!StringUtils.isEmpty(dllink)) {
                    break;
                }
            }
        }
        if (StringUtils.isEmpty(dllink)) {
            /* For filehosts (= no different qualities available) */
            logger.info("Failed to find any quality - downloading original file");
            dllink = (String) result.get("url");
            // final long filesize = JavaScriptEngineFactory.toLong(entries.get("size"), 0);
        }
        if (StringUtils.isEmpty(dllink)) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find final downloadurl via API");
        }
        logger.info("Successfully found dllink via API: " + dllink);
        return dllink;
    }

    /**
     * Advantages over website: <br/>
     * - Always precise expire-date <br/>
     * - All info we need via one single http request <br/>
     * - Consistent
     */
    protected AccountInfo fetchAccountInfoAPI(final Browser br, final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> entries = loginAPI(br, account);
        /** 2019-07-31: Better compare expire-date against their serverside time if possible! */
        final String server_timeStr = (String) entries.get("server_time");
        final Map<String, Object> result = (Map<String, Object>) entries.get("result");
        long expire_milliseconds_precise_to_the_second = 0;
        final long currentTime;
        if (server_timeStr != null && server_timeStr.matches("\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}")) {
            currentTime = TimeFormatter.getMilliSeconds(server_timeStr, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
        } else {
            /* Fallback */
            currentTime = br.getCurrentServerTime(System.currentTimeMillis());
        }
        String expireStr = (String) result.get("premium_expire");
        if (StringUtils.isEmpty(expireStr)) {
            /*
             * 2019-05-30: Seems to be a typo by the guy who develops the XFS script in the early versions of thei "API mod" :D 2019-07-28:
             * Typo is fixed in newer XFSv3 versions - still we'll keep both versions in just to make sure it will always work ...
             */
            expireStr = (String) result.get("premim_expire");
        }
        if (expireStr != null && expireStr.matches("\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}")) {
            expire_milliseconds_precise_to_the_second = TimeFormatter.getMilliSeconds(expireStr, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
        }
        ai.setUnlimitedTraffic();
        final long premiumDurationMilliseconds = expire_milliseconds_precise_to_the_second - currentTime;
        if (premiumDurationMilliseconds <= 0) {
            /* Expired premium or no expire date given --> Free Account */
            setAccountLimitsByType(account, AccountType.FREE);
        } else {
            /* Expire date is in the future --> Premium account */
            ai.setValidUntil(System.currentTimeMillis() + premiumDurationMilliseconds);
            setAccountLimitsByType(account, AccountType.PREMIUM);
        }
        final String premium_bandwidthBytesStr = (String) result.get("premium_bandwidth"); // Double as string
        final String traffic_leftBytesStr = (String) result.get("traffic_left");
        if (premium_bandwidthBytesStr != null) {
            ai.setTrafficLeft(SizeFormatter.getSize(premium_bandwidthBytesStr));
        } else if (traffic_leftBytesStr != null) {
            ai.setTrafficLeft(SizeFormatter.getSize(traffic_leftBytesStr));
        }
        {
            /* Now set less relevant account information */
            final Object balanceO = result.get("balance"); // Double returned as string
            if (balanceO != null) {
                final String balanceStr = balanceO.toString();
                if (balanceStr.matches("[0-9.]+")) {
                    ai.setAccountBalance(Double.parseDouble(balanceStr), Currency.getInstance("USD"));
                }
            }
            /* 2019-07-26: values can also be "inf" for "Unlimited": "storage_left":"inf" */
            // final long storage_left = JavaScriptEngineFactory.toLong(entries.get("storage_left"), 0);
            final Object storage_usedO = result.get("storage_used");
            if (storage_usedO != null) {
                ai.setUsedSpace(SizeFormatter.getSize(storage_usedO.toString()));
            }
        }
        final Object files_totalO = result.get("files_total");
        if (files_totalO instanceof Number) {
            ai.setFilesNum(((Number) files_totalO).intValue());
        }
        final String email = (String) result.get("email");
        if (this.enableAccountApiOnlyMode() && !StringUtils.isEmpty(email)) {
            /*
             * Each account is unique. Do not care what the user entered - trust what API returns! <br> This is not really important - more
             * visually so that something that makes sense is displayed to the user in his account managers' "Username" column!
             */
            account.setUser(email);
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* Devs only */
            String accStatus;
            if (ai.getStatus() != null && !ai.getStatus().startsWith("[API] ")) {
                accStatus = ai.getStatus();
            } else {
                accStatus = account.getType().toString();
            }
            ai.setStatus("[API] | DLs: " + account.hasProperty(PROPERTY_ACCOUNT_ALLOW_API_DOWNLOAD_ATTEMPT_IN_WEBSITE_MODE) + " | " + accStatus);
        }
        return ai;
    }

    /**
     * More info see supports_api()
     */
    protected final Map<String, Object> loginAPI(final Browser br, final Account account) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final String apikey = this.getAPIKeyFromAccount(account);
            if (!this.looksLikeValidAPIKey(apikey)) {
                throw new AccountInvalidException("Invalid API Key format!\r\nFind your API Key here: " + this.getAPILoginHelpURL());
            }
            getPage(br, this.getAPIBase() + "/account/info?key=" + apikey);
            final Map<String, Object> entries = this.checkErrorsAPI(br, null, account);
            final boolean loginAPICheckIfDownloadsAreAllowed = true;
            if (loginAPICheckIfDownloadsAreAllowed) {
                /**
                 * Find out whether or not we can download via API with this account. <br>
                 * Depending on the account-type and/or XFS API config, login via API may be successful but downloading via API is not
                 * possible. <br>
                 * We want to determine this here so later we can decide whether we want to try downloads via API.
                 */
                boolean apiDownloadsPossible = false;
                try {
                    final Browser brc = br.cloneBrowser();
                    getPage(brc, this.getAPIBase() + "/file/direct_link?key=" + apikey + "&file_code=");
                    /**
                     * Example positive responses: 2024-05-27: darkibox.com: {"status":200,"msg":"uploading"...} <br>
                     * 2024-05-27: send.cm: {"msg":"no file","server_time":"2024-05-27 00:00:00","status":404} <br>
                     */
                    final Map<String, Object> result = this.checkErrorsAPI(brc, null, account);
                    final String msg = (String) result.get("msg");
                    if (StringUtils.equalsIgnoreCase(msg, "uploading") && "200".equals(StringUtils.valueOfOrNull(result.get("status")))) {
                        /* 2024-05-27: */
                        apiDownloadsPossible = true;
                    }
                } catch (final PluginException ple) {
                    if (ple.getLinkStatus() == LinkStatus.ERROR_FILE_NOT_FOUND) {
                        /**
                         * Typically this happens when downloads are not possible via API: {"msg":"This function not allowed in
                         * API","server_time":"2023-11-30 15:53:27","status":403} <br>
                         */
                        /* {"server_time":"2023-11-30 15:53:33","status":404,"msg":"no file"} */
                        apiDownloadsPossible = true;
                    }
                } catch (final InterruptedException e) {
                    throw e;
                } catch (final Exception e) {
                    logger.log(e);
                    logger.info("Exception occured API download check");
                } finally {
                    logger.info("API download status: " + apiDownloadsPossible);
                    if (apiDownloadsPossible) {
                        account.setProperty(PROPERTY_ACCOUNT_ALLOW_API_DOWNLOAD_ATTEMPT_IN_WEBSITE_MODE, true);
                    } else {
                        account.removeProperty(PROPERTY_ACCOUNT_ALLOW_API_DOWNLOAD_ATTEMPT_IN_WEBSITE_MODE);
                    }
                }
            }
            return entries;
        }
    }

    protected final AvailableStatus requestFileInformationAPI(final DownloadLink link, final String apikey) throws Exception {
        massLinkcheckerAPI(new DownloadLink[] { link }, apikey);
        if (link.getAvailableStatus() == AvailableStatus.FALSE) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return link.getAvailableStatus();
    }

    /**
     * Checks multiple URLs via API. Only works when an apikey is given!
     */
    public boolean massLinkcheckerAPI(final DownloadLink[] urls, final String apikey) {
        if (urls == null || urls.length == 0 || !this.looksLikeValidAPIKey(apikey)) {
            return false;
        }
        boolean linkcheckerHasFailed = false;
        try {
            final Browser br = createNewBrowserInstance();
            this.prepBrowser(br, getMainPage());
            br.setCookiesExclusive(true);
            final StringBuilder sb = new StringBuilder();
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    /*
                     * We test max 50 links at once. 2020-05-29: XFS default API linkcheck limit is exactly 50 items. If you check more than
                     * 50 items, it will only return results for the first 50 items.
                     */
                    if (index == urls.length || links.size() == 50) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                final ArrayList<DownloadLink> apiLinkcheckLinks = new ArrayList<DownloadLink>();
                sb.delete(0, sb.capacity());
                for (final DownloadLink link : links) {
                    try {
                        resolveShortURL(br.cloneBrowser(), link, null);
                    } catch (final PluginException e) {
                        logger.log(e);
                        if (e.getLinkStatus() == LinkStatus.ERROR_FILE_NOT_FOUND) {
                            link.setAvailableStatus(AvailableStatus.FALSE);
                        } else if (e.getLinkStatus() == LinkStatus.ERROR_IP_BLOCKED) {
                            link.setAvailableStatus(AvailableStatus.TRUE);
                        } else {
                            link.setAvailableStatus(AvailableStatus.UNCHECKABLE);
                        }
                        if (!link.isNameSet()) {
                            setWeakFilename(link, null);
                        }
                        /*
                         * We cannot check shortLinks via API so if we're unable to convert them to TYPE_NORMAL we basically already checked
                         * them here. Also we have to avoid sending wrong fileIDs to the API otherwise linkcheck WILL fail!
                         */
                        continue;
                    }
                    sb.append(this.getFUIDFromURL(link));
                    sb.append("%2C");
                    apiLinkcheckLinks.add(link);
                }
                if (apiLinkcheckLinks.isEmpty()) {
                    /* Rare edge-case */
                    logger.info("Seems like we got only shortURLs -> Nothing left to be checked via API");
                    break;
                }
                getPage(br, getAPIBase() + "/file/info?key=" + apikey + "&file_code=" + sb.toString());
                Map<String, Object> entries = null;
                try {
                    entries = this.checkErrorsAPI(br, links.get(0), null);
                } catch (final Throwable e) {
                    logger.log(e);
                    /* E.g. invalid apikey, broken serverside API, developer mistake (e.g. sent fileIDs in invalid format) */
                    logger.info("Fatal failure");
                    return false;
                }
                final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) entries.get("result");
                for (final DownloadLink link : apiLinkcheckLinks) {
                    Map<String, Object> fileinfo = null;
                    final String thisFUID = this.getFUIDFromURL(link);
                    for (final Map<String, Object> fileInfoTmp : ressourcelist) {
                        String fuid_temp = (String) fileInfoTmp.get("filecode");
                        if (StringUtils.isEmpty(fuid_temp)) {
                            /* 2022-08-09 */
                            fuid_temp = (String) fileInfoTmp.get("file_code");
                        }
                        if (StringUtils.equals(fuid_temp, thisFUID)) {
                            fileinfo = fileInfoTmp;
                            break;
                        }
                    }
                    if (fileinfo == null) {
                        /**
                         * This should never happen. Possible reasons: <br>
                         * - Wrong APIKey <br>
                         * - We tried to check too many items at once <br>
                         * - API only allows users to check self-uploaded content --> Disable API linkchecking in plugin! <br>
                         * - API does not not allow linkchecking at all --> Disable API linkchecking in plugin! <br>
                         */
                        logger.warning("WTF failed to find information for fuid: " + this.getFUIDFromURL(link));
                        linkcheckerHasFailed = true;
                        continue;
                    }
                    /* E.g. check for "result":[{"status":404,"filecode":"xxxxxxyyyyyy"}] */
                    final int status = ((Number) fileinfo.get("status")).intValue();
                    if (!link.isNameSet()) {
                        setWeakFilename(link, null);
                    }
                    String filename = null;
                    boolean isVideohost = false;
                    if (status != 200) {
                        link.setAvailable(false);
                    } else {
                        link.setAvailable(true);
                        filename = (String) fileinfo.get("name");
                        if (StringUtils.isEmpty(filename)) {
                            filename = (String) fileinfo.get("file_title");
                        }
                        final long filesize = JavaScriptEngineFactory.toLong(fileinfo.get("size"), 0);
                        final Object canplay = fileinfo.get("canplay");
                        final Object views_started = fileinfo.get("views_started");
                        final Object views = fileinfo.get("views");
                        final Object length = fileinfo.get("length");
                        isVideohost = canplay != null || views_started != null || views != null || length != null;
                        /* Filesize is not always given especially not for videohosts. */
                        if (filesize > 0) {
                            link.setDownloadSize(filesize);
                        }
                    }
                    if (!isVideohost) {
                        isVideohost = this.internal_isVideohoster_enforce_video_filename(link, null);
                    }
                    if (!StringUtils.isEmpty(filename)) {
                        /*
                         * At least for videohosts, filenames from json would often not contain a file extension!
                         */
                        if (Encoding.isHtmlEntityCoded(filename)) {
                            filename = Encoding.htmlDecode(filename).trim();
                        }
                        if (isVideohost) {
                            filename = this.applyFilenameExtension(filename, ".mp4");
                        }
                        /* Trust API filenames -> Set as final filename. */
                        link.setFinalFileName(filename);
                    } else {
                        /* Use cached name */
                        final String name = link.getName();
                        if (name != null && isVideohost) {
                            link.setName(this.applyFilenameExtension(filename, ".mp4"));
                        }
                    }
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            logger.log(e);
            return false;
        } finally {
            if (linkcheckerHasFailed) {
                logger.info("Seems like massLinkcheckerAPI availablecheck is not supported by this host or currently broken");
            }
        }
        if (linkcheckerHasFailed) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Can be executed after API calls to check for- and handle errors. <br>
     * Example good API response: {"msg":"OK","server_time":"2020-05-25 13:09:37","status":200,"result":[{"...
     */
    protected Map<String, Object> checkErrorsAPI(final Browser br, final DownloadLink link, final Account account) throws NumberFormatException, PluginException {
        /**
         * 2019-10-31: TODO: Add support for more errorcodes e.g. downloadlimit reached, premiumonly, password protected, wrong password,
         * wrong captcha. [PW protected + captcha protected download handling is not yet implemented serverside]
         */
        final long defaultWaitAccount = 3 * 60 * 1000;
        final long defaultWaitMillis = 3 * 60 * 1000;
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException e) {
            logger.log(e);
            final String errormessage = "Invalid API response";
            if (link == null) {
                throw new AccountUnavailableException(errormessage, defaultWaitAccount);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormessage, defaultWaitMillis);
            }
        }
        final int status = ((Number) entries.get("status")).intValue();
        if (status == 200) {
            /* No error */
            return entries;
        }
        final String errormsg = (String) entries.get("msg");
        /**
         * TODO: Maybe first check for errormessage based on text, then handle statuscode. <br>
         * One statuscode can be returned with different errormessages!
         */
        /* First check for specific error messages */
        if (errormsg != null) {
            /* TODO: Check for more error messages e.g. {"msg":"Wrong auth","server_time":"2024-11-28 12:03:07","status":403} */
            if (errormsg.equalsIgnoreCase("This function not allowed in API")) {
                /* API does not allow user to download so basically we can't use it -> Temp disable account. */
                throw new AccountUnavailableException("API does not allow download | Contact support of this website", 5 * 60 * 1000l);
            } else if (errormsg.equalsIgnoreCase("Wrong auth")) {
                /* API does not allow user to download so basically we can't use it -> Temp disable account. */
                throw new AccountInvalidException(errormsg);
            }
            if (errormsg.equalsIgnoreCase("no file")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        }
        /* Handle misc / response-code related errors */
        if (status == 403) {
            /* Account related error */
            throw new AccountInvalidException(errormsg);
        } else if (status == 404) {
            /* {"msg":"No file","server_time":"2019-10-31 17:23:17","status":404} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            final String msg = "Unknown API error with status " + status;
            if (link == null) {
                throw new AccountUnavailableException(msg, defaultWaitAccount);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, defaultWaitMillis);
            }
        }
    }

    protected final String getAPIKeyFromAccount(final Account account) {
        synchronized (account) {
            String apikey;
            if (this.enableAccountApiOnlyMode()) {
                /* In API only mode, apikey is entered by user into password field. */
                apikey = account.getPass();
                if (apikey != null) {
                    apikey = apikey.trim();
                }
            } else {
                /* In website mode we store apikey as a property on our current account object. */
                apikey = account.getStringProperty(PROPERTY_ACCOUNT_apikey);
            }
            if (looksLikeValidAPIKey(apikey)) {
                return apikey;
            } else {
                return null;
            }
        }
    }

    /** @return apikey but only if it is considered valid! */
    protected final String getAPIKeyFromConfig() {
        final Class<? extends XFSConfig> cfgO = getConfigInterface();
        if (cfgO == null) {
            return null;
        }
        final String apikey = PluginJsonConfig.get(cfgO).getApikey();
        if (looksLikeValidAPIKey(apikey)) {
            return apikey;
        } else {
            return null;
        }
    }

    protected final DownloadMode getPreferredDownloadModeFromConfig() {
        final Class<? extends XFSConfigVideo> cfgO = getVideoConfigInterface();
        if (cfgO == null) {
            /* 2023-12-19: TODO: Return default from default config and not hardcoded */
            return DownloadMode.AUTO;
        } else {
            return PluginJsonConfig.get(cfgO).getPreferredDownloadMode();
        }
    }

    /**
     * This will try to return an apikey, preferably from a valid account. <br>
     * Uses API key from config as fallback.
     */
    protected final String getAPIKey() {
        final Account acc = AccountController.getInstance().getValidAccount(this.getHost());
        if (acc != null && this.getAPIKeyFromAccount(acc) != null) {
            return this.getAPIKeyFromAccount(acc);
        } else {
            return this.getAPIKeyFromConfig();
        }
    }

    @Override
    protected String getAPILoginHelpURL() {
        return getMainPage() + "/?op=my_account";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("^[a-z0-9]{16,}$")) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Use this to set filename based on filename inside URL or fuid as filename either before a linkcheck happens so that there is a
     * readable filename displayed in the linkgrabber or also for mass-linkchecking as in this case these is no filename given inside HTML.
     */
    protected void setWeakFilename(final DownloadLink link, final Browser br) {
        final String weakFilename = this.getFallbackFilename(link, br);
        if (weakFilename != null) {
            link.setName(weakFilename);
        }
    }

    /** Returns empty StringArray for filename, filesize, filehash, [more information in the future?] */
    public final String[] internal_getFileInfoArray() {
        return new String[3];
    }

    /**
     * This can 'automatically' detect whether a host supports embedding videos. <br />
     * Example: uqload.com<br>
     * Do not override unless really needed!
     */
    protected final boolean internal_isVideohosterEmbed(final Browser br) {
        return isVideohosterEmbed() || isVideohosterEmbedHTML(br);
    }

    /**
     * Decides whether to enforce a filename with a '.mp4' ending or not. <br>
     * Names are either enforced if the configuration of the script implies this or if it detects that embedding videos is possible. <br>
     * Do not override - at least try to avoid having to!!
     */
    protected final boolean internal_isVideohoster_enforce_video_filename(final DownloadLink link, final Browser br) {
        final URL_TYPE urltype = this.getURLType(link);
        final URL_TYPE urltype2 = br != null ? this.getURLType(br.getURL()) : null;
        if (isVideohoster_enforce_video_filename()) {
            return true;
        } else if (internal_isVideohosterEmbed(br)) {
            return true;
        } else if (urltype == URL_TYPE.EMBED_VIDEO || urltype == URL_TYPE.EMBED_VIDEO_2 || urltype == URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD) {
            return true;
        } else if (urltype2 == URL_TYPE.EMBED_VIDEO || urltype2 == URL_TYPE.EMBED_VIDEO_2 || urltype2 == URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public final boolean internal_supportsMassLinkcheck() {
        return this.supportsAPIMassLinkcheck() || this.supportsMassLinkcheckOverWebsite() || this.enableAccountApiOnlyMode();
    }

    /**
     * Override this and let it return true whenever an user provided API key is available to allow the plugin to do single linkchecks via
     * API. <br>
     *
     * @default false
     */
    protected boolean supportsAPISingleLinkcheck() {
        // return isAPIKey(this.getAPIKey());
        /* On Override, you would typically use the above line of code as return value. */
        return false;
    }

    /** @default false */
    protected boolean supportsAPIMassLinkcheck() {
        // return isAPIKey(this.getAPIKey());
        /* On Override, you would typically use the above line of code as return value. */
        return false;
    }

    /**
     * This can 'automatically' detect whether a host supports availablecheck via 'abuse' URL. <br />
     * Example: uploadboy.com<br>
     * Do not override - at least try to avoid having to!!
     */
    protected final boolean internal_supports_availablecheck_filename_abuse() {
        final boolean supportedByIndicatingHtmlCode = new Regex(getCorrectBR(br), "op=report_file&(?:amp;)?id=" + this.getFUIDFromURL(this.getDownloadLink())).matches();
        boolean allowedByAutoHandling = true;
        final SubConfiguration config = this.getPluginConfig();
        final long timestampLastFailure = config.getLongProperty(PROPERTY_PLUGIN_REPORT_FILE_AVAILABLECHECK_LAST_FAILURE_TIMESTAMP, 0);
        final String last_version = config.getStringProperty(PROPERTY_PLUGIN_REPORT_FILE_AVAILABLECHECK_LAST_FAILURE_VERSION, null);
        if (timestampLastFailure > 0 && StringUtils.equalsIgnoreCase(getPluginVersionHash(), last_version)) {
            final long timestampCooldown = timestampLastFailure + internal_waittime_on_alternative_availablecheck_failures();
            if (timestampCooldown > System.currentTimeMillis()) {
                logger.info("internal_supports_availablecheck_filename_abuse is still deactivated as it did not work on the last attempt");
                logger.info("Time until retry: " + TimeFormatter.formatMilliSeconds(timestampCooldown - System.currentTimeMillis(), 0));
                allowedByAutoHandling = false;
            }
        }
        return (this.supports_availablecheck_filename_abuse() || supportedByIndicatingHtmlCode) && allowedByAutoHandling;
    }

    protected final boolean internal_supports_availablecheck_alt() {
        boolean allowedByAutoHandling = true;
        final SubConfiguration config = this.getPluginConfig();
        final long timestampLastFailure = config.getLongProperty(PROPERTY_PLUGIN_ALT_AVAILABLECHECK_LAST_FAILURE_TIMESTAMP, 0);
        final String last_version = config.getStringProperty(PROPERTY_PLUGIN_ALT_AVAILABLECHECK_LAST_FAILURE_VERSION, null);
        if (timestampLastFailure > 0 && StringUtils.equalsIgnoreCase(getPluginVersionHash(), last_version)) {
            final long timestampCooldown = timestampLastFailure + internal_waittime_on_alternative_availablecheck_failures();
            if (timestampCooldown > System.currentTimeMillis()) {
                logger.info("internal_supports_availablecheck_alt is still deactivated as it did not work on the last attempt");
                logger.info("Time until retry: " + TimeFormatter.formatMilliSeconds(timestampCooldown - System.currentTimeMillis(), 0));
                allowedByAutoHandling = false;
            }
        }
        return supports_availablecheck_alt() && allowedByAutoHandling;
    }

    /**
     * Defines the time to wait until a failed linkcheck method will be tried again. This should be set to > 24 hours as its purpose is to
     * minimize unnecessary http requests.
     */
    protected long internal_waittime_on_alternative_availablecheck_failures() {
        return 7 * 24 * 60 * 60 * 1000;
    }

    /**
     * Function to check whether or not a filehost is running XFS API mod or not. Only works for APIs running on their main domain and not
     * any other/special domain! <br>
     * Example test working & API available: https://fastfile.cc/api/account/info <br>
     * Example not working but API available: https://api-v2.ddownload.com/api/account/info <br>
     * Example API not available (= XFS API Mod not installed): <br>
     */
    private boolean test_looks_like_supports_api() throws IOException {
        br.getPage(this.getAPIBase() + "/account/info");
        /* 2020-05-29: Answer we'd expect if API is available: {"msg":"Invalid key","server_time":"2020-05-29 17:16:36","status":400} */
        try {
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            /* No Exception -> json parsing was successful -> We got an API response. */
            return true;
            // final String msg = (String) entries.get("msg");
            // final String server_time = (String) entries.get("server_time");
            // if (!StringUtils.isEmpty(msg) && !StringUtils.isEmpty(server_time)) {
            // return true;
            // } else {
            // return false;
            // }
        } catch (final Throwable e) {
            return false;
        }
    }

    @Override
    public Class<? extends XFSConfig> getConfigInterface() {
        return null;
    }

    @Override
    public void reset() {
    }

    @Override
    public Boolean verifyDownloadableContent(Set<LazyHostPlugin> plugins, final URLConnectionAdapter urlConnection) {
        if (plugins != null) {
            plugins.add(getLazyP());
        }
        if (urlConnection.getCompleteContentLength() == 7 && urlConnection.getContentType().matches("(?i)^.*application/octet-stream.*")) {
            // filejoker
            // HTTP/1.1 200 OK
            // Content-Type: application/octet-stream
            // Content-Length: 7
            // ETag: "48ae7c8c-7"
            // Response: Expired = 7 length
            return Boolean.FALSE;
        } else if (urlConnection.getCompleteContentLength() == 7 && urlConnection.getContentType().matches("(?i)^.*text/html.*")) {
            // normal
            // HTTP/1.1 200 OK
            // Content-Type: text/html
            // Content-Length: 7
            // Response: Expired = 7 length
            return Boolean.FALSE;
        } else if (urlConnection.isContentDisposition()) {
            // HTTP/1.1 200 OK
            // Content-Type: text/html; charset=UTF-8
            // Content-Disposition: inline; filename=error.html
            final String contentDispositionHeader = urlConnection.getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_DISPOSITION);
            final String contentDispositionFileName = HTTPConnectionUtils.getFileNameFromDispositionHeader(contentDispositionHeader);
            final boolean inlineFlag = contentDispositionHeader.matches("(?i)^\\s*inline\\s*;?.*");
            if (inlineFlag && (contentDispositionFileName != null && contentDispositionFileName.matches("(?i)^.*\\.html?$"))) {
                return Boolean.FALSE;
            }
        }
        return null;
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        if (link == null) {
            /* No nothing */
            return;
        }
        link.removeProperty(PROPERTY_REFERER_REQUIRED);
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.SibSoft_XFileShare;
    }
}