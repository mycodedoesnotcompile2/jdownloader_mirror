//    jDownloader - Downloadmanager
//    Copyright (C) 2013  JD-Team support@jdownloader.org
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

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.net.URLHelper;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.components.config.RapidGatorConfig;
import org.jdownloader.plugins.components.config.RapidGatorConfig.PremiumDownloadBehaviorForSubscriberOnlyFiles;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.reconnect.ipcheck.BalancedWebIPCheck;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51521 $", interfaceVersion = 3, names = {}, urls = {})
public class RapidGatorNet extends PluginForHost {
    public RapidGatorNet(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/article/premium");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        /* Define custom browser headers and language settings */
        br.getHeaders().put("Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.3");
        br.getHeaders().put("Accept-Language", "en-US,en;q=0.8");
        br.getHeaders().put("Cache-Control", null);
        br.getHeaders().put("Pragma", null);
        br.setCookie(getHost(), "lang", "en");
        br.setCustomCharset("UTF-8");
        final int customReadTimeoutSeconds = PluginJsonConfig.get(RapidGatorConfig.class).getReadTimeout();
        br.setReadTimeout(customReadTimeoutSeconds * 1000);
        br.setConnectTimeout(1 * 60 * 1000);
        /* For API */
        br.addAllowedResponseCodes(401, 402, 501, 423);
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "rapidgator.net", "rapidgator.asia", "rg.to" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/file/([a-z0-9]{32}(?:/[^/<>]+\\.html)?|\\d+(?:/[^/<>]+\\.html)?)");
        }
        return ret.toArray(new String[0]);
    }

    private final int                API_SESSION_ID_REFRESH_TIMEOUT_MINUTES      = 45;
    /*
     * 2020-01-07: Use 120 minutes for the website login for now. Consider disabling this on negative feedback as frequent website logins
     * may lead to login-captchas!
     */
    private final int                WEBSITE_SESSION_ID_REFRESH_TIMEOUT_MINUTES  = 120;
    private static Map<String, Long> blockedIPsMap                               = new HashMap<String, Long>();
    private final String             PROPERTY_LAST_BLOCKED_IPS_MAP               = "rapidgatornet__last_blockedIPsMap";
    private final String             PROPERTY_LAST_DOWNLOAD_STARTED_TIMESTAMP    = "rapidgatornet__last_download_started_timestamp";
    private final String             PROPERTY_sessionid                          = "session_id";
    private final String             PROPERTY_timestamp_session_create_api       = "session_create";
    private final String             PROPERTY_timestamp_session_create_website   = "session_create_website";
    private final String             PROPERTY_HOTLINK                            = "HOTLINK";
    /* 2019-12-12: Lowered from 2 to 1 hour */
    private final long               FREE_RECONNECTWAIT_GENERAL_MILLIS           = 1 * 60 * 60 * 1001L;
    private final long               FREE_RECONNECTWAIT_DAILYLIMIT_MILLIS        = 3 * 60 * 60 * 1000L;
    private final long               FREE_RECONNECTWAIT_OTHERS_MILLIS            = 30 * 60 * 1000L;
    private final long               FREE_RECONNECTWAIT_BETWEEN_DOWNLOADS_MILLIS = 2 * 60 * 60 * 1000L;
    private final int                FREE_CAPTCHA_EXPIRE_TIME_MILLIS             = 105 * 1000;
    /* Don't touch the following! */
    private static AtomicInteger     freeRunning                                 = new AtomicInteger(0);
    private static final String      PROPERTY_LAST_USED_CAPTCHA_TYPE             = "last_used_captcha_type";
    private static final String      CAPTCHA_TYPE_RECAPTCHA                      = "recaptcha";

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/article/terms";
    }

    private String getContentURL(final DownloadLink link) {
        return "https://" + this.getHost() + "/file/" + getFID(link);
    }

    protected String getAPIBase() {
        return "https://" + this.getHost() + "/api/v2/";
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fileid = getFID(link);
        if (fileid != null) {
            return "rapidgator.net://" + fileid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), "(?i)/file/([a-z0-9]{32}|\\d+)").getMatch(0);
    }

    private String getURLFilename(final DownloadLink link) throws UnsupportedEncodingException, IllegalArgumentException {
        String urlfilename = new Regex(link.getPluginPatternMatcher(), "(?i).+/(.+)\\.html$").getMatch(0);
        if (urlfilename != null) {
            urlfilename = URLEncode.decodeURIComponent(urlfilename, "UTF-8", true);
        }
        return urlfilename;
    }

    /**
     * Returns filename from URL (if present) or file-ID.
     *
     * @throws IllegalArgumentException
     * @throws UnsupportedEncodingException
     */
    private String getFallbackFilename(final DownloadLink link) throws UnsupportedEncodingException, IllegalArgumentException {
        final String fname = getURLFilename(link);
        if (fname != null) {
            /* Final fallback */
            return fname;
        } else {
            return this.getFID(link);
        }
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        try {
            final String fname = getFallbackFilename(link);
            if (fname != null) {
                return fname;
            }
        } catch (Exception e) {
        }
        return super.getDefaultFileName(link);
    }

    @Override
    public String filterPackageID(String packageIdentifier) {
        return packageIdentifier.replaceAll("([^a-zA-Z0-9]+)", "");
    }

    private final char[] FILENAMEREPLACES = new char[] { ' ', '_' };

    @Override
    public char[] getFilenameReplaceMap() {
        return FILENAMEREPLACES;
    }

    @Override
    public boolean isHosterManipulatesFilenames() {
        return true;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        if (isPremiumAccount(acc)) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public boolean hasAutoCaptcha() {
        return false;
    }

    private String hotlinkDirectURL = null;

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        /* Use API during linkcheck if API usage is enabled in plugin settings. */
        final Account account = PluginJsonConfig.get(RapidGatorConfig.class).isEnableAPIPremium() ? AccountController.getInstance().getValidAccount(this.getHost()) : null;
        return requestFileInformation(link, account);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        final String session_id;
        if (account != null) {
            session_id = getAccountSession(account);
        } else {
            session_id = null;
        }
        if (session_id != null) {
            /* API usage is enabled by user, account is available and an API session is available too --> Use API-linkcheck */
            try {
                return requestFileInformationAPI(link, account, session_id);
            } catch (final AccountUnavailableException aue) {
                /* Fallback 1 */
                logger.log(aue);
                /*
                 * Do not disable account because of this [yet]. Only let this happen if the session failure occurs during download attempt.
                 */
                // this.handleAccountException(account, this.getLogger(), aue);
                logger.info("AccountUnavailableException happened during attempted API availablecheck -> Fallback to website availablecheck");
                return requestFileInformationWebsite(link, null);
            } catch (final AccountInvalidException aie) {
                /* Fallback 2 */
                logger.log(aie);
                /*
                 * Do not disable account because of this [yet]. Only let this happen if the session failure occurs during download attempt.
                 */
                // this.handleAccountException(account, this.getLogger(), aie);
                logger.info("AccountInvalidException happened during attempted API availablecheck -> Fallback to website availablecheck");
                return requestFileInformationWebsite(link, null);
            }
        } else {
            return requestFileInformationWebsite(link, null);
        }
    }

    private void prepDownloadHeader(final Browser br) {
        final String custom_referer = PluginJsonConfig.get(RapidGatorConfig.class).getReferer();
        if (!StringUtils.isEmpty(custom_referer)) {
            try {
                URLHelper.verifyURL(new URL(custom_referer));
                /*
                 * 2019-12-14: According to users, some special Referer will remove the captcha in free mode (I was unable to confirm) and
                 * lower the wait time between downloads from 120 to 60 minutes.
                 */
                br.setCurrentURL(custom_referer);
            } catch (final MalformedURLException ignore) {
                logger.exception("User given custom referer is not a valid URL: " + custom_referer, ignore);
            }
        }
    }

    private AvailableStatus requestFileInformationWebsite(final DownloadLink link, final Account account) throws Exception {
        hotlinkDirectURL = null;
        if (!link.isNameSet()) {
            link.setName(getFallbackFilename(link));
        }
        setBrowserExclusive();
        if (account != null) {
            this.loginWebsite(account, false);
        }
        prepDownloadHeader(br);
        final URLConnectionAdapter con = br.openGetConnection(this.getContentURL(link));
        try {
            if (this.looksLikeDownloadableContent(con)) {
                /**
                 * Looks like direct-downloadable item. </br>
                 * Either we're logged in as a premium user or this item was made hot-linked by a premium user.
                 */
                if (con.getCompleteContentLength() > 0) {
                    if (con.isContentDecoded()) {
                        link.setDownloadSize(con.getCompleteContentLength());
                    } else {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                }
                if (link.getFinalFileName() == null) {
                    final String filenameFromConnection = Plugin.getFileNameFromConnection(con);
                    if (filenameFromConnection != null) {
                        link.setFinalFileName(filenameFromConnection);
                    }
                }
                link.setProperty(PROPERTY_HOTLINK, true);
                hotlinkDirectURL = con.getURL().toExternalForm();
            } else {
                /* Not a direct-URL */
                br.followConnection();
                this.checkOfflineWebsite(br, link, true);
                String filename = br.getRegex("Downloading\\s*:\\s*</strong>\\s*<a href=\"\"[^>]*>([^<>\"]+)<").getMatch(0);
                if (filename == null) {
                    filename = br.getRegex("<title>\\s*Download file\\s*([^<>\"]+)</title>").getMatch(0);
                }
                final String filesize = br.getRegex("File size:\\s*<strong>([^<>\"]+)</strong>").getMatch(0);
                if (StringUtils.isNotEmpty(filename)) {
                    filename = Encoding.htmlDecode(filename).trim();
                    link.setFinalFileName(filename);
                }
                if (filesize != null) {
                    link.setDownloadSize(SizeFormatter.getSize(filesize));
                }
                final String md5 = br.getRegex(">\\s*MD5\\s*:\\s*([A-Fa-f0-9]{32})<").getMatch(0);
                if (md5 != null) {
                    link.setMD5Hash(md5);
                }
            }
        } finally {
            con.disconnect();
        }
        return AvailableStatus.TRUE;
    }

    private AvailableStatus requestFileInformationAPI(final DownloadLink link, final Account account, final String session_id) throws Exception {
        if (account == null) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        } else if (session_id == null) {
            /* This should never happen! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (!link.isNameSet()) {
            link.setName(getFallbackFilename(link));
        }
        /* Docs: https://rapidgator.net/article/api/file#info */
        br.getPage(getAPIBase() + "file/info?token=" + session_id + "&file_id=" + Encoding.urlEncode(this.getFID(link)));
        final Map<String, Object> response = this.handleErrors_api(session_id, link, account, br, true);
        final Map<String, Object> filemap = (Map<String, Object>) response.get("file");
        /* Error-Response maybe wrong - do not check for errors here! */
        // handleErrors_api(session_id, true, link, account, br.getHttpConnection());
        final String filename = filemap.get("name").toString();
        final Number filesize = (Number) filemap.get("size");
        final String filehash = (String) filemap.get("hash");
        if (filename != null) {
            link.setFinalFileName(filename);
        }
        if (filesize != null) {
            link.setVerifiedFileSize(filesize.longValue());
        }
        if (filehash != null) {
            link.setMD5Hash(filehash);
        }
        return AvailableStatus.TRUE;
    }

    public boolean isProxyRotationEnabledForLinkChecker() {
        return false;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownloadWebsite(link, null);
    }

    private boolean isPremiumAccount(final Account account) {
        return account != null && (AccountType.PREMIUM.equals(account.getType()) || AccountType.LIFETIME.equals(account.getType()));
    }

    private static WeakHashMap<Account, WeakHashMap<DownloadLink, Object>> PREMIUM_LINKS = new WeakHashMap<Account, WeakHashMap<DownloadLink, Object>>();

    @SuppressWarnings("deprecation")
    private void handleDownloadWebsite(final DownloadLink link, final Account account) throws Exception {
        final boolean isPremiumAccount = isPremiumAccount(account);
        if (!isPremiumAccount && checkShowFreeDialog(getHost())) {
            showFreeDialog(getHost());
        }
        if (account != null) {
            this.loginWebsite(account, true);
        }
        final RapidGatorConfig cfg = PluginJsonConfig.get(RapidGatorConfig.class);
        try {
            final String directlinkproperty = getDirectlinkProperty(account);
            String storedDirecturl = null;
            String currentIP = null;
            String finalDownloadURL = null;
            prepDownloadHeader(br);
            findDirecturl: if ((storedDirecturl = link.getStringProperty(directlinkproperty)) != null) {
                logger.info("Trying to re-use last stored directurl: " + storedDirecturl);
                finalDownloadURL = storedDirecturl;
            } else {
                this.dl = new jd.plugins.BrowserAdapter().openDownload(br, link, this.getContentURL(link), true, getMaxChunks(link, account));
                if (this.looksLikeDownloadableContent(dl.getConnection())) {
                    logger.info("Hotlinked file or premium account with direct-download enabled");
                    break findDirecturl;
                }
                this.dl = null;
                logger.info("No direct-URL -> Tring to generate fresh directurl");
                br.followConnection(true);
                final String errormessageSubscribersOnlyDownload = getErrormessageSubscriberOnlyDownload(br);
                final PremiumDownloadBehaviorForSubscriberOnlyFiles premiumDownloadBehaviorForSubscriberOnlyFiles = cfg.getPremiumDownloadBehaviorForSubscriberOnlyFiles();
                if (errormessageSubscribersOnlyDownload != null && isPremiumAccount && premiumDownloadBehaviorForSubscriberOnlyFiles == PremiumDownloadBehaviorForSubscriberOnlyFiles.SKIP) {
                    throw new AccountRequiredException(errormessageSubscribersOnlyDownload);
                }
                if (isPremiumAccount && isBuyFile(br, link, account)) {
                    /* 2022-11-07: can be *bypassed* for premium users by using API mode */
                    logger.info("File needs to be bought separately -> Trying to work around this limitation");
                    handlePremium_api(link, account);
                    return;
                }
                /* Premium account or active subscription */
                finalDownloadURL = br.getRegex("var premium_download_link\\s*=\\s*'(https?://[^<>\"']+)';").getMatch(0);
                if (finalDownloadURL == null) {
                    finalDownloadURL = br.getRegex("'(https?://pr_srv\\.rapidgator\\.net//\\?r=download/index\\&session_id=[A-Za-z0-9]+)'").getMatch(0);
                    if (finalDownloadURL == null) {
                        finalDownloadURL = br.getRegex("'(https?://pr\\d+\\.rapidgator\\.net//\\?r=download/index\\&session_id=[A-Za-z0-9]+)'").getMatch(0);
                    }
                }
                if (finalDownloadURL != null) {
                    /**
                     * Premium downloadlink found! </br>
                     * This does not mean that the user owns a premium account. It can also mean that this is a subscription-only file and
                     * the user owns the needed subscription. </br>
                     * The maps down below help us to determine the resumeability of such items.
                     */
                    logger.info("Premium account or active subscription");
                    if (account != null) {
                        synchronized (PREMIUM_LINKS) {
                            WeakHashMap<DownloadLink, Object> downloadsMap = PREMIUM_LINKS.get(account);
                            if (downloadsMap == null) {
                                downloadsMap = new WeakHashMap<DownloadLink, Object>();
                                PREMIUM_LINKS.put(account, downloadsMap);
                            }
                            downloadsMap.put(link, Boolean.TRUE);
                        }
                    }
                    break findDirecturl;
                }
                /* Free + free account + free download of subscriber-only file in premium mode */
                if (cfg.isActivateExperimentalWaittimeHandling() && !isPremiumAccount) {
                    currentIP = new BalancedWebIPCheck(br.getProxy()).getExternalIP().getIP();
                    logger.info("currentIP = " + currentIP);
                    synchronized (blockedIPsMap) {
                        /* Load list of saved IPs + timestamp of last download */
                        final Object lastdownloadmap = this.getPluginConfig().getProperty(PROPERTY_LAST_BLOCKED_IPS_MAP);
                        if (lastdownloadmap != null && lastdownloadmap instanceof Map && blockedIPsMap.isEmpty()) {
                            blockedIPsMap.putAll((Map<String, Long>) lastdownloadmap);
                        }
                    }
                    logger.info("blockedIPsMap: " + blockedIPsMap);
                    final long lastdownload_timestamp = getPluginSavedLastDownloadTimestamp(currentIP);
                    final long passedTimeSinceLastFreeDownloadMilliseconds = System.currentTimeMillis() - lastdownload_timestamp;
                    logger.info("Wait between free downloads to prevent your IP from getting blocked for 1 day!");
                    if (passedTimeSinceLastFreeDownloadMilliseconds < FREE_RECONNECTWAIT_GENERAL_MILLIS) {
                        throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Wait between free downloads to prevent your IP from getting blocked for 1 day!", FREE_RECONNECTWAIT_GENERAL_MILLIS - passedTimeSinceLastFreeDownloadMilliseconds);
                    }
                }
                final String startTimerUrl = br.getRegex("var startTimerUrl = '([^']+)';").getMatch(0);
                final String fid = br.getRegex("var fid = (\\d+);").getMatch(0);
                final String waitSecondsStr = br.getRegex("var secs = (\\d+);").getMatch(0);
                if (startTimerUrl == null || fid == null || waitSecondsStr == null) {
                    /* Check for reasons why download may be impossible. */
                    handleErrorsWebsite(br, link, account, currentIP, true);
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                if (isPremiumAccount && errormessageSubscribersOnlyDownload != null) {
                    /* Use owns premium account but can't download this file as premium user. */
                    logger.info("Performing free download in premium mode | subscribersOnlyDownload= " + errormessageSubscribersOnlyDownload);
                }
                logger.info("Pre download wait in seconds: " + waitSecondsStr);
                long waitMillis = Long.parseLong(waitSecondsStr) * 1000;
                final Browser br2 = br.cloneBrowser();
                br2.getHeaders().put("X-Requested-With", "XMLHttpRequest");
                br2.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
                /* Trigger serverside countdown. */
                br2.getPage(startTimerUrl + "?fid=" + fid);
                final Map<String, Object> respstarttimer = restoreFromString(br2.getRequest().getHtmlCode(), TypeRef.MAP);
                String state = (String) respstarttimer.get("state");
                if (!"started".equalsIgnoreCase(state)) {
                    /* This should be a very very rare case. */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Error in pre download step #1 | state: " + state);
                }
                final String sid = (String) respstarttimer.get("sid");
                if (StringUtils.isEmpty(sid)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                String recaptchaV2Response = null;
                final String lastUsedCaptchaType = this.getPluginConfig().getStringProperty(PROPERTY_LAST_USED_CAPTCHA_TYPE);
                if (cfg.isEnableFreeDownloadModeCaptchaDuringPreDownloadWait() && lastUsedCaptchaType != null) {
                    /**
                     * 2023-10-03: A small trick: We know their captcha key and can thus always obtain captcha solutions at any point of
                     * time. </br>
                     * Requesting the captcha here basically allows us to solve it during the serverside wait time which is impossible to do
                     * in browser.
                     */
                    final long timeBeforeCaptchaInput = Time.systemIndependentCurrentJVMTimeMillis();
                    if (CAPTCHA_TYPE_RECAPTCHA.equals(lastUsedCaptchaType)) {
                        /* reCaptcha captcha */
                        final CaptchaHelperHostPluginRecaptchaV2 rc2 = new CaptchaHelperHostPluginRecaptchaV2(this, br, "6LcSUAsUAAAAAKBeQQE893pf0Io66-mIeKWPl5yF");
                        this.waitBeforeInteractiveCaptcha(link, waitMillis, rc2.getSolutionTimeout());
                        recaptchaV2Response = rc2.getToken();
                    } else {
                        // unsupported captcha type
                        this.getPluginConfig().removeProperty(PROPERTY_LAST_USED_CAPTCHA_TYPE);
                    }
                    final long millisecondsPassedDuringCaptcha = Time.systemIndependentCurrentJVMTimeMillis() - timeBeforeCaptchaInput;
                    waitMillis -= millisecondsPassedDuringCaptcha;
                }
                final long finalWaittimeMillis = waitMillis + 1000;
                logger.info("Waiting pre-download seconds: " + finalWaittimeMillis / 1000);
                sleep(finalWaittimeMillis, link);
                br2.getPage("/download/AjaxGetDownloadLink?sid=" + sid);
                final Map<String, Object> respgetdownloadlink = restoreFromString(br2.getRequest().getHtmlCode(), TypeRef.MAP);
                state = (String) respgetdownloadlink.get("state");
                if (!"done".equalsIgnoreCase(state)) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Error in pre download step #2 | state: " + state + " | code: " + respgetdownloadlink.get("code"));
                }
                br.getPage("/download/captcha");
                handleErrorsWebsite(br, link, account, currentIP);
                final Form captchaform = br.getFormbyProperty("id", "captchaform");
                if (captchaform != null) {
                    boolean captchaSuccess = false;
                    int captchaAttempts = -1;
                    try {
                        do {
                            captchaAttempts++;
                            if (CaptchaHelperHostPluginRecaptchaV2.containsRecaptchaV2Class(br)) {
                                /* 2024-04-19: They are currently using this captcha. */
                                if (recaptchaV2Response == null) {
                                    /* Only solve captcha if it hasn't been solved before. */
                                    final CaptchaHelperHostPluginRecaptchaV2 rc2 = new CaptchaHelperHostPluginRecaptchaV2(this, br);
                                    recaptchaV2Response = rc2.getToken();
                                }
                                this.getPluginConfig().setProperty(PROPERTY_LAST_USED_CAPTCHA_TYPE, CAPTCHA_TYPE_RECAPTCHA);
                                /* 2024-04-19: Yes, captcha response needs to be two times in this Form. */
                                captchaform.put("DownloadCaptchaForm[verifyCode]", Encoding.urlEncode(recaptchaV2Response));
                                captchaform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                            } else if (br.containsHTML("//api\\.adscapchta\\.com/")) {
                                this.getPluginConfig().removeProperty(PROPERTY_LAST_USED_CAPTCHA_TYPE);
                                captchaform.put("DownloadCaptchaForm[captcha]", "");
                                final String captchaAdress = captchaform.getRegex("<iframe src=\'(https?://api\\.adscaptcha\\.com/NoScript\\.aspx\\?CaptchaId=\\d+&PublicKey=[^\'<>]+)").getMatch(0);
                                final String captchaType = new Regex(captchaAdress, "CaptchaId=(\\d+)&").getMatch(0);
                                if (captchaAdress == null || captchaType == null) {
                                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                                }
                                if (!"3017".equals(captchaType)) {
                                    throw new PluginException(LinkStatus.ERROR_FATAL, "ADSCaptcha: Captcha type not supported!");
                                }
                                final Browser adsCaptcha = br.cloneBrowser();
                                adsCaptcha.getPage(captchaAdress);
                                String challenge = adsCaptcha.getRegex("<img src=\"(https?://api\\.adscaptcha\\.com//Challenge\\.aspx\\?cid=[^\"]+)").getMatch(0);
                                if (StringUtils.isEmpty(challenge)) {
                                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                                }
                                final String code = adsCaptcha.getRegex("class=\"code\">([0-9a-f\\-]+)<").getMatch(0);
                                if (StringUtils.isEmpty(code)) {
                                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                                }
                                challenge = getCaptchaCode(challenge, link);
                                if (StringUtils.isEmpty(challenge)) {
                                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                                }
                                captchaform.put("adscaptcha_response_field", Encoding.urlEncode(challenge));
                                captchaform.put("adscaptcha_challenge_field", Encoding.urlEncode(code));
                            } else {
                                /* This should never happen! */
                                this.getPluginConfig().removeProperty(PROPERTY_LAST_USED_CAPTCHA_TYPE);
                                logger.warning("Unknown captcha type is required");
                                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                            }
                            br.submitForm(captchaform);
                            // Set-Cookie: failed_on_captcha=1; path=/ response if the captcha expired.
                            final boolean failedBecauseWeSentCaptchaResponseTooLate = "1".equals(br.getCookie(br.getHost(), "failed_on_captcha", Cookies.NOTDELETEDPATTERN));
                            if (failedBecauseWeSentCaptchaResponseTooLate) {
                                /* This should never happen! */
                                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Captcha timeout");
                            } else if (br.containsHTML("(>\\s*Please fix the following input errors|>\\s*The verification code is incorrect|api\\.recaptcha\\.net/|google\\.com/recaptcha/api/|//api\\.adscaptcha\\.com)")) {
                                invalidateLastChallengeResponse();
                                continue;
                            } else {
                                captchaSuccess = true;
                                break;
                            }
                        } while (captchaAttempts <= 4 && !captchaSuccess);
                        if (!captchaSuccess) {
                            throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                        } else {
                            validateLastChallengeResponse();
                        }
                    } finally {
                        useShortChallengeTimeoutToAvoidServersideBan = false;
                    }
                } else {
                    logger.info("Failed to find captchaform -> No captcha needed?");
                }
                finalDownloadURL = br.getRegex("'(https?://[A-Za-z0-9\\-_]+\\.[^/]+//\\?r=download/index&session_id=[A-Za-z0-9]+)'").getMatch(0);
                if (finalDownloadURL == null) {
                    // Old regex
                    finalDownloadURL = br.getRegex("location\\.href\\s*=\\s*'(https?://.*?)'").getMatch(0);
                    if (finalDownloadURL == null) {
                        /* 2020-02-06 */
                        finalDownloadURL = br.getRegex("(https?://[^/]+/download/[^<>\"\\']+)").getMatch(0);
                    }
                }
                if (finalDownloadURL == null) {
                    handleErrorsWebsite(br, link, account, currentIP);
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            if (cfg.isExperimentalEnforceSSL() && !StringUtils.startsWithCaseInsensitive(finalDownloadURL, "https")) {
                logger.info("Enforcing https on final downloadurl");
                finalDownloadURL = finalDownloadURL.replaceFirst("(?i)^http://", "https://");
            }
            try {
                if (this.dl == null) {
                    dl = new jd.plugins.BrowserAdapter().openDownload(br, link, finalDownloadURL, isResumeable(link, account), getMaxChunks(link, account));
                }
                /* Save directurl (yes, even though we don't yet know if it works) */
                link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
                /* 2020-03-17: Content-Disposition should always be given */
                if (!looksLikeDownloadableContent(dl.getConnection())) {
                    br.followConnection(true);
                    handleErrorsWebsite(br, link, account, currentIP);
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error");
                }
            } catch (final Exception e) {
                if (storedDirecturl != null && finalDownloadURL.equals(storedDirecturl)) {
                    link.removeProperty(directlinkproperty);
                    throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired?", e);
                } else {
                    throw e;
                }
            }
            if (!StringUtils.containsIgnoreCase(dl.getConnection().getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCEPT_RANGES), "bytes")) {
                /* First download attempt and resume is not possible according to header: Disable it for future download-attempts. */
                logger.info("Resume disabled: missing Accept-Ranges response!");
                link.setResumeable(false);
            }
            /**
             * Save timestamp when download was started. </br>
             * Serverside wait time until next download can be started counts from beginning of first/last download.
             */
            if (currentIP != null) {
                synchronized (blockedIPsMap) {
                    blockedIPsMap.put(currentIP, System.currentTimeMillis());
                    this.getPluginConfig().setProperty(PROPERTY_LAST_BLOCKED_IPS_MAP, new HashMap<String, Long>(blockedIPsMap));
                }
            }
            this.getPluginConfig().setProperty(PROPERTY_LAST_DOWNLOAD_STARTED_TIMESTAMP, System.currentTimeMillis());
            /* Add a download slot */
            controlMaxFreeDownloads(account, link, +1);
            try {
                /* Start download */
                dl.startDownload();
            } finally {
                /* Remove download slot */
                controlMaxFreeDownloads(account, link, -1);
            }
        } catch (final PluginException pe) {
            if (pe.getLinkStatus() == LinkStatus.ERROR_IP_BLOCKED && account != null) {
                /* IP based downloadlimit happend during account-download-attempt -> Temp disable account */
                throw new AccountUnavailableException(pe.getMessage(), pe.getValue());
            } else {
                throw pe;
            }
        }
    }

    protected void waitBeforeInteractiveCaptcha(final DownloadLink link, final long waitMillis, final int captchaTimeoutMillis) throws PluginException {
        if (waitMillis > captchaTimeoutMillis) {
            final int prePrePreDownloadWait = (int) (waitMillis - captchaTimeoutMillis);
            logger.info("Waittime is higher than interactive captcha timeout --> Waiting a part of it before solving captcha to avoid captcha-token-timeout");
            logger.info("Pre-pre download waittime seconds: " + (prePrePreDownloadWait / 1000));
            this.sleep(prePrePreDownloadWait, link);
        }
    }

    private static WeakHashMap<DownloadLink, WeakHashMap<Account, String>> INVALIDSESSIONMAP = new WeakHashMap<DownloadLink, WeakHashMap<Account, String>>();

    @Override
    public void clean() {
        try {
            super.clean();
        } finally {
            hotlinkDirectURL = null;
            synchronized (INVALIDSESSIONMAP) {
                // remove weak references
                INVALIDSESSIONMAP.size();
            }
            synchronized (PREMIUM_LINKS) {
                // call WeakHashMap.expungeStaleEntries
                for (final Entry<Account, WeakHashMap<DownloadLink, Object>> downloadsMap : PREMIUM_LINKS.entrySet()) {
                    downloadsMap.getValue().size();
                }
            }
        }
    }

    private boolean useShortChallengeTimeoutToAvoidServersideBan = false;

    @Override
    public int getChallengeTimeout(Challenge<?> challenge) {
        /**
         * If users need more than X seconds to enter the captcha [in free download mode before final download-step] and we actually send
         * the captcha input after this time has passed, rapidgator will 'ban' the IP of the user for at least 60 minutes. </br>
         * RG will first display a precise errormessage but then it will display the same message which is displayed when the user has
         * reached the daily/hourly download-limit. </br>
         * This function exists to avoid this. Instead of sending the captcha it can throw a retry exception, avoiding the 60+ minutes IP
         * 'ban'.
         */
        if (useShortChallengeTimeoutToAvoidServersideBan) {
            return FREE_CAPTCHA_EXPIRE_TIME_MILLIS;
        } else {
            return CFG_CAPTCHA.CFG.getDefaultChallengeTimeout();
        }
    }

    private void controlMaxFreeDownloads(final Account account, final DownloadLink link, final int num) {
        if (account == null) {
            synchronized (freeRunning) {
                final int before = freeRunning.get();
                final int after = before + num;
                freeRunning.set(after);
                logger.info("freeRunning(" + link.getName() + ")|max:" + getMaxSimultanFreeDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
            }
        }
    }

    private long getPluginSavedLastDownloadTimestamp(final String currentIP) {
        synchronized (blockedIPsMap) {
            final Iterator<Entry<String, Long>> it = blockedIPsMap.entrySet().iterator();
            while (it.hasNext()) {
                final Entry<String, Long> ipentry = it.next();
                final String ip = ipentry.getKey();
                final long timestamp = ipentry.getValue();
                if (StringUtils.isEmpty(ip) || System.currentTimeMillis() - timestamp >= FREE_RECONNECTWAIT_GENERAL_MILLIS * 10) {
                    /* Remove old entries */
                    it.remove();
                }
                if (ip != null && ip.equals(currentIP)) {
                    return timestamp;
                }
            }
        }
        return 0;
    }

    protected String getDirectlinkProperty(final Account account) {
        if (isPremiumAccount(null)) {
            return "premlink";
        } else if (account != null) {
            return "freelink2";
        } else {
            return "freelink";
        }
    }

    private boolean isPremiumDownload(final DownloadLink link, final Account account) {
        synchronized (PREMIUM_LINKS) {
            if (account == null) {
                return false;
            }
            final WeakHashMap<DownloadLink, Object> downloadsMap = PREMIUM_LINKS.get(account);
            return downloadsMap != null && downloadsMap.containsKey(link);
        }
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        Boolean resumeHint = null;
        final boolean resume;
        if (isPremiumAccount(account)) {
            resume = true;
        } else {
            // 2020-05-27, rapidgator now advertises that it doesn't support resume for free accounts
            // 2020-07-14: Resume works in free mode for most of all files. For some, server may return an "X-Error" header with the content
            // "Unexpected range request" - see code below.
            // 2020-08-XX: Resume officially only works in premium mode but according to some users it works in free mode too for some
            // times.
            if (isPremiumDownload(link, account)) {
                resumeHint = true;
                logger.info("resumeHint: Premium link is resumable");
            } else if (hotlinkDirectURL != null) {
                resumeHint = true;
                logger.info("resumeHint: Assume hotlink is resumable");
            } else {
                resumeHint = PluginJsonConfig.get(RapidGatorConfig.class).isEnableResumeFree();
                logger.info("resumeHint: Use plugin setting: " + resumeHint);
            }
            resume = link.getBooleanProperty(DownloadLink.PROPERTY_RESUMEABLE, resumeHint);
        }
        logger.info("ResumeHint: " + resumeHint + " | Used resume value: " + resume);
        return resume;
    }

    private int getMaxChunks(final DownloadLink link, final Account account) {
        if (isPremiumAccount(account) || isPremiumDownload(link, account)) {
            final long knownDownloadSize = link.getKnownDownloadSize();
            if (knownDownloadSize > 0 && knownDownloadSize <= 1024l * 1024l * 2) {
                // 2022-12-06: small files seem to create issues with multiple connections. Reference:
                // https://board.jdownloader.org/showthread.php?t=92303
                return 1;
            } else {
                // 21.11.16, check highest that can be handled without server issues
                return -5;
            }
        } else {
            /* Free & Free account */
            return 1;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return freeRunning.get() + 1;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        synchronized (account) {
            if (PluginJsonConfig.get(RapidGatorConfig.class).isEnableAPIPremium()) {
                return fetchAccountInfoAPI(account);
            } else {
                return fetchAccountInfoWebsite(account);
            }
        }
    }

    public AccountInfo fetchAccountInfoAPI(final Account account) throws Exception {
        synchronized (account) {
            final Map<String, Object> responsemap = loginAPI(account);
            return parseAPIAccountInfo(account, responsemap, new AccountInfo());
        }
    }

    public AccountInfo parseAPIAccountInfo(final Account account, final Map<String, Object> response_map, final AccountInfo ai) throws Exception {
        synchronized (account) {
            final Map<String, Object> storagemap = (Map<String, Object>) response_map.get("storage");
            if (storagemap != null) {
                ai.setUsedSpace(((Number) storagemap.get("total")).longValue() - ((Number) storagemap.get("left")).longValue());
            }
            final Map<String, Object> usermap = (Map<String, Object>) response_map.get("user");
            /*
             * E.g. "traffic":{"total":null,"left":null} --> Free Account
             */
            /*
             * 2019-12-16: Traffic is valid for the complete runtime of a premium package. If e.g. user owns a 1-year-account and traffic is
             * down to 0 after one week, account is still a premium account but worthless. Not even free downloads are possible with such
             * accounts!
             */
            /*
             * 2019-12-17: They might also have an unofficial daily trafficlimit of 50-100GB. After this the user will first get a new
             * password via E-Mail and if he continues to download 'too much', account might get temporarily banned.
             */
            if (Boolean.TRUE.equals(usermap.get("is_premium"))) {
                final Map<String, Object> trafficmap = (Map<String, Object>) usermap.get("traffic");
                final Number traffic_left = (Number) trafficmap.get("left");
                final Number traffic_max = (Number) trafficmap.get("total");
                final Number premium_end_time_timestamp = (Number) usermap.get("premium_end_time");
                if (premium_end_time_timestamp != null) {
                    /*
                     * 2019-12-23: Premium accounts expire too early if we just set the expire-date. Using their Android App even they will
                     * display the wrong expire date there. We have to add 24 hours to correct this.
                     */
                    ai.setValidUntil(premium_end_time_timestamp.longValue() * 1000 + (24 * 60 * 60 * 1000l), br);
                }
                if (traffic_left != null) {
                    ai.setTrafficLeft(traffic_left.longValue());
                }
                if (traffic_max != null) {
                    /* APIv2 */
                    ai.setTrafficMax(traffic_max.longValue());
                }
                setAccountLimitsByType(account, AccountType.PREMIUM);
            } else {
                setAccountLimitsByType(account, AccountType.FREE);
                /* API returns null value for traffic left for free accounts --> Display them as unlimited traffic! */
                ai.setUnlimitedTraffic();
            }
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                /* For developers: Display more information in GUI. */
                ai.setStatus(account.getType().getLabel() + " | Status for devs: " + usermap.get("state_label"));
            }
            return ai;
        }
    }

    public AccountInfo fetchAccountInfoWebsite(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        loginWebsite(account, true);
        if (!isPremiumAccount(account)) {
            /* Free account: Captcha is required for downloading. */
            setAccountLimitsByType(account, AccountType.FREE);
            ai.setUnlimitedTraffic();
            if (PluginJsonConfig.get(RapidGatorConfig.class).isActivateExperimentalWaittimeHandling()) {
                final String currentIP = new BalancedWebIPCheck(br.getProxy()).getExternalIP().getIP();
                final long lastdownload_timestamp = getPluginSavedLastDownloadTimestamp(currentIP);
                final long passedTimeSinceLastFreeDownloadMilliseconds = System.currentTimeMillis() - lastdownload_timestamp;
                if (passedTimeSinceLastFreeDownloadMilliseconds < FREE_RECONNECTWAIT_GENERAL_MILLIS) {
                    throw new AccountUnavailableException("IP limit reached", FREE_RECONNECTWAIT_GENERAL_MILLIS - passedTimeSinceLastFreeDownloadMilliseconds);
                }
            }
        } else {
            br.getPage("/profile/index");
            /*
             * 2019-12-16: Traffic is valid for the complete runtime of a premium package. If e.g. user owns a 1-year-account and traffic is
             * down to 0 after one week, account is still a premium account but worthless. Not even free downloads are possible with such
             * accounts!
             */
            final String availableTrafficStr = br.getRegex(">\\s*Bandwith available\\s*</td>\\s*<td>\\s*([^<>\"]*?) of").getMatch(0);
            final String availableTrafficMax = br.getRegex(">\\s*Bandwith available\\s*</td>\\s*<td>\\s*[^<>\"]*? of (\\d+(\\.\\d+)? (?:MB|GB|TB))").getMatch(0);
            logger.info("availableTraffic = " + availableTrafficStr);
            if (availableTrafficStr != null) {
                ai.setTrafficLeft(SizeFormatter.getSize(availableTrafficStr.trim()));
                if (availableTrafficMax != null) {
                    ai.setTrafficMax(SizeFormatter.getSize(availableTrafficMax));
                }
            } else {
                /*
                 * Probably not true but our errorhandling for empty traffic should work well enough to catch "not enough traffic left"
                 * related problems during download attempts.
                 */
                ai.setUnlimitedTraffic();
            }
            String expireDate = br.getRegex("Premium services will end on ([^<>\"]*?)\\.<br").getMatch(0);
            if (expireDate == null) {
                if (expireDate == null) {
                    expireDate = br.getRegex(">\\s*Premium till (\\d{4}-\\d{2}-\\d{2})").getMatch(0);
                    if (expireDate == null) {
                        expireDate = br.getRegex("(?:box-login|login-open).*Premium till (\\d{4}-\\d{2}-\\d{2})").getMatch(0);
                    }
                }
            }
            if (expireDate == null) {
                /**
                 * E.g. subscriptions
                 */
                br.getPage("/Payment/Payment");
                expireDate = br.getRegex("\\d+\\s*</td>\\s*<td style=\"width.*?>(\\d{4}-\\d{2}-\\d{2})\\s*<").getMatch(0);
            }
            if (expireDate != null) {
                /*
                 * 2019-12-18: Rapidgator accounts do have precise expire timestamps but we can only get them via API, see
                 * fetchAccountInfo_api. In website mode we set it like this to make sure that the user can use his account the whole last
                 * day no matter which exact time of the day it expires.
                 */
                expireDate += " 23:59:59";
                ai.setValidUntil(TimeFormatter.getMilliSeconds(expireDate, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH), br);
            } else {
                logger.warning("Could not find premium expire date!");
            }
            setAccountLimitsByType(account, AccountType.PREMIUM);
        }
        {
            final Browser brc = br.cloneBrowser();
            brc.getPage("/subscription/list");
            final String[][] activeSubscriptions = brc.getRegex("<td[^>]*>\\s*(\\d+-\\d+-\\d+ \\d+:\\d+:\\d+)\\s*</td>\\s*<td[^>]*>\\s*<a href=\"/subscription/links/id/lef\">\\s*(.*?)\\s*</a>\\s*</td>\\s*<td[^>]*>\\s*(.*?)\\s*</td>\\s*<td[^>]*>\\s*(\\d+-\\d+-\\d+ \\d+:\\d+:\\d+)\\s*</td>\\s*<td[^>]*>\\s*(active)\\s*</td>\\s*<td[^>]*>\\s*(.*?)\\s*</td>").getMatches();
            final StringBuilder sb = new StringBuilder();
            for (String activeSubscription[] : activeSubscriptions) {
                String subscriptionStart = activeSubscription[0];
                String subscriptionSite = activeSubscription[1];
                String subscriptionBandwidth = activeSubscription[2];
                String subscriptionEnd = activeSubscription[3];
                String subscriptionStatus = activeSubscription[4];
                String subscriptionAutoRenew = activeSubscription[4];
                if (sb.length() > 0) {
                    sb.append(",");
                }
                sb.append(subscriptionSite);
            }
            if (sb.length() > 0) {
                ai.setStatus(account.getType() + "|active subscriptions on=" + sb.toString());
            }
        }
        return ai;
    }

    protected void setAccountLimitsByType(final Account account, final AccountType type) {
        if (type == AccountType.PREMIUM || type == AccountType.LIFETIME) {
            /* Premium */
            account.setMaxSimultanDownloads(-1);
            /* Allow usage of multiple premium accounts at the same time */
            account.setConcurrentUsePossible(true);
        } else {
            /* Free / unknown / other */
            account.setMaxSimultanDownloads(1);
            /* Allow usage of multiple free accounts at the same time */
            account.setConcurrentUsePossible(true);
            /* Download limits are solely IP based so an IP change will also reset account limits. */
            account.setAllowReconnectToResetLimits(true);
        }
        account.setType(type);
    }

    /**
     * @param validateCookies
     *            true = Check whether stored cookies are still valid, if not, perform full login <br/>
     *            false = Set stored cookies and trust them if they're not older than 300000l
     *
     */
    private boolean loginWebsite(final Account account, final boolean validateCookies) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                /*
                 * Make sure that we're logged in. Doing this for every downloadlink might sound like a waste of server capacity but really
                 * it doesn't hurt anybody.
                 */
                br.setCookies(getHost(), cookies);
                if (!validateCookies) {
                    /* Do not validate cookies */
                    return false;
                }
                final long cookies_timestamp = account.getLongProperty(PROPERTY_timestamp_session_create_website, 0);
                logger.info("VerifyCookies:Timestamp:" + cookies_timestamp + "|Age:" + TimeFormatter.formatMilliSeconds((System.currentTimeMillis() - cookies_timestamp), 0));
                accessMainpage(br);
                if (isLoggedINWebsite(br)) {
                    logger.info("Successfully validated cookies:Timestamp:" + cookies_timestamp + "|Age:" + TimeFormatter.formatMilliSeconds((System.currentTimeMillis() - cookies_timestamp), 0));
                    if (sessionReUseAllowed(account, PROPERTY_timestamp_session_create_website, WEBSITE_SESSION_ID_REFRESH_TIMEOUT_MINUTES)) {
                        setAccountTypeWebsite(account, br);
                        setAccountSession(account, br);
                        return true;
                    } else {
                        logger.info("Session is valid but we aren't allowed to re-use it:Timestamp:" + cookies_timestamp + "|Age:" + TimeFormatter.formatMilliSeconds((System.currentTimeMillis() - cookies_timestamp), 0));
                    }
                } else {
                    logger.info("Cookie login failed:Timestamp:" + cookies_timestamp + "|Age:" + TimeFormatter.formatMilliSeconds((System.currentTimeMillis() - cookies_timestamp), 0));
                }
            }
            clearAccountSession(account, br);
            accessMainpage(br);
            boolean loginSuccess = false;
            boolean accountRequires2FALoginCode = false;
            for (int i = 1; i <= 3; i++) {
                logger.info("Website login attempt " + i + " of 3");
                br.getPage("/auth/login");
                Form loginform = br.getFormbyProperty("id", "login");
                if (loginform == null) {
                    /* 2024-04-30: Form id was changed to "registration" -> id="registration" hm */
                    loginform = br.getFormbyActionRegex(".*/auth/login");
                }
                if (loginform == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                String user = loginform.getBestVariable("email");
                String pass = loginform.getBestVariable("password");
                if (user == null) {
                    user = "LoginForm%5Bemail%5D";
                }
                if (pass == null) {
                    pass = "LoginForm%5Bpassword%5D";
                }
                loginform.put(user, Encoding.urlEncode(account.getUser()));
                loginform.put(pass, Encoding.urlEncode(account.getPass()));
                final String captcha_url = br.getRegex("\"(/auth/captcha/v/[a-z0-9]+)\"").getMatch(0);
                if (captcha_url != null) {
                    /* Login-captcha */
                    final DownloadLink dummyLink = new DownloadLink(this, "Account", this.getHost(), "https://" + this.getHost(), true);
                    final String code = getCaptchaCode(captcha_url, dummyLink);
                    loginform.put("LoginForm%5BverifyCode%5D", Encoding.urlEncode(code));
                } else if (i > 1 && !accountRequires2FALoginCode) {
                    /* 2nd+ attempt but no captcha and no 2FA login -> Invalid login credentials */
                    throw new AccountInvalidException();
                }
                if (accountRequires2FALoginCode) {
                    logger.info("2FA code required");
                    final String twoFACode = this.getTwoFACode(account, "\\d{6}");
                    loginform.put("LoginForm%5BtwoStepAuthCode%5D", twoFACode);
                }
                br.submitForm(loginform);
                if (isLoggedINWebsite(br)) {
                    logger.info("Login success");
                    loginSuccess = true;
                    break;
                } else if (br.containsHTML(">\\s*Wrong e-mail or password\\.\\s*<")) {
                    throw new AccountInvalidException();
                } else {
                    /* Try again - Maybe captcha and/or 2FA code is required for login. */
                    logger.info("Login failed");
                    if (br.containsHTML(">\\s*Invalid auth code")) {
                        /**
                         * 2FA code required or previously entered code is invalid. This also means that the users' login credentials are
                         * valid. </br>
                         * Ask user for 2FA login code in next round.
                         */
                        logger.info("2FA code needed");
                        accountRequires2FALoginCode = true;
                    }
                    continue;
                }
            }
            if (!loginSuccess) {
                /* Login failed -> Check why */
                if (accountRequires2FALoginCode) {
                    /* Valid login credentials but invalid 2FA code */
                    throw new AccountInvalidException(org.jdownloader.gui.translate._GUI.T.jd_gui_swing_components_AccountDialog_2FA_login_invalid());
                } else {
                    /* Invalid login credentials */
                    throw new AccountInvalidException();
                }
            } else {
                setAccountTypeWebsite(account, br);
                setAccountSession(account, br);
                return true;
            }
        }
    }

    private void clearAccountSession(Account account, Browser br) {
        synchronized (account) {
            br.clearCookies(null);
            final long cookies_timestamp = account.getLongProperty(PROPERTY_timestamp_session_create_website, 0);
            account.clearCookies("");
            account.removeProperty(PROPERTY_timestamp_session_create_website);
            if (cookies_timestamp > 0) {
                logger.info("ClearCookies:Timestamp:" + cookies_timestamp + "|Age:" + TimeFormatter.formatMilliSeconds((System.currentTimeMillis() - cookies_timestamp), 0));
            }
        }
    }

    private void setAccountSession(Account account, Browser br) {
        synchronized (account) {
            account.saveCookies(br.getCookies(br.getHost()), "");
            account.setProperty(PROPERTY_timestamp_session_create_website, System.currentTimeMillis());
        }
    }

    /**
     * Returns whether or not session is allowed to be re-used regardless of whether it is valid or not --> Only based on the max. time we
     * are using a session. Only call this if you have validated the session before and are sure that the current session is valid!!
     */
    private boolean sessionReUseAllowed(final Account account, final String session_create_property, final int session_refresh_timeoutMinutes) {
        if (session_refresh_timeoutMinutes > 0) {
            logger.info(String.format("Currently sessions are re-freshed every %d minutes", session_refresh_timeoutMinutes));
        }
        final long timestamp_session_validity = account.getLongProperty(session_create_property, 0) + session_refresh_timeoutMinutes * 60 * 1000l;
        if (session_refresh_timeoutMinutes > 0 && System.currentTimeMillis() > timestamp_session_validity) {
            /*
             * 2019-12-23: We could avoid checking sessions as we know their age before already but I currently want all session_ids to get
             * checked to get better log results/find serverside issues.
             */
            logger.info(String.format("session seems to be valid but we'll get a new one as current session is older than %d minutes", session_refresh_timeoutMinutes));
            return false;
        } else {
            if (session_refresh_timeoutMinutes > 0) {
                final long timestamp_remaining_session_validity = timestamp_session_validity - System.currentTimeMillis();
                logger.info("Unless it expires serverside, current session is internally considered valid for: " + TimeFormatter.formatMilliSeconds(timestamp_remaining_session_validity, 0));
            }
            logger.info("Re-using last session");
            return true;
        }
    }

    private boolean isLoggedINWebsite(final Browser br) {
        if (br.getCookie(br.getHost(), "user__", Cookies.NOTDELETEDPATTERN) != null || br.containsHTML("auth/logout\"")) {
            return true;
        } else {
            return false;
        }
    }

    private void setAccountTypeWebsite(final Account account, final Browser br) {
        if (br.containsHTML("Account\\s*:\\&nbsp;<a href=\"/article/premium\">\\s*Free\\s*</a>")) {
            this.setAccountLimitsByType(account, AccountType.FREE);
        } else {
            this.setAccountLimitsByType(account, AccountType.PREMIUM);
        }
    }

    /** Returns session_id stored on given account object. */
    private String getAccountSession(final Account account) {
        synchronized (account) {
            return account.getStringProperty(PROPERTY_sessionid, null);
        }
    }

    private Map<String, Object> loginAPI(final Account account) throws Exception {
        synchronized (account) {
            final long lastPleaseWait = account.getLongProperty("lastPleaseWait", -1);
            final long pleaseWait = lastPleaseWait > 0 ? ((5 * 60 * 1000l) - (System.currentTimeMillis() - lastPleaseWait)) : 0;
            if (pleaseWait > 5000) {
                throw new AccountUnavailableException("Frequest logins. Please wait!", pleaseWait);
            }
            String session_id = getAccountSession(account);
            if (session_id != null) {
                final long session_timestamp = account.getLongProperty(PROPERTY_timestamp_session_create_api, 0);
                logger.info("VerifySession:" + session_id + "|Timestamp:" + session_timestamp + "|Age:" + TimeFormatter.formatMilliSeconds((System.currentTimeMillis() - session_timestamp), 0));
                /* Try to re-use last token */
                br.getPage(getAPIBase() + "user/info?token=" + Encoding.urlEncode(session_id));
                try {
                    final Map<String, Object> response = handleErrors_api(null, null, account, br);
                    logger.info("Successfully validated last session:" + session_id + "|Timestamp:" + session_timestamp + "|Age:" + TimeFormatter.formatMilliSeconds((System.currentTimeMillis() - session_timestamp), 0));
                    if (sessionReUseAllowed(account, PROPERTY_timestamp_session_create_api, API_SESSION_ID_REFRESH_TIMEOUT_MINUTES)) {
                        final Map<String, Object> ret = new HashMap<String, Object>(response);
                        // required for later use of getAccountSession(Map)
                        ret.put("token", session_id);
                        return ret;
                    } else {
                        throw new AccountInvalidException("Existing session looks to be valid but we are not allowed to re-use it");
                    }
                } catch (final PluginException e) {
                    logger.log(e);
                }
            }
            clearAccountSession(account, session_id);
            br.clearCookies(null);
            /* Avoid full logins - RG will temporarily block accounts on too many full logins in a short time! */
            logger.info("Performing full login");
            /* Docs: https://rapidgator.net/article/api/user#login */
            br.getPage(getAPIBase() + "user/login?login=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
            final Map<String, Object> response = handleErrors_api(null, null, account, br);
            session_id = getAccountSession(response);
            if (StringUtils.isEmpty(session_id)) {
                /* This should never happen */
                throw new AccountUnavailableException("Fatal: Failed to find sessionID", 1 * 60 * 1000l);
            } else {
                /* Store session_id */
                setAccountSession(account, session_id);
                return response;
            }
        }
    }

    private void setAccountSession(Account account, final String session_id) {
        synchronized (account) {
            account.setProperty(PROPERTY_sessionid, session_id);
            account.setProperty(PROPERTY_timestamp_session_create_api, System.currentTimeMillis());
        }
    }

    private void clearAccountSession(Account account, final String clear_session_id) {
        synchronized (account) {
            final String session_id = getAccountSession(account);
            if (clear_session_id == null || clear_session_id.equals(session_id)) {
                account.setType(null);
                final long session_timestamp = account.getLongProperty(PROPERTY_timestamp_session_create_api, 0);
                account.removeProperty(PROPERTY_sessionid);
                account.removeProperty(PROPERTY_timestamp_session_create_api);
                if (session_id != null) {
                    logger.info("ClearSession:" + session_id + "|Timestamp:" + session_timestamp + "|Age:" + TimeFormatter.formatMilliSeconds((System.currentTimeMillis() - session_timestamp), 0));
                }
            }
        }
    }

    private void accessMainpage(final Browser br) throws Exception {
        br.getPage("https://" + this.getHost() + "/");
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (isPremiumAccount(account) && PluginJsonConfig.get(RapidGatorConfig.class).isEnableAPIPremium()) {
            /* Premium account -> API can be used if preferred by the user. */
            handlePremium_api(link, account);
        } else {
            /* Free account -> Website */
            handleDownloadWebsite(link, account);
        }
    }

    private Map<String, Object> handleErrors_api(final String session_id, final DownloadLink link, final Account account, final Browser br) throws Exception {
        return handleErrors_api(session_id, link, account, br, false);
    }

    private Map<String, Object> handleErrors_api(final String session_id, final DownloadLink link, final Account account, final Browser br, final boolean trustError404) throws Exception {
        final URLConnectionAdapter con = br.getHttpConnection();
        /* Handle bare responsecodes first, then API */
        if (con.getResponseCode() == 401) {
            /* Invalid logindata */
            throw new AccountInvalidException();
        } else if (con.getResponseCode() == 404) {
            handle404API(link, account, session_id, trustError404);
        } else if (con.getResponseCode() == 416) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 416", 5 * 60 * 1000l);
        } else if (con.getResponseCode() == 423) {
            // HTTP/1.1 423 Locked
            // {"response":null,"response_status":423,"response_details":"Error: Exceeded traffic"}
            // Hotlink?!
            /* 2019-12-16: {"response":null,"status":423,"details":"Error: Exceeded traffic"} --> See code below! */
        } else if (con.getResponseCode() == 500) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 500", 60 * 60 * 1000l);
        } else if (con.getResponseCode() == 503) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 503: Service Temporarily Unavailable", 5 * 60 * 1000l);
        }
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid API response", 60 * 1000l);
            } else {
                throw new AccountUnavailableException("Invalid API response", 60 * 1000);
            }
        }
        final Map<String, Object> response = (Map<String, Object>) entries.get("response");
        synchronized (account) {
            final Boolean success = (Boolean) entries.get("success");
            if (response == null && Boolean.TRUE.equals(success)) {
                /* No error */
                return entries;
            }
            String errorMessage = (String) entries.get("response_details");
            if (errorMessage == null) {
                errorMessage = (String) entries.get("details");
                if (errorMessage == null) {
                    /* There can be two types of response-maps */
                    /* E.g. {"error":"Denied by IP","success":false} */
                    errorMessage = (String) entries.get("error");
                }
            }
            final String details = (String) entries.get("details");
            final Object statusO = entries.get("status");
            final int status = (statusO != null && statusO instanceof Number) ? ((Number) statusO).intValue() : 200;
            if (statusO != null && status == 200) {
                /* No error */
                return response;
            }
            logger.info("ErrorMessage: " + errorMessage + "|Status:" + status + "|Details:" + details);
            if (errorMessage != null) {
                if (status == 423 || StringUtils.containsIgnoreCase(errorMessage, "Exceeded traffic")) {
                    /* 2019-12-16: {"response":null,"status":423,"details":"Error: Exceeded traffic"} */
                    throw new AccountUnavailableException("Reached daily downloadlimit", 5 * 60 * 1000);
                }
                if (errorMessage.contains("Denied by IP")) {
                    throw new AccountUnavailableException("Denied by IP", 2 * 60 * 60 * 1000l);
                } else if (errorMessage.contains("Please wait")) {
                    account.setProperty("lastPleaseWait", System.currentTimeMillis());
                    if (link == null) {
                        /* we are inside fetchAccountInfo */
                        throw new AccountUnavailableException("Frequent logins. Please wait", 5 * 60 * 1000l);
                    } else {
                        /* we are inside handlePremium */
                        throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server says: 'Please wait ...'", 10 * 60 * 1000l);
                    }
                } else if (errorMessage.contains("User is not PREMIUM") || errorMessage.contains("This file can be downloaded by premium only") || errorMessage.contains("You can download files up to")) {
                    if (session_id != null) {
                        clearAccountSession(account, session_id);
                    }
                    throw new AccountRequiredException();
                } else if (errorMessage.contains("Login or password is wrong") || errorMessage.contains("Error: Error e-mail or password")) {
                    throw new AccountInvalidException(errorMessage);
                } else if (errorMessage.contains("Password cannot be blank")) {
                    throw new AccountInvalidException(errorMessage);
                } else if (errorMessage.contains("User is FROZEN")) {
                    throw new AccountInvalidException(errorMessage);
                } else if (StringUtils.containsIgnoreCase(errorMessage, "Error: ACCOUNT LOCKED FOR VIOLATION OF OUR TERMS. PLEASE CONTACT SUPPORT.")) {
                    // most likely account sharing as result of shared account dbs.
                    throw new AccountInvalidException(errorMessage);
                } else if (errorMessage.contains("Parameter login or password is missing")) {
                    /*
                     * Unusual case but this may also happen frequently if users use strange chars as username/password so simply treat this
                     * as "login/password wrong"!
                     */
                    throw new AccountInvalidException(errorMessage);
                } else if (status == 401 && StringUtils.containsIgnoreCase(errorMessage, "Wrong e-mail or password")) {
                    /* 2019-12-14: {"response":null,"response_status":401,"response_details":"Error: Wrong e-mail or password."} */
                    throw new AccountInvalidException(errorMessage);
                } else if (status == 401 || StringUtils.containsIgnoreCase(errorMessage, "Session not exist") || StringUtils.containsIgnoreCase(errorMessage, "Session doesn't exist")) {
                    // {"response":null,"status":401,"details":"Error. Session doesn't exist"}
                    // {"response":null,"status":401,"details":"Error. Session not exist"}
                    handleInvalidSession(link, account, session_id, "401");
                }
            }
            if (status == 404) {
                handle404API(link, account, session_id, trustError404);
            } else if (status == 500) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "API error 500", 5 * 60 * 1000l);
            } else if (status == 503) {
                // {"response":null,"status":503,"details":"Download temporarily unavailable"}
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errorMessage, 30 * 60 * 1000l);
            } else if (StringUtils.containsIgnoreCase(errorMessage, "This download session is not for you")) {
                handleInvalidSession(link, account, session_id, "Error 'This download session is not for you'");
            } else if (StringUtils.containsIgnoreCase(errorMessage, "Session not found")) {
                handleInvalidSession(link, account, session_id, "Error 'Session not found'");
            } else if (errorMessage.contains("Error: You requested login to your account from unusual Ip address")) {
                /* User needs to confirm his current IP. */
                String statusMessage;
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    statusMessage = "\r\nBitte bestätige deine aktuelle IP Adresse über den Bestätigungslink per E-Mail um den Account wieder nutzen zu können.";
                } else {
                    statusMessage = "\r\nPlease confirm your current IP adress via the activation link you got per mail to continue using this account.";
                }
                throw new AccountUnavailableException(statusMessage, 1 * 60 * 1000l);
            } else {
                logger.info("Unknown error happened");
                if (link != null) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errorMessage, 60 * 1000l);
                } else {
                    throw new AccountUnavailableException(errorMessage, 60 * 1000);
                }
            }
            /*
             * Unknown error?! TODO: Throw exception here once Rapidgator plugin runs better with APIv2 and all other glitches have been
             * taken care of!
             */
            // throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return response;
    }

    private String getAccountSession(final Map<String, Object> map) throws PluginException {
        /** Returns session_id stored on given account object. */
        /* 2019-12-14: session_id == PHPSESSID cookie */
        String ret = (String) map.get("session_id");
        if (StringUtils.isEmpty(ret)) {
            /* 2019-12-14: APIv2 */
            ret = (String) map.get("token");
        }
        if (StringUtils.isEmpty(ret)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            return ret;
        }
    }

    public void handlePremium_api(final DownloadLink link, final Account account) throws Exception {
        String directurl = null;
        String session_id = null;
        if (hotlinkDirectURL != null) {
            directurl = hotlinkDirectURL;
        } else {
            final Map<String, Object> response_map = loginAPI(account);
            session_id = getAccountSession(response_map);
            final AccountInfo ai = account.getAccountInfo();
            if (ai != null) {
                parseAPIAccountInfo(account, response_map, ai);
            }
            this.requestFileInformationAPI(link, account, session_id);
            /* Docs: https://rapidgator.net/article/api/file#download */
            br.getPage(getAPIBase() + "file/download?token=" + session_id + "&file_id=" + Encoding.urlEncode(this.getFID(link)));
            final Map<String, Object> response = handleErrors_api(session_id, link, account, br, false);
            directurl = (String) response.get("download_url");
            if (StringUtils.isEmpty(directurl)) {
                /* This should never happen! */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        if (PluginJsonConfig.get(RapidGatorConfig.class).isExperimentalEnforceSSL()) {
            directurl = directurl.replaceFirst("(?i)^http://", "https://");
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, directurl, isResumeable(link, account), getMaxChunks(link, account));
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            logger.warning("The final dllink seems not to be a file!");
            br.followConnection(true);
            /* We are not logged in when e.g. hotlink is available. */
            if (session_id != null && br.getRequest().getHtmlCode().startsWith("{")) {
                handleErrors_api(session_id, link, account, br);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server did not respond with file content");
            }
        }
        try {
            dl.startDownload();
        } finally {
            synchronized (INVALIDSESSIONMAP) {
                final WeakHashMap<Account, String> map = INVALIDSESSIONMAP.get(link);
                if (map != null) {
                    map.remove(account);
                    if (map.size() == 0) {
                        INVALIDSESSIONMAP.remove(link);
                    }
                }
            }
        }
    }

    private void handle404API(final DownloadLink link, final Account account, final String session_id, final boolean trustError404) throws Exception {
        logger.info("Error 404 happened --> Trying to find out whether session is invalid or file is offline");
        if (link == null) {
            /* This should never happen */
            handleInvalidSession(link, account, session_id, null);
        }
        if (trustError404) {
            /* File offline */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            /*
             * 2019-12-14: Rapidgator API IN SOME SITUATIONS has a bug which will return invalid offline status. Do NOT trust this status
             * anymore! Wait and retry instead. If the file is offline, availableStatus will find that correct status eventually! This may
             * happen in two cases: 1. Free/Expired premium account tries to download via API.
             */
            /* Either bad session_id or file offline */
            /*
             * 2019-12-18: Seems like our session validity check still does not work which will lead to false positive 'file not found'
             * errors --> Avoid this and retry later instead! Proof: jdlog://6540330900751/
             */
            final boolean trust_api_session_validity_check = false;
            if (trust_api_session_validity_check) {
                logger.info("Checking for invalid session or 404 file not found");
                if (validateSessionAPI(account, session_id)) {
                    /* Trust previous error --> File is offline */
                    logger.info("Session is valid --> File is offline");
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else {
                    /* Get new session_id on next accountcheck */
                    logger.info("Session is invalid");
                    handleInvalidSession(link, account, session_id, null);
                }
            } else {
                /*
                 * Session validity check cannot be trusted either --> Check if URL is really offline; if yes, display offline; temp disable
                 * account and wait for new session
                 */
                requestFileInformationWebsite(link, null);
                logger.info("File is online --> Probably expired session");
                /* Probably expired session */
                handleInvalidSession(link, account, session_id, "404");
            }
        }
    }

    /** Checks if current login session is valid. */
    private boolean validateSessionAPI(final Account account, final String session_id) throws Exception {
        synchronized (account) {
            /*
             * 2019-12-16: Check running remote uploads to validate session as there is no extra API call available for verifying sessions
             * --> This should return the following for most users: {"response":[],"status":200,"details":null}
             */
            br.getPage(getAPIBase() + "remote/info?token=" + session_id);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            // Alternative way: this.getPage(this.API_BASEv2 + "trashcan/content?token=" + session_id);
            if (((Number) entries.get("status")).intValue() == 200) {
                return true;
            } else {
                return false;
            }
        }
    }

    /** Call this on expired session_id! */
    private void handleInvalidSession(final DownloadLink link, final Account account, final String session_id, final String error_hint) throws PluginException {
        synchronized (INVALIDSESSIONMAP) {
            if (link != null && account != null) {
                WeakHashMap<Account, String> map = INVALIDSESSIONMAP.get(link);
                if (map == null) {
                    // link never tried before -> add account/session_id combination
                    map = new WeakHashMap<Account, String>();
                    map.put(account, session_id);
                    INVALIDSESSIONMAP.put(link, map);
                    throwAccountUnavailableException(account, error_hint);
                } else if (!map.containsKey(account)) {
                    // link tried before, but never with account -> add account/session_id combination
                    map.put(account, session_id);
                    throwAccountUnavailableException(account, error_hint);
                } else if (!StringUtils.equals(map.get(account), session_id)) {
                    // we're tried this link before with same account but different session_id and still failing -> issue is with file and
                    // not
                    // session_id
                    map.remove(account);
                    if (map.size() == 0) {
                        INVALIDSESSIONMAP.remove(link);
                    }
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "File seems to be temporarily not available, please try again later", 30 * 60 * 1000l);
                } else {
                    throwAccountUnavailableException(account, error_hint);
                }
            } else {
                throwAccountUnavailableException(account, error_hint);
            }
        }
    }

    private void throwAccountUnavailableException(final Account account, final String error_hint) throws PluginException {
        /* We should not have to reset the session_id property here as it should happen automatically on next accountcheck! */
        final long waittime = 1 * 60 * 1000l;
        if (error_hint != null) {
            throw new AccountUnavailableException(String.format("[%s]Session expired - waiting before opening new session", error_hint), waittime);
        } else {
            throw new AccountUnavailableException("Session expired - waiting before opening new session", waittime);
        }
    }

    private void handleErrorsWebsite(final Browser br, final DownloadLink link, final Account account, final String currentIP) throws PluginException {
        handleErrorsWebsite(br, link, account, currentIP, false);
    }

    /**
     * Returns error message for files that require the user to be subscribed to a specific uploader to be able to download them. <br>
     *
     * This can even happen for premium account owners since an extra subscription is needed to download such files. </br>
     * This can be the same as when "isBuyFile()" returns true but with a more detailed error message.
     */
    private String getErrormessageSubscriberOnlyDownload(final Browser br) {
        return br.getRegex("(The files of this publisher \"[^\"<>]+\" can be downloaded only by subscribers\\.)").getMatch(0);
    }

    /**
     * Returns error message for files that need to be bought separately. <br>
     * This can be the same as when "isBuyFile()" returns true but with a more detailed error message.
     */
    private String getErrormessagePaidDownload(final Browser br) {
        return br.getRegex("\">\\s*(Buy for [0-9.]+ USD)").getMatch(0);
    }

    private void handleErrorsWebsite(final Browser br, final DownloadLink link, final Account account, final String currentIP, final boolean doExtendedOfflineCheck) throws PluginException {
        /* 2020-07-28: Resume can now also fail with error 500 and json: {"error":"Unexpected range request","success":false} */
        String errorMsgFromJson = null;
        if (br.getRequest().getHtmlCode().startsWith("{")) {
            /* Check for json response and parse it to extract error message from json. */
            try {
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                if (Boolean.FALSE.equals(entries.get("success"))) {
                    errorMsgFromJson = entries.get("error").toString();
                }
            } catch (final Exception e) {
                logger.log(e);
                logger.warning("Response looked like json but is not json");
            }
        }
        final String errorMsgHeader = br.getHttpConnection().getRequest().getResponseHeader("X-Error");
        if (StringUtils.equalsIgnoreCase("Unexpected range request", errorMsgHeader) || StringUtils.equalsIgnoreCase(errorMsgFromJson, "Unexpected range request")) {
            /* Resume impossible */
            if (isResumeable(link, account)) {
                /* Resume was attempted but failed. */
                logger.info("Resume impossible, disabling it for the next try");
                /* Disable resume on next attempt */
                link.setResumeable(false);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Resume failed");
            } else {
                /* Resume was already disabled? Then we cannot do anything about it --> Wait and retry later */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown resume related server error");
            }
        } else if (errorMsgFromJson != null) {
            /* e.g. {"error":"Denied by IP","success":false} */
            throw new PluginException(LinkStatus.ERROR_FATAL, errorMsgFromJson);
        }
        if (account != null) {
            /* Errors which should only happen in account mode */
            if (br.containsHTML("You have reached quota|You have reached daily quota of downloaded information for premium accounts")) {
                logger.info("You've reached daily download quota for " + account.getUser() + " account");
                throw new AccountUnavailableException("Daily downloadlimit reached", 5 * 60 * 1000);
            } else if (!this.isLoggedINWebsite(br)) {
                throw new AccountUnavailableException("Session expired?", 1 * 60 * 1000);
            }
        }
        /* Check for offline file */
        checkOfflineWebsite(br, link, doExtendedOfflineCheck);
        if (br.containsHTML("id=\"exceeded_storage\"")) {
            /**
             * 2024-10-31: <br>
             * Your storage space is full. Delete some files or upgrade to the new
             * <a href="/article/premium" style="color: #ff801a;">storage plan</a>.<br>
             * It looks like this error can happen even when a user is not logged in. At this moment we just assume that this means that the
             * uploaders' account is out of space and for this reason, the file can't be downloaded. </br>
             * This could also be a fake message which they display whenever the user tried to use a blocked proxy/VPN.
             *
             */
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Uploaders' storage is full. Wait until uploader buys more traffic to download this file");
        }
        /* Determine max wait time if a limit related error would happen in free download mode. */
        long passedTimeSinceLastDlMilliseconds = 0;
        if (currentIP != null) {
            long lastdownload_timestamp = getPluginSavedLastDownloadTimestamp(currentIP);
            if (lastdownload_timestamp == 0) {
                /*
                 * Fallback: Used when either user has disabled IP workaround handling: Just use timestamp of last started [free] download,
                 * ignore IP because this value will only be relevant if an IP limit is reached according to website.
                 */
                lastdownload_timestamp = this.getPluginConfig().getLongProperty(PROPERTY_LAST_DOWNLOAD_STARTED_TIMESTAMP, 0);
            }
            passedTimeSinceLastDlMilliseconds = System.currentTimeMillis() - lastdownload_timestamp;
        }
        final long timeMillisUntilNextFreeDownloadIsPossible = FREE_RECONNECTWAIT_BETWEEN_DOWNLOADS_MILLIS - passedTimeSinceLastDlMilliseconds;
        final long maxReconnectWait;
        if (timeMillisUntilNextFreeDownloadIsPossible > 0) {
            maxReconnectWait = timeMillisUntilNextFreeDownloadIsPossible;
        } else {
            maxReconnectWait = 2 * 60 * 60 * 1000;
        }
        final String reconnectWaitMinutesStr = br.getRegex("Delay between downloads must be not less than (\\d+) min[^<]*<").getMatch(0);
        if (reconnectWaitMinutesStr != null) {
            /*
             * Mostly 120 minutes. They do not provide the real time to wait but if we know when the last free download was started with the
             * current IP, we can calculate that.
             */
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, Math.min(maxReconnectWait, Long.parseLong(reconnectWaitMinutesStr) * 60 * 1000));
        }
        if (br.containsHTML(">\\s*Error\\. Link expired\\. You have reached your daily limit of downloads\\.")) {
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Link expired, or You've reached your daily limit ", Math.min(maxReconnectWait, FREE_RECONNECTWAIT_DAILYLIMIT_MILLIS));
        } else if (br.containsHTML(">\\s*File is already downloading\\s*<")) {
            /*
             * 2020-03-11: Do not throw ERROR_IP_BLOCKED error here as this error will usually only show up for 30-60 seconds between
             * downloads or upon instant retry of an e.g. interrupted free download --> Reconnect is not required
             */
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Wait before starting new downloads", 1 * 60 * 1000l);
        } else if (br.containsHTML("File is temporarily not available, please try again later")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "File is temporarily not available, please try again later");
        } else if (br.containsHTML(">\\s*File is temporarily unavailable, please try again later\\.\\s*Maintenance in data center")) {
            // maybe also a shadow ban?
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "File is temporarily unavailable, please try again later. Maintenance in data center");
        } else if (br.containsHTML(">\\s*You have reached your hourly downloads limit\\.")) {
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "You've reached your hourly downloads limit", Math.min(maxReconnectWait, FREE_RECONNECTWAIT_GENERAL_MILLIS));
        } else if (br.containsHTML(">\\s*You have reached your daily downloads limit")) {
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "You've reached your daily downloads limit", Math.min(maxReconnectWait, FREE_RECONNECTWAIT_GENERAL_MILLIS));
        } else if (br.containsHTML("You can`t download more than 1 file at a time in free mode\\.\\s*<|>\\s*Wish to remove the restrictions\\?")) {
            /*
             * 2020-03-11: Do not throw ERROR_IP_BLOCKED error here as this error will usually only show up for 30-60 seconds between
             * downloads or upon instant retry of an e.g. interrupted free download --> Reconnect is not required
             */
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "You can't download more than one file within a certain time period in free mode", PluginJsonConfig.get(RapidGatorConfig.class).getWaitSecondsOnErrorYouCantDownloadMoreThanOneFile() * 1000l);
        }
        if (br.containsHTML(">\\s*An unexpected error occurred\\s*\\.?\\s*<")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "An unexpected error occurred", 15 * 60 * 1000l);
        }
        /* Check if item is only downloadable for premium users. */
        final String premiumonly_subscribersOnlyDownload = getErrormessageSubscriberOnlyDownload(br);
        if (premiumonly_subscribersOnlyDownload != null) {
            throw new AccountRequiredException(premiumonly_subscribersOnlyDownload);
        }
        final String premiumonly_paidFile = getErrormessagePaidDownload(br);
        if (premiumonly_paidFile != null) {
            throw new AccountRequiredException(premiumonly_paidFile);
        }
        if (isBuyFile(br, link, account)) {
            /* 2022-11-07: Files that need to be purchased separately in order to be able to download them. */
            throw new AccountRequiredException("The files of this publisher can be downloaded only by subscribers.");
        }
        final String errormsgFreeFilesizeLimit = br.getRegex("'(You can download files up to ([\\d\\.]+ ?(MB|GB)) in free mode)\\s*<").getMatch(0);
        if (errormsgFreeFilesizeLimit != null) {
            if (isPremiumAccount(account)) {
                throw new AccountUnavailableException("Expired premium account!?", 30 * 60 * 1000);
            } else {
                throw new AccountRequiredException(errormsgFreeFilesizeLimit);
            }
        } else if (br.containsHTML("This file can be downloaded by premium only\\s*</div>")) {
            if (isPremiumAccount(account)) {
                throw new AccountUnavailableException("Expired premium account!?", 30 * 60 * 1000);
            } else {
                throw new AccountRequiredException("This file can be downloaded by premium only");
            }
        }
        /* Check for some generic http error response codes */
        if (br.getHttpConnection().getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
        } else if (br.getHttpConnection().getResponseCode() == 416) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 416", 10 * 60 * 1000l);
        }
    }

    private void checkOfflineWebsite(final Browser br, final DownloadLink link, final boolean doExtendedOfflineCheck) throws PluginException {
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*404 File not found")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (doExtendedOfflineCheck && !br.getURL().contains(this.getFID(link)) && !this.isBuyFile(br, link, null)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    /** Returns true if this is a file which needs to be bought separately. */
    private boolean isBuyFile(final Browser br, final DownloadLink link, final Account account) {
        if (StringUtils.containsIgnoreCase(br.getURL(), "/site/PleaseLogin")) {
            /*
             * 2024-07-17: Special links which display a long prompt when accessed without account and later on when logged in need to be
             * bought separately.
             */
            return true;
        } else if (StringUtils.containsIgnoreCase(br.getURL(), "/subscription/create")) {
            return true;
        } else if (br.containsHTML("/wallet/BuyFile/id/")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public Class<RapidGatorConfig> getConfigInterface() {
        return RapidGatorConfig.class;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        synchronized (PREMIUM_LINKS) {
            for (final Entry<Account, WeakHashMap<DownloadLink, Object>> downloadsMap : PREMIUM_LINKS.entrySet()) {
                downloadsMap.getValue().remove(link);
            }
        }
    }
}
