//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
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
import jd.plugins.PluginBrowser;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51779 $", interfaceVersion = 2, names = {}, urls = {})
public class FilerNet extends PluginForHost {
    private static final int     STATUSCODE_APIDISABLED                                        = 400;
    private static final String  ERRORMESSAGE_APIDISABLEDTEXT                                  = "API is disabled, please wait or use filer.net in your browser";
    private static final int     STATUSCODE_DOWNLOADTEMPORARILYDISABLED                        = 500;
    private static final String  ERRORMESSAGE_DOWNLOADTEMPORARILYDISABLEDTEXT                  = "Download temporarily disabled!";
    private static final int     STATUSCODE_UNKNOWNERROR                                       = 599;
    private static final String  ERRORMESSAGE_UNKNOWNERRORTEXT                                 = "Unknown file error";
    private static final String  DIRECT_WEB                                                    = "directlinkWeb";
    private static final String  DIRECT_API                                                    = "directlinkApi";
    /* Plugin settings */
    private static final String  SETTING_ENABLE_API_FOR_FREE_AND_FREE_ACCOUNT_DOWNLOADS        = "ENABLE_API_FOR_FREE_AND_FREE_ACCOUNT_DOWNLOADS";
    private static final boolean defaultSETTING_ENABLE_API_FOR_FREE_AND_FREE_ACCOUNT_DOWNLOADS = true;
    private static final String  DISABLE_HTTPS                                                 = "DISABLE_HTTPS";
    private static final boolean defaultSETTING_DISABLE_HTTPS                                  = false;
    private static final String  SETTING_WAIT_MINUTES_ON_ERROR_NO_FREE_SLOTS                   = "WAIT_MINUTES_ON_NO_FREE_SLOTS";
    private static final int     defaultSETTING_WAIT_MINUTES_ON_ERROR_NO_FREE_SLOTS            = 10;
    private static final String  SETTING_WAIT_MINUTES_ON_ERROR_CODE_415                        = "SETTING_WAIT_MINUTES_ON_ERROR_CODE_415";
    private static final int     defaultSETTING_WAIT_MINUTES_ON_ERROR_CODE_415                 = 5;
    /* API Docs: https://filer.net/api */
    public static final String   API_BASE                                                      = "https://api.filer.net/api";
    public static final String   BASE                                                          = "https://filer.net";

    @SuppressWarnings("deprecation")
    public FilerNet(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/register");
        setConfigElements();
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = new PluginBrowser<FilerNet>(this) {
            @Override
            public URLConnectionAdapter openRequestConnection(Request request, final boolean followRedirects) throws IOException {
                /**
                 * 2024-02-20: Ensure to enforce user-preferred protocol. </br>
                 * This can also be seen as a workaround since filer.net redirects from https to http on final download-attempt so without
                 * this, http protocol would be used even if user preferred https. <br>
                 * Atm we don't know if this is a filer.net server side bug or if this is intentional. <br>
                 * Asked support about this, waiting for feedback
                 */
                request.setURL(new URL(rewriteProtocol(request.getURL().toExternalForm())));
                return super.openRequestConnection(request, followRedirects);
            }

            @Override
            public Browser createNewBrowserInstance() {
                return FilerNet.this.createNewBrowserInstance();
            }
        };
        br.setFollowRedirects(true);
        br.getHeaders().put("User-Agent", "JDownloader");
        return br;
    }

    /** Prepares browser for website (=non-API) requests. */
    private Browser prepBrowserWebsite(final Browser br) {
        br.setFollowRedirects(true);
        br.getHeaders().put("Accept-Charset", null);
        // TODO: 2025-10-28: Why are we using this User-Agent?
        br.getHeaders().put("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_1) AppleWebKit/537.73.11 (KHTML, like Gecko) Version/7.0.1 Safari/537.73.11");
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "filer.net" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:app\\.php/)?(?:get|dl)/([a-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    private String getContentUrl(final DownloadLink link) {
        return rewriteProtocol("https://" + this.getHost() + "/get/" + getFileID(link));
    }

    public String getAPI_BASE() {
        return rewriteProtocol(API_BASE);
    }

    public String rewriteProtocol(String url) {
        if (this.getPluginConfig().getBooleanProperty(DISABLE_HTTPS, defaultSETTING_DISABLE_HTTPS)) {
            return url.replaceFirst("^https://", "http://");
        } else {
            return url.replaceFirst("^http://", "https://");
        }
    }

    private final String getFileID(DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public String getLinkID(DownloadLink link) {
        final String fileID = getFileID(link);
        if (fileID != null) {
            return getHost() + "://" + fileID;
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/agb.htm";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        // if (account == null || AccountType.PREMIUM.equals(account.getType())) {
        // return 0;
        // } else {
        // return 1;
        // }
        return 1;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 10;
    }

    @Override
    public int getTimegapBetweenConnections() {
        return 500;
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        return this.getFileID(link);
    }

    /** Using API: https://filer.net/api see "multi_status" */
    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        try {
            final StringBuilder sb = new StringBuilder();
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    /* Check up to 100 items with one request */
                    if (index == urls.length || links.size() == 100) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                sb.delete(0, sb.capacity());
                for (final DownloadLink link : links) {
                    if (sb.length() > 0) {
                        /* Add separator */
                        sb.append("|");
                    }
                    sb.append(this.getFileID(link));
                }
                final Map<String, Object> entries = (Map<String, Object>) this.callAPI(API_BASE + "/multi_status/" + sb.toString() + ".json");
                final List<Map<String, Object>> data = (List<Map<String, Object>>) entries.get("data");
                for (final DownloadLink link : links) {
                    final String fid = this.getFileID(link);
                    Map<String, Object> info = null;
                    for (final Map<String, Object> map : data) {
                        final String this_hash = (String) map.get("hash");
                        if (this_hash == null) {
                            continue;
                        }
                        if (this_hash.equals(fid)) {
                            info = map;
                            break;
                        }
                    }
                    if (info == null) {
                        /* No info about item found in json response -> Assume that this item is offline. */
                        link.setAvailable(false);
                        continue;
                    }
                    final String filename = info.get("name").toString();
                    final Number filesize = (Number) info.get("size");
                    link.setFinalFileName(filename);
                    if (filesize != null) {
                        link.setVerifiedFileSize(filesize.longValue());
                    }
                    link.setAvailable(true);
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            logger.log(e);
            return false;
        }
        return true;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Map<String, Object> resp = (Map<String, Object>) callAPI(null, getAPI_BASE() + "/status/" + getFileID(link) + ".json");
        final Map<String, Object> data = (Map<String, Object>) resp.get("data");
        link.setFinalFileName(data.get("name").toString());
        link.setVerifiedFileSize(((Number) data.get("size")).longValue());
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        handleFreeDownloads(link, null);
    }

    @SuppressWarnings({ "deprecation" })
    /** Handles free- and free account downloads. */
    public void handleFreeDownloads(final DownloadLink link, final Account account) throws Exception {
        if (this.getPluginConfig().getBooleanProperty(SETTING_ENABLE_API_FOR_FREE_AND_FREE_ACCOUNT_DOWNLOADS, defaultSETTING_ENABLE_API_FOR_FREE_AND_FREE_ACCOUNT_DOWNLOADS)) {
            doFreeAPI(account, link);
        } else {
            doFreeWebsite(account, link);
        }
    }

    private void doFreeAPI(final Account account, final DownloadLink link) throws Exception {
        if (checkShowFreeDialog(getHost())) {
            showFreeDialog(getHost());
        }
        final String storedDirecturl = link.getStringProperty(DIRECT_API);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Trying to re-use stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            final String fid = getFileID(link);
            String recaptchaV2Response = null;
            Map<String, Object> resp = (Map<String, Object>) callAPI(null, rewriteProtocol(BASE) + "/get/" + fid + ".json");
            Map<String, Object> data = (Map<String, Object>) resp.get("data");
            int statusCode = ((Number) resp.get("code")).intValue();
            final String reCaptchaKey = "6LdB1kcUAAAAAAVPepnD-6TEd4BXKzS7L4FZFkpO";
            final boolean captchaAlwaysRequired = true; // 2025-10-27
            if (statusCode == 203) {
                /* Pre download wait time */
                int i = 0;
                final boolean allowSolveCaptchaDuringWaitTime = true;
                do {
                    if (recaptchaV2Response != null) {
                        /**
                         * Nullify previous captcha response, that should be a rare case or even never happen. <br>
                         * If it happens, we need to ask the user to solve the captcha two times which we want to avoid.
                         */
                        recaptchaV2Response = null;
                        logger.warning("Wait loop is executed multiple times -> Nullify captcha response");
                    }
                    final String token = data.get("token").toString();
                    final int waitSeconds = ((Number) data.get("wait")).intValue();
                    long waitMillis = waitSeconds * 1000l;
                    if (i == 0 && allowSolveCaptchaDuringWaitTime) {
                        /* 2025-10-27: Special: Solve captcha "during" wait time. */
                        final long timeBefore = Time.systemIndependentCurrentJVMTimeMillis();
                        final CaptchaHelperHostPluginRecaptchaV2 rc2 = new CaptchaHelperHostPluginRecaptchaV2(this, br, reCaptchaKey);
                        if (waitMillis > rc2.getSolutionTimeout()) {
                            final long prePrePreDownloadWaitMillis = waitMillis - rc2.getSolutionTimeout();
                            logger.info("Waittime is higher than interactive captcha timeout --> Waiting a part of it before solving captcha to avoid captcha-token-timeout");
                            logger.info("Pre-pre download waittime seconds: " + (prePrePreDownloadWaitMillis / 1000));
                            this.sleep(prePrePreDownloadWaitMillis, link);
                        }
                        recaptchaV2Response = rc2.getToken();
                        final long passedMillis = Time.systemIndependentCurrentJVMTimeMillis() - timeBefore;
                        waitMillis = waitMillis - passedMillis;
                    }
                    /* Wait if any wait time is left */
                    if (waitMillis > 0) {
                        sleep(waitMillis, link);
                    }
                    resp = (Map<String, Object>) callAPI(null, rewriteProtocol(BASE) + "/get/" + fid + ".json?token=" + token);
                    statusCode = ((Number) resp.get("code")).intValue();
                } while (statusCode == 203 && ++i <= 2);
            }
            /* Check if captcha is required. Usually, captcha is required. */
            if (captchaAlwaysRequired && statusCode != 202) {
                logger.warning("Unexpected statusCode: " + statusCode + " and not 202");
            }
            if (statusCode == 202 || captchaAlwaysRequired) {
                final Browser brc = br.cloneBrowser();
                brc.setFollowRedirects(false);
                /* Solve captcha if it hasn't been solved before. */
                if (recaptchaV2Response == null) {
                    final CaptchaHelperHostPluginRecaptchaV2 rc2 = new CaptchaHelperHostPluginRecaptchaV2(this, br, reCaptchaKey);
                    recaptchaV2Response = rc2.getToken();
                }
                brc.postPage(rewriteProtocol(BASE) + "/get/" + fid + ".json", "g-recaptcha-response=" + Encoding.urlEncode(recaptchaV2Response) + "&hash=" + fid);
                dllink = brc.getRedirectLocation();
                if (dllink == null) {
                    this.checkErrorsAPI(account);
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                checkErrorsWebsite(account, true);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dl.setAllowFilenameFromURL(true);
            if (storedDirecturl == null) {
                link.setProperty(DIRECT_API, dl.getConnection().getURL().toExternalForm());
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(DIRECT_API);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        dl.startDownload();
    }

    @Deprecated
    private void doFreeWebsite(final Account account, final DownloadLink link) throws Exception {
        if (checkShowFreeDialog(getHost())) {
            showFreeDialog(getHost());
        }
        final String storedDirecturl = link.getStringProperty(DIRECT_WEB);
        String dllink = null;
        if (storedDirecturl != null) {
            logger.info("Trying to re-use stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            requestFileInformation(link);
            if (account != null) {
                this.loginWebsite(account, false);
            }
            br.getPage(getContentUrl(link));
            checkErrorsWebsite(account, false);
            Form continueForm = br.getFormbyKey("token");
            if (continueForm != null) {
                /* Captcha is not always required! */
                int wait = 60;
                final String waitSecondsStr = br.getRegex("id=\"time\"[^>]*>(\\d+)<").getMatch(0);
                if (waitSecondsStr != null) {
                    wait = Integer.parseInt(waitSecondsStr);
                }
                sleep(wait * 1000l, link);
                br.submitForm(continueForm);
            }
            int maxCaptchaTries = 4;
            int tries = 0;
            while (tries <= maxCaptchaTries) {
                logger.info(String.format("Captcha loop %d of %d", tries + 1, maxCaptchaTries + 1));
                continueForm = br.getFormbyKey("hash");
                if (continueForm == null) {
                    checkErrorsWebsite(account, false);
                    logger.info("Failed to find continueForm");
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final CaptchaHelperHostPluginRecaptchaV2 rc2 = new CaptchaHelperHostPluginRecaptchaV2(this, br);
                final String recaptchaV2Response = rc2.getToken();
                if (recaptchaV2Response == null) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
                continueForm.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                br.setFollowRedirects(false);
                br.submitForm(continueForm);
                dllink = br.getRedirectLocation();
                if (dllink != null) {
                    break;
                }
                tries++;
                continue;
            }
            if (dllink == null && this.br.containsHTML("data-sitekey")) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            } else if (dllink == null) {
                /* This should never happen! */
                checkErrorsWebsite(account, false);
                logger.warning("Failed to find final downloadurl");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                checkErrorsWebsite(account, true);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dl.setAllowFilenameFromURL(true);
            if (storedDirecturl == null) {
                link.setProperty(DIRECT_API, dl.getConnection().getURL().toExternalForm());
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(DIRECT_WEB);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        dl.startDownload();
    }

    private void errorNoFreeSlotsAvailable() throws PluginException {
        final int waitMinutes = this.getPluginConfig().getIntegerProperty(SETTING_WAIT_MINUTES_ON_ERROR_NO_FREE_SLOTS, defaultSETTING_WAIT_MINUTES_ON_ERROR_NO_FREE_SLOTS);
        throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "No free slots available, wait or buy premium!", waitMinutes * 60 * 1000l);
    }

    public Object loginAPI(final Account account, final boolean verifyLogins) throws Exception {
        synchronized (account) {
            br.getHeaders().put("Authorization", "Basic " + Encoding.Base64Encode(account.getUser() + ":" + account.getPass()));
            if (!verifyLogins) {
                return null;
            }
            return callAPI(account, getAPI_BASE() + "/profile.json");
        }
    }

    /* E.g. used for free account downloads */
    @Deprecated
    private void loginWebsite(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            /* Load cookies */
            prepBrowserWebsite(br);
            final Cookies cookies = account.loadCookies("");
            login_via_stored_cookies: if (cookies != null) {
                br.setCookies(cookies);
                if (!force) {
                    /* Do not validate cookies */
                    return;
                }
                br.getPage("https://" + this.getHost());
                if (this.isLoggedInWebsite(br)) {
                    logger.info("Successfully logged in via cookies");
                    account.saveCookies(this.br.getCookies(br.getHost()), "");
                    return;
                }
                logger.info("Cookie login failed");
                br.clearCookies(br.getHost());
            }
            logger.info("Performing full login");
            br.getPage("https://" + this.getHost() + "/login");
            final Form loginform = br.getFormbyKey("_username");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find loginform");
            }
            loginform.put("_username", Encoding.urlEncode(account.getUser()));
            loginform.put("_password", Encoding.urlEncode(account.getPass()));
            loginform.remove("_remember_me");
            loginform.put("_remember_me", "on");
            br.submitForm(loginform);
            if (!isLoggedInWebsite(br)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    private boolean isLoggedInWebsite(final Browser br) {
        if (br.containsHTML("/logout\"")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> entries = (Map<String, Object>) loginAPI(account, true);
        final Map<String, Object> data = (Map<String, Object>) entries.get("data");
        final AccountInfo ai = new AccountInfo();
        if (data.get("state").toString().equalsIgnoreCase("premium")) {
            account.setType(AccountType.PREMIUM);
            final Long trafficLeft = (Long) ReflectionUtils.cast(data.get("traffic"), Long.class);
            ai.setTrafficLeft(trafficLeft.longValue());
            final Long validUntil = (Long) ReflectionUtils.cast(data.get("until"), Long.class);
            ai.setValidUntil(validUntil.longValue() * 1000, br);
        } else {
            account.setType(AccountType.FREE);
            // ai.setUnlimitedTraffic();
            ai.setTrafficLeft(0);
            /* Display traffic left 0 but still allow account to be used for downloading. */
            ai.setSpecialTraffic(true);
        }
        final Object maxtrafficObject = data.get("maxtraffic");
        if (maxtrafficObject != null) {
            final Long maxtrafficValue = (Long) ReflectionUtils.cast(maxtrafficObject, Long.class);
            ai.setTrafficMax(maxtrafficValue.longValue());
        } else {
            /* fallback to hardcoded default */
            ai.setTrafficMax(SizeFormatter.getSize("125gb"));
        }
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (account.getType() == AccountType.FREE) {
            handleFreeDownloads(link, account);
        } else {
            requestFileInformation(link);
            this.loginAPI(account, false);
            br.setFollowRedirects(false);
            br.getPage(getAPI_BASE() + "/dl/" + getFileID(link) + ".json");
            final String dllink = br.getRedirectLocation();
            if (dllink == null) {
                this.checkErrorsAPI(account);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /* Important!! */
            br.getHeaders().put("Authorization", "");
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                this.checkErrorsWebsite(account, true);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dl.setAllowFilenameFromURL(true);
            this.dl.startDownload();
        }
    }

    private Object callAPI(final String url) throws Exception {
        return callAPI(null, url);
    }

    private Object callAPI(final Account account, final String url) throws Exception {
        return callAPI(account, br.createGetRequest(url));
    }

    /** Only use this if a json response is expected!! */
    private Object callAPI(final Account account, final Request req) throws Exception {
        final URLConnectionAdapter con = br.openRequestConnection(req);
        if (con.getResponseCode() == 401 && account != null) {
            con.disconnect();
            throw new AccountInvalidException();
        }
        br.followConnection();
        return checkErrorsAPI(account);
    }

    private Object checkErrorsAPI(final Account account) throws Exception {
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* Check for website errors and if that doesn't throw any exception, handle state as invalid API response. */
            checkErrorsWebsite(account, false);
            final String msg = "Invalid API response";
            final long wait = 1 * 60 * 1000;
            if (account == null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, wait);
            } else {
                throw new AccountUnavailableException(msg, wait);
            }
        }
        // TODO: Merge code- and "status" handling: error codes should be all we need here
        final Object code = entries.get("code");
        final String statusMessage = (String) entries.get("status");
        if (code != null) {
            if ("hour download limit reached".equals(code)) {
                final Map<String, Object> data = (Map<String, Object>) entries.get("data");
                int wait = 300;
                if (data != null) {
                    wait = ((Number) data.get("wait")).intValue();
                }
                // Waittime too small->Don't reconnect
                if (wait < 61) {
                    throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Wait before starting new downloads...", wait * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, wait * 1000l);
                }
            } else if ("user download slots filled".equals(code)) {
                errorNoFreeSlotsAvailable();
            } else if ("file captcha input needed".equals(code)) {
                // statusCode = 202;
            } else if ("file wait needed".equals(code)) {
                // statusCode = 203;
            }
        }
        if (code != null && (code instanceof Number || code.toString().matches("\\d+"))) {
            final int statusCode = Integer.parseInt(code.toString());
            if (statusCode >= 200 && statusCode < 300) {
                /* No error */
                /* 202 = file captcha input needed */
                /* 203 = file wait needed */
                return entries;
            }
            if (statusCode == STATUSCODE_APIDISABLED) {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, ERRORMESSAGE_APIDISABLEDTEXT, 2 * 60 * 60 * 1000l);
            } else if (statusCode == STATUSCODE_DOWNLOADTEMPORARILYDISABLED) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, ERRORMESSAGE_DOWNLOADTEMPORARILYDISABLEDTEXT, 2 * 60 * 60 * 1000l);
            } else if (statusCode == 504) {
                if (account == null || AccountType.FREE.equals(account.getType())) {
                    throw new AccountRequiredException(statusMessage);
                } else {
                    if (StringUtils.isEmpty(statusMessage)) {
                        throw new AccountUnavailableException("Traffic limit reached", 60 * 60 * 1000l);
                    } else {
                        throw new AccountUnavailableException(statusMessage, 60 * 60 * 1000l);
                    }
                }
            } else if (statusCode == 505) {
                /* {"code":"505","status":"file not found","data":[]} */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (statusCode == STATUSCODE_UNKNOWNERROR) {
                throw new PluginException(LinkStatus.ERROR_FATAL, ERRORMESSAGE_UNKNOWNERRORTEXT);
            } else {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Unknown API error");
            }
        }
        return entries;
    }

    private void checkErrorsWebsite(final Account account, final boolean afterDownload) throws PluginException {
        if (br.getHttpConnection().getResponseCode() == 401) {
            throw new AccountInvalidException();
        }
        final String errorcodeStr = new Regex(br.getURL(), "(?i).+/error/(\\d+)").getMatch(0);
        if (errorcodeStr != null) {
            final int errorcode = Integer.parseInt(errorcodeStr);
            if (errorcode == 415) {
                final int userConfiguredWaitMinutes = this.getPluginConfig().getIntegerProperty(SETTING_WAIT_MINUTES_ON_ERROR_CODE_415, defaultSETTING_WAIT_MINUTES_ON_ERROR_CODE_415);
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Error 415", userConfiguredWaitMinutes * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Error " + errorcodeStr, 15 * 60 * 1000l);
            }
        }
        if (br.containsHTML(">\\s*Maximale Verbindungen erreicht")) {
            errorNoFreeSlotsAvailable();
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (br.containsHTML(">\\s*Leider sind alle kostenlosen Download-Slots belegt|Im Moment sind leider alle Download-Slots für kostenlose Downloads belegt|Bitte versuche es später erneut oder behebe das Problem mit einem Premium")) {
            /* 2020-05-01 */
            errorNoFreeSlotsAvailable();
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (br.containsHTML(">\\s*Free Download Limit erreicht\\s*<")) {
            final String time = br.getRegex("<span id=\"time\">(\\d+)<").getMatch(0);
            if (account != null) {
                if (time != null) {
                    throw new AccountUnavailableException("Limit reached", (Integer.parseInt(time) + 60) * 1000l);
                } else {
                    throw new AccountUnavailableException("Limit reached", 60 * 60 * 1000l);
                }
            } else {
                if (time != null) {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Free limit reached", (Integer.parseInt(time) + 60) * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Free limit reached", 60 * 60 * 1000l);
                }
            }
        }
        if (afterDownload) {
            if (br.containsHTML("filer\\.net/register")) {
                errorNoFreeSlotsAvailable();
                /* This code should never be reached */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Final downloadurl did not lead to downloadable content", 3 * 60 * 1000l);
        }
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        link.removeProperty(DIRECT_WEB);
        link.removeProperty(DIRECT_API);
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), SETTING_ENABLE_API_FOR_FREE_AND_FREE_ACCOUNT_DOWNLOADS, "Enable API for free- and free account downloads?\r\nBy disabling this you will force JD to use the website instead.\r\nThis may trigger unexpected errors.").setDefaultValue(defaultSETTING_ENABLE_API_FOR_FREE_AND_FREE_ACCOUNT_DOWNLOADS));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), DISABLE_HTTPS, "Use HTTP instead of HTTPS").setDefaultValue(defaultSETTING_DISABLE_HTTPS));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SPINNER, getPluginConfig(), SETTING_WAIT_MINUTES_ON_ERROR_NO_FREE_SLOTS, "Wait minutes on error 'No free slots available'", 1, 600, 1).setDefaultValue(defaultSETTING_WAIT_MINUTES_ON_ERROR_NO_FREE_SLOTS));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SPINNER, getPluginConfig(), SETTING_WAIT_MINUTES_ON_ERROR_CODE_415, "Wait minutes on error 'Error 415'", 1, 600, 1).setDefaultValue(defaultSETTING_WAIT_MINUTES_ON_ERROR_CODE_415));
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        if (acc == null || !AccountType.PREMIUM.equals(acc.getType())) {
            /* no/free account, yes we can expect captcha */
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean hasAutoCaptcha() {
        return false;
    }
}