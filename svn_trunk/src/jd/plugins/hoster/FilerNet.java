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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.http.BasicAuthentication;
import jd.http.Browser;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
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
import jd.plugins.PluginBrowser;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51971 $", interfaceVersion = 2, names = {}, urls = {})
public class FilerNet extends PluginForHost {
    private static final int     STATUSCODE_APIDISABLED                             = 400;
    private static final String  ERRORMESSAGE_APIDISABLEDTEXT                       = "API is disabled, please wait or use filer.net in your browser";
    private static final int     STATUSCODE_DOWNLOADTEMPORARILYDISABLED             = 500;
    private static final String  ERRORMESSAGE_DOWNLOADTEMPORARILYDISABLEDTEXT       = "Download temporarily disabled!";
    private static final int     STATUSCODE_UNKNOWNERROR                            = 599;
    private static final String  ERRORMESSAGE_UNKNOWNERRORTEXT                      = "Unknown file error";
    private static final String  DIRECT_WEB                                         = "directlinkWeb";
    private static final String  PREMIUM_ONLY                                       = "premium_only";
    private static final String  DIRECT_API                                         = "directlinkApi";
    /* Plugin settings */
    private static final String  DISABLE_HTTPS                                      = "DISABLE_HTTPS_2";
    private static final boolean defaultSETTING_DISABLE_HTTPS                       = false;
    private static final String  SETTING_WAIT_MINUTES_ON_ERROR_NO_FREE_SLOTS        = "WAIT_MINUTES_ON_NO_FREE_SLOTS";
    private static final int     defaultSETTING_WAIT_MINUTES_ON_ERROR_NO_FREE_SLOTS = 10;
    private static final String  SETTING_WAIT_MINUTES_ON_ERROR_CODE_415             = "SETTING_WAIT_MINUTES_ON_ERROR_CODE_415";
    private static final int     defaultSETTING_WAIT_MINUTES_ON_ERROR_CODE_415      = 5;
    /* API Docs: https://filer.net/api */
    public static final String   API_BASE                                           = "https://filer.net/api";
    public static final String   WEBSITE_BASE                                       = "https://filer.net";
    private static final float   RECAPTCHA_ENTERPRISE_MIN_SCORE                     = 0.5f;

    @SuppressWarnings("deprecation")
    public FilerNet(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/register");
        setConfigElements();
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
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
                final String host = request.getURL().getHost();
                if (!"api.filer.net".equalsIgnoreCase(host) && !"filer.net".equalsIgnoreCase(host)) {
                    // api and website always redirect to https
                    request.setURL(new URL(rewriteProtocol(request.getURL().toExternalForm())));
                }
                return super.openRequestConnection(request, followRedirects);
            }

            @Override
            public Browser createNewBrowserInstance() {
                return FilerNet.this.createNewBrowserInstance();
            }
        };
        br.setFollowRedirects(true);
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setAllowedResponseCodes(400, 502);
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

    public String getAPI_BASE() {
        // api always redirects to https
        return API_BASE;
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
        final int max_checkable_items_per_request = 100;
        try {
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    /* Check up to X items with one request */
                    if (index == urls.length || links.size() == max_checkable_items_per_request) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                final Set<String> hashes = new HashSet<String>();
                for (final DownloadLink link : links) {
                    hashes.add(this.getFileID(link));
                }
                final Request request;
                if (hashes.size() > 1) {
                    // due to a bug requires minimum 2 entries in hashes
                    final Map<String, Object> postData = new HashMap<String, Object>();
                    postData.put("hashes", hashes);
                    request = br.createJSonPostRequest(getAPI_BASE() + "/multi_status.json", postData);
                } else {
                    request = br.createGetRequest(getAPI_BASE() + "/multi_status/" + hashes.iterator().next() + ".json");
                }
                final Map<String, Object> entries = (Map<String, Object>) this.callAPI(null, request);
                final Map<String, Object> data = (Map<String, Object>) entries.get("data");
                for (final DownloadLink link : links) {
                    final String fid = this.getFileID(link);
                    final Map<String, Object> info = (Map<String, Object>) data.get(fid);
                    if (info == null) {
                        /* No info about item found in json response -> Assume that this item is offline. */
                        link.setAvailable(false);
                        continue;
                    }
                    setFileInformation(link, info);
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

    public boolean setFileInformation(final DownloadLink link, Map<String, Object> map) throws Exception {
        final String fid = this.getFileID(link);
        if (!StringUtils.equals(fid, (String) map.get("file_hash"))) {
            link.setAvailable(false);
            return false;
        }
        final String file_name = map.get("file_name").toString();
        link.setFinalFileName(file_name);
        final Number file_size = (Number) map.get("file_size");
        if (file_size != null) {
            link.setVerifiedFileSize(file_size.longValue());
        }
        final Boolean premium_only = (Boolean) map.get("premium_only");
        if (Boolean.TRUE.equals(premium_only)) {
            link.setProperty(PREMIUM_ONLY, Boolean.TRUE);
        } else {
            link.removeProperty(PREMIUM_ONLY);
        }
        link.setAvailable(true);
        return true;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Map<String, Object> resp = (Map<String, Object>) callAPI(null, getAPI_BASE() + "/status/" + getFileID(link) + ".json");
        final Map<String, Object> data = (Map<String, Object>) resp.get("data");
        if (setFileInformation(link, data)) {
            return AvailableStatus.TRUE;
        } else {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        handleFreeDownloads(link, null);
    }

    /** Handles free- and free account downloads. */
    public void handleFreeDownloads(final DownloadLink link, final Account account) throws Exception {
        doWebsiteApi(account, link);
    }

    private boolean isPremiumOnly(final DownloadLink link) {
        return link != null && Boolean.TRUE.equals(link.getBooleanProperty(PREMIUM_ONLY, Boolean.FALSE));
    }

    @Override
    public boolean canHandle(DownloadLink downloadLink, Account account) throws Exception {
        if (isPremiumOnly(downloadLink) && !AccountType.PREMIUM.is(account)) {
            return false;
        }
        return super.canHandle(downloadLink, account);
    }

    private void doWebsiteApi(final Account account, final DownloadLink link) throws Exception {
        if (checkShowFreeDialog(getHost())) {
            showFreeDialog(getHost());
        }
        final String directurlproperty = DIRECT_WEB;
        final String storedDirecturl = link.getStringProperty(directurlproperty);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Trying to re-use stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            final String fid = getFileID(link);
            Map<String, Object> data = (Map<String, Object>) callAPI(null, "https://" + getHost() + "/api/file/" + fid);
            if (Boolean.TRUE.equals(data.get("premiumOnly"))) {
                link.setProperty(PREMIUM_ONLY, Boolean.TRUE);
            } else {
                link.removeProperty(PREMIUM_ONLY);
            }
            if (!canHandle(link, account)) {
                throw new AccountRequiredException("File is only downloadable by premium users");
            }
            /* 2025-11-20: key was changed and type is now reCaptcha Enterprise. */
            final String reCaptchaKey = "6LfUvREsAAAAAHd79QK9HOfIAEVGqK4G4JxovEEn";
            final CaptchaHelperHostPluginRecaptchaV2 rc2 = new CaptchaHelperHostPluginRecaptchaV2(this, br, reCaptchaKey) {
                @Override
                protected Map<String, Object> getV3Action(String source) {
                    final Map<String, Object> ret = new HashMap<String, Object>();
                    ret.put("action", "download");
                    return ret;
                }

                @Override
                protected boolean isEnterprise() {
                    return true;
                }

                /* 2025-12-11: Without this, the following error may happen: {"error":"Score too low: 0.3 (minimum: 0.5)"} */
                @Override
                public double getMinScore() {
                    return RECAPTCHA_ENTERPRISE_MIN_SCORE;
                }
            };
            String recaptchaV2Response = null;
            String token = null;
            handle_pre_download_wait: {
                /* Pre download wait time */
                final boolean allowSolveCaptchaDuringWaitTime = true;
                if (recaptchaV2Response != null) {
                    /**
                     * Nullify previous captcha response, that should be a rare case or even never happen. <br>
                     * If it happens, we need to ask the user to solve the captcha two times which we want to avoid.
                     */
                    recaptchaV2Response = null;
                    logger.warning("Wait loop is executed multiple times -> Nullify captcha response");
                }
                /* The following API call starts the server side pre download wait time. */
                data = (Map<String, Object>) callAPI(null, "/api/file/request/" + fid);
                token = data.get("t").toString();
                final int waitSeconds = ((Number) data.get("wt")).intValue();
                long waitMillis = waitSeconds * 1000l;
                if (allowSolveCaptchaDuringWaitTime) {
                    /* 2025-10-27: Special: Solve captcha "during" wait time. */
                    final long timeBefore = Time.systemIndependentCurrentJVMTimeMillis();
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
            }
            handle_captcha_send: {
                /* Solve captcha if it hasn't been solved before. */
                if (recaptchaV2Response == null) {
                    recaptchaV2Response = rc2.getToken();
                }
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("recaptcha", recaptchaV2Response);
                postdata.put("ticket", token);
                br.postPageRaw("/api/file/download", JSonStorage.serializeToJson(postdata));
                data = (Map<String, Object>) this.checkErrorsAPI(account);
                dllink = data.get("downloadUrl").toString();
                if (StringUtils.isEmpty(dllink)) {
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
                link.setProperty(directurlproperty, dl.getConnection().getURL().toExternalForm());
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directurlproperty);
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

    private BasicAuthentication getAPIBasicAuthentication(final Account account) throws MalformedURLException {
        synchronized (account) {
            return new BasicAuthentication(new URL(getAPI_BASE()).getHost(), account.getUser(), account.getPass(), null);
        }
    }

    public Object loginAPI(final Account account, final boolean verifyLogins) throws Exception {
        synchronized (account) {
            br.addAuthentication(getAPIBasicAuthentication(account));
            if (!verifyLogins) {
                return null;
            }
            return callAPI(account, getAPI_BASE() + "/profile.json");
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> entries = (Map<String, Object>) loginAPI(account, true);
        final Map<String, Object> data = (Map<String, Object>) entries.get("data");
        final AccountInfo ai = new AccountInfo();
        final Number register_date = (Number) data.get("register_date");
        if (register_date != null) {
            ai.setCreateTime(register_date.longValue() * 1000);
        }
        if (Boolean.TRUE.equals(data.get("premium"))) {
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(10);
            final Long trafficUsed = (Long) ReflectionUtils.cast(data.get("traffic"), Long.class);
            final Long trafficLeft = (Long) ReflectionUtils.cast(data.get("traffic_left"), Long.class);
            if (trafficLeft != null) {
                ai.setTrafficLeft(trafficLeft.longValue());
                if (trafficUsed != null) {
                    ai.setTrafficMax(trafficLeft.longValue() + trafficUsed.longValue());
                }
            }
            final Long validUntil = (Long) ReflectionUtils.cast(data.get("until"), Long.class);
            if (validUntil != null) {
                ai.setValidUntil(validUntil.longValue() * 1000, br);
            }
        } else {
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(10);
            // ai.setUnlimitedTraffic();
            ai.setTrafficLeft(0);
            /* Display traffic left 0 but still allow account to be used for downloading. */
            ai.setSpecialTraffic(true);
            /*
             * 2025-11-03: Free accounts do not provide any benefits for downloads and because a separate browser login would be needed to
             * support them, I've disabled support for free accounts for now.
             */
            final boolean allowFreeAccounts = false;
            if (!allowFreeAccounts) {
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    throw new AccountInvalidException("Kostenlose Accounts werden nicht unterstützt, da sie keine Vorteile gegenüber dem Herunterladen ohne Account bieten, z. B. höhere Geschwindigkeit oder kein Captcha.");
                } else {
                    throw new AccountInvalidException("Free accounts are not supported because they do not offer any benefits compared to downloading without an account, e.g. higher speed or no captcha.");
                }
            }
        }
        if (ai.getTrafficMax() == -1) {
            /* fallback to hardcoded default */
            ai.setTrafficMax(134217728000l/* SizeFormatter.getSize("125gb") */);
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
            /* When doing this request, API will answer with json AND a redirect location (if no error happens). */
            br.getPage(getAPI_BASE() + "/dl/" + getFileID(link) + ".json");
            final String dllink = br.getRedirectLocation();
            if (dllink == null) {
                this.checkErrorsAPI(account);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /* If we don't remove the auth header, a redirect to error 410 will happen: https://filer.net/error/410 */
            br.removeAuthentication(this.getAPIBasicAuthentication(account));
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
    private Object callAPI(final Account account, Request req) throws Exception {
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
                throw new PluginException(LinkStatus.ERROR_FATAL, "Unknown API error " + statusCode);
            }
        }
        final Object errorO = entries.get("error");
        final String message = (String) entries.get("message");
        if (errorO instanceof Number) {
            final int error = ((Number) errorO).intValue();
            if (error == 502) {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, message, 5 * 60 * 1000);
            } else {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Unknown API error " + error);
            }
        } else if (errorO instanceof String) {
            /* e.g. {"error":"Action mismatch: expected 'download', got ''"} */
            final String error = errorO.toString();
            final Regex captchaErrorMinScoreFailure = new Regex(error, "Score too low: ([0-9]\\.[0-9]) \\(minimum: ([0-9]\\.[0-9])\\)");
            if (captchaErrorMinScoreFailure.patternFind()) {
                // final String receivedScoreStr = captchaErrorMinScoreFailure.getMatch(0);
                final String minScoreStr = captchaErrorMinScoreFailure.getMatch(1);
                if (minScoreStr.equals(Float.toString(RECAPTCHA_ENTERPRISE_MIN_SCORE))) {
                    /*
                     * Their minScore hasn't changed and still equals our current hardcoded minScore -> Captcha Solver delivered wrong
                     * result -> Treat as captcha error (wrong captcha)
                     */
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA, error);
                } else {
                    throw new PluginException(LinkStatus.ERROR_FATAL, error);
                }
            }
            throw new PluginException(LinkStatus.ERROR_FATAL, "Unknown API error " + error);
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
        link.removeProperty(PREMIUM_ONLY);
    }

    private void setConfigElements() {
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