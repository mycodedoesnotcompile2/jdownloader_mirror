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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;
import org.jdownloader.captcha.v2.challenge.hcaptcha.CaptchaHelperHostPluginHCaptcha;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

@HostPlugin(revision = "$Revision: 53419 $", interfaceVersion = 2, names = {}, urls = {})
public class FilerNet extends PluginForHost {
    private static final int    STATUSCODE_APIDISABLED                             = 400;
    private static final String ERRORMESSAGE_APIDISABLEDTEXT                       = "API is disabled, please wait or use filer.net in your browser";
    private static final int    STATUSCODE_DOWNLOADTEMPORARILYDISABLED             = 500;
    private static final String ERRORMESSAGE_DOWNLOADTEMPORARILYDISABLEDTEXT       = "Download temporarily disabled!";
    private static final int    STATUSCODE_UNKNOWNERROR                            = 599;
    private static final String ERRORMESSAGE_UNKNOWNERRORTEXT                      = "Unknown file error";
    private static final String DIRECT_WEB                                         = "directlinkWeb";
    private static final String PREMIUM_ONLY                                       = "premium_only";
    private static final String DIRECT_API                                         = "directlinkApi";
    /* Plugin settings */
    private static final String SETTING_WAIT_MINUTES_ON_ERROR_NO_FREE_SLOTS        = "WAIT_MINUTES_ON_NO_FREE_SLOTS";
    private static final int    defaultSETTING_WAIT_MINUTES_ON_ERROR_NO_FREE_SLOTS = 10;
    private static final String SETTING_WAIT_MINUTES_ON_ERROR_CODE_415             = "SETTING_WAIT_MINUTES_ON_ERROR_CODE_415";
    private static final int    defaultSETTING_WAIT_MINUTES_ON_ERROR_CODE_415      = 5;
    /* API Docs: https://filer.net/api */
    public static final String  API_BASE                                           = "https://filer.net/api";
    public static final String  WEBSITE_BASE                                       = "https://filer.net";
    private static final double RECAPTCHA_ENTERPRISE_MIN_SCORE                     = 0.5d;

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
        final Browser br = super.createNewBrowserInstance();
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
            // message "params/filehash must NOT have fewer than 16 characters"
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:app\\.php/)?(?:get|dl)/([a-z0-9]{16,})");
        }
        return ret.toArray(new String[0]);
    }

    public String getAPI_BASE() {
        // api always redirects to https
        return API_BASE;
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
                    /* Due to an API bug, the other request requires minimum 2 entries in hashes. */
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
        try {
            final Map<String, Object> resp = (Map<String, Object>) callAPI(null, getAPI_BASE() + "/status/" + getFileID(link) + ".json");
            final Map<String, Object> data = (Map<String, Object>) resp.get("data");
            if (setFileInformation(link, data)) {
                return AvailableStatus.TRUE;
            }
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } catch (AccountRequiredException e) {
            logger.log(e);
            if (checkLinks(new DownloadLink[] { link })) {
                return link.getAvailableStatus();
            }
            throw e;
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
            final CaptchaHosterHelperInterface captchaHelper;
            if (true) {
                /**
                 * 2025-12-18: switched to hcaptcha, see https://filer.net/assets/GetFileView-BAC_MvhC-1766086422362.js
                 */
                captchaHelper = new CaptchaHelperHostPluginHCaptcha(this, br, "45623a98-7b08-43ae-b758-c21c13024e2a");
            } else {
                /**
                 * 2025-11-20: key was changed and type is now reCaptcha Enterprise. <br>
                 * Source of key: https://filer.net/assets/GetFileView-D0EkjwK_-1766004219124.js <br>
                 * --> Search for sitekey
                 */
                captchaHelper = new CaptchaHelperHostPluginRecaptchaV2(this, br, "6LfUvREsAAAAAHd79QK9HOfIAEVGqK4G4JxovEEn") {
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
                    public Double getMinScore() {
                        return RECAPTCHA_ENTERPRISE_MIN_SCORE;
                    }
                };
            }
            String ticket = null;
            handle_pre_download_wait_and_captcha: {
                final String captchaResponseToken = captchaHelper.getToken();
                /* The following API call starts the server side pre download wait time. */
                data = (Map<String, Object>) callAPI(null, "/api/file/request/" + fid + "?hCaptchaToken=" + captchaResponseToken);
                ticket = data.get("t").toString();
                final int waitSeconds = ((Number) data.get("wt")).intValue();
                /* Wait if any wait time is left */
                if (waitSeconds > 0) {
                    // +10 seconds for time safety to avoid
                    // {"error":"Wait time not elapsed","remainingTime":3}
                    sleep(TimeUnit.SECONDS.toMillis(waitSeconds + 10), link);
                }
            }
            handle_ticket_send: {
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("ticket", ticket);
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

    protected void errorNoFreeSlotsAvailable() throws PluginException {
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
        ai.setTrafficRefill(true);
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
                /* 2026-08-25: Small workaround to display negative traffic in GUI. */
                throwExceptionOnNegativeTraffic(trafficLeft.longValue());
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

    /**
     * Throws an {@link AccountUnavailableException} with a human readable recovery ETA if the given traffic_left value is negative. Returns
     * normally otherwise. </br> Negative traffic recovers over time: 50 GB is added back every 24 hours (which equals 1 GB every 28.8
     * minutes).
     */
    private void throwExceptionOnNegativeTraffic(final long trafficLeft) throws AccountUnavailableException {
        if (trafficLeft >= 0) {
            return;
        }
        final SIZEUNIT maxSizeUnit = CFG_GUI.MAX_SIZE_UNIT.getValue();
        final long negativeTrafficBytes = Math.abs(trafficLeft);
        final String negativeTrafficFormatted = SIZEUNIT.formatValue(maxSizeUnit, negativeTrafficBytes);
        /* Calculate how long we need to wait until the account has positive traffic again. */
        final long bytesPerGB = 1024 * 1024 * 1024l;
        /* 50 GB every 24 hours equals 1 GB every 1728000 ms (28.8 minutes). */
        final long millisPerGB = TimeUnit.HOURS.toMillis(24) / 50;
        /*
         * Divide the 24h interval by 50 up front so the smaller millisPerGB value enters the multiplication instead of the large 24h value.
         * This keeps full byte precision while pushing the long overflow far out of any realistic traffic range.
         */
        final long recoveryMillis = negativeTrafficBytes * millisPerGB / bytesPerGB;
        /* Build a human readable ETA (hours and minutes) from the full recovery time. */
        final long etaTotalMinutes = recoveryMillis / TimeUnit.MINUTES.toMillis(1);
        final long etaHours = etaTotalMinutes / 60;
        final long etaMinutes = etaTotalMinutes % 60;
        /*
         * Do not block the account for the full recovery time: wait at most 30 minutes (and at least 5 minutes) before re-checking, but
         * display the full ETA in the error message.
         */
        final long waitMillis = Math.max(TimeUnit.MINUTES.toMillis(5), Math.min(recoveryMillis, TimeUnit.MINUTES.toMillis(30)));
        throw new AccountUnavailableException("Kein Traffic übrig: -" + negativeTrafficFormatted + " | eta " + etaHours + "h:" + etaMinutes + "m", waitMillis);
    }

    /**
     * If the account is in the negative traffic range, throws an {@link AccountUnavailableException} with the same recovery ETA message as
     * {@link #fetchAccountInfo(Account)}. Returns normally if there is no {@link AccountInfo} yet or the traffic is not negative. </br> The
     * remaining traffic is read from the existing {@link AccountInfo} (note: AccountInfo may currently clamp negative values to zero, in
     * which case this check simply does nothing).
     */
    private void checkNegativeTrafficAndThrow(final Account account) throws AccountUnavailableException {
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            return;
        }
        throwExceptionOnNegativeTraffic(ai.getTrafficLeft());
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (account.getType() == AccountType.FREE) {
            handleFreeDownloads(link, account);
        } else {
            this.loginAPI(account, false);
            requestFileInformation(link);
            br.setFollowRedirects(false);
            /* When doing this request, API will answer with json AND a redirect location (if no error happens). */
            callAPI(account, br.createGetRequest(getAPI_BASE() + "/dl/" + getFileID(link) + ".json"));
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

    private Object callAPI(final Account account, final String url) throws Exception {
        return callAPI(account, br.createGetRequest(url));
    }

    /** Only use this if a json response is expected!! */
    private Object callAPI(final Account account, Request req) throws Exception {
        final URLConnectionAdapter con = br.openRequestConnection(req);
        try {
            br.followConnection(true);
            final Object ret = checkErrorsAPI(account);
            return ret;
        } finally {
            con.disconnect();
        }
    }

    // see https://filer.net/api/
    private enum ApiError {
        none {

            @Override
            protected boolean matches(Map<String, Object> entries) {
                return false;
            }

            @Override
            public Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                return entries;
            }

        },
        success {
            // {"code":200,"status":"success","data":{}}
            @Override
            protected boolean matches(Map<String, Object> entries) {
                if (super.matches(entries)) {
                    return true;
                }
                return "200".equals(String.valueOf(entries.get("code")));
            }

            @Override
            public Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                return entries;
            }

        },
        file_captcha_input_needed {
            // {"code":202,"status":"file captcha input needed","data":{}}
            @Override
            protected boolean matches(Map<String, Object> entries) {
                if (super.matches(entries)) {
                    return true;
                }
                return "202".equals(String.valueOf(entries.get("code")));
            }

            @Override
            public Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                return entries;
            }
        },
        file_wait_needed {
            // {"code":203,"status":"file wait needed","data":{}}
            @Override
            protected boolean matches(Map<String, Object> entries) {
                if (super.matches(entries)) {
                    return true;
                }
                return "203".equals(String.valueOf(entries.get("code")));
            }

            @Override
            public Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                return entries;
            }
        },
        redirect {
            // {"code":302,"status":"redirect","data":{}}
            @Override
            protected boolean matches(Map<String, Object> entries) {
                if (super.matches(entries)) {
                    return true;
                }
                return "302".equals(String.valueOf(entries.get("code")));
            }

            @Override
            public Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                return entries;
            }

        },
        authentication_required {
            // {"code":401,"status":"authentication required","data":{}}

            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "authentication required");
            }

        },
        invalid_credentials {
            // {"code":401,"status":"invalid credentials","data":{}}

            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                if (account != null) {
                    throw new AccountInvalidException("invalid credentials");
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected API error " + entries);
            }

        },
        authentication_failed {
            // {"code":401,"status":"authentication failed","data":{}}

            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                if (account != null) {
                    throw new AccountUnavailableException("authentication failed", TimeUnit.MINUTES.toMillis(30));
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected API error " + entries);
            }

        },
        account_suspended {
            // {"code":403,"status":"account suspended","data":{}}

            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                if (account != null) {
                    throw new AccountInvalidException("Account suspended, please contact filer.net support!");
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected API error " + entries);
            }
        },
        account_breach_locked {
            // {"code":403,"status":"account_breach_locked","data":{}}

            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                if (account != null) {
                    throw new AccountInvalidException("Security lock due to breached credentials, please reset password.");
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected API error " + entries);
            }
        },
        access_denied {
            // {"code":403,"status":"access denied","data":{}}

            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected API error " + entries);
            }
        },
        premium_required {
            // {"code":403,"status":"premium required","data":{}}
            // {"code":503,"status":"premium required","data":{}}

            @Override
            protected boolean matches(Map<String, Object> entries) {
                if (super.matches(entries)) {
                    return true;
                }
                return "503".equals(String.valueOf(entries.get("code")));
            }

            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                throw new AccountRequiredException("Premium access is required.");
            }
        },
        password_required {
            // {"code":403,"status":"password required","data":{}}
            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected API error " + entries);
            }

        },
        folder_password_required {
            // {"code":403,"status":"folder password required","data":{}}
            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected API error " + entries);
            }

        },
        download_not_allowed {
            // {"code":403,"status":"download not allowed","data":{}}
            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                if (account != null) {
                    throw new AccountUnavailableException("Download currently not possible", TimeUnit.MINUTES.toMillis(15));
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected API error " + entries);
            }

        },
        concurrent_download_limit_reached {
            // {"code":429,"status":"concurrent download limit reached","data":{"currentSlots":100,"maxSlots":100}}
            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                if (account != null) {
                    throw new AccountUnavailableException("Parallel download slots are full.", TimeUnit.MINUTES.toMillis(15));
                }
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Parallel download slots are full.", TimeUnit.MINUTES.toMillis(15));
            }

        },
        no_download_server_available {
            // {"code":503,"no download server available","data":{}}
            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Temporary delivery unavailability", TimeUnit.MINUTES.toMillis(15));
            }

        },
        invalid_hashes_format {
            // {"code":400,"invalid hashes format","data":{}}
            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected API error " + entries);
            }

        },
        file_not_found {
            /* {"code":"505","status":"file not found","data":[]} */
            @Override
            protected boolean matches(Map<String, Object> entries) {
                if (super.matches(entries)) {
                    return true;
                }
                return "505".equals(String.valueOf(entries.get("code")));
            }

            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "file not found");
            }

        },
        folder_not_found {
            /* {"code":"506","status":"folder not found","data":[]} */
            @Override
            protected boolean matches(Map<String, Object> entries) {
                if (super.matches(entries)) {
                    return true;
                }
                return "506".equals(String.valueOf(entries.get("code")));
            }

            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "folder_not_found");
            }

        },
        hour_download_limit_reached {
            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                final Map<String, Object> data = (Map<String, Object>) entries.get("data");
                final Number wait;
                if (data != null && data.get("wait") instanceof Number) {
                    wait = ((Number) data.get("wait"));
                } else {
                    wait = 300;
                }
                // Waittime too small->Don't reconnect
                if (wait.intValue() < 61) {
                    throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Wait before starting new downloads...", wait.intValue() * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, wait.intValue() * 1000l);
                }
            }

        },
        user_download_slots_filled {

            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                plugin.errorNoFreeSlotsAvailable();
                return entries;
            }
        },
        error {
            /* {"code":"500","status":"error","data":[]} */
            @Override
            protected boolean matches(Map<String, Object> entries) {
                if (super.matches(entries)) {
                    return true;
                }
                return "500".equals(String.valueOf(entries.get("code")));
            }

            @Override
            protected Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, Account account) throws Exception {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }

        };

        protected boolean matches(Map<String, Object> entries) {
            final String status = StringUtils.valueOfOrNull(entries.get("status"));
            if (status == null) {
                return false;
            }
            return StringUtils.equals(name(), status) || StringUtils.equals(name(), status.replace(" ", "_"));
        };

        protected static ApiError parseError(FilerNet plugin, Map<String, Object> entries, final Account account) throws Exception {
            for (final ApiError error : ApiError.values()) {
                if (error.matches(entries)) {
                    return error;
                }
            }
            return null;
        }

        protected abstract Map<String, Object> handle(FilerNet plugin, Map<String, Object> entries, final Account account) throws Exception;
    }

    private Object checkErrorsAPI(final Account account) throws Exception {
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* Check for website errors and if that doesn't throw any exception, handle state as invalid API response. */
            checkErrorsWebsite(account, false);
            final String msg = "Invalid API response";
            if (account == null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, TimeUnit.MINUTES.toMillis(1));
            } else {
                throw new AccountUnavailableException(msg, TimeUnit.MINUTES.toMillis(1));
            }
        }
        final ApiError apiError = ApiError.parseError(this, entries, account);
        if (apiError != null) {
            return apiError.handle(this, entries, account);
        }
        // TODO: Merge code- and "status" handling: error codes should be all we need here
        final Object codeObject = entries.get("code");
        final String statusObject = (String) entries.get("status");
        final Number code;
        if (codeObject == null) {
            code = null;
        } else if (codeObject instanceof Number) {
            code = (Number) codeObject;
        } else if (codeObject.toString().matches("\\d+")) {
            code = Integer.parseInt(codeObject.toString());
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String status;
        if (codeObject != null && !codeObject.toString().matches("\\d+") && !StringUtils.isNotEmpty(codeObject.toString())) {
            // still required? or can be removed?
            status = codeObject.toString();
        } else {
            status = statusObject;
        }
        if (status != null) {
            // check/update ApiError
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected API error " + entries);
        }
        if (code != null) {
            final int statusCode = code.intValue();
            switch (statusCode) {
            case STATUSCODE_APIDISABLED:
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, ERRORMESSAGE_APIDISABLEDTEXT, TimeUnit.HOURS.toMillis(2));
            case 504:
                /*
                 * Error 504 = traffic limit reached. If the account is in the negative traffic range, display the same detailed message
                 * (with recovery ETA) as fetchAccountInfo does instead of the generic traffic limit message.
                 */
                checkNegativeTrafficAndThrow(account);
                if (StringUtils.isEmpty(status)) {
                    throw new AccountUnavailableException("Error 504: Traffic limit reached", TimeUnit.MINUTES.toMillis(5));
                } else {
                    throw new AccountUnavailableException(status, TimeUnit.MINUTES.toMillis(5));
                }
            case STATUSCODE_UNKNOWNERROR:
                throw new PluginException(LinkStatus.ERROR_FATAL, ERRORMESSAGE_UNKNOWNERRORTEXT);
            default:
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected API error " + entries);
            }
        }
        final Object errorO = entries.get("error");
        final String message = (String) entries.get("message");
        if (errorO instanceof Number) { /*
                                         * Error codes and messages can be extracted from here:
                                         * https://filer.net/assets/ErrorPage-Br2HzfRN-1765742941422.js
                                         */
            final int error = ((Number) errorO).intValue();
            switch (error) {
            case 404:
                throw new PluginException(LinkStatus.ERROR_FATAL, getErrorMessage(null, message));
            case 500:
                /* SERVICE_TEMPORARILY_UNAVAILABLE */
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, getErrorMessage(null, message));
            case 501:
                /* SERVICE_BUSY */
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, getErrorMessage(null, message));
            case 502:
                /* {"error":502,"message":"CONCURRENT_DOWNLOAD_LIMIT"} */
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, getErrorMessage(null, message), TimeUnit.MINUTES.toMillis(3));
            case 503:
                /* HOURLY_DOWNLOAD_LIMIT */
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, getErrorMessage(null, message), TimeUnit.HOURS.toMillis(1));
            case 504:
                /* BANDWIDTH_QUOTA_EXCEEDED */
                break;
            case 505:
                /* PREMIUM_REQUIRED */
                throw new AccountRequiredException(getErrorMessage(null, message));
            default:
                throw new PluginException(LinkStatus.ERROR_FATAL, "Unknown API error code " + entries);
            }
        } else if (errorO instanceof String) {
            /* e.g. {"error":"Action mismatch: expected 'download', got ''"} */
            final String error = errorO.toString();
            final Regex captchaErrorMinScoreFailure = new Regex(error, "Score too low: ([0-9]\\.[0-9]) \\(minimum: ([0-9]\\.[0-9])\\)");
            if (captchaErrorMinScoreFailure.patternFind()) {
                // final String receivedScoreStr = captchaErrorMinScoreFailure.getMatch(0);
                final String minScoreStr = captchaErrorMinScoreFailure.getMatch(1);
                if (minScoreStr.equals(Double.toString(RECAPTCHA_ENTERPRISE_MIN_SCORE))) {
                    /*
                     * Their minScore hasn't changed and still equals our current hardcoded minScore -> Captcha Solver delivered wrong
                     * result -> Treat as captcha error (wrong captcha)
                     */
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA, error);
                } else {
                    throw new PluginException(LinkStatus.ERROR_FATAL, error);
                }
            } else if (error.equalsIgnoreCase("Invalid captcha")) {
                /* {"error":"Invalid captcha"} */
                // Along with http response 400
                throw new PluginException(LinkStatus.ERROR_CAPTCHA, error);
            } else if (error.equalsIgnoreCase("HOURLY_DOWNLOAD_LIMIT")) {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, getErrorMessage(error, message), TimeUnit.HOURS.toMillis(1));
            } else if (error.equalsIgnoreCase("TICKET_LIMIT_REACHED")) {
                /**
                 * HTTP/1.1 429 Too Many Requests {"error":"TICKET_LIMIT_REACHED","message":"You already have an active download ticket.
                 * Please use or wait for it to expire." ,"activeTickets":1,"maxTickets":1} <br>
                 * See: https://board.jdownloader.org/showthread.php?t=98395
                 */
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, getErrorMessage(error, message), TimeUnit.HOURS.toMillis(1));
            } else if (error.equalsIgnoreCase("Wait time not elapsed")) {
                /* {"error":"Wait time not elapsed","remainingTime":3} */
                // should not happen
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            throw new PluginException(LinkStatus.ERROR_FATAL, "Unknown API error code " + entries);
        }
        return entries;
    }

    private static final Map<String, String> ERROR_MESSAGES = new HashMap<String, String>();
    static {
        ERROR_MESSAGES.put("CONCURRENT_DOWNLOAD_LIMIT", "Concurrent download limit exceeded.");
        ERROR_MESSAGES.put("HOURLY_DOWNLOAD_LIMIT", "Hourly download limit reached.");
        ERROR_MESSAGES.put("BANDWIDTH_QUOTA_EXCEEDED", "Bandwidth quota exceeded.");
        ERROR_MESSAGES.put("SERVICE_TEMPORARILY_UNAVAILABLE", "Service temporarily unavailable");
        ERROR_MESSAGES.put("SERVICE_BUSY", "Service busy");
        ERROR_MESSAGES.put("PREMIUM_REQUIRED", "Premium account required to download this file");
    }

    public static String getErrorMessage(String errorKey, String defaultMessage) {
        if (errorKey == null || StringUtils.isEmpty(errorKey)) {
            return defaultMessage;
        }
        final String message = ERROR_MESSAGES.get(errorKey);
        return message != null ? message : defaultMessage;
    }

    private void checkErrorsWebsite(final Account account, final boolean afterDownload) throws PluginException {
        if (br.getHttpConnection().getResponseCode() == 401) {
            throw new AccountInvalidException();
        }
        if (StringUtils.containsIgnoreCase(br.getHttpConnection().getContentType(), "application/json")) {
            final Map<String, Object> error = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if ("ip_mismatch".equals(error.get("error"))) {
                if (account != null) {
                    throw new AccountUnavailableException("IP mismatch", TimeUnit.MINUTES.toMillis(5));
                }
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "IP mismatch", TimeUnit.MINUTES.toMillis(5));
            }
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