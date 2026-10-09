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
package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;
import org.jdownloader.gui.translate._GUI;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 53565 $", interfaceVersion = 3, names = {}, urls = {})
public class Fast2shareCom extends PluginForHost {
    /* API docs: https://fast2share.com/docs */
    private static final String API_BASE                   = "https://api.fast2share.com/v1";
    private static final String PROPERTY_ACCOUNT_TOKEN     = "fast2sharecom_token";
    private static final String PROPERTY_DIRECTURL         = "fast2sharecom_directurl";
    /** Boolean property, set to true for files that can only be downloaded with a premium account ("access":"premium"). */
    private static final String PROPERTY_PREMIUMONLY       = "fast2sharecom_premiumonly";

    public Fast2shareCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/");
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "fast2share.com", "f2s.im" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/f/([A-Za-z0-9\\-]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("Accept", "application/json");
        br.getHeaders().put("User-Agent", getClientIdentifier());
        br.setFollowRedirects(true);
        return br;
    }

    /** Identifier used both as User-Agent header and as "device_name" for the login API. */
    private String getClientIdentifier() {
        return "JDownloader Plugin Revision " + getVersion();
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        /* Only premium downloads are possible via the API. */
        return true;
    }

    public int getMaxChunks(final Account account) {
        if (account == null) {
            return 1;
        }
        switch (account.getType()) {
        case PREMIUM:
            /* 0 = maximum possible number of chunks. */
            return 0;
        case FREE:
        default:
            /* Free website downloads use a single connection. */
            return 1;
        }
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        return this.getFID(link);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        /*
         * If a valid account is available we can use the API. Otherwise we can only obtain file information from the public website.
         */
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        return requestFileInformation(link, account);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        final String fid = getFID(link);
        if (fid == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        this.setBrowserExclusive();
        if (account != null) {
            return requestFileInformationAPI(link, account);
        } else {
            return requestFileInformationWebsite(link);
        }
    }

    /** Availability check via API: GET /v1/files/{uuid} (works for public files and files owned by the logged in account). */
    private AvailableStatus requestFileInformationAPI(final DownloadLink link, final Account account) throws Exception {
        login(account, false);
        final Browser brc = br.cloneBrowser();
        brc.getPage(API_BASE + "/files/" + getFID(link));
        final Map<String, Object> entries = handleErrors(brc, account, link);
        /* These fields always exist for a valid file. */
        link.setFinalFileName(entries.get("name").toString());
        link.setVerifiedFileSize(((Number) entries.get("size")).longValue());
        link.setSha256Hash(entries.get("sha256").toString());
        if ("premium".equals(entries.get("access"))) {
            link.setProperty(PROPERTY_PREMIUMONLY, true);
        } else {
            link.removeProperty(PROPERTY_PREMIUMONLY);
        }
        if ("failed".equals(entries.get("status"))) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return AvailableStatus.TRUE;
    }

    /** Availability check via public website (used when no account is available). */
    private AvailableStatus requestFileInformationWebsite(final DownloadLink link) throws Exception {
        br.getPage("https://" + getHost() + "/f/" + getFID(link));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String filename = br.getRegex("<h1[^>]*class=\"fb-dl__name\"[^>]*>([^<]+)</h1>").getMatch(0);
        if (filename != null) {
            link.setName(Encoding.htmlDecode(filename).trim());
        } else {
            logger.warning("Failed to find filename");
        }
        final String filesize = br.getRegex("class=\"fb-dl__meta\"[^>]*>\\s*<span><b>([^<]+)</b>").getMatch(0);
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        if (br.containsHTML(">\\s*Downloading is a Premium feature")) {
            link.setProperty(PROPERTY_PREMIUMONLY, true);
        } else {
            link.removeProperty(PROPERTY_PREMIUMONLY);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        /* Downloads via the API always require an account; premium-only files additionally require a premium account. */
        if (link.getBooleanProperty(PROPERTY_PREMIUMONLY, false)) {
            throw new AccountRequiredException("This file can only be downloaded with a premium account");
        } else {
            throw new AccountRequiredException("A (free) account is required to download this file");
        }
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        /*
         * Premium accounts download via the API. Free accounts can only download non premium-only files through the website (with wait time
         * and captcha).
         */
        switch (account.getType()) {
        case PREMIUM:
            handleDownloadAPI(link, account);
            break;
        case FREE:
        default:
            handleFreeAccountDownloadWebsite(link, account);
            break;
        }
    }

    /** Premium download via API: fetches a fresh direct download URL and downloads it. */
    private void handleDownloadAPI(final DownloadLink link, final Account account) throws Exception {
        requestFileInformationAPI(link, account);
        String dllink = link.getStringProperty(PROPERTY_DIRECTURL);
        if (dllink != null) {
            logger.info("Re-using stored directurl: " + dllink);
        } else {
            dllink = fetchFreshDirecturl(link, account);
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, isResumeable(link, account), getMaxChunks(account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                checkDownloadErrors(link);
                super.handleConnectionErrors(br, br.getHttpConnection());
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } catch (final PluginException e) {
            link.removeProperty(PROPERTY_DIRECTURL);
            throw e;
        } catch (final IOException e) {
            /* Stored directurl may have expired (links expire after ~5 minutes). */
            link.removeProperty(PROPERTY_DIRECTURL);
            throw new PluginException(LinkStatus.ERROR_RETRY, "Directurl expired", e);
        }
        link.setProperty(PROPERTY_DIRECTURL, dllink);
        dl.startDownload();
    }

    /** Free download via website: website login, then solve the wait time + captcha of the free download form. */
    private void handleFreeAccountDownloadWebsite(final DownloadLink link, final Account account) throws Exception {
        loginWebsite(account, false);
        requestFileInformationWebsite(link);
        if (link.hasProperty(PROPERTY_PREMIUMONLY)) {
            /* This file cannot be downloaded with a free account. */
            throw new AccountRequiredException("This file can only be downloaded with a premium account");
        }
        final Form dlform = br.getFormbyProperty("id", "free-dl");
        if (dlform == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Read the pre-download wait time (seconds). */
        final String waitStr = br.getRegex("id=\"free-dl-btn\"[^>]*data-wait=\"(\\d+)\"").getMatch(0);
        final String sitekey = br.getRegex("class=\"cf-turnstile\"[^>]*data-sitekey=\"([^\"]+)\"").getMatch(0);
        if (sitekey == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final CaptchaHelperHostPluginCloudflareTurnstile ts = new CaptchaHelperHostPluginCloudflareTurnstile(this, br, sitekey);
        final long timeBeforeWait = Time.systemIndependentCurrentJVMTimeMillis();
        final long totalWaitMillis = waitStr != null ? Integer.parseInt(waitStr) * 1000l : 0;
        /*
         * If the wait time is higher than the captcha token's validity, wait a part of it before solving the captcha - otherwise the token
         * would expire before we can send it (see XFileSharingProBasic.waitBeforeInteractiveCaptcha).
         */
        final long captchaTimeoutMillis = ts.getSolutionTimeout();
        if (totalWaitMillis > captchaTimeoutMillis) {
            final long waitBeforeCaptchaMillis = totalWaitMillis - captchaTimeoutMillis;
            logger.info("Waittime is higher than captcha token validity -> Waiting a part of it before solving the captcha: " + (waitBeforeCaptchaMillis / 1000) + "s");
            this.sleep(waitBeforeCaptchaMillis, link);
        }
        /* Solve the Cloudflare Turnstile captcha. */
        final String token = ts.getToken();
        dlform.put("cf-turnstile-response", Encoding.urlEncode(token));
        /* Wait the remaining time (total wait time minus everything that already passed, including the captcha solving time). */
        final long remainingWaitMillis = totalWaitMillis - (Time.systemIndependentCurrentJVMTimeMillis() - timeBeforeWait);
        if (remainingWaitMillis > 0) {
            this.sleep(remainingWaitMillis, link);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dlform, isResumeable(link, account), getMaxChunks(account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            checkDownloadErrors(link);
            super.handleConnectionErrors(br, br.getHttpConnection());
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl.startDownload();
    }

    /** Requests a fresh direct download URL via GET /files/{uuid}/download. */
    private String fetchFreshDirecturl(final DownloadLink link, final Account account) throws Exception {
        login(account, false);
        final String fid = getFID(link);
        br.getPage(API_BASE + "/files/" + fid + "/download");
        /* Error when this is used with free account: {"error":"Direct download requires an active Premium subscription."} */
        if (br.getHttpConnection().getResponseCode() == 401) {
            /* Token expired -> force fresh login and retry once. */
            login(account, true);
            br.getPage(API_BASE + "/files/" + fid + "/download");
        }
        final Map<String, Object> entries = handleErrors(br, account, link);
        /* download_url always exists on a successful response. */
        return entries.get("download_url").toString();
    }

    private void checkDownloadErrors(final DownloadLink link) throws PluginException {
        final int code = br.getHttpConnection().getResponseCode();
        if (code == 403) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
        } else if (code == 404) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
        } else if (code == 429) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Rate limit reached", 5 * 60 * 1000l);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> user = login(account, true);
        /* These objects and fields always exist for a valid account. */
        final Map<String, Object> plan = (Map<String, Object>) user.get("plan");
        final Map<String, Object> usage = (Map<String, Object>) user.get("usage");
        final String planName = plan.get("name").toString();
        final AccountInfo ai = new AccountInfo();
        ai.setStatus("Plan: " + planName);
        /* Premium accounts download via the API; free accounts can download non premium-only files through the website. */
        final AccountType type = "free".equalsIgnoreCase(planName) ? AccountType.FREE : AccountType.PREMIUM;
        account.setType(type);
        switch (type) {
        case PREMIUM:
            account.setMaxSimultanDownloads(-1);
            setPremiumExpireDate(account, ai);
            break;
        case FREE:
        default:
            /* Free website downloads with wait time + captcha -> limit to a single simultaneous download. */
            account.setMaxSimultanDownloads(1);
            break;
        }
        /* Download quota = the account's (daily) download allowance. */
        final Map<String, Object> download_quota = (Map<String, Object>) user.get("download_quota");
        if (Boolean.TRUE.equals(download_quota.get("unlimited"))) {
            /* limit_bytes and remaining_bytes are null when unlimited. */
            ai.setUnlimitedTraffic();
        } else {
            final long remaining_bytes = ((Number) download_quota.get("remaining_bytes")).longValue();
            ai.setTrafficMax(((Number) download_quota.get("limit_bytes")).longValue());
            ai.setTrafficLeft(Math.max(0, remaining_bytes));
            if (remaining_bytes <= 0) {
                /* Download quota is used up -> account is unavailable until the quota resets (resets_at). */
                final long resetsAt = parseAPIDate(download_quota.get("resets_at").toString());
                final long waitMillis = resetsAt > System.currentTimeMillis() ? resetsAt - System.currentTimeMillis() : 5 * 60 * 1000l;
                throw new AccountUnavailableException(_GUI.T.account_error_no_traffic_left(), waitMillis);
            }
        }
        if (AccountType.FREE == type) {
            /* Free downloads happen through the website -> establish the website session now instead of on the first download attempt. */
            loginWebsite(account, true);
        }
        ai.setFilesNum(((Number) usage.get("file_count")).longValue());
        return ai;
    }

    /** Sets the premium plan expire date from GET /v1/account/settings (plan.expires_at), if it is not null. */
    @SuppressWarnings("unchecked")
    private void setPremiumExpireDate(final Account account, final AccountInfo ai) throws Exception {
        br.getPage(API_BASE + "/account/settings");
        final Map<String, Object> settings = handleErrors(br, account, null);
        final Map<String, Object> plan = (Map<String, Object>) settings.get("plan");
        final Object expiresAt = plan.get("expires_at");
        if (expiresAt == null) {
            /* No expire date -> e.g. lifetime plan. */
            return;
        }
        final long validUntil = parseAPIDate(expiresAt.toString());
        if (validUntil > 0) {
            ai.setValidUntil(validUntil, br);
        }
    }

    /**
     * Parses an API date/time string into epoch milliseconds, or -1 if it cannot be parsed. </br>
     * The real API responses deviate from the docs here: the docs show clean ISO 8601 (e.g. "2026-07-01T10:00:00+00:00") but some endpoints
     * actually return a raw PostgreSQL timestamptz (e.g. "2026-08-31 15:19:16.927653+00") with a space separator, microseconds and a 2-digit
     * timezone offset. Both variants are handled: the separator can be a space or 'T'; fractional seconds are dropped. Java 1.6's
     * SimpleDateFormat cannot parse microseconds (SSSSSS) or ISO timezones (X), so the offset is normalized to the RFC 822 form (e.g. "+00" /
     * "+00:00" -> "+0000") and parsed with the Z pattern.
     */
    private long parseAPIDate(final String dateStr) {
        final Regex dateRegex = new Regex(dateStr, "(\\d{4}-\\d{2}-\\d{2})[ T](\\d{2}:\\d{2}:\\d{2})(?:\\.\\d+)?([+-]\\d{2}(?::?\\d{2})?)?");
        final String date = dateRegex.getMatch(0);
        final String time = dateRegex.getMatch(1);
        if (date == null || time == null) {
            logger.warning("Failed to parse date: " + dateStr);
            return -1;
        }
        String offset = dateRegex.getMatch(2);
        if (offset != null) {
            offset = offset.replace(":", "");
            if (offset.length() == 3) {
                /* e.g. "+00" -> "+0000" */
                offset += "00";
            }
            return TimeFormatter.getMilliSeconds(date + " " + time + " " + offset, "yyyy-MM-dd HH:mm:ss Z", Locale.ENGLISH);
        } else {
            return TimeFormatter.getMilliSeconds(date + " " + time, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
        }
    }

    /**
     * Ensures a valid Bearer token is set on the browser and returns the token owner's account information (GET /v1/user). </br>
     * When validate is false and a stored token exists, that token is re-used without any request and null is returned. </br>
     * When validate is true the stored token is checked via GET /v1/user; if it is invalid a fresh login is performed.
     */
    private Map<String, Object> login(final Account account, final boolean validate) throws Exception {
        synchronized (account) {
            final String storedToken = account.getStringProperty(PROPERTY_ACCOUNT_TOKEN);
            if (storedToken != null) {
                setAuthHeader(storedToken);
                if (!validate) {
                    return null;
                }
                /* Validate stored token via GET /v1/user. */
                br.getPage(API_BASE + "/user");
                if (br.getHttpConnection().getResponseCode() == 200) {
                    logger.info("Token login successful");
                    return handleErrors(br, account, null);
                }
                logger.info("Token login failed -> performing full login");
                account.removeProperty(PROPERTY_ACCOUNT_TOKEN);
                br.getHeaders().remove("Authorization");
            }
            /* Full login via email + password. */
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("email", account.getUser());
            postdata.put("password", account.getPass());
            postdata.put("device_name", getClientIdentifier());
            br.postPageRaw(API_BASE + "/auth/login", JSonStorage.serializeToJson(postdata));
            Map<String, Object> entries = handleErrors(br, account, null);
            if (Boolean.TRUE.equals(entries.get("need_2fa"))) {
                /* Two-factor authentication required. */
                final String challenge = (String) entries.get("challenge");
                final String code = this.getTwoFACode(account, "[0-9]{6}");
                final Map<String, Object> twofa = new HashMap<String, Object>();
                twofa.put("challenge", challenge);
                twofa.put("code", code);
                br.postPageRaw(API_BASE + "/auth/2fa", JSonStorage.serializeToJson(twofa));
                entries = handleErrors(br, account, null);
            }
            /* A successful login response always contains the token. */
            final String token = entries.get("token").toString();
            account.setProperty(PROPERTY_ACCOUNT_TOKEN, token);
            setAuthHeader(token);
            /* Fetch account information so callers can rely on a non-null return after a fresh login. */
            br.getPage(API_BASE + "/user");
            return handleErrors(br, account, null);
        }
    }

    private void setAuthHeader(final String token) {
        br.getHeaders().put("Authorization", "Bearer " + token);
    }

    /**
     * Website login via the /login form (needed for free website downloads). </br>
     * This does not fetch any account information; it only establishes a valid website session (cookies).
     */
    private void loginWebsite(final Account account, final boolean validate) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("website");
            if (cookies != null) {
                br.setCookies(cookies);
                if (!validate) {
                    return;
                }
                br.getPage("https://" + getHost() + "/");
                if (isLoggedinWebsite(br)) {
                    logger.info("Website cookie login successful");
                    account.saveCookies(br.getCookies(br.getHost()), "website");
                    return;
                }
                logger.info("Website cookie login failed -> performing full website login");
                br.clearCookies(null);
            }
            br.getPage("https://" + getHost() + "/login");
            final Form loginform = br.getFormbyActionRegex(".*/login");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            loginform.put("email", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            /* Login form is protected by a Cloudflare Turnstile captcha. */
            final String sitekey = br.getRegex("class=\"cf-turnstile\"[^>]*data-sitekey=\"([^\"]+)\"").getMatch(0);
            if (sitekey != null) {
                final CaptchaHelperHostPluginCloudflareTurnstile ts = new CaptchaHelperHostPluginCloudflareTurnstile(this, br, sitekey);
                final String token = ts.getToken();
                loginform.put("cf-turnstile-response", Encoding.urlEncode(token));
            }
            br.submitForm(loginform);
            if (!isLoggedinWebsite(br)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "website");
        }
    }

    private boolean isLoggedinWebsite(final Browser br) {
        return br.containsHTML("/logout\"");
    }

    /** Invalidates the account's token server-side. Called once the account gets removed. */
    private void logout(final Account account) {
        synchronized (account) {
            final String token = account.getStringProperty(PROPERTY_ACCOUNT_TOKEN, null);
            if (token == null) {
                /* No stored token -> We cannot logout */
                return;
            }
            try {
                if (br == null) {
                    /* This plugin instance may already have gone through clean(). */
                    setBrowser(createNewBrowserInstance());
                }
                setAuthHeader(token);
                br.postPageRaw(API_BASE + "/auth/logout", "");
                logger.info("Logout successful");
                account.removeProperty(PROPERTY_ACCOUNT_TOKEN);
            } catch (final Exception e) {
                logger.log(e);
                logger.warning("Logout failed");
            }
        }
    }

    @Override
    public void onAccountRemove(final Account account) {
        logout(account);
    }

    /** Parses the JSON response and maps API/HTTP errors to the according exceptions. */
    private Map<String, Object> handleErrors(final Browser brc, final Account account, final DownloadLink link) throws Exception {
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException e) {
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid API response", 1 * 60 * 1000l);
            } else {
                throw new AccountUnavailableException("Invalid API response", 1 * 60 * 1000l);
            }
        }
        final int code = brc.getHttpConnection().getResponseCode();
        if (code < 400) {
            return entries;
        }
        final String error = (String) entries.get("error");
        final String msg = !StringUtils.isEmpty(error) ? error : "HTTP error " + code;
        switch (code) {
        case 401:
            /* Invalid or revoked token. */
            if (account != null) {
                account.removeProperty(PROPERTY_ACCOUNT_TOKEN);
            }
            throw new AccountInvalidException(msg);
        case 403:
            /* Quota/plan limit e.g. premium required. */
            if (link != null) {
                throw new AccountRequiredException(msg);
            } else {
                throw new AccountUnavailableException(msg, 5 * 60 * 1000l);
            }
        case 404:
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        case 429:
            /* Rate limit. */
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, 5 * 60 * 1000l);
            } else {
                throw new AccountUnavailableException(msg, 5 * 60 * 1000l);
            }
        default:
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, 5 * 60 * 1000l);
            } else {
                throw new AccountUnavailableException(msg, 5 * 60 * 1000l);
            }
        }
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            /* Downloads always require an account (a free account is enough for non premium-only files). */
            return false;
        }
        if (link.getBooleanProperty(PROPERTY_PREMIUMONLY, false) && AccountType.PREMIUM != account.getType()) {
            /* Premium-only file but the given account is not a premium account. */
            return false;
        }
        return super.canHandle(link, account);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}
