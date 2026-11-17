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

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Hash;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 54009 $", interfaceVersion = 3, names = {}, urls = {})
public class HanimeTv extends PluginForHost {
    /** Origin the website uses for the signature (window.top.location.origin). */
    public static final String  ORIGIN            = "https://hanime.tv";
    private static final String API_BASE          = "https://auth.hanime.tv";
    private static final String URL_CSRF          = "https://ct.hanime.tv/csrf-token";
    private static final String SIGNATURE_SALT_1  = ",Xkdi29,";
    private static final String SIGNATURE_SALT_2  = ",mn2,";
    private static final String SIGNATURE_VERSION = "web2";

    public HanimeTv(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(ORIGIN + "/premium");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms-of-use";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "hanime.tv" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/videos/hentai/([a-z0-9\\-]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
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
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(fid.replace("-", " ").trim() + "_1080p.mp4");
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.getURL().contains(fid)) {
            /* E.g. redirect to "https://hanime.tv/404" */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        /*
         * Since the 2026-11 website rebuild, video URLs are handled by HanimeTvCrawler (signed handshake -> encrypted HLS playlists which
         * are handed over to GenericM3u8). This hoster only validates the item and has no download logic of its own.
         */
        throw new PluginException(LinkStatus.ERROR_FATAL, "Direct downloads are not supported, use the crawler (HLS streams)");
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleFree(link);
    }

    /**
     * Calculates the x-signature header: hex(SHA-256(t + ",Xkdi29," + origin + ",mn2," + t)) with t = unix time in seconds (= x-time
     * header). Reverse engineered from the WASM module of the website.
     */
    public static String generateXSignature(final long timestampUnix, final String origin) {
        return Hash.getSHA256(timestampUnix + SIGNATURE_SALT_1 + origin + SIGNATURE_SALT_2 + timestampUnix);
    }

    /** Fetches a CSRF token. Also sets the cookie "htv_csrf_proof" in the given browser. */
    public static String fetchCsrfToken(final Browser br) throws Exception {
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Origin", ORIGIN);
        brc.getHeaders().put("Referer", ORIGIN + "/");
        brc.getHeaders().put("Accept", "application/json");
        brc.getPage(URL_CSRF);
        final Map<String, Object> entries = JSonStorage.restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        return entries.get("csrf_token").toString();
    }

    /**
     * Sets all headers needed for a request to the authed API (auth.hanime.tv). <br>
     * Returns the unix timestamp (seconds) used for x-time.
     */
    public static long setSignedApiHeaders(final Browser br, final String csrfToken) {
        final long timestampUnix = System.currentTimeMillis() / 1000;
        br.getHeaders().put("Origin", ORIGIN);
        br.getHeaders().put("Referer", ORIGIN + "/");
        br.getHeaders().put("Accept", "application/json");
        br.getHeaders().put("Content-Type", "application/json");
        br.getHeaders().put("x-signature-version", SIGNATURE_VERSION);
        br.getHeaders().put("x-signature", generateXSignature(timestampUnix, ORIGIN));
        br.getHeaders().put("x-time", Long.toString(timestampUnix));
        br.getHeaders().put("x-csrf-token", csrfToken);
        return timestampUnix;
    }

    /**
     * Logs in and makes sure the given browser contains the session cookies afterwards. <br>
     * The username of the account is the email address. <br>
     * Returns the session info (contains "user" and "session_expire_at_unix") or null if the stored cookies were used without validating
     * them (force == false).
     */
    public Map<String, Object> login(final Browser br, final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                br.setCookies(cookies);
                if (!force) {
                    /* Do not validate cookies. */
                    return null;
                }
                logger.info("Attempting cookie login");
                final Map<String, Object> session = fetchSession(br);
                if (session != null) {
                    logger.info("Cookie login successful");
                    account.saveCookies(collectCookies(br), "");
                    return session;
                }
                logger.info("Cookie login failed");
                br.clearCookies(null);
            }
            logger.info("Performing full login");
            br.getPage(ORIGIN + "/login");
            final String turnstileSiteKey = br.getRegex("\"turnstile_public_site_key\":\"([^\"]+)\"").getMatch(0);
            if (turnstileSiteKey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find Cloudflare Turnstile sitekey");
            }
            final String turnstileToken = new CaptchaHelperHostPluginCloudflareTurnstile(this, br, turnstileSiteKey).getToken();
            final Browser brc = br.cloneBrowser();
            final int maxLoginAttempts = 3;
            Map<String, Object> session = null;
            for (int attempt = 1; attempt <= maxLoginAttempts; attempt++) {
                final String csrfToken = fetchCsrfToken(brc);
                setSignedApiHeaders(brc, csrfToken);
                final Map<String, Object> postdata = new LinkedHashMap<String, Object>();
                postdata.put("email", account.getUser());
                postdata.put("password", account.getPass());
                postdata.put("csrf_token", csrfToken);
                postdata.put("turnstile_token", turnstileToken);
                brc.postPageRaw(API_BASE + "/api/v11/login", JSonStorage.serializeToJson(postdata));
                if (brc.getHttpConnection().getResponseCode() == 200) {
                    /* The login response contains the same session info as the keep-alive response. */
                    session = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                    break;
                }
                String errorId = null;
                String message = null;
                if (StringUtils.containsIgnoreCase(brc.getHttpConnection().getContentType(), "json")) {
                    final Map<String, Object> error = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                    errorId = (String) error.get("error_id");
                    message = (String) error.get("message");
                }
                if (errorId != null && errorId.startsWith("CSRF_ERROR_") && attempt < maxLoginAttempts) {
                    logger.info("CSRF token rejected, retrying: " + errorId + " | Attempt " + attempt + "/" + maxLoginAttempts);
                    continue;
                }
                if (brc.getHttpConnection().getResponseCode() == 429) {
                    throw new AccountUnavailableException("Too many login attempts", 10 * 60 * 1000l);
                }
                logger.info("Login failed: HTTP " + brc.getHttpConnection().getResponseCode() + " | " + errorId + " | " + message);
                throw new AccountInvalidException(message);
            }
            if (session == null) {
                /* Should never happen: The last attempt always ends with an exception or success. */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Login failed: Max attempts reached");
            }
            logger.info("Login successful");
            final Cookies sessionCookies = collectCookies(brc);
            br.setCookies(sessionCookies);
            account.saveCookies(sessionCookies, "");
            return session;
        }
    }

    private static Cookies collectCookies(final Browser br) {
        final Cookies ret = new Cookies();
        ret.add(br.getCookies("hanime.tv"));
        ret.add(br.getCookies("auth.hanime.tv"));
        ret.add(br.getCookies("ct.hanime.tv"));
        return ret;
    }

    /** Returns the session info (contains "user") or null if the session is invalid / the user is not logged in. */
    private Map<String, Object> fetchSession(final Browser br) throws Exception {
        final Browser brc = br.cloneBrowser();
        setSignedApiHeaders(brc, fetchCsrfToken(brc));
        brc.getPage(API_BASE + "/api/v11/keep-alive");
        if (brc.getHttpConnection().getResponseCode() != 200) {
            return null;
        }
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        return entries.get("user") != null ? entries : null;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        /* force == true: Never returns null */
        final Map<String, Object> session = login(br, account, true);
        final Map<String, Object> user = (Map<String, Object>) session.get("user");
        if (Boolean.TRUE.equals(user.get("is_able_to_access_premium"))) {
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
        }
        /* Example: 2025-08-05T09:34:32.290Z */
        ai.setCreateTime(TimeFormatter.getMilliSeconds(user.get("created_at").toString(), "yyyy-MM-dd'T'HH:mm:ss.SSSX", Locale.ENGLISH));
        final long sessionExpireTimestampMillis = ((Number) session.get("session_expire_at_unix")).longValue() * 1000;
        logger.info("Remaining session duration: " + TimeFormatter.formatMilliSeconds(sessionExpireTimestampMillis - System.currentTimeMillis(), 0));
        ai.setUnlimitedTraffic();
        return ai;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        /* No captchas at all (except for the login-captcha) */
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }
}