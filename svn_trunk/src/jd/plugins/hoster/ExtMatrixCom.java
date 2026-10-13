//jDownloader - Downloadmanager
//Copyright (C) 2011  JD-Team support@jdownloader.org
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
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 53587 $", interfaceVersion = 3, names = {}, urls = {})
public class ExtMatrixCom extends PluginForHost {
    public ExtMatrixCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/v2/premium");
    }

    /* Cloudflare Turnstile sitekey used for the login captcha. */
    private static final String TURNSTILE_SITEKEY = "0x4AAAAAAFJ5UTqieM77S3Qo";

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/v2/terms";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        /* Each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain */
        ret.add(new String[] { "extmatrix.com" });
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
            /* Only new links are supported: /v2/files/<fid>/<name>.html or just /v2/files/<fid>/<something> (e.g. "/0") */
            ret.add("https?://(?:[A-Za-z0-9]+\\.)?" + buildHostsPatternPart(domains) + "/v2/files/([A-Za-z0-9]+)(?:/([^/\\?#]+?)(?:\\.html)?)?");
        }
        return ret.toArray(new String[0]);
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

    private String getApiBase() {
        /* Domain without "www." redirects (301) to the www domain which turns POST requests into GET -> 405. */
        return "https://www." + getHost() + "/v2/api";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        /* TODO: Update once download information is available. */
        return true;
    }

    public int getMaxChunks(final Account account) {
        /* TODO: Update once download information is available. */
        return 1;
    }

    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        try {
            final Browser br = createNewBrowserInstance();
            final List<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    /* TODO: 2026-10-09: Check how many links the API accepts at once, check if 100 is really fine. */
                    if (index == urls.length || links.size() == 100) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                /* API expects the URLs as one string (format of multiple URLs unknown for now -> newline separated). */
                final StringBuilder sb = new StringBuilder();
                for (final DownloadLink link : links) {
                    if (sb.length() > 0) {
                        sb.append("\n");
                    }
                    sb.append(getContentURL(link));
                    if (!link.isNameSet()) {
                        /* Set weak filename */
                        final String urlFilename = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(1);
                        link.setName(urlFilename != null ? urlFilename : getFID(link));
                    }
                }
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("urls", sb.toString());
                br.getHeaders().put("Content-Type", "application/json");
                br.postPageRaw(getApiBase() + "/files/check-links", JSonStorage.serializeToJson(postdata));
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final List<Map<String, Object>> items = (List<Map<String, Object>>) entries.get("items");
                for (final DownloadLink link : links) {
                    final String fid = getFID(link);
                    Map<String, Object> match = null;
                    for (final Map<String, Object> item : items) {
                        if (StringUtils.equals(fid, (String) item.get("fileId"))) {
                            match = item;
                            break;
                        }
                    }
                    if (match == null) {
                        /* Item not returned -> Treat as offline. */
                        link.setAvailable(false);
                    } else if ("valid".equals(match.get("status"))) {
                        link.setAvailable(true);
                        link.setFinalFileName(match.get("name").toString());
                        link.setVerifiedFileSize(((Number) match.get("size")).longValue());
                    } else {
                        /* E.g. status "dead" or item missing in response. */
                        link.setAvailable(false);
                    }
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

    private String getContentURL(final DownloadLink link) {
        return link.getPluginPatternMatcher().replaceFirst("(?i)^http://", "https://");
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        checkLinks(new DownloadLink[] { link });
        if (!link.isAvailabilityStatusChecked()) {
            return AvailableStatus.UNCHECKED;
        } else if (!link.isAvailable()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            return AvailableStatus.TRUE;
        }
    }

    /** Performs login via API. Session is kept via cookies. */
    private Map<String, Object> login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                br.setCookies(getHost(), cookies);
                if (!force) {
                    /* Trust stored session without further check. */
                    return null;
                }
                /* Check session and obtain account information via website. */
                br.getPage("https://www." + getHost() + "/v2/account");
                final String userBlock = br.getRegex("user:\\{(id:\\d+,name:\"[^\"]+\".*?)\\}").getMatch(0);
                if (userBlock != null) {
                    final Map<String, Object> user = new HashMap<String, Object>();
                    user.put("name", new Regex(userBlock, "name:\"([^\"]+)\"").getMatch(0));
                    user.put("isPremium", "true".equals(new Regex(userBlock, "isPremium:(true|false)").getMatch(0)));
                    user.put("premiumEnd", Long.parseLong(new Regex(userBlock, "premiumEnd:(\\d+)").getMatch(0)));
                    logger.info("Successfully logged in via cookies");
                    return user;
                }
                logger.info("Cookie login failed");
                br.clearCookies(getHost());
                account.clearCookies("");
            }
            logger.info("Performing full login");
            Map<String, Object> entries = postLogin(account, null);
            if (isCaptchaRequired(entries)) {
                logger.info("Login captcha required");
                final String cfTurnstileResponse = new CaptchaHelperHostPluginCloudflareTurnstile(this, br, TURNSTILE_SITEKEY).getToken();
                entries = postLogin(account, cfTurnstileResponse);
            }
            final Map<String, Object> error = (Map<String, Object>) entries.get("error");
            if (error != null) {
                final String code = (String) error.get("code");
                final String message = (String) error.get("message");
                if ("captcha_required".equals(code)) {
                    /* Captcha was required again even though we sent a token -> Token was wrong/expired. */
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                } else if ("invalid_credentials".equals(code)) {
                    throw new AccountInvalidException(message);
                } else {
                    /* Unknown error code -> Do not blindly treat as invalid account. */
                    throw new AccountInvalidException("Login failed: " + code + " | " + message);
                }
            }
            account.saveCookies(br.getCookies(getHost()), "");
            return (Map<String, Object>) entries.get("user");
        }
    }

    private Map<String, Object> postLogin(final Account account, final String cfTurnstileResponse) throws Exception {
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("login", account.getUser());
        postdata.put("password", account.getPass());
        if (cfTurnstileResponse != null) {
            postdata.put("captchaToken", cfTurnstileResponse);
        }
        br.getHeaders().put("Content-Type", "application/json");
        br.postPageRaw(getApiBase() + "/auth/login", JSonStorage.serializeToJson(postdata));
        return restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
    }

    private boolean isCaptchaRequired(final Map<String, Object> entries) {
        final Map<String, Object> error = (Map<String, Object>) entries.get("error");
        /* Only the error code counts: "details.captchaRequired" can also be set for other errors e.g. "invalid_credentials". */
        return error != null && "captcha_required".equals(error.get("code"));
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        /* Force full login to obtain user information. */
        final Map<String, Object> user = login(account, true);
        if (Boolean.TRUE.equals(user.get("isPremium"))) {
            account.setType(AccountType.PREMIUM);
            /* Timestamp is in seconds. */
            ai.setValidUntil(((Number) user.get("premiumEnd")).longValue() * 1000, br);
        } else {
            account.setType(AccountType.FREE);
        }
        final String name = (String) user.get("name");
        if (!StringUtils.isEmpty(name)) {
            account.setUser(name);
        }
        return ai;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownload(link, null);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        login(account, false);
        handleDownload(link, account);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception {
        /** 2026-10-12: I was unable to find any files that can be downloaded without premium account. */
        if (account == null || !AccountType.PREMIUM.equals(account.getType())) {
            /* Free (anonymous) downloads are not possible. */
            throw new AccountRequiredException();
        }
        requestFileInformation(link);
        final String dllink = getDownloadURL(link, account);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, isResumeable(link, account), getMaxChunks(account));
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
            }
            br.followConnection(true);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl.startDownload();
    }

    /** Requests a (short-lived) final downloadurl via API. Premium account (session) required. */
    private String getDownloadURL(final DownloadLink link, final Account account) throws Exception {
        br.getHeaders().put("Content-Type", "application/json");
        br.getHeaders().put("Origin", "https://www.extmatrix.com");
        br.getHeaders().put("Referer", getContentURL(link));
        final String fid = this.getFID(link);
        br.getPage(getApiBase() + "/download/" + fid);
        Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> viewer = (Map<String, Object>) entries.get("viewer");
        if (br.getHttpConnection().getResponseCode() == 401 || (viewer != null && !Boolean.TRUE.equals(viewer.get("loggedIn")))) {
            /* Session expired -> Login again and retry once. */
            login(account, true);
            br.getHeaders().put("Content-Type", "application/json");
        }
        br.postPageRaw(getApiBase() + "/download/" + fid + "/link", "{}");
        entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String url = (String) entries.get("url");
        if (StringUtils.isEmpty(url)) {
            final Map<String, Object> error = (Map<String, Object>) entries.get("error");
            final String message = error != null ? (String) error.get("message") : null;
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, message);
        }
        return url;
    }

    /** Invalidates the account's session server-side. Called once the account gets removed. */
    private void logout(final Account account) {
        synchronized (account) {
            final Cookies cookies = account.loadCookies("");
            if (cookies == null) {
                /* No stored session -> We cannot logout */
                return;
            }
            try {
                if (br == null) {
                    /* This plugin instance may already have gone through clean(). */
                    setBrowser(createNewBrowserInstance());
                }
                br.setCookies(getHost(), cookies);
                br.postPageRaw(getApiBase() + "/auth/logout", "");
                logger.info("Logout successful");
                account.clearCookies("");
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

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account != null && AccountType.PREMIUM.equals(account.getType())) {
            /* Free (anonymous) downloads are not possible. */
            return true;
        }
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }
}
