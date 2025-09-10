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

import java.io.IOException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Application;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;

@HostPlugin(revision = "$Revision: 51469 $", interfaceVersion = 2, names = {}, urls = {})
public class FileFactory extends PluginForHost {
    public FileFactory(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + this.getHost() + "/pricing");
    }

    @Override
    public FEATURE[] getFeatures() {
        return new FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL, LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookie(getHost(), "filefactory_relaunch", "seen");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + this.getHost() + "/legal/terms";
    }

    private String getContentURL(final DownloadLink link) throws PluginException {
        return "https://www." + getHost() + "/file/" + this.getFUID(link);
    }

    private String getWebapiBase() {
        return "https://www." + getHost() + "/api";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (this.isPremiumAccount(account)) {
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if (this.isPremiumAccount(account)) {
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFUID(link);
        if (fid != null) {
            return getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "filefactory.com" });
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

    private static final Pattern PATTERN_FILE = Pattern.compile("/(?:file|stream)/([a-z0-9]+)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_FILE.pattern());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformationWebsite(null, link);
    }

    private AvailableStatus requestFileInformationWebsite(final Account account, final DownloadLink link) throws Exception {
        setBrowserExclusive();
        if (account != null) {
            this.loginWebsite(account, false);
        }
        final String contenturl = this.getContentURL(link);
        final boolean isDownload = PluginEnvironment.DOWNLOAD.equals(this.getPluginEnvironment());
        try {
            URLConnectionAdapter con;
            if (isDownload) {
                dl = new jd.plugins.BrowserAdapter().openDownload(br, link, contenturl, this.isResumeable(link, account), this.getMaxChunks(link, account));
                con = dl.getConnection();
            } else {
                con = br.openGetConnection(contenturl);
            }
            if (this.looksLikeDownloadableContent(con)) {
                link.setFinalFileName(Plugin.getFileNameFromConnection(con));
                if (con.getCompleteContentLength() > 0) {
                    if (con.isContentDecoded()) {
                        link.setDownloadSize(con.getCompleteContentLength());
                    } else {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                }
                link.setAvailable(true);
                return AvailableStatus.TRUE;
            }
            br.followConnection();
            checkErrorsWebsite(link, br);
            this.dl = null;
        } finally {
            if (!isDownload) {
                this.dl = null;
            }
        }
        if (isPasswordProtectedFile(br)) {
            link.setPasswordProtected(true);
        } else {
            link.setPasswordProtected(false);
        }
        /* Small hack: Correct json in html code to make br.getRegex calls down below work. */
        final String unescaped = PluginJSonUtils.unescape(br.getRequest().getHtmlCode());
        br.getRequest().setHtmlCode(unescaped);
        this.checkErrorsWebsite(link, br);
        String filename = br.getRegex("\"disp_filename\":\"([^\"]+)").getMatch(0);
        String filesizeBytesStr = br.getRegex("\"size\":\"\\$n(\\d+)").getMatch(0);
        if (filename != null) {
            link.setName(filename);
        }
        if (filesizeBytesStr != null) {
            link.setVerifiedFileSize(Long.parseLong(filesizeBytesStr));
        }
        return AvailableStatus.TRUE;
    }

    public void checkErrorsWebsite(final DownloadLink link, final Browser br) throws PluginException {
        // TODO: Add more error handling
        final String error_code = br.getRegex("\"errorCode\":\"([^\"]+)\"").getMatch(0);
        if (error_code == null) {
            return;
        }
        if (error_code.equalsIgnoreCase("FILE_NOT_FOUND")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            logger.info("Unknown error happened: " + error_code);
            throw new PluginException(LinkStatus.ERROR_FATAL, error_code);
        }
    }

    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        final Browser br = this.createNewBrowserInstance();
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account == null) {
            // logger.info("Login impossible -> Cannot use mass-linkchecking");
            return false;
        }
        try {
            loginWebsite(account, false);
        } catch (Exception e) {
            logger.log(e);
            logger.info("Login failed -> Cannot use mass-linkchecking");
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
                        sb.append("\n");
                    }
                    sb.append(this.getContentURL(link));
                }
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("links", sb.toString());
                // TODO: Fix this request
                br.postPageRaw(this.getWebapiBase() + "/tools/link-checker", JSonStorage.serializeToJson(postdata));
                /* Returns http response 401 when not logged in along with response {"error":"Unauthorized"} */
                final Map<String, Object> entries = checkErrorsWebapi(br, account, null);
                final List<Map<String, Object>> items = (List<Map<String, Object>>) entries.get("results");
                int this_index = -1;
                for (final DownloadLink link : links) {
                    this_index++;
                    if (this_index > items.size() - 1) {
                        logger.warning("Item missing in API response: " + link.getPluginPatternMatcher());
                        link.setAvailable(false);
                        continue;
                    }
                    final Map<String, Object> linkinfo = items.get(this_index);
                    /* If "importEligible" equals "owned", this file is owned by the currently logged in user. */
                    // final String importEligible = (String) linkinfo.get("importEligible");
                    final String status = linkinfo.get("status").toString();
                    if (status.equalsIgnoreCase("valid")) {
                        link.setAvailable(true);
                        final Map<String, Object> fileDetails = (Map<String, Object>) linkinfo.get("fileDetails");
                        link.setFinalFileName(fileDetails.get("name").toString());
                        link.setDownloadSize(SizeFormatter.getSize(fileDetails.get("size").toString()));
                    } else {
                        /* Assume that status equals "invalid" with error message "File not found or has been deleted.". */
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

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        loginWebsite(account, true);
        final AccountInfo ai = new AccountInfo();
        br.getPage(this.getWebapiBase() + "/dashboard");
        final Map<String, Object> entries = this.checkErrorsWebapi(br, account);
        final Map<String, Object> settings = (Map<String, Object>) entries.get("settings");
        if (Boolean.TRUE.equals(entries.get("isLifetime"))) {
            account.setType(AccountType.LIFETIME);
        } else if (Boolean.TRUE.equals(entries.get("isPremium"))) {
            account.setType(AccountType.PREMIUM);
            String nextRebillDate = "Never";
            if (Boolean.TRUE.equals(entries.get("hasActiveRebill"))) {
                nextRebillDate = entries.get("nextRebillDate").toString();
            }
            final int daysRemaining = ((Number) entries.get("daysRemaining")).intValue();
            ai.setValidUntil(System.currentTimeMillis() + daysRemaining * 24 * 60 * 60 * 1000, br);
            ai.setStatus(AccountType.PREMIUM.getLabel() + " | Next rebill date: " + nextRebillDate);
        } else {
            /* Expired/free account */
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(getMaxSimultanFreeDownloadNum());
        }
        final String usedSpaceGB_Str = (String) entries.get("usedSpace");
        if (usedSpaceGB_Str != null && usedSpaceGB_Str.matches("\\d+")) {
            ai.setUsedSpace(Long.parseLong(usedSpaceGB_Str) * 1024 * 1024 * 1024);
        }
        if (account.loadUserCookies() != null) {
            /* Ensure to have unique usernames when user is using cookie login */
            final String email = (String) settings.get("email");
            if (email != null) {
                account.setUser(email);
            } else {
                /* Possibly Google login user? */
                logger.warning("WTF account has no email?");
            }
        }
        return ai;
    }

    private void loginWebsite(final Account account, final boolean validateCookies) throws Exception {
        synchronized (account) {
            setBrowserExclusive();
            final Cookies cookies = account.loadCookies("");
            final Cookies userCookies = account.loadUserCookies();
            if (cookies != null || userCookies != null) {
                if (userCookies != null) {
                    br.setCookies(userCookies);
                } else {
                    br.setCookies(cookies);
                }
                if (!validateCookies) {
                    /* Do not verify cookies */
                    return;
                }
                /* Verify cookies */
                try {
                    this.checkLoginStatus(account);
                    logger.info("Cookie login successful");
                    if (userCookies == null) {
                        account.saveCookies(br.getCookies(br.getHost()), "");
                    }
                    return;
                } catch (final AccountInvalidException e) {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    if (userCookies != null) {
                        logger.info("User Cookie login failed");
                        if (account.hasEverBeenValid()) {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                        } else {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                        }
                    }
                }
            }
            logger.info("Performing full login");
            br.getPage("https://www." + getHost() + "/signin?from=%2Fdashboard");
            if (false) {
                // not required at the moment
                br.setCookie(br.getHost(), "cookieConsent", "accepted");
                br.setCookie(br.getHost(), "cookieConsentTimestamp", "2025-09-09T14:12:12.097Z");
            }
            if (false) {
                // not required at the moment?! only sets the recaptcha-verified cookie, LOL
                final CaptchaHelperHostPluginRecaptchaV2 rc = new CaptchaHelperHostPluginRecaptchaV2(this, br, "6Le6wT0rAAAAAEOzVh77jsWDtqGkwbXcvuPdmaeW") {
                    @Override
                    protected boolean isEnterprise() {
                        return true;
                    }

                    @Override
                    protected String getSiteUrl() {
                        return "https://www.filefactory.com/signin";
                    }

                    @Override
                    protected Map<String, Object> getV3Action() {
                        final Map<String, Object> ret = new HashMap<String, Object>();
                        ret.put("action", "SIGNIN");
                        return ret;
                    }

                };
                final String recaptchaV2Response = rc.getToken();
                final Map<String, Object> postdata1 = new HashMap<String, Object>();
                postdata1.put("action", "SIGNIN");
                postdata1.put("captchaToken", recaptchaV2Response);
                final Browser brc = br.cloneBrowser();
                brc.postPageRaw(this.getWebapiBase() + "/auth/pre-auth-check", JSonStorage.serializeToJson(postdata1));
                final Map<String, Object> entries1 = checkErrorsWebapi(brc, account);
                if (!Boolean.TRUE.equals(entries1.get("success"))) {
                    // throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
            }
            br.setCookie(getHost(), "recaptcha-verified", "true");
            br.setCookie(br.getHost(), "__Secure-authjs.callback-url", "https%3A%2F%2Fwww.filefactory.com");
            Browser brc = br.cloneBrowser();
            brc.getPage(this.getWebapiBase() + "/auth/csrf");
            final Map<String, Object> entries2 = checkErrorsWebapi(brc, account);
            final String csrfToken = entries2.get("csrfToken").toString();
            final Form loginForm = new Form();
            loginForm.setMethod(MethodType.POST);
            loginForm.setAction(this.getWebapiBase() + "/auth/callback/credentials");
            loginForm.put("email", URLEncoder.encode(account.getUser(), "UTF-8"));
            loginForm.put("password", URLEncoder.encode(account.getPass(), "UTF-8"));
            loginForm.put("csrfToken", csrfToken);
            loginForm.put("callbackUrl", "%2Fdashboard");
            br.submitForm(loginForm);
            checkErrorsWebapi(br, account, null, null);
            checkLoginStatus(account);
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownloadWebsite(link, null);
    }

    public void handleDownloadWebsite(final DownloadLink link, final Account account) throws Exception {
        requestFileInformationWebsite(account, link);
        /* If dl != null this means that we had a direct downloadable link. */
        if (this.dl == null) {
            final boolean isPremium = this.isPremiumAccount(account);
            if (!isPremium) {
                // ads
                if (checkShowFreeDialog(getHost())) {
                    showFreeDialog(getHost());
                }
            }
            final String dltoken = br.getRegex("requestToken\":\"([^\"]+)").getMatch(0);
            if (dltoken == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String fid = this.getFUID(link);
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("hash", fid);
            postdata.put("token", dltoken);
            // if (isPremium) {
            // postdata.put("type", "premium");
            // } else {
            // postdata.put("type", "free");
            // }
            /* 2025-09-09: Even in premium mode, the value of this is "free". */
            postdata.put("type", "free");
            /* Website has 45-60 seconds of pre download wait time for free (& free-account) users which can be skipped. */
            br.postPageRaw(this.getWebapiBase() + "/download/initiate", JSonStorage.serializeToJson(postdata));
            final Map<String, Object> entries = checkErrorsWebapi(br, account, link);
            String finallink = entries.get("url").toString();
            if (Application.getJavaVersion() < Application.JAVA17) {
                // TODO. Check if this is still working (if http redirects to https we do not need this)
                finallink = finallink.replaceFirst("(?i)https", "http");
            }
            final Number fileSize = (Number) entries.get("fileSize");
            if (fileSize != null) {
                link.setVerifiedFileSize(fileSize.longValue());
            }
            final String filename = (String) entries.get("filename");
            if (filename != null) {
                link.setFinalFileName(filename);
            }
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, finallink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                handleConnectionErrors(br, dl.getConnection());
                checkErrorsWebsite(link, br);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    @Override
    protected void throwFinalConnectionException(Browser br, URLConnectionAdapter con) throws PluginException, IOException {
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownloadWebsite(link, account);
    }

    /**
     * Checks if we're logged in via Web-API. <br>
     * Throws AccountInvalidException on invalid login.
     */
    private Map<String, Object> checkLoginStatus(final Account account) throws IOException, PluginException, InterruptedException {
        br.getPage(this.getWebapiBase() + "/auth/session");
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* Response "null" as plaintext */
            throw new AccountInvalidException(ignore, "Session expired");
        }
        checkErrorsWebapi(br, account, null, entries);
        return entries;
    }

    private Map<String, Object> checkErrorsWebapi(final Browser br, final Account account) throws PluginException, InterruptedException {
        return checkErrorsWebapi(br, account, null);
    }

    private Map<String, Object> checkErrorsWebapi(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String msg = "Invalid API response";
            final long waitMillis = 60 * 1000l;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, waitMillis, ignore);
            } else {
                throw new AccountUnavailableException(ignore, msg, 60 * 1000);
            }
        }
        checkErrorsWebapi(br, account, link, entries);
        return entries;
    }

    private void checkErrorsWebapi(final Browser br, final Account account, final DownloadLink link, final Map<String, Object> entries) throws PluginException, InterruptedException {
        if (StringUtils.containsIgnoreCase(br.getURL(), "/api/auth/session")) {
            if (entries == null || entries.size() == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else if (entries.get("user") == null) {
                throw new AccountInvalidException();
            }
        }
        if (StringUtils.containsIgnoreCase(br.getURL(), "/api/auth/csrf ")) {
            if (entries == null || entries.size() == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else if (entries.get("csrfToken") == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        if (StringUtils.containsIgnoreCase(br.getURL(), "signin?error=CredentialsSignin&code=credentials")) {
            throw new AccountInvalidException();
        }
    }

    private boolean isPasswordProtectedFile(final Browser br) {
        if (br.containsHTML("\"requiresPassword\":true")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account account) {
        return false;
    }

    @Override
    public boolean hasAutoCaptcha() {
        return false;
    }

    private String getFUID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), PATTERN_FILE).getMatch(0);
    }

    private boolean isPremiumAccount(final Account account) {
        if (account == null) {
            return false;
        } else if (account.getType() == AccountType.PREMIUM || account.getType() == AccountType.LIFETIME) {
            return true;
        } else {
            return false;
        }
    }
}