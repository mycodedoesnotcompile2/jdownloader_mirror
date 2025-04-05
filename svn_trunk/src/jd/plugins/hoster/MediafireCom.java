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
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.requests.GetRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
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
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.UserAgents;
import jd.plugins.decrypter.MediafireComFolder;
import jd.plugins.download.HashInfo;
import jd.utils.locale.JDL;

@HostPlugin(revision = "$Revision: 50926 $", interfaceVersion = 3, names = { "mediafire.com" }, urls = { "https?://(?:www\\.)?mediafire\\.com/file/([a-z0-9]+)(/([^/]+))?" })
public class MediafireCom extends PluginForHost {
    /** Settings stuff */
    private static final String FREE_TRIGGER_RECONNECT_ON_CAPTCHA = "FREE_TRIGGER_RECONNECT_ON_CAPTCHA";
    public static final String  PROPERTY_FILE_ID                  = "fileid";
    public static final String  PROPERTY_PRIVATE_FILE             = "privatefile";
    public static final String  PROPERTY_ACCOUNT_SESSION_TOKEN    = "session_token";

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String[] siteSupportedNames() {
        return new String[] { "mediafire.com", "mediafire" };
    }

    @Override
    public void init() {
        Browser.setRequestIntervalLimitGlobal(this.getHost(), 250);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setCustomCharset("utf-8");
        return br;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFUID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFUID(final DownloadLink link) {
        String fileID = link.getStringProperty(PROPERTY_FILE_ID);
        if (fileID == null && link.getPluginPatternMatcher() != null) {
            try {
                fileID = MediafireComFolder.getFileIDFRomURL(link.getPluginPatternMatcher());
            } catch (final MalformedURLException e) {
                e.printStackTrace();
            }
        }
        return fileID;
    }

    private String getFilenameFromURL(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(2);
    }

    private String getContentURL(final DownloadLink link) {
        return link.getPluginPatternMatcher().replaceFirst("(?)http://", "https://");
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return -15;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return -15;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, null);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        if (!checkLinks(new DownloadLink[] { link }, account) || !link.isAvailabilityStatusChecked()) {
            link.setAvailableStatus(AvailableStatus.UNCHECKABLE);
        } else if (!link.isAvailable()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return link.getAvailableStatus();
    }

    public static abstract class PasswordSolver {
        protected Browser       br;
        protected PluginForHost plg;
        protected DownloadLink  dlink;
        private final int       maxTries;
        private int             currentTry;

        public PasswordSolver(final PluginForHost plg, final Browser br, final DownloadLink downloadLink) {
            this.plg = plg;
            this.br = br;
            this.dlink = downloadLink;
            this.maxTries = 3;
            this.currentTry = 0;
        }

        abstract protected void handlePassword(String password) throws Exception;

        public boolean hasAutoCaptcha() {
            return false;
        }

        abstract protected boolean isCorrect();

        public void run() throws Exception {
            while (this.currentTry++ < this.maxTries) {
                String password = null;
                if ((password = this.dlink.getDownloadPassword()) != null) {
                } else {
                    password = plg.getUserInput(JDL.LF("PasswordSolver.askdialog", "Downloadpassword for %s/%s", this.plg.getHost(), this.dlink.getName()), this.dlink);
                }
                if (password == null) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, JDL.L("plugins.errors.wrongpassword", "Password wrong"));
                }
                this.handlePassword(password);
                if (!this.isCorrect()) {
                    this.dlink.setDownloadPassword(null);
                    continue;
                } else {
                    this.dlink.setDownloadPassword(password);
                    return;
                }
            }
            throw new PluginException(LinkStatus.ERROR_RETRY, "Password wrong");
        }
    }

    /*
     * https://www.mediafire.com/developers/core_api/1.5/getting_started/
     */
    @SuppressWarnings("deprecation")
    public MediafireCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/upgrade/");
        setConfigElements();
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/terms_of_service.php";
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> response = login(this.br, account, true);
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> user_info = (Map<String, Object>) response.get("user_info");
        final String isPremium = user_info.get("premium").toString();
        final Object used_storage_sizeO = user_info.get("used_storage_size");
        final Object bandwidthO = user_info.get("bandwidth");
        final String createDate = (String) user_info.get("created");
        if (createDate != null) {
            ai.setCreateTime(TimeFormatter.getMilliSeconds(createDate, "yyyy-MM-dd", Locale.ENGLISH));
        }
        if (used_storage_sizeO != null && used_storage_sizeO.toString().matches("\\d+")) {
            ai.setUsedSpace(Long.parseLong(used_storage_sizeO.toString()));
        }
        if (StringUtils.equalsIgnoreCase(isPremium, "yes")) {
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(-1);
            account.setConcurrentUsePossible(true);
        } else {
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(10);
            account.setConcurrentUsePossible(true);
        }
        if (bandwidthO != null && bandwidthO.toString().matches("\\d+")) {
            ai.setTrafficLeft(Long.parseLong(bandwidthO.toString()));
        } else {
            ai.setUnlimitedTraffic();
        }
        return ai;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    public static void storeDirecturl(final DownloadLink link, final Account account, final String directurl) {
        link.setProperty(getDirecturlProperty(link, account), directurl);
    }

    public static String getDirecturlProperty(final DownloadLink link, final Account account) {
        String propertyStr = "directurl";
        if (account != null) {
            propertyStr += "_" + account.getType();
        }
        return propertyStr;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownload(link, null);
    }

    /** Use this for free- and free account download. */
    public void handleDownload(final DownloadLink link, final Account account) throws Exception {
        if (account != null) {
            this.login(br, account, false);
        } else {
            final String randomUserAgent = UserAgents.stringUserAgent();
            br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, randomUserAgent);
        }
        final String directurlproperty = getDirecturlProperty(link, account);
        final String storedDirecturl = link.getStringProperty(directurlproperty);
        String finalDownloadurl = null;
        if (storedDirecturl != null) {
            /* Download of previously stored directurl */
            logger.info("Trying to re-use stored directurl: " + storedDirecturl);
            finalDownloadurl = storedDirecturl;
        } else if (account != null && AccountType.PREMIUM.equals(account.getType())) {
            /* Premium download */
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("link_type", "direct_download");
            query.appendEncoded("quick_key", this.getFUID(link));
            final Map<String, Object> resp = apiCommand(link, account, "file/get_links.php", query);
            final Map<String, Object> linkmap = (Map<String, Object>) JavaScriptEngineFactory.walkJson(resp, "links/{0}");
            finalDownloadurl = (String) linkmap.get("direct_download");
            if (StringUtils.isEmpty(finalDownloadurl)) {
                /* Check for errors */
                // {"response":{"action":"file\/get_links","links":[{"quickkey":"removed","error":"User lacks
                // permissions"}],"result":"Success","current_api_version":"1.5"}}
                if (StringUtils.equalsIgnoreCase(PluginJSonUtils.getJson(br, "error"), "User lacks permissions")) {
                    throw new AccountRequiredException("Incorrect account been used to download this file");
                } else {
                    /* This should never happen */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        } else {
            /* Free download */
            requestFileInformation(link, account);
            if (link.hasProperty(PROPERTY_PRIVATE_FILE)) {
                throw new AccountRequiredException("Private file: Only downloadable for users with permission and owner");
            }
            final String contenturl = this.getContentURL(link);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, contenturl, this.isResumeable(link, account), this.getMaxChunks(account));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                logger.info("Found hotlinked item");
                dl.startDownload();
                return;
            }
            dl = null;
            br.followConnection();
            if (this.getIPLimitReachedSecondsStr(br) != null) {
                /* IP limit sits on combination of IP + User-Agent (+ maybe cookies) */
                logger.info("Detected IP limit -> Trying to avoid it");
                int i = 0;
                final int imax = 5;
                boolean success = false;
                limitAvoidanceLoop: do {
                    logger.info("Trying to avoid IP limit round " + i + "/" + imax);
                    if (account == null) {
                        br.clearCookies(null);
                    } else {
                        /* If we got a free account we do not want to clear cookies, only change User-Agent. */
                    }
                    final String randomUserAgent = UserAgents.stringUserAgent();
                    br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, randomUserAgent);
                    this.sleep(2000, link);
                    br.getPage(contenturl);
                    i++;
                    if (this.getIPLimitReachedSecondsStr(br) == null) {
                        logger.info("Successfully avoided limit");
                        success = true;
                        break limitAvoidanceLoop;
                    }
                } while (!this.isAbort() && imax <= 5);
                if (!success) {
                    logger.info("Failed to avoid limit -> Exception will happen down below in errorhandling");
                }
            }
            int trycounter = -1;
            Form captchaForm = getCaptchaForm(br);
            if (captchaForm != null) {
                boolean captchSuccess = false;
                do {
                    trycounter++;
                    logger.info("CaptchaForm loop number: " + trycounter);
                    handleNonAPIErrors(link, br);
                    if (captchaForm.containsHTML("g-recaptcha-response")) {
                        /* If user does not want to solve captchas, the following code will trigger an exception. */
                        handleReconnectBehaviorOnCaptcha(account);
                        final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                        captchaForm.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                        br.submitForm(captchaForm);
                    } else if (captchaForm.containsHTML("for=\"customCaptchaCheckbox\"")) {
                        /* Mediafire custom checkbox "captcha" */
                        /* If user does not want to solve captchas, the following code will trigger an exception. */
                        handleReconnectBehaviorOnCaptcha(account);
                        captchaForm.put("mf_captcha_response", "1");
                        br.submitForm(captchaForm);
                    } else {
                        logger.warning("Unknown/Unsupported captcha type required");
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    captchaForm = getCaptchaForm(br);
                    if (getCaptchaForm(br) != null) {
                        logger.info("Wrong captcha");
                        continue;
                    } else {
                        captchSuccess = true;
                        break;
                    }
                } while (trycounter <= 3 && finalDownloadurl == null);
                if (!captchSuccess) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
            }
            this.handlePW(link);
            finalDownloadurl = br.getRegex("kNO\\s*=\\s*\"(https?://.*?)\"").getMatch(0);
            logger.info("Kno= " + finalDownloadurl);
            if (finalDownloadurl == null) {
                /* pw protected files can directly redirect to download */
                finalDownloadurl = br.getRedirectLocation();
            }
            if (finalDownloadurl == null) {
                finalDownloadurl = br.getRegex("href\\s*=\\s*\"(https?://[^\"]+)\"\\s*id\\s*=\\s*\"downloadButton\"").getMatch(0);
                if (finalDownloadurl == null) {
                    finalDownloadurl = br.getRegex("(" + MediafireComFolder.TYPE_DIRECT + ")").getMatch(0);
                }
            }
        }
        if (StringUtils.isEmpty(finalDownloadurl)) {
            this.handleNonAPIErrors(link, br);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        link.setProperty(directurlproperty, finalDownloadurl);
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, finalDownloadurl, this.isResumeable(link, account), this.getMaxChunks(account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                handleServerErrors();
                handleNonAPIErrors(link, br);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directurlproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired?", e);
            } else {
                throw e;
            }
        }
        dl.startDownload();
    }

    private Form getCaptchaForm(final Browser br) {
        return br.getFormbyProperty("name", "form_captcha");
    }

    private void handleServerErrors() throws PluginException {
        if (dl.getConnection().getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403, ", 30 * 60 * 1000l);
        } else if (dl.getConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404, ", 30 * 60 * 1000l);
        }
    }

    private void handleReconnectBehaviorOnCaptcha(final Account account) throws PluginException {
        if (this.getPluginConfig().getBooleanProperty(FREE_TRIGGER_RECONNECT_ON_CAPTCHA, false)) {
            if (account != null) {
                logger.info("Captcha reconnect setting active & free account used --> TEMPORARILY_UNAVAILABLE");
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Waiting some time to avoid captcha in free account mode", 30 * 60 * 1000l);
            } else {
                logger.info("Captcha reconnect setting active & NO account used --> IP_BLOCKED");
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Reconnecting or waiting some time to avoid captcha in free mode", 30 * 60 * 1000l);
            }
        }
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload(link, account);
    }

    private void handlePW(final DownloadLink link) throws Exception {
        final String pwProtectedString = "aria-labelledby\\s*=\\s*\"passwordmsg\"|class\\s*=\\s*\"passwordPrompt\"";
        if (!br.containsHTML(pwProtectedString)) {
            link.setPasswordProtected(false);
            return;
        }
        link.setPasswordProtected(true);
        logger.info("Handle possible PW");
        new PasswordSolver(this, br, link) {
            String curPw = null;

            @Override
            protected void handlePassword(final String password) throws Exception {
                curPw = password;
                final Form form = getPasswordForm();
                if (form == null) {
                    logger.warning("Failed to find passwordForm");
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                form.put("downloadp", Encoding.urlEncode(curPw));
                final boolean followRedirect = br.isFollowingRedirects();
                try {
                    br.setFollowRedirects(false);
                    br.submitForm(form);
                } finally {
                    br.setFollowRedirects(followRedirect);
                }
            }

            @Override
            protected boolean isCorrect() {
                Form form = getPasswordForm();
                if (form != null) {
                    return false;
                } else {
                    return true;
                }
            }

            protected Form getPasswordForm() {
                Form form = br.getFormByRegex(pwProtectedString);
                if (form != null) {
                    return form;
                }
                form = br.getFormbyProperty("name", "download");
                if (form != null && !form.containsHTML(pwProtectedString)) {
                    if (form.getInputField("downloadp") != null) {
                        logger.warning("Maybe passwordform(?)");
                        return form;
                    } else {
                        logger.warning("Wrong passwordform(?) --> Returning null");
                        return null;
                    }
                }
                return form;
            }
        }.run();
    }

    public Map<String, Object> login(final Browser br, final Account account, boolean force) throws Exception {
        final Cookies cookies = account.loadCookies("");
        if (cookies != null) {
            br.setCookies(cookies);
            if (!force) {
                return null;
            }
            logger.info("Checking cookie validity");
            try {
                final Map<String, Object> resp = apiCommand(null, account, "user/get_info.php", null);
                final String email = (String) JavaScriptEngineFactory.walkJson(resp, "user_info/email");
                if (StringUtils.equalsIgnoreCase(email, account.getUser())) {
                    logger.info("Cookie login successful");
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return resp;
                } else {
                    logger.info("Cookie login failed");
                }
            } catch (final PluginException ignore) {
                /*
                 * E.g. {"response":{"action":"user\/get_info","message":"The supplied Session Token is expired or invalid","error":105,
                 * "result":"Error","current_api_version":"1.5"}}
                 */
                logger.exception("API session login threw exception -> Full login required", ignore);
            }
        }
        logger.info("Performing full login");
        br.clearCookies(null);
        br.getPage("https://www." + this.getHost() + "/login/");
        Form form = br.getFormbyProperty("id", "form_login1");
        if (form == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (form.getAction() == null) {
            final String[] posturls = br.getRegex("mSendDataByPostJSON\\('(/[^'\"]+)'").getColumn(0);
            if (posturls != null && posturls.length == 1) {
                /* Exactly one result -> The result we want */
                form.setAction(posturls[0]);
            } else {
                /* Use hardcoded action value */
                form.setAction("/dynamic/client_login/mediafire.php");
            }
        }
        final String security = br.getRegex("security\\s*:\\s*\"([^\"]+)").getMatch(0);
        if (security != null) {
            form.put("security", security);
        } else {
            logger.warning("Failed to find value for field 'security'");
        }
        /* We want to get long lasting cookies! */
        form.put("login_remember", "true");
        form.put("login_email", Encoding.urlEncode(account.getUser()));
        form.put("login_pass", Encoding.urlEncode(account.getPass()));
        // submit via the same browser
        br.submitForm(form);
        /* 2021-04-29: This might return an error via json but as long as we get the cookie all is fine! */
        final String cookie = br.getCookie(br.getHost(), "user", Cookies.NOTDELETEDPATTERN);
        if (cookie == null || cookie.equalsIgnoreCase("x")) {
            throw new AccountInvalidException();
        }
        br.getPage("/myaccount/");
        String sessionToken = br.getRegex("parent\\.bqx\\(\"([a-f0-9]+)\"\\)").getMatch(0);
        if (sessionToken == null) {
            sessionToken = br.getRegex("LoadIframeLightbox\\('/templates/tos\\.php\\?token=([a-f0-9]+)").getMatch(0);
        }
        if (StringUtils.isEmpty(sessionToken)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        account.setProperty(PROPERTY_ACCOUNT_SESSION_TOKEN, sessionToken);
        // apiCommand(account, "device/get_status.php", null);
        final Map<String, Object> resp = apiCommand(null, account, "user/get_info.php", null);
        account.saveCookies(br.getCookies(br.getHost()), "");
        return resp;
    }

    public Map<String, Object> apiCommand(final DownloadLink link, final Account account, final String command, UrlQuery query) throws Exception {
        String sessionToken = null;
        if (account != null) {
            sessionToken = this.getSessionToken(account);
            if (sessionToken == null) {
                /* This should never happen. */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        br.setAllowedResponseCodes(403);
        if (query == null) {
            query = new UrlQuery();
        }
        if (sessionToken != null) {
            query.add("session_token", Encoding.urlEncode(sessionToken));
        }
        query.add("response_format", "json");
        // website still uses 1.4, api is up to 1.5 at this stage -raztoki20160101
        final String url = "https://www.mediafire.com/api/1.5/" + command + "?" + query.toString();
        final GetRequest request = br.createGetRequest(url);
        request.getHeaders().put("Accept", "*/*");
        request.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        br.getPage(request);
        return handleApiError(br, link, account);
    }

    private Map<String, Object> handleApiError(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        // FYI you can have errors even though it's success
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> resp = (Map<String, Object>) entries.get("response");
        final String result = (String) resp.get("result");
        if (StringUtils.equalsIgnoreCase(result, "Error")) {
            // TODO: Implement more errorcodes: Separate them for the different types of errors, then handle them.
            final String message = resp.get("message").toString();
            if (link == null) {
                /* Not in context of download -> Must be account/login error */
                throw new AccountInvalidException(message);
            }
            final int errorcode = ((Number) resp.get("error")).intValue();
            /* List of errorcodes: https://www.mediafire.com/developers/core_api/1.5/getting_started/#error_codes */
            switch (errorcode) {
            case 104:
                /*
                 * Invalid folderID and no account provided: If an account is provided, mediafire will just return all of the users' private
                 * files (wtf) instead of displaying a proper errormessage.
                 */
                /*
                 * {"response":{"action":"folder\/get_info","message":"Session Token is missing","error":104,"result":"Error",
                 * "current_api_version":"1.5"}}
                 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            case 105:
                dumpSession(account);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Session expired");
            case 110:
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            case 111:
                /**
                 * E.g. we tried to link-check a folder-ID as file-ID. {"response":{"action":"file\/get_info","message":"Quick Key is
                 * missing","error":111,"result":"Error", "current_api_version":"1.5"}}
                 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            case 114:
                /* E.g. private folder */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            case 173:
                throw new AccountRequiredException();
            default:
                // unknown error!
                throw new PluginException(LinkStatus.ERROR_FATAL, message);
            }
        }
        return resp;
    }

    private String getSessionToken(final Account account) {
        return account.getStringProperty(PROPERTY_ACCOUNT_SESSION_TOKEN);
    }

    private void dumpSession(final Account account) {
        account.removeProperty(PROPERTY_ACCOUNT_SESSION_TOKEN);
        account.clearCookies("");
    }

    @Override
    public boolean isProxyRotationEnabledForLinkChecker() {
        return false;
    }

    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        final Account aa = AccountController.getInstance().getValidAccount(this);
        return checkLinks(urls, aa);
    }

    public boolean checkLinks(final DownloadLink[] urls, final Account account) {
        final Browser oldBr = this.br;
        try {
            setBrowser(oldBr.cloneBrowser());
            final StringBuilder sb = new StringBuilder();
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            final Map<String, DownloadLink> linkMap = new HashMap<String, DownloadLink>();
            while (true) {
                links.clear();
                linkMap.clear();
                while (true) {
                    // maximum number of quickkeys allowed is 500.
                    if (links.size() == 100 || index == urls.length) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                sb.delete(0, sb.capacity());
                boolean addDelimiter = false;
                for (final DownloadLink link : links) {
                    if (!link.isNameSet()) {
                        /* Set weak filename. */
                        final String filenameFromURL = this.getFilenameFromURL(link);
                        if (filenameFromURL != null) {
                            link.setName(Encoding.htmlDecode(filenameFromURL).trim());
                        }
                    }
                    if (addDelimiter) {
                        sb.append(",");
                    } else {
                        addDelimiter = true;
                    }
                    final String id = getFUID(link);
                    linkMap.put(id, link);
                    sb.append(id);
                }
                final UrlQuery query = new UrlQuery();
                query.add("quick_key", sb.toString());
                try {
                    apiCommand(links.get(0), account, "file/get_info.php", query);
                } catch (final PluginException ple) {
                    if (ple.getLinkStatus() == LinkStatus.ERROR_FILE_NOT_FOUND) {
                        /* Ignore exception --> All checked items are offline. */
                        getLogger().log(ple);
                    } else {
                        throw ple;
                    }
                }
                final Map<String, Object> apiResponse = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                List<Map<String, Object>> file_infos = null;
                Object infos = JavaScriptEngineFactory.walkJson(apiResponse, "response/file_infos");
                if (infos == null) {
                    infos = JavaScriptEngineFactory.walkJson(apiResponse, "response/file_info");
                }
                if (infos != null && infos instanceof List) {
                    file_infos = (List<Map<String, Object>>) infos;
                } else if (infos != null && infos instanceof Map) {
                    file_infos = new ArrayList<Map<String, Object>>();
                    file_infos.add((Map<String, Object>) infos);
                }
                if (file_infos != null) {
                    for (final Map<String, Object> file_info : file_infos) {
                        final DownloadLink item = linkMap.remove(file_info.get("quickkey"));
                        if (item != null) {
                            parseFileInfo(item, file_info);
                        }
                    }
                }
                /* All items that API did not return in answer must be offline. */
                for (final DownloadLink offline : linkMap.values()) {
                    offline.setAvailableStatus(AvailableStatus.FALSE);
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            getLogger().log(e);
            return false;
        } finally {
            setBrowser(oldBr);
        }
        return true;
    }

    public static void parseFileInfo(final DownloadLink link, final Map<String, Object> file_info) {
        final String filename = (String) file_info.get("filename");
        final Long size = JavaScriptEngineFactory.toLong(file_info.get("size"), -1);
        final String hash = (String) file_info.get("hash");
        final String privacy = (String) file_info.get("privacy");
        final String pass = (String) file_info.get("password_protected");
        final String delete_date = (String) file_info.get("delete_date");
        if (!StringUtils.isEmpty(filename)) {
            link.setFinalFileName(filename);
        }
        if (size != null && size >= 0) {
            link.setVerifiedFileSize(size);
        }
        if (!StringUtils.isEmpty(hash)) {
            link.setHashInfo(HashInfo.parse(hash));
        }
        if (StringUtils.equalsIgnoreCase(privacy, "public")) {
            /* Public file */
            link.removeProperty(PROPERTY_PRIVATE_FILE);
        } else {
            /* Possibly a private file */
            link.setProperty(PROPERTY_PRIVATE_FILE, true);
        }
        if (!StringUtils.isEmpty(pass) && PluginJSonUtils.parseBoolean(pass)) {
            link.setPasswordProtected(true);
        } else {
            link.setPasswordProtected(false);
        }
        /* 2020-06-29: Some files will have all information given bur are deleted if delete_date exists! */
        if (delete_date != null && delete_date.matches("\\d{4}-\\d{2}-\\d{2}.*")) {
            /**
             * For files parsed in context of a folder: </br>
             * We can't really be sure if the file is online until we actually try to download it but also in browser all files as part of
             * folders look to be online when viewing folders.
             */
            link.setAvailableStatus(AvailableStatus.FALSE);
        } else {
            link.setAvailableStatus(AvailableStatus.TRUE);
        }
    }

    private void handleNonAPIErrors(final DownloadLink link, final Browser br) throws PluginException, IOException {
        // error checking below!
        final String errorcodeStr = UrlQuery.parse(br.getURL()).get("errno");
        if (errorcodeStr != null && errorcodeStr.matches("\\d+")) {
            switch (Integer.parseInt(errorcodeStr)) {
            case 320:
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "File is removed by the originating user or MediaFire");
            case 323:
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Dangerous File Blocked");
            case 326:
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Dangerous File Identified by Google Safe Browsing");
            case 378:
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "File Removed for Violation (of TOS)");
            case 380:
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Claimed by a copyright holder through a valid DMCA request");
            case 382:
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "File Belongs to Suspended Account");
            case 386:
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "File Blocked for Violation");
            case 388:
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Identified as copyrighted work");
            case 394:
                /*
                 * The file you attempted to download is an archive that is encrypted or password protected. MediaFire does not support
                 * unlimited downloads of encrypted or password protected archives and the limit for this file has been reached. MediaFire
                 * understands the need for users to transfer encrypted and secured files, we offer this service starting at $1.50 per
                 * month. We have informed the owner that sharing of this file has been limited and how they can resolve this issue.
                 */
                throw new PluginException(LinkStatus.ERROR_FATAL, "Download not possible, retriction based on uploaders account");
            case 999:
                throw new PluginException(LinkStatus.ERROR_FATAL, "File has been set to private, only owner can download.");
            default:
                throw new PluginException(LinkStatus.ERROR_FATAL, "Unknown errorcode: " + errorcodeStr);
            }
        }
        /* Check for URL based errors */
        if (StringUtils.containsIgnoreCase(br.getURL(), "download_repair.php")) {
            /*
             * Very rare case e.g.
             * https://www.mediafire.com/download_repair.php?flag=9&dkey=REDACTED&qkey=REDACTED&ip=REDACTED&ref=https%3A%2F%2Fwww%
             * 2Emediafire%2Ecom%2F
             */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "We are generating a new download key for this file.", 5 * 60 * 1000l);
        }
        if (br.containsHTML("class=\"error\\-title\">\\s*Temporarily Unavailable\\s*</p>")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "This file is temporarily unavailable!", 30 * 60 * 1000l);
        } else if (br.containsHTML("class=\"error-title\"[^>]*>\\s*This download is currently unavailable\\s*<")) {
            final String time = br.getRegex("we will retry your download again in (\\d+) seconds\\.?\\s*<").getMatch(0);
            long t = ((time != null ? Long.parseLong(time) : 60) * 1000l) + 2;
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "This file is temporarily unavailable!", t);
        }
        final String ipLimitReachedWaitSecondsStr = getIPLimitReachedSecondsStr(br);
        if (ipLimitReachedWaitSecondsStr != null) {
            /**
             * E.g.
             * <h2 class="MFUltraDialog-heading">Download Threshold Exceeded<span id="count_down"></span></h2>
             */
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Downloadlimit reached", Long.parseLong(ipLimitReachedWaitSecondsStr) * 1000);
        }
    }

    private String getIPLimitReachedSecondsStr(final Browser br) {
        return br.getRegex("var limitReachedTTL = (\\d+);").getMatch(0);
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), FREE_TRIGGER_RECONNECT_ON_CAPTCHA, "Free downloads: Reconnect if captcha input needed?\r\n<html><p style=\"color:#F62817\"><b>WARNING: This setting can prevent captchas but it can also lead to an infinite reconnect loop!</b></p></html>").setDefaultValue(false));
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        /* Usually no captchas, not even for free downloads */
        return false;
    }
}