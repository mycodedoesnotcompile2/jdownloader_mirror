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

import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

import javax.swing.JComponent;
import javax.swing.JLabel;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.gui.swing.components.linkbutton.JLink;
import jd.http.Browser;
import jd.http.Cookies;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.NexusmodsComCrawler;

import org.appwork.storage.TypeRef;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtPasswordField;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.gui.InputChangedCallbackInterface;
import org.jdownloader.plugins.accounts.AccountBuilderInterface;

@HostPlugin(revision = "$Revision: 50510 $", interfaceVersion = 3, names = {}, urls = {})
public class NexusmodsCom extends PluginForHost {
    public NexusmodsCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://users.nexusmods.com/register/memberships");
    }

    @Override
    public String getAGBLink() {
        return "https://help.nexusmods.com/article/18-terms-of-service";
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static Browser prepBrAPI(final Browser br, final Account account) throws PluginException {
        br.getHeaders().put("User-Agent", "JDownloader");
        final String apikey = getApikey(account);
        br.getHeaders().put("apikey", apikey);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "nexusmods.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_NORMAL.pattern() + "|" + PATTERN_NEXUS_DOWNLOAD_MANAGER.pattern());
        }
        return ret.toArray(new String[0]);
    }

    private static final Pattern PATTERN_NORMAL                                = Pattern.compile("/Core/Libs/Common/Widgets/DownloadPopUp\\?id=(\\d+).+", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_NEXUS_DOWNLOAD_MANAGER                = Pattern.compile("nxm://([^/]+)/mods/(\\d+)/files/(\\d+)\\?key=([a-zA-Z0-9_/\\+\\=\\-%]+)\\&expires=(\\d+)\\&user_id=(\\d+)", Pattern.CASE_INSENSITIVE);
    /* API documentation: https://app.swaggerhub.com/apis-docs/NexusMods/nexus-mods_public_api_params_in_form_data/1.0 */
    public static final String   API_BASE                                      = "https://api.nexusmods.com/v1";
    public static final String   PROPERTY_game_domain_name                     = "game_domain_name";
    public static final String   PROPERTY_game_id                              = "game_id";
    public static final String   PROPERTY_mod_id                               = "mod_id";
    public static final String   PROPERTY_file_id                              = "file_id";
    private String               dllink                                        = null;
    private static final boolean WEBSITE_ACCOUNT_ALWAYS_NEEDED_FOR_DOWNLOADING = true;

    private String getContentURL(final DownloadLink link) {
        final String game_id = getGameID(link);
        final String game_domain_name = getGameDomainName(link);
        final String mod_id = this.getModID(link);
        final String file_id = this.getFileID(link);
        if (game_domain_name != null && mod_id != null && file_id != null) {
            return NexusmodsComCrawler.generateContentURL(game_domain_name, mod_id, file_id);
        } else {
            return NexusmodsComCrawler.generatePluginPatternMatcher(game_id, file_id);
        }
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String file_id = getFileID(link);
        if (file_id == null) {
            return super.getLinkID(link);
        }
        String linkid = getHost() + "://" + file_id;
        final String nxm_dlkey = new Regex(link.getPluginPatternMatcher(), PATTERN_NEXUS_DOWNLOAD_MANAGER).getMatch(3);
        if (nxm_dlkey != null) {
            linkid += "_" + nxm_dlkey;
        }
        return linkid;
    }

    /* URLs for their official downloadmanager --> We support them too --> Also see linkIsAPICompatible */
    private boolean isSpecialNexusModmanagerDownloadURL(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), PATTERN_NEXUS_DOWNLOAD_MANAGER).patternFind();
    }

    /** Returns the file_id (this comment is here as the URL contains multiple IDs) */
    private String getFileID(final DownloadLink link) {
        final String storedFileID = link.getStringProperty(PROPERTY_file_id);
        if (storedFileID != null) {
            return storedFileID;
        }
        String fileID = new Regex(link.getPluginPatternMatcher(), PATTERN_NORMAL).getMatch(0);
        if (fileID == null) {
            fileID = new Regex(link.getPluginPatternMatcher(), PATTERN_NEXUS_DOWNLOAD_MANAGER).getMatch(2);
        }
        return fileID;
    }

    private String getGameDomainName(final DownloadLink link) {
        final String storedGameDomainName = link.getStringProperty(PROPERTY_game_domain_name);
        if (storedGameDomainName != null) {
            return storedGameDomainName;
        }
        return new Regex(link.getPluginPatternMatcher(), PATTERN_NEXUS_DOWNLOAD_MANAGER).getMatch(0);
    }

    private String getGameID(final DownloadLink link) {
        final String storedGameID = link.getStringProperty(PROPERTY_game_id);
        if (storedGameID != null) {
            return storedGameID;
        }
        return new Regex(link.getPluginPatternMatcher(), "game_id=(\\d+)").getMatch(0);
    }

    private String getModID(final DownloadLink link) {
        final String storedModID = link.getStringProperty(PROPERTY_mod_id);
        if (storedModID != null) {
            return storedModID;
        }
        return new Regex(link.getPluginPatternMatcher(), PATTERN_NEXUS_DOWNLOAD_MANAGER).getMatch(1);
    }

    public static boolean isOfflineWebsite(final Browser br) {
        if (br.getHttpConnection().getResponseCode() == 404) {
            return true;
        } else if (br.containsHTML("No files have been uploaded yet|>\\s*File not found\\s*<|>\\s*Not found\\s*<|/noimage-1\\.png")) {
            return true;
        } else {
            return false;
        }
    }

    /** Account is either required because the files are 'too large' or also for 'adult files', or for 'no reason at all'. */
    public boolean isLoginRequiredWebsite(final Browser br) {
        if (br.containsHTML("<h1>\\s*Error\\s*</h1>") && br.containsHTML("<h2>\\s*Adult-only content\\s*</h2>")) {
            // adult only content.
            return true;
        } else if (br.containsHTML("You need to be a member and logged in to download files larger")) {
            // large files
            return true;
        } else if (br.containsHTML(">\\s*Please login or signup to download this file\\s*<")) {
            return true;
        } else if (WEBSITE_ACCOUNT_ALWAYS_NEEDED_FOR_DOWNLOADING) {
            return true;
        } else {
            return false;
        }
    }

    private void ensureDownloadable(final DownloadLink link) throws PluginException {
        if (new Regex(link.getPluginPatternMatcher(), PATTERN_NORMAL).patternFind() && this.getGameID(link) == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Invalid link");
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dllink = null;
        if (!link.isNameSet()) {
            link.setName(this.getFileID(link) + ".zip");
        }
        ensureDownloadable(link);
        final Account acc = AccountController.getInstance().getValidAccount(this.getHost());
        final String apikey = getApikey(acc);
        if (acc != null && apikey != null) {
            return requestFileInformationAPI(link, acc);
        } else {
            return requestFileInformationWebsite(link);
        }
    }

    private AvailableStatus requestFileInformationWebsite(final DownloadLink link) throws Exception {
        if (isSpecialNexusModmanagerDownloadURL(link)) {
            /* Cannot check here but let's assume the status by expire param */
            final long expireTimstamp = Long.parseLong(UrlQuery.parse(link.getPluginPatternMatcher()).get("expires")) * 1000;
            if (expireTimstamp < System.currentTimeMillis()) {
                return AvailableStatus.UNCHECKABLE;
            } else {
                final long validFor = expireTimstamp - System.currentTimeMillis();
                logger.info("nxm:// URL shall be valid for another: " + TimeFormatter.formatMilliSeconds(validFor, 1));
                return AvailableStatus.TRUE;
            }
        } else {
            br.getPage(getContentURL(link));
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            String filename = br.getRegex("<div class=\"header\"[^>]*>([^<]+)<").getMatch(0);
            if (filename != null) {
                filename = Encoding.htmlDecode(filename).trim();
                link.setName(filename);
            } else {
                logger.warning("Failed to find filename");
            }
            final String filesize = br.getRegex(">\\s*folder\\s*</i>\\s*<span>([^<]+)</span>").getMatch(0);
            if (filesize != null) {
                link.setDownloadSize(SizeFormatter.getSize(filesize));
            } else {
                logger.warning("Failed to find filesize");
            }
            return AvailableStatus.TRUE;
        }
    }

    private AvailableStatus requestFileInformationAPI(final DownloadLink link, final Account account) throws Exception {
        this.ensureDataNeededForAPIUsage(link);
        prepBrAPI(br, account);
        final String game_domain_name = getGameDomainName(link);
        final String mod_id = this.getModID(link);
        final String file_id = this.getFileID(link);
        if (file_id == null || mod_id == null || game_domain_name == null) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(API_BASE + String.format("/games/%s/mods/%s/files/%s", game_domain_name, mod_id, file_id));
        handleErrorsAPI(br);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        return setFileInformationAPI(link, entries, game_domain_name, mod_id, file_id);
    }

    /**
     * Typically, game_domain_name is needed for API requests while in the links added by the user, only game_id, mod_id and file_id is
     * given. </br> If needed, this function will find the additional data via website.
     */
    private void ensureDataNeededForAPIUsage(final DownloadLink link) throws IOException, PluginException {
        final String game_domain_name = getGameDomainName(link);
        final String mod_id = this.getModID(link);
        final String file_id = this.getFileID(link);
        if (game_domain_name != null && mod_id != null && file_id != null) {
            /* Needed data is already available */
            return;
        }
        final String game_id = getGameID(link);
        if (game_id == null) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        logger.info("Trying to find additional information to be able to obtain file information from API");
        final Browser brc = br.cloneBrowser();
        brc.setFollowRedirects(false);
        brc.getPage(NexusmodsComCrawler.generatePluginPatternMatcher(game_id, file_id));
        if (brc.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* We expect a redirect which will contain the required strings "game_domain_name" and "mod_id" in the redirect-URL. */
        final String redirect = brc.getRedirectLocation();
        if (redirect == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Regex urlinfo = new Regex(redirect, "(?i)https?://[^/]+/([^/]+)/mods/(\\d+)");
        if (!urlinfo.patternFind()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String game_domain_name_from_url = urlinfo.getMatch(0);
        final String mod_id_from_url = urlinfo.getMatch(1);
        link.setProperty(PROPERTY_game_domain_name, game_domain_name_from_url);
        link.setProperty(PROPERTY_mod_id, mod_id_from_url);
    }

    public static AvailableStatus setFileInformationAPI(final DownloadLink link, final Map<String, Object> entries, final String game_domain_name, final String mod_id, final String file_id) throws Exception {
        String filename = (String) entries.get("file_name");
        final Number size_in_bytes = (Number) entries.get("size_in_bytes");
        if (!StringUtils.isEmpty(filename)) {
            link.setFinalFileName(filename);
        } else {
            /* Fallback */
            filename = game_domain_name + "_" + mod_id + "_" + file_id;
            link.setName(filename);
        }
        if (size_in_bytes != null) {
            link.setVerifiedFileSize(size_in_bytes.longValue());
        }
        final String description = (String) entries.get("description");
        if (!StringUtils.isEmpty(description) && StringUtils.isEmpty(link.getComment())) {
            link.setComment(description);
        }
        final String external_virus_scan_url = (String) entries.get("external_virus_scan_url");
        if (external_virus_scan_url != null) {
            final String sha256_hash = new Regex(external_virus_scan_url, "(?i)virustotal\\.com/(?:gui/)?file/([a-f0-9]+)/.*").getMatch(0);
            if (sha256_hash != null) {
                link.setSha256Hash(sha256_hash);
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        handleDownload(link, null);
    }

    public int getMaxChunks(final Account account) {
        return 0;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Deprecated
    public void loginWebsite(final Account account) throws Exception {
        synchronized (account) {
            if (isAPIOnlyMode()) {
                /* This should never happen */
                throw new AccountInvalidException("Login with username + password is not supported!");
            }
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            boolean loggedIN = false;
            if (cookies != null) {
                br.setCookies(cookies);
                br.getPage("https://www." + getHost());
                if (!isLoggedinCookies()) {
                    logger.info("Existing login invalid: Full login required!");
                    br.clearCookies(getHost());
                } else {
                    loggedIN = true;
                }
            }
            if (!loggedIN) {
                br.getPage("https://users." + this.getHost() + "/auth/sign_in");
                final Form loginform = br.getFormbyKey("user%5Blogin%5D");
                if (loginform == null) {
                    logger.warning("Failed to find loginform");
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                String reCaptchaKey = br.getRegex("grecaptcha\\.execute\\('([^<>\"\\']+)'").getMatch(0);
                if (reCaptchaKey == null) {
                    /* 2020-01-08: Fallback */
                    reCaptchaKey = "6Lf4vsIUAAAAAN6TyJATjxQbMAcKjBZ3rOc0ijrp";
                }
                loginform.put("user%5Blogin%5D", Encoding.urlEncode(account.getUser()));
                loginform.put("user%5Bpassword%5D", Encoding.urlEncode(account.getPass()));
                final DownloadLink original = this.getDownloadLink();
                if (original == null) {
                    this.setDownloadLink(new DownloadLink(this, "Account", getHost(), "http://" + br.getRequest().getURL().getHost(), true));
                }
                try {
                    /* 2019-11-20: Invisible reCaptchaV2 is always required */
                    final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, reCaptchaKey).getToken();
                    if (recaptchaV2Response == null) {
                        throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                    }
                    loginform.put("g-recaptcha-response%5Blogin%5D", Encoding.urlEncode(recaptchaV2Response));
                } finally {
                    if (original == null) {
                        this.setDownloadLink(null);
                    }
                }
                br.submitForm(loginform);
                if (!isLoggedinCookies()) {
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, "Login failed.\r\nIf you own a premium account you should disable website login in Settings --> Plugin Settings --> nexusmods.com\r\nBe sure to delete your account and try again after changing this setting!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                }
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    private boolean isLoggedinCookies() {
        return br.getCookie(br.getURL(), "pass_hash", Cookies.NOTDELETEDPATTERN) != null && br.getCookie(br.getURL(), "member_id", Cookies.NOTDELETEDPATTERN) != null && br.getCookie(br.getURL(), "sid", Cookies.NOTDELETEDPATTERN) != null;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        String apikey = getApikey(account);
        if (apikey != null) {
            return fetchAccountInfoAPI(account);
        } else {
            /* Website mode */
            /* Old code! Website mode doesn't work anymore! */
            final AccountInfo ai = new AccountInfo();
            loginWebsite(account);
            br.getPage("/users/myaccount?tab=api%20access");
            /* Try to find apikey - prefer API */
            /* TODO: Maybe generate apikey is it is not yet available */
            /* 2019-11-19: Turned this off as it is nothing that we should do. */
            // Form requestApiForm = null;
            // final Form[] forms = br.getForms();
            // for (final Form tmpForm : forms) {
            // final InputField actionField = tmpForm.getInputFieldByName("action");
            // final InputField application_slugField = tmpForm.getInputFieldByName("application_slug");
            // if (actionField != null && actionField.getValue().equals("request-key") && application_slugField == null) {
            // logger.info("Found 'request apikey' Form");
            // requestApiForm = tmpForm;
            // break;
            // }
            // }
            // if (requestApiForm != null) {
            // logger.info("Requesting apikey for the first time ...");
            // this.submitForm(requestApiForm);
            // }
            apikey = br.getRegex("id=\"personal_key\"[^>]*>([^<>\"]+)<").getMatch(0);
            if (apikey != null) {
                /* TODO: Consider removing original login data once we found an apikey for safety reasons! */
                logger.info("Found apikey");
                saveApikey(account, apikey);
                return fetchAccountInfoAPI(account);
            } else {
                logger.info("Failed to find apikey - continuing via website");
                br.getPage("/users/myaccount");
                if (StringUtils.equalsIgnoreCase(br.getRegex("\"premium-desc\">\\s*(.*?)\\s*<").getMatch(0), "Inactive")) {
                    account.setType(AccountType.FREE);
                    account.setMaxSimultanDownloads(this.getMaxSimultanFreeDownloadNum());
                    account.setConcurrentUsePossible(false);
                } else {
                    account.setType(AccountType.PREMIUM);
                    account.setMaxSimultanDownloads(this.getMaxSimultanPremiumDownloadNum());
                    account.setConcurrentUsePossible(true);
                }
                return ai;
            }
        }
    }

    private AccountInfo fetchAccountInfoAPI(final Account account) throws Exception {
        if (isAPIOnlyMode()) {
            account.setPass(account.getPass());
        }
        if (!isAPIKey(getApikey(account))) {
            throw new AccountInvalidException("Invalid API key format");
        }
        prepBrAPI(br, account);
        br.getPage(API_BASE + "/users/validate.json");
        handleErrorsAPI(br);
        final Map<String, Object> user = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String email = (String) user.get("email");
        if (!StringUtils.isEmpty(email)) {
            /* User can enter whatever he wants into username field in JDownloader but we want unique usernames. */
            account.setUser(email);
        }
        final AccountInfo ai = new AccountInfo();
        if ((Boolean) user.get("is_premium") == Boolean.TRUE) {
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(this.getMaxSimultanPremiumDownloadNum());
            account.setConcurrentUsePossible(true);
            ai.setUnlimitedTraffic();
            ai.setStatus("Premium user");
        } else if ((Boolean) user.get("is_supporter") == Boolean.TRUE) {
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(this.getMaxSimultanPremiumDownloadNum());
            account.setConcurrentUsePossible(true);
            ai.setUnlimitedTraffic();
            ai.setStatus("Supporter");
        } else {
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(this.getMaxSimultanPremiumDownloadNum());
            account.setConcurrentUsePossible(false);
            if (isAPIOnlyMode()) {
                ai.setStatus("Free user [Only pre generated nxm:// URLs can be downloaded]");
                ai.setUnlimitedTraffic();
            } else {
                ai.setStatus("Free user");
                ai.setTrafficLeft(0);
            }
        }
        return ai;
    }

    public static void handleErrorsAPI(final Browser br) throws PluginException {
        if (br.getHttpConnection().getResponseCode() == 400) {
            /*
             * 2020-01-15: Attempted free account download fails:
             * {"code":400,"message":"Provided key and expire time isn't correct for this user/file."}
             */
            throw new AccountRequiredException("Error 400: nxm URL expired");
        } else if (br.getHttpConnection().getResponseCode() == 401) {
            /* {"message":"Please provide a valid API Key"} */
            throw new AccountInvalidException("Invalid or expired API Key");
        } else if (br.getHttpConnection().getResponseCode() == 403) {
            /*
             * According to API documentation, this may happen if we try to download a file via API with a free account (downloads are only
             * possible via website!)
             */
            /*
             * {"code":403, "message":
             * "You don't have permission to get download links from the API without visting nexusmods.com - this is for premium users only."
             * }
             */
            throw new AccountRequiredException();
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            /* {"error":"File ID '12345' not found"} */
            /* {"code":404,"message":"No Game Found: xskyrimspecialedition"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 429) {
            /* {"msg":"You have fired too many requests. Please wait for some time."} */
            /* TODO: Maybe check which limit ends first (daily / hourly) to display an even more precise waittime! */
            String reset_date = br.getRequest().getResponseHeader("X-RL-Hourly-Reset").toString();
            long waitMillis = 30 * 60 * 1000l;
            if (reset_date != null) {
                /* Try to find the exact waittime */
                reset_date = reset_date.substring(0, reset_date.lastIndexOf(":")) + "00";
                final long reset_timestamp = TimeFormatter.getMilliSeconds(reset_date, "yyyy-MM-dd'T'HH:mm:ssZ", Locale.ENGLISH);
                final long waittime_until_reset = reset_timestamp - System.currentTimeMillis();
                if (waittime_until_reset > 0) {
                    /* Wait exact waittime + 5 extra seconds */
                    waitMillis = waittime_until_reset + 5000l;
                }
            }
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "API limit has been reached", waitMillis);
        }
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload(link, account);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        ensureDownloadable(link);
        if (getApikey(account) != null) {
            this.ensureDataNeededForAPIUsage(link);
            prepBrAPI(br, account);
            /* We do not have to perform an extra onlinecheck - if the file is offline, the download request will return 404. */
            // requestFileInformationAPI(link);
            if (account.getType() == AccountType.FREE && !this.isSpecialNexusModmanagerDownloadURL(link)) {
                // logger.info("Only premium account download is possible as information for free account download is missing");
                throw new AccountRequiredException("Free Account users can only download 'nxm://' URLs!");
            }
            final String game_domain_name = link.getStringProperty(PROPERTY_game_domain_name);
            final String mod_id = this.getModID(link);
            final String file_id = this.getFileID(link);
            if (file_id == null || mod_id == null || game_domain_name == null) {
                /* This should never happen */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            String url = API_BASE + String.format("/games/%s/mods/%s/files/%s/download_link.json", game_domain_name, mod_id, file_id);
            if (account.getType() == AccountType.FREE) {
                final UrlQuery query = UrlQuery.parse(link.getPluginPatternMatcher());
                final String dlExpires = query.get("expires");
                if (Long.parseLong(dlExpires) * 1000 < System.currentTimeMillis()) {
                    /* Do not use LinkStatus FILE_NOT_FOUND here because we can be pretty sure that this file is online! */
                    throw new PluginException(LinkStatus.ERROR_FATAL, "This downloadurl has expired");
                }
                url += "?key=" + query.get("key") + "&expires=" + dlExpires;
            }
            br.getPage(url);
            handleErrorsAPI(br);
            final List<Map<String, Object>> mirrors = (List<Map<String, Object>>) restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
            if (mirrors == null || mirrors.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unable to find any downloadable mirrors");
            }
            /*
             * First element = User preferred mirror. Users can set their preferred mirror here: https://www.nexusmods.com/users/myaccount
             * --> Premium membership preferences
             */
            final Map<String, Object> mirror = mirrors.get(0);
            final String mirrorName = (String) mirror.get("name");
            this.dllink = (String) mirror.get("URI");
            if (StringUtils.isEmpty(this.dllink)) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unable to find download link for mirror: " + mirrorName);
            }
            logger.info("Selected random mirror: " + mirrorName);
        } else {
            /* Website handling. Important! Login before requestFileInformation! */
            loginWebsite(account);
            requestFileInformation(link);
        }
        /* Free- and premium download handling is the same. */
        if (StringUtils.isEmpty(dllink)) {
            if (this.isLoginRequiredWebsite(br)) {
                if (account == null) {
                    throw new AccountRequiredException();
                } else {
                    /*
                     * 2019-01-23: Added errorhandling but this should never happen because if an account exists we should be able to
                     * download!
                     */
                    throw new AccountUnavailableException("Session expired?", 5 * 60 * 1000l);
                }
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, this.isResumeable(link, account), getMaxChunks(account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (this.isSpecialNexusModmanagerDownloadURL(link)) {
                /* Most likely that downloadurl is expired so user has to delete- and re-add it! */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "URL expired?");
            } else {
                handleConnectionErrors(br, dl.getConnection());
            }
        }
        dl.startDownload();
    }

    public static String getApikey(final Account account) {
        if (account == null) {
            return null;
        } else if (isAPIOnlyMode()) {
            return account.getPass();
        } else {
            return account.getStringProperty("apikey");
        }
    }

    private void saveApikey(final Account account, final String apikey) {
        account.setProperty("apikey", apikey);
    }

    private static boolean isAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("[A-Za-z0-9\\-=/\\+_]{64,}")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public AccountBuilderInterface getAccountFactory(final InputChangedCallbackInterface callback) {
        if (isAPIOnlyMode()) {
            /* API login */
            return new NexusmodsAccountFactory(callback);
        } else {
            /* Website login */
            return super.getAccountFactory(callback);
        }
    }

    public static class NexusmodsAccountFactory extends MigPanel implements AccountBuilderInterface {
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        private final JLabel      apikeyLabel;

        private String getPassword() {
            if (this.pass == null) {
                return null;
            } else {
                return new String(this.pass.getPassword());
            }
        }

        public boolean updateAccount(Account input, Account output) {
            if (!StringUtils.equals(input.getUser(), output.getUser())) {
                output.setUser(input.getUser());
                return true;
            } else if (!StringUtils.equals(input.getPass(), output.getPass())) {
                output.setPass(input.getPass());
                return true;
            } else {
                return false;
            }
        }

        private final ExtPasswordField pass;

        public NexusmodsAccountFactory(final InputChangedCallbackInterface callback) {
            super("ins 0, wrap 2", "[][grow,fill]", "");
            add(new JLabel("Where can I find my API key?"));
            add(new JLink("Click here", "https://www.nexusmods.com/users/myaccount?tab=api%20access"));
            add(apikeyLabel = new JLabel("<html><u><b>Personal</b></u> API Key [premium accounts only]:</html>"));
            add(this.pass = new ExtPasswordField() {
                @Override
                public void onChanged() {
                    callback.onChangedInput(this);
                }
            }, "");
            pass.setHelpText("Enter your API Key");
        }

        @Override
        public JComponent getComponent() {
            return this;
        }

        @Override
        public void setAccount(Account defaultAccount) {
            if (defaultAccount != null) {
                // name.setText(defaultAccount.getUser());
                pass.setText(defaultAccount.getPass());
            }
        }

        @Override
        public boolean validateInputs() {
            final String pw = getPassword();
            if (NexusmodsCom.isAPIKey(pw)) {
                apikeyLabel.setForeground(Color.BLACK);
                return true;
            } else {
                apikeyLabel.setForeground(Color.RED);
                return false;
            }
        }

        @Override
        public Account getAccount() {
            return new Account(null, getPassword());
        }
    }

    private static boolean isAPIOnlyMode() {
        // final NexusmodsConfigInterface cfg = PluginJsonConfig.get(NexusmodsCom.NexusmodsConfigInterface.class);
        // return !cfg.isEnableWebsiteMode();
        /* 2020-01-15: Website login is broken, downloads are only possible via free account */
        return true;
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://www.nexusmods.com/users/myaccount?tab=api%20access";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("[A-Za-z0-9\\-=/\\+_]{64,}")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (WEBSITE_ACCOUNT_ALWAYS_NEEDED_FOR_DOWNLOADING && account == null) {
            /* Downloads without account are not possible */
            return false;
        } else if (account.getType() != AccountType.PREMIUM && !isSpecialNexusModmanagerDownloadURL(link)) {
            /* Free account users can only download special URLs which contain authorization information. */
            return false;
        } else {
            return super.canHandle(link, account);
        }
    }

    @Override
    public void reset() {
    }

}