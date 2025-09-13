//jDownloader - Downloadmanager
//Copyright (C) 2010  JD-Team support@jdownloader.org
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
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.http.Browser;
import jd.http.Cookie;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.decrypter.PCloudComFolder;
import jd.utils.locale.JDL;

@HostPlugin(revision = "$Revision: 51494 $", interfaceVersion = 2, names = {}, urls = {})
@PluginDependencies(dependencies = { PCloudComFolder.class })
public class PCloudCom extends PluginForHost {
    @SuppressWarnings("deprecation")
    public PCloudCom(PluginWrapper wrapper) {
        super(wrapper);
        setConfigElements();
        this.enablePremium("https://my." + getHost() + "/#page=register");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    private static List<String[]> getPluginDomains() {
        return PCloudComFolder.getPluginDomains();
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
            /* No regex needed */
            ret.add("");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        PCloudComFolder.prepBR(br);
        return br;
    }

    @Override
    public String getPluginContentURL(final DownloadLink link) {
        String folderid = null;
        try {
            folderid = this.getFolderID(link);
        } catch (final PluginException ignore) {
        }
        final String parentfolderid = link.getStringProperty("plain_parentfolderid");
        final String sourceurl = link.getStringProperty("mainlink");
        if (folderid != null && parentfolderid != null) {
            return "https://u.pcloud.link/publink/show?code=" + folderid + "#folder=" + parentfolderid + "&tpl=publicfoldergrid";
        } else if (sourceurl != null) {
            return sourceurl;
        } else {
            return super.getPluginContentURL(link);
        }
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        String id = null;
        try {
            final String fileid = getFID(link);
            final String folderID = getFolderID(link);
            if (fileid != null) {
                id = folderID + "_" + fileid;
            }
        } catch (final Throwable e) {
        }
        if (id != null) {
            return "pcloud://" + id;
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public String getAGBLink() {
        return "https://my." + getHost() + "/#page=policies&tab=terms-of-service";
    }

    /* Plugin Settings */
    private static final String DOWNLOAD_ZIP                                    = "DOWNLOAD_ZIP_2";
    private static final String MOVE_FILES_TO_ACCOUNT                           = "MOVE_FILES_TO_ACCOUNT";
    private static final String DELETE_FILE_AFTER_DOWNLOADLINK_CREATION         = "DELETE_FILE_AFTER_DOWNLOADLINK_CREATION";
    private static final String DELETE_FILE_FOREVER_AFTER_DOWNLOADLINK_CREATION = "DELETE_FILE_FOREVER_AFTER_DOWNLOADLINK_CREATION";
    private static final String EMPTY_TRASH_AFTER_DOWNLOADLINK_CREATION         = "EMPTY_TRASH_AFTER_DOWNLOADLINK_CREATION";
    /* Errorcodes */
    private static final int    STATUS_CODE_OKAY                                = 0;
    public static final int     STATUS_CODE_DOWNLOAD_PASSWORD_INVALID           = 1125;
    private static final int    STATUS_CODE_INVALID_LOGIN                       = 2000;
    private static final int    STATUS_CODE_MAYBE_OWNER_ONLY                    = 2003;
    public static final int     STATUS_CODE_DOWNLOAD_PASSWORD_REQUIRED          = 2258;
    private static final int    STATUS_CODE_WRONG_LOCATION                      = 2321;
    private static final int    STATUS_CODE_PREMIUMONLY                         = 7005;
    /* Account properties */
    private static final String PROPERTY_ACCOUNT_API_HOST                       = "account_api";
    private static final String PROPERTY_ACCOUNT_AUTH_TOKEN                     = "account_auth";
    private static final String PROPERTY_ACCOUNT_DEVICE_ID                      = "account_device_id";

    public static String getAPIDomain(final String linkDomain) {
        if (linkDomain.matches("(?i).*e\\d*\\.pcloud\\.(com|link)")) {
            // europe data center
            return "eapi.pcloud.com";
        } else {
            // us data center
            return "api.pcloud.com";
        }
    }

    public static String getAPIDomain(final DownloadLink link) throws Exception {
        final String mainLink = link.getStringProperty("mainlink");
        if (mainLink != null) {
            return getAPIDomain(new URL(mainLink).getHost());
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (this.isCompleteFolder(link)) {
            return false;
        } else {
            return true;
        }
    }

    private int getMaxChunks(final DownloadLink link, final Account account) {
        if (this.isCompleteFolder(link)) {
            return 1;
        } else {
            return 0;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, Exception {
        return requestFileInformation(link, null);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws IOException, Exception {
        this.setBrowserExclusive();
        final String filename = link.getStringProperty("plain_name");
        final String filesize = link.getStringProperty("plain_size");
        if (filename == null || filesize == null) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        link.setFinalFileName(filename);
        link.setDownloadSize(Long.parseLong(filesize));
        getDownloadURL(link, account);
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String directLinkProperty = account != null ? "account_dllink" : "free_dllink";
        final String storedDirectlink = link.getStringProperty(directLinkProperty);
        String downloadURL = null;
        if (storedDirectlink != null) {
            logger.info("Attempting to re-use stored directurl: " + storedDirectlink);
            downloadURL = storedDirectlink;
        } else {
            downloadURL = getDownloadURL(link, account);
            if (downloadURL == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, downloadURL, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            link.setProperty(directLinkProperty, dl.getConnection().getURL().toExternalForm());
        } catch (final Exception e) {
            if (storedDirectlink != null) {
                link.removeProperty(directLinkProperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        dl.startDownload();
    }

    private String getDownloadURL(final DownloadLink link, final Account account) throws Exception {
        final String folderID = getFolderID(link);
        String account_auth = null;
        String account_api = null;
        if (account != null) {
            synchronized (account) {
                account_auth = login(account, false);
                account_api = account.getStringProperty(PROPERTY_ACCOUNT_API_HOST);
                if (account_api == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
        if (isCompleteFolder(link)) {
            if (account_auth != null) {
                br.getPage("https://" + getAPIDomain(link) + "/showpublink?code=" + folderID + "&auth=" + account_auth);
            } else {
                br.getPage("https://" + getAPIDomain(link) + "/showpublink?code=" + folderID);
            }
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            handleAPIErrors(br);
            /* Select all IDs of the folder to download all as .zip */
            final String[] fileids = br.getRegex("\"fileid\": (\\d+)").getColumn(0);
            if (fileids == null || fileids.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            String dllink = "https://" + getAPIDomain(link) + "/getpubzip?fileids=";
            for (int i = 0; i < fileids.length; i++) {
                final String currentID = fileids[i];
                if (i == fileids.length - 1) {
                    dllink += currentID;
                } else {
                    dllink += currentID + "%2C";
                }
            }
            dllink += "&filename=" + link.getStringProperty("plain_name", null) + "&code=" + folderID;
            if (account_auth != null) {
                dllink += "&auth=" + account_auth;
            }
            return dllink;
        } else {
            final String fileID = getFID(link);
            if (StringUtils.equals(account_api, getAPIDomain(link)) && this.getPluginConfig().getBooleanProperty(MOVE_FILES_TO_ACCOUNT, defaultMOVE_FILES_TO_ACCOUNT)) {
                /*
                 * Only possible to copy files on same data center region!
                 *
                 * not yet implemented for complete folder(zip)
                 */
                /* tofolderid --> 0 = root */
                final String fileid = getFID(link);
                getAPISafe("https://" + getAPIDomain(link) + "/copypubfile?fileid=" + fileid + "&tofolderid=0&code=" + folderID + "&auth=" + account_auth);
                final String new_fileid = PluginJSonUtils.getJsonValue(br, "fileid");
                final String new_hash = PluginJSonUtils.getJsonValue(br, "hash");
                final String api_filename = PluginJSonUtils.getJsonValue(br, "name");
                if (new_fileid == null || new_hash == null || api_filename == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                getAPISafe("/getfilelink?fileid=" + new_fileid + "&hashCache=" + new_hash + "&forcedownload=1&auth=" + account_auth);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final List<Object> ressourcelist = (List) entries.get("hosts");
                final String path = PluginJSonUtils.getJsonValue(br, "path");
                if (ressourcelist == null || path == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String download_host = (String) ressourcelist.get(new Random().nextInt(ressourcelist.size() - 1));
                final String directurl = "https://" + download_host + path;
                if (this.getPluginConfig().getBooleanProperty(DELETE_FILE_AFTER_DOWNLOADLINK_CREATION, defaultDELETE_DELETE_FILE_AFTER_DOWNLOADLINK_CREATION)) {
                    /*
                     * It sounds crazy but we'll actually delete the file before we download it as the directlink will still be valid and
                     * this way we avoid filling up the space of our account.
                     */
                    getAPISafe("/deletefile?fileid=" + new_fileid + "&name=" + Encoding.urlEncode(api_filename) + "&id=000-0&auth=" + account_auth);
                    if (this.getPluginConfig().getBooleanProperty(DELETE_FILE_FOREVER_AFTER_DOWNLOADLINK_CREATION, defaultDELETE_FILE_FOREVER_AFTER_DOWNLOADLINK_CREATION)) {
                        /* Delete file inside trash (FOREVER) in case user wants that. */
                        getAPISafe("/trash_clear?fileid=" + new_fileid + "&id=000-0&auth=" + account_auth);
                    }
                }
                if (this.getPluginConfig().getBooleanProperty(EMPTY_TRASH_AFTER_DOWNLOADLINK_CREATION, defaultEMPTY_TRASH_AFTER_DOWNLOADLINK_CREATION)) {
                    /* Let's empty the trash in case the user wants this. */
                    getAPISafe("/trash_clear?folderid=0&auth=" + account_auth);
                }
                return directurl;
            }
            final UrlQuery query = new UrlQuery();
            query.add("code", folderID);
            query.add("forcedownload", "1");
            query.add("fileid", fileID);
            if (account_auth != null) {
                query.add("auth", account_auth);
            }
            String passCode = link.getDownloadPassword();
            Map<String, Object> resp = null;
            int passwordAttempts = 0;
            do {
                if (passCode != null) {
                    query.addAndReplace("linkpassword", Encoding.urlEncode(passCode));
                }
                br.getPage("https://" + getAPIDomain(link) + "/getpublinkdownload?" + query.toString());
                resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final int errorcode = ((Number) resp.get("result")).intValue();
                if (errorcode == STATUS_CODE_MAYBE_OWNER_ONLY && account_auth != null) {
                    br.getPage("/getfilelink?" + query.toString());
                    resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                }
                if (br.getHttpConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                final int statusCode = ((Number) resp.get("result")).intValue();
                if (statusCode == STATUS_CODE_DOWNLOAD_PASSWORD_REQUIRED || statusCode == STATUS_CODE_DOWNLOAD_PASSWORD_INVALID) {
                    logger.info("Password invalid/required | password = " + passCode);
                    if (passwordAttempts >= 3) {
                        link.setDownloadPassword(null);
                        throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
                    }
                    link.setPasswordProtected(true);
                    passCode = getUserInput("Password?", link);
                    passwordAttempts++;
                    continue;
                } else {
                    if (passCode == null) {
                        link.setPasswordProtected(false);
                    }
                    break;
                }
            } while (!this.isAbort());
            if (passCode != null) {
                /* Save for later */
                link.setDownloadPassword(passCode);
            }
            final List<String> hosts = (List<String>) resp.get("hosts");
            String dllink = resp.get("path").toString();
            if (dllink == null || hosts == null || hosts == null || hosts.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dllink = "https://" + hosts.get(new Random().nextInt(hosts.size() - 1)) + dllink;
            return dllink;
        }
    }

    private boolean isCompleteFolder(final DownloadLink link) {
        return link.getBooleanProperty("complete_folder", false);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    private String login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            /* Re-use device_id once one has been stored to this account. */
            String device_id = account.getStringProperty(PROPERTY_ACCOUNT_DEVICE_ID);
            if (device_id == null) {
                device_id = generateDeviceID();
            }
            final String location_1_us = "api.pcloud.com";
            final String location_2_eu = "eapi.pcloud.com";
            final Cookies cookies = account.loadCookies("");
            final Cookies userCookies = account.loadUserCookies();
            String auth_token = account.getStringProperty(PROPERTY_ACCOUNT_AUTH_TOKEN);
            String account_api_host = account.getStringProperty(PROPERTY_ACCOUNT_API_HOST, location_1_us);
            if (userCookies != null) {
                /* Validate user cookies */
                final Cookie auth_token_cookie = userCookies.get("pcauth");
                final Cookie locationid = userCookies.get("locationid");
                if (auth_token_cookie == null || StringUtils.isEmpty(auth_token_cookie.getValue()) || locationid == null || StringUtils.isEmpty(locationid.getValue())) {
                    logger.info("Users' cookies are incomplete or invalid!");
                    throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                }
                auth_token = auth_token_cookie.getValue();
                if ("1".equals(locationid.getValue())) {
                    account_api_host = location_1_us;
                } else {
                    account_api_host = location_2_eu;
                }
                /* Save data as property so it can be used outside this function without the need to access/parse users' cookies again. */
                account.setProperty(PROPERTY_ACCOUNT_API_HOST, account_api_host);
                account.setProperty(PROPERTY_ACCOUNT_AUTH_TOKEN, auth_token);
            }
            if ((cookies != null || userCookies != null) && !StringUtils.isEmpty(auth_token)) {
                if (userCookies != null) {
                    br.setCookies(userCookies);
                } else {
                    br.setCookies(cookies);
                }
                if (!force) {
                    logger.info("Trust token without checking");
                    return auth_token;
                }
                br.getPage("https://" + account_api_host + "/userinfo?auth=" + Encoding.urlEncode(auth_token) + "&getlastsubscription=1");
                try {
                    final Map<String, Object> resp = this.handleAPIErrors(br);
                    logger.info("Token login successful");
                    updateAccountInfo(account, resp);
                    if (userCookies != null) {
                        /*
                         * Ensure that we have unique usernames since for cookie login, user could enter another/wrong e-mail into username
                         * field.
                         */
                        final String email = (String) resp.get("email");
                        if (email != null) {
                            account.setUser(email);
                        }
                    }
                    return auth_token;
                } catch (final PluginException e) {
                    /* Wrong token = Will typically fail with errorcode 2000 */
                    logger.info("Token login failed");
                    logger.log(e);
                    br.clearCookies(null);
                    if (userCookies != null) {
                        if (account.hasEverBeenValid()) {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                        } else {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                        }
                    }
                }
            }
            logger.info("Performing full login");
            /* Depending on which selection the user met when he registered his account, a different endpoint is required for login. */
            logger.info("Trying to login via US-API endpoint");
            final UrlQuery query1 = new UrlQuery();
            query1.appendEncoded("os", "4");
            query1.appendEncoded("language", "en");
            query1.appendEncoded("cannotusegooglelogin", "false");
            query1.appendEncoded("cannotuseapplelogin", "false");
            query1.appendEncoded("cannotusefacebooklogin", "false");
            query1.appendEncoded("getlogins", "1");
            query1.appendEncoded("email", account.getUser());
            final String path = "/user/preparelogin";
            br.getPage("https://" + location_1_us + path + "?" + query1.toString());
            Map<String, Object> resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final int statusCode = ((Number) resp.get("result")).intValue();
            if (statusCode == STATUS_CODE_WRONG_LOCATION || statusCode == STATUS_CODE_INVALID_LOGIN) {
                logger.info("Trying to login via EU-API endpoint");
                br.getPage("https://" + location_2_eu + path + "?" + query1.toString());
                resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                this.handleAPIErrors(resp);
            }
            final boolean captchaRequired = ((Boolean) resp.get("usegrecaptcha")).booleanValue();
            final List<Map<String, Object>> logins = (List<Map<String, Object>>) resp.get("logins");
            boolean requiresTOTPEmailLogin = true;
            if (logins != null) {
                /* TODO: Evaluate all 4 different types of logins. */
                for (final Map<String, Object> login : logins) {
                    final int type = ((Number) login.get("type")).intValue();
                    if (type == 4 && Boolean.TRUE.equals(login.get("preferred"))) {
                        requiresTOTPEmailLogin = false;
                        break;
                    }
                }
            }
            String twoFACode = null;
            if (requiresTOTPEmailLogin) {
                final UrlQuery query2 = new UrlQuery();
                query2.appendEncoded("email", account.getUser());
                query2.appendEncoded("os", "4");
                query2.appendEncoded("language", "en");
                br.getPage("/user/sendemailtfamail?" + query2.toString());
                resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                this.handleAPIErrors(resp);
                twoFACode = this.getTwoFACode(account, "\\d{8}");
                final UrlQuery query3 = new UrlQuery();
                query3.appendEncoded("email", account.getUser());
                query3.appendEncoded("code", twoFACode);
                br.getPage("/user/checkemailtfacode?" + query3.toString());
                resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                this.handleAPIErrors(resp);
                /* TOTP success -> Next step */
                query1.appendEncoded("emailtfacode", twoFACode);
                br.getPage("/user/preparelogin?" + query1.toString());
                resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                this.handleAPIErrors(resp);
            }
            /* Final step: send password */
            final UrlQuery query4 = new UrlQuery();
            query4.appendEncoded("username", account.getUser());
            query4.appendEncoded("password", account.getPass());
            query4.appendEncoded("deviceid", device_id);
            query4.appendEncoded("language", "en");
            if (twoFACode != null) {
                query4.appendEncoded("emailtfacode", twoFACode);
            }
            query4.appendEncoded("_t", Long.toString(System.currentTimeMillis()));
            query4.appendEncoded("logout", "1");
            query4.appendEncoded("getlastsubscription", "1");
            query4.appendEncoded("promoinfo", "1");
            query4.appendEncoded("os", "4");
            query4.appendEncoded("osversion", "0.0.0");
            if (captchaRequired) {
                /* Obtain captcha token for login */
                final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, "6Ld3iconAAAAAMeIhfk_3jTdIPSNX_OcSY6QlvZR") {
                    @Override
                    public TYPE getType() {
                        return TYPE.INVISIBLE;
                    }
                }.getToken();
                query4.appendEncoded("gresponse", recaptchaV2Response);
            }
            br.postPage("/login", query4);
            resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            this.handleAPIErrors(resp);
            updateAccountInfo(account, resp);
            auth_token = resp.get("auth").toString();
            if (StringUtils.isEmpty(auth_token)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            br.setCookie(br.getHost(), "pcauth", auth_token);
            account_api_host = br.getHost(true);
            getAPISafe("/userinfo?auth=" + Encoding.urlEncode(auth_token) + "&getlastsubscription=1");
            account.setProperty(PROPERTY_ACCOUNT_API_HOST, account_api_host);
            account.setProperty(PROPERTY_ACCOUNT_AUTH_TOKEN, auth_token);
            account.setProperty(PROPERTY_ACCOUNT_DEVICE_ID, device_id);
            account.saveCookies(br.getCookies(br.getHost()), "");
            return auth_token;
        }
    }

    public static String generateDeviceID() {
        String characters = "abcdefghijklmnopqrstuvwxyz0123456789";
        StringBuilder token = new StringBuilder(30);
        java.util.Random random = new java.util.Random();
        for (int i = 0; i < 30; i++) {
            int index = random.nextInt(characters.length());
            token.append(characters.charAt(index));
        }
        return token.toString();
    }

    private void updateAccountInfo(final Account account, final Map<String, Object> resp) throws PluginException {
        if (Boolean.FALSE.equals(resp.get("emailverified"))) {
            if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                throw new AccountInvalidException("\r\nDein Account ist noch nicht verifiziert!\r\nPrüfe deine E-Mails und verifiziere deinen Account!");
            } else {
                throw new AccountInvalidException("\r\nYour account is not yet verified!\r\nCheck your mails and verify it!");
            }
        }
        if (Boolean.TRUE.equals(resp.get("premiumlifetime"))) {
            account.setType(AccountType.LIFETIME);
            account.setMaxSimultanDownloads(20);
        } else if (Boolean.TRUE.equals(resp.get("premium"))) {
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(20);
        } else {
            account.setType(AccountType.FREE);
            /* Last checked: 2020-10-06 */
            account.setMaxSimultanDownloads(this.getMaxSimultanFreeDownloadNum());
        }
        account.setConcurrentUsePossible(true);
        // final Number quota = (Number) resp.get("quota");
        // if (quota != null) {
        // }
        // final Number usedquota_bytes = (Number) resp.get("usedquota");
        // if (usedquota_bytes != null) {
        // account.getAccountInfo().setUsedSpace(usedquota_bytes.longValue());
        // }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload(link, account);
    }

    private String getFolderID(final DownloadLink dl) throws PluginException {
        final String ret = dl.getStringProperty("plain_code");
        if (ret == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            return ret;
        }
    }

    private String getFID(final DownloadLink dl) throws PluginException {
        final String ret = dl.getStringProperty("plain_fileid");
        if (ret == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            return ret;
        }
    }

    private Map<String, Object> getAPISafe(final String accesslink) throws IOException, PluginException {
        br.getPage(accesslink);
        return handleAPIErrors(this.br);
    }

    private Map<String, Object> postAPISafe(final String accesslink, final String postdata) throws IOException, PluginException {
        br.postPage(accesslink, postdata);
        return handleAPIErrors(this.br);
    }

    private Map<String, Object> handleAPIErrors(final Browser br) throws PluginException {
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        return handleAPIErrors(entries);
    }

    private Map<String, Object> handleAPIErrors(final Map<String, Object> entries) throws PluginException {
        final int statusCode = ((Number) entries.get("result")).intValue();
        final String errormessage = (String) entries.get("error");
        try {
            switch (statusCode) {
            case STATUS_CODE_OKAY:
                /* Everything ok */
                break;
            case 1029:
                throw new AccountRequiredException();
            case STATUS_CODE_DOWNLOAD_PASSWORD_INVALID:
                throw new PluginException(LinkStatus.ERROR_RETRY, "Download password invalid");
            case STATUS_CODE_INVALID_LOGIN:
                throw new AccountInvalidException(errormessage);
            case 2008:
                /* Account is out of storage space */
                throw new AccountInvalidException(errormessage);
            case 2009:
                /* { "result": 2009, "error": "File not found."} */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            case 2012:
                /* Invalid email TOTP code */
                throw new AccountInvalidException(errormessage);
            case STATUS_CODE_DOWNLOAD_PASSWORD_REQUIRED:
                throw new PluginException(LinkStatus.ERROR_RETRY, "Download password required");
            case STATUS_CODE_WRONG_LOCATION:
                // wrong location
                /*
                 * {"result": 2321, "error": "This user is on another location.", "location": { "label": "US", "id": 1, "binapi":
                 * "binapi.pcloud.com", "api": "api.pcloud.com" }}
                 */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            case 5002:
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server error 'Internal error, no servers available. Try again later.'", 5 * 60 * 1000l);
            case 7002:
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            case STATUS_CODE_PREMIUMONLY:
                throw new AccountRequiredException();
            case STATUS_CODE_MAYBE_OWNER_ONLY:
                /* file might be set to preview only download */
                /* "error": "Access denied. You do not have permissions to perform this operation." */
                throw new AccountRequiredException(errormessage);
            case 7014:
                /*
                 * 2016-08-31: Added support for this though I'm not sure about this - I guess some kind of account traffic limit has been
                 * reached!
                 */
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
            default:
                /* Unknown error */
                throw new PluginException(LinkStatus.ERROR_FATAL, errormessage);
            }
        } catch (final PluginException e) {
            logger.info(this.getHost() + ": Exception: statusCode: " + statusCode + " statusMessage: " + errormessage);
            throw e;
        }
        return entries;
    }

    private static final boolean defaultDOWNLOAD_ZIP                                    = false;
    private static final boolean defaultMOVE_FILES_TO_ACCOUNT                           = false;
    private static final boolean defaultDELETE_DELETE_FILE_AFTER_DOWNLOADLINK_CREATION  = false;
    private static final boolean defaultDELETE_FILE_FOREVER_AFTER_DOWNLOADLINK_CREATION = false;
    private static final boolean defaultEMPTY_TRASH_AFTER_DOWNLOADLINK_CREATION         = false;

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, "Crawler settings:"));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), PCloudCom.DOWNLOAD_ZIP, JDL.L("plugins.hoster.PCloudCom.DownloadZip", "Download .zip file of all files in the folder?")).setDefaultValue(defaultDOWNLOAD_ZIP));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, "Host plugin settings:"));
        final ConfigEntry moveFilesToAcc = new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), MOVE_FILES_TO_ACCOUNT, JDL.L("plugins.hoster.PCloudCom.MoveFilesToAccount", "1. Move files with too high traffic to account before downloading them to avoid downloadlimits?")).setDefaultValue(defaultMOVE_FILES_TO_ACCOUNT);
        getConfig().addEntry(moveFilesToAcc);
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), MOVE_FILES_TO_ACCOUNT, JDL.L("plugins.hoster.PCloudCom.DeleteMovedFiles", "2. Delete moved files after downloadlink-generation?")).setEnabledCondidtion(moveFilesToAcc, true).setDefaultValue(defaultDELETE_DELETE_FILE_AFTER_DOWNLOADLINK_CREATION));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), DELETE_FILE_FOREVER_AFTER_DOWNLOADLINK_CREATION, JDL.L("plugins.hoster.PCloudCom.DeleteMovedFilesForever", "3. Delete moved files FOREVER (inside trash) after downloadlink-generation?")).setEnabledCondidtion(moveFilesToAcc, true).setDefaultValue(defaultDELETE_FILE_FOREVER_AFTER_DOWNLOADLINK_CREATION));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), EMPTY_TRASH_AFTER_DOWNLOADLINK_CREATION, JDL.L("plugins.hoster.PCloudCom.EmptyTrashAfterSuccessfulDownload", "4. Empty trash after downloadlink-generation?")).setEnabledCondidtion(moveFilesToAcc, true).setDefaultValue(defaultEMPTY_TRASH_AFTER_DOWNLOADLINK_CREATION));
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }
}