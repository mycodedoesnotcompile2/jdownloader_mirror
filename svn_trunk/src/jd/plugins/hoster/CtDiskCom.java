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
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.UserAgents;
import jd.plugins.decrypter.CtDiskComFolder;

@HostPlugin(revision = "$Revision: 51847 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { CtDiskComFolder.class })
public class CtDiskCom extends PluginForHost {
    public static final String                WEBAPI_BASE       = "https://webapi.ctfile.com";
    private static final Pattern              PATTERN_F         = Pattern.compile("/f/(\\d+)-(\\d+)(-([a-f0-9]+))?", Pattern.CASE_INSENSITIVE);
    private static final Pattern              PATTERN_FS        = Pattern.compile("/fs/(\\d+)-(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern              PATTERN_FILE      = Pattern.compile("/file/(\\d+)-(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern              PATTERN_F_TEMPDIR = Pattern.compile("/f/tempdir-([A-Za-z0-9_-]+)", Pattern.CASE_INSENSITIVE);
    public static final String                PROPERTY_FILEID   = "fileid";
    public static final String                PROPERTY_USERID   = "userid";
    public static final String                PROPERTY_FILE_CHK = "file_chk";
    /* don't touch the following! */
    private static Map<String, AtomicInteger> freeRunning       = new HashMap<String, AtomicInteger>();

    public CtDiskCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost());
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("Accept-Language", "en-gb, en;q=0.9");
        br.setConnectTimeout(120 * 1000);
        br.setReadTimeout(120 * 1000);
        br.setFollowRedirects(true);
        return br;
    }

    public static String[] getAnnotationNames() {
        return Plugin.buildAnnotationNames(CtDiskComFolder.getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return this.buildSupportedNames(CtDiskComFolder.getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String[]> pluginDomains = CtDiskComFolder.getPluginDomains();
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:[A-Za-z0-9]+\\.)?" + Plugin.buildHostsPatternPart(domains) + "(" + PATTERN_F.pattern() + "|" + PATTERN_FS.pattern() + "|" + PATTERN_FILE.pattern() + "|" + PATTERN_F_TEMPDIR.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String rewriteHost(String host) {
        return this.rewriteHost(CtDiskComFolder.getPluginDomains(), host, "ctfile.com");
    }

    @Override
    public boolean hasAutoCaptcha() {
        return false;
    }

    private String getDomainCorrectedURL(final String url) {
        final String host_current = Browser.getHost(url);
        final String host_new = this.correctHost(host_current);
        return url.replaceFirst(Pattern.quote(host_current), host_new);
    }

    protected String correctHost(String host) {
        return this.getMappedHost(CtDiskComFolder.getPluginDomains(), host);
    }

    public static String getUserID(final DownloadLink link) {
        final String userid = link.getStringProperty(CtDiskCom.PROPERTY_USERID);
        if (userid != null) {
            return userid;
        } else {
            return CtDiskCom.getUserID(link.getPluginPatternMatcher());
        }
    }

    public static String getUserID(final String url) {
        String userid = new Regex(url, PATTERN_F).getMatch(0);
        if (userid != null) {
            return userid;
        }
        userid = new Regex(url, PATTERN_FS).getMatch(0);
        if (userid != null) {
            return userid;
        }
        userid = new Regex(url, PATTERN_FILE).getMatch(0);
        return userid;
    }

    public static String getFileID(final DownloadLink link) {
        final String fid = link.getStringProperty(CtDiskCom.PROPERTY_FILEID);
        if (fid != null) {
            return fid;
        }
        return CtDiskCom.getFileID(link.getPluginPatternMatcher());
    }

    public static String getFileID(final String url) {
        String fileid = new Regex(url, PATTERN_F).getMatch(1);
        if (fileid != null) {
            return fileid;
        }
        fileid = new Regex(url, PATTERN_FS).getMatch(1);
        if (fileid != null) {
            return fileid;
        }
        fileid = new Regex(url, PATTERN_FILE).getMatch(1);
        return fileid;
    }

    public static String getFileHash(final String url) {
        String filehash = new Regex(url, PATTERN_F).getMatch(3);
        return filehash;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String userid = getUserID(link);
        final String fileid = getFileID(link);
        if (userid != null && fileid != null) {
            return this.getHost() + "://" + "/user/" + userid + "/file/" + fileid;
        } else {
            return super.getLinkID(link);
        }
    }

    private Map<String, Object> filemap = null;

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final boolean isDownload = PluginEnvironment.DOWNLOAD.equals(this.getPluginEnvironment());
        final String passCodeFromURL = UrlQuery.parse(link.getPluginPatternMatcher()).get("p");
        String passCode = link.getDownloadPassword();
        if (passCode == null) {
            passCode = passCodeFromURL;
        }
        int passwordAttempts = 0;
        Map<String, Object> entries = null;
        filemap = null;
        int code = 0;
        String message = null;
        final String file_hash = getFileHash(link.getPluginPatternMatcher());
        do {
            final UrlQuery query = new UrlQuery();
            final Regex type_tempdir = new Regex(link.getPluginPatternMatcher(), PATTERN_F_TEMPDIR);
            if (type_tempdir.patternFind()) {
                /* 2021-08-10 */
                query.add("path", "f");
                query.add("f", type_tempdir.getMatch(0));
                query.add("token", "false");
                query.add("r", "0." + System.currentTimeMillis());
                final String parentDirURL = link.getStringProperty(CtDiskComFolder.PROPERTY_PARENT_DIR);
                if (parentDirURL != null) {
                    query.appendEncoded("ref", parentDirURL);
                }
            } else {
                /* Old type */
                String userid = CtDiskCom.getUserID(link.getPluginPatternMatcher());
                String fileid = CtDiskCom.getFileID(link.getPluginPatternMatcher());
                if (userid == null || fileid == null) {
                    /* This should never happen */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                if (file_hash != null) {
                    query.add("path", "f");
                    query.add("f", userid + "-" + fileid + "-" + file_hash);
                } else {
                    query.add("f", userid + "-" + fileid);
                }
                query.add("ref", "");
            }
            query.appendEncoded("passcode", passCode != null ? passCode : "");
            br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
            /*
             * 2019-11-21: Next request will return 404 if we use a wrong referer value.
             */
            br.getHeaders().put("Origin", "https://" + new URL(link.getPluginPatternMatcher()).getHost());
            /* Referer is not necessarily needed */
            br.getHeaders().put("Referer", link.getPluginPatternMatcher());
            br.getPage(CtDiskCom.WEBAPI_BASE + "/getfile.php?" + query.toString());
            entries = this.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            filemap = (Map<String, Object>) entries.get("file");
            message = filemap != null ? (String) filemap.get("message") : null;
            if (filemap != null) {
                message = (String) filemap.get("message");
            } else {
                message = (String) entries.get("message");
            }
            code = ((Number) entries.get("code")).intValue();
            if (code != 401 && code != 423) {
                /* No password required or correct password has been entered. */
                break;
            }
            /* Password required or entered password was wrong */
            if (passCode != null) {
                this.logger.info("User entered wrong password: " + passCode);
            } else {
                this.logger.info("This file is password protected");
            }
            link.setPasswordProtected(true);
            link.setDownloadPassword(null);
            if (!isDownload) {
                /* Only ask for password in download mode */
                this.logger.info("Not asking for password because we're currently in linkcheck");
                return AvailableStatus.TRUE;
            } else if (passwordAttempts >= 2) {
                this.logger.info("Too many wrong password attempts");
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
            } else {
                if (passCode != null) {
                    this.logger.info("User entered wrong password: " + passCode);
                } else {
                    this.logger.info("Password required --> Asking user for the first time");
                }
                passCode = this.getUserInput("Password?", link);
                passwordAttempts += 1;
                continue;
            }
        } while (true);
        /**
         * 2025-11-18: For password protected files, we cannot know the online-status before correct password is entered. </br>
         * This first handle password, then store it if it was correct, then check for offline status!
         */
        if (passCode != null) {
            link.setDownloadPassword(passCode);
        } else {
            /* No password required or no password required anymore */
            link.setPasswordProtected(false);
            link.setDownloadPassword(null);
        }
        if (code == 403) {
            /* {"code":403,"file":{"code":403,"message":"The share link is not fully opened."} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
        } else if (code == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
        } else if (code == 503) {
            /* {"code":503,"message":"\u8be5\u5171\u4eab\u4e0d\u5b58\u5728\u6216\u5df2\u5931\u6548\u3002"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
        } else if (code == 504) {
            /* {"code":504,"file":{"code":504,"message":"User sharing is blocked."} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
        } else if (code != 200) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Error code " + code + " | " + message);
        }
        final String filename = (String) filemap.get("file_name");
        final String filesizeStr = (String) filemap.get("file_size");
        /* Save as property on DownloadLink as this information is not given for every linktype! */
        /*
         * We need fileID and userID but it is not always given e.g. if user pre-enters WRONG download-password which is then already used
         * during linkcheck!
         */
        final Object useridO = filemap.get("userid");
        final Object file_idO = filemap.get("file_id");
        final Object file_chkO = filemap.get("file_chk");
        if (file_idO != null && useridO != null) {
            link.setProperty(CtDiskCom.PROPERTY_USERID, useridO.toString());
            link.setProperty(CtDiskCom.PROPERTY_FILEID, file_idO.toString());
        }
        /* We need this to download but e.g. for password protected items it is only given after entering the correct password. */
        if (file_chkO != null) {
            link.setProperty(CtDiskCom.PROPERTY_FILE_CHK, file_chkO);
        }
        if (!StringUtils.isEmpty(filename)) {
            link.setFinalFileName(filename);
        }
        if (!StringUtils.isEmpty(filesizeStr)) {
            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
        }
        return AvailableStatus.TRUE;
    }

    private <T> T get(Object object, String key, Class<T> T) {
        Object ret = JavaScriptEngineFactory.walkJson(object, key);
        if (ret == null) {
            ret = JavaScriptEngineFactory.walkJson(object, "file/" + key);
        }
        return (T) ret;
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        return CtDiskCom.getFileID(link);
    }

    public void doFree(final DownloadLink link, final Account account, final boolean resume, final int maxchunks) throws Exception, PluginException {
        this.requestFileInformation(link);
        String userid = CtDiskCom.getUserID(link);
        String fileid = CtDiskCom.getFileID(link);
        if (userid == null) {
            /* This may happen for old URLs before <= revision 28648! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Browser brc = br.cloneBrowser();
        /* 2021-10-22: This enables us to start multiple simultaneous downloads in free mode. */
        final String rua = UserAgents.stringUserAgent();
        brc.getHeaders().put("User-Agent", rua);
        String folder_id = "";
        String file_chk = null;
        if (fileid == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        file_chk = link.getStringProperty(CtDiskCom.PROPERTY_FILE_CHK);
        if (folder_id == null || StringUtils.isEmpty(file_chk)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final int wait_seconds = ((Number) filemap.get("wait_seconds")).intValue();
        final long start_time = ((Number) filemap.get("start_time")).longValue();
        /* 2025-11-18: wait_time cannot be skipped anymore */
        this.sleep(wait_seconds * 1000, link); // typically 30 seconds
        final UrlQuery query = new UrlQuery();
        query.add("uid", userid);
        query.add("fid", fileid);
        query.add("folder_id", "0");
        query.add("share_id", "");
        query.add("file_chk", file_chk);
        query.add("start_time", Long.toString(start_time));
        query.add("wait_seconds", Integer.toString(wait_seconds));
        query.add("mb", "0");
        query.add("app", "0");
        query.add("acheck", "1");
        /* 2025-11-18: verifycode can be empty */
        query.add("verifycode", (String) filemap.get("verifycode"));
        query.add("rd", "0." + System.currentTimeMillis());
        brc.getPage("/get_file_url.php?" + query.toString());
        final Map<String, Object> entries = this.restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        /* error handling */
        final String errormsg = (String) entries.get("message");
        if (errormsg != null) {
            /*
             * 2021-10-22: E.g. {"code":503,"message":"require for verifycode"} --> Probably captcha but looks like a rate-limit captcha
             * which can be avoided by simply retrying later.
             */
            if (errormsg.equalsIgnoreCase("require for verifycode")) {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Rate limit reached (captcha required) | Try again later | Serverside error: " + errormsg, 1 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_FATAL, errormsg);
            }
        }
        final String dllink = (String) entries.get("downurl");
        if (dllink == null || !dllink.startsWith("http")) {
            /* Most likely website would redirect to login-page. */
            /*
             * E.g.
             * {"code":302,"url":"https:\/\/www.ctfile.com\/tokenGo.php?token=&url=https%3A%2F%2Fwww.ctfile.com%2Fp%2Flogin","file_name"
             * :"test.zip","xhr":true}
             */
            throw new AccountRequiredException();
        }
        /* Evaluate additional data we should get via current json. */
        final String file_name = (String) entries.get("file_name");
        if (!StringUtils.isEmpty(file_name)) {
            link.setFinalFileName(file_name);
        }
        final Number file_size = (Number) entries.get("file_size");
        if (file_size != null) {
            link.setVerifiedFileSize(file_size.longValue());
        }
        this.dl = jd.plugins.BrowserAdapter.openDownload(this.br, link, dllink, resume, maxchunks);
        if (!this.looksLikeDownloadableContent(this.dl.getConnection())) {
            try {
                br.followConnection(true);
            } catch (final IOException e) {
                this.logger.log(e);
            }
            if (br.containsHTML("window\\.location\\.href\\s*=\\s*\"https?://[^/]+/premium/")) {
                /* 2021-10-22: Redirect to "buy premium" page if user tries to start more than 1 simultaneous downloads. */
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Wait before starting more downloads", 1 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        /* add a download slot */
        this.controlMaxFreeDownloads(account, link, +1);
        try {
            /* start the dl */
            this.dl.startDownload();
        } finally {
            /* remove download slot */
            this.controlMaxFreeDownloads(account, link, -1);
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        AccountInfo ai = new AccountInfo();
        this.login(account);
        if (br.getURL() == null || !br.getURL().contains("action=account_info")) {
            br.getPage("https://home." + br.getHost() + "/iajax.php?item=profile&action=account_info");
        }
        /* 2020-02-04: "user_info":"\u6708\u4ed8\u9ad8\u7ea7VIP\u4f1a\u5458<\/small> <username>" */
        /* 2022-30-06: "user_info":"<small>Speedy Member<\/small> username","username":".... */
        final String user_info = PluginJSonUtils.getJson(this.br, "user_info");
        if (user_info != null && user_info.matches("(?i).*VIP.*?<.*")) {
            ai.setStatus("VIP");
            account.setType(AccountType.PREMIUM);
        } else if (user_info != null && user_info.matches("(?i).*Speedy.*?<.*")) {
            ai.setStatus("Speedy Member");
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
        }
        ai.setUnlimitedTraffic();
        return ai;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/help.php?item=service";
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        // Works best this way. Maximum that worked for me was 6
        return Integer.MAX_VALUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        this.doFree(link, null, true, 1);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (account.getType() == AccountType.FREE) {
            this.requestFileInformation(link);
            this.login(account);
            this.doFree(link, account, true, 1);
        } else {
            /* Login first, then do availablecheck as this will display directurls immediately! */
            this.login(account);
            this.requestFileInformation(link);
            // TODO: 2025-11-18: Use json parser here
            /* 2020-02-04: There are different final downloadlinks (mirrors?!) available: vip_dx_url, vip_lt_url, vip_yd_url */
            String dllink = PluginJSonUtils.getJson(this.br, "vip_dx_url");
            if (StringUtils.isEmpty(dllink)) {
                this.logger.warning("Failed to find dllink");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            this.dl = jd.plugins.BrowserAdapter.openDownload(this.br, link, dllink, true, -5);
            if (!this.looksLikeDownloadableContent(this.dl.getConnection())) {
                br.followConnection(true);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            this.dl.startDownload();
        }
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        /* Only login-captcha. */
        return false;
    }

    private void login(final Account account) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final String main_login_host = "400gb.com"; // this may differ from this.getHost()
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                this.logger.info("Checking stored cookies");
                final String currenthost = account.getStringProperty("lasthost", main_login_host);
                this.setCookies(this.br, cookies);
                br.getPage("https://home." + currenthost + "/iajax.php?item=profile&action=account_info");
                if (this.loggedIN(this.br)) {
                    /* Save new cookie timestamp */
                    this.logger.info("Stored cookies are valid");
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    account.setProperty("lasthost", br.getHost());
                    return;
                } else {
                    this.logger.info("Stored cookies are invalid");
                    br.clearCookies(null);
                }
            }
            this.logger.info("Performing full login");
            br.getPage("https://www." + main_login_host + "/p/login");
            int logincounter = 0;
            do {
                if (br.containsHTML(">\\s*为了您的安全，本次登录请使用手机客户端扫码登录。") || logincounter > 0) {
                    this.logger.info("QR code app login required");
                    final String loginqr = br.getRegex("\"(/getQRcode\\.php\\?loginkey=[^<>\"]+)\"").getMatch(0);
                    if (loginqr == null) {
                        this.logger.warning("Failed to find login qrcode");
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    boolean isWaitingForUserAppLogin = true;
                    int loopcounter = 0;
                    final int maxloops = 20;
                    final int waitInBetween = 5000;
                    final int totalWait = maxloops * waitInBetween;
                    final Thread dialog = this.showAppLoginInformation(br.getHost(), totalWait);
                    String responsecodeStr;
                    try {
                        this.logger.info(String.format(Locale.ROOT, "User has %d seconds time to login via app", waitInBetween / 1000));
                        /* First wait some time to let user read dialog */
                        Thread.sleep(waitInBetween);
                        /*
                         * Now display the 'captcha' --> QR code which can be used to auth user via QR code via their app. This is a
                         * workaround as there is currently no easy way to display a QR code without click field or text field. Ideally we
                         * should be able to auto-close it once user is logged-in the same way we are doing it with the login dialog.
                         */
                        final DownloadLink dlinkbefore = this.getDownloadLink();
                        try {
                            final DownloadLink dl_dummy;
                            if (dlinkbefore != null) {
                                dl_dummy = dlinkbefore;
                            } else {
                                dl_dummy = new DownloadLink(this, "Account", this.getHost(), "https://" + account.getHoster(), true);
                                this.setDownloadLink(dl_dummy);
                            }
                            this.getCaptchaCode(loginqr, this.getDownloadLink());
                        } finally {
                            this.setDownloadLink(dlinkbefore);
                        }
                        do {
                            loopcounter++;
                            this.logger.info(String.format(Locale.ROOT, "Waiting for user to login via app %d / %d", loopcounter, maxloops));
                            br.getPage("https://www." + br.getHost() + "/api.php?item=login&action=qrcode&r=0." + System.currentTimeMillis());
                            /* 2020-02-10: Website is waiting 5 seconds between requests */
                            responsecodeStr = PluginJSonUtils.getJson(this.br, "code");
                            if (!dialog.isAlive()) {
                                this.logger.info("Dialog was closed");
                                break;
                            } else if (StringUtils.isEmpty(responsecodeStr) || responsecodeStr.equals("205")) {
                                /* {"code":205,"message":"waiting for scan"} */
                                this.logger.info("User has not yet logged in via app");
                                Thread.sleep(waitInBetween);
                                continue;
                            }
                            this.logger.info("QR login status changed");
                            isWaitingForUserAppLogin = false;
                            break;
                        } while (isWaitingForUserAppLogin && loopcounter <= maxloops);
                        if (isWaitingForUserAppLogin) {
                            /* User needed to much time or did not login via app at all. */
                            this.logger.info("Timeout: User did not login via app?");
                            throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
                        } else if (responsecodeStr == null || !responsecodeStr.equals("200")) {
                            throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
                        }
                    } finally {
                        dialog.interrupt();
                    }
                    this.logger.info("Seems like app login might have been successful");
                    final String pubcookie = PluginJSonUtils.getJson(this.br, "pubcookie");
                    final String ct_uid = PluginJSonUtils.getJson(this.br, "ct_uid");
                    if (StringUtils.isEmpty(pubcookie) || StringUtils.isEmpty(ct_uid)) {
                        /* This should not happen but indicates login failure! */
                        this.logger.info("Failed to get login cookies from API ...");
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
                    }
                    br.setCookie(br.getHost(), "pubcookie", pubcookie);
                    br.setCookie(br.getHost(), "ct_uid", ct_uid);
                    br.getPage("https://home." + br.getHost(false) + "/iajax.php?item=profile&action=account_info");
                    break;
                    /* Now let cookies be checked. We should be loggedIN now! */
                }
                if (logincounter > 0) {
                    this.logger.info("Login failed in login loop");
                    break;
                }
                final Form loginform = br.getFormByInputFieldKeyValue("action", "login");
                if (loginform == null) {
                    this.logger.warning("Failed to find loginform");
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                loginform.put("email", Encoding.urlEncode(account.getUser()));
                loginform.put("password", Encoding.urlEncode(account.getPass()));
                /* 2020-02-04: Captcha handling might be outdated at least it has not been tested with new login method! */
                final String captchaURL = br.getRegex("\"(/randcodeV2_login\\.php\\?r=\\d+)\"").getMatch(0);
                if (captchaURL != null) {
                    final DownloadLink dummy = new DownloadLink(this, "Account login", br.getHost(), br.getHost(), true);
                    final String code = this.getCaptchaCode(captchaURL, dummy);
                    loginform.put("randcodeV2", Encoding.urlEncode(code));
                }
                br.submitForm(loginform);
                logincounter++;
            } while (!this.loggedIN(this.br));
            if (!this.loggedIN(this.br)) {
                account.clearCookies("");
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
            final Cookies freshCookies = br.getCookies(br.getHost());
            account.saveCookies(freshCookies, "");
            /* This host is changing their main domain frequently --> Store current main domain for later usage. */
            account.setProperty("lasthost", br.getHost());
            this.setCookies(this.br, freshCookies);
        }
    }

    /* Sets cookies on all known domains. */
    private void setCookies(final Browser br, final Cookies cookies) {
        br.setCookies(cookies);
        for (final String[] domains : CtDiskComFolder.getPluginDomains()) {
            for (final String domain : domains) {
                br.setCookies(domain, cookies);
            }
        }
    }

    boolean loggedIN(final Browser br) {
        if (br.getCookie(br.getHost(), "pubcookie", Cookies.NOTDELETEDPATTERN) != null) {
            return true;
        } else {
            return false;
        }
    }

    private Thread showAppLoginInformation(final String host, final int totalWait) {
        final Thread thread = new Thread() {
            @Override
            public void run() {
                try {
                    final String host_without_tld = host.split("\\.")[0];
                    String message = "";
                    final String title;
                    title = host + " - App login required";
                    message += "Hello dear " + host + " user\r\n";
                    message += "A QR code will appear in some seconds.\r\n";
                    message += "The QR code is not a new type of captcha but a QR code to login via mobile app.\r\n";
                    message += "Please open the " + host_without_tld + " mobile app, scan the QR code and login.\r\n";
                    message += "Do not close this dialog until you did login via app!\r\n";
                    message += "It will auto-close once you logged in via app!";
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, title, message);
                    dialog.setTimeout(totalWait);
                    final ConfirmDialogInterface ret = UIOManager.I().show(ConfirmDialogInterface.class, dialog);
                    ret.throwCloseExceptions();
                } catch (final Throwable e) {
                    CtDiskCom.this.getLogger().log(e);
                }
            };
        };
        thread.setDaemon(true);
        thread.start();
        return thread;
    }

    /**
     * Prevents more than one free download from starting at a given time. One step prior to dl.startDownload(), it adds a slot to maxFree
     * which allows the next singleton download to start, or at least try.
     *
     * This is needed because this website may ask for a session captcha once per session (per X time) so starting multiple downloads at the
     * same time could result in multiple captchas -> We want to avoid that.
     *
     * @param num
     *            : (+1|-1)
     */
    protected void controlMaxFreeDownloads(final Account account, final DownloadLink link, final int num) {
        if (account != null) {
            return;
        }
        final AtomicInteger freeRunning = this.getFreeRunning();
        synchronized (freeRunning) {
            final int before = freeRunning.get();
            final int after = before + num;
            freeRunning.set(after);
            this.logger.info("freeRunning(" + link.getName() + ")|max:" + this.getMaxSimultanFreeDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
        }
    }

    protected AtomicInteger getFreeRunning() {
        synchronized (CtDiskCom.freeRunning) {
            AtomicInteger ret = CtDiskCom.freeRunning.get(this.getHost());
            if (ret == null) {
                ret = new AtomicInteger(0);
                CtDiskCom.freeRunning.put(this.getHost(), ret);
            }
            return ret;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        /* 2021-10-22: Tested up to 5 */
        final int max = 5;
        final int running = this.getFreeRunning().get();
        final int ret = Math.min(running + 1, max);
        return ret;
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* 2021-10-22: Remove stored directurls in debug mode/IDE for easier debugging. */
            link.removeProperty("directlink");
        }
    }
}