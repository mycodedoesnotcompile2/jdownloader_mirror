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
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
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

@HostPlugin(revision = "$Revision: 51443 $", interfaceVersion = 3, names = {}, urls = {})
public class PornboxCom extends PluginForHost {
    public PornboxCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/application/membership");
    }

    public static final String PROPERTY_CONTENT_ID = "content_id";

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX, LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/application/docs/terms";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "pornbox.com" });
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
            ret.add("");
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
        return link.getStringProperty(PROPERTY_CONTENT_ID);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    private String dllink = null;

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        return requestFileInformation(link, account);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        dllink = null;
        this.setBrowserExclusive();
        if (account == null) {
            throw new AccountRequiredException("Account required to download paid videos");
        }
        this.login(account, false);
        final String file_id = this.getFID(link);
        /* Browser calls this URL with parameter "label_id" which is optional so we do not include it. */
        br.getPage("https://" + getHost() + "/video_file/" + file_id);
        /* Returns error 403 if we are not logged in. */
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> urls = (Map<String, Object>) entries.get("urls");
        dllink = urls.get("mp4").toString();
        /* TODO: Find out what the information in field "attestation" means. */
        // final Object attestation = entries.get("attestation");
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        // TODO: Check if there exists free content that is officially downloadable
        throw new AccountRequiredException("Account required to download paid videos");
        // handleDownload(link, null);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        requestFileInformation(link, account);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            }
            try {
                br.followConnection(true);
            } catch (final IOException e) {
                logger.log(e);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    public boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            final Cookies userCookies = account.loadUserCookies();
            if (cookies != null || userCookies != null) {
                logger.info("Attempting cookie login");
                if (userCookies != null) {
                    br.setCookies(userCookies);
                } else {
                    br.setCookies(cookies);
                }
                if (!force) {
                    /* Don't validate cookies */
                    return false;
                }
                br.getPage("https://" + this.getHost() + "/chat/total-unread-messages");
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                if (entries.containsKey("total_unread_messages")) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return true;
                } else {
                    if (userCookies != null) {
                        logger.info("User Cookie login failed");
                        if (account.hasEverBeenValid()) {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                        } else {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                        }
                    }
                    /* Fallthrough -> Try full login */
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                }
            }
            logger.info("Performing full login");
            final Browser brc = br.cloneBrowser();
            brc.getPage("https://account.analvids.com/jdialog/box/1.js");
            final String jdialog3_anti_bot_cookie = brc.getRegex("var pin_hash = '([^\"\\']+)';").getMatch(0);
            if (jdialog3_anti_bot_cookie == null) {
                /*
                 * Without this we will not be able to access any pornbox.com login pages -> We cannot obtain a csrf cookie -> We cannot
                 * login
                 */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            br.setCookie(getHost(), "JDIALOG3", jdialog3_anti_bot_cookie);
            String reCaptchaSitekey = null;
            String lastLoginFailedMsg = null;
            int attempt = 0;
            while (!this.isAbort() && attempt <= 2) {
                attempt++;
                br.getPage("https://" + this.getHost() + "/signin");
                final String csrftoken = br.getRegex("window\\.CSRF_TOKEN = \"([^\"]+)\";").getMatch(0);
                if (csrftoken == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final long proofOfWork = this.generateProofOfWork(account.getUser(), account.getPass());
                final Form loginform = new Form();
                loginform.setMethod(MethodType.POST);
                loginform.setAction("/user/auth");
                loginform.put("login", Encoding.urlEncode(account.getUser()));
                loginform.put("password", Encoding.urlEncode(account.getPass()));
                loginform.put("proofOfWorkCode", Long.toString(proofOfWork));
                loginform.put("cache", "false");
                loginform.put("showError", "false");
                loginform.put("dataType", "json");
                if (reCaptchaSitekey != null) {
                    final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, reCaptchaSitekey).getToken();
                    loginform.put("captcha_public", Encoding.urlEncode(recaptchaV2Response));
                }
                br.getHeaders().put("x-csrf-token", csrftoken);
                br.submitForm(loginform);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                lastLoginFailedMsg = (String) entries.get("message");
                if (Boolean.TRUE.equals(entries.get("need_show_captcha")) && reCaptchaSitekey == null) {
                    /* Retry only once */
                    reCaptchaSitekey = entries.get("sitekey").toString();
                    continue;
                }
                checkErrorsAPI(entries);
                logger.info("Login looks to be successful");
                /**
                 * e.g. "https://account.analvids.com//authorize?action=..." <br>
                 * Accessing that URL will finally authorize us and get us valid login cookies.
                 */
                final String loginurl = entries.get("url").toString();
                br.getPage(loginurl);
                logger.info("Full login successful");
                account.saveCookies(br.getCookies(br.getHost()), "");
                return true;
            }
            throw new AccountInvalidException(lastLoginFailedMsg);
        }
    }

    private Object checkErrorsAPI(final Browser br) throws PluginException {
        final Object entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
        if (entries instanceof List) {
            return entries;
        }
        final Map<String, Object> map = (Map<String, Object>) entries;
        checkErrorsAPI(map);
        return entries;
    }

    private void checkErrorsAPI(final Map<String, Object> map) throws PluginException {
        String message = (String) map.get("message");
        if ("error".equals(map.get("result"))) {
            /* E.g. {"result":"error","message":" Username or password incorrect\n"} */
            throw new AccountInvalidException(message);
        }
        final String code = (String) map.get("code");
        if (code != null) {
            /**
             * E.g. accessing /member/subscription without valid cookies <br>
             * {"text":"User not authenticated","code":"user_not_authenticated"}
             */
            final String message2 = (String) map.get("text");
            throw new AccountInvalidException(message2);
        }
        /* No error */
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        /**
         * Returns http response 500 when login cookies expired <br>
         * Returns [] (empty array) when account is a free account.
         */
        br.getPage("/member/subscription");
        /* This website doesn't have any traffic limited accounts. */
        ai.setUnlimitedTraffic();
        final List<Map<String, Object>> memberships = (List<Map<String, Object>>) this.checkErrorsAPI(br);
        for (final Map<String, Object> membership : memberships) {
            final Number how_many_days_until_goods_expires = (Number) membership.get("how_many_days_until_goods_expires");
            if (how_many_days_until_goods_expires == null) {
                continue;
            }
            final int how_many_days_until_goods_expires_int = how_many_days_until_goods_expires.intValue();
            if (how_many_days_until_goods_expires_int <= 0) {
                continue;
            }
            final String expires = membership.get("expires").toString();
            account.setType(AccountType.PREMIUM);
            ai.setValidUntil(TimeFormatter.getMilliSeconds(expires, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH), br);
            final String cancelledStr;
            if (Boolean.TRUE.equals(membership.get("is_cancelled"))) {
                cancelledStr = "Yes";
            } else {
                cancelledStr = "No";
            }
            /** humanize_resource_name e.g. "1 Month Full Access Membership" */
            ai.setStatus(membership.get("humanize_resource_name").toString() + " | Cancelled: " + cancelledStr);
            return ai;
        }
        /* No premium package found -> User has a free account. */
        account.setType(AccountType.FREE);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    /* See: https://pornbox.com/assets/app-b39fbaab171d6410d58ece5de94999aa.js */
    private long generateProofOfWork(String username, String password) throws NoSuchAlgorithmException, InterruptedException, PluginException {
        Random random = new Random();
        // Generate random 32-bit unsigned integer equivalent (0 to 4294967295)
        long nonce = random.nextInt() & 0xFFFFFFFFL;
        int attempts = 0;
        MessageDigest md5 = MessageDigest.getInstance("MD5");
        while (attempts < 10000) {
            nonce++;
            attempts++;
            // Create the input string: username + password + nonce
            String input = username + password + Long.toString(nonce);
            // Compute MD5 hash
            md5.reset();
            byte[] hashBytes = md5.digest(input.getBytes());
            // Convert to hex string
            String hexHash = bytesToHex(hashBytes);
            // Check if hash starts with "00"
            if (hexHash.startsWith("00")) {
                return nonce;
            }
        }
        /* This should never happen */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    private static String bytesToHex(byte[] bytes) {
        StringBuilder hexString = new StringBuilder();
        for (byte b : bytes) {
            String hex = Integer.toHexString(0xFF & b);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }

    private static boolean verifyProofOfWork(String username, String password, long nonce) throws NoSuchAlgorithmException {
        MessageDigest md5 = MessageDigest.getInstance("MD5");
        String input = username + password + Long.toString(nonce);
        byte[] hashBytes = md5.digest(input.getBytes());
        String hexHash = bytesToHex(hashBytes);
        return hexHash.startsWith("00");
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}