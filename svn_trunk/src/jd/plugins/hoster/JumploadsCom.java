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
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.config.Property;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
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
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 51124 $", interfaceVersion = 3, names = {}, urls = {})
public class JumploadsCom extends PluginForHost {
    public JumploadsCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/premium");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/help/privacy";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "jumploads.com", "goloady.com" });
        return ret;
    }

    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("goloady.com");
        return deadDomains;
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/file/([A-Za-z0-9\\-_]+)(/([^/]+))?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2021-04-20: Main domain has changed from goloady.com to jumploads.com -> Old content can still be online! */
        return this.rewriteHost(getPluginDomains(), host);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return true;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 1;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return -5;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    private String getFallbackFilename(final DownloadLink link) {
        final String urlSlug = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(2);
        if (urlSlug != null) {
            final String[] segments = urlSlug.split("-");
            String weakFilename;
            if (segments.length >= 2) {
                /* Assume that file extension is last part of slug. */
                weakFilename = urlSlug.substring(0, urlSlug.lastIndexOf("-")) + "." + segments[segments.length - 1];
            } else {
                weakFilename = urlSlug;
            }
            weakFilename = weakFilename.replace("-", " ").trim();
            link.setName(weakFilename);
            return Encoding.htmlDecode(urlSlug);
        } else {
            /* Last chance fallback */
            return this.getFID(link);
        }
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return this.getHost() + "://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public void correctDownloadLink(final DownloadLink link) {
        /* Always use current host */
        final String oldDomain = Browser.getHost(link.getPluginPatternMatcher());
        link.setPluginPatternMatcher(link.getPluginPatternMatcher().replace(oldDomain + "/", this.getHost() + "/"));
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        if (!link.isNameSet()) {
            link.setName(this.getFallbackFilename(link));
        }
        br.getPage(link.getPluginPatternMatcher());
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("(?i)>\\s*?The file you are trying to download is no longer available|>\\s*?This could be due to the following reasons>\\s*?The file has been removed because of")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("filename[^\"]+\"[^>]*>([^<]+)</h2>").getMatch(0);
        String filesize = br.getRegex("filesize[^\"]*\"[^>]*>([^<]+)</h2>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            /* 2025-01-29: Set final filename here because server sends useless crap via Content-Disposition header */
            link.setFinalFileName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        if (!StringUtils.isEmpty(filesize)) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    private boolean isPrivateContent(final Browser br) {
        if (br.containsHTML("(?i)>\\s*?Content you have requested is Private")) {
            return true;
        } else {
            return false;
        }
    }

    private boolean isPasswordProtectedContent(final Browser br) {
        if (br.containsHTML("class=\"passwordLockedFile\"")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        handleDownload(link, null, "free_directlink");
    }

    private void handleDownload(final DownloadLink link, final Account account, final String directlinkproperty) throws Exception, PluginException {
        String dllink = checkDirectLink(link, directlinkproperty);
        // String dllink = null;
        /* 2020-04-07: Password protected content will also set this to false but password protected content is very rare! */
        boolean directDownloadEnabled = true;
        if (dllink == null) {
            /* 2019-08-13: E.g. premium download or free account download of self-uploaded files */
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, link.getPluginPatternMatcher(), this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                /* Direct link */
                dl.startDownload();
                return;
            }
            directDownloadEnabled = false;
            br.followConnection(true);
            /* 2020-04-07: E.g. premium account with disabled direct download */
            dllink = br.getRegex("href=\"((https?://[^/]+)?/dl/[^\"]+)\"").getMatch(0);
            if (dllink == null) {
                br.getHeaders().put("x-requested-with", "XMLHttpRequest");
                if (isPasswordProtectedContent(br)) {
                    /*
                     * 2020-04-27: Serverside broken e.g. https://www.jumploads.com/file/UKwbfe5PPn38TIDq-ygq9g/1mb.test --> Password =
                     * 123456
                     */
                    final boolean pw_protected_is_serverside_broken = true;
                    if (pw_protected_is_serverside_broken) {
                        throw new PluginException(LinkStatus.ERROR_FATAL, "Password protected URLs are serverside broken: Contact " + this.getHost() + " support");
                    }
                    final String owner = br.getRegex("own=\"(\\d+)\"").getMatch(0);
                    final String fileID_internal = br.getRegex("file=\"(\\d+)\"").getMatch(0);
                    if (owner == null || fileID_internal == null) {
                        logger.warning("Password handling failed: owner = " + owner + " | fileID_internal = " + fileID_internal);
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    String passCode = link.getDownloadPassword();
                    if (passCode == null) {
                        passCode = getUserInput("Password?", link);
                    }
                    br.postPage("/api/0/verifyfiledownloadpasscode?useraccess=&access_token=" + "br68ufmo5ej45ue1q10w68781069v666l2oh1j2ijt94", "owner=" + owner + "&file=" + fileID_internal + "&pass_code=" + Encoding.urlEncode(passCode));
                    final String result = PluginJSonUtils.getJson(br, "result");
                    if (StringUtils.isEmpty(result) || result.equalsIgnoreCase("failed")) {
                        link.setDownloadPassword(null);
                        throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password");
                    }
                    /* Store valid downloadpassword for previous download-attempts */
                    link.setDownloadPassword(passCode);
                }
                String free_server = br.getRegex("(?:freeaccess|data-srv)\\s*=\\s*\"([^\"]+)\"").getMatch(0);
                final String freetoken = br.getRegex("(?:freetoken|data-dl)\\s*=\\s*\"([^\"]+)\"").getMatch(0);
                if (freetoken == null || free_server == null) {
                    handleErrors(br);
                    if (account != null && account.getType() == AccountType.PREMIUM) {
                        errorEnableDirectDownload();
                        /* This code should never be reached. */
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    /* This token is used for all captcha attempts for current downloadlink! */
                    logger.warning("freetoken = " + freetoken + " | free_server = " + free_server);
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                /* 2019-08-13: Captcha is skippable */
                final boolean captchaSkippable = true;
                if (!captchaSkippable) {
                    boolean success = false;
                    for (int i = 0; i <= 4; i++) {
                        final long timeBefore = System.currentTimeMillis();
                        final String code = this.getCaptchaCode("/captcha/php/captcha.php", link);
                        if (i == 0) {
                            /* Wait only for first attempt */
                            waitTime(link, timeBefore);
                        }
                        br.postPage("/captcha/php/check_captcha.php", "captcha_code=" + Encoding.urlEncode(code) + "&token=" + freetoken);
                        if (br.toString().trim().equalsIgnoreCase("not_match")) {
                            continue;
                        }
                        /* Success = response equals "code_match" (without "") */
                        success = true;
                        break;
                    }
                    if (!success) {
                        throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                    }
                }
                if (Encoding.isUrlCoded(free_server)) {
                    free_server = Encoding.urlDecode(free_server, true);
                }
                dllink = free_server + "download.php?accesstoken=" + freetoken;
            }
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (!directDownloadEnabled && account != null && account.getType() == AccountType.PREMIUM) {
                errorEnableDirectDownload();
                /* This code should never be reached. */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                handleErrors(br);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        dl.startDownload();
    }

    /** 2020-04-07: With this disabled, downloads will fail quite often thus users should enable direct downloads! */
    private void errorEnableDirectDownload() throws PluginException {
        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Enable 'Direct Download' in account settings");
    }

    protected void waitTime(final DownloadLink link, final long timeBefore) throws PluginException {
        /* 2019-08-13: Default waittime: 45 seconds */
        /* Ticket Time */
        /* 2019-08-13: Waittime is skippable */
        final boolean waitSkippable = true;
        final String waitStr = br.getRegex("id=\"freetimer\"[^>]*?>(\\d+)<").getMatch(0);
        if (waitSkippable) {
            /* Very rare case! */
            logger.info("Skipping pre-download waittime: " + waitStr);
            return;
        }
        final int extraWaitSeconds = 1;
        int wait;
        if (waitStr != null && waitStr.matches("\\d+")) {
            int passedTime = (int) ((System.currentTimeMillis() - timeBefore) / 1000) - extraWaitSeconds;
            logger.info("Found waittime, parsing waittime: " + waitStr);
            wait = Integer.parseInt(waitStr);
            /*
             * Check how much time has passed during eventual captcha event before this function has been called and see how much time is
             * left to wait.
             */
            wait -= passedTime;
            if (passedTime > 0) {
                /* This usually means that the user had to solve a captcha which cuts down the remaining time we have to wait. */
                logger.info("Total passed time during captcha: " + passedTime);
            }
        } else {
            /* No waittime at all */
            wait = 0;
        }
        if (wait > 0) {
            logger.info("Waiting final waittime: " + wait);
            sleep(wait * 1000l, link);
        } else if (wait < -extraWaitSeconds) {
            /* User needed more time to solve the captcha so there is no waittime left :) */
            logger.info("Congratulations: Time to solve captcha was higher than waittime --> No waittime left");
        } else {
            /* No waittime at all */
            logger.info("Found no waittime");
        }
    }

    private void handleErrors(final Browser br) throws PluginException {
        final String premiumonlyText = br.getRegex(">\\s*(As a Free user you can download file up[^<]+)</h2>").getMatch(0);
        if (premiumonlyText != null) {
            throw new AccountRequiredException(premiumonlyText);
        } else if (br.containsHTML(">\\s*?This link only for premium user")) {
            /* 2019-08-13: It seems like basically all files are premiumonly(?) */
            throw new AccountRequiredException();
        } else if (br.containsHTML(">\\s*This link is for premium only user")) {
            throw new AccountRequiredException();
        } else if (isPrivateContent(br)) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Private content, only downloadable by owner");
        }
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        final String dllink = link.getStringProperty(property);
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                con = br2.openHeadConnection(dllink);
                if (!looksLikeDownloadableContent(con)) {
                    throw new IOException();
                } else {
                    return dllink;
                }
            } catch (final Exception e) {
                logger.log(e);
                link.setProperty(property, Property.NULL);
                return null;
            } finally {
                if (con != null) {
                    con.disconnect();
                }
            }
        } else {
            return null;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    private void login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            final String path_userinfo = "/drive/";
            if (cookies != null) {
                this.br.setCookies(this.getHost(), cookies);
                br.getPage("https://www." + this.getHost() + path_userinfo);
                /* 2020-04-07: Seems like their cookies are only valid for a very short time */
                if (this.isLoggedin(br)) {
                    logger.info("Successfully loggedin via cookies");
                    account.saveCookies(this.br.getCookies(this.getHost()), "");
                    return;
                } else {
                    logger.info("Failed to login via cookies");
                }
            }
            logger.info("Performing full login");
            br.getPage("https://www." + getHost() + "/user/login");
            final long ts = System.currentTimeMillis();
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("em", account.getUser());
            postdata.put("passw", account.getPass());
            postdata.put("robo", "__");
            postdata.put("___uctmp", ts);
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("x-requested-with", "XMLHttpRequest");
            brc.getHeaders().put("origin", "https://www." + this.getHost());
            brc.getHeaders().put("referer", "https://www." + this.getHost() + "/user/login");
            brc.getHeaders().put("sec-fetch-dest", "empty");
            brc.getHeaders().put("sec-fetch-mode", "cors");
            brc.getHeaders().put("sec-fetch-site", "same-origin");
            brc.postPageRaw("/app/user/signin?_tm=" + ts, JSonStorage.serializeToJson(postdata));
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (Boolean.TRUE.equals(entries.get("error"))) {
                throw new AccountInvalidException(entries.get("message").toString());
            }
            /* Set values from json as cookies which should grant us logged-in state. */
            final String[] login_cookie_keys = new String[] { "uid", "ut", "ud", "si" };
            for (final String key : login_cookie_keys) {
                final String value = (String) entries.get(key);
                if (value == null) {
                    throw new AccountInvalidException("Failed to find login cookie with key " + key);
                }
                br.setCookie(br.getHost(), "__" + key, value);
            }
            brc.getPage(path_userinfo + "?_svx=" + ts);
            /* Double-check */
            if (!this.isLoggedin(brc)) {
                throw new AccountInvalidException("Login failed for unknown reasons; contact support if this continues to happen");
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    private boolean isLoggedin(final Browser br) {
        if (br.containsHTML("/user/logout")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        final AccountInfo ai = new AccountInfo();
        final String userinfoJson = br.getRegex("ume = (\\{.*?\\}), ").getMatch(0);
        if (userinfoJson != null) {
            /* Obtain account information from json source */
            /* 2025-06-05: same as emload.com */
            final Map<String, Object> entries = restoreFromString(userinfoJson, TypeRef.MAP);
            final Map<String, Object> namemap = (Map<String, Object>) entries.get("name");
            final Map<String, Object> bw = (Map<String, Object>) entries.get("bw");
            final Map<String, Object> premiuminfo = (Map<String, Object>) entries.get("pro");
            /* Ensure to set a unique username: Due to cookie login, user could enter whatever he wants into username field in GUI. */
            if (account.loadUserCookies() != null) {
                if (account.getUser().contains("@")) {
                    final String email = StringUtils.valueOfOrNull(entries.get("email"));
                    if (email != null) {
                        account.setUser(email);
                    }
                } else if (namemap != null) {
                    final String fname = StringUtils.valueOfOrNull(namemap.get("fname"));
                    if (fname != null) {
                        account.setUser(fname);
                    }
                }
            }
            /* 35GB per day, see: https://www.jumploads.com/premium */
            // final long trafficMax = 35000000000l;
            final long trafficMax = ((Number) bw.get("avl")).longValue();
            final long trafficUsed = ((Number) bw.get("cons")).longValue();
            ai.setTrafficRefill(true);
            ai.setTrafficLeft(trafficMax - trafficUsed);
            ai.setTrafficMax(trafficMax);
            /* E.g. Lifetime Free Account */
            if (premiuminfo != null) {
                ai.setValidUntil(((Number) premiuminfo.get("expiry")).longValue() * 1000);
                account.setType(AccountType.PREMIUM);
                account.setMaxSimultanDownloads(this.getMaxSimultanPremiumDownloadNum());
                account.setConcurrentUsePossible(true);
            } else {
                account.setType(AccountType.FREE);
                /* free accounts can still have captcha */
                account.setMaxSimultanDownloads(this.getMaxSimultanFreeDownloadNum());
                account.setConcurrentUsePossible(false);
            }
        } else {
            final Regex trafficRegex = br.getRegex(">(\\d+[^<]*) of (\\d+[^<]+)</h2>\\s*<h2[^>]*>\\s*Used Bandwidth\\s*</h2>");
            if (!trafficRegex.patternFind()) {
                logger.warning("Failed to extract account traffic info");
            }
            final String traffic_usedStr = trafficRegex.getMatch(0);
            final String traffic_maxStr = trafficRegex.getMatch(1);
            final Regex spaceRegex = br.getRegex(">(\\d+[^<]*) of (\\d+[^<]+)</h2>\\s*<h2[^>]*>\\s*Used Space\\s*</h2>");
            if (!spaceRegex.patternFind()) {
                logger.warning("Failed to extract account space info");
            }
            final String space_usedStr = spaceRegex.getMatch(0);
            final String expireStr = br.getRegex(">\\s*Expires on (\\d{4}-\\d{1,2}-\\d{1,2})").getMatch(0);
            long expireTimestamp = 0;
            if (expireStr != null) {
                expireTimestamp = TimeFormatter.getMilliSeconds(expireStr, "yyyy-MM-dd", Locale.ENGLISH);
            }
            final boolean isPremium = br.containsHTML(">\\s*Pro\\s*</div>");
            if (isPremium || expireTimestamp > System.currentTimeMillis()) {
                /* Premium */
                if (expireTimestamp > System.currentTimeMillis()) {
                    ai.setValidUntil(expireTimestamp, br);
                }
                account.setType(AccountType.PREMIUM);
                account.setConcurrentUsePossible(true);
            } else {
                /* Free & expired premium */
                account.setType(AccountType.FREE);
                /* free accounts can still have captcha */
                account.setConcurrentUsePossible(false);
            }
            long traffic_left = 0;
            if (traffic_usedStr != null && traffic_maxStr != null) {
                final long traffic_used = SizeFormatter.getSize(traffic_usedStr);
                final long traffic_max = SizeFormatter.getSize(traffic_maxStr);
                traffic_left = traffic_max - traffic_used;
            }
            if (traffic_left > 0) {
                ai.setTrafficLeft(traffic_left);
            } else {
                logger.warning("Failed to find trafficleft");
                ai.setUnlimitedTraffic();
            }
            if (space_usedStr != null) {
                ai.setUsedSpace(SizeFormatter.getSize(space_usedStr));
            }
        }
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        /* 2019-08-13: Important: Do NOT login before availablecheck!! */
        requestFileInformation(link);
        login(account, false);
        if (account.getType() == AccountType.FREE) {
            handleDownload(link, account, "account_free_directlink");
        } else {
            handleDownload(link, account, "premium_directlink");
        }
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final jd.plugins.Account acc) {
        if (acc == null) {
            /* no account, yes we can expect captcha */
            return true;
        } else if (acc.getType() == AccountType.FREE) {
            /* Free accounts can have captchas */
            return true;
        } else {
            /* Premium accounts do not have captchas */
            return false;
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}