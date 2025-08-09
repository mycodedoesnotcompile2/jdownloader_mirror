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

import java.awt.Color;
import java.net.URL;
import java.util.ArrayList;
import java.util.Currency;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter.HighlightPainter;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtPasswordField;
import org.appwork.swing.components.ExtTextField;
import org.appwork.swing.components.ExtTextHighlighter;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.net.URLHelper;
import org.jdownloader.gui.InputChangedCallbackInterface;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.accounts.AccountBuilderInterface;
import org.jdownloader.plugins.components.XFileSharingProBasic;
import org.jdownloader.plugins.components.config.XFSConfigSendCm;
import org.jdownloader.plugins.components.config.XFSConfigSendCm.LoginMode;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.gui.swing.components.linkbutton.JLink;
import jd.http.Browser;
import jd.http.Cookie;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.DefaultEditAccountPanelAPIKeyLogin;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51312 $", interfaceVersion = 3, names = {}, urls = {})
public class SendCm extends XFileSharingProBasic {
    public SendCm(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    private static final String PROPERTY_ACCOUNT_FORCE_API_LOGIN     = "force_api_login";
    private static final String PROPERTY_ACCOUNT_FORCE_WEBSITE_LOGIN = "force_website_login";

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            final List<LazyPlugin.FEATURE> ret = new ArrayList<LazyPlugin.FEATURE>();
            if (requiresCookieLogin()) {
                ret.add(LazyPlugin.FEATURE.COOKIE_LOGIN_ONLY);
            } else {
                ret.add(LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL);
            }
            return ret.toArray(new LazyPlugin.FEATURE[0]);
        } else {
            return super.getFeatures();
        }
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: 2020-11-06: null<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "send.now", "send.cm", "sendit.cloud", "usersfiles.com", "tusfiles.com", "tusfiles.net" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("usersfiles.com");
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
        return XFileSharingProBasic.buildAnnotationUrls(getPluginDomains());
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2022-07-27: sendit.cloud and usersfiles.com have been merged from another plugin into this one. */
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
            return true;
        }
    }

    @Override
    public int getMaxChunks(final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 1;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return -10;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return 10;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return 10;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 10;
    }

    @Override
    public AvailableStatus requestFileInformationWebsite(final DownloadLink link, Account account) throws Exception {
        final AvailableStatus status = super.requestFileInformationWebsite(link, account);
        // 2023-04-12: MD5 seems to be base64 encoded but doesn't match, maybe just fake info?!*/
        final String sha256 = br.getRegex("SHA-256\\s*:\\s*</b>\\s*([a-f0-9]{64})\\s*</span>").getMatch(0);
        if (sha256 != null) {
            link.setSha256Hash(sha256);
        }
        return status;
    }

    @Override
    protected void resolveShortURL(final Browser br, final DownloadLink link, final Account account) throws Exception {
        /* 2023-04-21: Special handling for when downloadlimit is reached during this handling -> Look for fuid in cookies. */
        synchronized (link) {
            try {
                super.resolveShortURL(br, link, account);
            } catch (final PluginException exc) {
                logger.info("Attempting special handling to find real FUID");
                final Cookies cookies = br.getCookies(br.getHost());
                final Set<String> realFUIDs = new HashSet<String>();
                for (final Cookie cookie : cookies.getCookies()) {
                    if (cookie.getKey() != null && cookie.getKey().matches("c_[A-Za-z0-9]+")) {
                        final String value = cookie.getValue();
                        if (value != null && value.matches("[A-Za-z0-9]{12}")) {
                            realFUIDs.add(value);
                        }
                    }
                }
                if (realFUIDs.size() != 1) {
                    /* No result or more than one -> Problem! */
                    logger.info("Failed to find real FUID");
                    throw exc;
                }
                /* Success! */
                final String realFUID = realFUIDs.iterator().next();
                final String contentURL = this.getContentURL(link);
                final String urlNew = URLHelper.parseLocation(new URL(this.getMainPage(link)), buildNormalURLPath(link, realFUID));
                logger.info("resolve URL|old: " + contentURL + "|new:" + urlNew);
                link.setPluginPatternMatcher(urlNew);
            }
        }
    }

    @Override
    public void doFree(final DownloadLink link, final Account account) throws Exception, PluginException {
        if (allowAPIDownloadIfApikeyIsAvailable(link, account)) {
            /**
             * 2023-10-16: Special: For "Free accounts" with paid "Premium bandwidth". </br>
             * Looks like this is supposed to help with Cloudflare problems.
             */
            final String directurl = this.getDllinkAPI(link, account);
            handleDownload(link, account, null, directurl);
        } else {
            super.doFree(link, account);
        }
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (allowAPIDownloadIfApikeyIsAvailable(link, account)) {
            final String directurl = this.getDllinkAPI(link, account);
            handleDownload(link, account, null, directurl);
        } else {
            super.handlePremium(link, account);
        }
    }

    private boolean isFreeAccountWithPremiumTraffic(final Account account) {
        if (account != null && account.getType() == AccountType.FREE && account.getAccountInfo() != null && account.getAccountInfo().getTrafficLeft() > 0) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean loginWebsite(final DownloadLink link, final Account account, final boolean validateCookies) throws Exception {
        try {
            return super.loginWebsite(link, account, validateCookies);
        } catch (final PluginException e) {
            return handleLoginWebsite2FA(e, link, account, validateCookies);
        }
    }

    @Override
    public String getLoginURL() {
        return getMainPage() + "/?op=login";
    }

    @Override
    protected String regExTrafficLeft(final Browser br) {
        final String betterTrafficLeft = br.getRegex(">\\s*Premium Bandwidth.*?<h3[^>]*>(\\d+[^<]+)</h3>").getMatch(0);
        if (betterTrafficLeft != null) {
            return betterTrafficLeft;
        } else {
            return super.regExTrafficLeft(br);
        }
    }

    @Override
    public String[] scanInfo(final String html, String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        String betterFilename = br.getRegex("class\\s*=\\s*\"modal-title\"\\s*id=\"qr\"[^>]*>\\s*([^<]*?)\\s*</h\\d+>").getMatch(0);
        if (betterFilename == null) {
            betterFilename = br.getRegex("data-feather\\s*=\\s*\"file\"[^>]*>\\s*</i>\\s*([^<]*?)\\s*</h\\d+>").getMatch(0);
            if (betterFilename == null) {
                betterFilename = br.getRegex("\\&text=([^\"]+)\" target=\"_blank\">\\s*Share on Telegram").getMatch(0);
            }
        }
        if (betterFilename != null) {
            fileInfo[0] = betterFilename;
        }
        final String betterFilesize = br.getRegex("id=\"downloadbtn[>]*><i [^>]*></i>\\s*Download \\[([^<\\]]+)\\]</button>").getMatch(0);
        if (betterFilesize != null) {
            fileInfo[1] = betterFilesize;
        }
        return fileInfo;
    }

    @Override
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account, final boolean checkAll) throws NumberFormatException, PluginException {
        super.checkErrors(br, html, link, account, checkAll);
        if (br.containsHTML(">\\s*Not allowed")) {
            /*
             * 2024-06-24: Not sure what this means. Possibly this happens for premium-only files. In this case, website does not provide
             * any information on filename/size.
             */
            throw new PluginException(LinkStatus.ERROR_FATAL, "Website error 'Not allowed'");
        }
        if (br.containsHTML(">\\s*You can download up to")) {
            /**
             * 2025-08-07: "</i> You can download up to&nbsp;<strong>1 GB</strong>&nbsp;without an account.&nbsp;<a
             * href='https://send.now/register'"
             */
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Free download limit reached. Change IP or try again with a free or paid account.");
        }
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.containsHTML(">\\s*The file you were looking for doesn")) {
            return true;
        } else if (br.containsHTML(">\\s*The file you were looking for doesn")) {
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }

    @Override
    protected String getPremiumOnlyErrorMessage(final Browser br) {
        String msg = br.getRegex(">\\s*(This file is available for[^<]+)").getMatch(0);
        if (msg != null) {
            msg = Encoding.htmlDecode(msg).trim();
            return msg;
        } else {
            return super.getPremiumOnlyErrorMessage(br);
        }
    }

    @Override
    public boolean isPasswordProtectedHTML(final Browser br, final Form pwForm) {
        if (pwForm != null && pwForm.containsHTML("Enter File Password")) {
            return true;
        } else {
            return super.isPasswordProtectedHTML(br, pwForm);
        }
    }

    @Override
    protected boolean supports_availablecheck_alt() {
        /* 2022-10-04 */
        return true;
    }

    @Override
    protected boolean supports_availablecheck_filename_abuse() {
        /* 2021-09-29 */
        return false;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2023-05-28 */
        return false;
    }

    @Override
    protected String regexAPIKey(final Browser br) {
        final String apikey = br.getRegex("<span class=\"input-group-text\"[^>]*>\\s*([a-z0-9]{20,})\\s*</span>").getMatch(0);
        if (apikey != null) {
            return apikey;
        } else {
            return super.regexAPIKey(br);
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        if (account.hasProperty(PROPERTY_ACCOUNT_FORCE_WEBSITE_LOGIN)) {
            return this.fetchAccountInfoWebsite(account);
        } else if (this.enableAccountApiOnlyMode() || account.hasProperty(PROPERTY_ACCOUNT_FORCE_API_LOGIN)) {
            return this.fetchAccountInfoAPI(this.br, account);
        } else {
            return this.fetchAccountInfoWebsite(account);
        }
    }

    @Override
    protected AccountInfo fetchAccountInfoAPI(final Browser br, final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> entries = loginAPI(br, account);
        /** 2019-07-31: Better compare expire-date against their serverside time if possible! */
        final String server_timeStr = (String) entries.get("server_time");
        final Map<String, Object> result = (Map<String, Object>) entries.get("result");
        long expire_milliseconds_precise_to_the_second = 0;
        final String email = (String) result.get("email");
        final long currentTime;
        if (server_timeStr != null && server_timeStr.matches("\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}")) {
            currentTime = TimeFormatter.getMilliSeconds(server_timeStr, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
        } else {
            /* Fallback */
            currentTime = System.currentTimeMillis();
        }
        String premium_expireStr = (String) result.get("premium_expire");
        if (StringUtils.isEmpty(premium_expireStr)) {
            /*
             * 2019-05-30: Seems to be a typo by the guy who develops the XFS script in the early versions of thei "API mod" :D 2019-07-28:
             * Typo is fixed in newer XFSv3 versions - still we'll keep both versions in just to make sure it will always work ...
             */
            premium_expireStr = (String) entries.get("premim_expire");
        }
        final String premium_bandwidthBytesStr = (String) result.get("premium_bandwidth");
        long premium_bandwidthBytes = -1;
        final String traffic_leftBytesStr = (String) result.get("traffic_left");
        /*
         * 2019-08-22: For newly created free accounts, an expire-date will always be given, even if the account has never been a premium
         * account. This expire-date will usually be the creation date of the account then --> Handling will correctly recognize it as a
         * free account!
         */
        if (premium_expireStr != null && premium_expireStr.matches("\\d{4}\\-\\d{2}\\-\\d{2} \\d{2}:\\d{2}:\\d{2}")) {
            expire_milliseconds_precise_to_the_second = TimeFormatter.getMilliSeconds(premium_expireStr, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
        }
        if (premium_bandwidthBytesStr != null) {
            premium_bandwidthBytes = SizeFormatter.getSize(premium_bandwidthBytesStr);
            ai.setTrafficLeft(premium_bandwidthBytes);
        } else if (traffic_leftBytesStr != null) {
            ai.setTrafficLeft(SizeFormatter.getSize(traffic_leftBytesStr));
        } else {
            ai.setUnlimitedTraffic();
        }
        final long premiumDurationMilliseconds = expire_milliseconds_precise_to_the_second - currentTime;
        if (premiumDurationMilliseconds <= 0) {
            /* Expired premium or no expire date given --> It is usually a Free Account */
            setAccountLimitsByType(account, AccountType.FREE);
            // ai.setExpired(true);
        } else {
            /* Expire date is in the future --> It is a premium account */
            ai.setValidUntil(System.currentTimeMillis() + premiumDurationMilliseconds);
            setAccountLimitsByType(account, AccountType.PREMIUM);
        }
        if (premium_bandwidthBytes <= 0) {
            if (account.getType() == AccountType.FREE) {
                /* Free account without premium traffic */
                throw new AccountInvalidException("\r\nYou can only use premium accounts or free accounts with premium traffic via API login.\r\nUse login via username and password or buy traffic.");
            } else {
                /* Premium account without traffic */
                String msg = "This is a paid account but no longer has direct link traffic available. This can be purchased from this page: " + getHost() + "/pricing.";
                msg += "\r\nTo use the direct_link API, you need a Pro or Premium account and direct link traffic.";
                msg += "\r\nTo use JDownloader for downloading from send.cm website, you need a paid account and 'direct link traffic'.";
                throw new AccountInvalidException(msg);
            }
        }
        {
            /* Now set less relevant account information */
            final Object balanceO = result.get("balance");
            if (balanceO != null) {
                final String balanceStr = balanceO.toString();
                if (balanceStr.matches("\\d+\\.\\d+")) {
                    final double balance = Double.parseDouble(balanceStr);
                    ai.setAccountBalance(balance * 100, Currency.getInstance("USD"));
                }
            }
            /* 2019-07-26: values can also be "inf" for "Unlimited": "storage_left":"inf" */
            // final long storage_left = JavaScriptEngineFactory.toLong(entries.get("storage_left"), 0);
            final Object storage_used_bytesO = result.get("storage_used");
            if (storage_used_bytesO != null) {
                ai.setUsedSpace(SizeFormatter.getSize(storage_used_bytesO.toString()));
            }
        }
        if (this.enableAccountApiOnlyMode() && !account.hasProperty(PROPERTY_ACCOUNT_FORCE_WEBSITE_LOGIN) && !StringUtils.isEmpty(email)) {
            /*
             * Each account is unique. Do not care what the user entered - trust what API returns! </br> This is not really important - more
             * visually so that something that makes sense is displayed to the user in his account managers' "Username" column!
             */
            account.setUser(email);
        } else if (StringUtils.equals(account.getUser(), this.getAPIKeyFromAccount(account))) {
            logger.info("User has entered API key as username & password -> Set email as username");
            account.setUser(email);
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* Devs only */
            String accStatus;
            if (ai.getStatus() != null) {
                accStatus = ai.getStatus();
            } else {
                accStatus = account.getType().toString();
            }
            ai.setStatus("[API] " + accStatus);
        }
        return ai;
    }

    @Override
    protected String getDllinkAPI(final DownloadLink link, final Account account) throws Exception {
        logger.info("Trying to get dllink via API");
        final String apikey = getAPIKeyFromAccount(account);
        if (StringUtils.isEmpty(apikey)) {
            /* This should never happen */
            throw new IllegalArgumentException("apikey is null");
        }
        final String fuid = this.getFUIDFromURL(link);
        final String fileid_to_download;
        if (fuid.matches("[a-z0-9]{12}")) {
            fileid_to_download = fuid;
        } else {
            /* Special: Short URL -> Usually not supported by API but they've somehow integrated it. */
            fileid_to_download = "d/" + fuid;
        }
        getPage(this.getAPIBase() + "/file/direct_link?key=" + apikey + "&file_code=" + Encoding.urlEncode(fileid_to_download));
        final Map<String, Object> entries = this.checkErrorsAPI(this.br, link, account);
        final Map<String, Object> result = (Map<String, Object>) entries.get("result");
        final String dllink = result.get("url").toString();
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find dllink via API");
        }
        logger.info("Successfully found dllink via API");
        return dllink;
    }

    @Override
    protected boolean allowAPIDownloadIfApikeyIsAvailable(final DownloadLink link, final Account account) {
        if (account == null) {
            return false;
        }
        final String apikey = getAPIKeyFromAccount(account);
        if (apikey != null && (isFreeAccountWithPremiumTraffic(account) || account.getType() == AccountType.PREMIUM)) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected boolean supportsAPIMassLinkcheck() {
        if (looksLikeValidAPIKey(this.getAPIKey())) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected boolean enableAccountApiOnlyMode() {
        // TODO: Try to remove this override
        final XFSConfigSendCm cfg = PluginJsonConfig.get(XFSConfigSendCm.class);
        if (cfg.getLoginMode() == LoginMode.API) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected boolean supportsAPISingleLinkcheck() {
        return looksLikeValidAPIKey(this.getAPIKey());
    }

    @Override
    public boolean massLinkcheckerAPI(final DownloadLink[] urls, final String apikey) {
        /**
         * Copy & paste from superclass and modified to set file hash on DownloadLink items.
         */
        if (urls == null || urls.length == 0 || !this.looksLikeValidAPIKey(apikey)) {
            return false;
        }
        boolean linkcheckerHasFailed = false;
        try {
            final Browser br = createNewBrowserInstance();
            this.prepBrowser(br, getMainPage());
            br.setCookiesExclusive(true);
            final StringBuilder sb = new StringBuilder();
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    /*
                     * We test max 50 links at once. 2020-05-29: XFS default API linkcheck limit is exactly 50 items. If you check more than
                     * 50 items, it will only return results for the first 50 items.
                     */
                    if (index == urls.length || links.size() == 50) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                final ArrayList<DownloadLink> apiLinkcheckLinks = new ArrayList<DownloadLink>();
                sb.delete(0, sb.capacity());
                for (final DownloadLink link : links) {
                    try {
                        resolveShortURL(br.cloneBrowser(), link, null);
                    } catch (final PluginException e) {
                        logger.log(e);
                        if (e.getLinkStatus() == LinkStatus.ERROR_FILE_NOT_FOUND) {
                            link.setAvailableStatus(AvailableStatus.FALSE);
                        } else if (e.getLinkStatus() == LinkStatus.ERROR_IP_BLOCKED) {
                            link.setAvailableStatus(AvailableStatus.TRUE);
                        } else {
                            link.setAvailableStatus(AvailableStatus.UNCHECKABLE);
                        }
                        if (!link.isNameSet()) {
                            setWeakFilename(link, null);
                        }
                        /*
                         * We cannot check shortLinks via API so if we're unable to convert them to TYPE_NORMAL we basically already checked
                         * them here. Also we have to avoid sending wrong fileIDs to the API otherwise linkcheck WILL fail!
                         */
                        continue;
                    }
                    sb.append(this.getFUIDFromURL(link));
                    sb.append("%2C");
                    apiLinkcheckLinks.add(link);
                }
                if (apiLinkcheckLinks.isEmpty()) {
                    /* Rare edge-case */
                    logger.info("Seems like we got only shortURLs -> Nothing left to be checked via API");
                    break;
                }
                getPage(br, getAPIBase() + "/file/info?key=" + apikey + "&file_code=" + sb.toString());
                Map<String, Object> entries = null;
                try {
                    entries = this.checkErrorsAPI(br, links.get(0), null);
                } catch (final Throwable e) {
                    logger.log(e);
                    /* E.g. invalid apikey, broken serverside API, developer mistake (e.g. sent fileIDs in invalid format) */
                    logger.info("Fatal failure");
                    return false;
                }
                final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) entries.get("result");
                for (final DownloadLink link : apiLinkcheckLinks) {
                    Map<String, Object> fileinfo = null;
                    final String thisFUID = this.getFUIDFromURL(link);
                    for (final Map<String, Object> fileInfoTmp : ressourcelist) {
                        String fuid_temp = (String) fileInfoTmp.get("filecode");
                        if (StringUtils.isEmpty(fuid_temp)) {
                            /* 2022-08-09 */
                            fuid_temp = (String) fileInfoTmp.get("file_code");
                        }
                        if (StringUtils.equals(fuid_temp, thisFUID)) {
                            fileinfo = fileInfoTmp;
                            break;
                        }
                    }
                    if (fileinfo == null) {
                        /**
                         * This should never happen. Possible reasons: <br>
                         * - Wrong APIKey <br>
                         * - We tried to check too many items at once <br>
                         * - API only allows users to check self-uploaded content --> Disable API linkchecking in plugin! <br>
                         * - API does not not allow linkchecking at all --> Disable API linkchecking in plugin! <br>
                         */
                        logger.warning("WTF failed to find information for fuid: " + this.getFUIDFromURL(link));
                        linkcheckerHasFailed = true;
                        continue;
                    }
                    /* E.g. check for "result":[{"status":404,"filecode":"xxxxxxyyyyyy"}] */
                    final int status = ((Number) fileinfo.get("status")).intValue();
                    if (!link.isNameSet()) {
                        setWeakFilename(link, null);
                    }
                    final String hash_sha256 = (String) fileinfo.get("file_sha256");
                    if (hash_sha256 != null) {
                        link.setSha256Hash(hash_sha256);
                    }
                    String filename = null;
                    boolean isVideohost = false;
                    if (status != 200) {
                        link.setAvailable(false);
                    } else {
                        link.setAvailable(true);
                        filename = (String) fileinfo.get("name");
                        if (StringUtils.isEmpty(filename)) {
                            filename = (String) fileinfo.get("file_title");
                        }
                        final long filesize = JavaScriptEngineFactory.toLong(fileinfo.get("size"), 0);
                        final Object canplay = fileinfo.get("canplay");
                        final Object views_started = fileinfo.get("views_started");
                        final Object views = fileinfo.get("views");
                        final Object length = fileinfo.get("length");
                        isVideohost = canplay != null || views_started != null || views != null || length != null;
                        /* Filesize is not always given especially not for videohosts. */
                        if (filesize > 0) {
                            link.setDownloadSize(filesize);
                        }
                    }
                    if (!isVideohost) {
                        isVideohost = this.internal_isVideohoster_enforce_video_filename(link, null);
                    }
                    if (!StringUtils.isEmpty(filename)) {
                        /*
                         * At least for videohosts, filenames from json would often not contain a file extension!
                         */
                        if (Encoding.isHtmlEntityCoded(filename)) {
                            filename = Encoding.htmlDecode(filename).trim();
                        }
                        if (isVideohost) {
                            filename = this.applyFilenameExtension(filename, ".mp4");
                        }
                        /* Trust API filenames -> Set as final filename. */
                        link.setFinalFileName(filename);
                    } else {
                        /* Use cached name */
                        final String name = link.getName();
                        if (name != null && isVideohost) {
                            link.setName(this.applyFilenameExtension(filename, ".mp4"));
                        }
                    }
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            logger.log(e);
            return false;
        } finally {
            if (linkcheckerHasFailed) {
                logger.info("Seems like massLinkcheckerAPI availablecheck is not supported by this host or currently broken");
            }
        }
        if (linkcheckerHasFailed) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public Class<? extends XFSConfigSendCm> getConfigInterface() {
        return XFSConfigSendCm.class;
    }

    @Override
    public AccountBuilderInterface getAccountFactory(final InputChangedCallbackInterface callback) {
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* 2025-08-07: Testing */
            return new SendCmAccountFactory(callback, this);
        } else {
            /* Website login */
            final XFSConfigSendCm cfg = PluginJsonConfig.get(XFSConfigSendCm.class);
            if (cfg.getLoginMode() == LoginMode.API || cfg.getLoginMode() == LoginMode.AUTO || cfg.getLoginMode() == LoginMode.DEFAULT) {
                return new DefaultEditAccountPanelAPIKeyLogin(callback, this);
            } else {
                return super.getAccountFactory(callback);
            }
        }
    }

    public static class SendCmAccountFactory extends MigPanel implements AccountBuilderInterface {
        /**
         *
         */
        private static final long serialVersionUID = 1L;

        protected String getPassword() {
            if (this.pass == null) {
                return null;
            } else {
                return new String(this.pass.getPassword());
            }
        }

        protected String getUsername() {
            if (name == null) {
                return "";
            } else {
                if (_GUI.T.jd_gui_swing_components_AccountDialog_help_username().equals(this.name.getText())) {
                    return null;
                }
                return this.name.getText();
            }
        }

        protected String getApikey() {
            if (apikey == null) {
                return null;
            } else {
                return this.apikey.getText();
            }
        }

        private final ExtTextField                  name;
        private final ExtPasswordField              pass;
        private final ExtPasswordField              apikey;
        private final JLabel                        apikeyLabel;
        private final InputChangedCallbackInterface callback;
        private JLabel                              usernameLabel = null;
        private final JLabel                        passwordOrCookiesLabel;
        private final SendCm                        plg;
        private final boolean                       usernameIsEmail;
        private final boolean                       websiteLoginCookieLoginOnly;
        private final boolean                       websiteLoginCookieLoginOptional;

        public boolean updateAccount(Account input, Account output) {
            boolean changed = false;
            if (!StringUtils.equals(input.getUser(), output.getUser())) {
                output.setUser(input.getUser());
                changed = true;
            }
            if (!StringUtils.equals(input.getPass(), output.getPass())) {
                output.setPass(input.getPass());
                changed = true;
            }
            return changed;
        }

        public SendCmAccountFactory(final InputChangedCallbackInterface callback, final SendCm plg) {
            super("ins 0, wrap 2", "[][grow,fill]", "");
            this.plg = plg;
            this.callback = callback;
            this.usernameIsEmail = this.plg.hasFeature(FEATURE.USERNAME_IS_EMAIL);
            this.websiteLoginCookieLoginOnly = this.plg.hasFeature(FEATURE.COOKIE_LOGIN_ONLY);
            this.websiteLoginCookieLoginOptional = this.plg.hasFeature(FEATURE.COOKIE_LOGIN_OPTIONAL);
            final String domain = this.plg.getHost();
            final String apikey_help_url_without_protocol = domain + "/?op=my_account";
            final String apikey_help_url = "https://" + apikey_help_url_without_protocol;
            add(new JLabel("Premium account:"));
            add(new JLink("Enter API key (click here to find it)", apikey_help_url));
            add(apikeyLabel = new JLink("Premium API Key: ", apikey_help_url));
            add(this.apikey = new ExtPasswordField() {
                @Override
                public void onChanged() {
                    callback.onChangedInput(apikey);
                }
            }, "");
            this.apikey.setHelpText("Obtain API key here: " + apikey_help_url_without_protocol);
            if (websiteLoginCookieLoginOnly) {
                add(new JLabel(_GUI.T.jd_gui_swing_components_AccountDialog_generic_instructions()));
                add(new JLink(_GUI.T.jd_gui_swing_components_AccountDialog_generic_instructions_click_here_for_instructions(), apikey_help_url));
                add(new JLabel("Free account:"));
                add(new JLink("Enter username & cookies", "https://support.jdownloader.org/Knowledgebase/Article/View/account-cookie-login-instructions"));
            } else {
                add(new JLabel("Free account:"));
                add(new JLink("Enter username & pass or cookies", "https://support.jdownloader.org/Knowledgebase/Article/View/account-cookie-login-instructions"));
            }
            if (this.usernameIsEmail) {
                add(usernameLabel = new JLabel(_GUI.T.jd_gui_swing_components_AccountDialog_email()));
            } else {
                add(usernameLabel = new JLabel(_GUI.T.jd_gui_swing_components_AccountDialog_name()));
            }
            add(this.name = new ExtTextField() {
                @Override
                public void onChanged() {
                    callback.onChangedInput(name);
                }

                {
                    final HighlightPainter painter = new DefaultHighlighter.DefaultHighlightPainter(Color.yellow);
                    addTextHighlighter(new ExtTextHighlighter(painter, Pattern.compile("^(\\s+)")));
                    addTextHighlighter(new ExtTextHighlighter(painter, Pattern.compile("(\\s+)$")));
                    refreshTextHighlighter();
                }
            });
            if (this.usernameIsEmail) {
                name.setHelpText(_GUI.T.jd_gui_swing_components_AccountDialog_help_email());
            } else {
                name.setHelpText(_GUI.T.jd_gui_swing_components_AccountDialog_help_username());
            }
            if (websiteLoginCookieLoginOnly) {
                add(passwordOrCookiesLabel = new JLink("<HTML><U>" + _GUI.T.jd_gui_swing_components_AccountDialog_cookies() + "</U></HTML>", "https://support.jdownloader.org/Knowledgebase/Article/View/account-cookie-login-instructions"));
            } else if (websiteLoginCookieLoginOptional) {
                String labelTxt = _GUI.T.jd_gui_swing_components_AccountDialog_pass_or_cookies();
                labelTxt = labelTxt.replaceFirst("(?i)" + Pattern.quote(_GUI.T.jd_gui_swing_components_AccountDialog_cookies()), "<U>" + _GUI.T.jd_gui_swing_components_AccountDialog_cookies() + "</U>");
                if (!_GUI.T.jd_gui_swing_components_AccountDialog_pass_or_cookies().matches(labelTxt)) {
                    labelTxt = "<HTML>" + labelTxt + "</HTML>";
                    add(passwordOrCookiesLabel = new JLink(labelTxt, "https://support.jdownloader.org/Knowledgebase/Article/View/account-cookie-login-instructions"));
                } else {
                    add(passwordOrCookiesLabel = new JLabel(labelTxt));
                }
            } else {
                /* Normal username & password login */
                add(passwordOrCookiesLabel = new JLabel(_GUI.T.jd_gui_swing_components_AccountDialog_pass()));
            }
            // TODO: Fix unclickable password field (only rightclick works to enter it)
            add(this.pass = new ExtPasswordField() {
                @Override
                public void onChanged() {
                    callback.onChangedInput(pass);
                }

                {
                    final HighlightPainter painter = new DefaultHighlighter.DefaultHighlightPainter(Color.yellow);
                    addTextHighlighter(new ExtTextHighlighter(painter, Pattern.compile("^(\\s+)")) {
                        public boolean highlight(javax.swing.text.Highlighter highlighter, CharSequence charSequence) {
                            if (Cookies.parseCookiesFromString(charSequence.toString()) != null) {
                                return false;
                            } else {
                                return super.highlight(highlighter, charSequence);
                            }
                        };
                    });
                    addTextHighlighter(new ExtTextHighlighter(painter, Pattern.compile("(\\s+)$")) {
                        public boolean highlight(javax.swing.text.Highlighter highlighter, CharSequence charSequence) {
                            if (Cookies.parseCookiesFromString(charSequence.toString()) != null) {
                                return false;
                            } else {
                                return super.highlight(highlighter, charSequence);
                            }
                        };
                    });
                    applyTextHighlighter(null);
                }
            }, "");
            if (websiteLoginCookieLoginOnly) {
                pass.setHelpText(_GUI.T.BuyAndAddPremiumAccount_layoutDialogContent_cookies());
            } else if (websiteLoginCookieLoginOptional) {
                pass.setHelpText(_GUI.T.BuyAndAddPremiumAccount_layoutDialogContent_pass_or_cookies());
            } else {
                /* Normal username & password login */
                pass.setHelpText(_GUI.T.BuyAndAddPremiumAccount_layoutDialogContent_pass());
            }
            // pass.setFocusable(true);
            // pass.setEnabled(true);
            // pass.setBackground(Color.CYAN);
            final ExtTextField dummy = new ExtTextField();
            dummy.paste();
            final String clipboard = dummy.getText();
            if (StringUtils.isNotEmpty(clipboard)) {
                /* Automatically put exported cookies json string into password field in case that's the current clipboard content. */
                final Cookies userCookies = Cookies.parseCookiesFromJsonString(clipboard, null);
                if ((websiteLoginCookieLoginOnly || websiteLoginCookieLoginOptional) && userCookies != null) {
                    /*
                     * Cookie login is supported and users' clipboard contains exported cookies at this moment -> Auto-fill password field
                     * with them.
                     */
                    // TODO: Check why this doesn't work
                    pass.setPassword(clipboard.toCharArray());
                    // pass.setText(clipboard);
                } else if (this.apikey != null && this.plg.looksLikeValidAPIKey(clipboard)) {
                    this.apikey.setPassword(clipboard.toCharArray());
                } else if (userCookies == null && clipboard.trim().length() > 0) {
                    /* Auto fill username field with clipboard content. */
                    name.setText(clipboard);
                }
            }
        }

        public InputChangedCallbackInterface getCallback() {
            return callback;
        }

        public void setAccount(final Account defaultAccount) {
            if (defaultAccount != null) {
                name.setText(defaultAccount.getUser());
                pass.setText(defaultAccount.getPass());
                apikey.setText(defaultAccount.getPass());
            }
        }

        @Override
        public boolean validateInputs() {
            final boolean userok;
            final boolean passok;
            boolean apikey_ok = false;
            final String apikey = this.getApikey();
            if (apikey != null && apikeyLabel != null) {
                if (plg.looksLikeValidAPIKey(apikey)) {
                    apikey_ok = true;
                    this.apikeyLabel.setForeground(Color.BLACK);
                } else {
                    apikey_ok = false;
                    this.apikeyLabel.setForeground(Color.RED);
                }
            }
            if (StringUtils.isEmpty(this.getUsername())) {
                usernameLabel.setForeground(Color.RED);
                userok = false;
            } else if (this.usernameIsEmail && !plg.looksLikeValidEmailAddress(null, this.getUsername())) {
                /* E-Mail is needed but user did not enter a valid-looking e-mail address. */
                usernameLabel.setForeground(Color.RED);
                userok = false;
            } else {
                usernameLabel.setForeground(Color.BLACK);
                userok = true;
            }
            final String pw = getPassword();
            final Cookies cookies = Cookies.parseCookiesFromString(pw);
            if (StringUtils.isEmpty(pw)) {
                /* Password field is never allowed to be empty/null. */
                passok = false;
            } else if (websiteLoginCookieLoginOnly && cookies == null) {
                /* Cookies are needed but not given. */
                passok = false;
            } else if (!websiteLoginCookieLoginOnly && !websiteLoginCookieLoginOptional && cookies != null) {
                /* Cookies are given while user is not allowed to use cookies. */
                passok = false;
            } else {
                passok = true;
            }
            if (!passok) {
                passwordOrCookiesLabel.setForeground(Color.RED);
            } else {
                passwordOrCookiesLabel.setForeground(Color.BLACK);
            }
            if (apikey_ok) {
                // TODO: Evaluate if foreground change for passwordOrCookiesLabel is a good idea
                passwordOrCookiesLabel.setForeground(Color.BLACK);
                if (StringUtils.isEmpty(this.getUsername())) {
                    /* TODO: Find a better solution since this may cause an infinite loop */
                    // this.name.setText(apikey);
                }
                return true;
            } else if (userok && passok) {
                // TODO: Evaluate if foreground change for apikeyLabel is a good idea
                apikeyLabel.setForeground(Color.BLACK);
                return true;
            } else {
                return false;
            }
        }

        @Override
        public Account getAccount() {
            if (plg.looksLikeValidAPIKey(this.getApikey())) {
                /* Use API key as password */
                final String apikey = this.getApikey();
                final Account account = new Account(getUsername(), apikey);
                account.setProperty(PROPERTY_ACCOUNT_apikey, apikey);
                account.setProperty(PROPERTY_ACCOUNT_FORCE_API_LOGIN, true);
                return account;
            } else {
                /**
                 * Workaround for users who set this setting to API but login via website/free-account <br>
                 * The API login mode is still needed for headless installations! <br>
                 *
                 * This workaround is required so that PluginForHost.validateLogins will not fail due to API key validation when
                 * enableAccountApiOnlyMode() returns true.
                 */
                final XFSConfigSendCm cfg = PluginJsonConfig.get(XFSConfigSendCm.class);
                cfg.setLoginMode(LoginMode.DEFAULT);
                final Account account = new Account(getUsername(), getPassword());
                account.setProperty(PROPERTY_ACCOUNT_FORCE_WEBSITE_LOGIN, true);
                return account;
            }
        }

        @Override
        public JComponent getComponent() {
            return this;
        }
    }
}