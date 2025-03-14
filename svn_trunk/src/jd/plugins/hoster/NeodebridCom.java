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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 50303 $", interfaceVersion = 3, names = { "neodebrid.com" }, urls = { "https?://(?:www\\.)?neodebrid\\.com/dl/([A-Z0-9]+)" })
public class NeodebridCom extends PluginForHost {
    /** Tags: cocoleech.com */
    private static final String          API_BASE                   = "https://neodebrid.com/api/v2";
    private final String                 PROPERTY_MAXCHUNKS         = "neodebridcom_maxchunks";
    private final String                 PROPERTY_ACCOUNT_api_token = "api_token";
    private static final boolean         api_supports_free_accounts = false;
    private static MultiHosterManagement mhm                        = new MultiHosterManagement("neodebrid.com");

    @SuppressWarnings("deprecation")
    public NeodebridCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/premium");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/tos";
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        int maxChunks = link.getIntegerProperty(PROPERTY_MAXCHUNKS, 1);
        return maxChunks;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            link.setName(getFID(link));
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (this.isOffline(this.br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("(?i)<b>\\s*Filename\\s*:\\s*</b>([^<>\"]+)<").getMatch(0);
        String filesize = br.getRegex("(?i)<b>\\s*Filesize\\s*:\\s*</b>([^<>\"]+)<").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        handleDLSelfhosted(null, link);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link);
        handleDLSelfhosted(account, link);
    }

    private void handleDLSelfhosted(final Account account, final DownloadLink link) throws Exception {
        String dllink = checkDirectLink(link, this.getHost() + "directurl_selfhosted");
        if (dllink == null) {
            dllink = getDllinkWebsiteCaptcha(account, link);
        }
        if (StringUtils.isEmpty(dllink)) {
            logger.warning("Failed to find dllink");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        link.setProperty(this.getHost() + "directlink", dllink);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 404) {
                /* 2019-08-26: This happens quite often! */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 3 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Final downloadlink did not lead to downloadable content");
            }
        }
        this.dl.startDownload();
    }

    private void handleDLMultihoster(final Account account, final DownloadLink link, final String dllink) throws Exception {
        if (StringUtils.isEmpty(dllink)) {
            mhm.handleErrorGeneric(account, link, "dllinknull", 50, 5 * 60 * 1000l);
        }
        link.setProperty(this.getHost() + "directlink", dllink);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            /* 402 - Payment required */
            if (dl.getConnection().getResponseCode() == 402) {
                /* 2019-05-03: E.g. free account[or expired premium], only 1 download per day (?) possible */
                throw new AccountUnavailableException(_GUI.T.account_error_no_traffic_left(), 3 * 60 * 1000l);
            }
            /* Only check for json based errors if response looks to be json. */
            if (br.getRequest().getHtmlCode().startsWith("{")) {
                handleErrorsAPI(account, link, restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP));
            }
            mhm.handleErrorGeneric(account, link, "unknown_dl_error", 10, 5 * 60 * 1000l);
        }
        this.dl.startDownload();
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        String dllink = checkDirectLink(link, this.getHost() + "directlink");
        if (dllink == null) {
            if (account.getType() == AccountType.FREE && !api_supports_free_accounts) {
                /* First try to get saved directurl */
                dllink = this.checkDirectLink(link, this.getHost() + "directurl_selfhosted");
                if (dllink == null) {
                    dllink = generateDllinkWebsiteFreeMode(account, link);
                }
            } else {
                dllink = getDllinkAPI(account, link);
            }
        }
        handleDLMultihoster(account, link, dllink);
    }

    private Map<String, Object> callAPI(final Account account, final DownloadLink link, final Browser br, final String relativeURL) throws Exception {
        return callAPI(account, link, br, relativeURL, true);
    }

    /** Calls API and handles errors if needed. */
    private Map<String, Object> callAPI(final Account account, final DownloadLink link, final Browser br, final String relativeURL, final boolean handleErrors) throws Exception {
        br.getPage(API_BASE + relativeURL);
        try {
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            this.handleErrorsAPI(account, link, entries);
            return entries;
        } catch (final JSonMapperException e) {
            logger.log(e);
            throw new AccountUnavailableException("Invalid API response", 3 * 60 * 1000);
        }
    }

    /**
     * Generated downloadlinks via API.
     *
     * @throws Exception
     */
    private String getDllinkAPI(final Account account, final DownloadLink link) throws Exception {
        this.loginAPI(account, true);
        final Map<String, Object> response = this.callAPI(account, link, br, "/download?token=" + this.getApiToken(account) + "&link=" + Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this)));
        link.setProperty(PROPERTY_MAXCHUNKS, response.get("chunks"));
        return response.get("download").toString();
    }

    /** Generates downloadlinks via website. */
    private String generateDllinkWebsiteFreeMode(final Account account, final DownloadLink link) throws IOException, PluginException, InterruptedException {
        /* 2019-08-24: Not required */
        // this.loginAPI(account);
        /* Try to re-use previously generated URLs so we're not wasting traffic! */
        String internal_url = link.getStringProperty(this.getHost() + "selfhosted_free_download_url", null);
        final boolean generate_new_internal_url;
        if (internal_url != null) {
            br.getPage(internal_url);
            generate_new_internal_url = this.isOffline(this.br);
        } else {
            generate_new_internal_url = true;
        }
        if (generate_new_internal_url) {
            logger.info("Generating new directurl");
            br.getPage("https://" + this.getHost() + "/process?link=" + Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this)));
            handleErrorsWebsite(br, account, link);
            internal_url = br.getRegex("(https?://[^/]+/dl/[A-Z0-9]+)").getMatch(0);
            if (StringUtils.isEmpty(internal_url)) {
                logger.warning("Failed to find generated 'internal_url'");
                if (br.toString().length() <= 100) {
                    /* Assume that http answer == errormessage */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Error: " + br.toString(), 5 * 60 * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to find generated 'internal_url' or unknown error occured", 5 * 60 * 1000l);
                }
            } else if (isOffline(this.br)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            link.setProperty(this.getHost() + "selfhosted_free_download_url", internal_url);
            this.br.getPage(internal_url);
        } else {
            logger.info("Re-using previously generated- and saved directurl");
        }
        return getDllinkWebsiteCaptcha(account, link);
    }

    private String getDllinkWebsiteCaptcha(final Account account, final DownloadLink link) throws IOException, PluginException, InterruptedException {
        /* 2019-08-24: Login is not required */
        // this.loginAPI(account);
        Form captchaform = br.getFormbyActionRegex(".*/redirect.*");
        if (captchaform == null) {
            /* Fallback */
            captchaform = br.getForm(0);
        }
        if (captchaform == null) {
            logger.warning("captchaform is null");
        }
        final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
        captchaform.put("g-recaptcha-response", recaptchaV2Response);
        final Browser brc = br.cloneBrowser();
        brc.setFollowRedirects(false);
        brc.submitForm(captchaform);
        final String dllink = brc.getRedirectLocation();
        if (!StringUtils.isEmpty(dllink)) {
            /* Store directurl */
            link.setProperty(this.getHost() + "directurl_selfhosted", dllink);
        }
        return dllink;
    }

    private boolean isOffline(final Browser br) {
        return br.getHttpConnection().getResponseCode() == 404 || br.containsHTML("Link not found, please re-generate your link");
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        String dllink = link.getStringProperty(property);
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                br2.setFollowRedirects(true);
                con = br2.openHeadConnection(dllink);
                if (this.looksLikeDownloadableContent(con)) {
                    return dllink;
                }
            } catch (final Exception e) {
                logger.log(e);
                return null;
            } finally {
                if (con != null) {
                    con.disconnect();
                }
            }
        }
        return null;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        Map<String, Object> user = loginAPI(account, true);
        if (br.getURL() == null || !br.getURL().contains("/info?token")) {
            user = this.callAPI(account, null, br, "/info?token=" + this.getApiToken(account));
        }
        final Number validuntilO = (Number) user.get("timestamp");
        long validuntil = -1;
        if (validuntilO != null) {
            validuntil = validuntilO.longValue() * 1000;
        }
        /* 2019-07-05: Will usually return 'Unlimited' for premium accounts and 'XX GB' for free accounts */
        final String traffic_leftStr = (String) user.get("traffic_left");
        int filesPerDayLeft = 0;
        /**
         * 2021-01-03: Free (-Account) limits: 5 links per day (per IP and or account). 10 Minute waittime between generating direct-URLs.
         */
        final AccountInfo ai = new AccountInfo();
        if (validuntil > System.currentTimeMillis()) {
            /* Premium account */
            /* Premium accounts have unlimited files per day */
            filesPerDayLeft = -1;
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(Integer.MAX_VALUE);
            ai.setValidUntil(validuntil, this.br);
        } else {
            /**
             * 2019-08-26: API will always return static value '1 GB' trafficleft for free accounts which is wrong! </br>
             *
             * 2019-08-26: FREE (account) Limits are based on IP which means it makes no difference whether we download without account or
             * with a free account! </br>
             *
             * 2023-01-17: Looks like free accounts aren't allowed to download anything anymore.
             */
            final boolean enable_free_account_traffic_workaround = false;
            String accountStatus = "Free account";
            if (enable_free_account_traffic_workaround) {
                /* Try to find correct 'trafficleft' value via website. */
                /* 2021-01-03: Workaround for small serverside bug (returns broken website on first request after first API login) */
                br.clearCookies(null);
                br.getPage(API_BASE + "/info?token=" + this.getApiToken(account));
                this.br.getPage("https://" + this.getHost() + "/home");
                final Regex filesLeftRegex = br.getRegex(">\\s*Files per day\\s*:\\s*</b>\\s*(\\d+)\\s*/\\s*(\\d+)\\s*<br>");
                final Regex trafficRegex = br.getRegex(">\\s*Traffic\\s*:\\s*</b>\\s*([^<>\"]+)\\s*/\\s*([^<>\"]+)<");
                final String trafficLeftStrTmp = trafficRegex.getMatch(0);
                final String trafficMaxStrTmp = trafficRegex.getMatch(1);
                final String filesPerDayUsedStr = filesLeftRegex.getMatch(0);
                final String filesPerDayMaxStr = filesLeftRegex.getMatch(1);
                if (filesPerDayMaxStr != null && filesPerDayUsedStr != null) {
                    filesPerDayLeft = Integer.parseInt(filesPerDayMaxStr) - Integer.parseInt(filesPerDayUsedStr);
                } else {
                    filesPerDayLeft = 0;
                }
                accountStatus += " [" + filesPerDayLeft + " files per day left today]";
            } else {
                ai.setTrafficLeft(0);
                ai.setExpired(true);
            }
            account.setType(AccountType.FREE);
            if (accountStatus != null) {
                ai.setStatus(accountStatus);
            }
            /* 2021-01-03: Usually 10 minutes waittime after every download */
            account.setMaxSimultanDownloads(1);
        }
        if ("Unlimited".equalsIgnoreCase(traffic_leftStr)) {
            /* 2023-01-17: Typically premium accounts got "unlimited" traffic. */
            ai.setUnlimitedTraffic();
        } else {
            /* Check for daily limit of downloadable files. */
            if (filesPerDayLeft == 0) {
                logger.info("Setting ZERO trafficleft because filesPerDayLeft is 0");
                ai.setTrafficLeft(0);
            } else if (filesPerDayLeft > 0) {
                logger.info("filesPerDayLeft > 0 so we should be able to download");
                ai.setUnlimitedTraffic();
            } else {
                /* Parse given traffic value. */
                ai.setTrafficLeft(SizeFormatter.getSize(traffic_leftStr));
            }
        }
        /**
         * Same list on website: https://neodebrid.com/status
         */
        final Map<String, Object> entries = this.callAPI(account, null, br, "/status");
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        final List<Map<String, Object>> hosters = (List<Map<String, Object>>) entries.get("result");
        for (final Map<String, Object> hosterinfo : hosters) {
            final String domain = (String) hosterinfo.get("host");
            final MultiHostHost mhost = new MultiHostHost(domain);
            if (!"online".equalsIgnoreCase(hosterinfo.get("status").toString())) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    private Map<String, Object> loginAPI(final Account account, final boolean validateToken) throws Exception {
        synchronized (account) {
            String api_token = getApiToken(account);
            Map<String, Object> entries = null;
            if (api_token != null) {
                if (!validateToken) {
                    return null;
                }
                logger.info("Checking existing token...");
                try {
                    entries = this.callAPI(account, null, br, "/info?token=" + this.getApiToken(account), false);
                    logger.info("Stored token was valid");
                    return entries;
                } catch (final PluginException ignore) {
                    /**
                     * E.g. {"status":"error","reason":"Session expired. Please log-in again."} or </br>
                     * {"status":"error","reason":"Token not found."}
                     */
                    logger.info("Stored token was INVALID, performing full login");
                    br.clearCookies(null);
                    account.removeProperty(PROPERTY_ACCOUNT_api_token);
                }
            }
            entries = this.callAPI(account, null, br, "/login?email=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()), false);
            api_token = (String) entries.get("api_token");
            if (StringUtils.isEmpty(api_token)) {
                /* This should never happen! */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            account.setProperty(PROPERTY_ACCOUNT_api_token, api_token);
            return entries;
        }
    }

    /** Returns stored API token (login session token) */
    private String getApiToken(final Account account) {
        return account.getStringProperty(PROPERTY_ACCOUNT_api_token);
    }

    private void handleErrorsAPI(final Account account, final DownloadLink link, final Map<String, Object> entries) throws PluginException, InterruptedException {
        final String status = (String) entries.get("status");
        final String errorStr = (String) entries.get("reason");
        if (status == null || "success".equalsIgnoreCase(status)) {
            /* No error */
            return;
        }
        /* First check for all errors which mean that account is invalid/permanently not available (e.g. wrong login credentials). */
        if (errorStr.matches("(?i)Wrong credentials.*")) {
            throw new AccountInvalidException(errorStr);
        } else if (errorStr.matches("(?i)IP Blocked.*")) {
            throw new AccountUnavailableException(errorStr, 5 * 60 * 1000l);
        } else if (errorStr.matches("(?i)Filehost not supported.*")) {
            mhm.putError(account, link, 5 * 60 * 1000l, errorStr);
        } else if (errorStr.matches("(?i)Token not found.*")) {
            logger.info("api_token has expired --> Deleting it - and trying again");
            account.removeProperty(PROPERTY_ACCOUNT_api_token);
            throw new PluginException(LinkStatus.ERROR_RETRY);
        } else if (errorStr.matches("(?i)User not premium.*")) {
            logger.info("User has a free account and plugin tried to download from premium-only host");
            mhm.putError(account, link, 5 * 60 * 1000l, "This host is not supported via free account");
        } else {
            logger.info("Unknown API error happened");
            if (link == null) {
                /* Must be account related error */
                throw new AccountUnavailableException(errorStr, 3 * 60 * 1000l);
            } else {
                mhm.handleErrorGeneric(account, link, errorStr, 50, 5 * 60 * 1000l);
            }
        }
    }

    private void handleErrorsWebsite(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        /** 2019-08-24: TODO */
        if (br.containsHTML("Error: Maximum filesize for free users")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Max freeuser filesize reached");
        }
    }
}