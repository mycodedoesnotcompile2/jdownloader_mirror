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
import java.util.concurrent.TimeUnit;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
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

@HostPlugin(revision = "$Revision: 51773 $", interfaceVersion = 3, names = { "cboxera.com" }, urls = { "" })
public class CboxeraCom extends PluginForHost {
    private static final String          API_BASE            = "https://api.cboxera.com";
    /* 2020-03-24: Static implementation as key is nowhere to be found via API request. */
    private static final String          RECAPTCHAv2_SITEKEY = "6Ldq4FwUAAAAAJ81U4lQEvQXps384V7eCWJWxdjf";
    private static MultiHosterManagement mhm                 = new MultiHosterManagement("cboxera.com");
    private static final String          PROPERTY_logintoken = "token";
    private static final String          PROPERTY_directlink = "directlink";

    @SuppressWarnings("deprecation")
    public CboxeraCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/premium");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(new int[] { 401 });
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/terms";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        /* handle premium should never get called */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    private void handleDL(final Account account, final DownloadLink link) throws Exception {
        final String directlinkproperty = this.getHost() + PROPERTY_directlink;
        final String storedDirectlink = link.getStringProperty(directlinkproperty);
        String dllink = null;
        if (storedDirectlink != null) {
            logger.info("Re-using stored directlink: " + storedDirectlink);
            dllink = storedDirectlink;
        } else {
            this.loginAPI(account, false);
            br.postPageRaw(API_BASE + "/private/generatelink", String.format("{\"link\":\"%s\"}", link.getDefaultPlugin().buildExternalDownloadURL(link, this)));
            final Map<String, Object> entries = (Map<String, Object>) this.handleErrorsAPI(br, account, link);
            dllink = entries.get("dlink").toString();
            if (AccountType.FREE.equals(account.getType())) {
                /**
                 * For free users, they're trying to send them over an URL shortener to display ads. <br>
                 * Let's try to bypass that.
                 */
                final UrlQuery query = UrlQuery.parse(dllink);
                final String public_file_download_id = query.get("id");
                if (public_file_download_id == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                br.getPage(API_BASE + "/public/download-info/?id=" + public_file_download_id);
                final Map<String, Object> dlresp = (Map<String, Object>) this.handleErrorsAPI(br, account, link);
                /* Typically this: https://ouo.io/qs/XXXyyy?s=https://..." */
                final String ad_url = dlresp.get("dlink").toString();
                final UrlQuery ad_url_query = UrlQuery.parse(ad_url);
                final String real_final_url = ad_url_query.get("s");
                if (StringUtils.isEmpty(real_final_url) || !StringUtils.startsWithCaseInsensitive(real_final_url, "http")) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                dllink = real_final_url;
            }
        }
        try {
            /*
             * Connection limits: 2020-03-24: According to API docs "Max Connections: 15 per user/minute" --> WTF --> Set it to unlimited
             * for now
             */
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), 0);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                handleErrorsAPI(this.br, account, link);
                mhm.handleErrorGeneric(account, link, "unknown_dl_error", 20, 5 * 60 * 1000l);
            }
        } catch (final Exception e) {
            if (storedDirectlink != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirectlink == null) {
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        handleDL(account, link);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        Map<String, Object> entries = loginAPI(account, true);
        if (br.getURL() == null || !StringUtils.containsIgnoreCase(br.getURL(), "/private/user/info")) {
            br.getPage(API_BASE + "/private/user/info");
            entries = (Map<String, Object>) handleErrorsAPI(br, account, null);
        }
        /* 2020-03-24: E.g. free account: "subscription":{"is_vip":false,"bw_limit":"25GB","days":0} */
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> subscription = (Map<String, Object>) entries.get("subscription");
        final String trafficleft = (String) subscription.get("bw_limit");
        final String account_type_key;
        if (Boolean.TRUE.equals(subscription.get("is_vip"))) {
            account.setType(AccountType.PREMIUM);
            final int premium_days_left = ((Number) subscription.get("days")).intValue();
            final long validuntil = System.currentTimeMillis() + TimeUnit.DAYS.toMillis(premium_days_left);
            ai.setValidUntil(validuntil, this.br);
            account_type_key = "vip";
        } else {
            account.setType(AccountType.FREE);
            account_type_key = "free";
        }
        ai.setTrafficLeft(SizeFormatter.getSize(trafficleft));
        br.getPage(API_BASE + "/public/host-status");
        final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) this.handleErrorsAPI(br, account, null);
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (Map<String, Object> hostinfo : ressourcelist) {
            final String host = hostinfo.get("name").toString();
            /* Get host information for current account type. */
            hostinfo = (Map<String, Object>) hostinfo.get(account_type_key);
            final MultiHostHost mhost = new MultiHostHost(host);
            if ("no".equalsIgnoreCase(hostinfo.get("supported").toString())) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            try {
                /* Given in this format: "5 GB" */
                final String size_limitStr = (String) hostinfo.get("size_limit");
                final String bandwidth_limitStr = (String) hostinfo.get("bandwidth_limit");
                final long size_limit = SizeFormatter.getSize(size_limitStr);
                final long bandwidth_limit = SizeFormatter.getSize(bandwidth_limitStr);
                mhost.setTrafficLeft(Math.min(size_limit, bandwidth_limit));
            } catch (final Exception ignore) {
                /* Ignore this */
                logger.info("Detected host with unexpected limit information: " + host);
            }
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    private Map<String, Object> loginAPI(final Account account, final boolean checkAuthToken) throws IOException, PluginException, InterruptedException {
        String token = account.getStringProperty(PROPERTY_logintoken);
        if (token != null) {
            logger.info("Attempting token login");
            br.getHeaders().put("Authorization", "Bearer " + token);
            /*
             * 2020-03-24: No idea how long their token is supposed to last but I guess it should be permanent because a full login requires
             * a captcha!
             */
            if (!checkAuthToken) {
                /* Do not check token */
                return null;
            }
            br.getPage(API_BASE + "/private/user/info");
            try {
                final Map<String, Object> entries = (Map<String, Object>) this.handleErrorsAPI(br, account, null);
                /* No exception -> Success */
                logger.info("Token login successful");
                return entries;
            } catch (final PluginException e) {
                logger.info("Token login failed");
                br.getHeaders().put("Authorization", "");
            }
        }
        logger.info("Performing full login");
        /* 2020-03-24: This is their reCaptchaV2 domain which means this needs to be accessed to be able to solve the captcha! */
        // br.getPage("https://www." + this.getHost() + "/login/");
        /* Set request so we do not actually have to call the website in this plugin which is using the API for 100% of all requests! */
        br.setRequest(br.createGetRequest("https://www." + getHost() + "/login/"));
        final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, RECAPTCHAv2_SITEKEY).getToken();
        final String postData = String.format("{\"email\": \"%s\",\"password\": \"%s\",\"token\":\"%s\"}", account.getUser(), account.getPass(), recaptchaV2Response);
        br.postPageRaw(API_BASE + "/public/login", postData);
        final Map<String, Object> entries = (Map<String, Object>) handleErrorsAPI(br, account, null);
        token = entries.get("token").toString();
        if (StringUtils.isEmpty(token)) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        account.setProperty(PROPERTY_logintoken, token);
        /* We don't really need the cookies but the timestamp ;) */
        account.saveCookies(br.getCookies(br.getHost()), "");
        br.getHeaders().put("Authorization", "Bearer " + token);
        return (Map<String, Object>) this.handleErrorsAPI(br, account, null);
    }

    private Object handleErrorsAPI(final Browser br, final Account account, final DownloadLink link) throws PluginException {
        Map<String, Object> entries = null;
        try {
            final Object resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
            if (resp != null && !(resp instanceof Map)) {
                /* No error */
                return resp;
            }
            entries = (Map<String, Object>) resp;
        } catch (final JSonMapperException jme) {
            /* This should never happen. */
            final String msg = "Invalid API response";
            final long waitMillis = 60 * 1000l;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, waitMillis);
            } else {
                throw new AccountUnavailableException(msg, 60 * 1000);
            }
        }
        /**
         * On download attempt, response may contain additional information: <br>
         * {"error":true,"link":"https://...","msg":"Link checker service error. Please try again."}
         *
         */
        final Object errorO = entries.get("error");
        if (Boolean.FALSE.equals(errorO)) {
            /* No error -> Check for this case just in case there can be a "msg" field while the response is not an error response. */
            return entries;
        }
        String errormsg = (String) entries.get("msg");
        if (StringUtils.isEmpty(errormsg)) {
            errormsg = (String) entries.get("status");
        }
        if (StringUtils.isEmpty(errormsg)) {
            /* No error */
            return entries;
        }
        if (errormsg.equalsIgnoreCase("No Authorization was found") || errormsg.equalsIgnoreCase("Invalid Password")) {
            /* Usually goes along with http response 401 */
            throw new AccountInvalidException(errormsg);
        } else if (errormsg.equalsIgnoreCase("Invalid token")) {
            /* Existing session expired. */
            /* Usually goes along with http response 401. Temp. disable account so token can be refreshed on next account check! */
            throw new AccountUnavailableException(errormsg, 1 * 60 * 1000l);
        }
        final PluginEnvironment pe = this.getPluginEnvironment();
        if (PluginEnvironment.ACCOUNT_CHECK.equals(pe)) {
            /* Error happened during account check -> Is account related */
            throw new AccountInvalidException(errormsg);
        }
        /* All following errors shall be download related */
        final Regex error_wait = new Regex(errormsg, "You can post your link after (\\d+) minutes? (\\d+) seconds?");
        if (error_wait.patternFind()) {
            /* Single link error */
            final String minutesStr = error_wait.getMatch(0);
            final String secondsStr = error_wait.getMatch(1);
            final long wait = Long.parseLong(minutesStr) * 60 * 1001 + Long.parseLong(secondsStr) * 1001;
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, wait);
        }
        if (StringUtils.containsIgnoreCase(errormsg, "Your Link is not supported")) {
            /* {"error":true,"link":"https://...","msg":"Your Link is not supported. Check supported hosts @host-status page."} */
            mhm.putError(account, link, 5 * 60 * 1000l, errormsg);
        }
        /* Check response code errors */
        if (br.getHttpConnection().getResponseCode() == 401) {
            /* Typically with: {"status":"invalid token"} */
            throw new AccountInvalidException();
        }
        /* Unknown/other errors */
        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, 5 * 60 * 1000);
    }
}