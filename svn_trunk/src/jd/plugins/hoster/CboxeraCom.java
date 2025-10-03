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

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
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
import jd.plugins.components.PluginJSonUtils;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@HostPlugin(revision = "$Revision: 51606 $", interfaceVersion = 3, names = { "cboxera.com" }, urls = { "" })
public class CboxeraCom extends PluginForHost {
    private static final String          API_BASE            = "https://api.cboxera.com";
    /* 2020-03-24: Static implementation as key is nowhere to be found via API request. */
    private static final String          RECAPTCHAv2_SITEKEY = "6Ldq4FwUAAAAAJ81U4lQEvQXps384V7eCWJWxdjf";
    private static MultiHosterManagement mhm                 = new MultiHosterManagement("cboxera.com");
    /* Connection limits: 2020-03-24: According to API docs "Max Connections: 15 per user/minute" --> WTF --> Set it to unlimited for now */
    private static final int             defaultMAXCHUNKS    = 0;
    private static final boolean         defaultRESUME       = true;
    private static final String          PROPERTY_logintoken = "token";
    private static final String          PROPERTY_directlink = "directlink";

    @SuppressWarnings("deprecation")
    public CboxeraCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/premium");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
        // return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/terms";
    }

    private Browser prepBR(final Browser br) {
        br.setCookiesExclusive(true);
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(new int[] { 401 });
        return br;
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
        String dllink = checkDirectLink(link, this.getHost() + PROPERTY_directlink);
        br.setFollowRedirects(true);
        if (dllink == null) {
            this.loginAPI(account, false);
            br.postPageRaw(API_BASE + "/private/generatelink", String.format("{\"link\":\"%s\"}", link.getDefaultPlugin().buildExternalDownloadURL(link, this)));
            dllink = PluginJSonUtils.getJsonValue(br, "dlink");
            if (StringUtils.isEmpty(dllink)) {
                handleErrors(this.br, account, link);
                mhm.handleErrorGeneric(account, link, "dllinknull", 50, 5 * 60 * 1000l);
            }
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, defaultRESUME, defaultMAXCHUNKS);
        link.setProperty(this.getHost() + PROPERTY_directlink, dl.getConnection().getURL().toString());
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            handleErrors(this.br, account, link);
            mhm.handleErrorGeneric(account, link, "unknown_dl_error", 20, 5 * 60 * 1000l);
        }
        this.dl.startDownload();
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        prepBR(this.br);
        handleDL(account, link);
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        String dllink = link.getStringProperty(property);
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                br2.setFollowRedirects(true);
                con = br2.openHeadConnection(dllink);
                if (!this.looksLikeDownloadableContent(con)) {
                    throw new IOException();
                } else {
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
        } else {
            return null;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        prepBR(this.br);
        final AccountInfo ai = new AccountInfo();
        loginAPI(account, true);
        if (br.getURL() == null || !br.getURL().contains("/private/user/info")) {
            br.getPage(API_BASE + "/private/user/info");
            handleErrors(br, account, null);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        /* 2020-03-24: E.g. free account: "subscription":{"is_vip":false,"bw_limit":"25GB","days":0} */
        final Map<String, Object> subscription = (Map<String, Object>) entries.get("subscription");
        boolean is_premium = ((Boolean) subscription.get("is_vip"));
        final String trafficleft = (String) subscription.get("bw_limit");
        if (!is_premium) {
            account.setType(AccountType.FREE);
        } else {
            account.setType(AccountType.PREMIUM);
            final long premium_days_left = JavaScriptEngineFactory.toLong(subscription.get("days"), -1);
            final long validuntil = System.currentTimeMillis() + TimeUnit.DAYS.toMillis(premium_days_left);
            ai.setValidUntil(validuntil, this.br);
        }
        ai.setTrafficLeft(SizeFormatter.getSize(trafficleft));
        br.getPage(API_BASE + "/public/host-status");
        final String account_type_key;
        if (account.getType() == AccountType.FREE) {
            account_type_key = "free";
        } else {
            account_type_key = "vip";
        }
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
        for (Map<String, Object> hostinfo : ressourcelist) {
            final String host = (String) hostinfo.get("name");
            if (StringUtils.isEmpty(host)) {
                /* This should never happen */
                continue;
            }
            /* Get host information for on current account type. */
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
            } catch (final Throwable ignore) {
                /* Ignore this */
            }
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    private void loginAPI(final Account account, final boolean forceAuthCheck) throws IOException, PluginException, InterruptedException {
        String token = account.getStringProperty(PROPERTY_logintoken);
        if (token != null) {
            logger.info("Attempting token login");
            br.getHeaders().put("Authorization", "Bearer " + token);
            /*
             * 2020-03-24: No idea how long their token is supposed to last but I guess it should be permanent because a full login requires
             * a captcha!
             */
            if (!forceAuthCheck) {
                /* We trust our token --> Do not check them */
                logger.info("Trust login token");
                return;
            }
            br.getPage(API_BASE + "/private/user/info");
            if (br.getHttpConnection().getResponseCode() == 200) {
                logger.info("Token login successful");
                /* We don't really need the cookies but the timestamp ;) */
                account.saveCookies(br.getCookies(br.getHost()), "");
                return;
            } else {
                /* Most likely 401 unauthorized */
                logger.info("Token login failed");
                br.clearAll();
            }
        }
        if (StringUtils.isEmpty(account.getUser()) || !account.getUser().matches(".+@.+\\..+")) {
            if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nBitte gib deine E-Mail Adresse ins Benutzername Feld ein!", PluginException.VALUE_ID_PREMIUM_DISABLE);
            } else {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nPlease enter your e-mail address in the username field!", PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
        }
        /* Drop previous headers & cookies */
        logger.info("Performing full login");
        this.prepBR(this.br);
        /* 2020-03-24: This is their reCaptchaV2 domain which means this needs to be accessed to be able to solve the captcha! */
        // br.getPage("https://www." + this.getHost() + "/login/");
        /* Set request so we do not actually have to call the website in this plugin which is using the API for 100% of all requests! */
        br.setRequest(br.createGetRequest("https://www." + this.getHost() + "/login/"));
        final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br, RECAPTCHAv2_SITEKEY).getToken();
        final String postData = String.format("{\"email\": \"%s\",\"password\": \"%s\",\"token\":\"%s\"}", account.getUser(), account.getPass(), recaptchaV2Response);
        br.postPageRaw(API_BASE + "/public/login", postData);
        token = PluginJSonUtils.getJson(br, "token");
        if (StringUtils.isEmpty(token)) {
            handleErrors(br, account, null);
            /* This should never happen - do not permanently disable accounts for unexpected login errors! */
            throw new PluginException(LinkStatus.ERROR_PREMIUM, "Unknown login failure", PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
        }
        account.setProperty(PROPERTY_logintoken, token);
        /* We don't really need the cookies but the timestamp ;) */
        account.saveCookies(br.getCookies(br.getHost()), "");
        br.getHeaders().put("Authorization", "Bearer " + token);
    }

    private void handleErrors(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        final String errormsg = getErrormessage(br);
        if (!StringUtils.isEmpty(errormsg)) {
            if (errormsg.equalsIgnoreCase("No Authorization was found") || errormsg.equalsIgnoreCase("Invalid Password")) {
                /* Usually goes along with http response 401 */
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            } else if (errormsg.equalsIgnoreCase("Invalid token")) {
                /* Existing session expired. */
                /* Usually goes along with http response 401. Temp. disable account so token can be refreshed on next account check! */
                throw new AccountUnavailableException(errormsg, 1 * 60 * 1000l);
            } else if (new Regex(errormsg, "You can post your link after \\d+ minutes \\d+ seconds").matches()) {
                final String minutesStr = new Regex(errormsg, "(\\d+)\\s*minutes?").getMatch(0);
                final String secondsStr = new Regex(errormsg, "(\\d+)\\s*seconds?").getMatch(0);
                final long wait = Long.parseLong(minutesStr) * 60 * 1001 + Long.parseLong(secondsStr) * 1001;
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, wait);
            }
            /*
             * All other errors e.g.: {"error":true,"msg":"Free Users allowed maximum 500 MB filesize for uploaded.net"} --> This particular
             * example should never happen, see function canHandle
             */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, 5 * 60 * 1000);
        }
    }

    private String getErrormessage(final Browser br) {
        String errormsg = null;
        try {
            final Map<String, Object> entries = restoreFromString(br.toString(), TypeRef.MAP);
            errormsg = (String) entries.get("msg");
        } catch (final Throwable e) {
        }
        return errormsg;
    }
}