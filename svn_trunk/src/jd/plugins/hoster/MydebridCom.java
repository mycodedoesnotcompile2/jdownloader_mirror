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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.components.antiDDoSForHost;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.host.PluginFinder;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.PluginException;
import jd.plugins.components.MultiHosterManagement;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 50050 $", interfaceVersion = 3, names = { "mydebrid.com" }, urls = { "" })
public class MydebridCom extends antiDDoSForHost {
    /* Documentation: https://api.mydebrid.com/v1/ */
    private static final String                  API_BASE             = "https://api.mydebrid.com/v1";
    private static MultiHosterManagement         mhm                  = new MultiHosterManagement("mydebrid.com");
    private static LinkedHashMap<String, Object> individualHostLimits = new LinkedHashMap<String, Object>();
    private static final int                     defaultMAXDOWNLOADS  = -1;
    private static final int                     defaultMAXCHUNKS     = 1;
    private static final boolean                 defaultRESUME        = true;
    private static final String                  PROPERTY_logintoken  = "token";
    private static final String                  PROPERTY_directlink  = "directlink";

    public MydebridCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/sign-up");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setAllowedResponseCodes(new int[] { 400 });
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms-of-service";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            return false;
        } else {
            mhm.runCheck(account, link);
            return super.canHandle(link, account);
        }
    }

    @Override
    public void handleFree(final DownloadLink downloadLink) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        /* handle premium should never get called */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    private void handleDL(final Account account, final DownloadLink link) throws Exception {
        final String directlinkproperty = this.getHost() + PROPERTY_directlink;
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        String dllink;
        if (storedDirecturl != null) {
            logger.info("Trying to re-use stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            this.loginAPI(account, false);
            final String token = account.getStringProperty(PROPERTY_logintoken);
            postPage(API_BASE + "/get-download-url", String.format("token=%s&fileUrl=%s", Encoding.urlEncode(token), Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this))));
            final Map<String, Object> entries = handleErrors(br, account, null);
            dllink = (String) entries.get("downloadUrl");
            if (StringUtils.isEmpty(dllink)) {
                /* This should never happen */
                mhm.handleErrorGeneric(account, link, "Failed to find final downloadurl", 50, 5 * 60 * 1000l);
            }
        }
        boolean resume = defaultRESUME;
        int maxchunks = defaultMAXCHUNKS;
        if (individualHostLimits.containsKey(link.getHost())) {
            final Map<String, Integer> limitMap = (Map<String, Integer>) individualHostLimits.get(link.getHost());
            resume = limitMap.get("resumable") == 1 ? true : false;
            maxchunks = limitMap.get("max_chunks").intValue();
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, resume, maxchunks);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection();
                handleErrors(this.br, account, link);
                mhm.handleErrorGeneric(account, link, "Unknown download error", 20, 5 * 60 * 1000l);
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirecturl == null) {
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        this.dl.startDownload();
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        mhm.runCheck(account, link);
        handleDL(account, link);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        Map<String, Object> entries = loginAPI(account, true);
        final String token = account.getStringProperty(PROPERTY_logintoken);
        if (br.getURL() == null || !br.getURL().contains("/account-status")) {
            this.postPage(API_BASE + "/account-status", "token=" + Encoding.urlEncode(token));
            entries = handleErrors(br, account, null);
        }
        final AccountInfo ai = new AccountInfo();
        boolean is_premium = "premium".equalsIgnoreCase((String) entries.get("accountType"));
        /* 2020-05-06: This will usually return "unlimited" */
        // final String trafficleft = (String) entries.get("remainingTraffic");
        if (!is_premium) {
            /* Assume free accounts cannot be used to download anything */
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(defaultMAXDOWNLOADS);
            ai.setTrafficLeft(0);
            ai.setExpired(true);
        } else {
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(defaultMAXDOWNLOADS);
            final String expireDate = (String) entries.get("expiryDate");
            if (!StringUtils.isEmpty(expireDate)) {
                ai.setValidUntil(TimeFormatter.getMilliSeconds(expireDate, "MM-dd-yyyy", Locale.ENGLISH), this.br);
            }
            ai.setUnlimitedTraffic();
        }
        postPage(API_BASE + "/get-hosts", "token=" + token);
        final Map<String, Object> get_hosts_resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) get_hosts_resp.get("hosts");
        final PluginFinder finder = new PluginFinder();
        for (final Map<String, Object> hostinfo : ressourcelist) {
            final String domain = hostinfo.get("name").toString();
            final MultiHostHost mhost = new MultiHostHost(domain);
            /* These fields contain either the string "unlimited" */
            final Object trafficMaxO = hostinfo.get("dailyLimit");
            final Object trafficLeftO = hostinfo.get("remaining");
            if (trafficLeftO instanceof Number) {
                mhost.setTrafficLeft(((Number) trafficLeftO).longValue());
            }
            if (trafficMaxO instanceof Number) {
                mhost.setTrafficMax(((Number) trafficMaxO).longValue());
            }
            mhost.setMaxDownloads(((Number) hostinfo.get("maxDownloads")).intValue());
            int max_chunks = ((Number) hostinfo.get("maxChunks")).intValue();
            if (max_chunks <= 0) {
                max_chunks = 1;
            } else if (max_chunks > 1) {
                max_chunks = -max_chunks;
            }
            final boolean canResume = ((Boolean) hostinfo.get("resumable")).booleanValue();
            final int resumable;
            if (canResume) {
                resumable = 1;
            } else {
                resumable = 0;
            }
            final String originalHost = finder.assignHost(domain);
            if (originalHost == null) {
                /* This should never happen */
                logger.info("Skipping host because failed to find supported/original host: " + domain);
                continue;
            }
            supportedhosts.add(mhost);
            final LinkedHashMap<String, Integer> limits = new LinkedHashMap<String, Integer>();
            limits.put("max_chunks", max_chunks);
            limits.put("resumable", resumable);
            individualHostLimits.put(originalHost, limits);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    private Map<String, Object> loginAPI(final Account account, final boolean forceAuthCheck) throws Exception {
        String token = account.getStringProperty(PROPERTY_logintoken);
        Map<String, Object> entries = null;
        if (token != null) {
            logger.info("Attempting token login");
            if (!forceAuthCheck) {
                /* Do not validate token */
                return null;
            }
            this.postPage(API_BASE + "/account-status", "token=" + Encoding.urlEncode(token));
            if (br.getHttpConnection().getResponseCode() == 200) {
                logger.info("Token login successful");
                /* We don't really need the cookies but the timestamp ;) */
                entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                return entries;
            } else {
                /* Most likely 401 unauthorized */
                logger.info("Token login failed");
                br.clearCookies(null);
                account.removeProperty(PROPERTY_logintoken);
            }
        }
        /* Drop previous headers & cookies */
        logger.info("Performing full login");
        final String postData = String.format("username=%s&password=%s", Encoding.urlEncode(account.getUser()), Encoding.urlEncode(account.getPass()));
        postPage(API_BASE + "/login", postData);
        entries = handleErrors(br, account, null);
        token = PluginJSonUtils.getJson(br, "token");
        if (StringUtils.isEmpty(token)) {
            /* This should never happen - do not permanently disable accounts for unexpected login errors! */
            throw new PluginException(LinkStatus.ERROR_PREMIUM, "Unknown login failure", PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
        }
        account.setProperty(PROPERTY_logintoken, token);
        return entries;
    }

    /** Handles errors according to: https://api.mydebrid.com/v1/#errors */
    private Map<String, Object> handleErrors(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException jme) {
            final String errortext = "Bad API response";
            if (link != null) {
                mhm.handleErrorGeneric(account, this.getDownloadLink(), errortext, 50, 5 * 60 * 1000l);
            } else {
                throw Exceptions.addSuppressed(new AccountUnavailableException(errortext, 1 * 60 * 1000l), jme);
            }
        }
        if (Boolean.TRUE.equals(entries.get("success"))) {
            /* No error */
            return entries;
        }
        final String errormsg = entries.get("error").toString();
        if (errormsg.equalsIgnoreCase("INVALID_CREDENTIALS")) {
            /* Usually goes along with http response 400 */
            throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
        } else if (errormsg.equalsIgnoreCase("TOKEN_EXPIRED") || errormsg.equalsIgnoreCase("INVALID_TOKEN")) {
            /* Existing session expired. */
            throw new AccountUnavailableException(errormsg, 1 * 60 * 1000l);
        } else if (errormsg.equalsIgnoreCase("LIMIT_EXCEEDED")) {
            /* Limit of individual host reached/exceeded */
            mhm.putError(account, link, 5 * 60 * 1000l, errormsg);
        } else if (errormsg.equalsIgnoreCase("HOST_UNAVAILABLE")) {
            mhm.putError(account, link, 5 * 60 * 1000l, errormsg);
        } else if (errormsg.equalsIgnoreCase("FILE_NOT_FOUND")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (errormsg.equalsIgnoreCase("INVALID_URL")) {
            /* This should never happen (?) --> Retry */
            mhm.handleErrorGeneric(account, link, errormsg, 50);
        } else {
            /* Handle all other errors e.g. "MISSING_PARAMS" or "Error processing input" or any other unknown error */
            if (link == null) {
                /* No DownloadLink available --> Handle as account error */
                throw new AccountUnavailableException(errormsg, 10 * 60 * 1000l);
            } else {
                mhm.handleErrorGeneric(account, link, errormsg, 50);
            }
        }
        /* This code should never be reached */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }
}