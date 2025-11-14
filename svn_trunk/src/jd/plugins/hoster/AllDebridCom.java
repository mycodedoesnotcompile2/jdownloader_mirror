//    jDownloader - Downloadmanager
//    Copyright (C) 2015  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.WeakHashMap;

import javax.swing.JComponent;
import javax.swing.JLabel;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.linkcrawler.CheckableLink;
import jd.gui.swing.components.linkbutton.JLink;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DefaultEditAccountPanelAPIKeyLogin;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.PluginProgress;
import jd.plugins.components.MultiHosterManagement;
import jd.plugins.download.DownloadInterface;
import jd.plugins.download.DownloadLinkDownloadable;
import jd.plugins.download.HashInfo;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtPasswordField;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.parser.UrlQuery;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.InputChangedCallbackInterface;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.downloads.columns.ETAColumn;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.PluginTaskID;
import org.jdownloader.plugins.accounts.AccountBuilderInterface;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@HostPlugin(revision = "$Revision: 51818 $", interfaceVersion = 3, names = { "alldebrid.com" }, urls = { "https?://alldebrid\\.com/f/([A-Za-z0-9\\-_]+)" })
public class AllDebridCom extends PluginForHost {
    public AllDebridCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/offer/");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader " + getVersion());
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/tos/";
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return "alldebrid://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    protected boolean supportsUpdateDownloadLink(CheckableLink checkableLink) {
        return checkableLink != null && checkableLink.getDownloadLink() != null;
    }

    private boolean isSelfhosted(final DownloadLink link) {
        if (link != null && canHandle(link.getPluginPatternMatcher())) {
            return true;
        } else {
            return false;
        }
    }

    private static MultiHosterManagement                 mhm                                = new MultiHosterManagement("alldebrid.com");
    private static WeakHashMap<Account, HashSet<String>> RATE_LIMITED                       = new WeakHashMap<Account, HashSet<String>>();
    @Deprecated
    public static final String                           api_base_v_40                      = "https://api.alldebrid.com/v4";
    public static final String                           api_base_v_41                      = "https://api.alldebrid.com/v4.1";
    private static final String                          PROPERTY_apikey                    = "apiv4_apikey";
    private final String                                 PROPERTY_maxchunks                 = "alldebrid_maxchunks";
    private static final String                          ERROR_CODE_LINK_PASSWORD_PROTECTED = "LINK_PASS_PROTECTED";

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        if (account.getPass() != null && !this.looksLikeValidAPIKey(account.getPass())) {
            /*
             * Do not store the users' real password! The only thing we need is the users' apikey.
             */
            logger.info("Nullifying password field as it contains a non-apikey value");
            account.setPass(null);
        }
        final AccountInfo ai = new AccountInfo();
        login(account, ai, true);
        /* They got 3 arrays of types of supported websites --> We want to have the "hosts" Array only! */
        final boolean includeStreamingItems = false;
        final Map<String, Object> data;
        if (includeStreamingItems) {
            /* Warning! Old endpoint! Do not use anymore! */
            br.getPage(api_base_v_40 + "/user/hosts");
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            data = (Map<String, Object>) entries.get("data");
        } else {
            data = this.callAPI("/user/hosts", account, null);
        }
        final String[] allowedServiceTypes = new String[] { "hosts", "streams" };
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        final HashSet<String> streamDomains = new HashSet<String>();
        for (final String serviceType : allowedServiceTypes) {
            final Object supportedHostsInfoO = data.get(serviceType);
            if (supportedHostsInfoO == null) {
                continue;
            } else if (!(supportedHostsInfoO instanceof Map)) {
                /* When "&hostsOnly=true" is used, "streams" is an empty list. */
                continue;
            }
            final Map<String, Object> supportedHostsInfo = (Map<String, Object>) supportedHostsInfoO;
            final Iterator<Entry<String, Object>> iterator = supportedHostsInfo.entrySet().iterator();
            boolean turbobitWorkaroundDone = false;
            while (iterator.hasNext()) {
                final Entry<String, Object> entry = iterator.next();
                // final String hosterKey = entry.getKey();
                final Map<String, Object> hosterinfos = (Map<String, Object>) entry.getValue();
                final String host_without_tld = hosterinfos.get("name").toString();
                final Number quota = (Number) hosterinfos.get("quota");
                final Number quotaMax = (Number) hosterinfos.get("quotaMax");
                final String quotaType = (String) hosterinfos.get("quotaType");
                final List<String> domains = (List<String>) hosterinfos.get("domains");
                final MultiHostHost mhost = new MultiHostHost();
                mhost.setName(host_without_tld);
                mhost.setDomains(domains);
                /*
                 * 2020-04-01: This check will most likely never be required as free accounts officially cannot be used via API at all and
                 * JD also does not accept them but we're doing this check nevertheless.
                 */
                if (Boolean.FALSE.equals(hosterinfos.get("status"))) {
                    /* Hosts flaggedas not working may still be working thus we will just flag them as unstable. */
                    // mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                    mhost.setStatus(MultihosterHostStatus.WORKING_UNSTABLE);
                } else if (account.getType() == AccountType.FREE && !"free".equalsIgnoreCase(hosterinfos.get("type").toString())) {
                    mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST_NOT_FOR_THIS_ACCOUNT_TYPE);
                    logger.info("This host cannot be used with current account type: " + host_without_tld);
                }
                /* Set individual host limits */
                if (quotaType != null && quota != null && quotaMax != null) {
                    if (quotaType.equalsIgnoreCase("traffic")) {
                        /* traffic means traffic in MB */
                        mhost.setTrafficLeft(quota.longValue() * 1024 * 1024);
                        mhost.setTrafficMax(quotaMax.longValue() * 1024 * 1024);
                    } else if (quotaType.equalsIgnoreCase("nb_download")) {
                        /* nb_download means number of links left to download */
                        final long linksLeft = quota.longValue();
                        final long linksMax = quotaMax.longValue();
                        // -1 = Unlimited
                        if (linksLeft != -1 && linksMax != -1) {
                            mhost.setLinksLeft(linksLeft);
                            mhost.setLinksMax(linksMax);
                        }
                    } else {
                        // No limit or unsupported type of limit
                        logger.warning("Got unknown limit type: " + quotaType);
                    }
                }
                if (serviceType.equals("streams")) {
                    mhost.setStatusText("Stream service");
                    /* Collect stream domains for filtering later on. */
                    streamDomains.addAll(mhost.getDomains());
                }
                if (host_without_tld.equals("turbobit") && domains.contains("hitfile.net")) {
                    /*
                     * Workaround for them putting turbobit.net and hitfile.net into one entry but this way upper handling would only detect
                     * one of them.
                     */
                    if (turbobitWorkaroundDone) {
                        /* This should never happen. */
                        logger.info("WTF: Looks like API list of supported host contains Turbobit/Hitfile item twice");
                        continue;
                    }
                    final MultiHostHost hitfile = new MultiHostHost("hitfile.net");
                    hitfile.setTrafficLeft(mhost.getTrafficLeft());
                    hitfile.setTrafficMax(mhost.getTrafficMax());
                    hitfile.setLinksLeft(mhost.getLinksLeft());
                    hitfile.setLinksMax(mhost.getLinksMax());
                    hitfile.setUnlimitedTraffic(mhost.isUnlimitedTraffic());
                    hitfile.setUnlimitedLinks(mhost.isUnlimitedLinks());
                    supportedhosts.add(hitfile);
                    final MultiHostHost turbobit = new MultiHostHost("turbobit.net");
                    turbobit.setTrafficLeft(mhost.getTrafficLeft());
                    turbobit.setTrafficMax(mhost.getTrafficMax());
                    turbobit.setLinksLeft(mhost.getLinksLeft());
                    turbobit.setLinksMax(mhost.getLinksMax());
                    turbobit.setUnlimitedTraffic(mhost.isUnlimitedTraffic());
                    turbobit.setUnlimitedLinks(mhost.isUnlimitedLinks());
                    supportedhosts.add(turbobit);
                    turbobitWorkaroundDone = true;
                } else {
                    supportedhosts.add(mhost);
                }
            }
        }
        /* Set list of supported hosts */
        final List<MultiHostHost> results = ai.setMultiHostSupportV2(this, supportedhosts);
        final boolean filterJDownloaderUnsupportedStreamHosts = true;
        if (includeStreamingItems && filterJDownloaderUnsupportedStreamHosts && streamDomains.size() > 0) {
            /* Filter all stream items which are not supported by JDownloader in order to lower the size of our final list. */
            final List<MultiHostHost> allowedresults = new ArrayList<MultiHostHost>();
            for (final MultiHostHost mhost : results) {
                boolean isStreamService = false;
                for (final String domain : mhost.getDomains()) {
                    if (streamDomains.contains(domain)) {
                        isStreamService = true;
                        break;
                    }
                }
                if (!isStreamService) {
                    /* This is not a stream service -> Allow it to be added to final list. */
                    allowedresults.add(mhost);
                    continue;
                }
                /* This is a streaming service -> Check if we want to add it to our final list. */
                final MultihosterHostStatus status = mhost.getStatus();
                final String domain = mhost.getDomain();
                if (status == MultihosterHostStatus.DEACTIVATED_JDOWNLOADER_UNSUPPORTED) {
                    logger.info("Ignore unsupported stream domain: " + domain);
                } else if (status != MultihosterHostStatus.WORKING && status != MultihosterHostStatus.WORKING_UNSTABLE) {
                    logger.info("Ignore non working stream domain: " + domain);
                } else {
                    allowedresults.add(mhost);
                }
            }
            logger.info("Results initially: " + supportedhosts.size() + " | Results after filtering unsupported stream hosts: " + allowedresults.size());
            ai.setMultiHostSupportV2(this, allowedresults);
        }
        return ai;
    }

    public Map<String, Object> login(final Account account, final AccountInfo ai, final boolean validateApikey) throws Exception {
        synchronized (account) {
            String apikey = getApiKey(account);
            if (apikey != null) {
                setAuthHeader(br, apikey);
                if (!validateApikey) {
                    return null;
                }
                logger.info("Attempting login with existing apikey");
                final Map<String, Object> userinfo = getAccountInfo(account, ai, apikey);
                logger.info("Apikey login successful");
                return userinfo;
            }
            /* PIN login */
            apikey = performPINLogin(account);
            account.setProperty(PROPERTY_apikey, apikey);
            setAuthHeader(br, apikey);
            return getAccountInfo(account, ai, apikey);
        }
    }

    private String performPINLogin(final Account account) throws Exception {
        logger.info("Performing PIN login");
        final Map<String, Object> data = this.callAPI("/pin/get", account, null);
        final String user_url = data.get("user_url").toString();
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("check", data.get("check").toString());
        query.appendEncoded("pin", data.get("pin").toString());
        final int maxSecondsServerside = ((Number) data.get("expires_in")).intValue();
        final int maxWaitSecondsClientside = 1200;
        final int waitSecondsForDialog = Math.min(maxSecondsServerside, maxWaitSecondsClientside);
        final Thread dialog = showPINLoginInformation(user_url, waitSecondsForDialog);
        int secondsWaited = 0;
        final int waitSecondsPerLoop = 3;
        String apikey = null;
        try {
            while (true) {
                logger.info("Waiting for user to authorize application. Seconds waited: " + secondsWaited + "/" + maxSecondsServerside);
                Thread.sleep(waitSecondsPerLoop * 1000);
                secondsWaited += waitSecondsPerLoop;
                /** Example response: { "status": "success", "data": { "activated": false, "expires_in": 590 }}} */
                final Map<String, Object> resp = this.callAPI("/pin/check", query, account, null);
                apikey = (String) resp.get("apikey");
                final int secondsLeftServerside = ((Number) data.get("expires_in")).intValue();
                if (!StringUtils.isEmpty(apikey)) {
                    logger.info("Stopping because: Found apikey!");
                    break;
                } else if (secondsWaited >= maxSecondsServerside) {
                    logger.info("Stopping because: Timeout #1 | User did not perform authorization within " + maxSecondsServerside + " seconds");
                    break;
                } else if (secondsLeftServerside <= waitSecondsPerLoop) {
                    logger.info("Stopping because: Timeout #2");
                    break;
                } else if (secondsWaited >= maxWaitSecondsClientside) {
                    logger.info("Stopping because: Timeout #3");
                    break;
                } else if (!dialog.isAlive()) {
                    logger.info("Stopping because: Dialog closed!");
                    break;
                } else if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    throw new InterruptedException();
                } else if (secondsWaited + waitSecondsPerLoop >= maxSecondsServerside) {
                    logger.info("Stopping because: Timeout #1 | User did not perform authorization within " + maxSecondsServerside + " seconds");
                    break;
                }
            }
        } finally {
            dialog.interrupt();
        }
        if (StringUtils.isEmpty(apikey)) {
            throw new AccountInvalidException("Authorization failed!\r\nLogin in your browser and confirm the code you see to allow JDownloader to login into your account.\r\nDo not close this pairing dialog until you have confirmed the code via browser!");
        }
        return apikey;
    }

    private Map<String, Object> getAccountInfo(final Account account, final AccountInfo ai, final String apikey) throws Exception {
        synchronized (account) {
            setAuthHeader(br, apikey);
            final Map<String, Object> data = this.callAPI("/user", account, null);
            final Map<String, Object> user = (Map<String, Object>) data.get("user");
            final String userName = (String) user.get("username");
            if (!StringUtils.isEmpty(userName)) {
                account.setUser(userName);
            }
            final Number premiumUntil = (Number) user.get("premiumUntil");
            if (premiumUntil != null) {
                ai.setValidUntil(premiumUntil.longValue() * 1000l, br);
            }
            if (Boolean.TRUE.equals(user.get("isTrial"))) {
                /*
                 * 2020-03-27: Premium "test" accounts which last 7 days and have a total of 25GB as quota. Once that limit is reached, they
                 * can only download from "Free" hosts (only a hand full of hosts).
                 */
                ai.setStatus("Trial account, reverts to free once traffic is used up");
                final Number remainingTrialQuota = (Number) user.get("remainingTrialQuota");
                /* 2020-03-27: Hardcoded maxTraffic value */
                final long maxTraffic = SizeFormatter.getSize("25GB");
                if (remainingTrialQuota != null) {
                    final long remainingTrialTraffic = remainingTrialQuota.longValue() * 1000 * 1000;
                    ai.setTrafficLeft(remainingTrialTraffic);
                    if (remainingTrialTraffic <= maxTraffic) {
                        ai.setTrafficMax(maxTraffic);
                    }
                } else {
                    ai.setTrafficMax(maxTraffic);
                }
            } else if (!Boolean.TRUE.equals(user.get("isPremium"))) {
                /*
                 * "Real" free account (or expired trial premium [= user downloaded more than 25GB trial quota]) --> Cannot download and
                 * cannot even login via API officially!
                 */
                account.setType(AccountType.FREE);
                throw new AccountInvalidException("Free accounts are not supported!");
            }
            final Number fidelityPoints = (Number) user.get("fidelityPoints");
            if (fidelityPoints != null) {
                ai.setPremiumPoints(fidelityPoints.longValue());
            }
            account.setType(AccountType.PREMIUM);
            // final List<String> notifications = (List<String>) user.get("notifications");
            return user;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new AccountRequiredException();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        final String directlinkproperty = getDirectLinkProperty(link, account);
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Trying to re-use stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            dllink = this.generteFreshDirecturl(link, account);
        }
        final URL url = new URL(dllink);
        Browser.setRequestIntervalLimitGlobal(url.getHost(), 250);
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, this.getMaxChunks(account, link, url));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                this.checkErrorsWebsite(br, link, account);
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Directurl did not lead to downloadable content");
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        try {
            dl.startDownload();
        } catch (final PluginException e) {
            if (StringUtils.containsIgnoreCase(e.getMessage(), "Server: Too Many Requests")) {
                setRateLimit(link, account, url);
            }
            throw e;
        }
    }

    private static void setAuthHeader(final Browser br, final String auth) {
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + auth);
    }

    private String getDirectLinkProperty(final DownloadLink link, final Account account) {
        return this.getHost() + "directurl";
    }

    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        final String directlinkproperty = getDirectLinkProperty(link, account);
        final String pawsProperty = getDirectLinkProperty(link, account) + "_paws";
        /* Try to re-use previously generated directurl */
        String dllink = checkDirectLink(account, link, directlinkproperty);
        if (!StringUtils.isEmpty(dllink)) {
            logger.info("Re-using stored directurl: " + dllink);
        } else {
            dllink = generteFreshDirecturl(link, account);
        }
        dllink = updateProtocolInDirecturl(dllink);
        final URL url = new URL(dllink);
        Browser.setRequestIntervalLimitGlobal(url.getHost(), 250);
        final boolean useVerifiedFileSize;
        /* "paws" handling = old handling to disable setting verified filesize via API. */
        final boolean paws = link.getBooleanProperty(pawsProperty, false);
        if (!paws) {
            logger.info("don't use verified filesize because 'paws'!");
            useVerifiedFileSize = false;
        } else if (link.getVerifiedFileSize() > 0) {
            final Browser brc = br.cloneBrowser();
            brc.setFollowRedirects(true);
            final URLConnectionAdapter check = brc.openGetConnection(dllink);
            try {
                if (!looksLikeDownloadableContent(check)) {
                    brc.followConnection(true);
                    this.checkErrorsWebsite(brc, link, account);
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Final downloadurl did not lead to downloadable content");
                } else if (check.getCompleteContentLength() < 0) {
                    logger.info("don't use verified filesize because complete content length isn't available:" + check.getCompleteContentLength() + "==" + link.getVerifiedFileSize());
                    useVerifiedFileSize = false;
                } else if (check.getCompleteContentLength() == link.getVerifiedFileSize()) {
                    logger.info("use verified filesize because it matches complete content length:" + check.getCompleteContentLength() + "==" + link.getVerifiedFileSize());
                    useVerifiedFileSize = true;
                } else {
                    if (link.getDownloadCurrent() > 0) {
                        throw new PluginException(LinkStatus.ERROR_FATAL, "Cannot resume different file:" + check.getCompleteContentLength() + "!=" + link.getVerifiedFileSize());
                    } else {
                        logger.info("don't use verified filesize because it doesn't match complete content length:" + check.getCompleteContentLength() + "!=" + link.getVerifiedFileSize());
                        useVerifiedFileSize = false;
                    }
                }
            } finally {
                check.disconnect();
            }
        } else {
            logger.info("use verified filesize");
            useVerifiedFileSize = true;
        }
        final String host = Browser.getHost(link.getDownloadURL());
        final DownloadLinkDownloadable downloadLinkDownloadable = new DownloadLinkDownloadable(link) {
            @Override
            public HashInfo getHashInfo() {
                return null;
            }

            @Override
            public boolean isHashCheckEnabled() {
                return false;
            }

            @Override
            public long getVerifiedFileSize() {
                if (useVerifiedFileSize) {
                    return super.getVerifiedFileSize();
                } else {
                    return -1;
                }
            }

            @Override
            public String getHost() {
                final DownloadInterface dli = getDownloadInterface();
                if (dli != null) {
                    final URLConnectionAdapter connection = dli.getConnection();
                    if (connection != null) {
                        return connection.getURL().getHost();
                    }
                }
                return host;
            }
        };
        final int chunks = getMaxChunks(account, link, url);
        logger.info("Max allowed chunks: " + chunks);
        dl = new jd.plugins.BrowserAdapter().openDownload(br, downloadLinkDownloadable, br.createGetRequest(dllink), true, chunks);
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            this.checkErrorsWebsite(br, link, account);
            /* unknown error */
            logger.severe("Error: Unknown Error");
            mhm.handleErrorGeneric(account, link, "Final downloadurl did not lead to a file", 50);
        }
        try {
            dl.startDownload();
        } catch (final PluginException e) {
            if (StringUtils.containsIgnoreCase(e.getMessage(), "Server: Too Many Requests")) {
                setRateLimit(link, account, url);
            }
            throw e;
        }
    }

    private void checkErrorsWebsite(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        final URLConnectionAdapter con = br.getHttpConnection();
        checkRateLimit(br, con, account, link);
        if (con.getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404");
        } else if (con.getResponseCode() == 416) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 416 - try later or with less chunks");
        } else if (br.containsHTML("range not ok")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Range not ok");
        }
        String errorServerNotAllowed = br.getRegex(">(Server not allowed[^<]+)").getMatch(0);
        if (errorServerNotAllowed != null) {
            /* User has tried to use a non-allowed server IP or VPN IP. */
            errorServerNotAllowed = Encoding.htmlOnlyDecode(errorServerNotAllowed);
            errorServerNotAllowed += " | allow it here: alldebrid.com/vpn/ and here: alldebrid.com/account/";
            if (account != null) {
                throw new AccountUnavailableException(errorServerNotAllowed, 5 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, errorServerNotAllowed);
            }
        }
    }

    private void setRateLimit(final DownloadLink link, final Account account, final URL downloadURL) {
        synchronized (RATE_LIMITED) {
            HashSet<String> set = RATE_LIMITED.get(account);
            if (set == null) {
                set = new HashSet<String>();
                RATE_LIMITED.put(account, set);
            }
            set.add(downloadURL.getHost());
        }
    }

    private String generteFreshDirecturl(final DownloadLink link, final Account account) throws Exception {
        logger.info("Generating fresh directurl");
        this.login(account, new AccountInfo(), false);
        String downloadPassword = link.getDownloadPassword();
        final String url;
        final boolean isSelfhosted = isSelfhosted(link);
        if (isSelfhosted) {
            url = link.getPluginPatternMatcher();
        } else {
            url = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
        }
        int counter = 0;
        Map<String, Object> data = null;
        boolean passwordSuccess = false;
        do {
            counter++;
            final UrlQuery postdata = new UrlQuery();
            postdata.appendEncoded("link", url);
            if (counter > 1) {
                downloadPassword = getUserInput("Password?", link);
            }
            if (downloadPassword != null) {
                postdata.appendEncoded("password", downloadPassword);
            }
            try {
                data = this.callAPI("/link/unlock", postdata, account, link);
                logger.info("Breaking loop because: User entered correct password or none was needed");
                passwordSuccess = true;
                break;
            } catch (final PluginException e) {
                if (br.containsHTML(ERROR_CODE_LINK_PASSWORD_PROTECTED)) {
                    /*
                     * Stored password was wrong or this was the first attempt and we didn't know the item was password protected so now we
                     * know we need to ask the user to enter a download password.
                     */
                    counter++;
                    continue;
                } else {
                    throw e;
                }
            }
        } while (counter <= 3);
        if (!passwordSuccess) {
            throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
        }
        if (!StringUtils.isEmpty(downloadPassword)) {
            /* Entered password looks to be correct -> Store password */
            link.setDownloadPassword(downloadPassword);
        }
        final Object delayID = data.get("delayed");
        if (delayID != null) {
            /* Server side download required, see https://docs.alldebrid.com/#delayed-links */
            cacheDLChecker(link, account, delayID.toString());
        }
        String dllink = (String) data.get("link");
        if (StringUtils.isEmpty(dllink)) {
            /**
             * Assume that this is a stream download e.g. for eporner.com </br> https://docs.alldebrid.com/#streaming-links
             */
            final String linkID = data.get("id").toString();
            Map<String, Object> best = null;
            int bestQualityRank = 0;
            final List<Map<String, Object>> streams = (List<Map<String, Object>>) data.get("streams");
            for (final Map<String, Object> stream : streams) {
                final int qualityRank = ((Number) stream.get("qualityRank")).intValue();
                if (best == null || qualityRank > bestQualityRank) {
                    best = stream;
                    bestQualityRank = qualityRank;
                }
            }
            final String streamID = best.get("id").toString();
            final UrlQuery streampostdata = new UrlQuery();
            streampostdata.appendEncoded("id", linkID);
            streampostdata.appendEncoded("stream", streamID);
            data = this.callAPI("/link/streaming", streampostdata, account, link);
            dllink = (String) data.get("link");
        }
        if (isSelfhosted) {
            link.setFinalFileName(data.get("filename").toString());
            link.setVerifiedFileSize(((Number) data.get("filesize")).longValue());
        }
        link.setProperty(PROPERTY_maxchunks, data.get("max_chunks"));
        link.setProperty(getDirectLinkProperty(link, account) + "_paws", data.get("paws"));
        link.setProperty(getDirectLinkProperty(link, account), dllink);
        return dllink;
    }

    private String updateProtocolInDirecturl(String dllink) throws MalformedURLException {
        if (PluginJsonConfig.get(AlldebridComConfig.class).isUseHTTPSForDownloads()) {
            /* Do not change url and assume protocol is https. */
            return dllink;
        }
        // https://svn.jdownloader.org/issues/87886
        final URL url = new URL(dllink);
        if (StringUtils.equals("https", url.getProtocol())) {
            logger.info("https for final downloadurls is disabled:" + dllink);
            String newDllink = dllink.replaceFirst("(?i)https://", "http://");
            if (url.getPort() != -1) {
                // remove custom https port
                newDllink = dllink.replace(":" + url.getPort() + "/", "/");
            }
            /* Check if link has changed */
            if (!newDllink.equals(dllink)) {
                logger.info("New final downloadurl: " + newDllink);
                dllink = newDllink;
            }
        }
        return dllink;
    }

    private void cacheDLChecker(final DownloadLink link, final Account account, final String delayID) throws Exception {
        final PluginProgress waitProgress = new PluginProgress(0, 100, null) {
            protected long lastCurrent    = -1;
            protected long lastTotal      = -1;
            protected long startTimeStamp = -1;

            @Override
            public PluginTaskID getID() {
                return PluginTaskID.WAIT;
            }

            @Override
            public String getMessage(Object requestor) {
                if (requestor instanceof ETAColumn) {
                    final long eta = getETA();
                    if (eta >= 0) {
                        return TimeFormatter.formatMilliSeconds(eta, 0);
                    }
                    return "";
                }
                return "Preparing your delayed file";
            }

            @Override
            public void updateValues(long current, long total) {
                super.updateValues(current, total);
                if (startTimeStamp == -1 || lastTotal == -1 || lastTotal != total || lastCurrent == -1 || lastCurrent > current) {
                    lastTotal = total;
                    lastCurrent = current;
                    startTimeStamp = System.currentTimeMillis();
                    // this.setETA(-1);
                    return;
                }
                long currentTimeDifference = System.currentTimeMillis() - startTimeStamp;
                if (currentTimeDifference <= 0) {
                    return;
                }
                long speed = (current * 10000) / currentTimeDifference;
                if (speed == 0) {
                    return;
                }
                long eta = ((total - current) * 10000) / speed;
                this.setETA(eta);
            }
        };
        waitProgress.setIcon(new AbstractIcon(IconKey.ICON_WAIT, 16));
        waitProgress.setProgressSource(this);
        final UrlQuery postdata = new UrlQuery();
        postdata.appendEncoded("id", delayID);
        try {
            /* See https://docs.alldebrid.com/#delayed-links */
            final int maxWaitSeconds = 300;
            /* 2020-03-27: API docs say checking every 5 seconds is recommended */
            final int waitSecondsPerLoop = 5;
            int waitSecondsLeft = maxWaitSeconds;
            /* 1 = still processing, 2 = Download link available, 3 = Error */
            int delayedStatus = 1;
            int currentProgress = 0;
            do {
                final Map<String, Object> data = this.callAPI("/link/delayed", postdata, account, link);
                delayedStatus = (int) JavaScriptEngineFactory.toLong(data.get("status"), 3);
                currentProgress = (int) ((Number) data.get("progress")).doubleValue() * 100;
                /* 2020-03-27: We cannot trust their 'time_left' :D */
                // final int timeLeft = (int) JavaScriptEngineFactory.toLong(entries.get("time_left"), 30);
                /* 2020-03-27: Do not trust their 100% :D */
                currentProgress = Math.min(currentProgress, 100);
                /* Update progress values which user will see in GUI. */
                link.addPluginProgress(waitProgress);
                waitProgress.updateValues(currentProgress, 100);
                /* Check abort conditions */
                if (this.isAbort()) {
                    logger.info("Exit delayed loop because: aborted by user");
                    throw new InterruptedException();
                } else if (delayedStatus != 1) {
                    logger.info("Exit delayed loop because: delayedStatus != 1 | delayedStatus=" + delayedStatus);
                    break;
                } else if (waitSecondsLeft <= 0) {
                    logger.info("Exit delayed loop because: waitSecondsLeft <=0 | waitSecondsLeft=" + waitSecondsLeft);
                    break;
                }
                logger.info(String.format(Locale.ROOT, "Waiting for file to get loaded onto server - seconds left %d / %d", waitSecondsLeft, maxWaitSeconds));
                for (int sleptSeconds = 0; sleptSeconds < waitSecondsPerLoop; sleptSeconds++) {
                    if (isAbort()) {
                        logger.info("Exit delayed loop because: aborted by user");
                        throw new InterruptedException();
                    }
                    Thread.sleep(1000);
                }
                waitSecondsLeft -= waitSecondsPerLoop;
            } while (true);
            if (delayedStatus == 2) {
                return;
            } else {
                mhm.handleErrorGeneric(account, link, "Serverside download failure in 'delayed' handling", 50, 5 * 60 * 1000l);
            }
        } finally {
            link.removePluginProgress(waitProgress);
        }
    }

    private String checkDirectLink(final Account account, final DownloadLink link, final String property) {
        final String dllink = link.getStringProperty(property);
        if (dllink == null) {
            return null;
        }
        URLConnectionAdapter con = null;
        try {
            final Browser br2 = br.cloneBrowser();
            br2.setFollowRedirects(true);
            // HEAD requests are causing issues serverside, headers are missing in combination with keepalive
            con = br2.openGetConnection(dllink);
            if (looksLikeDownloadableContent(con)) {
                return dllink;
            } else {
                br2.followConnection(true);
                this.checkErrorsWebsite(br2, link, account);
                throw new IOException();
            }
        } catch (final Exception e) {
            logger.log(e);
            link.removeProperty(property);
            link.removeProperty(property + "_paws");
            return null;
        } finally {
            if (con != null) {
                con.disconnect();
            }
        }
    }

    private int getMaxChunks(final Account account, final DownloadLink link, final URL downloadURL) {
        /* 2024-05-17: Chunks limited to 16 RE: admin, limit is 32/IP/Server */
        final int defaultMaxChunks = 16;
        int chunks = Math.abs(link.getIntegerProperty(PROPERTY_maxchunks, defaultMaxChunks));
        if (chunks > 1 && defaultMaxChunks > 1) {
            chunks = -Math.min(chunks, defaultMaxChunks);
        } else {
            chunks = 1;
        }
        if (chunks != 1) {
            /* Check if we are rate limited */
            synchronized (RATE_LIMITED) {
                final HashSet<String> set = RATE_LIMITED.get(account);
                if (set != null && set.contains(downloadURL.getHost())) {
                    /* Host in rate-limit -> Allow max 1 chunk */
                    chunks = 1;
                }
            }
        }
        return chunks;
    }

    private void checkRateLimit(final Browser br, URLConnectionAdapter con, final Account account, final DownloadLink link) throws PluginException {
        if (con.getResponseCode() == 429 || br.containsHTML("rate limiting, please retry")) {
            Browser.setRequestIntervalLimitGlobal(br.getHost(), 2000);
            setRateLimit(link, account, con.getURL());
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Too many requests:" + br.getHost());
        }
    }

    public String getApiKey(final Account account) {
        if (this.looksLikeValidAPIKey(account.getPass())) {
            /* User has entered API-key-like string into the password field -> Use that. */
            return account.getPass();
        } else {
            /* No API key in password field -> Return saved API key property (or null). */
            return account.getStringProperty(PROPERTY_apikey);
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        return requestFileInformation(link, account);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            /* Account needed to check such links. */
            return AvailableStatus.UNCHECKABLE;
        }
        if (!link.isNameSet()) {
            link.setName(this.getFID(link));
        }
        generteFreshDirecturl(link, account);
        /* No exception = File is online */
        return AvailableStatus.TRUE;
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return AlldebridComConfig.class;
    }

    public static interface AlldebridComConfig extends PluginConfigInterface {
        public static final TRANSLATION TRANSLATION = new TRANSLATION();

        public static class TRANSLATION {
            public String getUseHTTPSForDownloads_label() {
                return "Use https for final download urls?";
            }
        }

        @AboutConfig
        @DefaultBooleanValue(true)
        boolean isUseHTTPSForDownloads();

        void setUseHTTPSForDownloads(boolean b);
    }

    @Override
    public AccountBuilderInterface getAccountFactory(final InputChangedCallbackInterface callback) {
        return new AlldebridSpecialLogin(callback, this);
    }

    public class AlldebridSpecialLogin extends MigPanel implements AccountBuilderInterface {
        /**
         *
         */
        private static final long serialVersionUID = 1L;

        protected String getPassword() {
            if (this.pass == null) {
                return null;
            } else {
                return StringUtils.trim(new String(this.pass.getPassword()));
            }
        }

        private final ExtPasswordField pass;
        private final AllDebridCom     plg;

        public boolean updateAccount(Account input, Account output) {
            if (!StringUtils.equals(input.getPass(), output.getPass())) {
                output.setPass(input.getPass());
                return true;
            } else {
                return false;
            }
        }

        public AlldebridSpecialLogin(final InputChangedCallbackInterface callback, final PluginForHost plg) {
            super("ins 0, wrap 2", "[][grow,fill]", "");
            this.plg = (AllDebridCom) plg;
            add(new JLabel(_GUI.T.jd_gui_swing_components_AccountDialog_generic_instructions()));
            final String helplink = this.plg.getAPILoginHelpURL();
            add(new JLink(_GUI.T.jd_gui_swing_components_AccountDialog_generic_instructions_click_here_for_instructions(), helplink));
            this.add(new JLink("<HTML><U>Enter your API key or click on 'Save' to initiate browser login.</U></HTML>", helplink));
            add(this.pass = new ExtPasswordField() {
                @Override
                public void onChanged() {
                    callback.onChangedInput(pass);
                }
            }, "");
            pass.setHelpText(_GUI.T.jd_gui_swing_components_AccountDialog_api_key_help());
        }

        @Override
        public boolean handleClipboardAutoFill() {
            return DefaultEditAccountPanelAPIKeyLogin.handleClipboardAutoFill(pass, null, this.plg);
        }

        public void setAccount(final Account defaultAccount) {
            pass.setText(defaultAccount.getPass());
        }

        @Override
        public boolean validateInputs() {
            return true;
        }

        @Override
        public Account getAccount() {
            return new Account(null, getPassword());
        }

        @Override
        public JComponent getComponent() {
            return this;
        }
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("[a-zA-Z0-9]{20}")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://help." + getHost() + "/en/Third%20party%20tools/jdownloader";
    }

    private Map<String, Object> callAPI(final String path, final Account account, final DownloadLink link) throws Exception {
        br.getPage(api_base_v_41 + path);
        return handleErrors(account, link);
    }

    private Map<String, Object> callAPI(final String path, final UrlQuery postdata, final Account account, final DownloadLink link) throws Exception {
        br.postPage(api_base_v_41 + path, postdata);
        return handleErrors(account, link);
    }

    /** See https://docs.alldebrid.com/#all-errors */
    private Map<String, Object> handleErrors(final Account account, final DownloadLink link) throws PluginException, Exception {
        /* 2020-03-25: E.g. {"status": "error", "error": {"code": "AUTH_BAD_APIKEY","message": "The auth apikey is invalid"}} */
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
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
        final String status = (String) entries.get("status");
        if (!"error".equalsIgnoreCase(status)) {
            final Map<String, Object> data = (Map<String, Object>) entries.get("data");
            return data;
        }
        final boolean isSelfhosted = this.isSelfhosted(link);
        final Map<String, Object> errormap = (Map<String, Object>) entries.get("error");
        final String errorcode = errormap.get("code").toString();
        final String message = errormap.get("message").toString();
        final HashSet<String> accountErrorsPermanent = new HashSet<String>();
        accountErrorsPermanent.add("AUTH_MISSING_APIKEY");
        accountErrorsPermanent.add("AUTH_BAD_APIKEY");
        accountErrorsPermanent.add("AUTH_USER_BANNED");
        accountErrorsPermanent.add("PIN_EXPIRED");
        accountErrorsPermanent.add("PIN_INVALID");
        accountErrorsPermanent.add("ACCOUNT_INVALID");
        final HashSet<String> accountErrorsTemporary = new HashSet<String>();
        accountErrorsTemporary.add("MAINTENANCE");
        accountErrorsTemporary.add("AUTH_BLOCKED");
        accountErrorsTemporary.add("ALREADY_SENT");
        accountErrorsTemporary.add("NO_SERVER");
        accountErrorsTemporary.add("FREE_TRIAL_LIMIT_REACHED");
        accountErrorsTemporary.add("PIN_ALREADY_AUTHED");
        accountErrorsTemporary.add("PIN_EXPIRED");
        accountErrorsTemporary.add("INSUFFICIENT_BALANCE");
        final HashSet<String> downloadErrorsHostUnavailable = new HashSet<String>();
        downloadErrorsHostUnavailable.add("LINK_HOST_NOT_SUPPORTED");
        downloadErrorsHostUnavailable.add("LINK_HOST_UNAVAILABLE");
        downloadErrorsHostUnavailable.add("LINK_HOST_FULL");
        downloadErrorsHostUnavailable.add("LINK_HOST_LIMIT_REACHED");
        downloadErrorsHostUnavailable.add("USER_LINK_INVALID");
        final HashSet<String> downloadErrorsFileUnavailable = new HashSet<String>();
        downloadErrorsFileUnavailable.add("LINK_IS_MISSING");
        downloadErrorsFileUnavailable.add("BAD_LINK");
        downloadErrorsFileUnavailable.add("LINK_TOO_MANY_DOWNLOADS");
        downloadErrorsFileUnavailable.add("LINK_ERROR");
        downloadErrorsFileUnavailable.add("LINK_TEMPORARY_UNAVAILABLE");
        downloadErrorsFileUnavailable.add("LINK_NOT_SUPPORTED");
        downloadErrorsFileUnavailable.add("MUST_BE_PREMIUM");
        downloadErrorsFileUnavailable.add("DOWNLOAD_FAILED");
        downloadErrorsFileUnavailable.add("DELAYED_INVALID_ID");
        if (errorcode.equalsIgnoreCase("LINK_DOWN")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (errorcode.equalsIgnoreCase("LINK_HOST_NOT_SUPPORTED")) {
            if (isSelfhosted) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                mhm.putError(account, link, 5 * 60 * 1000l, message);
            }
        } else if (errorcode.equalsIgnoreCase(ERROR_CODE_LINK_PASSWORD_PROTECTED)) {
            link.setDownloadPassword(null);
            link.setPasswordProtected(true);
            throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
        } else if (accountErrorsPermanent.contains(errorcode)) {
            /* This is the only error which allows us to remove the apikey and re-login. */
            account.removeProperty(PROPERTY_apikey);
            throw new AccountInvalidException(message);
        } else if (accountErrorsTemporary.contains(errorcode)) {
            throw new AccountUnavailableException(message, 5 * 60 * 1000);
        } else if (downloadErrorsHostUnavailable.contains(errorcode)) {
            mhm.putError(account, link, 5 * 60 * 1000l, message);
        } else if (downloadErrorsFileUnavailable.contains(errorcode)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, message);
        } else {
            /*
             * Unknown/Generic error --> Assume it is a download issue but display it as temp. account issue if no DownloadLink is given.
             */
            /*
             * E.g. LINK_ERROR, LINK_IS_MISSING, STREAM_INVALID_GEN_ID, STREAM_INVALID_STREAM_ID, DELAYED_INVALID_ID, REDIRECTOR_XXX,
             * MAGNET_XXX, USER_LINK_INVALID, MISSING_NOTIF_ENDPOINT
             */
            logger.info("Unknown API error: " + errorcode);
            if (link != null) {
                mhm.handleErrorGeneric(account, link, message, 50);
            } else {
                /* Temp disable account */
                throw new AccountUnavailableException(message, 5 * 60 * 1000);
            }
        }
        return entries;
    }

    private Thread showPINLoginInformation(final String pin_url, final int timeoutSeconds) {
        final String host = getHost();
        final Thread thread = new Thread() {
            public void run() {
                try {
                    String message = "";
                    final String title;
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        title = "Alldebrid - Login";
                        message += "Hallo liebe(r) alldebrid NutzerIn\r\n";
                        message += "Um deinen " + host + " Account in JDownloader verwenden zu können, musst du folgende Schritte beachten:\r\n";
                        message += "1. Öffne diesen Link im Browser falls das nicht automatisch passiert:\r\n\t'" + pin_url + "'\t\r\n";
                        message += "2. Bestätige den PIN Code im Browser.\r\n";
                        message += "3. Optional: Falls du noch nicht in deinem AD Account im Browser eingeloggt bist, logge dich ein und bestätige den PIN Code erneut.\r\n";
                        message += "Dein Account sollte nach wenigen Sekunden von JDownloader akzeptiert werden.\r\n";
                    } else {
                        title = "Alldebrid - Login";
                        message += "Hello dear alldebrid user\r\n";
                        message += "In order to use " + host + " in JDownloader, you need to follow these steps:\r\n";
                        message += "1. Open this URL in your browser if it wasn't opened automatically:\r\n\t'" + pin_url + "'\t\r\n";
                        message += "2. Confirm the PIN Code you see in the browser window.\r\n";
                        message += "3. Optional: If you haven't been logged in into your AD account in your browser already, login and confirm the PIN Code again.\r\n";
                        message += "Your account should be accepted in JDownloader within a few seconds.\r\n";
                    }
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, title, message);
                    dialog.setTimeout(timeoutSeconds * 1000);
                    if (CrossSystem.isOpenBrowserSupported() && !Application.isHeadless()) {
                        CrossSystem.openURL(pin_url);
                    }
                    final ConfirmDialogInterface ret = UIOManager.I().show(ConfirmDialogInterface.class, dialog);
                    ret.throwCloseExceptions();
                } catch (final Throwable e) {
                    getLogger().log(e);
                }
            };
        };
        thread.setDaemon(true);
        thread.start();
        return thread;
    }

    private Thread showNewLocationLoginInformation(final String pin_url, final String api_errormsg, final int timeoutSeconds) {
        final Thread thread = new Thread() {
            public void run() {
                try {
                    String message = "";
                    final String title;
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        title = "Alldebrid - Login von neuem Standort bestätigen";
                        message += "Hallo liebe(r) alldebrid NutzerIn";
                        message += "\r\nDu versuchst gerade, deinen Account an einem neuen Standort zu verwenden.";
                        message += "\r\nBestätige diesen Loginversuch mit dem Link, den du per E-Mail erhalten hast, um deinen Account in JD weiterverwenden zu können.";
                        message += "\r\nDieser dialog schließt sich automatisch, sobald du den Login bestätigt hast.";
                        if (api_errormsg != null) {
                            message += "\r\nFehlermeldung der Alldebrid API:";
                            message += "\r\n" + api_errormsg;
                        }
                    } else {
                        title = "Alldebrid - Confirm new location";
                        message += "Hello dear alldebrid user";
                        message += "\r\nYou are trying to use this service from a new location.";
                        message += "\r\nYou've received an e-mail with a link to confirm this new location.";
                        message += "\r\n Confirm this e-mail to continue using your Alldebrid account in JDownloader.";
                        message += "\r\nOnce accepted, this dialog will be closed automatically.";
                        if (api_errormsg != null) {
                            message += "\r\nMessage from Alldebrid API:";
                            message += "\r\n" + api_errormsg;
                        }
                    }
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, title, message);
                    dialog.setTimeout(timeoutSeconds * 1000);
                    if (pin_url != null && CrossSystem.isOpenBrowserSupported() && !Application.isHeadless()) {
                        CrossSystem.openURL(pin_url);
                    }
                    final ConfirmDialogInterface ret = UIOManager.I().show(ConfirmDialogInterface.class, dialog);
                    ret.throwCloseExceptions();
                } catch (final Throwable e) {
                    getLogger().log(e);
                }
            };
        };
        thread.setDaemon(true);
        thread.start();
        return thread;
    }
}