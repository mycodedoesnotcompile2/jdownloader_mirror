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

import java.awt.Dimension;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.Icon;
import javax.swing.JScrollPane;

import jd.PluginWrapper;
import jd.config.Property;
import jd.gui.swing.jdgui.BasicJDTable;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
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
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginConfigPanelNG;
import jd.plugins.PluginException;
import jd.plugins.components.MultiHosterManagement;
import jd.plugins.components.PluginJSonUtils;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtDateColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.ConditionalSkipReasonException;
import org.jdownloader.plugins.WaitingSkipReason;
import org.jdownloader.plugins.WaitingSkipReason.CAUSE;
import org.jdownloader.plugins.components.usenet.UsenetAccountConfigInterface;
import org.jdownloader.plugins.components.usenet.UsenetServer;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.config.TakeValueFromSubconfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

@HostPlugin(revision = "$Revision: 50127 $", interfaceVersion = 3, names = { "offcloud.com" }, urls = { "" })
public class OffCloudCom extends UseNet {
    /** Using API: https://github.com/offcloud/offcloud-api */
    /* Properties */
    private static final String                   PROPERTY_DOWNLOADTYPE                                = "offclouddownloadtype";
    private static final String                   PROPERTY_DOWNLOADTYPE_instant                        = "instant";
    private static final String                   PROPERTY_DOWNLOADTYPE_cloud                          = "cloud";
    private static final String                   PROPERTY_ACCOUNT_TIMESTAMP_LAST_TIME_DELETED_HISTORY = "last_time_deleted_history";
    private static final String                   PROPERTY_ACCOUNT_ACTIVE_PACKAGES_LIST                = "premium_packages_list";
    /* Other constants & properties */
    private static final String                   API_BASE                                             = "https://offcloud.com/api/";
    private static final String                   WEBSITE_BASE                                         = "https://offcloud.com/";
    private static final String                   NOCHUNKS                                             = "NOCHUNKS";
    private static final String                   NORESUME                                             = "NORESUME";
    /* Connection limits */
    private static final boolean                  ACCOUNT_PREMIUM_RESUME                               = true;
    private static final int                      ACCOUNT_PREMIUM_MAXCHUNKS                            = 0;
    private static final int                      ACCOUNT_PREMIUM_MAXDOWNLOADS                         = 20;
    private static final long                     CLOUD_MAX_WAITTIME                                   = 600000l;
    private int                                   statuscode                                           = 0;
    /* Contains <host><number of max possible chunks per download> */
    private static HashMap<String, Integer>       hostMaxchunksMap                                     = new HashMap<String, Integer>();
    /* Contains <host><number of max possible simultan downloads> */
    private static HashMap<String, Integer>       hostMaxdlsMap                                        = new HashMap<String, Integer>();
    /* Contains <host><number of currently running simultan downloads> */
    private static HashMap<String, AtomicInteger> hostRunningDlsNumMap                                 = new HashMap<String, AtomicInteger>();
    /* List of hosts which are only available via cloud (queue) download system */
    public static List<String>                    cloudOnlyHosts                                       = new ArrayList<String>();
    private Account                               currAcc                                              = null;
    private DownloadLink                          currDownloadLink                                     = null;
    private static Object                         CTRLLOCK                                             = new Object();
    private static AtomicInteger                  maxPrem                                              = new AtomicInteger(1);
    private static MultiHosterManagement          mhm                                                  = new MultiHosterManagement("offcloud.com");

    public OffCloudCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost());
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.USENET, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        // define custom browser headers and language settings.
        br.getHeaders().put("Accept", "application/json, text/plain, */*");
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setCustomCharset("utf-8");
        br.setConnectTimeout(60 * 1000);
        br.setReadTimeout(60 * 1000);
        br.setAllowedResponseCodes(500);
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/legal";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    private void setConstants(final Account acc, final DownloadLink dl) {
        this.currAcc = acc;
        this.currDownloadLink = dl;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            /* without account its not possible to download the link */
            return false;
        }
        /* Make sure that we do not start more than the allowed number of max simultan downloads for the current host. */
        synchronized (hostRunningDlsNumMap) {
            final String currentHost = link.getHost();
            if (hostRunningDlsNumMap.containsKey(currentHost) && hostMaxdlsMap.containsKey(currentHost)) {
                final int maxDlsForCurrentHost = hostMaxdlsMap.get(currentHost);
                final AtomicInteger currentRunningDlsForCurrentHost = hostRunningDlsNumMap.get(currentHost);
                if (currentRunningDlsForCurrentHost.get() >= maxDlsForCurrentHost) {
                    /*
                     * Max downloads for specific host for this MOCH reached --> Avoid irritating/wrong 'Account missing' errormessage for
                     * this case - wait and retry!
                     */
                    throw new ConditionalSkipReasonException(new WaitingSkipReason(CAUSE.HOST_TEMP_UNAVAILABLE, 15 * 1000, null));
                }
            }
        }
        mhm.runCheck(account, link);
        return super.canHandle(link, account);
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(DownloadLink link, Account account) throws Exception {
        if (isUsenetLink(link)) {
            /* handle premium should never be called */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        super.handleMultiHost(link, account);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        if (isUsenetLink(link)) {
            super.handleMultiHost(link, account);
            return;
        }
        mhm.runCheck(account, link);
        /*
         * When JD is started the first time and the user starts downloads right away, a full login might not yet have happened but it is
         * needed to get the individual host limits.
         */
        synchronized (CTRLLOCK) {
            if (hostMaxchunksMap.isEmpty() || hostMaxdlsMap.isEmpty()) {
                logger.info("Performing accountcheck to set individual host limits");
                this.fetchAccountInfo(account);
            }
        }
        setConstants(account, link);
        this.login(account, false);
        String dllink = checkDirectLink(link);
        String requestID = null;
        if (dllink == null) {
            final String url = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
            if (cloudOnlyHosts.contains(link.getHost())) {
                final long timeStarted = System.currentTimeMillis();
                link.setProperty(PROPERTY_DOWNLOADTYPE, PROPERTY_DOWNLOADTYPE_cloud);
                this.postRawAPISafe("https://" + getHost() + "/cloud/request", "{\"url\":\"" + url + "\",\"conversion\":\"\"}");
                // TODO: Use json parser
                requestID = PluginJSonUtils.getJsonValue(br, "requestId");
                if (requestID == null) {
                    /* Should never happen */
                    mhm.handleErrorGeneric(account, link, "cloud_requestIdnull", 50, 5 * 60 * 1000l);
                }
                String status = null;
                do {
                    this.sleep(5000l, link);
                    this.postRawAPISafe("https://" + getHost() + "/cloud/status", "{\"requestIds\":[\"" + requestID + "\"]}");
                    status = PluginJSonUtils.getJsonValue(br, "status");
                } while (System.currentTimeMillis() - timeStarted < CLOUD_MAX_WAITTIME && "downloading".equals(status));
                final String filename = PluginJSonUtils.getJsonValue(br, "fileName");
                if (!"downloaded".equals(status)) {
                    logger.warning("Cloud failed");
                    /* Should never happen but will happen */
                    mhm.handleErrorGeneric(account, link, "cloud_download_failed_reason_unknown", 50, 5 * 60 * 1000l);
                }
                /* Filename needed in URL or server will return bad filenames! */
                dllink = "https://" + getHost() + "/cloud/download/" + requestID + "/" + Encoding.urlEncode(filename);
            } else {
                link.setProperty(PROPERTY_DOWNLOADTYPE, PROPERTY_DOWNLOADTYPE_instant);
                this.postAPISafe(API_BASE + "instant/download", "proxyId=&url=" + Encoding.urlEncode(url));
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                requestID = (String) entries.get("requestId");
                if (requestID == null) {
                    /* Should never happen */
                    mhm.handleErrorGeneric(account, link, "instant_requestIdnull", 50, 5 * 60 * 1000l);
                }
                dllink = (String) entries.get("url");
                if (StringUtils.isEmpty(dllink)) {
                    /* Should never happen */
                    mhm.handleErrorGeneric(account, link, "dllinknull", 50, 5 * 60 * 1000l);
                }
            }
            dllink = dllink.replaceAll("\\\\/", "/");
        }
        link.setProperty("offcloudrequestId", requestID);
        handleDL(account, link, dllink);
    }

    @SuppressWarnings("deprecation")
    private void handleDL(final Account account, final DownloadLink link, final String dllink) throws Exception {
        /* First set hardcoded limit */
        int maxChunks = ACCOUNT_PREMIUM_MAXCHUNKS;
        /* Then check if we got an individual limit. */
        if (hostMaxchunksMap != null) {
            final String thishost = link.getHost();
            synchronized (hostMaxchunksMap) {
                if (hostMaxchunksMap.containsKey(thishost)) {
                    maxChunks = hostMaxchunksMap.get(thishost);
                }
            }
        }
        /* Then check if chunks failed before. */
        if (link.getBooleanProperty(this.getHost() + NOCHUNKS, false)) {
            maxChunks = 1;
        }
        boolean resume = ACCOUNT_PREMIUM_RESUME;
        if (link.getBooleanProperty(OffCloudCom.NORESUME, false)) {
            resume = false;
            link.setProperty(OffCloudCom.NORESUME, Boolean.valueOf(false));
        }
        final String directlinkproperty = getDirectlinkproperty();
        link.setProperty(directlinkproperty, dllink);
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, resume, maxChunks);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getResponseCode() == 416) {
                    logger.info("Resume impossible, disabling it for the next try");
                    link.setChunksProgress(null);
                    link.setProperty(OffCloudCom.NORESUME, Boolean.valueOf(true));
                    throw new PluginException(LinkStatus.ERROR_RETRY);
                } else if (dl.getConnection().getResponseCode() == 503 && link.getBooleanProperty(this.getHost() + OffCloudCom.NOCHUNKS, false) == false) {
                    // New V2 chunk errorhandling
                    /* unknown error, we disable multiple chunks */
                    link.setProperty(this.getHost() + OffCloudCom.NOCHUNKS, Boolean.valueOf(true));
                    throw new PluginException(LinkStatus.ERROR_RETRY);
                }
                updatestatuscode();
                handleAPIErrors(this.br);
                mhm.handleErrorGeneric(account, link, "unknowndlerror", 50, 5 * 60 * 1000l);
            }
            controlSlot(+1);
            try {
                if (!this.dl.startDownload()) {
                    try {
                        if (dl.externalDownloadStop()) {
                            return;
                        }
                    } catch (final Throwable e) {
                    }
                    /* unknown error, we disable multiple chunks */
                    if (link.getBooleanProperty(this.getHost() + OffCloudCom.NOCHUNKS, false) == false) {
                        link.setProperty(this.getHost() + OffCloudCom.NOCHUNKS, Boolean.valueOf(true));
                        throw new PluginException(LinkStatus.ERROR_RETRY);
                    }
                } else if (PluginJsonConfig.get(jd.plugins.hoster.OffCloudCom.OffCloudComPluginConfigInterface.class).isDeleteDownloadHistorySingleLinkEnabled()) {
                    /* Delete download history entry of downloaded file from history immediately after each download */
                    final String requestID = link.getStringProperty("offcloudrequestId", null);
                    deleteSingleDownloadHistoryEntry(requestID);
                }
            } catch (final PluginException e) {
                e.printStackTrace();
                // New V2 chunk errorhandling
                /* unknown error, we disable multiple chunks */
                if (link.getBooleanProperty(this.getHost() + OffCloudCom.NOCHUNKS, false) == false) {
                    link.setProperty(this.getHost() + OffCloudCom.NOCHUNKS, Boolean.valueOf(true));
                    throw new PluginException(LinkStatus.ERROR_RETRY);
                }
                throw e;
            } finally {
                // remove usedHost slot from hostMap
                // remove download slot
                controlSlot(-1);
            }
        } catch (final Exception e) {
            link.setProperty(directlinkproperty, Property.NULL);
            throw e;
        }
    }

    private String checkDirectLink(final DownloadLink link) {
        String dllink = link.getStringProperty(getDirectlinkproperty());
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                con = br2.openHeadConnection(dllink);
                if (this.looksLikeDownloadableContent(con)) {
                    return dllink;
                } else {
                    throw new IOException();
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
        setConstants(account, null);
        this.login(account, true);
        final AccountInfo ai = new AccountInfo();
        br.postPage("https://" + getHost() + "/stats/usage-left", "");
        final Map<String, Object> usageleft = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        long remaininglinksnum = ((Number) usageleft.get("links")).longValue();
        final long premiumUsageBytes = ((Number) usageleft.get("premiumUsage")).longValue();
        postAPISafe("https://" + getHost() + "/stats/addons", "");
        /*
         * Basically, at the moment we got 3 account types: Premium, Free account with generate-links feature, Free Account without
         * generate-links feature (used free account, ZERO traffic)
         */
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> activePackages = (List<Map<String, Object>>) entries.get("data");
        Map<String, Object> packagePremium = null;
        Map<String, Object> packageUnlimitedLinks = null;
        String packagesCommaSeparated = "";
        for (final Map<String, Object> packagemap : activePackages) {
            final String packagetype = packagemap.get("type").toString();
            /*
             * 2018-02-07: For some reason, the 'link-unlimited' package (if available) will always expire 1 month after the
             * "premium-downloading" package which is why we get our data from here. At this stage I have no idea whether this applies for
             * all accounts or only our test account as some years ago, they had different addons you could purchase and nowdays this is all
             * a lot simpler.
             */
            if (packagetype.equalsIgnoreCase("premium-downloading")) {
                packagePremium = packagemap;
            } else if (packagetype.equalsIgnoreCase("link-unlimited")) {
                packageUnlimitedLinks = packagemap;
            }
            if (packagesCommaSeparated.length() > 0) {
                packagesCommaSeparated += ", ";
            }
            packagesCommaSeparated += packagetype;
        }
        if (activePackages.size() == 0) {
            /* Free usually only has 1 package with packageType "premium-link-increase" */
            account.setType(AccountType.FREE);
            /* Important: If we found our package, get the remaining links count from there as the other one might be wrong! */
            final Object remainingLinksCount = entries.get("remainingLinksCount");
            if (remainingLinksCount instanceof Number) {
                remaininglinksnum = ((Number) remainingLinksCount).longValue();
            }
        } else if (packagePremium != null) {
            account.setType(AccountType.PREMIUM);
            ai.setUnlimitedTraffic();
            ai.setValidUntil(TimeFormatter.getMilliSeconds(packagePremium.get("activeTill").toString(), "yyyy-MM-dd'T'HH:mm:ss.SSSX", Locale.ENGLISH), br);
        } else {
            /* This should never happen */
            account.setType(AccountType.UNKNOWN);
        }
        if (packagePremium == null && remaininglinksnum == 0) {
            /* Free account that has no link generations left. */
            throw new AccountUnavailableException("No link generations left", 5 * 60 * 1000);
        }
        final SIZEUNIT maxSizeUnit = (SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue();
        String statusText = account.getType().getLabel() + " | Used so far: " + SIZEUNIT.formatValue(maxSizeUnit, premiumUsageBytes);
        if (packagesCommaSeparated != null && packagesCommaSeparated.length() > 0) {
            statusText += " | Addons: " + packagesCommaSeparated;
        }
        ai.setStatus(statusText);
        /* Only add hosts which are listed as 'active' (working) */
        postAPISafe("https://" + getHost() + "/stats/sites", "");
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        final List<String> supportedHostsTmp = new ArrayList<String>();
        final Map<String, Object> stats = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> filehosts = (List<Map<String, Object>>) stats.get("fs");
        /**
         * Explanation of their status-types: Healthy = working, Fragile = may work or not - if not will be fixed within the next 72 hours
         * (support also said it means that they currently have no accounts for this host), Limited = broken, will be fixed tomorrow, dead =
         * site offline or their plugin is completely broken, Limited = There are special daily limits for a host but it should work (even
         * though it is marked RED on the site), Awaiting Demand = Host is unsupported but is shown in that list so if a lot of users try
         * URLs of such hosts, Offcloud will see that there is demand and maybe add it to the list of supported hosts.
         */
        cloudOnlyHosts.clear();
        hostMaxchunksMap.clear();
        hostMaxdlsMap.clear();
        this.getAPISafe("https://" + getHost() + "/api/sites/chunks");
        final List<Map<String, Object>> chunklimits = (List<Map<String, Object>>) restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
        final OffCloudComPluginConfigInterface cfg = PluginJsonConfig.get(jd.plugins.hoster.OffCloudCom.OffCloudComPluginConfigInterface.class);
        for (final Map<String, Object> domaininfo : filehosts) {
            final String status = domaininfo.get("isActive").toString().toLowerCase(Locale.ENGLISH);
            // final String name = domaininfo.get("name").toString();
            final String domain = domaininfo.get("displayName").toString();
            final List<String> domains = (List<String>) domaininfo.get("tags");
            final Object deadFor = domaininfo.get("deadFor");
            /* Workaround to get real/mapped domain */
            supportedHostsTmp.clear();
            supportedHostsTmp.add(domain);
            ai.setMultiHostSupport(this, supportedHostsTmp);
            final List<String> realHostArrayTmp = ai.getMultiHostSupport();
            if (realHostArrayTmp != null && !realHostArrayTmp.isEmpty()) {
                /* Legacy handling */
                /* Add real host to full list of supported hosts */
                if (status.equals("cloud only")) {
                    cloudOnlyHosts.add(domain);
                }
            }
            final Object maxTrafficBytesO = domaininfo.get("maxAmount");
            final MultiHostHost mhost = new MultiHostHost(domain);
            mhost.addDomains(domains);
            Map<String, Object> thisHostChunkLimitsMap = null;
            for (final Map<String, Object> map : chunklimits) {
                final String host = map.get("host").toString();
                if (mhost.getDomains().contains(host)) {
                    thisHostChunkLimitsMap = map;
                    break;
                }
            }
            if (deadFor != null) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            } else if (!cfg.isShowHostersWithStatusAwaitingDemand() && status.equals("awaiting demand")) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            } else if (status.equals("fragile")) {
                mhost.setStatus(MultihosterHostStatus.WORKING_UNSTABLE);
            }
            mhost.setStatusText(status);
            if (maxTrafficBytesO instanceof Number) {
                final long maxTrafficBytes = ((Number) maxTrafficBytesO).longValue();
                mhost.setTrafficLeft(maxTrafficBytes);
                mhost.setTrafficMax(maxTrafficBytes);
            }
            if (thisHostChunkLimitsMap != null) {
                /* Set host specific limits */
                /* Legacy handling */
                final Object maxdls_object = thisHostChunkLimitsMap.get("maxChunksGlobal");
                final int maxchunks = ((Number) thisHostChunkLimitsMap.get("maxChunks")).intValue();
                hostMaxchunksMap.put(domain, this.correctChunks(maxchunks));
                if (maxdls_object instanceof Number) {
                    final int maxdls = ((Number) maxdls_object).intValue();
                    hostMaxdlsMap.put(domain, this.correctMaxdls(maxdls));
                }
                /* New handling */
                mhost.setMaxChunks(maxchunks);
            }
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        /* 2020-12-11: Check this account more often */
        account.setRefreshTimeout(15 * 60 * 1000l);
        /* Let's handle some settings stuff. */
        if (cfg.isClearAllowedIpAddressesEnabled()) {
            this.clearAllowedIPAddresses();
        }
        if (cfg.isDeleteDownloadHistoryCompleteInstantEnabled()) {
            this.deleteCompleteDownloadHistory(PROPERTY_DOWNLOADTYPE_instant);
            account.setProperty(PROPERTY_ACCOUNT_TIMESTAMP_LAST_TIME_DELETED_HISTORY, System.currentTimeMillis());
        }
        if (cfg.isDeleteDownloadHistoryCompleteCloudEnabled()) {
            this.deleteCompleteDownloadHistory(PROPERTY_DOWNLOADTYPE_cloud);
            account.setProperty(PROPERTY_ACCOUNT_TIMESTAMP_LAST_TIME_DELETED_HISTORY, System.currentTimeMillis());
        }
        if (packageUnlimitedLinks != null) {
            /* User has active "unlimited links" package */
            account.setProperty("accinfo_linksleft", -1);
        } else {
            account.setProperty("accinfo_linksleft", remaininglinksnum);
        }
        /* Save packages as json as we will use that information later */
        account.setProperty(PROPERTY_ACCOUNT_ACTIVE_PACKAGES_LIST, JSonStorage.serializeToJson(activePackages));
        return ai;
    }

    /**
     * Log into users' account and set login cookie
     *
     * @throws InterruptedException
     */
    private void login(final Account account, final boolean validateCookies) throws IOException, PluginException, InterruptedException {
        final Cookies cookies = account.loadCookies("");
        if (cookies != null) {
            br.setCookies(WEBSITE_BASE, cookies);
            if (!validateCookies) {
                logger.info("Trust cookies without check");
                return;
            }
            this.postAPISafe(API_BASE + "login/check", "");
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Object loggedINO = entries.get("loggedIn");
            if (loggedINO instanceof Number && ((Number) loggedINO).intValue() == 1) {
                logger.info("Cookie login successful");
                account.saveCookies(br.getCookies(br.getHost()), "");
                return;
            } else {
                logger.info("Cookie login failed");
                br.clearCookies(null);
                account.clearCookies("");
            }
        }
        logger.info("Performing full login");
        postAPISafe(API_BASE + "login/classic", "username=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
        final String logincookie = br.getCookie(this.getHost(), "connect.sid", Cookies.NOTDELETEDPATTERN);
        if (logincookie == null) {
            /* This should never happen as we got errorhandling for invalid logindata */
            throw new AccountUnavailableException("Unknown login error", 5 * 60 * 1000l);
        }
        account.saveCookies(br.getCookies(br.getHost()), "");
    }

    /**
     * This simply accesses the table of allowed IP addresses in the account and removes all IPs but the current one --> Avoids unnerving
     * "confirm your IP address" e-mails.
     */
    private void clearAllowedIPAddresses() {
        logger.info("Remove IP handling active: Removing all registered IPs but the current one");
        try {
            postAPISafe("https://www." + getHost() + "/account/registered-ips", "");
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final List<Map<String, Object>> data = (List<Map<String, Object>>) entries.get("data");
            if (data == null || data.size() == 0) {
                logger.info("Failed to find any data to remove");
                return;
            }
            final int ipcount = data.size();
            logger.info("Found " + ipcount + " active IPs");
            /* Delete all allowed IPs except the one the user has at the moment (first in list). */
            int index = -1;
            for (final Map<String, Object> ipinfo : data) {
                index++;
                if (index == 0) {
                    /* Skip first item as this is our own IP. Removing it would log us out. */
                    continue;
                }
                final String ip = ipinfo.get("ip").toString();
                postRawAPISafe("https://www." + getHost() + "/account/ip/remove/", "{\"ip\":\"" + ip + "\"}");
                final Map<String, Object> ip_deleted_response = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                if (Boolean.TRUE.equals(ip_deleted_response.get("result"))) {
                    logger.info("Successfully removed IP: " + ip);
                } else {
                    logger.warning("Failed to remove IP: " + ip);
                }
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
            }
        } catch (final Throwable ignore) {
            logger.warning("FATAL error occured in IP-remove handling!");
            ignore.printStackTrace();
        }
    }

    /**
     * Deletes the complete offcloud download history.
     *
     * Last revision with old (single request-ID delete) handling: 29703
     **/
    @SuppressWarnings({ "unchecked", "rawtypes" })
    private void deleteCompleteDownloadHistory(final String downloadtype) throws Exception {
        try {
            /* First let's find all requestIDs to delete. */
            int deletedDownloadHistoryEntriesNum = 0;
            logger.info("Deleting complete download history");
            final ArrayList<String> requestIDs = new ArrayList<String>();
            boolean isEnd = false;
            int page = 0;
            do {
                logger.info("Decrypting requestIDs of page: " + page);
                this.postRawAPISafe("https://" + getHost() + "/" + downloadtype + "/history", "{\"page\":" + page + "}");
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final List<Object> history = (List) entries.get("history");
                for (final Object historyentry_object : history) {
                    final Map<String, Object> historyentry = (Map<String, Object>) historyentry_object;
                    final String status = (String) historyentry.get("status");
                    /* Do not delete e.g. cloud-downloads which are still to be completely downloaded! */
                    if (!status.equals("downloading")) {
                        final String requestId = (String) historyentry.get("requestId");
                        if (requestId == null) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "WTF");
                        }
                        requestIDs.add(requestId);
                    }
                }
                isEnd = ((Boolean) entries.get("isEnd")).booleanValue();
                page++;
            } while (!isEnd);
            final int req_ids_size = requestIDs.size();
            logger.info("Found " + req_ids_size + " requestIDs to delete - starting deletion");
            /* Now let's delete them */
            int counter_success = 0;
            ArrayList<String> ids_to_delete = new ArrayList<String>();
            int index = 0;
            String postData = null;
            while (true) {
                ids_to_delete.clear();
                while (true) {
                    /* We delete 200 IDs at once */
                    if (index == requestIDs.size() || ids_to_delete.size() > 199) {
                        break;
                    }
                    ids_to_delete.add(requestIDs.get(index));
                    index++;
                }
                int temp_index = 0;
                /* Make sure not to send empty requests. */
                if (ids_to_delete.size() > 0) {
                    postData = "{\"requests\":[";
                    for (final String id : ids_to_delete) {
                        if (temp_index > 0) {
                            postData += ",";
                        }
                        postData += "\"" + id + "\"";
                        temp_index++;
                        counter_success++;
                        deletedDownloadHistoryEntriesNum++;
                    }
                    postData += "]}";
                    this.postRawAPISafe("https://" + getHost() + "/" + downloadtype + "/remove", postData);
                }
                if (index == requestIDs.size()) {
                    break;
                }
            }
            if (counter_success == req_ids_size) {
                logger.info("Successfully deleted all requestIDs: " + req_ids_size);
            } else {
                logger.info("Failed to delete some requestIDs. Successfully deleted " + counter_success + " of " + requestIDs.size() + " requestIDs");
            }
            this.currAcc.setProperty("req_ids_size", deletedDownloadHistoryEntriesNum);
        } catch (final Throwable ignore) {
            logger.warning("Failed to clear complete download history");
            ignore.printStackTrace();
        }
    }

    private boolean deleteSingleDownloadHistoryEntry(final String requestID) {
        final String downloadtype = getDownloadType();
        boolean success = false;
        try {
            try {
                logger.info("Trying to delete requestID from history: " + requestID);
                br.getPage("https://" + getHost() + "/" + downloadtype + "/remove/" + requestID);
                final Map<String, Object> resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                if (Boolean.TRUE.equals(resp.get("success"))) {
                    success = true;
                }
            } catch (final Throwable e) {
                success = false;
            }
        } catch (final Throwable ignore) {
            logger.log(ignore);
        }
        if (success) {
            logger.info("Succeeded to delete requestID from download " + downloadtype + " history: " + requestID);
        } else {
            logger.warning("Failed to delete requestID from download " + downloadtype + " history: " + requestID);
        }
        return success;
    }

    private String getDownloadType() {
        String type;
        if (PROPERTY_DOWNLOADTYPE_cloud.equals(this.currDownloadLink.getStringProperty(PROPERTY_DOWNLOADTYPE, null))) {
            type = PROPERTY_DOWNLOADTYPE_cloud;
        } else {
            type = PROPERTY_DOWNLOADTYPE_instant;
        }
        return type;
    }

    /* Returns the time difference between now and the last time the complete download history has been deleted. */
    private long getLast_deleted_complete_download_history_time_ago(final Account acc) {
        return System.currentTimeMillis() - acc.getLongProperty(PROPERTY_ACCOUNT_TIMESTAMP_LAST_TIME_DELETED_HISTORY, System.currentTimeMillis());
    }

    private void prepareBrForJsonRequest() {
        /* Set these headers before every request else json (ajax) requests will fail! */
        br.getHeaders().put("Accept", "Accept   application/json, text/plain, */*");
        br.getHeaders().put("Content-Type", "application/json;charset=utf-8");
    }

    private String getAPISafe(final String accesslink) throws IOException, PluginException, InterruptedException {
        br.getPage(accesslink);
        updatestatuscode();
        handleAPIErrors(this.br);
        return this.br.getRequest().getHtmlCode();
    }

    private String postAPISafe(final String accesslink, final String postdata) throws IOException, PluginException, InterruptedException {
        br.postPage(accesslink, postdata);
        updatestatuscode();
        handleAPIErrors(this.br);
        return this.br.getRequest().getHtmlCode();
    }

    private String postRawAPISafe(final String accesslink, final String postdata) throws IOException, PluginException, InterruptedException {
        prepareBrForJsonRequest();
        br.postPageRaw(accesslink, postdata);
        updatestatuscode();
        handleAPIErrors(this.br);
        return this.br.getRequest().getHtmlCode();
    }

    /**
     * 0 = everything ok, 1-99 = "error"-errors, 100-199 = "not_available"-errors, 200-299 = Other (html) [download] errors, sometimes mixed
     * with the API errors., 666 = hell
     */
    private void updatestatuscode() {
        // TODO: Use json parser
        String error = PluginJSonUtils.getJsonValue(br, "error");
        if (error == null) {
            error = PluginJSonUtils.getJsonValue(br, "not_available");
        }
        if (error != null) {
            if (error.equals("Please enter a valid email address.")) {
                statuscode = 1;
            } else if (error.equals("NOPREMIUMACCOUNTS")) {
                statuscode = 2;
            } else if (error.equals("User not authorized")) {
                statuscode = 3;
            } else if (error.equals("Purchase a premium downloading addon to continue with this operation.")) {
                statuscode = 4;
            } else if (error.equals("The credentials entered are wrong.") || error.equals("The credentials entered are wrong. ")) {
                statuscode = 5;
            } else if (error.equals("File will not be downloaded due to an error.")) {
                statuscode = 6;
            } else if (error.equals("The supported site is temporarily disabled. We are working to resolve the problem quickly. Please try again later. Sorry for the inconvenience.") || error.contains("The supported site is temporarily disabled")) {
                statuscode = 7;
            } else if (error.equals("User is not allowed this operation.")) {
                statuscode = 8;
            } else if (error.equals("IP address needs to be registered. Check your email for further information.")) {
                statuscode = 9;
            } else if (error.equals("There is a networking issue. Please contact our support to get some help.")) {
                statuscode = 10;
            } else if (error.matches("Premium account quota for .*? exceeded today\\. Please try to download again later\\.") || error.matches("You have exceeded your daily quota for.*?")) {
                statuscode = 11;
            } else if (error.equals("The file is not available on the server.")) {
                statuscode = 12;
            } else if (error.equals("Unregistered IP address detected.")) {
                statuscode = 13;
            } else if (error.equals("Incorrect url")) {
                statuscode = 14;
            } else if (error.matches("We are sorry but .*? links are supported only via Cloud downloading\\.")) {
                statuscode = 15;
            } else if (error.matches("Error during downloading .+")) {
                statuscode = 16;
            } else if (error.equals("premium")) {
                statuscode = 100;
            } else if (error.equals("links")) {
                statuscode = 101;
            } else if (error.equals("proxy")) {
                statuscode = 102;
            } else if (error.equals("video")) {
                statuscode = 103;
            } else if (error.equals("cloud")) {
                statuscode = 104;
            } else {
                statuscode = 666;
            }
        } else {
            /* Check for html errors */
            /* The following errors will usually happen after the attempt to download the generated 'directlink'. */
            if (br.containsHTML("We\\'re sorry but your download ticket couldn\\'t have been found, please repeat the download process\\.")) {
                /* TODO: Remove this as it is an errormessage from uploaded.net which they directly forwarded. */
                statuscode = 200;
            } else if (br.containsHTML(">Error: Error\\[Account isn\\'t Premium\\!\\]<")) {
                statuscode = 201;
            } else if (br.containsHTML(">Error: We are sorry, but the requested URL cannot be downloaded now")) {
                statuscode = 202;
            } else if (br.containsHTML(">Error: \\[cURL:56\\] Problem")) {
                statuscode = 203;
            } else if (br.containsHTML(">Error: Premium account is out of bandwidth")) {
                statuscode = 204;
            } else if (br.containsHTML(">Error: The requested URL was not found on the server<")) {
                statuscode = 205;
            } else {
                /* No way to tell that something unpredictable happened here --> status should be fine. */
                statuscode = 0;
            }
        }
    }

    private void handleAPIErrors(final Browser br) throws PluginException, InterruptedException {
        String statusMessage = null;
        try {
            switch (statuscode) {
            case 0:
                /* Everything ok */
                break;
            case 1:
                /* No email entered --> Should never happen as we validate user-input before -> permanently disable account */
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    statusMessage = "\r\nUngültiger Benutzername oder ungültiges Passwort!\r\nDu bist dir sicher, dass dein eingegebener Benutzername und Passwort stimmen? Versuche folgendes:\r\n1. Falls dein Passwort Sonderzeichen enthält, ändere es (entferne diese) und versuche es erneut!\r\n2. Gib deine Zugangsdaten per Hand (ohne kopieren/einfügen) ein.";
                } else {
                    statusMessage = "\r\nInvalid username/password!\r\nYou're sure that the username and password you entered are correct? Some hints:\r\n1. If your password contains special characters, change it (remove them) and try again!\r\n2. Type in your username/password by hand without copy & paste.";
                }
                throw new PluginException(LinkStatus.ERROR_PREMIUM, statusMessage, PluginException.VALUE_ID_PREMIUM_DISABLE);
            case 2:
                /* No premiumaccounts available for used host --> Temporarily disable it */
                statusMessage = "There are currently no premium accounts available for this host";
                mhm.putError(this.currAcc, this.currDownloadLink, 5 * 60 * 1000l, statusMessage);
            case 3:
                if (this.currAcc.getType() == AccountType.FREE) {
                    /* Free account limits reached and an additional download-try failed -> permanently disable account */
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        statusMessage = "\r\nFree Account Limits erreicht. Kaufe dir einen premium Account um weiter herunterladen zu können.";
                    } else {
                        statusMessage = "\r\nFree account limits reached. Buy a premium account to continue downloading.";
                    }
                    this.currAcc.getAccountInfo().setTrafficLeft(0);
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, statusMessage, PluginException.VALUE_ID_PREMIUM_DISABLE);
                } else {
                    /*
                     * If this happens while we're using a premium account it can be considered as a server issue - do NOT permanently
                     * disable premium accounts of this happens!
                     */
                    logger.warning("WTF: 'User not authorized' error happened in premium handling --> Probably serverside issue --> Retrying");
                    mhm.handleErrorGeneric(this.currAcc, this.currDownloadLink, "user_not_authorized_server_error_internal_errorcode_3", 50, 5 * 60 * 1000l);
                }
            case 4:
                freeAccountLimitReached();
            case 5:
                /* Username or password invalid -> permanently disable account */
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    statusMessage = "\r\nUngültiger Benutzername oder ungültiges Passwort!\r\nDu bist dir sicher, dass dein eingegebener Benutzername und Passwort stimmen? Versuche folgendes:\r\n1. Falls dein Passwort Sonderzeichen enthält, ändere es (entferne diese) und versuche es erneut!\r\n2. Gib deine Zugangsdaten per Hand (ohne kopieren/einfügen) ein.";
                } else {
                    statusMessage = "\r\nInvalid username/password!\r\nYou're sure that the username and password you entered are correct? Some hints:\r\n1. If your password contains special characters, change it (remove them) and try again!\r\n2. Type in your username/password by hand without copy & paste.";
                }
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            case 6:
                /* "File will not be downloaded due to an error." --> WTF */
                // statusMessage = "WTF";
                statusMessage = "File will not be downloaded due to an error.";
                mhm.handleErrorGeneric(this.currAcc, this.currDownloadLink, statusMessage, 50, 5 * 60 * 1000l);
            case 7:
                /* Host is temporarily disabled --> Temporarily disable it */
                statusMessage = "Host is temporarily disabled";
                mhm.putError(this.currAcc, this.currDownloadLink, 5 * 60 * 1000l, statusMessage);
            case 8:
                /*
                 * 'User is not allowed this operation.' --> Basically the meaning is unknown but in this case, the host does not work
                 * either --> Disable it for a short period of time.
                 */
                statusMessage = "'User is not allowed this operation.' --> Host is temporarily disabled";
                mhm.putError(this.currAcc, this.currDownloadLink, 5 * 60 * 1000l, statusMessage);
            case 9:
                /* User needs to confirm his current IP. */
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    statusMessage = "\r\nBitte bestätige deine aktuelle IP Adresse über den Bestätigungslink per E-Mail um den Account wieder nutzen zu können.";
                } else {
                    statusMessage = "\r\nPlease confirm your current IP adress via the activation link you got per mail to continue using this account.";
                }
                throw new PluginException(LinkStatus.ERROR_PREMIUM, statusMessage, PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
            case 10:
                /* Networking issues --> Serverside problems --> Temporarily disable account */
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    statusMessage = "\r\nServerseitige Netzwerkprobleme - bitte den offcloud Support kontaktieren.";
                } else {
                    statusMessage = "\r\nServerside networking problems - please contact the Offcloud support.";
                }
                mhm.handleErrorGeneric(this.currAcc, this.currDownloadLink, statusMessage, 50, 5 * 60 * 1000l);
            case 11:
                /*
                 * Daily quota of host reached --> Temporarily disable it (usually hosts are then listed as "Limited" and will not be added
                 * to the supported host list on next fetchAccountInfo anyways
                 */
                statusMessage = "Hosts' (daily) quota reached - temporarily disabling it";
                mhm.putError(this.currAcc, this.currDownloadLink, 5 * 60 * 1000l, statusMessage);
            case 12:
                /* File is offline --> Display correct status to user. */
                statusMessage = "File is offline";
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            case 13:
                /*
                 * Happens when a user tries to download directlink of another user - should never happen inside JD but if, the user will
                 * probably have to confirm his current IP.
                 */
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    statusMessage = "\r\nBitte bestätige deine aktuelle IP Adresse über den Bestätigungslink per E-Mail um den Account wieder nutzen zu können.";
                } else {
                    statusMessage = "\r\nPlease confirm your current IP adress via the activation link you got per mail to continue using this account.";
                }
                throw new PluginException(LinkStatus.ERROR_PREMIUM, statusMessage, PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
            case 14:
                /* Invalid url --> Host probably not supported --> Disable it */
                statusMessage = "Host is temporarily disabled";
                mhm.putError(this.currAcc, this.currDownloadLink, 5 * 60 * 1000l, statusMessage);
            case 15:
                /*
                 * Current host is only supported via cloud downloading --> Add to Cloud-Array and try again
                 * 
                 * This should only happen if e.g. a user starts JD and starts downloads right away before the cloudOnlyHosts array gets
                 * updated. This cann be considered as a small workaround.
                 */
                statusMessage = "This host is only supported via cloud downloading";
                cloudOnlyHosts.add(this.currDownloadLink.getHost());
                throw new PluginException(LinkStatus.ERROR_RETRY, "This host is only supported via cloud downloading");
            case 16:
                /*
                 * Current host is only supported via cloud downloading --> Add to Cloud-Array and try again
                 * 
                 * This should only happen if e.g. a user starts JD and starts downloads right away before the cloudOnlyHosts array gets
                 * updated. This extra errorhandling can be considered as a small workaround.
                 */
                statusMessage = "Cloud download failed 'Error during downloading'";
                mhm.handleErrorGeneric(this.currAcc, this.currDownloadLink, statusMessage, 50, 5 * 60 * 1000l);
            case 100:
                /* Free account limits reached -> permanently disable account */
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    statusMessage = "\r\nFree Account Limits erreicht. Kaufe dir einen premium Account um weiter herunterladen zu können.";
                } else {
                    statusMessage = "\r\nFree account limits reached. Buy a premium account to continue downloading.";
                }
                throw new PluginException(LinkStatus.ERROR_PREMIUM, statusMessage, PluginException.VALUE_ID_PREMIUM_DISABLE);
            case 101:
                statusMessage = "You must purchase a Link increase addon for this download";
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, statusMessage);
            case 102:
                statusMessage = "You must purchase a proxy downloading addon for this download";
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, statusMessage);
            case 103:
                statusMessage = "You must purchase a video sharing site support addon for this download";
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, statusMessage);
            case 104:
                statusMessage = "You must purchase a cloud downloading upgrade addon for this download";
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, statusMessage);
            case 200:
                /* Free account limits reached -> permanently disable account */
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    statusMessage = "\r\nDownload-Ticket defekt --> Neuversuch";
                } else {
                    statusMessage = "\r\nDownload-ticket broken --> Retry link";
                }
                mhm.handleErrorGeneric(this.currAcc, this.currDownloadLink, statusMessage, 50, 5 * 60 * 1000l);
            case 201:
                /* Host account of multihost is expired --> Disable host for a long period of time! */
                statusMessage = "Host account of multihost is expired";
                mhm.putError(this.currAcc, this.currDownloadLink, 5 * 60 * 1000l, statusMessage);
            case 202:
                /* Specified link cannot be downloaded right now (for some time) */
                statusMessage = "Link cannot be downloaded at the moment";
                mhm.putError(this.currAcc, this.currDownloadLink, 3 * 60 * 1000l, statusMessage);
            case 203:
                /*
                 * Strange forwarded cURL error --> We know it usually only happens when traffic of a host is gone --> Disable it for a long
                 * time
                 */
                statusMessage = "Strange cURL error -> Host traffic gone";
                mhm.putError(this.currAcc, this.currDownloadLink, 3 * 60 * 1000l, statusMessage);
            case 204:
                /*
                 * Traffic of host account of multihost is empty.
                 */
                statusMessage = "Host traffic gone";
                mhm.putError(this.currAcc, this.currDownloadLink, 3 * 60 * 1000l, statusMessage);
            case 205:
                /*
                 * Basically this is an internal problem - the errormessage itself has no particular meaning for the user - we can only try
                 * some more before giving up.
                 */
                statusMessage = "Server says 'Error: The requested URL was not found on the server'";
                mhm.handleErrorGeneric(this.currAcc, this.currDownloadLink, statusMessage, 50, 5 * 60 * 1000l);
            default:
                /* Unknown error */
                statusMessage = "Unknown error";
                logger.info(this.getHost() + ": Unknown API error");
                mhm.handleErrorGeneric(this.currAcc, this.currDownloadLink, statusMessage, 50, 5 * 60 * 1000l);
            }
        } catch (final PluginException e) {
            logger.info(this.getHost() + ": Exception: statusCode: " + statuscode + " statusMessage: " + statusMessage);
            throw e;
        }
    }

    private void freeAccountLimitReached() throws PluginException {
        /*
         * Free account limits reached and an additional download-try failed or account cookie is invalid -> permanently disable account
         */
        String statusMessage;
        if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
            statusMessage = "\r\nFree Account Limits erreicht. Kaufe einen premium Account um weiter herunterladen zu können.";
        } else {
            statusMessage = "\r\nFree account limits reached. Buy a premium account to continue downloading.";
        }
        throw new PluginException(LinkStatus.ERROR_PREMIUM, statusMessage, PluginException.VALUE_ID_PREMIUM_DISABLE);
    }

    /** Corrects input so that it fits what we use in our plugins. */
    private int correctChunks(int maxchunks) {
        if (maxchunks < 1) {
            maxchunks = 1;
        } else if (maxchunks > 1) {
            maxchunks = -maxchunks;
        }
        /* Else maxchunks == 1 */
        return maxchunks;
    }

    /** Corrects input so that it fits what we use in our plugins. */
    private int correctMaxdls(int maxdls) {
        if (maxdls < 1) {
            maxdls = 1;
        } else if (maxdls > 20) {
            maxdls = 20;
        }
        /* Else we should have a valid value! */
        return maxdls;
    }

    /**
     * Prevents more than one free download from starting at a given time. One step prior to dl.startDownload(), it adds a slot to maxFree
     * which allows the next singleton download to start, or at least try.
     *
     * This is needed because xfileshare(website) only throws errors after a final dllink starts transferring or at a given step within pre
     * download sequence. But this template(XfileSharingProBasic) allows multiple slots(when available) to commence the download sequence,
     * this.setstartintival does not resolve this issue. Which results in x(20) captcha events all at once and only allows one download to
     * start. This prevents wasting peoples time and effort on captcha solving and|or wasting captcha trading credits. Users will experience
     * minimal harm to downloading as slots are freed up soon as current download begins.
     *
     * @param controlSlot
     *            (+1|-1)
     */
    private void controlSlot(final int num) {
        synchronized (CTRLLOCK) {
            final String currentHost = this.currDownloadLink.getHost();
            int was = maxPrem.get();
            maxPrem.set(Math.min(Math.max(1, maxPrem.addAndGet(num)), ACCOUNT_PREMIUM_MAXDOWNLOADS));
            logger.info("maxPrem was = " + was + " && maxPrem now = " + maxPrem.get());
            AtomicInteger currentRunningDls = new AtomicInteger(0);
            if (hostRunningDlsNumMap.containsKey(currentHost)) {
                currentRunningDls = hostRunningDlsNumMap.get(currentHost);
            }
            currentRunningDls.set(currentRunningDls.get() + num);
            hostRunningDlsNumMap.put(currentHost, currentRunningDls);
        }
    }

    public static interface OffCloudComPluginConfigInterface extends PluginConfigInterface {
        class Translation {
            public String getDeleteDownloadHistorySingleLinkEnabled_description() {
                return "<html>Delete downloaded link entry from the offcloud 'Instant' & 'Cloud' download history after successful download?\r\n<html><b>Note that this does NOT delete the complete download history but only the entry of the SUCCESSFULLY downloaded link!</b></html>";
            }

            public String getDeleteDownloadHistoryCompleteInstantEnabled_description() {
                return "<html>Delete complete 'Instant' download history on each account check?\r\n<html><p style=\"color:#F62817\">Note that this process happens during the account check.\r\nEspecially if you have a lot of links, the first time can take over 10 minutes!</p></html>";
            }

            public String getDeleteDownloadHistoryCompleteCloudEnabled_description() {
                return "<html>Delete complete 'Cloud' download history on each account check?\r\n<html><p style=\"color:#F62817\">Note that this process happens during the account check.\r\nEspecially if you have a lot of links, the first time can take over 10 minutes!\r\nOnly failed- and completed entries will be deleted - entries which are still downloading will NOT be deleted!</p></html>";
            }

            public String getClearAllowedIpAddressesEnabled_description() {
                return "<html>Activate 'Confirm IP' workaround?\r\nIn case you often get E-Mails from offcloud to confirm your current IP address, this setting may help.\r\nThis will always delete all of your allowed IPs except your current IP from your offcloud account.\r\n<html><p style=\"color:#F62817\">WARNING: Do NOT use this function in case you\r\n-Use multiple internet connections (IPs) at the same time\r\n-Share your offcloud account with friends\r\n-Use one or more proxies (or VPNs)</p></html>";
            }

            public String getShowHostersWithStatusAwaitingDemand_label() {
                return "<html>Include hosts with status 'Awaiting Demand' in hostlist?</html>";
            }

            public String getShowHostersWithStatusAwaitingDemand_description() {
                return "<html>Keep in mind that such hosts are not (yet) supported by Offcloud; the only purpose of this is that when users try to download from such hosts via JDownloader, Offcloud can see that there is demand and might add them to the list of supported hosts in the future.<html>";
            }
        }

        public static final Translation TRANSLATION = new Translation();

        @AboutConfig
        @DefaultBooleanValue(false)
        @TakeValueFromSubconfig("CLEAR_DOWNLOAD_HISTORY_SINGLE_LINK")
        boolean isDeleteDownloadHistorySingleLinkEnabled();

        void setDeleteDownloadHistorySingleLinkEnabled(boolean b);

        @AboutConfig
        @DefaultBooleanValue(false)
        @TakeValueFromSubconfig("CLEAR_DOWNLOAD_HISTORY_COMPLETE")
        boolean isDeleteDownloadHistoryCompleteInstantEnabled();

        void setDeleteDownloadHistoryCompleteInstantEnabled(boolean b);

        @AboutConfig
        @DefaultBooleanValue(false)
        @TakeValueFromSubconfig("CLEAR_DOWNLOAD_HISTORY_COMPLETE_CLOUD")
        boolean isDeleteDownloadHistoryCompleteCloudEnabled();

        void setDeleteDownloadHistoryCompleteCloudEnabled(boolean b);

        @AboutConfig
        @DefaultBooleanValue(false)
        @TakeValueFromSubconfig("CLEAR_ALLOWED_IP_ADDRESSES")
        boolean isClearAllowedIpAddressesEnabled();

        void setClearAllowedIpAddressesEnabled(boolean b);

        @AboutConfig
        @DefaultBooleanValue(false)
        boolean isShowHostersWithStatusAwaitingDemand();

        void setShowHostersWithStatusAwaitingDemand(boolean b);
    }

    @Override
    public int getMaxSimultanDownload(final DownloadLink link, final Account account) {
        if (account != null && (isUsenetLink(link) || link == null)) {
            /* 2020-06-15: Limit according to admin. See also: https://offcloud.com/api/sites/chunks */
            return 3;
        } else {
            return maxPrem.get();
        }
    }

    @Override
    public void extendAccountSettingsPanel(Account account, final PluginConfigPanelNG panel) {
        super.extendAccountSettingsPanel(account, panel);
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            /* When ai is not set, the information below isn't available anyways */
            return;
        }
        final long last_deleted_complete_download_history_time_ago = getLast_deleted_complete_download_history_time_ago(account);
        final long last_deleted_links_count = account.getLongProperty("req_ids_size", -1);
        final String deleted_links_user_display;
        String lastDeletedCompleteDownloadlistUserDisplay;
        if (last_deleted_complete_download_history_time_ago == 0) {
            lastDeletedCompleteDownloadlistUserDisplay = _GUI.T.lit_never();
            deleted_links_user_display = "-";
        } else {
            lastDeletedCompleteDownloadlistUserDisplay = TimeFormatter.formatMilliSeconds(last_deleted_complete_download_history_time_ago, 0);
            deleted_links_user_display = Long.toString(last_deleted_links_count);
        }
        String linksleft = account.getStringProperty("accinfo_linksleft", "?");
        if (linksleft.equals("-1")) {
            linksleft = _GUI.T.lit_unlimited();
        }
        panel.addSeperator();
        panel.addStringPair(_GUI.T.plugins_offcloudcom_linksleft(), linksleft);
        panel.addStringPair(_GUI.T.plugins_offcloudcom_historydeleted(), lastDeletedCompleteDownloadlistUserDisplay);
        panel.addStringPair(_GUI.T.plugins_offcloudcom_ACCOUNT_HISTORYDELETED_COUNT(), deleted_links_user_display);
        addPackageInformation: if (true) {
            List<Map<String, Object>> activePackages = null;
            try {
                activePackages = (List<Map<String, Object>>) restoreFromString(account.getStringProperty(PROPERTY_ACCOUNT_ACTIVE_PACKAGES_LIST), TypeRef.OBJECT);
            } catch (final Throwable ignore) {
                break addPackageInformation;
            }
            if (activePackages == null || activePackages.size() == 0) {
                /* No extra information available */
                break addPackageInformation;
            }
            /* Prepare/parse data */
            final List<Map<String, Date>> parsed_data = new ArrayList<Map<String, Date>>();
            try {
                for (final Map<String, Object> activePackage : activePackages) {
                    final long millis = TimeFormatter.getMilliSeconds(activePackage.get("activeTill").toString(), "yyyy-MM-dd'T'HH:mm:ss.SSSX", Locale.ENGLISH);
                    final Date date = new Date(millis);
                    final HashMap<String, Date> map = new HashMap<String, Date>();
                    map.put(activePackage.get("type").toString(), date);
                    parsed_data.add(map);
                }
            } catch (final Throwable ignore) {
                logger.log(ignore);
                logger.warning("Unexpected format of item in activePackages");
                break addPackageInformation;
            }
            panel.addSeperator();
            panel.addDescriptionPlain("Active addons");
            final Icon icon_okay = NewTheme.I().getIcon(IconKey.ICON_OK, 16);
            final ExtTableModel<Map<String, Date>> tableModel = new ExtTableModel<Map<String, Date>>("OffcloudPremiumAddonsTable") {
                @Override
                protected void initColumns() {
                    addColumn(new ExtTextColumn<Map<String, Date>>("Premium addon") {
                        @Override
                        public String getStringValue(final Map<String, Date> data) {
                            return data.keySet().iterator().next();
                        }

                        @Override
                        public Icon getIcon(Map<String, Date> data) {
                            return icon_okay;
                        }
                    });
                    addColumn(new ExtDateColumn<Map<String, Date>>(_GUI.T.premiumaccounttablemodel_column_expiredate()) {
                        @Override
                        protected Date getDate(Map<String, Date> data, Date date) {
                            return data.values().iterator().next();
                        }
                    });
                }
            };
            tableModel._fireTableStructureChanged(parsed_data, false);
            final BasicJDTable<Map<String, Date>> table = new BasicJDTable<Map<String, Date>>(tableModel);
            table.setPreferredScrollableViewportSize(new Dimension(table.getPreferredSize().width, table.getRowHeight() * table.getRowCount()));
            table.setSearchEnabled(false);
            table.addMouseWheelListener(new MouseWheelListener() {
                @Override
                public void mouseWheelMoved(MouseWheelEvent e) {
                    /*
                     * Forward event to upper panel so that the scrolling happens in it and not in our table which is always full-size and
                     * has no vertical scrollbar.
                     */
                    panel.dispatchEvent(e);
                }
            });
            final JScrollPane scrollPane = new JScrollPane(table);
            panel.add(scrollPane);
        }
    }

    private final String getDirectlinkproperty() {
        return this.getHost() + "directlink";
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        if (link == null) {
            return;
        }
        /*
         * Sometimes saved offcloud directlinks cause problems, are very slow or time out so this gives us a higher chance of a working
         * download after a reset.
         */
        link.removeProperty(getDirectlinkproperty());
        link.setProperty("offcloudrequestId", Property.NULL);
    }

    /** Usenet is available as of 2020-06-15 */
    @Override
    public List<UsenetServer> getAvailableUsenetServer() {
        final List<UsenetServer> ret = new ArrayList<UsenetServer>();
        // ret.addAll(UsenetServer.createServerList("usenet.offcloud.com", false, 119));
        ret.addAll(UsenetServer.createServerList("usenet.offcloud.com", true, 563));
        return ret;
    }

    public static interface OffCloudComUsenetAccountConfig extends UsenetAccountConfigInterface {
    }

    @Override
    public Class<? extends OffCloudComPluginConfigInterface> getConfigInterface() {
        return OffCloudComPluginConfigInterface.class;
    }
}