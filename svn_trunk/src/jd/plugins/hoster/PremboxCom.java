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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.ConditionalSkipReasonException;
import org.jdownloader.plugins.WaitingSkipReason;
import org.jdownloader.plugins.WaitingSkipReason.CAUSE;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.http.Browser;
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

@HostPlugin(revision = "$Revision: 51132 $", interfaceVersion = 3, names = { "prembox.com" }, urls = { "" })
public class PremboxCom extends PluginForHost {
    private static final String                   CLEAR_DOWNLOAD_HISTORY                     = "CLEAR_DOWNLOAD_HISTORY_COMPLETE";
    /* Properties */
    private static final String                   PROPERTY_DOWNLOADTYPE                      = "premboxdownloadtype";
    private static final String                   PROPERTY_DOWNLOADTYPE_instant              = "premboxdownloadtype_instant";
    private static final String                   PROPERTY_DOWNLOADTYPE_cloud                = "premboxdownloadtype_cloud";
    private static final String                   PROPERTY_ENFORCE_CLOUD_DOWNLOAD            = "premboxdownloadtype_enforce_cloud_download";
    private static final String                   PROPERTY_ACCOUNT_last_time_deleted_history = "last_time_deleted_history";
    /* Other constants & properties */
    private static final String                   API_SERVER                                 = "http://prembox.com/uapi";
    private static MultiHosterManagement          mhm                                        = new MultiHosterManagement("prembox.com");
    private static final String                   PROPERTY_DLLINK_GENERATED_TIMESTAMP        = "PREMBOX_DLLINK_GENERATED_TIMESTAMP";
    /*
     * This is the interval in which the complete download history will be deleted from the account (if setting is checked by the user && JD
     * does check the account)
     */
    private static final long                     DELETE_COMPLETE_DOWNLOAD_HISTORY_INTERVAL  = 1 * 60 * 60 * 1000l;
    /* Contains <host><Boolean resume possible|impossible> */
    private static HashMap<String, Boolean>       hostResumeMap                              = new HashMap<String, Boolean>();
    /* Contains <host><number of max possible chunks per download> */
    private static HashMap<String, Integer>       hostMaxchunksMap                           = new HashMap<String, Integer>();
    /* Contains <host><number of max possible simultan downloads> */
    private static HashMap<String, Integer>       hostMaxdlsMap                              = new HashMap<String, Integer>();
    /* Contains <host><number of currently running simultan downloads> */
    private static HashMap<String, AtomicInteger> hostRunningDlsNumMap                       = new HashMap<String, AtomicInteger>();
    /* List of hosts which are only available via cloud (queue) download system */
    public static ArrayList<String>               cloudOnlyHosts                             = new ArrayList<String>();
    private static Object                         CTRLLOCK                                   = new Object();
    private static AtomicInteger                  maxPrem                                    = new AtomicInteger(1);

    @SuppressWarnings("deprecation")
    public PremboxCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/register.html");
        this.setConfigElements();
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setCustomCharset("utf-8");
        br.setConnectTimeout(60 * 1000);
        br.setReadTimeout(60 * 1000);
        /* Not necessarily needed as long as we use the API */
        br.setCookie(getHost(), "lang", "en");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/contact.html";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
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
                    throw new ConditionalSkipReasonException(new WaitingSkipReason(CAUSE.HOST_TEMP_UNAVAILABLE, 2 * 1000, getHost() + ": Individual host max concurrent download limit reached"));
                }
            }
        }
        return super.canHandle(link, account);
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        /* handle premium should never be called */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        /*
         * When JD is started the first time and the user starts downloads right away, a full login might not yet have happened but it is
         * needed to get the individual host limits.
         */
        synchronized (CTRLLOCK) {
            if (hostMaxchunksMap.isEmpty() || hostMaxdlsMap.isEmpty() || hostResumeMap.isEmpty()) {
                logger.info("Calling fetchAccountInfo to set individual host limits");
                this.fetchAccountInfo(account);
            }
        }
        if (!attemptStoredDownloadurlDownload(link)) {
            final long timestampGeneratedDirecturlExpires = link.getLongProperty(PROPERTY_DLLINK_GENERATED_TIMESTAMP, System.currentTimeMillis()) + 3 * 24 * 60 * 60 * 1000;
            final long timeUntilNextTryPossible = System.currentTimeMillis() - timestampGeneratedDirecturlExpires;
            if (timeUntilNextTryPossible > 0) {
                /**
                 * 2017-09-27: According to admin, generated directurls last 3 days. <br />
                 * Their system is messed up which means if we request a new directurl because the old generated one does not work, this
                 * pulls more traffic from the users' account this 'JDownloader is wasting traffic'. <br />
                 * We've told the admin numerous times that the issue is on their side and happens because of bad design but he wanted us to
                 * fix it by re-using generated directurls for the full 3 days [= do not generate new URL if current url is < 3 days old,
                 * regardless whether that url is downloadable or not].
                 */
                logger.info("Old generated directurl has been generated less than 3 days ago --> NOT generating new directurl");
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Not yet allowed to generate new directurl", timeUntilNextTryPossible);
            }
            logger.info("Generating new directurl");
            String dllink = null;
            final UrlQuery query = getLoginPassPostData(account);
            final String url = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
            query.appendEncoded("url", url);
            if (cloudOnlyHosts.contains(link.getHost()) || link.hasProperty(PROPERTY_ENFORCE_CLOUD_DOWNLOAD)) {
                /* Try max 10 minutes */
                int counter = 0;
                int count_max = 15;
                /* TODO: Use json parser here */
                query.appendEncoded("directDownload", "0");
                br.postPage(API_SERVER + "/downloadLink", query);
                this.handleErrors(br, account, link);
                /* 'downloadLink' value will be "fileNotReadyYet" at this stage. */
                do {
                    // this.postAPISafe(API_SERVER + "/serverFileStatus", "login=" + JSonUtils.escape(this.currAcc.getUser()) + "&pass=" +
                    // JSonUtils.escape(this.currAcc.getPass()) + "&url=" + Encoding.urlEncode(this.currDownloadLink.getDownloadURL()));
                    final UrlQuery query2 = getLoginPassPostData(account);
                    query2.appendEncoded("url", url);
                    br.postPage(API_SERVER + "/serverFileStatus", query2);
                    this.handleErrors(br, account, link);
                    dllink = PluginJSonUtils.getJsonValue(br, "downloadLink");
                    if (!StringUtils.isEmpty(dllink)) {
                        break;
                    }
                    /* As long as the file is not downloaded to the prembox servers, 'downloadLink' field will be "". */
                    counter++;
                    /* Admin asked us to use 20 seconds. */
                    this.sleep(20000l, link);
                } while (counter <= count_max);
                if (StringUtils.isEmpty(dllink) || !dllink.startsWith("http")) {
                    /* Should never happen */
                    mhm.handleErrorGeneric(account, link, "Serverside download timed out", 20);
                }
            } else {
                link.setProperty(PROPERTY_DOWNLOADTYPE, PROPERTY_DOWNLOADTYPE_instant);
                /* TODO: Use json parser here */
                query.appendEncoded("directDownload", "1");
                br.postPage(API_SERVER + "/downloadLink", query);
                this.handleErrors(br, account, link);
                // this.postAPISafe(API_SERVER + "/serverFileStatus", "directDownload=1&login=" + JSonUtils.escape(this.currAcc.getUser()) +
                // "&pass=" + JSonUtils.escape(this.currAcc.getPass()) + "&url=" +
                // Encoding.urlEncode(this.currDownloadLink.getDownloadURL()));
                dllink = PluginJSonUtils.getJsonValue(br, "downloadLink");
                /* Check if the url starts with "http" --> Extra errorhandling for faulty API responses */
                if (StringUtils.isEmpty(dllink) || !dllink.startsWith("http")) {
                    /* Should never happen */
                    mhm.handleErrorGeneric(account, link, "dllinknull", 50);
                }
            }
            link.setProperty(PROPERTY_DLLINK_GENERATED_TIMESTAMP, System.currentTimeMillis());
            link.setProperty(getDirectlinkProperty(), dllink);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, isResumable(link), getMaxChunks(link));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                handleErrors(this.br, account, link);
                mhm.handleErrorGeneric(account, link, "unknowndlerror", 50);
            }
        } else {
            logger.info("Re-using generated directurl for downloading");
        }
        controlSlot(+1);
        try {
            this.dl.startDownload();
        } finally {
            // remove usedHost slot from hostMap
            // remove download slot
            controlSlot(-1);
        }
    }

    private final String getDirectlinkProperty() {
        return this.getHost() + "directlink";
    }

    private boolean isResumable(final DownloadLink link) {
        /* Then check if we got an individual limit. */
        synchronized (hostMaxchunksMap) {
            if (hostMaxchunksMap.containsKey(link.getHost())) {
                return hostResumeMap.get(link.getHost());
            }
        }
        return true;
    }

    private int getMaxChunks(final DownloadLink link) {
        /* Then check if we got an individual limit. */
        synchronized (hostMaxchunksMap) {
            if (hostMaxchunksMap.containsKey(link.getHost())) {
                return hostMaxchunksMap.get(link.getHost());
            }
        }
        return 0;
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link) throws Exception {
        final String url = link.getStringProperty(getDirectlinkProperty());
        if (StringUtils.isEmpty(url)) {
            return false;
        } else {
            try {
                final Browser brc = br.cloneBrowser();
                dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, isResumable(link), getMaxChunks(link));
                if (this.looksLikeDownloadableContent(dl.getConnection())) {
                    return true;
                } else {
                    brc.followConnection(true);
                    /*
                     * E.g. 403 with plaintext response: File is not available under this URL. Add file link again to your list and start
                     * download.
                     */
                    throw new IOException();
                }
            } catch (final Throwable e) {
                logger.log(e);
                try {
                    dl.getConnection().disconnect();
                } catch (Throwable ignore) {
                }
                return false;
            }
        }
    }

    @SuppressWarnings({ "unchecked", "deprecation" })
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> resp_login = login(account);
        final Map<String, Object> resp_login_data = (Map<String, Object>) resp_login.get("data");
        final Map<String, Object> trafficStandard = (Map<String, Object>) JavaScriptEngineFactory.walkJson(resp_login_data, "traffic/standard");
        final Map<String, Object> trafficDaily = (Map<String, Object>) JavaScriptEngineFactory.walkJson(resp_login_data, "traffic/daily");
        String accountStatustext = null;
        long trafficLeftTotal = 0;
        long expireTimestampStandard = JavaScriptEngineFactory.toLong(trafficStandard.get("expireTstamp"), -1);
        long expireTimestampDaily = JavaScriptEngineFactory.toLong(trafficDaily.get("expireTstamp"), -1);
        long expireTimestamp;
        /* Accounts can also have negative traffic (=no traffic left) */
        long trafficLeftStandard = JavaScriptEngineFactory.toLong(trafficStandard.get("left"), 0);
        long traffic_left_daily = JavaScriptEngineFactory.toLong(trafficDaily.get("left"), 0);
        final String accounttype = (String) resp_login_data.get("accountType");
        if (traffic_left_daily > 0 && trafficLeftStandard > 0) {
            accountStatustext = " account with daily & standard traffic";
            trafficLeftTotal = traffic_left_daily + trafficLeftStandard;
        } else if (traffic_left_daily > 0) {
            accountStatustext = " account with daily traffic";
            trafficLeftTotal = traffic_left_daily;
        } else if (trafficLeftStandard > 0) {
            accountStatustext = " account with standard traffic";
            trafficLeftTotal = trafficLeftStandard;
        } else {
            accountStatustext = " account without traffic";
            trafficLeftTotal = 0;
        }
        if (expireTimestampStandard > expireTimestampDaily) {
            expireTimestamp = expireTimestampStandard;
        } else {
            expireTimestamp = expireTimestampDaily;
        }
        expireTimestamp = expireTimestamp * 1000;
        final AccountInfo ai = new AccountInfo();
        ai.setTrafficRefill(false);
        if ("premium".equals(accounttype) && expireTimestamp > System.currentTimeMillis()) {
            account.setType(AccountType.PREMIUM);
            accountStatustext = "Premium" + accountStatustext;
        } else {
            account.setType(AccountType.FREE);
            accountStatustext = "Free" + accountStatustext;
        }
        /* 2021-09-16: Free- and premium accounts can have expire-dates(?) */
        if (expireTimestamp > System.currentTimeMillis()) {
            ai.setValidUntil(expireTimestamp, this.br);
        }
        ai.setStatus(accountStatustext);
        ai.setTrafficLeft(trafficLeftTotal);
        br.getPage(API_SERVER + "/supportedHosts");
        final Map<String, Object> resp_supportedHosts = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> domaininfos = (List<Map<String, Object>>) resp_supportedHosts.get("data");
        /**
         * Explanation of their status-types: Healthy = working, Fragile = may work or not - if not will be fixed within the next 72 hours
         * (support also said it means that they currently have no accounts for this host), Limited = broken, will be fixed tomorrow, dead =
         * site offline or their plugin is completely broken, Limited = There are special daily limits for a host but it should work (even
         * though it is marked RED on the site)
         */
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        final HashSet<String> cloudOnlyDomains = new HashSet<String>();
        for (final Map<String, Object> domaininfo : domaininfos) {
            final boolean canResume = ((Boolean) domaininfo.get("resumable")).booleanValue();
            final String domain = (String) domaininfo.get("host");
            if (Integer.parseInt(domaininfo.get("serverOnly").toString()) == 1) {
                cloudOnlyDomains.add(domain);
            }
            final MultiHostHost mhost = new MultiHostHost(domain);
            if (Integer.parseInt(domaininfo.get("tmpTurnedOff").toString()) == 0) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            mhost.setMaxChunks(((Number) domaininfo.get("maxChunks")).intValue());
            mhost.setResumable(canResume);
            mhost.setMaxDownloads(((Number) domaininfo.get("maxDownloads")).intValue());
            supportedhosts.add(mhost);
        }
        /* Set list of supported hosts */
        final List<MultiHostHost> mhosts = ai.setMultiHostSupportV2(this, supportedhosts);
        synchronized (hostMaxchunksMap) {
            /* Set additional individual host special properties lists. */
            cloudOnlyHosts.clear();
            hostMaxchunksMap.clear();
            hostMaxdlsMap.clear();
            hostResumeMap.clear();
            for (final MultiHostHost mhost : mhosts) {
                boolean isCloudOnlyHost = false;
                for (final String domain : mhost.getDomains()) {
                    hostMaxchunksMap.put(domain, this.correctChunks(mhost.getMaxChunks()));
                    hostMaxdlsMap.put(domain, this.correctMaxdls(mhost.getMaxDownloads()));
                    hostResumeMap.put(domain, mhost.isResumable());
                    if (!isCloudOnlyHost && cloudOnlyDomains.contains(domain)) {
                        isCloudOnlyHost = true;
                    }
                }
                if (isCloudOnlyHost) {
                    if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                        mhost.setStatusText("Cloud only");
                    }
                    for (final String domain : mhost.getDomains()) {
                        cloudOnlyHosts.add(domain);
                    }
                }
            }
        }
        if (this.getPluginConfig().getBooleanProperty(CLEAR_DOWNLOAD_HISTORY, default_CLEAR_DOWNLOAD_HISTORY) && getLast_deleted_complete_download_history_time_ago(account) >= DELETE_COMPLETE_DOWNLOAD_HISTORY_INTERVAL) {
            /*
             * Go in here if user wants to have it's history deleted && last deletion was before DELETE_COMPLETE_DOWNLOAD_HISTORY_INTERVAL
             * or never executed (0).
             */
            this.deleteCompleteDownloadHistory(account, PROPERTY_DOWNLOADTYPE_instant);
        }
        return ai;
    }

    private UrlQuery getLoginPassPostData(final Account account) throws Exception {
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("login", account.getUser());
        query.appendEncoded("pass", account.getPass());
        return query;
    }

    /**
     * Log into users' account and set login cookie
     *
     * @throws Exception
     */
    private Map<String, Object> login(final Account account) throws Exception {
        br.postPage(API_SERVER + "/login", getLoginPassPostData(account));
        return this.handleErrors(br, account, null);
    }

    /**
     * Deletes the complete download list / history.
     **/
    private void deleteCompleteDownloadHistory(final Account account, final String downloadtype) throws Exception {
        /* This moves the downloaded files/entries to the download history. */
        br.postPage(API_SERVER + "/clearFileList", getLoginPassPostData(account));
        boolean success1 = false;
        boolean success2 = false;
        try {
            this.handleErrors(br, account, null);
            success1 = true;
        } catch (final Throwable ignore) {
            logger.log(ignore);
            logger.warning("Failed to delete file list");
        }
        /* This deletes the download history. */
        br.postPage(API_SERVER + "/clearHistory", getLoginPassPostData(account));
        try {
            this.handleErrors(br, account, null);
            success2 = true;
        } catch (final Throwable ignore) {
            logger.warning("Failed to delete download history");
        }
        if (success1 && success2) {
            account.setProperty(PROPERTY_ACCOUNT_last_time_deleted_history, System.currentTimeMillis());
        } else {
            logger.info("Parts of clearing download history failed -> Do not set timestamp so this action will be retried again on next account check");
        }
    }

    /* Returns the time difference between now and the last time the complete download history has been deleted. */
    private long getLast_deleted_complete_download_history_time_ago(final Account account) {
        return System.currentTimeMillis() - account.getLongProperty(PROPERTY_ACCOUNT_last_time_deleted_history, 0);
    }

    private Map<String, Object> handleErrors(final Browser br, final Account account, final DownloadLink link) throws Exception {
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid API response", 60 * 1000l);
            } else {
                throw new AccountUnavailableException("Invalid API response", 60 * 1000);
            }
        }
        if (Boolean.TRUE.equals(entries.get("success"))) {
            /* No error */
            return entries;
        }
        final String error = (String) entries.get("error");
        final Object errorDescrO = entries.get("errorDescr");
        String errorDescription = null; // more detailed errormessage
        if (errorDescrO instanceof List) {
            final List<String> errorDescriptionList = (List<String>) errorDescrO;
            if (errorDescriptionList.isEmpty()) {
                logger.warning("WTF empty errorDescriptionList");
            } else {
                if (errorDescriptionList.size() > 1) {
                    /* Multiple error-strings? We cannot yet deal with that... */
                    logger.warning("WTF errorDescriptionList contains more than one item: " + errorDescriptionList.size());
                }
                errorDescription = errorDescriptionList.get(0);
            }
        }
        if (error.equalsIgnoreCase("loginFailed") || error.equalsIgnoreCase("invalidLoginOrPassword")) {
            throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
        } else if (error.equalsIgnoreCase("fileNotFound")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (error.equalsIgnoreCase("invalidAccount")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Cannot download this file with this account", 3 * 60 * 1000l);
        } else if (error.equalsIgnoreCase("invalidUrl")) {
            if (StringUtils.containsIgnoreCase(errorDescription, "File size cannot be lower than")) {
                /* 2017-05-04 */
                /* E.g. {"success":false,"error":"invalidUrl","errorDescr":["File size cannot be lower than 8 KB",""]} */
                /* --> Skip this URL but do not disable the complete host! */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Cannot download this file with this account", 3 * 60 * 1000l);
            } else if (StringUtils.containsIgnoreCase(errorDescription, "Maximum supported file size in direct download mode is")) {
                /* 2021-09-16: Typically "Maximum supported file size in direct download mode is 15 GB" */
                if (link.hasProperty(PROPERTY_ENFORCE_CLOUD_DOWNLOAD)) {
                    /* This should never happen! */
                    mhm.putError(account, link, 5 * 60 * 1000l, errorDescription);
                } else {
                    /* 2021-09-16: Workaround for API design flaw. */
                    link.setProperty(PROPERTY_ENFORCE_CLOUD_DOWNLOAD, true);
                    throw new PluginException(LinkStatus.ERROR_RETRY, "Retry with cloud download instead of direct download");
                }
            } else {
                mhm.putError(account, link, 3 * 60 * 1000l, "Host unsupported ?!");
            }
        } else if (error.equalsIgnoreCase("tooManyConcurrentDownloads")) {
            throw new AccountUnavailableException("Too many concurrent downloads with this account", 30 * 1000l);
        } else if (error.equalsIgnoreCase("notPossibleToDownload")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Cannot download this file with this account at the moment", 3 * 60 * 1000l);
        } else if (error.equalsIgnoreCase("emptyUrl") || error.equalsIgnoreCase("tooLongUrl")) {
            /* This one should never happen! */
            mhm.handleErrorGeneric(account, link, "Empty or too long URL", 10);
        } else {
            logger.info("Unknown error happened: " + error);
            if (link == null) {
                throw new AccountUnavailableException(error, 5 * 60 * 1000l);
            } else {
                mhm.handleErrorGeneric(account, link, error, 50);
            }
        }
        /* This code should never be reached */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    /** Corrects input so that it fits what we use in our plugins. */
    private int correctChunks(int maxchunks) {
        if (maxchunks <= 1) {
            return 1;
        } else {
            return -maxchunks;
        }
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
            final String currentHost = this.getDownloadLink().getHost();
            int was = maxPrem.get();
            maxPrem.set(Math.min(Math.max(1, maxPrem.addAndGet(num)), this.getMaxSimultanPremiumDownloadNum()));
            logger.info("maxPrem was = " + was + " && maxPrem now = " + maxPrem.get());
            AtomicInteger currentRunningDls = new AtomicInteger(0);
            if (hostRunningDlsNumMap.containsKey(currentHost)) {
                currentRunningDls = hostRunningDlsNumMap.get(currentHost);
            }
            currentRunningDls.set(currentRunningDls.get() + num);
            hostRunningDlsNumMap.put(currentHost, currentRunningDls);
        }
    }

    private final boolean default_CLEAR_DOWNLOAD_HISTORY = false;

    public void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), CLEAR_DOWNLOAD_HISTORY, "Delete download history every 60 minutes?").setDefaultValue(default_CLEAR_DOWNLOAD_HISTORY));
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 10;
    }

    @Override
    public int getMaxSimultanDownload(final DownloadLink link, final Account account) {
        return maxPrem.get();
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        if (link == null) {
            return;
        }
        /* Allow generation of new directurl on reset. */
        link.removeProperty(PROPERTY_DLLINK_GENERATED_TIMESTAMP);
        /* Remove this special flag needed for a workaround. */
        link.removeProperty(PROPERTY_ENFORCE_CLOUD_DOWNLOAD);
    }
}