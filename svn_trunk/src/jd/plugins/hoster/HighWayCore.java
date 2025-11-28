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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.downloads.columns.ETAColumn;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.ConditionalSkipReasonException;
import org.jdownloader.plugins.PluginTaskID;
import org.jdownloader.plugins.WaitForAccountTrafficSkipReason;
import org.jdownloader.plugins.WaitingSkipReason;
import org.jdownloader.plugins.WaitingSkipReason.CAUSE;
import org.jdownloader.plugins.components.usenet.UsenetAccountConfigInterface;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginProgress;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 51885 $", interfaceVersion = 1, names = {}, urls = {})
public abstract class HighWayCore extends UseNet {
    private static final String                            PATTERN_TV                             = "(?i)https?://[^/]+/onlinetv\\.php\\?id=.+";
    private static final int                               STATUSCODE_PASSWORD_NEEDED_OR_WRONG    = 13;
    /* Contains <host><Boolean resume possible|impossible> */
    private static Map<String, Map<String, Boolean>>       hostResumeMap                          = new HashMap<String, Map<String, Boolean>>();
    /* Contains <host><number of max possible chunks per download> */
    private static Map<String, Map<String, Integer>>       hostMaxchunksMap                       = new HashMap<String, Map<String, Integer>>();
    /* Contains <host><number of max possible simultan downloads> */
    private static Map<String, Map<String, Integer>>       hostMaxdlsMap                          = new HashMap<String, Map<String, Integer>>();
    /* Contains <host><number of currently running simultaneous downloads> */
    private static Map<String, Map<String, AtomicInteger>> hostRunningDlsNumMap                   = new HashMap<String, Map<String, AtomicInteger>>();
    private static Map<String, Object>                     mapLockMap                             = new HashMap<String, Object>();
    private final int                                      defaultMAXCHUNKS                       = -4;
    private final boolean                                  defaultRESUME                          = true;
    private final String                                   PROPERTY_ACCOUNT_MAXCHUNKS             = "maxchunks";
    private final String                                   PROPERTY_ACCOUNT_RESUME                = "resume";
    private final String                                   PROPERTY_ACCOUNT_MAX_DOWNLOADS_ACCOUNT = "max_downloads_account";
    private final String                                   PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET  = "max_downloads_usenet";
    private final String                                   PROPERTY_ACCOUNT_USENET_USERNAME       = "usenetU";
    private final String                                   PROPERTY_ACCOUNT_USENET_PASSWORD       = "usenetP";

    public static interface HighWayMeConfigInterface extends UsenetAccountConfigInterface {
    };

    protected abstract MultiHosterManagement getMultiHosterManagement();

    public HighWayCore(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        HighWayCore.prepBRHighway(br);
        return br;
    }

    public static Browser prepBRHighway(final Browser br) {
        br.setCookiesExclusive(true);
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "JDownloader");
        /* API returns error messages in different languages depending on this header. */
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_LANGUAGE, System.getProperty("user.language"));
        br.setCustomCharset("utf-8");
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(503);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        if (this.useApikeyLogin()) {
            return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.USENET, LazyPlugin.FEATURE.API_KEY_LOGIN };
        } else {
            return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.USENET };
        }
    }

    /** Returns true if an account is required to download the given item. */
    private boolean requiresAccount(final DownloadLink link) {
        if (link.getPluginPatternMatcher() != null && link.getPluginPatternMatcher() != null && link.getPluginPatternMatcher().matches(PATTERN_TV)) {
            /* Account required. */
            return true;
        } else {
            /* No account required. */
            return false;
        }
    }

    /**
     * API docs: https://high-way.me/threads/highway-api.201/ </br>
     * According to admin we can 'hammer' the API every 60 seconds
     */
    protected abstract String getAPIBase();

    protected abstract String getWebsiteBase();

    /** If enabled, apikey is used for login instead of username:password. */
    protected abstract boolean useApikeyLogin();

    protected Object getMapLock() {
        synchronized (mapLockMap) {
            Object ret = mapLockMap.get(getHost());
            if (ret == null) {
                ret = new Object();
                mapLockMap.put(getHost(), ret);
            }
            return ret;
        }
    }

    protected <T> Map<String, T> getMap(final Map<String, Map<String, T>> map) {
        synchronized (map) {
            Object ret = map.get(getHost());
            if (ret == null) {
                ret = new HashMap<String, Object>();
                map.put(getHost(), (Map<String, T>) ret);
            }
            return (Map<String, T>) ret;
        }
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
        if (link == null || link.getPluginPatternMatcher() == null) {
            return null;
        } else if (link.getPluginPatternMatcher().matches(PATTERN_TV)) {
            try {
                return UrlQuery.parse(link.getPluginPatternMatcher()).get("id");
            } catch (final MalformedURLException e) {
                /* This should never happen! */
                e.printStackTrace();
                return null;
            }
        } else {
            /* Assume that we have a direct downloadable URL. */
            try {
                return new URL(link.getPluginPatternMatcher()).getPath();
            } catch (final MalformedURLException e) {
                e.printStackTrace();
                return null;
            }
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        return this.requestFileInformation(link, account);
    }

    protected AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        if (isUsenetLink(link)) {
            return super.requestFileInformation(link);
        } else if (link.getPluginPatternMatcher().matches(PATTERN_TV)) {
            if (!link.isNameSet()) {
                link.setName(this.getFID(link) + ".mp4");
            }
            if (account == null) {
                return AvailableStatus.UNCHECKABLE;
            }
            this.login(account, false);
            final String json_url = link.getPluginPatternMatcher().replaceAll("(?i)stream=(?:0|1)", "") + "&json=1";
            br.getPage(json_url);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Number code = (Number) entries.get("code");
            if (code != null && code.intValue() != 0) {
                /* E.g. {"code":"8","error":"ID nicht bekannt"} */
                final String errormessage = (String) entries.get("error");
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, errormessage);
            }
            this.checkErrors(br, link, account);
            link.setFinalFileName(entries.get("name").toString());
            final String filesizeBytesStr = entries.get("size").toString();
            if (filesizeBytesStr != null && filesizeBytesStr.matches("\\d+")) {
                link.setVerifiedFileSize(Long.parseLong(filesizeBytesStr));
            }
        } else {
            /* Direct-URL download. */
            final String filenameFromURL = Plugin.getFileNameFromURL(new URL(link.getPluginPatternMatcher()));
            final String fallbackFilename;
            if (filenameFromURL != null) {
                fallbackFilename = filenameFromURL;
            } else {
                fallbackFilename = this.getFID(link);
            }
            if (!link.isNameSet() && fallbackFilename != null) {
                /* Set fallback name */
                link.setName(Encoding.htmlDecode(fallbackFilename).trim());
            }
            if (account == null) {
                /* Some items might be checkable without account but we require an account for all items just in case. */
                logger.info("Cannot check this link without account");
                return AvailableStatus.UNCHECKABLE;
            }
            this.login(account, false);
            checkDirecturl(link);
        }
        return AvailableStatus.TRUE;
    }

    private void checkDirecturl(final DownloadLink link) throws IOException, PluginException {
        URLConnectionAdapter con = null;
        try {
            con = br.openHeadConnection(link.getPluginPatternMatcher());
            handleSelfhostedFileConnectionErrors(br, con);
            final String connectionFilename = getFileNameFromConnection(con);
            if (!StringUtils.isEmpty(connectionFilename)) {
                link.setFinalFileName(connectionFilename);
            }
            if (con.getCompleteContentLength() != -1) {
                if (con.isContentDecoded()) {
                    link.setDownloadSize(con.getCompleteContentLength());
                } else {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
            }
        } finally {
            try {
                con.disconnect();
            } catch (Throwable e) {
            }
        }
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (!requiresAccount(link)) {
            return true;
        }
        /* Make sure that we do not start more than the allowed number of max simultaneous downloads for the current host. */
        synchronized (getMapLock()) {
            final Map<String, AtomicInteger> hostRunningDlsNumMap = getMap(HighWayCore.hostRunningDlsNumMap);
            final Map<String, Integer> hostMaxdlsMap = getMap(HighWayCore.hostMaxdlsMap);
            if (hostRunningDlsNumMap.containsKey(link.getHost()) && hostMaxdlsMap.containsKey(link.getHost())) {
                final int maxDlsForCurrentHost = hostMaxdlsMap.get(link.getHost());
                final AtomicInteger currentRunningDlsForCurrentHost = hostRunningDlsNumMap.get(link.getHost());
                if (currentRunningDlsForCurrentHost.get() >= maxDlsForCurrentHost) {
                    /*
                     * Max downloads for specific host for this MOCH reached --> Avoid irritating/wrong 'Account missing' errormessage for
                     * this case - wait and retry!
                     */
                    final String msg;
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        msg = "Download von diesem Hoster aktuell nicht möglich: Zu viele gleichzeitige Downloads (max " + maxDlsForCurrentHost + ")";
                    } else {
                        msg = "Downloads of this host are currently not possible: Too many simultaneous downloads (max " + maxDlsForCurrentHost + ")";
                    }
                    throw new ConditionalSkipReasonException(new WaitingSkipReason(CAUSE.HOST_TEMP_UNAVAILABLE, 2 * 1000, msg));
                }
            }
        }
        return super.canHandle(link, account);
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        this.handleSelfhostedFileDownload(link, null);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (isUsenetLink(link)) {
            super.handleMultiHost(link, account);
            return;
        } else {
            handleSelfhostedFileDownload(link, account);
        }
    }

    private void handleSelfhostedFileDownload(final DownloadLink link, final Account account) throws Exception {
        if (this.requiresAccount(link) && account == null) {
            /* This should never happen as canHandle already returns false when account is required but not available. */
            throw new AccountRequiredException();
        }
        this.requestFileInformation(link, account);
        if (account != null) {
            this.login(account, false);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, link.getPluginPatternMatcher(), this.isResumeable(link, account), this.getMaxChunks(link, account));
        handleSelfhostedFileConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    protected void handleSelfhostedFileConnectionErrors(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            if (con.getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                this.throwFinalConnectionException(br, con);
            }
        }
    }

    /**
     * Block download slots for files which have to be downloaded to the multihost first? If this returns true, new downloads will not be
     * allowed as long as cacheDLChecker is running/waiting.
     */
    protected boolean blockDownloadSlotsForCloudDownloads(final Account account) {
        return true;
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        /*
         * When JD is started the first time and the user starts downloads right away, a full login might not yet have happened but it is
         * needed to get the individual host limits.
         */
        synchronized (account) {
            synchronized (getMapLock()) {
                if (getMap(HighWayCore.hostMaxchunksMap).isEmpty() || getMap(HighWayCore.hostMaxdlsMap).isEmpty()) {
                    logger.info("Performing full login to set individual host limits");
                    this.fetchAccountInfo(account);
                }
            }
        }
        if (isUsenetLink(link)) {
            super.handleMultiHost(link, account);
            return;
        }
        boolean resume = this.isResumeable(link, account);
        final int maxChunks = this.getMaxChunks(link, account);
        final String directlinkproperty = this.getHost() + "directlink";
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            this.login(account, false);
            /* Request creation of downloadlink */
            Map<String, Object> entries = null;
            String passCode = link.getDownloadPassword();
            int counter = 0;
            int statuscode;
            final Boolean storedInformationRequiresSpecialWorkaround = (Boolean) link.getProperty(IsraCloud.PROPERTY_REQUIRES_SPECIAL_WORKAROUND);
            do {
                if (counter > 0) {
                    passCode = getUserInput("Password?", link);
                }
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("link", link.getDefaultPlugin().buildExternalDownloadURL(link, this));
                if (counter == 0 && storedInformationRequiresSpecialWorkaround != null) {
                    /* Provide special workaround information via pass field that is only used by one plugin. */
                    query.appendEncoded("pass", "special_workaround_state_" + storedInformationRequiresSpecialWorkaround);
                } else if (passCode != null) {
                    query.appendEncoded("pass", passCode);
                }
                br.getPage(getWebsiteBase() + "load.php?json=1&" + query.toString());
                entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                statuscode = ((Number) entries.get("code")).intValue();
                if (statuscode != STATUSCODE_PASSWORD_NEEDED_OR_WRONG) {
                    break;
                } else if (counter >= 2) {
                    logger.info("Password loop exceeded max retries");
                    break;
                } else {
                    if (passCode != null) {
                        logger.info("Wrong password: " + passCode);
                    } else {
                        logger.info("Password required");
                    }
                    link.setPasswordProtected(true);
                    counter++;
                }
            } while (true);
            if (statuscode == STATUSCODE_PASSWORD_NEEDED_OR_WRONG) {
                link.setDownloadPassword(null);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
            } else if (passCode != null) {
                /* Password has been entered correctly or previously given password was correct --> Save it */
                link.setDownloadPassword(passCode);
            }
            this.checkErrors(entries, link, account);
            final Object infoMsgO = entries.get("info");
            if (infoMsgO instanceof String) {
                /* Low traffic warning message: Usually something like "Less than 10% traffic remaining" */
                displayBubbleNotification((String) infoMsgO, (String) infoMsgO);
            }
            entries = this.cacheDLChecker(entries, this.br, link, account);
            dllink = entries.get("download").toString();
            /* Validate URL */
            dllink = new URL(dllink).toExternalForm();
            String hash = (String) entries.get("hash");
            if (hash != null && !StringUtils.equalsIgnoreCase("hash", "null")) {
                if (hash.matches("(?i)md5:[a-f0-9]{32}")) {
                    hash = hash.substring(hash.lastIndexOf(":") + 1);
                    logger.info("Set md5 hash given by multihost: " + hash);
                    link.setMD5Hash(hash);
                } else if (hash.matches("(?i)sha1:[a-f0-9]{40}")) {
                    hash = hash.substring(hash.lastIndexOf(":") + 1);
                    logger.info("Set sha1 hash given by multihost: " + hash);
                    link.setSha1Hash(hash);
                } else if (hash.matches("(?i)sha265:[a-f0-9]{40}")) {
                    hash = hash.substring(hash.lastIndexOf(":") + 1);
                    logger.info("Set sha265 hash given by multihost: " + hash);
                    link.setSha256Hash(hash);
                } else {
                    logger.info("Unsupported file-hash string: " + hash);
                }
            }
            link.setProperty(directlinkproperty, dllink);
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, resume, maxChunks);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                this.checkErrors(this.br, link, account);
                getMultiHosterManagement().handleErrorGeneric(account, null, "unknowndlerror", 1, 3 * 60 * 1000l);
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        controlSlot(+1);
        try {
            dl.startDownload();
        } finally {
            controlSlot(-1);
        }
    }

    private Map<String, Object> cacheDLChecker(final Map<String, Object> loadDotPHPJson, final Browser br, final DownloadLink link, final Account account) throws Exception {
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
                // return status;
                return "Waiting for cache download...";
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
                final long speed = (current * 10000) / currentTimeDifference;
                if (speed == 0) {
                    return;
                }
                long eta = ((total - current) * 10000) / speed;
                this.setETA(eta);
            }
        };
        waitProgress.setIcon(new AbstractIcon(IconKey.ICON_WAIT, 16));
        waitProgress.setProgressSource(this);
        final String cachePollingURL = (String) loadDotPHPJson.get("cache");
        if (StringUtils.isEmpty(cachePollingURL)) {
            logger.info("Stepping out of cache handling because: No polling URL available -> No cache handling needed");
            return loadDotPHPJson;
        }
        Map<String, Object> entries = null;
        final int maxWaitSeconds = 90;
        int secondsWaited = 0;
        int retryInSecondsThisRound = 30;
        String textForJD = null;
        try {
            do {
                /**
                 * cacheStatus possible values and what they mean: </br>
                 * d = download </br>
                 * w = wait (retry) </br>
                 * q = in queue </br>
                 * qn = Download has been added to queue </br>
                 * i = direct download without cache </br>
                 * s = Cached download is ready for downloading
                 */
                br.getPage(cachePollingURL);
                entries = this.checkErrors(br, link, account);
                final Object cacheStatusO = entries.get("cacheStatus");
                if (cacheStatusO != null && cacheStatusO.toString().matches("(?i)i|s")) {
                    logger.info("Stepping out of cache handling because: Detected valid cacheStatus");
                    return entries;
                }
                retryInSecondsThisRound = ((Number) entries.get("retry_in_seconds")).intValue();
                textForJD = (String) entries.get("for_jd");
                if (!blockDownloadSlotsForCloudDownloads(account)) {
                    /**
                     * Throw exception right away so other download candidates will be tried. </br>
                     * This may speed up downloads significantly for some users.
                     */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, textForJD, retryInSecondsThisRound * 1000l);
                }
                /* Wait and re-check */
                final int retryInSeconds = Math.min(retryInSecondsThisRound, maxWaitSeconds - secondsWaited);
                this.sleep(retryInSeconds * 1000l, link, textForJD);
                secondsWaited += retryInSeconds;
                final int currentProgress = ((Number) entries.get("percentage_Complete")).intValue();
                link.addPluginProgress(waitProgress);
                waitProgress.updateValues(currentProgress, 100);
                logger.info("Cache handling: Seconds waited " + secondsWaited + "/" + maxWaitSeconds);
                if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    throw new InterruptedException();
                } else if (secondsWaited >= maxWaitSeconds) {
                    logger.info("Stopping because: Exceeded max wait seconds " + maxWaitSeconds);
                    break;
                }
                /* Continue */
            } while (true);
        } finally {
            link.removePluginProgress(waitProgress);
        }
        logger.info("Cache handling: Timeout");
        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, textForJD, retryInSecondsThisRound * 1000l);
    }

    @SuppressWarnings({ "unchecked" })
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        synchronized (account) {
            this.login(account, true);
            br.getPage(this.getAPIBase() + "?hoster&user");
            final AccountInfo ai = new AccountInfo();
            final Map<String, Object> entries = this.checkErrors(this.br, null, account);
            final Map<String, Object> accountInfo = (Map<String, Object>) entries.get("user");
            final int accountResume = ((Number) accountInfo.get("resume")).intValue();
            final long premiumUntil = ((Number) accountInfo.get("premium_bis")).longValue();
            final long premiumTraffic = ((Number) accountInfo.get("premium_traffic")).longValue();
            final long trafficLeftToday = ((Number) accountInfo.get("traffic_remain_today")).longValue();
            ai.setTrafficRefill(false);
            ai.setTrafficLeft(trafficLeftToday);
            /* Set account type and account information */
            if (Boolean.TRUE.equals(entries.get("premium"))) {
                /* Premium account */
                account.setType(AccountType.PREMIUM);
                ai.setTrafficLeft(premiumTraffic);
                ai.setTrafficMax(((Number) accountInfo.get("premium_max")).longValue());
                ai.setValidUntil(premiumUntil * 1000, br);
            } else {
                /* Free account */
                account.setType(AccountType.FREE);
                final long free_traffic_max_daily = ((Number) accountInfo.get("free_traffic")).longValue();
                final long free_traffic_left = ((Number) accountInfo.get("remain_free_traffic")).longValue();
                ai.setTrafficMax(free_traffic_max_daily);
                ai.setTrafficLeft(free_traffic_left);
                /* Only free accounts have a daily download limit -> Display that in GUI. */
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    ai.setStatus(StringUtils.valueOfOrNull(accountInfo.get("type")) + " | Heute übrig: " + SIZEUNIT.formatValue((SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue(), trafficLeftToday));
                } else {
                    ai.setStatus(StringUtils.valueOfOrNull(accountInfo.get("type")) + " | Remaining today: " + SIZEUNIT.formatValue((SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue(), trafficLeftToday));
                }
            }
            account.setConcurrentUsePossible(true);
            /* Set supported hosts, host specific limits and account limits. */
            account.setProperty(PROPERTY_ACCOUNT_MAXCHUNKS, accountInfo.get("max_chunks"));
            account.setProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_ACCOUNT, accountInfo.get("max_connection"));
            if (accountResume == 1) {
                account.setProperty(PROPERTY_ACCOUNT_RESUME, true);
            } else {
                account.setProperty(PROPERTY_ACCOUNT_RESUME, false);
            }
            final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
            /* List of items which should not be displayed in JD GUI. */
            final HashSet<String> ignoreItems = new HashSet<String>();
            ignoreItems.add("beta");
            ignoreItems.add("TV Recorder");
            ignoreItems.add("WebDav");
            boolean supportsUsenet = false;
            /* Available hosts are returned by API depending on users' account type e.g. free users have much less supported hosts. */
            final List<Map<String, Object>> array_hoster = (List<Map<String, Object>>) entries.get("hoster");
            for (final Map<String, Object> hostermap : array_hoster) {
                final int activeStatus = ((Number) hostermap.get("active")).intValue();
                final String domain = hostermap.get("name").toString();
                if (ignoreItems.contains(domain)) {
                    /* Skip hardcoded list of items which should never be displayed in GUI. */
                    continue;
                }
                final MultiHostHost mhost = new MultiHostHost(domain);
                if (activeStatus != 1) {
                    mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                }
                mhost.setTrafficCalculationFactorPercent(((Number) hostermap.get("berechnung")).shortValue());
                mhost.setMaxChunks(((Number) hostermap.get("chunks")).intValue());
                mhost.setMaxDownloads(((Number) hostermap.get("downloads")).intValue());
                mhost.setResumable(((Number) hostermap.get("resume")).intValue() == 1 ? true : false);
                final long maxTrafficDaily = ((Number) hostermap.get("maxTrafficDaily")).longValue();
                // final long usedTrafficDaily = ((Number) hostermap.get("usedTrafficDaily")).longValue();
                final long remainingTrafficDaily = ((Number) hostermap.get("remainingTrafficDaily")).longValue();
                /* -1 = no limit */
                if (maxTrafficDaily != -1 && remainingTrafficDaily != -1) {
                    mhost.setTrafficMax(maxTrafficDaily);
                    mhost.setTrafficLeft(remainingTrafficDaily);
                }
                supportedhosts.add(mhost);
                /* Check if usenet is supported */
                if (!supportsUsenet && domain.equalsIgnoreCase("usenet")) {
                    supportsUsenet = true;
                }
            }
            /* Get- and store usenet logindata. These can differ from the logindata the user has added but may as well be equal to those. */
            final Map<String, Object> usenetLogins;
            if (supportsUsenet && (usenetLogins = (Map<String, Object>) accountInfo.get("usenet")) != null && !usenetLogins.isEmpty()) {
                logger.info("Usenet is supported");
                final String usenetUsername = usenetLogins.get("username").toString();
                if (this.useApikeyLogin()) {
                    /* Try to set unique username as user could enter anything in the username field in this case */
                    if (!StringUtils.isEmpty(usenetUsername)) {
                        account.setUser(usenetUsername);
                    }
                }
                account.setProperty(PROPERTY_ACCOUNT_USENET_USERNAME, usenetUsername);
                account.setProperty(PROPERTY_ACCOUNT_USENET_PASSWORD, usenetLogins.get("pass"));
                account.setProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET, accountInfo.get("usenet_connection"));
            } else {
                logger.info("Usenet is NOT supported");
                account.removeProperty(PROPERTY_ACCOUNT_USENET_USERNAME);
                account.removeProperty(PROPERTY_ACCOUNT_USENET_PASSWORD);
                account.removeProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET);
            }
            /* Assign items so they can be matched against our internal plugin domains. */
            final List<MultiHostHost> mhosts = ai.setMultiHostSupportV2(this, supportedhosts);
            synchronized (getMapLock()) {
                final Map<String, Integer> hostMaxchunksMap = getMap(HighWayCore.hostMaxchunksMap);
                final Map<String, Integer> hostMaxdlsMap = getMap(HighWayCore.hostMaxdlsMap);
                final Map<String, Boolean> hostResumeMap = getMap(HighWayCore.hostResumeMap);
                hostMaxchunksMap.clear();
                hostMaxdlsMap.clear();
                hostResumeMap.clear();
                /* Set some special information on the now found ral/internal plugin domains. */
                for (final MultiHostHost mhost : mhosts) {
                    for (final String domain : mhost.getDomains()) {
                        hostMaxchunksMap.put(domain, mhost.getMaxChunks());
                        hostMaxdlsMap.put(domain, mhost.getMaxDownloads());
                        hostResumeMap.put(domain, mhost.isResumable());
                    }
                }
            }
            return ai;
        }
    }

    @Override
    protected String getUseNetUsername(final Account account) {
        synchronized (account) {
            return account.getStringProperty(PROPERTY_ACCOUNT_USENET_USERNAME);
        }
    }

    @Override
    protected String getUseNetPassword(final Account account) {
        synchronized (account) {
            return account.getStringProperty(PROPERTY_ACCOUNT_USENET_PASSWORD);
        }
    }

    /**
     * Login without errorhandling
     *
     * @return true = cookies validated </br>
     *         false = cookies set but not validated
     *
     * @throws PluginException
     * @throws InterruptedException
     */
    public void login(final Account account, final boolean validateCookies) throws IOException, PluginException, InterruptedException {
        synchronized (account) {
            if (useApikeyLogin()) {
                if (!this.looksLikeValidAPIKey(account.getPass())) {
                    throw new AccountInvalidException("Invalid API key format");
                }
            }
            br.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Basic " + Encoding.Base64Encode(account.getUser() + ":" + account.getPass()));
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                br.setCookies(cookies);
                if (!validateCookies) {
                    /* Do not validate cookies */
                    return;
                }
                logger.info("Checking cookies");
                br.getPage(this.getAPIBase() + "?logincheck");
                /* Don't check for errors here as a failed login can trigger error dialogs which we don't want here! */
                // this.checkErrors(this.br, account);
                try {
                    this.checkErrors(br, null, account);
                    /* No exception --> Success */
                    logger.info("Cookie login successful");
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return;
                } catch (final PluginException ignore) {
                    logger.log(ignore);
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                }
            }
            logger.info("Performing full login");
            final UrlQuery query = new UrlQuery();
            if (this.useApikeyLogin()) {
                query.appendEncoded("apikey", account.getPass());
            } else {
                query.appendEncoded("user", account.getUser());
                query.appendEncoded("pass", account.getPass());
            }
            br.postPage(getAPIBase() + "?login", query);
            this.checkErrors(this.br, null, account);
            /* No Exception --> Assume that login was successful */
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    protected abstract void exceptionAccountInvalid(final Account account) throws PluginException;

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        synchronized (getMapLock()) {
            final String downloadLinkHost = link.getHost();
            final String hostFromLink;
            final Map<String, Boolean> hostResumeMap = getMap(HighWayCore.hostResumeMap);
            if (hostResumeMap.containsKey(downloadLinkHost)) {
                return hostResumeMap.get(downloadLinkHost);
            } else if (hostResumeMap.containsKey(hostFromLink = Browser.getHost(link.getPluginPatternMatcher(), true))) {
                /* E.g. selfhosted items download */
                return hostResumeMap.get(hostFromLink);
            }
        }
        return this.defaultRESUME;
    }

    private int getMaxChunks(final DownloadLink link, final Account account) {
        Number maxChunks = null;
        synchronized (getMapLock()) {
            final Map<String, Integer> hostMaxchunksMap = getMap(HighWayCore.hostMaxchunksMap);
            final String downloadLinkHost = link.getHost();
            final String hostFromLink = Browser.getHost(link.getPluginPatternMatcher(), true);
            if (hostMaxchunksMap.containsKey(downloadLinkHost)) {
                maxChunks = hostMaxchunksMap.get(downloadLinkHost);
            } else if (hostMaxchunksMap.containsKey(hostFromLink)) {
                /* E.g. selfhosted items download */
                maxChunks = hostMaxchunksMap.get(hostFromLink);
            }
        }
        if (maxChunks == null && account != null) {
            /* Look for account specific chunk limit. */
            maxChunks = (Number) account.getProperty(PROPERTY_ACCOUNT_MAXCHUNKS);
        }
        if (maxChunks == null) {
            /* Fallback to default */
            maxChunks = defaultMAXCHUNKS;
        }
        return correctChunks(maxChunks.intValue());
    }

    /** Corrects input so that it fits what we use in our plugins. */
    private int correctChunks(final int maxchunks) {
        if (maxchunks > 1) {
            /* Negative number means "up to X chunks". */
            return -maxchunks;
        } else {
            return maxchunks;
        }
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
        synchronized (getMapLock()) {
            final Map<String, AtomicInteger> hostRunningDlsNumMap = getMap(HighWayCore.hostRunningDlsNumMap);
            AtomicInteger currentRunningDls = hostRunningDlsNumMap.get(this.getDownloadLink().getHost());
            if (currentRunningDls == null) {
                currentRunningDls = new AtomicInteger(0);
                hostRunningDlsNumMap.put(this.getDownloadLink().getHost(), currentRunningDls);
            }
            currentRunningDls.addAndGet(num);
        }
    }

    private Map<String, Object> checkErrors(final Browser br, final DownloadLink link, final Account account) throws PluginException, InterruptedException {
        try {
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            this.checkErrors(entries, link, account);
            return entries;
        } catch (final JSonMapperException jse) {
            final String errormsg = "Invalid API response";
            final long waitMillis = 30 * 1000l;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, waitMillis);
            } else {
                throw new AccountUnavailableException(errormsg, waitMillis);
            }
        }
    }

    private void checkErrors(final Map<String, Object> entries, final DownloadLink link, final Account account) throws PluginException, InterruptedException {
        final Number code = (Number) entries.get("code");
        String msg = (String) entries.get("error");
        if (code == null && msg == null) {
            /* No error -> We're good :) */
            return;
        }
        int retrySeconds = 180;
        final Object retryInSecondsO = entries.get("retry_in_seconds");
        if (retryInSecondsO != null && (retryInSecondsO instanceof Number || retryInSecondsO.toString().matches("\\d+"))) {
            retrySeconds = Integer.parseInt(retryInSecondsO.toString());
        }
        if (code != null) {
            switch (code.intValue()) {
            case 0:
                /* No error */
                return;
            case 1:
                /* Invalid logindata */
                this.exceptionAccountInvalid(account);
            case 2:
                /* Session expired (this should never happen) */
                throw new AccountUnavailableException(msg, retrySeconds * 1000l);
            case 3:
                /* Not enough premium traffic available */
                throw new AccountUnavailableException(msg, retrySeconds * 1000l);
            case 4:
                /* User requested too many simultaneous downloads */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, retrySeconds * 1000l);
            case 5:
                /* Premium package expired --> Temp. deactivate account so next account-check will set correct new account status */
                throw new AccountUnavailableException(msg, retrySeconds * 1000l);
            case 6:
                /* No- or no valid URL was provided (this should never happen) */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, retrySeconds * 1000l);
            case 7:
                /* There is no case 7 (lol) */
            case 8:
                /* Temp. error try again later */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, retrySeconds * 1000l);
            case 9:
                /* File is offline */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            case 10:
                /* Host is not supported or not supported for free account users */
                getMultiHosterManagement().putError(account, link, retrySeconds * 1000l, msg);
            case 11:
                /**
                 * Host (not multihost) is currently under maintenance or offline --> Disable it for some time </br>
                 * 2021-11-08: Admin asked us not to disable host right away when this error happens as it seems like this error is more
                 * rleated to single files/fileservers -> Done accordingly.
                 */
                // mhm.putError(account, this.getDownloadLink(), retrySeconds * 1000l, msg);
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, retrySeconds * 1000l);
            case 12:
                /* Multihost itself is currently under maintenance --> Temp. disable account for some minutes */
                throw new AccountUnavailableException(msg, retrySeconds * 1000l);
            case 13:
                /* Password required or sent password was wrong --> This should never happen here as upper handling should handle this! */
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
            case 14:
                /* Host specific (= account specific) download limit has been reached --> Try file later */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, retrySeconds * 1000l);
            case 15:
                /*
                 * Host specific download request limit has been reached. This is basically the protection of this multihost against users
                 * trying to start a lot of downloads of limited hosts at the same time, trying to exceed the multihosts daily host specific
                 * limits.
                 */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, retrySeconds * 1000l);
            case 16:
                /* Error, user is supposed to contact support of this multihost. */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, retrySeconds * 1000l);
            default:
                /* Unknown/other errorcodes */
                if (link != null) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, retrySeconds * 1000l);
                } else {
                    throw new AccountUnavailableException(msg, retrySeconds * 1000l);
                }
            }
        } else {
            /* Handle legacy API error messages */
            if (msg.equalsIgnoreCase("UserOrPassInvalid")) {
                throw new AccountInvalidException();
            } else {
                logger.warning("Unknown- or no error message: " + msg);
            }
        }
    }

    @Override
    protected int getMaxSimultanDownload(final DownloadLink link, final Account account) {
        final int defaultMaxdlsUsenet = 10;
        final int defaultMaxdlsSelfhosted = 5;
        if (account != null && link != null) {
            if (isUsenetLink(link)) {
                return account.getIntegerProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET, defaultMaxdlsUsenet);
            } else if (link != null && link.getHost().equals(this.getHost())) {
                return defaultMaxdlsSelfhosted;
            } else {
                /* Look for host specific limit */
                synchronized (getMapLock()) {
                    final Map<String, Integer> hostMaxdlsMap = getMap(HighWayCore.hostMaxdlsMap);
                    if (hostMaxdlsMap.containsKey(link.getHost())) {
                        return hostMaxdlsMap.get(link.getHost());
                    }
                }
            }
        } else if (link != null && link.getHost().equals(this.getHost())) {
            return defaultMaxdlsSelfhosted;
        } else if (account != null) {
            /* Return max simultan downloads per account. */
            // return account.getMaxSimultanDownloads();
            return account.getIntegerProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_ACCOUNT, defaultMaxdlsUsenet);
        }
        return 1;
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://www." + getHost() + "/profile";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("[A-Za-z0-9]{32}")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean enoughTrafficFor(final DownloadLink link, final Account account) throws Exception {
        /**
         * 2025-09-12: Special: When a link is cached, it can be downloaded even if host specific traffic is empty. <br>
         * To account for this, this plugin contains a modified version of the original function.
         */
        if (account == null) {
            return true;
        }
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            return true;
        }
        final long trafficLeft = ai.getTrafficLeft();
        final long minimum = 1024;
        final long downloadSize = link.getView().getBytesTotalEstimated();
        long trafficNeeded;
        if (downloadSize > 0) {
            trafficNeeded = Math.max(minimum, downloadSize - link.getView().getBytesLoaded());
        } else {
            trafficNeeded = minimum;
        }
        if (account.isMultiHost() && !account.getHoster().equals(link.getHost())) {
            /* Check for domain specific limits of multihost items. */
            /* Verify again if host is still supported because plugins can modify list on runtime */
            final MultiHostHost mhost = ai.getMultihostSupportedHost(link.getHost());
            if (mhost == null) {
                /* Host is not supported (anymore) */
                return false;
            }
            /**
             * In some cases, individual hosts can have different traffic calculation values than 100%. <br>
             * This calculation applies for the global account-traffic and not for the individual host. </br>
             * Example: File size is 1GB, individual host traffic calculation factor is 400% <br>
             * Account traffic needed: 4GB <br>
             * Individual host traffic needed: 1GB
             */
            trafficNeeded = (trafficNeeded * mhost.getTrafficCalculationFactorPercent()) / 100;
        }
        if (!ai.isUnlimitedTraffic() && !ai.isSpecialTraffic()) {
            /* Check if enough traffic is left */
            if (trafficNeeded > trafficLeft) {
                if (ai.isTrafficRefill()) {
                    final long howMuchTrafficIsMissing = trafficNeeded - trafficLeft;
                    throw new ConditionalSkipReasonException(new WaitForAccountTrafficSkipReason(account, howMuchTrafficIsMissing));
                } else {
                    return false;
                }
            }
        }
        return true;
    }
}