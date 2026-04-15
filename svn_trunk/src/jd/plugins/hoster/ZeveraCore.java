package jd.plugins.hoster;

import java.net.MalformedURLException;
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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.parser.UrlQuery;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.requests.FormData;
import jd.http.requests.PostFormDataRequest;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.MultiHosterManagement;

//IMPORTANT: this class must stay in jd.plugins.hoster because it extends another plugin (UseNet) which is only available through PluginClassLoader
abstract public class ZeveraCore extends UseNet {
    /* Connection limits */
    private final String                         API_STATUS_FOR_QUEUE_DEFERRED_HANDLING = "deferred";
    private static final String                  PROPERTY_ACCOUNT_AUTH_TOKEN            = "access_token";
    private static final HashSet<String>         global_cache_hosts                     = new HashSet<String>();
    /* Map of timestamps of cache only items that are not in cache and thus shouldn't be retried. */
    private static final Map<DownloadLink, Long> cache_unavailable_timestamps           = new WeakHashMap<DownloadLink, Long>();

    protected abstract MultiHosterManagement getMultiHosterManagement();

    public ZeveraCore(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        final List<LazyPlugin.FEATURE> ret = new ArrayList<LazyPlugin.FEATURE>();
        ret.add(LazyPlugin.FEATURE.MULTIHOST);
        if (this.supportsUsenet(null)) {
            ret.add(LazyPlugin.FEATURE.USENET);
        }
        if (!usePairingLogin(null)) {
            ret.add(LazyPlugin.FEATURE.API_KEY_LOGIN);
        }
        return ret.toArray(new LazyPlugin.FEATURE[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://www." + this.getHost() + "/legal#tos";
    }

    /** Must override!! */
    public abstract String getClientID();

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        /* Resume is always possible */
        return true;
    }

    public int getMaxChunks(final Account account) {
        /* 2024-02-21: Workaround until "X-Cached" auto handling is working as expected. */
        return 1;
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
        if (this.isSelfhostedContent(link)) {
            try {
                return UrlQuery.parse(link.getPluginPatternMatcher()).get("id");
            } catch (final Throwable e) {
                return null;
            }
        } else {
            return null;
        }
    }

    /**
     * Returns how many max. chunks per file are allowed for current download mode based on account availability and account type. <br />
     * Override this function to set chunks settings!
     */
    public int getDownloadModeMaxChunks(final Account account) {
        if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 0;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        /* Direct URLs can be downloaded without account! */
        return Integer.MAX_VALUE;
    }

    public int getMaxSimultaneousFreeAccountDownloads() {
        /* No free downloads at all possible by default */
        return 0;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookiesExclusive(true);
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "JDownloader");
        // br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public boolean isSpeedLimited(final DownloadLink link, final Account account) {
        return false;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        if (isUsenetLink(link)) {
            return super.requestFileInformation(link);
        } else {
            final Account account = AccountController.getInstance().getValidAccount(this.getHost());
            requestFileInformationSelfhosted(link, account);
            return AvailableStatus.TRUE;
        }
    }

    protected Map<String, Object> requestFileInformationSelfhosted(final DownloadLink link, final Account account) throws Exception {
        final String fileID = getFID(link);
        if (fileID == null) {
            /* This should never happen. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (!link.isNameSet()) {
            /* Set meaningful names in case content is offline */
            link.setName(fileID);
        }
        if (account == null) {
            /* Cannot check without account */
            throw new AccountRequiredException();
        }
        /* See: https://app.swaggerhub.com/apis-docs/premiumize.me/api/1.6.7#/ */
        callAPI(this.br, account, "/api/item/details?id=" + fileID);
        final Map<String, Object> entries = this.handleAPIErrors(this, br, link, account);
        final Object detailsO = entries.get("details");
        final Map<String, Object> details;
        if (detailsO instanceof Map) {
            details = (Map<String, Object>) detailsO;
        } else {
            details = entries;
        }
        final String filename = (String) details.get("name");
        final Number filesize = (Number) details.get("size");
        if (!StringUtils.isEmpty(filename)) {
            link.setFinalFileName(filename);
        }
        if (filesize != null) {
            link.setVerifiedFileSize(filesize.longValue());
        }
        return details;
    }

    @Override
    public boolean canHandle(DownloadLink link, Account account) throws Exception {
        if (account == null) {
            return false;
        }
        final Long cache_failed_timestamp;
        synchronized (cache_unavailable_timestamps) {
            cache_failed_timestamp = cache_unavailable_timestamps.get(link);
        }
        if (cache_failed_timestamp != null && Time.systemIndependentCurrentJVMTimeMillis() - cache_failed_timestamp.longValue() < TimeUnit.DAYS.toMillis(10)) {
            /* Item has already been tried recently and was not in cache thus we know that we cannot handle it (at this moment). */
            return false;
        } else {
            return super.canHandle(link, account);
        }
    }

    private final Pattern PATTERN_SELFHOSTED = Pattern.compile("https?://[^/]+/file\\?id=.+", Pattern.CASE_INSENSITIVE);

    private boolean isSelfhostedContent(final DownloadLink link) {
        if (link == null) {
            return false;
        }
        final String url = link.getPluginPatternMatcher();
        if (url == null) {
            return false;
        }
        if (new Regex(url, PATTERN_SELFHOSTED).patternFind()) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new AccountRequiredException();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDLSelfhosted(link, account);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        if (isUsenetLink(link)) {
            super.handleMultiHost(link, account);
            return;
        } else {
            login(this.br, account, false);
            final String directlinkproperty = account.getHoster() + "directlink";
            final String storedDirecturl = link.getStringProperty(directlinkproperty);
            String dllink = null;
            if (storedDirecturl != null) {
                logger.info("Trying to re-use stored directurl: " + storedDirecturl);
                dllink = storedDirecturl;
            } else {
                final String external_url = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
                final String hash_md5 = link.getMD5Hash();
                final String hash_sha1 = link.getSha1Hash();
                final String hash_sha256 = link.getSha256Hash();
                /* https://app.swaggerhub.com/apis-docs/premiumize.me/api/1.6.7#/transfer/transferDirectdl */
                /**
                 * If enabled, API key will be included in URL even though we're already sending it via Authorization header to work around
                 * a server side bug. <br>
                 * 2026-04-14: It's unknown whether or not this workaround is still needed.
                 */
                final boolean useAPIKeyWorkaround = true;
                final boolean isCacheHost;
                synchronized (global_cache_hosts) {
                    isCacheHost = global_cache_hosts.contains(link.getHost());
                }
                if (isCacheHost) {
                    /* Host is "cache only host" which means that we can only download it if it's in the cache of this multihoster. */
                    String url = "https://www." + account.getHoster() + "/api/cache/check";
                    final UrlQuery query = new UrlQuery();
                    if (!useAPIKeyWorkaround && !usePairingLogin(account)) {
                        query.appendEncoded("apikey", getAPIKey(account));
                    }
                    query.appendEncoded("items[]", external_url);
                    url += "?" + query.toString();
                    br.getPage(url);
                    final Map<String, Object> entries = this.handleAPIErrors(this, br, link, account);
                    final List<Object> response = (List<Object>) entries.get("response");
                    if (!Boolean.TRUE.equals(response.get(0))) {
                        synchronized (cache_unavailable_timestamps) {
                            cache_unavailable_timestamps.put(link, Time.systemIndependentCurrentJVMTimeMillis());
                        }
                        throw new PluginException(LinkStatus.ERROR_RETRY, "Item not in cache");
                    }
                }
                String url = "https://www." + account.getHoster() + "/api/transfer/directdl";
                if (!useAPIKeyWorkaround && !usePairingLogin(account)) {
                    url += "?apikey=" + getAPIKey(account);
                }
                final PostFormDataRequest req = br.createPostFormDataRequest(url);
                req.addFormData(new FormData("src", external_url));
                req.addFormData(new FormData("folder_id", "null"));
                if (hash_md5 != null) {
                    req.addFormData(new FormData("hash_md5", hash_md5));
                }
                if (hash_sha1 != null) {
                    req.addFormData(new FormData("hash_sha1", hash_md5));
                }
                if (hash_sha256 != null) {
                    req.addFormData(new FormData("hash_sha256", hash_sha256));
                }
                br.getPage(req);
                final boolean useSlotBlockingQueueHandling = true;
                Map<String, Object> entries;
                if (useSlotBlockingQueueHandling) {
                    /* 2023-07-17 */
                    entries = this.handleAPIErrors(this, br, link, account, API_STATUS_FOR_QUEUE_DEFERRED_HANDLING);
                    String status = (String) entries.get("status");
                    if (API_STATUS_FOR_QUEUE_DEFERRED_HANDLING.equalsIgnoreCase(status)) {
                        logger.info("Executing slot-blocking queue handling");
                        final Number delay = (Number) entries.get("delay");
                        final boolean allowUnlimitedRetries = false; // 2023-08-03
                        // final int maxWaitSeconds = delay != null ? Math.min(delay.intValue() * 480, 2400) : 120;
                        /* TODO: Remove this long slot blocking wait handling as soon as new general account handling is released. */
                        final int maxWaitSeconds = 16 * 60 * 60;
                        int passedSeconds = 0;
                        final int waitSecondsPerLoop = 10;
                        final String maxWaitHumanReadable;
                        if (allowUnlimitedRetries) {
                            maxWaitHumanReadable = "Unlimited";
                        } else {
                            maxWaitHumanReadable = Integer.toString(maxWaitSeconds);
                        }
                        do {
                            this.sleep(waitSecondsPerLoop * 1000l, link, "deferred queue handling: Waiting sec " + passedSeconds + "/" + maxWaitHumanReadable);
                            passedSeconds += waitSecondsPerLoop;
                            br.getPage(req.cloneRequest());
                            entries = this.handleAPIErrors(this, br, link, account, API_STATUS_FOR_QUEUE_DEFERRED_HANDLING);
                            status = (String) entries.get("status");
                            logger.info("Waited seconds: " + passedSeconds + "/" + maxWaitHumanReadable + " | Serverside 'delay' value: " + delay);
                            if (!StringUtils.equals(status, "deferred")) {
                                logger.info("Stopping because: Status != deferred");
                                break;
                            } else if (!allowUnlimitedRetries && passedSeconds >= maxWaitSeconds) {
                                logger.info("Stopping because: Timeout -> Could not find final downloadurl");
                                break;
                            }
                        } while (!this.isAbort());
                        this.handleAPIErrors(this, entries, br, link, account, null);
                    }
                } else {
                    /* Old handling: No special handling for queue-downloads. */
                    entries = this.handleAPIErrors(this, br, link, account);
                }
                dllink = (String) entries.get("location");
                final String filename = (String) entries.get("filename");
                if (!StringUtils.isEmpty(filename) && link.getFinalFileName() == null) {
                    link.setFinalFileName(filename);
                }
                if (StringUtils.isEmpty(dllink)) {
                    handleAPIErrors(this, this.br, link, account);
                    /* This should never happen. */
                    getMultiHosterManagement().handleErrorGeneric(account, link, "Failed to find final downloadurl", 2, 5 * 60 * 1000l);
                }
                /*
                 * 2019-11-29: TODO: This is a workaround! They're caching data. This means that it may also happen that a slightly
                 * different file will get delivered (= different file hash). This is a bad workaround to "disable" the hash check of our
                 * original file thus prevent JD to display CRC errors when there are none. Premiumize is advised to at least return the
                 * correct MD5 hash so that we can set it accordingly but for now, we only have this workaround. See also:
                 * https://svn.jdownloader.org/issues/87604
                 */
                final boolean forceDisableCRCCheck = true;
                final long originalSourceFilesize = link.getView().getBytesTotal();
                long thisFilesize = JavaScriptEngineFactory.toLong(entries.get("filesize"), -1l);
                if (forceDisableCRCCheck || originalSourceFilesize > 0 && thisFilesize > 0 && thisFilesize != originalSourceFilesize) {
                    logger.info("Dumping existing hashes to prevent errors because of cache download");
                    link.setHashInfo(null);
                }
            }
            try {
                probeDownload(link, account, dllink, true);
            } catch (final Exception e) {
                if (storedDirecturl != null) {
                    link.removeProperty(directlinkproperty);
                    throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
                } else {
                    throw e;
                }
            }
            link.setProperty(directlinkproperty, dllink);
            final long verifiedFileSize = link.getVerifiedFileSize();
            final long completeContentLength = dl.getConnection().getCompleteContentLength();
            if (completeContentLength != verifiedFileSize) {
                logger.info("Update Filesize: old=" + verifiedFileSize + "|new=" + completeContentLength);
                link.setVerifiedFileSize(completeContentLength);
            }
            dl.startDownload();
        }
    }

    /** Prepares start of a download. Throws exception if non-file-content is returned on http request. */
    private void probeDownload(final DownloadLink link, final Account account, final String dllink, final boolean isMultihostHandling) throws Exception {
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), getMaxChunks(account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection();
            if (isMultihostHandling) {
                /* Only check for API issues if we got a json response. */
                if (br.getHttpConnection().getContentType().contains("application/json")) {
                    handleAPIErrors(this, br, link, account);
                }
                getMultiHosterManagement().handleErrorGeneric(account, link, "Unknown download error", 50, 5 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Final downloadurl did not lead to downloadable content", 3 * 60 * 1000l);
            }
        }
        // TODO: 2024-04-26: Work in progress: Make use of this
        // final int maxChunks;
        // final String cacheStatus = dl.getConnection().getRequest().getResponseHeader("X-Cached");
        // if (StringUtils.equalsIgnoreCase(cacheStatus, "hit")) {
        // maxChunks = getMaxChunks(account);
        // } else {
        // maxChunks = 1;
        // }
    }

    private void handleDLSelfhosted(final DownloadLink link, final Account account) throws Exception {
        if (!this.isSelfhostedContent(link)) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Map<String, Object> details = requestFileInformationSelfhosted(link, account);
        final String dllink = details.get("link").toString();
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        probeDownload(link, account, dllink, false);
        dl.startDownload();
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(br, account, true);
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> userinfo = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String customerID = userinfo.get("customer_id").toString();
        if (customerID != null) {
            account.setUser(customerID);
        }
        final Number fair_use_usedO = (Number) userinfo.get("limit_used");
        final Number space_usedO = (Number) userinfo.get("space_used");
        final Object premium_untilO = userinfo.get("premium_until");
        if (space_usedO != null) {
            ai.setUsedSpace(space_usedO.longValue());
        }
        /* E.g. free account: "premium_until":false or "premium_until":null */
        if (premium_untilO instanceof Number) {
            /* Premium account */
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(getMaxSimultanPremiumDownloadNum());
            final boolean boosterWorkaroundActive = isBoosterPointsUnlimitedTrafficWorkaroundActive(account);
            final double d = fair_use_usedO.doubleValue();
            final int fairUsagePercentUsed = (int) (d * 100.0);
            final int fairUsagePercentLeft = 100 - fairUsagePercentUsed;
            if (fairUsagePercentUsed >= 100 && !boosterWorkaroundActive) {
                throw new AccountUnavailableException("Fair use limit reached", 5 * 60 * 1000l);
            }
            String statustext = String.format(Locale.ROOT, "Premium | Fair-Use Status: %d%% left", fairUsagePercentLeft);
            if (boosterWorkaroundActive) {
                statustext += " | Unlimited Traffic Booster workaround enabled";
            }
            ai.setStatus(statustext);
            ai.setValidUntil(((Number) premium_untilO).longValue() * 1000, br);
            ai.setUnlimitedTraffic();
        } else {
            /* Free account -> Either expired premium account or account has always been a free account. */
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(getMaxSimultaneousFreeAccountDownloads());
            /** Default = Free accounts do not have any traffic. */
            ai.setTrafficLeft(0);
            ai.setExpired(true);
        }
        callAPI(br, account, "/api/services/list");
        synchronized (global_cache_hosts) {
            global_cache_hosts.clear();
            final Map<String, Object> hosterinfo = this.handleAPIErrors(this, br, null, account);
            final HashSet<String> supportedHostsMainDomains = new HashSet<String>();
            final HashSet<String> supportedHostsMainDomainsWithoutCache = new HashSet<String>();
            final String[] hosterListTypesToAdd = new String[] { "directdl", "queue" };
            for (final String hosterListTypeToAdd : hosterListTypesToAdd) {
                final List<String> hosts = (List<String>) hosterinfo.get(hosterListTypeToAdd);
                if (hosts == null || hosts.isEmpty()) {
                    continue;
                }
                supportedHostsMainDomains.addAll(hosts);
                supportedHostsMainDomainsWithoutCache.addAll(hosts);
            }
            final List<String> cachehosts = (List<String>) hosterinfo.get("cache");
            final Map<String, Number> fairusefactor = (Map<String, Number>) hosterinfo.get("fairusefactor");
            if (cachehosts != null) {
                supportedHostsMainDomains.addAll(cachehosts);
            }
            if (supportsUsenet(account)) {
                supportedHostsMainDomainsWithoutCache.add("usenet");
            }
            final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
            final Map<String, Object> aliasesmap = (Map<String, Object>) hosterinfo.get("aliases");
            for (final String mainDomain : supportedHostsMainDomains) {
                final boolean isCacheHost = cachehosts != null && cachehosts.contains(mainDomain) && !supportedHostsMainDomainsWithoutCache.contains(mainDomain);
                final MultiHostHost mhost = new MultiHostHost(mainDomain);
                final List<String> allDomains = (List<String>) aliasesmap.get(mainDomain);
                if (allDomains != null) {
                    mhost.addDomains(allDomains);
                }
                final Number fairusefactorThisHost = fairusefactor.get(mainDomain);
                if (fairusefactorThisHost != null && fairusefactorThisHost.intValue() != 1) {
                    /* E.g. factor 4 -> 400% traffic is deducted when downloading from that hoster. */
                    mhost.setTrafficCalculationFactorPercent((short) (fairusefactorThisHost.intValue() * 100));
                }
                // if (cachehosts != null && cachehosts.contains(mainDomain) && !supportedHostsMainDomains.contains(mainDomain)) {
                // mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                // mhost.setStatusText("Cache only (only for usage in KODI and other 3rd party addons)");
                // }
                if (isCacheHost) {
                    mhost.setStatus(MultihosterHostStatus.WORKING_UNSTABLE);
                    mhost.setStatusText("Cache only");
                    global_cache_hosts.addAll(allDomains);
                }
                supportedhosts.add(mhost);
            }
            ai.setMultiHostSupportV2(this, supportedhosts);
        }
        return ai;
    }

    public void login(final Browser br, final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final boolean useWorkaround = true;
            if (useWorkaround) {
                br.setCookie(getHost(), "sdk_login", getAPIKey(account));
            }
            if (usePairingLogin(account)) {
                /*
                 * 2019-06-26: New: TODO: We need a way to get the usenet logindata without exposing the original account logindata/apikey!
                 */
                final String clientID = this.getClientID();
                try {
                    boolean hasTriedOldToken = false;
                    final long tokenValidUntil = account.getLongProperty("token_valid_until", 0);
                    if (System.currentTimeMillis() > tokenValidUntil) {
                        logger.info("Token has expired");
                    } else if (setAuthHeader(br, account)) {
                        hasTriedOldToken = true;
                        callAPI(br, account, "/api/account/info");
                        try {
                            this.handleAPIErrors(this, br, null, account);
                            logger.info("Token login successful");
                            return;
                        } catch (final Throwable ignore) {
                            logger.info("Token expired or user has revoked access --> Full login required");
                        }
                        logger.info("Token expired or user has revoked access --> Full login required");
                    }
                    br.postPage("https://www." + account.getHoster() + "/token", "response_type=device_code&client_id=" + clientID);
                    Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    final long interval_seconds = ((Number) entries.get("interval")).longValue();
                    final long expires_in_seconds = ((Number) entries.get("expires_in")).longValue() - interval_seconds;
                    final long expires_in_timestamp = System.currentTimeMillis() + expires_in_seconds * 1000l;
                    final String verification_uri = (String) entries.get("verification_uri");
                    final String device_code = (String) entries.get("device_code");
                    final String user_code = (String) entries.get("user_code");
                    if (StringUtils.isEmpty(device_code) || StringUtils.isEmpty(user_code) || StringUtils.isEmpty(verification_uri)) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    boolean success = false;
                    int loop = 0;
                    int internal_max_loops_limit = 120;
                    final Thread dialog = showPairingLoginInformation(verification_uri, user_code);
                    String access_token = null;
                    try {
                        do {
                            logger.info("Waiting for user to authorize application: " + loop);
                            Thread.sleep(interval_seconds * 1001l);
                            br.postPage("https://www." + account.getHoster() + "/token", "grant_type=device_code&client_id=" + clientID + "&code=" + device_code);
                            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                            access_token = (String) entries.get(PROPERTY_ACCOUNT_AUTH_TOKEN);
                            if (!StringUtils.isEmpty(access_token)) {
                                success = true;
                                break;
                            } else if (!dialog.isAlive()) {
                                logger.info("Dialog closed!");
                                break;
                            }
                            loop++;
                        } while (!success && System.currentTimeMillis() < expires_in_timestamp && loop < internal_max_loops_limit);
                    } finally {
                        dialog.interrupt();
                    }
                    final String token_type = (String) entries.get("token_type");
                    if (!success) {
                        final String errormsg = "User did not confirm pairing code!\r\nDo not close the pairing dialog until you've confirmed the code via browser!";
                        if (hasTriedOldToken) {
                            /*
                             * Do not display permanent error if we still have an old token. Maybe something else has failed and the old
                             * token will work fine again on the next try.
                             */
                            throw new AccountUnavailableException(errormsg, 5 * 60 * 1000l);
                        } else {
                            throw new AccountInvalidException();
                        }
                    } else if (!"bearer".equals(token_type)) {
                        /* This should never happen! */
                        throw new AccountInvalidException("Unsupported token_type");
                    }
                    account.setProperty(PROPERTY_ACCOUNT_AUTH_TOKEN, access_token);
                    account.setProperty("token_valid_until", System.currentTimeMillis() + ((Number) entries.get("expires_in")).longValue());
                    setAuthHeader(br, account);
                    callAPI(br, account, "/api/account/info");
                    this.handleAPIErrors(this, br, null, account);
                    /* No Exception = Success */
                } finally {
                    /*
                     * Users may enter login data through webinterface which means they may even enter their real password which we do not
                     * want to store in our account database. Also we do not want this field to be empty (null) as this would look strange
                     * in the account manager.
                     */
                    account.setPass(null);
                }
            } else {
                if (account.hasEverBeenValid() && !force) {
                    /* Do not validate logins */
                    return;
                }
                callAPI(br, account, "/api/account/info");
                this.handleAPIErrors(this, br, null, account);
                /* No Exception = Success */
            }
        }
    }

    /**
     * For API calls AFTER logging-in, NOT for initial 'pairing' API calls (oauth login)!
     *
     * @throws Exception
     */
    private void callAPI(final Browser br, final Account account, String url) throws Exception {
        url = "https://www." + account.getHoster() + url;
        if (!url.contains("?")) {
            url += "?";
        } else {
            url += "&";
        }
        url += "client_id=" + this.getClientID();
        if (!this.usePairingLogin(account)) {
            /*
             * Without pairing login we need an additional parameter. It will also work with pairing mode when that parameter is given with
             * a wrong value but that may change in the future so this is to avoid issues!
             */
            url += "&apikey=" + URLEncode.encodeURIComponent(getAPIKey(account));
        }
        br.getPage(url);
    }

    @Override
    protected String getUseNetUsername(final Account account) {
        if (usePairingLogin(account)) {
            /* Login via access_token:access_token */
            return account.getStringProperty(PROPERTY_ACCOUNT_AUTH_TOKEN);
        } else {
            /* Login via APIKEY:APIKEY */
            return account.getPass();
        }
    }

    @Override
    protected String getUseNetPassword(final Account account) {
        if (usePairingLogin(account)) {
            /* Login via access_token:access_token */
            return account.getStringProperty(PROPERTY_ACCOUNT_AUTH_TOKEN);
        } else {
            /* Login via APIKEY:APIKEY */
            return account.getPass();
        }
    }

    /**
     * @return true: Account has 'access_token' property so pairing login was used. </br>
     *         false: Account does not have 'access_token' property so API Key login was used.
     */
    public static boolean setAuthHeader(final Browser br, final Account account) {
        final String access_token = account.getStringProperty(PROPERTY_ACCOUNT_AUTH_TOKEN);
        if (access_token != null) {
            br.getHeaders().put("Authorization", "Bearer " + access_token);
            return true;
        } else {
            return false;
        }
    }

    private Thread showPairingLoginInformation(final String verification_url, final String user_code) {
        final Thread thread = new Thread() {
            public void run() {
                try {
                    final String host = Browser.getHost(verification_url);
                    final String host_without_tld = host.split("\\.")[0];
                    String message = "";
                    final String title;
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        title = host + " - neue Login-Methode";
                        message += "Hallo liebe(r) " + host + " NutzerIn\r\n";
                        message += "Um deinen Account in JDownloader verwenden zu können, musst du folgende Schritte beachten:\r\n";
                        message += "1. Gehe sicher, dass du im Browser in deinem " + host_without_tld + " Account eingeloggt bist.\r\n";
                        message += "2. Öffne diesen Link im Browser falls das nicht automatisch passiert:\r\n\t'" + verification_url + "'\t\r\n";
                        message += "3. Gib im Browser folgenden Code ein: " + user_code + "\r\n";
                        message += "Dein Account sollte nach einigen Sekunden von JDownloader akzeptiert werden.\r\n";
                    } else {
                        title = host + " - New login method";
                        message += "Hello dear " + host + " user\r\n";
                        message += "In order to use this service in JDownloader you need to follow these steps:\r\n";
                        message += "1. Make sure that you're logged in your " + host_without_tld + " account with your browser.\r\n";
                        message += "2. Open this URL in your browser it that did not already happen automatically:\r\n\t'" + verification_url + "'\t\r\n";
                        message += "3. Enter the following code in the browser window: " + user_code + "\r\n";
                        message += "Your account should be accepted in JDownloader within a few seconds.\r\n";
                    }
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, title, message);
                    dialog.setTimeout(2 * 60 * 1000);
                    if (CrossSystem.isOpenBrowserSupported() && !Application.isHeadless()) {
                        CrossSystem.openURL(verification_url);
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

    public boolean supportsUsenet(final Account account) {
        return false;
    }

    /**
     * Indicates whether or not the new 'pairing' login is supported & enabled: https://alexbilbie.com/2016/04/oauth-2-device-flow-grant/
     */
    @Deprecated
    public boolean usePairingLogin(final Account account) {
        return false;
    }

    /**
     * 2019-08-21: Premiumize.me has so called 'booster points' which basically means that users with booster points can download more than
     * normal users can with their fair use limit: https://www.premiumize.me/booster </br>
     * Premiumize has not yet integrated this in their API which means accounts with booster points will run into the fair-use-limit in
     * JDownloader and will not be able to download any more files then. </br>
     * This workaround can set accounts to unlimited traffic so that users will still be able to download.</br>
     * Remove this workaround once Premiumize has integrated their booster points into their API.
     */
    public boolean isBoosterPointsUnlimitedTrafficWorkaroundActive(final Account account) {
        return false;
    }

    public String getAPIKey(final Account account) throws AccountInvalidException {
        final String str = account.getPass();
        if (this.looksLikeValidAPIKey(str)) {
            return str;
        } else {
            throw new AccountInvalidException("Invalid API key format");
        }
    }

    public Map<String, Object> handleAPIErrors(final Plugin plugin, final Browser br, final DownloadLink link, final Account account) throws Exception {
        return handleAPIErrors(plugin, br, link, account, null);
    }

    public Map<String, Object> handleAPIErrors(final Plugin plugin, final Browser br, final DownloadLink link, final Account account, final String errorStrIgnore) throws Exception {
        /* E.g. {"status":"error","error":"topup_required","message":"Please purchase premium membership or activate free mode."} */
        Map<String, Object> entries = null;
        try {
            entries = JavaScriptEngineFactory.jsonToJavaMap(br.getRequest().getHtmlCode());
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid API response", 60 * 1000l);
            } else {
                throw new AccountUnavailableException("Invalid API response", 60 * 1000);
            }
        }
        handleAPIErrors(plugin, entries, br, link, account, errorStrIgnore);
        return entries;
    }

    public void handleAPIErrors(final Plugin plugin, final Map<String, Object> entries, final Browser br, final DownloadLink link, final Account account) throws Exception {
        handleAPIErrors(plugin, entries, br, link, account, null);
    }

    public void handleAPIErrors(final Plugin plugin, final Map<String, Object> entries, final Browser br, final DownloadLink link, final Account account, final String errorStrIgnore) throws Exception {
        final Map<String, Object> errormap = (Map<String, Object>) entries.get("error");
        final String status = (String) entries.get("status");
        final String message = (String) entries.get("message");
        /* API can control how long we should wait until next retry. */
        final Number delaySecondsO = (Number) entries.get("delay");
        final long retryInMilliseconds = delaySecondsO != null ? delaySecondsO.longValue() * 1000 : 1 * 60 * 1000;
        if (errorStrIgnore != null && (StringUtils.equalsIgnoreCase(status, errorStrIgnore) || StringUtils.equalsIgnoreCase(message, errorStrIgnore))) {
            /* Ignore this particular errormessage for now. */
            return;
        }
        if ("error".equalsIgnoreCase(status) && !StringUtils.isEmpty(message)) {
            /* This field is not always given! */
            final String errortype = (String) entries.get("error");
            if ("topup_required".equalsIgnoreCase(errortype)) {
                /**
                 * 2019-07-27: Currently if a user tries to download files via free account and gets this error message, it is unclear
                 * whether: <br>
                 * 1. Premium is required to download <br>
                 * 2. User needs to activate free mode first to download this file. <br>
                 * 3. User has activated free mode but this file is not allowed to be downloaded via free account. <br>
                 * 2026-04-13: Free mode does not exist anymore, it once existed here: premiumize.me/free
                 */
                /* Premium account - probably no traffic left */
                throw new AccountUnavailableException("Traffic empty or fair use limit reached?", retryInMilliseconds);
            } else if (message.matches("(?i).*customer_id and pin parameter missing or not logged in.*")) {
                throw new AccountInvalidException();
            } else if (message.matches("(?i).*Not logged in.*")) {
                throw new AccountInvalidException();
            } else if (message.matches("(?i).*Fair use limit reached.*")) {
                throw new AccountUnavailableException(message, 5 * 60 * 1000l);
            } else if (message.matches("(?i).*content not in cache.*")) {
                /* 2019-02-19: Not all errors have an errortype given */
                /* E.g. {"status":"error","message":"content not in cache"} */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, message, retryInMilliseconds);
            } else if (message.matches("(?i).*file not found.*")) {
                /*
                 * { "status": "error", "message": "Error: file not found"}
                 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
            } else if (message.matches("(?i).*item not found.*")) {
                /*
                 * 2020-07-16: This should only happen for selfhosted cloud items --> Offline: {"status":"error","message":"item not found"}
                 * --> Trust offline status
                 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
            } else if (message.matches("(?i).*Daily linklimit for this service reached.*")) {
                /*
                 * {"status":"error","message":"Daily linklimit for this service reached."}
                 */
                this.getMultiHosterManagement().putError(account, link, 30 * 60 * 1000l, message);
            } else {
                /* Unknown error */
                if (plugin instanceof PluginForDecrypt) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
                } else if (link == null) {
                    /* Account/login related error */
                    throw new AccountUnavailableException(message, retryInMilliseconds);
                } else if (errortype != null) {
                    getMultiHosterManagement().handleErrorGeneric(account, link, errortype, 2, retryInMilliseconds);
                } else {
                    getMultiHosterManagement().handleErrorGeneric(account, link, message, 2, retryInMilliseconds);
                }
            }
        } else if (API_STATUS_FOR_QUEUE_DEFERRED_HANDLING.equalsIgnoreCase(status)) {
            /*
             * Most likely user tried to download a file of a host which is in the "queue" list of supported host. Such files first need to
             * be downloaded 100% serverside until the user can download them.
             */
            getMultiHosterManagement().handleErrorGeneric(account, link, message != null ? message : "Wait for serverside download until you can download this file", 50, retryInMilliseconds);
        } else if (errormap != null) {
            /* 2023-05-20: new json zevera.com(?) */
            final String messageNew = errormap.get("message").toString();
            if (messageNew.matches("(?i).*file not found.*")) {
                /*
                 * {"jsonrpc":"2.0","id":1,"error":{"code":0,"message":"File not found or not your file"}}
                 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
            } else {
                if (plugin instanceof PluginForDecrypt) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
                } else if (link == null) {
                    /* Account/login related error */
                    throw new AccountUnavailableException(messageNew, retryInMilliseconds);
                } else {
                    getMultiHosterManagement().handleErrorGeneric(account, link, messageNew, 2, retryInMilliseconds);
                }
            }
        }
    }

    public static String getCloudID(final String url) throws MalformedURLException {
        if (url == null) {
            return null;
        }
        final UrlQuery query = UrlQuery.parse(url);
        final String folder_id = query.get("folder_id");
        if (folder_id != null) {
            return folder_id;
        } else {
            return query.get("id");
        }
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://www." + getHost() + "/account";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("[a-z0-9]{16}")) {
            return true;
        } else {
            return false;
        }
    }
}