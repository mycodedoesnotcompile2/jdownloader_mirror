//jDownloader - Downloadmanager
//Copyright (C) 2009  JD-Team support@jdownloader.org
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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.downloads.columns.ETAColumn;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.PluginTaskID;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.PluginProgress;
import jd.plugins.components.MultiHosterManagement;

/**
 *
 * @author raztoki
 * @author psp
 * @author bilalghouri
 */
@HostPlugin(revision = "$Revision: 51462 $", interfaceVersion = 3, names = { "linksnappy.com" }, urls = { "https?://(?:www\\.)?linksnappy\\.com/torrents/(\\d+)/download" })
public class LinkSnappyCom extends PluginForHost {
    private static MultiHosterManagement mhm = new MultiHosterManagement("linksnappy.com");

    public LinkSnappyCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/home");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader " + getVersion());
        br.addAllowedResponseCodes(new int[] { 425, 429, 502, 503, 504, 507 });
        br.setConnectTimeout(2 * 60 * 1000);
        br.setReadTimeout(2 * 60 * 1000);
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/tos";
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return "linksnappy://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        try {
            /* Try to get this information from map which is saved on Account object every time the account gets checked. */
            final Map<String, Map<String, Object>> allHosterInfoMap = (Map<String, Map<String, Object>>) account.getProperty(PROPERTY_HOSTER_INFO_MAP);
            final Map<String, Object> thisHosterInfoMap = allHosterInfoMap.get(link.getHost());
            final int connlimit = Integer.parseInt(thisHosterInfoMap.get("connlimit").toString());
            if (connlimit > 1) {
                return -connlimit;
            } else {
                return connlimit;
            }
        } catch (final Throwable e) {
            final int maxchunksDefault = 1;
            logger.log(e);
            logger.warning("Missing or faulty hostermap for host: " + link.getHost());
            return maxchunksDefault;
        }
    }

    /**
     * Defines max. wait time for cached downloads after last serverside progress change. </br>
     * Longer time than this and progress of serverside download did not change --> Abort
     */
    private final int    CACHE_WAIT_THRESHOLD     = 10 * 60000;
    private final String PROPERTY_DIRECTURL       = "linksnappycomdirectlink";
    private final String PROPERTY_HOSTER_INFO_MAP = "hoster_info_map";

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, AccountController.getInstance().getValidAccount(this.getHost()));
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        if (!link.isNameSet()) {
            /* Set fallback name */
            link.setName(this.getFID(link));
        }
        if (account == null) {
            throw new AccountRequiredException();
        }
        loginAPI(account, false);
        basicLinkCheck(br.cloneBrowser(), br.createGetRequest(link.getPluginPatternMatcher()), link, null, null);
        return AvailableStatus.TRUE;
    }

    @Override
    protected void throwFinalConnectionException(Browser br, URLConnectionAdapter con) throws PluginException, IOException {
        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Final downloadurl did not lead to downloadable content");
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.loginAPI(account, false);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, modifySSL(link.getPluginPatternMatcher()), true, 0);
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        return api_fetchAccountInfo(account, true);
    }

    private AccountInfo api_fetchAccountInfo(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            final Map<String, Object> userResponse = loginAPI(account, force);
            final Map<String, Object> usermap = (Map<String, Object>) userResponse.get("return");
            final Object expireTimestampO = usermap.get("expire");
            /*
             * final String accountType = (String) entries.get("accountType"); // "free" for free accounts and "elite" for premium AND
             * lifetime accounts
             */
            final AccountInfo ac = new AccountInfo();
            if ("lifetime".equalsIgnoreCase(expireTimestampO.toString()) || "2177388000".equals(expireTimestampO.toString())) {
                /* 2177388000 -> Valid until 2038 -> Lifetime account */
                account.setType(AccountType.LIFETIME);
            } else {
                long validUntil = -1;
                if (expireTimestampO instanceof Number) {
                    validUntil = ((Number) expireTimestampO).longValue() * 1000;
                } else if (expireTimestampO instanceof String) {
                    final String expireStr = expireTimestampO.toString();
                    if ("expired".equalsIgnoreCase(expireStr)) {
                        validUntil = -1;
                    } else if (expireStr.matches("\\d+")) {
                        validUntil = Long.parseLong(expireTimestampO.toString()) * 1000;
                    }
                }
                if (validUntil > System.currentTimeMillis()) {
                    ac.setValidUntil(validUntil);
                    account.setType(AccountType.PREMIUM);
                } else {
                    account.setType(AccountType.FREE);
                }
            }
            final Number usedSpace = (Number) usermap.get("usedspace");
            if (usedSpace != null) {
                ac.setUsedSpace(usedSpace.longValue());
            }
            final SIZEUNIT maxSizeUnit = (SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue();
            final Number trafficUsedTodayBytes = (Number) usermap.get("trafficused");
            final Object trafficleftGlobalO = usermap.get("trafficleft"); // mostly "unlimited"
            String trafficMaxDailyHumanReadable = "N/A";
            final Number maxtrafficDailyBytesO = (Number) usermap.get("maxtraffic");
            if (maxtrafficDailyBytesO != null) {
                trafficMaxDailyHumanReadable = SIZEUNIT.formatValue(maxSizeUnit, maxtrafficDailyBytesO.longValue());
            }
            if (trafficleftGlobalO instanceof String) {
                /* Value should be "unlimited" */
                ac.setUnlimitedTraffic();
            } else if (trafficleftGlobalO instanceof Number) {
                /* Also check for negative traffic */
                final long trafficleft = ((Number) trafficleftGlobalO).longValue();
                if (trafficleft <= 0) {
                    ac.setTrafficLeft(0);
                } else {
                    ac.setTrafficLeft(trafficleft);
                }
                if (maxtrafficDailyBytesO != null) {
                    ac.setTrafficMax(maxtrafficDailyBytesO.longValue());
                }
            }
            if (trafficUsedTodayBytes != null) {
                ac.setStatus(String.format("%s | Today's usage: %s/%s", account.getType().getLabel(), SIZEUNIT.formatValue(maxSizeUnit, trafficUsedTodayBytes.longValue()), trafficMaxDailyHumanReadable));
            }
            br.getPage("/api/FILEHOSTS");
            final Map<String, Object> hosterMapResponse = this.handleErrors(br, null, account);
            final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
            /* Connection info map */
            final Map<String, Map<String, Object>> allHosterInfoMap = new HashMap<String, Map<String, Object>>();
            final Map<String, Object> hosterMap = (Map<String, Object>) hosterMapResponse.get("return");
            final Iterator<Entry<String, Object>> it = hosterMap.entrySet().iterator();
            while (it.hasNext()) {
                final Entry<String, Object> entry = it.next();
                final Map<String, Object> thisHosterInformation = (Map<String, Object>) entry.getValue();
                final String domain = entry.getKey();
                final int status = Integer.parseInt(thisHosterInformation.get("Status").toString());
                // final int connlimit = Integer.parseInt(thisHosterInformation.get("connlimit").toString());
                // final long noretry = JavaScriptEngineFactory.toLong(hosterInformation.get("noretry"), 0);
                final long usage = ((Number) thisHosterInformation.get("Usage")).longValue();
                final boolean resume = ((Number) thisHosterInformation.get("resume")).intValue() == 1;
                final Object quotaO = thisHosterInformation.get("Quota");
                final long canDownload = ((Number) thisHosterInformation.get("canDownload")).longValue();
                /* Workaround to find real host. */
                final ArrayList<String> tempList = new ArrayList<String>();
                tempList.add(domain);
                final List<String> realHosts = ac.setMultiHostSupport(this, tempList);
                if (realHosts != null && !realHosts.isEmpty()) {
                    /* Legacy handling */
                    final String realHost = realHosts.get(0);
                    allHosterInfoMap.put(realHost, thisHosterInformation);
                }
                final MultiHostHost mhost = new MultiHostHost(domain);
                if (canDownload != 1) {
                    mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                } else if (status != 1) {
                    /* Host is currently not working or disabled */
                    mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                }
                // mhost.setTrafficUsed(usage);
                mhost.setResumable(resume);
                // mhost.setMaxChunks(connlimit);
                if (quotaO instanceof Number) {
                    mhost.setTrafficMax(((Number) quotaO).longValue());
                    mhost.setTrafficLeft(mhost.getTrafficMax() - usage);
                }
                supportedhosts.add(mhost);
            }
            account.setProperty(PROPERTY_HOSTER_INFO_MAP, allHosterInfoMap);
            // final List<String> mapped = ac.setMultiHostSupport(this, supportedHosts);
            /* Free account information & downloading is only possible via website; not via API! */
            if (AccountType.FREE == account.getType()) {
                /* Try to find Free Account limits to display them properly */
                /* 2025-09-08: Looks like free accounts cannot be used for downloading anymore. */
                logger.info("Trying to obtain free account information from website");
                try {
                    br.getPage("/download");
                    /*
                     * 2019-09-05: Free Accounts must be verified via mail or they cannot download anything. E.g.
                     * "</i>Account Status : Unverified. Please verify your email OR purchase <a href="/myaccount/
                     * extend">an Elite account</a> in order to start download." OR
                     * ">Activation code has been blocked due to violation of our terms of service. Buy Elite membership in order to Download."
                     */
                    final Regex remainingURLS = br.getRegex("id\\s*=\\s*\"linkleft\">\\s*(\\d+)\\s*</span>\\s*out of (\\d+) premium link");
                    final String remainingDailyURLsStr = remainingURLS.getMatch(0);
                    final String maxDailyURLsStr = remainingURLS.getMatch(1);
                    final int remainingURLs = Integer.parseInt(remainingDailyURLsStr);
                    if (remainingURLs == 0) {
                        /* 0 links left for today --> ZERO trafficleft */
                        ac.setTrafficLeft(0);
                    } else {
                        ac.setUnlimitedTraffic();
                    }
                    ac.setStatus(String.format("Free Account [%s of %s daily links left]", remainingDailyURLsStr, maxDailyURLsStr));
                } catch (final Exception ignore) {
                    logger.exception("Failed to find free Account limits --> Setting ZERO trafficleft", ignore);
                    ac.setTrafficLeft(0);
                    ac.setStatus("Free Account [Failed to find number of URLs left]");
                }
            }
            ac.setMultiHostSupportV2(this, supportedhosts);
            return ac;
        }
    }

    private void errorDailyLimitReached(final DownloadLink link, final Account account, final String suggestedErrorMessage) throws PluginException {
        final String msg;
        if (suggestedErrorMessage != null) {
            msg = suggestedErrorMessage;
        } else {
            msg = "Account has exceeded the daily quota";
        }
        if (link != null) {
            /* Daily specific host downloadlimit reached --> Disable host for some time */
            mhm.putError(account, link, 10 * 60 * 1000l, msg);
        } else {
            /* Daily total downloadlimit for account is reached */
            logger.info("Daily limit reached");
            /* Workaround for account overview display bug so users see at least that there is no traffic left */
            final AccountInfo ai = account.getAccountInfo();
            if (ai != null) {
                ai.setTrafficLeft(0);
            }
            throw new AccountUnavailableException(msg, 5 * 60 * 1000);
        }
    }

    /**
     * Check with linksnappy server if file needs to get downloaded first before the user can download it from there (2019-05-14: E.g.
     * rapidgator.net URLs).
     **/
    private void cacheDLChecker(final DownloadLink link, final Account account, final String id) throws Exception {
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
                return "Preparing your file: " + lastCurrent + "%";
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
        try {
            long lastProgressChange = System.currentTimeMillis();
            int lastProgress = -1;
            int round = 1;
            while (System.currentTimeMillis() - lastProgressChange < CACHE_WAIT_THRESHOLD) {
                logger.info("Checking cache status round: " + round);
                br.getPage("https://" + this.getHost() + "/api/CACHEDLSTATUS?id=" + Encoding.urlEncode(id));
                final Map<String, Object> data = this.handleErrors(br, link, account);
                if (data.get("return") == null) {
                    logger.warning("Bad cache state/answer");
                    break;
                }
                final Map<String, Object> cacheReturnStatus = (Map<String, Object>) data.get("return");
                final Integer currentProgress = (int) JavaScriptEngineFactory.toLong(cacheReturnStatus.get("percent"), 0);
                // download complete?
                if (currentProgress.intValue() == 100) {
                    // cache finished, lets go to download part
                    return;
                }
                link.addPluginProgress(waitProgress);
                waitProgress.updateValues(currentProgress.intValue(), 100);
                this.sleep(10000, link, "Preparing your file: " + currentProgress + "%");
                if (currentProgress.intValue() != lastProgress) {
                    lastProgressChange = System.currentTimeMillis();
                    lastProgress = currentProgress.intValue();
                }
                round++;
            }
        } finally {
            link.removePluginProgress(waitProgress);
        }
        mhm.handleErrorGeneric(account, link, "Cache handling timeout", 10);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        this.loginAPI(account, false);
        if (attemptStoredDownloadurlDownload(link, account)) {
            logger.info("Using previously generated final downloadurl");
        } else {
            logger.info("Generating new downloadurl");
            final String urlRequest;
            if (AccountType.FREE == account.getType()) {
                urlRequest = "https://" + this.getHost() + "/api/linkfree?genLinks=";
                /* Free Account download - not possible via API! */
                loginWebsite(account, false);
            } else {
                urlRequest = "https://" + this.getHost() + "/api/linkgen?genLinks=";
                this.loginAPI(account, false);
            }
            Map<String, Object> entries = null;
            String passCode = link.getDownloadPassword();
            String dllink = null;
            Boolean enteredCorrectPassword = null;
            for (int i = 0; i <= 3; i++) {
                if (i > 0) {
                    passCode = getUserInput("Password?", link);
                }
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("link", link.getDefaultPlugin().buildExternalDownloadURL(link, this));
                postdata.put("type", "");
                if (!StringUtils.isEmpty(passCode)) {
                    postdata.put("linkpass", passCode);
                }
                br.getPage(urlRequest + URLEncode.encodeURIComponent(JSonStorage.serializeToJson(postdata)));
                entries = this.parseJson(br, link, account);
                final List<Object> ressourcelist = (List<Object>) entries.get("links");
                if (ressourcelist == null) {
                    enteredCorrectPassword = null;
                    break;
                }
                entries = (Map<String, Object>) ressourcelist.get(0);
                final String message = this.getError(entries);
                if (this.isErrorDownloadPasswordRequiredOrWrong(message)) {
                    logger.info("User entered wrong password: " + passCode);
                    continue;
                } else {
                    /* User has entered correct password or none was needed. */
                    enteredCorrectPassword = true;
                    break;
                }
            }
            if (Boolean.FALSE.equals(enteredCorrectPassword)) {
                /* Allow next candidate to try */
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
            }
            if (Boolean.TRUE.equals(enteredCorrectPassword) && passCode != null) {
                link.setDownloadPassword(passCode);
            }
            /* Check for other errors */
            handleErrors(link, account, entries);
            /* 2021-02-18: Downloadurl will always be returned even if file hasn't been downloaded successfully serverside yet! */
            dllink = modifySSL((String) entries.get("generated"));
            if (StringUtils.isEmpty(dllink)) {
                /* This should never happen */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to find final downloadurl");
            }
            if (entries.containsKey("cacheDL")) {
                logger.info("Checking caching file...");
                cacheDLChecker(link, account, (String) entries.get("hash"));
            }
            final String filename = (String) entries.get("filename");
            if (link.getFinalFileName() == null && filename != null && filename.contains("_")) {
                // fix server side broken filenames, unicode characters collapsed to _
                // remove all unicode characters and compare existing and server side filename
                final String existingName = link.getName();
                final String check = existingName.replaceAll("[^a-zA-Z0-9\\.]", "");
                final String check2 = URLEncode.decodeURIComponent(filename).replaceAll("[^a-zA-Z0-9\\.]", "");
                if (StringUtils.equalsIgnoreCase(check, check2)) {
                    // do not trust/use server side filename and keep existing name
                    link.setFinalFileName(existingName);
                }
            }
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                this.handleDownloadErrors(link, account);
                handleConnectionErrors(br, dl.getConnection());
            }
            link.setProperty(PROPERTY_DIRECTURL, dllink);
        }
        if (dl.startDownload() && PluginJsonConfig.get(LinkSnappyComConfig.class).isClearDownloadHistoryEnabled()) {
            /**
             * Check if user wants JD to clear serverside download history in linksnappy account after each successful download. </br>
             * Also make sure we get no exception as our download was successful. </br>
             * NOTE: Even failed downloads will appear in the download history - but they will also be cleared once there is one successful
             * download.
             */
            logger.info("Clearing download history");
            try {
                br.getPage("https://" + this.getHost() + "/api/DELETELINK?type=filehost&hash=all");
                this.handleErrors(br, link, account);
                /* No exception = Success! */
                logger.info("Delete history succeeded!");
            } catch (final Throwable ignore) {
                logger.log(ignore);
                logger.warning("Delete download history failed");
            }
        }
    }

    private String modifySSL(final String url) {
        if (url == null) {
            return null;
        }
        if (PluginJsonConfig.get(LinkSnappyComConfig.class).isSSLDownloadsEnabled()) {
            return url.replaceFirst("(?i)^http://", "https://");
        } else {
            return url.replaceFirst("(?i)^https://", "http://");
        }
    }

    private void handleDownloadErrors(final DownloadLink link, final Account account) throws PluginException, IOException, InterruptedException {
        final int dlResponseCode = dl.getConnection().getResponseCode();
        if (dlResponseCode == 401) {
            /*
             * Claimed ip session changed mid session. not physically possible in JD... but user could have load balancing software or
             * router or isps' also can do this. a full retry should happen
             */
            throw new PluginException(LinkStatus.ERROR_RETRY, "Your IP has been changed. Please retry");
        } else if (dlResponseCode == 425) {
            /*
             * This error code will occur only when the link is being cached in LS system. You have to wait till its finished. Check
             * "/api/CACHEDLSTATUS" for current progress.
             */
            throw new PluginException(LinkStatus.ERROR_RETRY, "Link is being cached. Please wait.");
        } else if (dlResponseCode == 502) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Connection timeout from filehost", 5 * 60 * 1000l);
        } else if (dlResponseCode == 503) {
            // Max 10 retries above link, 5 seconds waittime between = max 2 minutes trying -> Then deactivate host
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "503 error", 5 * 60 * 1000l);
        } else if (dlResponseCode == 504) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid response. Retrying", 5 * 60 * 1000l);
        } else if (dlResponseCode == 507) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Moving to new server", 5 * 60 * 1000l);
        } else if (dlResponseCode == 509) {
            /* out of traffic should not retry! throw exception on first response! */
            errorDailyLimitReached(link, account, null);
        } else if (dlResponseCode == 429) {
            // what does ' max connection limit' error mean??, for user to that given hoster??, or user to that linksnappy finallink
            // server?? or linksnappy global (across all finallink servers) connections
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Max Connection limit reached", 5 * 60 * 1000l);
        }
        if (dl.getConnection() == null || !this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            mhm.handleErrorGeneric(account, link, "Unknown download error", 10, 5 * 60 * 1000l);
        }
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final Account account) throws Exception {
        final String url = modifySSL(link.getStringProperty(PROPERTY_DIRECTURL));
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        boolean valid = false;
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                valid = true;
                return true;
            } else {
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            logger.log(e);
            return false;
        } finally {
            if (!valid) {
                link.removeProperty(PROPERTY_DIRECTURL);
                try {
                    dl.getConnection().disconnect();
                } catch (Throwable ignore) {
                }
                this.dl = null;
            }
        }
    }

    /**
     * Parses API json (without error handling for API answer). </br>
     * Takes care about invalid API responses (non-json responses).
     */
    private Map<String, Object> parseJson(final Browser br, final DownloadLink link, final Account account) throws PluginException, InterruptedException {
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String msg = "Invalid API response";
            final long wait = 1 * 60 * 1000;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, wait);
            } else {
                throw new AccountUnavailableException(msg, wait);
            }
        }
        return entries;
    }

    private Map<String, Object> handleErrors(final Browser br, final DownloadLink link, final Account account) throws PluginException, InterruptedException {
        final Map<String, Object> entries = parseJson(br, link, account);
        handleErrors(link, account, entries);
        return entries;
    }

    private void handleErrors(final DownloadLink link, final Account account, final Map<String, Object> entries) throws PluginException, InterruptedException {
        final String errormsg = getError(entries);
        if (errormsg == null) {
            return;
        }
        if (StringUtils.containsIgnoreCase(errormsg, "Two-Factor Verification Required")) {
            /* User needs to confirm link via email before he can login via API via JDownloader. */
            String redirect = (String) entries.get("redirect");
            try {
                /* Create full URL out of relative URL. */
                redirect = br.getURL(redirect).toString();
            } catch (final Throwable ignore) {
            }
            throw new AccountUnavailableException("\r\n" + errormsg + "\r\n" + redirect, 5 * 60 * 1000l);
        }
        if (new Regex(errormsg, "(?i)No server available for this filehost, Please retry after few minutes").matches()) {
            /* Temp disable complete filehost for some minutes */
            mhm.putError(account, link, 5 * 60 * 1000l, errormsg);
        } else if (new Regex(errormsg, "(?i)You have reached max download request").matches()) {
            /* Too many requests -> Disable currently used filehost for some minutes. */
            mhm.putError(account, link, 5 * 60 * 1000l, "Too many requests. Please wait 5 minutes");
        } else if (new Regex(errormsg, "(?i)You have reached max download limit of").matches()) {
            try {
                account.getAccountInfo().setTrafficLeft(0);
            } catch (final Throwable ignore) {
                // Catch NPE
            }
            throw new AccountUnavailableException("\r\nLimit Reached. Please purchase elite membership!", 1 * 60 * 1000);
        } else if (new Regex(errormsg, "(?i)Invalid file URL format\\.").matches()) {
            /*
             * Update by Bilal Ghouri: Should not disable support for the entire host for this error. it means the host is online but the
             * link format is not added on linksnappy.
             */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "URL format not supported by multihoster " + this.getHost());
        } else if (new Regex(errormsg, "(?i)File not found").matches() || new Regex(errormsg, "(?i)File deleted on.*").matches()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (new Regex(errormsg, "(?i)Your Account has Expired").matches()) {
            /*
             * 2019-09-03 "{"status": "ERROR", "error": "Your Account has Expired, Please <a
             * href=\"https://linksnappy.com/myaccount/extend\">extend it</a>"}"
             */
            /*
             * This message may also happens if you try to download with a free account with UN-confirmed E-Mail!! Browser will show a more
             * precise errormessage in this case!
             */
            throw new AccountUnavailableException("Account expired", 5 * 60 * 1000l);
        } else if (isErrorDownloadPasswordRequiredOrWrong(errormsg)) {
            /** This error will usually be handled outside of here! */
            link.setDownloadPassword(null);
            throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
        } else if (new Regex(errormsg, "(?i)Please upgrade to Elite membership").matches()) {
            /* 2019-09-05: Free Account daily downloadlimit reached */
            throw new AccountUnavailableException("Daily downloadlimit reached", 10 * 60 * 1000l);
        } else if (new Regex(errormsg, "(?i)Incorrect Username or Password").matches()) {
            throw new AccountInvalidException(errormsg);
        } else if (new Regex(errormsg, "(?i)Account has exceeded the daily quota").matches()) {
            errorDailyLimitReached(null, account, errormsg);
            /* This code will never be reached. */
            throw new AccountInvalidException(errormsg);
        } else {
            logger.warning("Misc API error occured: " + errormsg);
            if (link == null) {
                /* Temp disable account */
                throw new AccountUnavailableException(errormsg, 10 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, 5 * 60 * 1000l);
            }
        }
    }

    private boolean isErrorDownloadPasswordRequiredOrWrong(final String msg) {
        if (msg != null && msg.matches("(?i)This file requires password")) {
            return true;
        } else {
            return false;
        }
    }

    private String getError(final Map<String, Object> map) {
        if (map == null) {
            return null;
        }
        final Object status = map.get("status");
        final Object error = map.get("error");
        if (status != null) {
            if ("OK".equalsIgnoreCase(status.toString())) {
                return null;
            } else {
                if (error instanceof String) {
                    return error.toString();
                } else {
                    return "unknown/" + status + "/" + error;
                }
            }
        } else if (error != null) {
            if (error instanceof String) {
                return error.toString();
            } else {
                return "unknown/" + status + "/" + error;
            }
        } else {
            return null;
        }
    }

    private Map<String, Object> loginAPI(final Account account, final boolean validateCookies) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            final String relative_url_userdetails = "/api/USERDETAILS";
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(this.getHost(), cookies);
                if (!validateCookies) {
                    /* Do not validate cookies. */
                    return null;
                }
                logger.info("Validating cookies");
                br.getPage("https://" + this.getHost() + relative_url_userdetails);
                try {
                    final Map<String, Object> entries = this.handleErrors(br, null, account);
                    logger.info("Cached login successful");
                    /* Save new cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return entries;
                } catch (final Throwable e) {
                    logger.log(e);
                    logger.info("Cached login failed");
                    br.clearCookies(null);
                    account.clearCookies("");
                }
            }
            /* Full login is required */
            logger.info("Performing full login");
            br.getPage("https://" + this.getHost() + "/api/AUTHENTICATE?" + "username=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
            this.handleErrors(br, null, account);
            account.saveCookies(br.getCookies(br.getHost()), "");
            br.getPage("https://" + this.getHost() + relative_url_userdetails);
            final Map<String, Object> entries2 = this.handleErrors(br, null, account);
            return entries2;
        }
    }

    @Deprecated
    private void loginWebsite(final Account account, final boolean verifyCookies) throws Exception {
        /* 2019-09-05: API cookies and website cookies are the same and can be used for both! */
        this.loginAPI(account, verifyCookies);
        if (verifyCookies) {
            br.getPage("https://" + this.getHost());
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        /* This should never get called. */
        throw new AccountRequiredException();
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return LinkSnappyComConfig.class;
    }

    public static interface LinkSnappyComConfig extends PluginConfigInterface {
        final String                    text_ClearDownloadHistoryEnabled = "Clear download history after each successful download?";
        final String                    text_SSLDownloadsEnabledEnabled  = "Enable SSL Downloads?";
        public static final TRANSLATION TRANSLATION                      = new TRANSLATION();

        public static class TRANSLATION {
            public String getClearDownloadHistoryEnabled_label() {
                return text_ClearDownloadHistoryEnabled;
            }

            public String getSSLDownloadsEnabled_label() {
                return text_SSLDownloadsEnabledEnabled;
            }
        }

        @AboutConfig
        @DefaultBooleanValue(false)
        @DescriptionForConfigEntry(text_ClearDownloadHistoryEnabled)
        boolean isClearDownloadHistoryEnabled();

        void setClearDownloadHistoryEnabled(boolean b);

        @AboutConfig
        @DefaultBooleanValue(false)
        @DescriptionForConfigEntry(text_SSLDownloadsEnabledEnabled)
        boolean isSSLDownloadsEnabled();

        void setSSLDownloadsEnabled(boolean b);
    }
}