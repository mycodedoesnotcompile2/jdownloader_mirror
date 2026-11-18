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
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.appwork.exceptions.WTFException;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
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
import jd.plugins.PluginConfigPanelNG;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

/**
 * Multihoster zapisz.se, uses the official API: https://zapisz.se/api
 */
@HostPlugin(revision = "$Revision: 54047 $", interfaceVersion = 3, names = { "zapisz.se" }, urls = { "https?://zapisz\\.se/files/(\\d+)/([^/]+)?/?" })
public class ZapiszSe extends PluginForHost {
    private static final String          PROPERTY_ACCOUNT_HAPPY_HOURS = "happy_hours";
    private static final String          WEBSITE_BASE                 = "https://zapisz.se";
    private static final String          API_BASE                     = WEBSITE_BASE + "/api/v1";
    private static final String          PROPERTY_FILE_ID             = "zapisz_file_id";
    private static final String          PROPERTY_ACCOUNT_MAX_CHUNKS  = "max_chunks";
    private static final String          PROPERTY_ACCOUNT_RESUME      = "resume";
    private static final String          PROPERTY_LINK_MAX_CHUNKS     = "zapisz_max_chunks";
    private static final String          PROPERTY_LINK_RESUME         = "zapisz_resume";
    /* Fallbacks until the account/link has been checked, API defaults: "chunks":1, "resume":false */
    private static final int             DEFAULT_MAX_CHUNKS           = 1;
    private static final boolean         DEFAULT_RESUME               = false;
    /* Max number of status checks while zapisz.se is still preparing a file (HTTP 409). */
    private static final int             MAX_PREPARE_CHECKS           = 60;
    private static final long            PREPARE_CHECK_INTERVAL       = TimeUnit.SECONDS.toMillis(3);
    private static MultiHosterManagement mhm                          = new MultiHosterManagement("zapisz.se");
    /**
     * Translation of API error codes into readable English messages. This is needed because the API unfortunately does not return any
     * localized/human readable error messages, only raw error codes.
     */
    private static final Map<String, String> ERROR_CODE_TRANSLATIONS = new HashMap<String, String>();
    static {
        ERROR_CODE_TRANSLATIONS.put("INVALID_API_KEY", "Invalid or revoked API key");
    }

    public ZapiszSe(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(WEBSITE_BASE + "/profile/api");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "JDownloader");
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT, "application/json");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return WEBSITE_BASE + "/terms.html";
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return "zapisz://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        /* Preferred: Value of "download.resume" of the /download response, stored on the link. */
        if (link != null && link.hasProperty(PROPERTY_LINK_RESUME)) {
            return link.getBooleanProperty(PROPERTY_LINK_RESUME, DEFAULT_RESUME);
        } else if (account != null) {
            /* Value of "download.resume" of /account, stored in fetchAccountInfo. */
            return account.getBooleanProperty(PROPERTY_ACCOUNT_RESUME, DEFAULT_RESUME);
        } else {
            return DEFAULT_RESUME;
        }
    }

    /* No @Override: looked up by the framework. */
    public int getMaxChunks(final DownloadLink link, final Account account) {
        /* Preferred: Value of "download.chunks" of the /download response, stored on the link. */
        if (link != null && link.hasProperty(PROPERTY_LINK_MAX_CHUNKS)) {
            return link.getIntegerProperty(PROPERTY_LINK_MAX_CHUNKS, DEFAULT_MAX_CHUNKS);
        } else if (account != null) {
            /* Value of "download.chunks" of /account, stored in fetchAccountInfo. */
            return account.getIntegerProperty(PROPERTY_ACCOUNT_MAX_CHUNKS, DEFAULT_MAX_CHUNKS);
        } else {
            return DEFAULT_MAX_CHUNKS;
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        /* Account required to download any content of this website. */
        throw new AccountRequiredException();
    }

    /** Download of a file which has already been added to the account of the user (zapisz.se/files/<id>). */
    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.setAuthHeader(account);
        this.downloadFile(link, account, Long.parseLong(this.getFID(link)), false);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        this.setAuthHeader(account);
        final long storedFileId = link.getLongProperty(PROPERTY_FILE_ID, -1);
        final long fileId;
        if (storedFileId != -1) {
            logger.info("Re-using stored fileID: " + storedFileId);
            fileId = storedFileId;
        } else {
            fileId = this.addLink(link, account);
            link.setProperty(PROPERTY_FILE_ID, fileId);
        }
        this.downloadFile(link, account, fileId, storedFileId != -1);
    }

    /** Adds the given link to the account and returns its file_id. */
    private long addLink(final DownloadLink link, final Account account) throws Exception {
        final Map<String, Object> postdata = new HashMap<String, Object>();
        final List<String> links = new ArrayList<String>();
        links.add(link.getDefaultPlugin().buildExternalDownloadURL(link, this));
        postdata.put("links", links);
        final Map<String, Object> resp;
        synchronized (account) {
            /* API: Only one batch per user at a time. */
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            /* Required header, 16-128 chars of [A-Za-z0-9_-]. A new key for an already added valid file does not charge transfer again. */
            brc.getHeaders().put("Idempotency-Key", UUID.randomUUID().toString().replace("-", ""));
            brc.postPageRaw(API_BASE + "/files", JSonStorage.serializeToJson(postdata));
            resp = this.checkErrors(brc, link, account);
        }
        final List<Map<String, Object>> results = (List<Map<String, Object>>) resp.get("results");
        final Map<String, Object> result = results.get(0);
        final String status = result.get("status").toString();
        if (status.equals("added")) {
            return ((Number) result.get("file_id")).longValue();
        }
        final String errorCode = result.get("error_code").toString();
        logger.info("Failed to add link | status: " + status + " | error_code: " + errorCode);
        if (errorCode.equals("HOSTING-NOT-SUPPORTED")) {
            mhm.putError(account, link, TimeUnit.MINUTES.toMillis(5), "Host not supported: " + errorCode);
            /* Unreachable code */
            throw new WTFException();
        } else if (errorCode.matches("(?i).*(TRANSFER|BALANCE).*")) {
            /* Error code for missing transfer is not documented -> Match by keyword. */
            throw new AccountUnavailableException("Not enough transfer left: " + errorCode, TimeUnit.MINUTES.toMillis(5));
        }
        mhm.handleErrorGeneric(account, link, errorCode, 3, TimeUnit.MINUTES.toMillis(3));
        /* Unreachable code */
        throw new WTFException();
    }

    private void downloadFile(final DownloadLink link, final Account account, final long fileId, final boolean fileIdWasStored) throws Exception {
        String downloadurl = null;
        for (int i = 0; i < MAX_PREPARE_CHECKS; i++) {
            final Browser brc = br.cloneBrowser();
            brc.postPageRaw(API_BASE + "/files/" + fileId + "/download", "");
            final int responsecode = brc.getHttpConnection().getResponseCode();
            if (responsecode == 409) {
                /* File is not ready yet (or operation is still running) */
                logger.info("File is not ready yet, check " + (i + 1) + "/" + MAX_PREPARE_CHECKS);
                this.sleep(PREPARE_CHECK_INTERVAL, link);
                continue;
            } else if (responsecode == 404 || responsecode == 410) {
                this.handleFileGone(link, account, fileId, fileIdWasStored, "HTTP " + responsecode);
            }
            final Map<String, Object> resp = this.checkErrors(brc, link, account);
            final Map<String, Object> download = (Map<String, Object>) resp.get("download");
            downloadurl = download.get("url").toString();
            /* Connection limits of this file, preferred over the account values. */
            link.setProperty(PROPERTY_LINK_MAX_CHUNKS, ((Number) download.get("chunks")).intValue());
            link.setProperty(PROPERTY_LINK_RESUME, ((Boolean) download.get("resume")).booleanValue());
            break;
        }
        if (downloadurl == null) {
            mhm.handleErrorGeneric(account, link, "File was not prepared in time", 5, TimeUnit.MINUTES.toMillis(3));
            /* Unreachable code */
            throw new WTFException();
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, downloadurl, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            final int responsecode = dl.getConnection().getResponseCode();
            br.followConnection(true);
            switch (responsecode) {
            case 401:
            case 403:
                throw new AccountInvalidException("API access denied");
            case 404:
            case 410:
                this.handleFileGone(link, account, fileId, fileIdWasStored, "HTTP " + responsecode);
                break;
            case 429:
                throw new AccountUnavailableException("Rate limit reached", this.getRetryAfterMillis(br));
            default:
                mhm.handleErrorGeneric(account, link, "Unknown download error: HTTP " + responsecode, 20, TimeUnit.MINUTES.toMillis(5));
                break;
            }
        }
        dl.startDownload();
    }

    /** Called if the file on zapisz.se does not exist anymore or expired. */
    private void handleFileGone(final DownloadLink link, final Account account, final long fileId, final boolean fileIdWasStored, final String reason) throws Exception {
        if (fileIdWasStored) {
            link.removeProperty(PROPERTY_FILE_ID);
            throw new PluginException(LinkStatus.ERROR_RETRY, "Stored fileID expired: " + reason);
        } else if (getFID(link) != null) {
            /* Direct (selfhosted) zapisz.se link -> Nothing we can do. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, reason);
        }
        mhm.handleErrorGeneric(account, link, "File gone directly after adding: " + reason, 5, TimeUnit.MINUTES.toMillis(3));
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        this.setAuthHeader(account);
        br.getPage(API_BASE + "/account");
        final Map<String, Object> accountResp = this.checkErrors(br, null, account);
        final Map<String, Object> accountmap = (Map<String, Object>) accountResp.get("account");
        final Map<String, Object> limits = (Map<String, Object>) accountResp.get("limits");
        /* Make the account identifiable in the GUI (user enters API key only) */
        account.setUser(accountmap.get("username").toString());
        account.setType(AccountType.PREMIUM);
        account.setMaxSimultanDownloads(((Number) limits.get("parallel_downloads")).intValue());
        final Map<String, Object> downloadinfo = (Map<String, Object>) accountResp.get("download");
        account.setProperty(PROPERTY_ACCOUNT_MAX_CHUNKS, ((Number) downloadinfo.get("chunks")).intValue());
        account.setProperty(PROPERTY_ACCOUNT_RESUME, ((Boolean) downloadinfo.get("resume")).booleanValue());
        account.setConcurrentUsePossible(true);
        ai.setTrafficLeft(((Number) accountmap.get("transfer_balance_bytes")).longValue());
        /* Supported hosts */
        br.getPage(API_BASE + "/hosts");
        final Map<String, Object> hostsResp = this.checkErrors(br, null, account);
        final List<Map<String, Object>> hosts = (List<Map<String, Object>>) hostsResp.get("hosts");
        final Map<String, Object> happyHours = (Map<String, Object>) hostsResp.get("happy_hours");
        final List<String> happyHourExcludedKeys = (List<String>) happyHours.get("excluded");
        final List<String> happyHourHostNames = new ArrayList<String>();
        final List<String> unavailableHostNames = new ArrayList<String>();
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (final Map<String, Object> host : hosts) {
            final boolean isHappyHourHost = !happyHourExcludedKeys.contains(host.get("key").toString());
            if (isHappyHourHost) {
                /* Hosts which are not excluded from happy hours */
                happyHourHostNames.add(host.get("label").toString());
            }
            final MultiHostHost mhost = new MultiHostHost();
            if (isHappyHourHost) {
                mhost.setStatusText("Participates in happy hour " + happyHours.get("start_time") + "-" + happyHours.get("end_time") + " (" + happyHours.get("timezone") + ")");
            }
            mhost.setName(host.get("label").toString());
            for (final String domain : (List<String>) host.get("domains")) {
                mhost.addDomain(domain);
            }
            /* Transfer multiplier: 200 = 1 GB file costs 2 GB transfer. */
            mhost.setTrafficCalculationFactorPercent(((Number) host.get("transfer_percent")).shortValue());
            if (!Boolean.TRUE.equals(host.get("enabled")) || !Boolean.TRUE.equals(host.get("active"))) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                unavailableHostNames.add(host.get("label").toString());
            }
            supportedhosts.add(mhost);
        }
        logger.info("Happy hour active: " + isHappyHourNow(happyHours.get("start_time").toString(), happyHours.get("end_time").toString(), happyHours.get("timezone").toString()) + " | " + happyHours.get("start_time") + "-" + happyHours.get("end_time") + " " + happyHours.get("timezone") + " | Happy hour hosts " + happyHourHostNames.size() + "/" + hosts.size() + ": " + StringUtils.join(happyHourHostNames, ", "));
        logger.info("Currently unavailable hosts " + unavailableHostNames.size() + "/" + hosts.size() + ": " + StringUtils.join(unavailableHostNames, ", "));
        /* Store happy hour info as json so it can be displayed via extendAccountSettingsPanel. */
        final Map<String, Object> happyHourInfo = new LinkedHashMap<String, Object>();
        happyHourInfo.put("start_time", happyHours.get("start_time"));
        happyHourInfo.put("end_time", happyHours.get("end_time"));
        happyHourInfo.put("timezone", happyHours.get("timezone"));
        happyHourInfo.put("hosts", happyHourHostNames);
        account.setProperty(PROPERTY_ACCOUNT_HAPPY_HOURS, JSonStorage.serializeToJson(happyHourInfo));
        ai.setMultiHostSupportV2(this, supportedhosts);
        return ai;
    }

    @Override
    public void extendAccountSettingsPanel(final Account account, final PluginConfigPanelNG panel) {
        super.extendAccountSettingsPanel(account, panel);
        final String happyHoursJson = account.getStringProperty(PROPERTY_ACCOUNT_HAPPY_HOURS);
        if (happyHoursJson == null) {
            /* Account has not been checked yet. */
            return;
        }
        final Map<String, Object> happyHourInfo;
        try {
            happyHourInfo = JSonStorage.restoreFromString(happyHoursJson, TypeRef.MAP);
        } catch (final Throwable e) {
            logger.log(e);
            return;
        }
        final String startTime = happyHourInfo.get("start_time").toString();
        final String endTime = happyHourInfo.get("end_time").toString();
        final String timezone = happyHourInfo.get("timezone").toString();
        final List<String> hostNames = (List<String>) happyHourInfo.get("hosts");
        panel.addHeader("Happy hour", new AbstractIcon(IconKey.ICON_PREMIUM, 18));
        panel.addStringPair("Time:", startTime + " - " + endTime + " (" + timezone + ")");
        panel.addStringPair("Happy hour in:", getHappyHourCountdown(startTime, endTime, timezone));
        panel.addStringPair("Hosts included (" + hostNames.size() + "):", StringUtils.join(hostNames, ", "));
    }

    /** Returns the time until the next happy hour starts or "Now for <time until it ends>" if it is running right now. */
    private static String getHappyHourCountdown(final String startTime, final String endTime, final String timezone) {
        final Calendar now = Calendar.getInstance(TimeZone.getTimeZone(timezone));
        final long dayMillis = TimeUnit.DAYS.toMillis(1);
        final long nowMillis = TimeUnit.HOURS.toMillis(now.get(Calendar.HOUR_OF_DAY)) + TimeUnit.MINUTES.toMillis(now.get(Calendar.MINUTE)) + TimeUnit.SECONDS.toMillis(now.get(Calendar.SECOND));
        if (isHappyHourNow(startTime, endTime, timezone)) {
            final long untilEnd = ((TimeUnit.MINUTES.toMillis(parseMinutesOfDay(endTime)) - nowMillis) % dayMillis + dayMillis) % dayMillis;
            return "Now for " + TimeFormatter.formatMilliSeconds(untilEnd, TimeFormatter.HIDE_SECONDS);
        } else {
            final long untilStart = ((TimeUnit.MINUTES.toMillis(parseMinutesOfDay(startTime)) - nowMillis) % dayMillis + dayMillis) % dayMillis;
            return TimeFormatter.formatMilliSeconds(untilStart, TimeFormatter.HIDE_SECONDS);
        }
    }

    /** Evaluated at display time since the "active" flag of the API is only a snapshot of the last account check. */
    private static boolean isHappyHourNow(final String startTime, final String endTime, final String timezone) {
        final Calendar now = Calendar.getInstance(TimeZone.getTimeZone(timezone));
        final int nowMinutes = now.get(Calendar.HOUR_OF_DAY) * 60 + now.get(Calendar.MINUTE);
        final int startMinutes = parseMinutesOfDay(startTime);
        final int endMinutes = parseMinutesOfDay(endTime);
        if (startMinutes <= endMinutes) {
            return nowMinutes >= startMinutes && nowMinutes < endMinutes;
        } else {
            /* Time window spans midnight */
            return nowMinutes >= startMinutes || nowMinutes < endMinutes;
        }
    }

    /** Converts "HH:mm" to minutes of the day. */
    private static int parseMinutesOfDay(final String time) {
        final String[] parts = time.split(":");
        return Integer.parseInt(parts[0]) * 60 + Integer.parseInt(parts[1]);
    }

    private void setAuthHeader(final Account account) {
        /* The password field holds the API key. */
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + account.getPass());
    }

    private long getRetryAfterMillis(final Browser br) {
        final String retryAfter = br.getRequest().getResponseHeader(HTTPConstants.HEADER_RESPONSE_RETRY_AFTER);
        if (retryAfter != null && retryAfter.matches("\\d+")) {
            return TimeUnit.SECONDS.toMillis(Long.parseLong(retryAfter));
        } else {
            return TimeUnit.MINUTES.toMillis(1);
        }
    }

    /** Parses the API response and handles errors which are not file specific. link == null: Account check. */
    private Map<String, Object> checkErrors(final Browser br, final DownloadLink link, final Account account) throws PluginException, InterruptedException {
        final int responsecode = br.getHttpConnection().getResponseCode();
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException e) {
            throw new AccountUnavailableException("Invalid API response | HTTP " + responsecode, TimeUnit.MINUTES.toMillis(1));
        }
        final Map<String, Object> error = (Map<String, Object>) entries.get("error");
        if (error == null) {
            return entries;
        }
        final String code = error.get("code").toString();
        switch (responsecode) {
        case 401:
        case 403:
            throw new AccountInvalidException(this.translateErrorCode(code));
        case 429:
            throw new AccountUnavailableException("Rate limit reached: " + code, this.getRetryAfterMillis(br));
        case 503:
            throw new AccountUnavailableException("API or service disabled: " + code, TimeUnit.MINUTES.toMillis(5));
        default:
            break;
        }
        if (link == null) {
            throw new AccountUnavailableException("API error: " + this.translateErrorCode(code) + " | HTTP " + responsecode, TimeUnit.MINUTES.toMillis(5));
        }
        mhm.handleErrorGeneric(account, link, "API error: " + this.translateErrorCode(code) + " | HTTP " + responsecode, 3, TimeUnit.MINUTES.toMillis(3));
        /* Unreachable code */
        throw new WTFException();
    }

    /** Returns a readable message for the given API error code or the raw code if no translation is known. */
    private String translateErrorCode(final String code) {
        final String translation = ERROR_CODE_TRANSLATIONS.get(code);
        if (translation != null) {
            return translation;
        } else {
            return code;
        }
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/profile/api";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("za_[a-f0-9]{64}")) {
            return true;
        } else {
            return false;
        }
    }
}
