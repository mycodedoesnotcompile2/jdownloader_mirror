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
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;

import org.appwork.exceptions.WTFException;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Application;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
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

@HostPlugin(revision = "$Revision: 53530 $", interfaceVersion = 3, names = { "dailyleech.com" }, urls = { "" })
public class DailyleechCom extends PluginForHost {
    /** DailyLeech Link API v1, see DAILYLEECH_LINK_API.md */
    private static final String          API_BASE                   = "https://dailyleech.com/v2/api/v1/";
    private static final String          HOSTS_URL                  = "https://dailyleech.com/v2/api/hosts.php";
    /** Page where the member finds their API Username and API key. */
    private static final String          JDOWNLOADER_LOGIN_HELP_URL = "https://dailyleech.com/v2/jdownloader";
    /** This is the old project of proleech.link owner */
    private static MultiHosterManagement mhm                        = new MultiHosterManagement("dailyleech.com");
    /** Last file.php download URL for a link (short-lived, attempted first before regenerating). */
    private static final String          PROPERTY_DIRECTURL         = "dailyleechcom_directurl";
    /** file.php token of a finished link, used to request a fresh download URL without resubmitting. */
    private static final String          PROPERTY_FILE_TOKEN        = "dailyleechcom_file_token";
    /** job_id of an unfinished job, used to resume polling after a client restart instead of resubmitting. */
    private static final String          PROPERTY_JOB_ID            = "dailyleechcom_job_id";
    /** Per-link Idempotency-Key so a retried submit within 24h returns the original job instead of a second chat post. */
    private static final String          PROPERTY_IDEMPOTENCY       = "dailyleechcom_idempotency_key";

    public DailyleechCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(JDOWNLOADER_LOGIN_HELP_URL);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "JDownloader " + getVersion());
        return br;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final Account account) {
        return -10;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/cbox/terms.php";
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
        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        if (!attemptStoredDownloadurlDownload(link, account)) {
            final String dllink = getDllink(link, account);
            if (StringUtils.isEmpty(dllink)) {
                /* This should never happen */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            link.setProperty(PROPERTY_DIRECTURL, dllink);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                mhm.handleErrorGeneric(account, link, "Unknown download error", 50, 5 * 60 * 1000l);
            }
        }
        this.dl.startDownload();
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final Account account) throws Exception {
        final String url = link.getStringProperty(PROPERTY_DIRECTURL);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        boolean valid = false;
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, this.isResumeable(link, account), this.getMaxChunks(account));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                valid = true;
                return true;
            } else {
                link.removeProperty(PROPERTY_DIRECTURL);
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            logger.log(e);
            return false;
        } finally {
            if (!valid) {
                try {
                    dl.getConnection().disconnect();
                } catch (final Throwable ignore) {
                }
                dl = null;
            }
        }
    }

    /**
     * Returns a fresh download URL for the given link: re-uses a stored file token if possible, otherwise submits the link and polls the
     * resulting job until the bot has generated the file.
     */
    private String getDllink(final DownloadLink link, final Account account) throws Exception {
        final String storedToken = link.getStringProperty(PROPERTY_FILE_TOKEN);
        if (storedToken != null) {
            final String url = requestFileURL(account, link, storedToken);
            if (url != null) {
                return url;
            }
            logger.info("Stored file token expired -> Regenerating");
            link.removeProperty(PROPERTY_FILE_TOKEN);
        }
        final String token = generateFileToken(link, account);
        link.setProperty(PROPERTY_FILE_TOKEN, token);
        final String url = requestFileURL(account, link, token);
        if (StringUtils.isEmpty(url)) {
            /* Should not happen. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return url;
    }

    /** Exchanges a file token for the real download URL via file.php. Returns null when the token has expired (410 EXPIRED). */
    private String requestFileURL(final Account account, final DownloadLink link, final String token) throws Exception {
        final Map<String, Object> params = new HashMap<String, Object>();
        params.put("token", token);
        final Map<String, Object> resp = callAPI(account, "file.php", params, null);
        if (!isOK(resp) && "EXPIRED".equals(resp.get("code"))) {
            return null;
        }
        final Map<String, Object> data = checkErrors(resp, account, link);
        return data.get("url").toString();
    }

    /** Submits the link (or resumes an already submitted job) and polls until the bot produced a file. Returns the file token. */
    private String generateFileToken(final DownloadLink link, final Account account) throws Exception {
        String jobID = link.getStringProperty(PROPERTY_JOB_ID);
        if (jobID == null) {
            jobID = submitLink(link, account);
            link.setProperty(PROPERTY_JOB_ID, jobID);
        } else {
            logger.info("Resuming existing job: " + jobID);
        }
        final Map<String, Object> jobdata = pollJob(account, link, jobID);
        /* Job has finished (done or failed) -> it will no longer be polled, so forget its id. */
        link.removeProperty(PROPERTY_JOB_ID);
        return handleJobResult(jobdata, account, link);
    }

    /** Posts a single link via submit.php and returns the created job_id. */
    private String submitLink(final DownloadLink link, final Account account) throws Exception {
        final String sourceurl = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
        String idempotencyKey = link.getStringProperty(PROPERTY_IDEMPOTENCY);
        if (idempotencyKey == null) {
            idempotencyKey = UUID.randomUUID().toString();
            link.setProperty(PROPERTY_IDEMPOTENCY, idempotencyKey);
        }
        final List<String> links = new ArrayList<String>();
        links.add(sourceurl);
        final Map<String, Object> params = new HashMap<String, Object>();
        params.put("links", links);
        final Map<String, Object> resp = callAPI(account, "submit.php", params, idempotencyKey);
        if (!isOK(resp)) {
            if ("NO_GOOD_LINK".equals(resp.get("code"))) {
                /* The single link we sent was rejected -> evaluate its per-link reason. */
                handleLinkError(getFirstLink(resp), null, account, link);
                /* Unreachable code */
                throw new WTFException();
            }
            /* Check for account related errors */
            checkErrors(resp, account, link);
            /* Unreachable code */
            throw new WTFException();
        }
        final Map<String, Object> data = (Map<String, Object>) resp.get("data");
        return data.get("job_id").toString();
    }

    /** Polls job.php until the job is done or failed and returns the job data object. */
    private Map<String, Object> pollJob(final Account account, final DownloadLink link, final String jobID) throws Exception {
        final long timeout = 20 * 60 * 1000l;
        final long startTime = System.currentTimeMillis();
        int waitSeconds = 5;
        final Map<String, Object> params = new HashMap<String, Object>();
        params.put("job_id", jobID);
        while (true) {
            this.sleep(waitSeconds * 1000l, link);
            final Map<String, Object> resp = callAPI(account, "job.php", params, null);
            final Map<String, Object> data = checkErrors(resp, account, link);
            final String state = data.get("state").toString();
            if ("done".equals(state) || "failed".equals(state)) {
                return data;
            }
            if (this.isAbort()) {
                throw new InterruptedException();
            } else if (System.currentTimeMillis() - startTime > timeout) {
                /*
                 * Give up for now but keep the job_id so a later retry resumes polling this same job instead of creating a second chat
                 * post.
                 */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Timeout while waiting for the bot to generate the file", 5 * 60 * 1000l);
            }
            final Object pollAfter = data.get("poll_after_seconds");
            if (pollAfter instanceof Number) {
                waitSeconds = ((Number) pollAfter).intValue();
            } else {
                waitSeconds = 5;
            }
            logger.info("Job state: " + state + " | Next poll in " + waitSeconds + "s");
        }
    }

    /** Evaluates a finished job and returns the file token of our link, or throws the matching error. */
    private String handleJobResult(final Map<String, Object> jobdata, final Account account, final DownloadLink link) throws Exception {
        final Map<String, Object> linkEntry = getFirstLink(jobdata);
        final String linkState = linkEntry.get("state").toString();
        if ("done".equals(linkState)) {
            final Map<String, Object> file = (Map<String, Object>) linkEntry.get("file");
            return file.get("token").toString();
        }
        /* Link did not produce a file -> map the failure. */
        handleLinkError(linkEntry, jobdata, account, link);
        /* Unreachable code */
        throw new WTFException();
    }

    /**
     * Maps a non-successful link entry (and optional job-level reason) to the matching JDownloader error. This method always throws.
     */
    private void handleLinkError(final Map<String, Object> linkEntry, final Map<String, Object> jobdata, final Account account, final DownloadLink link) throws Exception {
        final String linkState = linkEntry.get("state").toString();
        String code = (String) linkEntry.get("code");
        String message = (String) linkEntry.get("message");
        if (StringUtils.isEmpty(code) && jobdata != null) {
            /* Fall back to the job-level failure reason. */
            code = (String) jobdata.get("code");
            message = (String) jobdata.get("message");
        }
        final String display = !StringUtils.isEmpty(message) ? message : (!StringUtils.isEmpty(code) ? code : linkState);
        /* File is offline. */
        if ("dead".equals(linkState) || "DEAD_LINK".equals(code)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Host not supported. */
        if ("unsupported".equals(linkState) || "HOST_UNSUPPORTED".equals(code)) {
            mhm.putError(account, link, 5 * 60 * 1000l, "Host not supported: " + display);
            /* Unreachable code */
            throw new WTFException();
        }
        /* Daily per-host or fair-use limit reached. */
        if ("QUOTA".equals(code)) {
            mhm.putError(account, link, 10 * 60 * 1000l, display);
            /* Unreachable code */
            throw new WTFException();
        }
        /* Account state changed while the job was waiting. */
        if ("CHAT_BANNED".equals(code) || "FREE_ACCOUNT".equals(code) || "BLOCKED".equals(code) || "UNVERIFIED".equals(code) || "NO_ACCOUNT".equals(code)) {
            throw new AccountInvalidException(display);
        }
        /* Permanently broken link input. */
        if ("BAD_URL".equals(code) || "BAD_INPUT".equals(code) || "DUPLICATE".equals(code) || "BODY_GUARD".equals(code) || "BODY_TOO_LONG".equals(code)) {
            throw new PluginException(LinkStatus.ERROR_FATAL, display);
        }
        /*
         * Everything else (unchecked, expired, retryable bot/chat codes, and unknown finished codes) is treated as "failed, may retry
         * later". Retrying needs a NEW Idempotency-Key so the link is posted again.
         */
        link.removeProperty(PROPERTY_IDEMPOTENCY);
        mhm.handleErrorGeneric(account, link, display, 20, 5 * 60 * 1000l);
        /* Unreachable code */
        throw new WTFException();
    }

    private Map<String, Object> getFirstLink(final Map<String, Object> container) {
        final List<Map<String, Object>> links = (List<Map<String, Object>>) container.get("links");
        return links.get(0);
    }

    /**
     * Sends a POST request to the given API endpoint with the account credentials in the JSON body and returns the parsed response
     * envelope. Does not throw on API-level errors; inspect {@link #isOK(Map)} / call {@link #checkErrors}.
     */
    private Map<String, Object> callAPI(final Account account, final String endpoint, final Map<String, Object> params, final String idempotencyKey) throws Exception {
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("apiusername", account.getUser());
        postdata.put("apikey", account.getPass());
        if (params != null) {
            postdata.putAll(params);
        }
        if (idempotencyKey != null) {
            br.getHeaders().put("Idempotency-Key", idempotencyKey);
        }
        try {
            br.postPageRaw(API_BASE + endpoint, JSonStorage.serializeToJson(postdata));
        } finally {
            if (idempotencyKey != null) {
                br.getHeaders().remove("Idempotency-Key");
            }
        }
        try {
            return restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException e) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid API response", 1 * 60 * 1000l, e);
        }
    }

    private boolean isOK(final Map<String, Object> resp) {
        return Boolean.TRUE.equals(resp.get("ok"));
    }

    /** Returns the "data" object of a successful response, or throws the matching error for a request-level failure (API docs §7.1). */
    private Map<String, Object> checkErrors(final Map<String, Object> resp, final Account account, final DownloadLink link) throws Exception {
        if (isOK(resp)) {
            return (Map<String, Object>) resp.get("data");
        }
        final String code = resp.get("code").toString();
        final String message = (String) resp.get("message");
        final String display = !StringUtils.isEmpty(message) ? message : code;
        /* Invalid credentials -> open the page where the member can find the correct API Username and API key. */
        if ("AUTH_KEY".equals(code)) {
            if (!account.hasEverBeenValid() && CrossSystem.isOpenBrowserSupported() && !Application.isHeadless()) {
                CrossSystem.openURL(JDOWNLOADER_LOGIN_HELP_URL);
            }
            throw new AccountInvalidException(display);
        }
        /* Other permanent account errors. */
        if ("BLOCKED".equals(code) || "UNVERIFIED".equals(code) || "FREE_ACCOUNT".equals(code) || "CHAT_NAME_UNSUPPORTED".equals(code)) {
            throw new AccountInvalidException(display);
        }
        /* Temporary account errors. */
        if ("RATE".equals(code)) {
            throw new AccountUnavailableException(display, 10 * 60 * 1000l);
        } else if ("API_DISABLED".equals(code)) {
            throw new AccountUnavailableException(display, 30 * 60 * 1000l);
        }
        /* Too many unfinished jobs -> wait for a running job to finish, then retry. */
        if ("PENDING_LIMIT".equals(code)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, display, 1 * 60 * 1000l);
        }
        /* Temporary server-side problems. */
        if ("CONFIG".equals(code) || "UNAVAILABLE".equals(code) || "QUOTA_UNAVAILABLE".equals(code)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, display, 5 * 60 * 1000l);
        }
        /* The download link expired -> submit the source link again. */
        if ("EXPIRED".equals(code)) {
            throw new PluginException(LinkStatus.ERROR_RETRY, display);
        }
        /* Client-side bugs (wrong method, credentials in URL, bad/too large input, too many links, reused idempotency key). */
        if ("METHOD".equals(code) || "CREDENTIALS_IN_URL".equals(code) || "BAD_INPUT".equals(code) || "TOO_MANY_LINKS".equals(code) || "IDEMPOTENCY_CONFLICT".equals(code) || "NOT_FOUND".equals(code)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, code + ": " + display);
        }
        /* No link could be submitted -> evaluate the per-link reason. */
        if ("NO_GOOD_LINK".equals(code) && link != null) {
            handleLinkError(getFirstLink(resp), null, account, link);
            /* Unreachable code */
            throw new WTFException();
        }
        /* Fallback for unknown request errors. */
        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, code + ": " + display, 5 * 60 * 1000l);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> me = checkErrors(callAPI(account, "me.php", null, null), account, null);
        final boolean premium = Boolean.TRUE.equals(me.get("premium"));
        final String premiumUntil = (String) me.get("premium_until");
        if (premium) {
            account.setType(AccountType.PREMIUM);
            if (premiumUntil != null) {
                /* premium_until is given in Asia/Ho_Chi_Minh (UTC+7). */
                final long validUntil = TimeFormatter.getMilliSeconds(premiumUntil + " +0700", "yyyy-MM-dd HH:mm:ss Z", Locale.ENGLISH);
                if (validUntil > 0) {
                    ai.setValidUntil(validUntil, this.br);
                }
            }
        } else {
            account.setType(AccountType.FREE);
            ai.setExpired(true);
        }
        /* Status line. */
        final Map<String, Object> submit = (Map<String, Object>) me.get("submit");
        if (submit != null && !Boolean.TRUE.equals(submit.get("open"))) {
            final StringBuilder status = new StringBuilder();
            status.append(premium ? "Premium" : "Free");
            status.append(" | Submitting currently disabled");
            ai.setStatus(status.toString());
            ai.setTrafficLeft(0);
            return ai;
        }
        /* quota is null for a non-premium account or when usage could not be read. */
        final Map<String, Object> quota = (Map<String, Object>) me.get("quota");
        if (quota != null) {
            ai.setTrafficLeft(((Number) quota.get("traffic_left_bytes")).longValue());
        } else if (premium) {
            ai.setUnlimitedTraffic();
        } else {
            ai.setTrafficLeft(0);
        }
        /* Supported hosts: use the public hosts.php list and overlay today's per-host usage from me.php. */
        ai.setMultiHostSupportV2(this, buildSupportedHosts(account, quota));
        account.setConcurrentUsePossible(true);
        return ai;
    }

    private List<MultiHostHost> buildSupportedHosts(final Account account, final Map<String, Object> quota) throws Exception {
        /* Per-host usage today, keyed by host name. */
        final Map<String, Map<String, Object>> usedByHost = new HashMap<String, Map<String, Object>>();
        if (quota != null) {
            final Object hostsUsedObj = quota.get("hosts_used");
            if (hostsUsedObj instanceof List) {
                final List<Map<String, Object>> hostsUsed = (List<Map<String, Object>>) hostsUsedObj;
                for (final Map<String, Object> hostUsed : hostsUsed) {
                    usedByHost.put(hostUsed.get("host").toString(), hostUsed);
                }
            }
        }
        final Browser brc = br.cloneBrowser();
        brc.getPage(HOSTS_URL);
        final Map<String, Object> hostsResp = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!isOK(hostsResp)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find list of supported hosts");
        }
        final Map<String, Object> hostsData = (Map<String, Object>) hostsResp.get("data");
        final List<Map<String, Object>> hosts = (List<Map<String, Object>>) hostsData.get("hosts");
        final List<MultiHostHost> supportedHosts = new ArrayList<MultiHostHost>();
        for (final Map<String, Object> host : hosts) {
            final String domain = host.get("host").toString();
            final MultiHostHost mhost = new MultiHostHost(domain);
            if (!Boolean.TRUE.equals(host.get("online"))) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            final Map<String, Object> used = usedByHost.get(domain);
            /* Per-host daily traffic limit (null cap means no limit). */
            final Object capBytes = host.get("cap_bytes");
            if (capBytes instanceof Number) {
                final long trafficMax = ((Number) capBytes).longValue();
                long trafficUsed = 0;
                if (used != null) {
                    trafficUsed = ((Number) used.get("bytes_used")).longValue();
                }
                mhost.setTrafficLeftAndMax(Math.max(0, trafficMax - trafficUsed), trafficMax);
            }
            /* Per-host daily file/link limit (null cap means no limit). */
            final Object capFiles = host.get("cap_files");
            if (capFiles instanceof Number) {
                final long linksMax = ((Number) capFiles).longValue();
                long linksUsed = 0;
                if (used != null && used.get("files_used") != null) {
                    /* files_used may be null when the host has no file-count limit. */
                    linksUsed = ((Number) used.get("files_used")).longValue();
                }
                mhost.setLinksLeftAndMax(Math.max(0, linksMax - linksUsed), linksMax);
            }
            supportedHosts.add(mhost);
        }
        return supportedHosts;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return getMaxSimultanPremiumDownloadNum();
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        /*
         * The API allows at most 2 unfinished jobs per member (one job = one link here). A job only counts as unfinished while the bot is
         * generating the file, not during the actual file transfer, but keeping the simultaneous download count at 2 avoids running into
         * PENDING_LIMIT.
         */
        return 2;
    }
}
