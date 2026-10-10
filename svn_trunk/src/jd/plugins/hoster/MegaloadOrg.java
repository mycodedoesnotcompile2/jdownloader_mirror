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
import java.util.concurrent.TimeUnit;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.requests.PostRequest;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 53573 $", interfaceVersion = 3, names = {}, urls = {})
public class MegaloadOrg extends PluginForHost {
    /* API docs: "MEGALOAD Downloader API" (account/plan access to the Downloader API is required). */
    private static final String API_BASE                   = "https://megaload.org/API";
    /** The API rejects unsupported User-Agents/client versions with HTTP 426. */
    private static final String API_USER_AGENT             = "MEGALOAD-Desktop/2.1.8";
    private static final String PROPERTY_ACCOUNT_TOKEN     = "megaloadorg_token";
    private static final String PROPERTY_ACCOUNT_RESUME    = "megaloadorg_resume";
    private static final String PROPERTY_ACCOUNT_CHUNKS    = "megaloadorg_chunks";
    private static final String PROPERTY_ACCOUNT_WAIT_SECS = "megaloadorg_wait_seconds";
    /** Public file GUID. Set for short links (s.ashx?c=...) once they have been resolved. */
    private static final String PROPERTY_FILE_GUID         = "megaloadorg_guid";
    /** Boolean property, set to true for files with availability "restricted" (Premium access required). */
    private static final String PROPERTY_PREMIUMONLY       = "megaloadorg_premiumonly";

    public MegaloadOrg(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/Plans.aspx");
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "megaload.org" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            /* Group 1: public file GUID, group 2: short code which redirects to the file link. */
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:File\\.aspx\\?id=([a-fA-F0-9\\-]{36})|s\\.ashx\\?c=([A-Za-z0-9]+))");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("Accept", "application/json");
        br.getHeaders().put("User-Agent", API_USER_AGENT);
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        /* Resume is only allowed if the account permissions say so (see fetchAccountInfo). */
        if (account == null) {
            return false;
        }
        return account.getBooleanProperty(PROPERTY_ACCOUNT_RESUME, false);
    }

    public int getMaxChunks(final Account account) {
        if (account == null) {
            return 1;
        }
        /* 0 = maximum possible number of chunks, negative values = up to N chunks. */
        return account.getIntegerProperty(PROPERTY_ACCOUNT_CHUNKS, 1);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String guid = getFile_GUID(link);
        if (guid != null) {
            return this.getHost() + "://" + guid;
        }
        final String shortCode = getShortCode(link);
        if (shortCode != null) {
            return this.getHost() + "://s/" + shortCode;
        }
        return super.getLinkID(link);
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        final String guid = urlinfo.getMatch(0);
        return guid != null ? guid : urlinfo.getMatch(1);
    }

    private String getFile_GUID(final DownloadLink link) {
        final String storedGuid = link.getStringProperty(PROPERTY_FILE_GUID);
        if (storedGuid != null) {
            return storedGuid;
        }
        final String guid = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
        if (guid != null) {
            return guid.toLowerCase(Locale.ROOT);
        }
        return null;
    }

    private String getShortCode(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(1);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        return requestFileInformation(link, account);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        this.setBrowserExclusive();
        if (account == null) {
            /* The file check API requires authentication. */
            return AvailableStatus.UNCHECKABLE;
        }
        final String fid = StringUtils.firstNotEmpty(getFile_GUID(link), getShortCode(link));
        if (fid == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Map<String, Object> entries = apiRequest(account, link, API_BASE + "/DesktopFileInfo.ashx?id=" + fid, null);
        if (!link.hasProperty(PROPERTY_FILE_GUID)) {
            /* Obtain long uuid from API. We need that later for downloading, especially if the added link only contained a short uuid. */
            link.setProperty(PROPERTY_FILE_GUID, StringUtils.firstNotEmpty(StringUtils.valueOrEmpty((String) entries.get("fileId")).toLowerCase(Locale.ROOT), getFile_GUID(link)));
        }
        /* name and size are not always present. */
        final String name = (String) entries.get("name");
        if (!StringUtils.isEmpty(name)) {
            link.setFinalFileName(name);
        }
        final Object size = entries.get("size");
        if (size != null) {
            link.setVerifiedFileSize(((Number) size).longValue());
        }
        final String availability = entries.get("availability").toString();
        link.removeProperty(PROPERTY_PREMIUMONLY);
        if ("online".equals(availability)) {
            return AvailableStatus.TRUE;
        } else if ("processing".equals(availability)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "File is still being processed", TimeUnit.MINUTES.toMillis(5));
        } else if ("restricted".equals(availability)) {
            link.setProperty(PROPERTY_PREMIUMONLY, true);
            return AvailableStatus.TRUE;
        } else {
            /* "unavailable": deleted, expired or inaccessible to the current account. */
            /*
             * e.g. {"contract":"desktop-file-info-1","available":false,"availability":"unavailable",
             * "message":"Deleted, expired or inaccessible file."}
             */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        throw new AccountRequiredException("An account with Downloader API access is required to download from this host");
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link, account);
        final String fileId = getFile_GUID(link);
        if (fileId == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (link.getBooleanProperty(PROPERTY_PREMIUMONLY, false) && AccountType.PREMIUM != account.getType()) {
            throw new AccountRequiredException("This file can only be downloaded with a premium account");
        }
        final int waitSeconds = account.getIntegerProperty(PROPERTY_ACCOUNT_WAIT_SECS, 0);
        if (waitSeconds > 0) {
            this.sleep(waitSeconds * 1000l, link);
        }
        /* Creating a ticket reserves traffic -> only do this when really downloading and never cache/reuse it. */
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("fileId", fileId);
        final Map<String, Object> ticket = apiRequest(account, link, API_BASE + "/Desktop.ashx?action=download_ticket", JSonStorage.serializeToJson(postdata));
        final String dllink = ticket.get("downloadUrl").toString();
        /* The download host may differ from the API host: br does not contain the Bearer token so it is not leaked. */
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, isResumeable(link, account), getMaxChunks(account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            try {
                br.followConnection(true);
            } catch (final IOException e) {
                logger.log(e);
            }
            checkDownloadErrors();
            super.handleConnectionErrors(br, dl.getConnection());
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl.startDownload();
    }

    private void checkDownloadErrors() throws PluginException {
        final int code = br.getHttpConnection().getResponseCode();
        switch (code) {
        case 401:
            /* Ticket rejected -> a new ticket will be requested on retry. */
            throw new PluginException(LinkStatus.ERROR_RETRY, "Download ticket rejected");
        case 403:
        case 404:
        case 429:
        case 503:
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error " + code, TimeUnit.MINUTES.toMillis(5));
        default:
            break;
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> profile = login(account, true);
        final AccountInfo ai = new AccountInfo();
        ai.setStatus("Plan: " + profile.get("planName"));
        if ("free".equalsIgnoreCase(profile.get("planCode").toString())) {
            account.setType(AccountType.FREE);
        } else {
            account.setType(AccountType.PREMIUM);
        }
        final int simultaneousDownloads = ((Number) profile.get("simultaneousDownloads")).intValue();
        account.setMaxSimultanDownloads(simultaneousDownloads > 0 ? simultaneousDownloads : -1);
        /* Without module "advanced_download" only a single connection without resume is allowed. */
        final Map<String, Object> modules = (Map<String, Object>) profile.get("modules");
        if (Boolean.TRUE.equals(modules.get("advanced_download"))) {
            final int maxThreads = ((Number) profile.get("maxThreadsPerDownload")).intValue();
            account.setProperty(PROPERTY_ACCOUNT_RESUME, Boolean.TRUE.equals(profile.get("supportsResume")));
            account.setProperty(PROPERTY_ACCOUNT_CHUNKS, maxThreads > 1 ? -maxThreads : (maxThreads == 1 ? 1 : 0));
        } else {
            account.setProperty(PROPERTY_ACCOUNT_RESUME, false);
            account.setProperty(PROPERTY_ACCOUNT_CHUNKS, 1);
        }
        account.setProperty(PROPERTY_ACCOUNT_WAIT_SECS, ((Number) profile.get("waitSeconds")).intValue());
        if (Boolean.TRUE.equals(profile.get("dailyTrafficUnlimited"))) {
            ai.setUnlimitedTraffic();
        } else {
            ai.setTrafficMax(((Number) profile.get("dailyTrafficLimitBytes")).longValue());
            ai.setTrafficLeft(Math.max(0, ((Number) profile.get("dailyTrafficRemainingBytes")).longValue()));
        }
        if (Boolean.TRUE.equals(profile.get("downloadBlocked"))) {
            /* restrictionReason is optional. */
            final Object reason = profile.get("restrictionReason");
            throw new AccountUnavailableException(reason != null ? reason.toString() : "Downloads are currently blocked for this account", TimeUnit.MINUTES.toMillis(30));
        }
        return ai;
    }

    /**
     * Makes sure a token is stored and returns the profile map. </br>
     * validate=false: Re-use a stored token without any request (returns null) or login if there is none. </br>
     * validate=true: Check the stored token via action=profile and login again if it is no longer valid.
     */
    @SuppressWarnings("unchecked")
    private Map<String, Object> login(final Account account, final boolean validate) throws Exception {
        synchronized (account) {
            final String storedToken = getAccountToken(account);
            if (storedToken != null) {
                if (!validate) {
                    return null;
                }
                final Browser brc = sendApiRequest(storedToken, API_BASE + "/Desktop.ashx?action=profile", null);
                if (brc.getHttpConnection().getResponseCode() != 401) {
                    return (Map<String, Object>) handleErrors(brc, account, null).get("profile");
                }
                logger.info("Token login failed -> performing full login");
                account.removeProperty(PROPERTY_ACCOUNT_TOKEN);
            }
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("identity", account.getUser());
            postdata.put("password", account.getPass());
            final Browser brc = sendApiRequest((String) null, API_BASE + "/Desktop.ashx?action=login", JSonStorage.serializeToJson(postdata));
            final Map<String, Object> entries = handleErrors(brc, account, null);
            account.setProperty(PROPERTY_ACCOUNT_TOKEN, entries.get("token").toString());
            return (Map<String, Object>) entries.get("profile");
        }
    }

    private Browser createAuthBrowser(final String token) {
        final Browser brc = br.cloneBrowser();
        if (token != null) {
            brc.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + token);
        }
        return brc;
    }

    /** Sends an authenticated API request (GET if postJson is null) and retries once with a fresh login if the token was rejected. */
    private Map<String, Object> apiRequest(final Account account, final DownloadLink link, final String url, final String postJson) throws Exception {
        login(account, false);
        Browser brc = sendApiRequest(account, url, postJson);
        if (brc.getHttpConnection().getResponseCode() == 401) {
            logger.info("Token rejected -> performing new login");
            account.removeProperty(PROPERTY_ACCOUNT_TOKEN);
            login(account, false);
            brc = sendApiRequest(account, url, postJson);
        }
        return handleErrors(brc, account, link);
    }

    private Browser sendApiRequest(final String token, final String url, final String postJson) throws Exception {
        final Browser brc = createAuthBrowser(token);
        if (postJson != null) {
            final PostRequest request = brc.createJSonPostRequest(url, postJson);
            brc.getPage(request);
        } else {
            brc.getPage(url);
        }
        return brc;
    }

    private Browser sendApiRequest(final Account account, final String url, final String postJson) throws Exception {
        return sendApiRequest(getAccountToken(account), url, postJson);
    }

    private String getAccountToken(final Account account) {
        return account.getStringProperty(PROPERTY_ACCOUNT_TOKEN, null);
    }

    /** Revokes the session server-side. Called once the account gets removed. */
    private void logout(final Account account) {
        synchronized (account) {
            final String token = getAccountToken(account);
            if (token == null) {
                return;
            }
            try {
                if (br == null) {
                    /* This plugin instance may already have gone through clean(). */
                    setBrowser(createNewBrowserInstance());
                }
                sendApiRequest(token, API_BASE + "/Desktop.ashx?action=logout", "{}");
                logger.info("Logout successful");
                account.removeProperty(PROPERTY_ACCOUNT_TOKEN);
            } catch (final Exception e) {
                logger.log(e);
                logger.warning("Logout failed");
            }
        }
    }

    @Override
    public void onAccountRemove(Account account) {
        logout(account);
    }

    /** Parses the JSON response and maps API/HTTP errors to the according exceptions. */
    private Map<String, Object> handleErrors(final Browser brc, final Account account, final DownloadLink link) throws Exception {
        final int code = brc.getHttpConnection().getResponseCode();
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException e) {
            /* Infrastructure errors may contain HTML -> must not be interpreted as invalid login or deleted file. */
            logger.log(e);
        }
        if (entries != null && code < 400) {
            return entries;
        }
        String msg = null;
        if (entries != null) {
            /* Prefer the human readable "message" over the "error" key. Error formats vary, so "error" is only used as fallback. */
            final Object message = entries.get("message");
            final Object error = entries.get("error");
            if (message instanceof String && !StringUtils.isEmpty((String) message)) {
                msg = (String) message;
            } else if (error instanceof String) {
                msg = (String) error;
            }
        }
        if (StringUtils.isEmpty(msg)) {
            msg = "HTTP error " + code;
        }
        if (entries != null) {
            switch (code) {
            case 401:
                throw new AccountInvalidException(msg);
            case 403:
                if (link != null) {
                    throw new AccountUnavailableException(msg, TimeUnit.MINUTES.toMillis(15));
                } else {
                    /* Access restriction or no Downloader API access with this plan. */
                    throw new AccountInvalidException(msg);
                }
            case 404:
                if (link != null) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                break;
            case 426:
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported client version: " + msg);
            default:
                break;
            }
        }
        /* 400, 429, 503, HTML error pages and everything else. */
        if (link != null) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, TimeUnit.MINUTES.toMillis(5));
        } else {
            throw new AccountUnavailableException(msg, TimeUnit.MINUTES.toMillis(5));
        }
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            /* Downloads always require an account. */
            return false;
        }
        if (link.getBooleanProperty(PROPERTY_PREMIUMONLY, false) && !AccountType.PREMIUM.is(account)) {
            return false;
        }
        return super.canHandle(link, account);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}
