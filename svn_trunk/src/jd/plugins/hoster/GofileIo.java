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
import java.util.Currency;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.http.requests.GetRequest;
import jd.http.requests.PostRequest;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.GoFileIoCrawler;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.net.HTTPHeader;
import org.jdownloader.plugins.components.config.GofileIoConfig;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@HostPlugin(revision = "$Revision: 50911 $", interfaceVersion = 3, names = { "gofile.io" }, urls = { "" })
public class GofileIo extends PluginForHost {
    public GofileIo(PluginWrapper wrapper) {
        super(wrapper);
        enablePremium("https://" + getHost() + "/premium");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static final String COOKIE_ACCOUNT_TOKEN = "accountToken";

    @Override
    public FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        return str != null && str.matches("^[a-zA-Z0-9]{32}$");
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/";
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/myprofile";
    }

    private String getAPIBase() {
        return "https://api.gofile.io";
    }

    private String getFolderIDFromURL(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), "(?:c=|/d/)([A-Za-z0-9]+)").getMatch(0);
    }

    private String getShortFileIDFromURL(final DownloadLink link) throws PluginException {
        return new Regex(link.getPluginPatternMatcher(), "#file=([a-f0-9]+)").getMatch(0);
    }

    @Override
    public void init() {
        Browser.setRequestIntervalLimitGlobal(getHost(), 500);
    }

    /* Connection stuff */
    private static final String        PROPERTY_DANGEROUS_FILE         = "dangerous_file";
    private static final String        PROPERTY_DIRECTURL              = "directurl";
    private static final String        PROPERTY_INTERNAL_FILEID        = "internal_fileid";
    private static final String        PROPERTY_PARENT_FOLDER_ID       = "parent_folder_id";
    public static final String         PROPERTY_PARENT_FOLDER_SHORT_ID = "parent_folder_short_id";
    /* Don't touch the following! */
    private static final AtomicInteger freeRunning                     = new AtomicInteger(0);

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        /* 2025-03-28: Looks to be the same limit for free and premium users. */
        return -3;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return this.requestFileInformation(link, null);
    }

    protected static AtomicReference<String> GUEST_TOKEN             = new AtomicReference<String>();
    protected static AtomicLong              TOKEN_TIMESTAMP         = new AtomicLong(-1);
    protected final static long              TOKEN_EXPIRE            = 30 * 60 * 1000l;
    protected static AtomicReference<String> WEBSITE_TOKEN           = new AtomicReference<String>();
    protected static AtomicLong              WEBSITE_TOKEN_TIMESTAMP = new AtomicLong(-1);

    public static String getWebsiteToken(final Plugin plugin, final Browser br) throws Exception {
        synchronized (WEBSITE_TOKEN) {
            String token = WEBSITE_TOKEN.get();
            if (!StringUtils.isEmpty(token) && Time.systemIndependentCurrentJVMTimeMillis() - WEBSITE_TOKEN_TIMESTAMP.get() < TOKEN_EXPIRE) {
                return token;
            } else {
                final Browser brc = br.cloneBrowser();
                final GetRequest req = brc.createGetRequest("https://" + plugin.getHost() + "/dist/js/global.js");
                GofileIo.getPage(plugin, brc, req);
                token = brc.getRegex("websiteToken\\s*(?::|=)\\s*\"(.*?)\"").getMatch(0);
                if (token == null) {
                    /* 2024-01-26 / 2024-12-03 */
                    token = brc.getRegex("(?:fetchData|appdata)\\.wt\\s*(?::|=)\\s*\"(.*?)\"").getMatch(0);
                    if (token == null) {
                        /* 2024-03-11 */
                        token = brc.getRegex("wt\\s*:\\s*\"([^\"]+)").getMatch(0);
                    }
                }
                if (StringUtils.isEmpty(token)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                WEBSITE_TOKEN.set(token);
                WEBSITE_TOKEN_TIMESTAMP.set(Time.systemIndependentCurrentJVMTimeMillis());
                return token;
            }
        }
    }

    public static Request getPage(final Plugin plugin, final Browser br, final Request request) throws IOException, PluginException, DecrypterRetryException {
        final int retrySeconds = 5;
        final int maxtries = 5;
        for (int i = 0; i <= maxtries; i++) {
            final URLConnectionAdapter con = br.openRequestConnection(request);
            try {
                if (con.getResponseCode() == 429) {
                    br.followConnection(true);
                    if (plugin instanceof PluginForDecrypt) {
                        final PluginForDecrypt pluginForDecrypt = (PluginForDecrypt) plugin;
                        pluginForDecrypt.sleep(retrySeconds * 1000, pluginForDecrypt.getCurrentLink().getCryptedLink());
                    } else {
                        Thread.sleep(retrySeconds * 1000);
                    }
                    request.resetConnection();
                    continue;
                } else if (con.getResponseCode() == 200) {
                    br.followConnection();
                    return request;
                } else {
                    br.followConnection(true);
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            } catch (final InterruptedException e) {
                if (plugin instanceof PluginForHost) {
                    throw new PluginException(LinkStatus.ERROR_RETRY, null, e);
                } else {
                    throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT, null, null, e);
                }
            } finally {
                con.disconnect();
            }
        }
        if (plugin instanceof PluginForHost) {
            throw new PluginException(LinkStatus.ERROR_RETRY);
        } else {
            throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT);
        }
    }

    public static String getAndSetGuestToken(final Plugin plugin, final Browser br) throws Exception {
        synchronized (GUEST_TOKEN) {
            final String existingToken = GUEST_TOKEN.get();
            String token = null;
            if (!StringUtils.isEmpty(existingToken) && Time.systemIndependentCurrentJVMTimeMillis() - TOKEN_TIMESTAMP.get() < TOKEN_EXPIRE) {
                /* Re-use existing token */
                token = existingToken;
            } else {
                final Browser brc = br.cloneBrowser();
                final boolean usePOSTRequest = true;
                if (usePOSTRequest) {
                    /* 2024-03-11: New */
                    final PostRequest req = brc.createJSonPostRequest("https://api." + plugin.getHost() + "/accounts", new HashMap<String, Object>());
                    req.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://" + plugin.getHost()));
                    req.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_REFERER, "https://" + plugin.getHost()));
                    GofileIo.getPage(plugin, brc, req);
                } else {
                    final GetRequest req = brc.createGetRequest("https://api." + plugin.getHost() + "/createAccount");
                    req.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://" + plugin.getHost()));
                    req.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_REFERER, "https://" + plugin.getHost()));
                    GofileIo.getPage(plugin, brc, req);
                }
                final Map<String, Object> response = JSonStorage.restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                if (!"ok".equalsIgnoreCase(response.get("status").toString())) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                token = JavaScriptEngineFactory.walkJson(response, "data/token").toString();
                if (StringUtils.isEmpty(token)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                GUEST_TOKEN.set(token);
                TOKEN_TIMESTAMP.set(Time.systemIndependentCurrentJVMTimeMillis());
            }
            br.setCookie(plugin.getHost(), COOKIE_ACCOUNT_TOKEN, token);
            return token;
        }
    }

    @Override
    public String getPluginContentURL(final DownloadLink link) {
        final String parentFolderShortID = link.getStringProperty(PROPERTY_PARENT_FOLDER_SHORT_ID);
        final String parentFolderID = link.getStringProperty(PROPERTY_PARENT_FOLDER_ID);
        if (parentFolderShortID != null) {
            return "https://" + getHost() + "/d/" + parentFolderShortID;
        } else if (parentFolderID != null) {
            /* Link to next folder which contains this file */
            return "https://" + getHost() + "/d/" + parentFolderID;
        } else {
            return super.getPluginContentURL(link);
        }
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        this.setBrowserExclusive();
        if (account != null) {
            this.setLoginCookiesAPI(br, account);
        } else {
            /* 2021-11-30: Token cookie is even needed to check directURLs! */
            getAndSetGuestToken(this, br);
        }
        final boolean allowDirecturlLinkcheck = true;
        if (allowDirecturlLinkcheck && this.checkDirectLink(link, account) != null) {
            logger.info("Availablecheck via directurl complete");
            return AvailableStatus.TRUE;
        }
        final String folderID = getFolderIDFromURL(link);
        if (folderID == null) {
            /* This should never happen! */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Re-crawl folder item in order to obtain a fresh direct-downloadable URL. */
        final GoFileIoCrawler crawler = (GoFileIoCrawler) this.getNewPluginForDecryptInstance(this.getHost());
        final CryptedLink cl = new CryptedLink(this.getPluginContentURL(link));
        /* Make sure that user is not asked for password again if we already know it. */
        cl.setDecrypterPassword(link.getDownloadPassword());
        final ArrayList<DownloadLink> crawlerResults = crawler.decryptIt(cl, null);
        DownloadLink freshLink = null;
        for (final DownloadLink result : crawlerResults) {
            if (StringUtils.equals(this.getLinkID(link), this.getLinkID(result))) {
                freshLink = result;
                break;
            }
        }
        if (freshLink == null) {
            /* File must have been deleted from folder */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Set property of fresh directurl. */
        link.setProperty(PROPERTY_DIRECTURL, freshLink.getProperty(PROPERTY_DIRECTURL));
        /* Set password we got from our crawler results just in case it has changed. */
        if (freshLink.getDownloadPassword() != null) {
            link.setDownloadPassword(freshLink.getDownloadPassword());
        }
        return AvailableStatus.TRUE;
    }

    private String checkDirectLink(final DownloadLink link, final Account account) {
        String dllink = link.getStringProperty(PROPERTY_DIRECTURL);
        if (dllink == null) {
            return null;
        }
        URLConnectionAdapter con = null;
        try {
            final Browser br2 = br.cloneBrowser();
            /* HEAD request is only possible for anonymous/GUEST users. */
            if (account != null) {
                con = br2.openGetConnection(dllink);
            } else {
                con = br2.openHeadConnection(dllink);
            }
            if (!this.looksLikeDownloadableContent(con)) {
                throw new IOException();
            }
            if (con.getCompleteContentLength() > 0) {
                if (con.isContentDecoded()) {
                    link.setDownloadSize(con.getCompleteContentLength());
                } else {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
            }
            final String serverFilename = getFileNameFromConnection(con);
            if (serverFilename != null) {
                link.setFinalFileName(serverFilename);
            }
            return dllink;
        } catch (final Exception e) {
            logger.log(e);
            return null;
        } finally {
            if (con != null) {
                con.disconnect();
            }
        }
    }

    public static void parseFileInfo(final DownloadLink link, final Map<String, Object> entry) {
        // link, download?
        String downloadURL = (String) entry.get("link");
        if (StringUtils.isEmpty(downloadURL)) {
            // directLink, streaming?
            downloadURL = (String) entry.get("directLink");
        }
        final long size = JavaScriptEngineFactory.toLong(entry.get("size"), -1);
        if (size > 0) {
            link.setVerifiedFileSize(size);
        }
        final String name = (String) entry.get("name");
        final String md5 = (String) entry.get("md5");
        final String description = (String) entry.get("description");
        if (!StringUtils.isEmpty(name)) {
            link.setFinalFileName(name);
        }
        if (!StringUtils.isEmpty(md5)) {
            link.setMD5Hash(md5);
        }
        if (StringUtils.isEmpty(link.getComment()) && !StringUtils.isEmpty(description)) {
            link.setComment(description);
        }
        /*
         * 2021-03-30: Check if the file contains malicious software according to their system. We could still download it but it's
         * impossible via website so let's not do it either.
         */
        final List<Object> dangers = (List<Object>) entry.get("v" + "i" + "ruses");
        if (dangers != null && !dangers.isEmpty()) {
            link.setProperty(PROPERTY_DANGEROUS_FILE, true);
        } else {
            link.removeProperty(PROPERTY_DANGEROUS_FILE);
        }
        if (!StringUtils.isEmpty(downloadURL)) {
            link.setProperty(PROPERTY_DIRECTURL, downloadURL);
        }
        link.setProperty(PROPERTY_INTERNAL_FILEID, entry.get("id"));
        link.setProperty(PROPERTY_PARENT_FOLDER_ID, entry.get("parentFolder"));
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        requestFileInformation(link, account);
        final boolean isDangerousFile = link.getBooleanProperty(PROPERTY_DANGEROUS_FILE, false);
        final String directurl = link.getStringProperty(PROPERTY_DIRECTURL, null);
        if (isDangerousFile && !PluginJsonConfig.get(GofileIoConfig.class).isAllowMaliciousFileDownload()) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "This file was flagged as to contain malicious software! You can allow download of such files in the plugin settings.");
        } else if (StringUtils.isEmpty(directurl)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, directurl, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (br.containsHTML("Monthly download limit exceeded")) {
                // Monthly download limit exceeded (2462.99GB/1000GB per 30 days). Please upgrade to premium for higher download limits.
                final String msgForUser;
                if (br.getRequest().getHtmlCode().length() <= 200) {
                    /* Error message is plain html code */
                    msgForUser = br.getRequest().getHtmlCode().trim();
                } else {
                    msgForUser = "Monthly download limit exceeded";
                }
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, msgForUser, TimeUnit.DAYS.toMillis(1));
            } else if (br.containsHTML("Server is currently overloaded.\\s*Premium accounts have priority access.\\s*Please upgrade or retry later.")) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server is currently overloaded. Premium accounts have priority access. Please upgrade or retry later.", 30 * 60 * 1000l);
            } else if (br.getURL().matches("(?i)https?://[^/]+/d/[a-f0-9\\-]+")) {
                /* Redirect to main/folder URL -> Most likely directurl expired */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error: Directurl expired?", 3 * 60 * 1000l);
            } else {
                if (br.getRequest().getHtmlCode().length() <= 200) {
                    /* Looks like plaintext error response -> Display to user */
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Unknown error: " + br.getRequest().getHtmlCode());
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
        /* Add a download slot */
        controlMaxFreeDownloads(account, link, +1);
        try {
            /* Start download */
            dl.startDownload();
        } finally {
            /* Remove download slot */
            controlMaxFreeDownloads(account, link, -1);
        }
    }

    @Override
    public boolean looksLikeDownloadableContent(final URLConnectionAdapter urlConnection) {
        /* 2021-07-05: Override to allow 0-byte-files. */
        return urlConnection != null && (urlConnection.getResponseCode() == 200 || urlConnection.getResponseCode() == 206) && urlConnection.isContentDisposition();
    }

    protected void controlMaxFreeDownloads(final Account account, final DownloadLink link, final int num) {
        if (account != null) {
            return;
        }
        synchronized (freeRunning) {
            final int before = freeRunning.get();
            final int after = before + num;
            freeRunning.set(after);
            logger.info("freeRunning(" + link.getName() + ")|max:" + getMaxSimultanFreeDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
        }
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    // private void setLoginHeadersAPI(final Browser br, final Account account) {
    // br.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + account.getPass());
    // br.getHeaders().put(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://" + getHost());
    // br.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT, "*/*");
    // setLoginCookiesAPI(br, account);
    // }

    private void setLoginCookiesAPI(final Browser br, final Account account) {
        br.setCookie(getHost(), COOKIE_ACCOUNT_TOKEN, account.getPass());
    }

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        final GetRequest request = new GetRequest(getAPIBase() + "/accounts/website");
        request.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + account.getPass());
        request.getHeaders().put(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://" + getHost());
        request.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT, "*/*");
        br.getPage(request);
        final Map<String, Object> data = this.handleAPIErrors(account, null);
        final AccountInfo ai = new AccountInfo();
        final Number createTime = (Number) data.get("createTime");
        final Number usedSpaceBytes = (Number) data.get("storage");
        final Number trafficMaxBytes = (Number) data.get("subscriptionLimitDirectTraffic");
        final String email = (String) data.get("email");
        if (createTime != null) {
            ai.setCreateTime(createTime.longValue() * 1000);
        }
        if (StringUtils.isNotEmpty(email)) {
            account.setUser(email);
        }
        if (usedSpaceBytes != null) {
            ai.setUsedSpace(usedSpaceBytes.longValue());
        }
        final String tier = (String) data.get("tier");
        if ("guest".equals(tier)) {
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(1);
            ai.setStatus("Guest Account");
            return ai;
        } else if ("premium".equals(tier)) {
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(-1);
        } else if ("standard".equals(tier)) {
            // standard -> expired premium
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(1);
            ai.setStatus("Standard(expired premium) Account");
            return ai;
        } else {
            throw new AccountInvalidException("Unsupported tier:" + tier);
        }
        if (trafficMaxBytes != null) {
            ai.setTrafficMax(trafficMaxBytes.longValue());
            final Map<String, Object> statsCurrent = (Map<String, Object>) data.get("statsCurrent");
            final Number trafficUsedToday = statsCurrent != null ? (Number) statsCurrent.get("trafficWebDownloaded") : null;
            long trafficUsed = 0;
            if (trafficUsedToday != null) {
                trafficUsed += trafficUsedToday.longValue();
            }
            final Map<String, Object> statsHistory = (Map<String, Object>) data.get("statsHistory");
            sumHistoryTraffic: if (statsHistory != null) {
                Map.Entry<String, Object> yearEntry = null;
                for (Entry<String, Object> historyEntry : statsHistory.entrySet()) {
                    if (yearEntry == null || Integer.parseInt(historyEntry.getKey()) > Integer.parseInt(yearEntry.getKey())) {
                        yearEntry = historyEntry;
                    }
                }
                if (yearEntry == null) {
                    break sumHistoryTraffic;
                }
                final Map<String, Object> year = (Map<String, Object>) yearEntry.getValue();
                Map.Entry<String, Object> monthEntry = null;
                for (Entry<String, Object> yearHistoryEntry : year.entrySet()) {
                    if (monthEntry == null || Integer.parseInt(yearHistoryEntry.getKey()) > Integer.parseInt(monthEntry.getKey())) {
                        monthEntry = yearHistoryEntry;
                    }
                }
                if (monthEntry == null) {
                    break sumHistoryTraffic;
                }
                final Map<String, Object> month = (Map<String, Object>) monthEntry.getValue();
                for (Entry<String, Object> dayEntry : month.entrySet()) {
                    final Map<String, Object> day = (Map<String, Object>) dayEntry.getValue();
                    final Number trafficWebDownloaded = (Number) day.get("trafficWebDownloaded");
                    if (trafficWebDownloaded != null) {
                        trafficUsed += trafficWebDownloaded.longValue();
                    }
                }
            }
            ai.setTrafficLeft(trafficMaxBytes.longValue() - trafficUsed);
        }
        final String premiumType = (String) data.get("premiumType");
        final String subscriptionProvider = (String) data.get("subscriptionProvider");
        final Number subscriptionEndDate = (Number) data.get("subscriptionEndDate");
        if ("credit".equals(premiumType)) {
            final Number credit = (Number) data.get("credit");
            final Number creditTrafficRate = (Number) data.get("creditTrafficRate");
            final String currency = data.get("currency").toString();
            ai.setAccountBalance(credit.doubleValue(), Currency.getInstance(currency));
            ai.setStatus("Credits Premium Account: Balance " + credit + " " + currency);
            ai.setTrafficLeft((long) (credit.doubleValue() / creditTrafficRate.doubleValue() * (1000 * 1000 * 1000 * 1000l)));
            ai.setValidUntil(-1);
            ai.setTrafficRefill(false);
            // add support for expire/valid until calculation
        } else if ("subscription".equals(premiumType) && subscriptionEndDate != null) {
            ai.setStatus("Premium(" + subscriptionProvider + ") Account");
            if (9999999999l == subscriptionEndDate.longValue()) {
                /* No expire date, eg subscriptionProvider=patreon */
                ai.setValidUntil(-1);
            } else {
                ai.setValidUntil(subscriptionEndDate.longValue() * 1000);
            }
            ai.setTrafficRefill(true);
        } else {
            throw new AccountInvalidException("Unsupported premiumType:" + premiumType);
        }
        return ai;
    }

    private Map<String, Object> handleAPIErrors(final Account account, final DownloadLink link) throws Exception {
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String msg = "Invalid API response";
            final long wait = 1 * 60 * 1000;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, wait, ignore);
            } else {
                throw new AccountUnavailableException(ignore, msg, wait);
            }
        }
        final Map<String, Object> data = (Map<String, Object>) entries.get("data");
        if (data != null && !data.isEmpty()) {
            /* No error */
            return data;
        }
        /* E.g. {"status":"error-wrongToken","data":{}} */
        final String status = entries.get("status").toString();
        if (StringUtils.isEmpty(status)) {
            /* No error */
            return entries;
        }
        // TODO: Add support for more error messages
        if (status.equalsIgnoreCase("error-wrongToken")) {
            /* Comes with http response 401 */
            throw new AccountInvalidException("Invalid API key");
        } else {
            /* Unknown error or link based error */
            if (link == null) {
                throw new AccountUnavailableException(status, 3 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, status);
            }
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return freeRunning.get() + 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }
}