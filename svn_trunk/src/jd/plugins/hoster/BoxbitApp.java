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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.BrowserAdapter;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 51056 $", interfaceVersion = 3, names = { "boxbit.app" }, urls = { "https://download\\.boxbit\\.app/([a-f0-9]{32})(/([^/]+))?" })
public class BoxbitApp extends PluginForHost {
    /**
     * New project of: geragera.com.br </br>
     * API docs: https://boxbit.readme.io/reference/introduction
     */
    private static final String          API_BASE                         = "https://api.boxbit.app";
    private static MultiHosterManagement mhm                              = new MultiHosterManagement("boxbit.app");
    private static final int             defaultMAXCHUNKS                 = 1;
    private static final String          PROPERTY_userid                  = "userid";
    private static final String          PROPERTY_logintoken              = "token";
    private static final String          PROPERTY_logintoken_valid_until  = "token_valid_until";
    private static final String          PROPERTY_DOWNLOADLINK_directlink = "directlink";
    private static final String          PROPERTY_DOWNLOADLINK_maxchunks  = "maxchunks";

    @SuppressWarnings("deprecation")
    public BoxbitApp(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/!/register");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/";
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setCookiesExclusive(true);
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "JDownloader");
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT, "application/json");
        br.setAllowedResponseCodes(new int[] { 422, 429 });
        return br;
    }

    private void setLoginHeader(final Browser br, final Account account) {
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + this.getLoginToken(account));
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
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws PluginException, IOException {
        if (!link.isNameSet()) {
            /* Set weak filename */
            final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
            final String fid = urlinfo.getMatch(0);
            final String filenameFromURL = urlinfo.getMatch(2);
            if (filenameFromURL != null) {
                link.setName(Encoding.htmlDecode(filenameFromURL));
            } else {
                link.setName(fid);
            }
        }
        final Browser brc = br.cloneBrowser();
        URLConnectionAdapter con = null;
        try {
            con = brc.openHeadConnection(link.getPluginPatternMatcher());
            handleConnectionErrors(brc, con);
            if (con.getCompleteContentLength() > 0) {
                if (con.isContentDecoded()) {
                    link.setVerifiedFileSize(-1);
                    link.setDownloadSize(con.getCompleteContentLength());
                } else {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
            }
            final String filename = Plugin.getFileNameFromConnection(con);
            if (filename != null) {
                link.setFinalFileName(filename);
            }
            parseAndSetMaxChunksLimitFromHeader(link, con);
        } finally {
            try {
                if (con != null) {
                    con.disconnect();
                }
            } catch (final Throwable e) {
            }
        }
        return AvailableStatus.TRUE;
    }

    private void parseAndSetMaxChunksLimitFromHeader(final DownloadLink link, final URLConnectionAdapter con) {
        /* Get max allowed number of chunks from header. */
        final String maxChunksStr = con.getRequest().getResponseHeader("X-Max-Chunks");
        if (maxChunksStr == null) {
            logger.info("Failed to find X-Max-Chunks header");
            return;
        } else if (!maxChunksStr.matches("\\d+")) {
            logger.warning("X-Max-Chunks came in unsupported format!");
            return;
        }
        logger.info("Max chunks for this item: " + maxChunksStr);
        link.setProperty(PROPERTY_DOWNLOADLINK_maxchunks, Integer.parseInt(maxChunksStr));
    }

    @Override
    protected void throwConnectionExceptions(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        if (con.getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        super.throwConnectionExceptions(br, con);
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleSelfhostedFileDownload(link, null);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleSelfhostedFileDownload(link, account);
    }

    public void handleSelfhostedFileDownload(final DownloadLink link, final Account account) throws Exception {
        if (account != null) {
            this.setLoginHeader(br, account);
        }
        int maxChunks = this.getMaxChunks(link);
        logger.info("maxChunks(BeforeOpen): " + maxChunks);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, link.getPluginPatternMatcher(), this.isResumeable(link, account), maxChunks);
        this.handleConnectionErrors(br, dl.getConnection());
        parseAndSetMaxChunksLimitFromHeader(link, dl.getConnection());
        maxChunks = this.getMaxChunks(link);
        logger.info("maxChunks(AfterOpen): " + maxChunks);
        BrowserAdapter.applySettings(dl, this.isResumeable(link, account), maxChunks);
        dl.startDownload();
    }

    private void handleMultihosterFileDownload(final Account account, final DownloadLink link) throws Exception {
        final String directlinkproperty = this.getHost() + PROPERTY_DOWNLOADLINK_directlink;
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Trying to re-use stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            this.loginAPI(account, false);
            String passCode = link.getDownloadPassword();
            if (link.isPasswordProtected() && passCode == null) {
                passCode = getUserInput("Password?", link);
            }
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("link", link.getDefaultPlugin().buildExternalDownloadURL(link, this));
            if (passCode != null) {
                postdata.put("password", passCode);
            }
            br.postPageRaw(API_BASE + "/users/" + this.getUserID(account) + "/downloader/request-file", JSonStorage.serializeToJson(postdata));
            this.handleErrors(this.br, account, link);
            if (passCode != null) {
                /* Given password is correct --> Save it for later usage */
                link.setDownloadPassword(passCode);
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            dllink = (String) entries.get("link");
            if (StringUtils.isEmpty(dllink)) {
                /* This should never happen */
                mhm.handleErrorGeneric(account, link, "dllinknull", 50, 5 * 60 * 1000l);
            }
            link.setProperty(PROPERTY_DOWNLOADLINK_maxchunks, entries.get("max_chunks"));
        }
        int maxChunks = this.getMaxChunks(link);
        logger.info("maxChunks(BeforeOpen): " + maxChunks);
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), maxChunks);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                /* 2021-09-07: Don't jump into errorhandling here as we typically won't get a json response here. */
                // handleErrors(this.br, account, link);
                mhm.handleErrorGeneric(account, link, "Unknown download error", 50, 5 * 60 * 1000l);
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirecturl == null) {
            link.setProperty(this.getHost() + PROPERTY_DOWNLOADLINK_directlink, dl.getConnection().getURL().toExternalForm());
        }
        parseAndSetMaxChunksLimitFromHeader(link, dl.getConnection());
        maxChunks = this.getMaxChunks(link);
        logger.info("maxChunks(AfterOpen): " + maxChunks);
        BrowserAdapter.applySettings(dl, this.isResumeable(link, account), maxChunks);
        dl.startDownload();
    }

    private int getMaxChunks(final DownloadLink link) {
        int chunks = link.getIntegerProperty(PROPERTY_DOWNLOADLINK_maxchunks, defaultMAXCHUNKS);
        if (chunks > 1) {
            chunks = -chunks;
        }
        return chunks;
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        handleMultihosterFileDownload(account, link);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        Map<String, Object> user = loginAPI(account, true);
        /* Access userInfo if it hasn't been accessed already. */
        if (br.getURL() == null || !br.getURL().contains("/users/")) {
            user = accessUserInfo(this.br, account);
        }
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> subscription = (Map<String, Object>) user.get("subscription");
        List<Map<String, Object>> hosts = null;
        final Object currentSubscriptionO = subscription.get("current");
        if (currentSubscriptionO == null) {
            setAccountInfo(account, ai, AccountType.FREE);
        } else {
            final Map<String, Object> currentSubscription = (Map<String, Object>) currentSubscriptionO;
            if ((Boolean) currentSubscription.get("is_active")) {
                setAccountInfo(account, ai, AccountType.PREMIUM);
                final String end_date = currentSubscription.get("end_date").toString();
                ai.setValidUntil(TimeFormatter.getMilliSeconds(end_date, "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", Locale.ENGLISH));
                ai.setStatus("Package: " + currentSubscription.get("package_name"));
                hosts = (List<Map<String, Object>>) currentSubscription.get("filehosts");
            } else {
                /* Expired premium account?? */
                setAccountInfo(account, ai, AccountType.FREE);
            }
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* For devs: Display token validity in GUI. */
            String status = ai.getStatus();
            if (status == null) {
                status = "";
            }
            status += " | TokenValidity: " + new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(this.getLoginTokenValidity(account));
            ai.setStatus(status);
        }
        final ArrayList<MultiHostHost> supportedhostslist = new ArrayList<MultiHostHost>();
        /* Can be null for free accounts! */
        if (hosts != null) {
            br.getPage(API_BASE + "/filehosts/domains");
            /* Contains mapping e.g. "uploaded" --> [uploaded.net, uploaded.to, ul.to] */
            final Map<String, Object> hostMapping = this.handleErrors(br, account, null);
            for (final Map<String, Object> hostInfo : hosts) {
                final Map<String, Object> hostDetails = (Map<String, Object>) hostInfo.get("details");
                final String hostIdentifier = (String) hostDetails.get("identifier");
                final List<String> fullDomains = (List<String>) hostMapping.get(hostIdentifier);
                if (fullDomains == null || fullDomains.isEmpty()) {
                    logger.info("Skipping invalid/empty entry: " + hostInfo);
                    continue;
                }
                final MultiHostHost mhost = new MultiHostHost();
                if (hostDetails.get("status").toString().equalsIgnoreCase("working")) {
                    mhost.setStatus(MultihosterHostStatus.WORKING);
                } else {
                    mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                }
                mhost.setDomains(fullDomains);
                supportedhostslist.add(mhost);
            }
        }
        ai.setMultiHostSupportV2(this, supportedhostslist);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    private void setAccountInfo(final Account account, final AccountInfo ai, final AccountType type) {
        if (type == AccountType.PREMIUM) {
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(type);
            /* Free accounts cannot be used for downloading */
            ai.setTrafficLeft(0);
            ai.setExpired(true);
        }
        account.setMaxSimultanDownloads(Integer.MAX_VALUE);
    }

    private Map<String, Object> loginAPI(final Account account, final boolean forceAuthCheck) throws IOException, PluginException, InterruptedException {
        synchronized (account) {
            final String token = getLoginToken(account);
            final String userid = getUserID(account);
            if (token != null && userid != null) {
                setLoginHeader(br, account);
                final long tokenRemainingTimeMillis = getLoginTokenRemainingMillis(account);
                /* Force token refresh if it has expired or is valid for less than <minimumTokenValidity> only. */
                final long minimumTokenValidity = 60 * 60 * 1000;
                final boolean tokenRefreshRequired = tokenRemainingTimeMillis < minimumTokenValidity;
                if (!forceAuthCheck && !tokenRefreshRequired) {
                    /* We trust our token --> Do not check them */
                    logger.info("Trust login token without check");
                    return null;
                } else {
                    if (tokenRefreshRequired) {
                        logger.info("Token refresh required");
                    } else {
                        logger.info("Attempting token login");
                        try {
                            final Map<String, Object> entries = accessUserInfo(br, account);
                            logger.info("Token login successful");
                            return entries;
                        } catch (final PluginException ignore) {
                            logger.info("Token login failed --> Trying token refresh");
                        }
                    }
                    logger.info("Attempting token refresh");
                    /* Send empty POST request with existing token to get a fresh token */
                    br.postPage(API_BASE + "/auth/refresh", new UrlQuery());
                    try {
                        final Map<String, Object> entries = this.handleErrors(br, account, null);
                        logger.info("Token refresh successful");
                        /* Store new token */
                        setAuthTokenData(account, (Map<String, Object>) entries.get("auth"));
                        /* Set new auth header */
                        setLoginHeader(br, account);
                        return entries;
                    } catch (final PluginException tokenRefreshFailure) {
                        logger.warning("Token refresh failed");
                        br.getHeaders().remove(HTTPConstants.HEADER_REQUEST_AUTHORIZATION);
                        /* Try full login as last-chance */
                    }
                }
            }
            logger.info("Performing full login");
            br.postPageRaw(API_BASE + "/auth/login", "{\"email\":\"" + account.getUser() + "\",\"password\":\"" + account.getPass() + "\"}");
            final Map<String, Object> entries = this.handleErrors(this.br, account, null);
            account.setProperty(PROPERTY_userid, JavaScriptEngineFactory.walkJson(entries, "user/uuid").toString());
            setAuthTokenData(account, (Map<String, Object>) entries.get("auth"));
            setLoginHeader(br, account);
            return entries;
        }
    }

    private static void setAuthTokenData(final Account account, final Map<String, Object> auth) {
        account.setProperty(PROPERTY_logintoken, auth.get("access_token").toString());
        /* 2021-08-18: tokens are usually valid for 72 hours */
        account.setProperty(PROPERTY_logintoken_valid_until, System.currentTimeMillis() + ((Number) auth.get("expires_in")).longValue() * 1000);
    }

    private String getLoginToken(final Account account) {
        return account.getStringProperty(PROPERTY_logintoken);
    }

    private long getLoginTokenValidity(final Account account) {
        return account.getLongProperty(PROPERTY_logintoken_valid_until, 0);
    }

    private long getLoginTokenRemainingMillis(final Account account) {
        final long validityTimestamp = getLoginTokenValidity(account);
        if (validityTimestamp < System.currentTimeMillis()) {
            return 0;
        } else {
            return validityTimestamp - System.currentTimeMillis();
        }
    }

    private String getUserID(final Account account) {
        return account.getStringProperty(PROPERTY_userid);
    }

    /**
     * https://boxbit.readme.io/reference/user-filehost-list
     *
     * @throws InterruptedException
     * @throws PluginException
     */
    private Map<String, Object> accessUserInfo(final Browser br, final Account account) throws IOException, PluginException, InterruptedException {
        br.getPage(API_BASE + "/users/" + getUserID(account) + "/?with[]=subscription&with[]=current_subscription_filehosts&with[]=current_subscription_filehost_usages");
        return this.handleErrors(br, account, null);
    }

    /** Handle errors according to: https://boxbit.readme.io/reference/error-codes */
    private Map<String, Object> handleErrors(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String msg = "Invalid API response";
            final long waitMillis = 60 * 1000l;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, waitMillis);
            } else {
                throw new AccountUnavailableException(msg, 60 * 1000);
            }
        }
        final String message = (String) entries.get("message");
        if (br.getHttpConnection().getResponseCode() == 401) {
            /* Authentication failure */
            throw new AccountInvalidException(message);
        }
        /* All possible error-keys go along with http response 422 except "busy_worker" --> 429 */
        final List<String> errors = (List<String>) entries.get("errors");
        if (errors == null || errors.isEmpty()) {
            /* No error */
            return entries;
        }
        for (final String error : errors) {
            if (error.equalsIgnoreCase("subscription_not_active")) {
                /* Account doesn't have any paid subscription package */
                throw new AccountInvalidException(message);
            } else if (error.equalsIgnoreCase("unsupported_filehost") || error.equalsIgnoreCase("user_missing_filehost") || error.equalsIgnoreCase("filehost_unavailable") || error.equalsIgnoreCase("filehost_links_quota_exceeded") || error.equalsIgnoreCase("filehost_traffic_quota_exceeded")) {
                /* Errors that immediately temporarily disable a host */
                mhm.putError(account, link, 5 * 60 * 1000l, message);
            } else if (error.equalsIgnoreCase("busy_worker")) {
                /* E.g. {"identifier":"mega","message":"Already processing another file on this filehost.","errors":["busy_worker"]} */
                mhm.putError(account, link, 1 * 60 * 1000l, message);
            } else if (error.equalsIgnoreCase("wrong_password")) {
                /* Missing or wrong password */
                link.setPasswordProtected(true); // Enable this so next time we'll ask the user to enter the password
                link.setDownloadPassword(null);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
            } else if (error.equalsIgnoreCase("file_not_found")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                /*
                 * Stuff we may know but purposely handle as generic error: blocked_ip, temporarily_unavailable, ip_address_not_br
                 */
                logger.info("Detected unknown/generic errorkey: " + error);
            }
        }
        /* They also got "file_not_found" but we still don't trust such errors when multihosts return them! */
        /* No known error? Handle as generic one. */
        mhm.handleErrorGeneric(account, link, message, 50);
        /* Unreachable code */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        /**
         * Officially, free downloads aren't possible at all but technically, their selfhosted direct URLs are also downloadable for free
         * users. <br>
         * We are limiting them to 1 though because there are limits which we only know in account mode since their account API tells us how
         * many simultaneous downloads are allowed.
         */
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        /* No captchas required for downloads over this plugin. */
        return false;
    }
}