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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.formatter.TimeFormatter;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.AccountControllerEvent;
import jd.controlling.AccountControllerListener;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

/** API docs: https://a.hatfile.com/JD2/doc.html */
@HostPlugin(revision = "$Revision: 53023 $", interfaceVersion = 3, names = {}, urls = {})
public class HatfileCom extends PluginForHost {
    public static final String  API_BASE                      = "https://a.hatfile.com/JD2/api.php";
    private static final String PROPERTY_SESSION              = "session";
    private static final String PROPERTY_SESSION_EXPIRES_AT   = "session_expires_at";
    private static final String PROPERTY_DIRECTURL            = "directurl";
    private static final String PROPERTY_DIRECTURL_EXPIRES_AT = "directurl_expires_at";
    private static final String PROPERTY_PREMIUM_REQUIRED     = "premium_required";
    private static final String PROPERTY_RESUMABLE            = "resumable";
    private static final String PROPERTY_MAXCHUNKS            = "maxchunks";
    /**
     * Guards {@link #registerLogoutListener()} so the AccountControllerListener only gets added once per account. Must only be checked/set
     * on the stable plugin instance cached in {@link Account#getPlugin()}, not on the short-lived instances created for a single account
     * check/download.
     */
    private final AtomicBoolean LOGOUT_LISTENER_REGISTERED    = new AtomicBoolean(false);

    public HatfileCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/premium");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(new int[] { 400, 401, 403, 404, 500 });
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "hatfile.com" });
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
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([A-F0-9]{6}-[A-F0-9]{6})(/([^/]*))?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/pages/rules";
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
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

    private String getContentURL(final DownloadLink link) {
        return "https://" + this.getHost() + "/" + getFID(link) + "/";
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        String filenameFromURL = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(2);
        if (filenameFromURL != null) {
            return filenameFromURL;
        }
        return getFID(link);
    }

    private Map<String, Object> postAPI(final Map<String, Object> postData) throws Exception {
        br.getHeaders().put("Content-Type", "application/json");
        br.postPageRaw(API_BASE, JSonStorage.serializeToJson(postData));
        return restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
    }

    /** Checks the "success"/"status" fields of a response and returns the "data" object on success, else throws. */
    @SuppressWarnings("unchecked")
    private Map<String, Object> getData(final Map<String, Object> response) throws Exception {
        final boolean success = (Boolean) response.get("success");
        final int status = ((Number) response.get("status")).intValue();
        if (success && status == 0) {
            return (Map<String, Object>) response.get("data");
        }
        final String msg = (String) response.get("msg");
        switch (status) {
        case 1:
            /* Invalid credentials */
        case 2:
            /* Account inactive */
            throw new AccountInvalidException(msg);
        case 3:
            /* Premium account required */
            throw new AccountRequiredException(msg);
        case 4:
            /* Session expired */
        case 5:
            /* Invalid session */
            throw new AccountUnavailableException("Session expired", 60 * 1000l);
        case 101:
            /* Invalid HatFile URL */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, msg);
        case 102:
            /* File offline */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, msg);
        case 103:
            /* Storage unavailable (I guess this should never happen during downloading) */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, 5 * 60 * 1000l);
        case 104:
            /* Not enough Premium traffic */
            throw new AccountUnavailableException(msg, 60 * 60 * 1000l);
        case 500:
            /* Server error */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, 5 * 60 * 1000l);
        default:
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg);
        }
    }

    /** Performs a fresh login, stores the returned session on the account and returns the login response data. */
    private Map<String, Object> login(final Account account) throws Exception {
        synchronized (account) {
            final Map<String, Object> postData = new HashMap<String, Object>();
            postData.put("action", "login");
            postData.put("login", account.getUser());
            postData.put("password", account.getPass());
            final Map<String, Object> data = getData(postAPI(postData));
            final String session = (String) data.get("session");
            final long expiresInMillis = ((Number) data.get("session_expires_in")).longValue() * 1000l;
            account.setProperty(PROPERTY_SESSION, session);
            account.setProperty(PROPERTY_SESSION_EXPIRES_AT, System.currentTimeMillis() + expiresInMillis);
            return data;
        }
    }

    /** Checks Premium status/traffic of an existing session without creating a new login token. */
    private Map<String, Object> accountCheck(final String session) throws Exception {
        final Map<String, Object> postData = new HashMap<String, Object>();
        postData.put("action", "account");
        postData.put("session", session);
        return getData(postAPI(postData));
    }

    /** Returns a session token, re-using the stored one until 30 minutes before its expiry, else performs a fresh login. */
    private String getSession(final Account account, final boolean forceRefresh) throws Exception {
        synchronized (account) {
            if (!forceRefresh) {
                final String session = account.getStringProperty(PROPERTY_SESSION, null);
                final long expiresAt = account.getLongProperty(PROPERTY_SESSION_EXPIRES_AT, 0);
                /* Re-use the session until 30 minutes before it actually expires. */
                if (session != null && System.currentTimeMillis() < expiresAt - 30 * 60 * 1000l) {
                    return session;
                }
            }
            final Map<String, Object> data = login(account);
            return (String) data.get("session");
        }
    }

    /** Invalidates the account's session server-side. Called via the AccountController listener once the account gets removed. */
    private void logout(final Account account) {
        final String session = account.getStringProperty(PROPERTY_SESSION, null);
        if (session == null) {
            /* No stored session -> We cannot logout */
            return;
        }
        try {
            if (br == null) {
                /* This plugin instance may already have gone through clean() (e.g. after its last account check/download). */
                setBrowser(createNewBrowserInstance());
            }
            final Map<String, Object> postData = new HashMap<String, Object>();
            postData.put("action", "logout");
            postData.put("session", session);
            postAPI(postData);
            logger.info("Logout successful");
            account.removeProperty(PROPERTY_SESSION);
            account.removeProperty(PROPERTY_SESSION_EXPIRES_AT);
        } catch (final Exception e) {
            logger.log(e);
        }
    }

    /** Registers the logout listener exactly once per account, anchored on the stable plugin instance from {@link Account#getPlugin()}. */
    private void registerLogoutListener(final Account account) {
        final PluginForHost accPlugin = account.getPlugin();
        if (accPlugin instanceof HatfileCom) {
            ((HatfileCom) accPlugin).doRegisterLogoutListener();
        }
    }

    private void doRegisterLogoutListener() {
        if (LOGOUT_LISTENER_REGISTERED.compareAndSet(false, true)) {
            AccountController.getInstance().getEventSender().addListener(new AccountControllerListener() {
                @Override
                public void onAccountControllerEvent(final AccountControllerEvent event) {
                    if (AccountControllerEvent.Types.REMOVED.equals(event.getType())) {
                        final Account removedAccount = event.getAccount();
                        if (removedAccount != null && getHost().equalsIgnoreCase(removedAccount.getHoster())) {
                            logout(removedAccount);
                        }
                    }
                }
            });
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        // registerLogoutListener(account);
        final String session = getSession(account, false);
        final Map<String, Object> data = accountCheck(session);
        final AccountInfo ai = new AccountInfo();
        if (Boolean.TRUE.equals(data.get("premium"))) {
            account.setType(AccountType.PREMIUM);
            account.setConcurrentUsePossible(true);
            ai.setValidUntil(TimeFormatter.getMilliSeconds((String) data.get("premium_expire"), "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH), br);
        } else {
            account.setType(AccountType.FREE);
        }
        if (Boolean.TRUE.equals(data.get("traffic_unlimited"))) {
            ai.setUnlimitedTraffic();
        } else {
            ai.setTrafficLeft(((Number) data.get("trafficleft")).longValue());
            ai.setTrafficMax(((Number) data.get("trafficmax_daily")).longValue());
        }
        return ai;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Map<String, Object> postData = new HashMap<String, Object>();
        postData.put("action", "fileinfo");
        postData.put("url", getContentURL(link));
        final Map<String, Object> data = getData(postAPI(postData));
        final boolean online = (Boolean) data.get("online");
        if (!online) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        link.setFinalFileName((String) data.get("filename"));
        link.setVerifiedFileSize(((Number) data.get("filesize")).longValue());
        link.setMD5Hash((String) data.get("md5"));
        if (Boolean.TRUE.equals(data.get("premium_required"))) {
            link.setProperty(PROPERTY_PREMIUM_REQUIRED, true);
        } else {
            link.removeProperty(PROPERTY_PREMIUM_REQUIRED);
        }
        link.setProperty(PROPERTY_RESUMABLE, data.get("resumable"));
        link.setProperty(PROPERTY_MAXCHUNKS, ((Number) data.get("maxchunks")).intValue());
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        /* All HatFile downloads require an active Premium account. */
        throw new AccountRequiredException();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link);
        if (link.hasProperty(PROPERTY_PREMIUM_REQUIRED) && !AccountType.PREMIUM.equals(account.getType())) {
            throw new AccountRequiredException();
        }
        String directurl = link.getStringProperty(PROPERTY_DIRECTURL, null);
        final long directurlExpiresAt = link.getLongProperty(PROPERTY_DIRECTURL_EXPIRES_AT, 0);
        if (directurl != null && System.currentTimeMillis() >= directurlExpiresAt) {
            directurl = null;
            link.removeProperty(PROPERTY_DIRECTURL);
            link.removeProperty(PROPERTY_DIRECTURL_EXPIRES_AT);
        }
        boolean isFreshDirecturl = false;
        if (directurl == null) {
            directurl = requestDirectURL(account, link, false);
            isFreshDirecturl = true;
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, directurl, isResumeable(link, account), getMaxChunks(link, account));
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            link.removeProperty(PROPERTY_DIRECTURL);
            link.removeProperty(PROPERTY_DIRECTURL_EXPIRES_AT);
            if (isFreshDirecturl) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Final downloadurl did not lead to downloadable content");
            }
            /* Stored directurl was stale -> request a fresh one and retry once. */
            directurl = requestDirectURL(account, link, true);
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, directurl, isResumeable(link, account), getMaxChunks(link, account));
            if (!looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Final downloadurl did not lead to downloadable content");
            }
        }
        link.setProperty(PROPERTY_DIRECTURL, dl.getConnection().getURL().toExternalForm());
        dl.startDownload();
    }

    /** Requests a fresh direct download URL via the API and stores its expiry on the link. */
    private String requestDirectURL(final Account account, final DownloadLink link, final boolean forceFreshSession) throws Exception {
        String session = getSession(account, forceFreshSession);
        Map<String, Object> data;
        try {
            data = requestDownload(session, link);
        } catch (final AccountUnavailableException e) {
            if (!"Session expired".equals(e.getMessage())) {
                throw e;
            }
            session = getSession(account, true);
            data = requestDownload(session, link);
        }
        final String directurl = (String) data.get("direct_url");
        link.setProperty(PROPERTY_DIRECTURL_EXPIRES_AT, ((Number) data.get("expires_at")).longValue());
        updateTraffic: {
            if (Boolean.FALSE.equals(data.get("charged"))) {
                logger.info("User was not charged any traffic for this download");
                break updateTraffic;
            }
            final Map<String, Object> trafficinfo = (Map<String, Object>) data.get("account");
            if (trafficinfo == null) {
                break updateTraffic;
            }
            if (Boolean.TRUE.equals(trafficinfo.get("traffic_unlimited"))) {
                /* User has unlimited traffic -> No need to update traffic info */
                break updateTraffic;
            }
            final AccountInfo ai = account.getAccountInfo();
            if (ai == null) {
                break updateTraffic;
            }
            ai.setTrafficLeft(((Number) trafficinfo.get("trafficleft")).longValue());
            ai.setTrafficMax(((Number) trafficinfo.get("trafficmax_daily")).longValue());
        }
        return directurl;
    }

    private Map<String, Object> requestDownload(final String session, final DownloadLink link) throws Exception {
        final Map<String, Object> postData = new HashMap<String, Object>();
        postData.put("action", "download");
        postData.put("session", session);
        postData.put("url", getContentURL(link));
        return getData(postAPI(postData));
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return link.getBooleanProperty(PROPERTY_RESUMABLE, true);
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        final int maxchunks = link.getIntegerProperty(PROPERTY_MAXCHUNKS, 0);
        if (maxchunks > 0) {
            return -maxchunks;
        } else {
            return 0;
        }
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    @Override
    public boolean hasAutoCaptcha() {
        return false;
    }

    @Override
    public void update(final DownloadLink downloadLink, final Account account, final long bytesTransfered) {
        /* Do nothing since traffic is updated right away after each download attempt. */
    }
}
