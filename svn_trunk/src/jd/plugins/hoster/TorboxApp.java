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
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.gui.notify.gui.BubbleNotifyConfig.BubbleNotifyEnabledState;
import org.jdownloader.gui.notify.gui.CFG_BUBBLE;
import org.jdownloader.plugins.components.usenet.UsenetServer;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
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
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 50299 $", interfaceVersion = 3, names = { "torbox.app" }, urls = { "" })
public class TorboxApp extends UseNet {
    /* Docs: https://api-docs.torbox.app/ */
    private final String                 API_BASE                                                 = "https://api.torbox.app/v1/api";
    private static MultiHosterManagement mhm                                                      = new MultiHosterManagement("torbox.app");
    private final String                 PROPERTY_ACCOUNT_NOTIFICATIONS_DISPLAYED_UNTIL_TIMESTAMP = "notifications_displayed_until_timestamp";
    private final String                 PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET                    = "max_downloads_usenet";
    private final String                 PROPERTY_ACCOUNT_USENET_USERNAME                         = "usenetU";
    private final String                 PROPERTY_ACCOUNT_USENET_PASSWORD                         = "usenetP";
    private final String                 PROPERTY_ACCOUNT_USENET_SERVER                           = "usenetS";

    public TorboxApp(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + this.getHost() + "/pricing");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "JDownloader");
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.USENET, LazyPlugin.FEATURE.API_KEY_LOGIN, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    @Override
    public String getAGBLink() {
        return "https://" + this.getHost() + "/terms";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        if (isUsenetLink(link)) {
            return super.requestFileInformation(link);
        } else {
            return AvailableStatus.UNCHECKABLE;
        }
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        /**
         * 2024-06-12: Max 16 total connections according to admin. </br>
         * We'll be doing it this way right know, knowing that the user can easily try to exceed that limit with JDownloader.
         */
        return -16;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            return false;
        } else {
            return super.canHandle(link, account);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (isUsenetLink(link)) {
            super.handleMultiHost(link, account);
            return;
        } else {
            /* This should never happen */
            throw new AccountRequiredException();
        }
    }

    private String getPropertyKey(final String property) {
        return this.getHost() + "_" + property;
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        this.login(account, false);
        final String directlinkproperty = this.getPropertyKey("directlink");
        String storedDirecturl = link.getStringProperty(directlinkproperty);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Trying to re-use stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            logger.info("Creating or finding internal file_id");
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("link", link.getDefaultPlugin().buildExternalDownloadURL(link, this));
            if (!StringUtils.isEmpty(link.getDownloadPassword())) {
                query.appendEncoded("password", link.getDownloadPassword());
            }
            final Request req_createwebdownload = br.createPostRequest(API_BASE + "/webdl/createwebdownload", query);
            final Map<String, Object> entries = (Map<String, Object>) this.callAPI(br, req_createwebdownload, account, link);
            /**
             * These two strings can be used to identify the unique item/link we just added. </br>
             * We could cache them but instead we will simply rely on the API to do this for us. </br>
             * Once a download was started successfully we save- and re-use the direct-URL, that should be enough - we do not want to
             * overcomplicate things.
             */
            final String file_id = entries.get("webdownload_id").toString();
            final String hash = entries.get("hash").toString();
            /*
             * 2024-06-13: Disabled this handling.
             */
            cacheCheck: if (true) {
                /* Optional step */
                final boolean doCachecheck = false;
                if (!doCachecheck) {
                    break cacheCheck;
                }
                /* Items need to be cached before they can be downloaded -> Check cache status */
                logger.info("Checking downloadability of internal file_id: " + file_id + " | hash: " + hash);
                final UrlQuery query_checkcached = new UrlQuery();
                query_checkcached.appendEncoded("hash", hash);
                query_checkcached.appendEncoded("format", "object");
                /* 2024-06-13: Use this for now to avoid problems with outdated response from this API call. */
                query_checkcached.appendEncoded("bypass_cache", "true");
                final Request req_checkcached = br.createGetRequest(API_BASE + "/webdl/checkcached?" + query_checkcached.toString());
                final Object resp_checkcached = this.callAPI(br, req_checkcached, account, link);
                Map<String, Object> cachemap = null;
                if (resp_checkcached instanceof Map) {
                    cachemap = (Map<String, Object>) ((Map<String, Object>) resp_checkcached).get(hash);
                } else {
                    /* Assume we got a list -> Find cache-map */
                    final List<Map<String, Object>> cacheitems = (List<Map<String, Object>>) resp_checkcached;
                    for (final Map<String, Object> cacheitem : cacheitems) {
                        final String thishash = cacheitem.get("hash").toString();
                        if (thishash.equals(hash)) {
                            cachemap = cacheitem;
                            break;
                        }
                    }
                }
                if (cachemap == null) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Serverside download is not yet ready", 30 * 1000);
                }
            }
            logger.info("Trying to init download for internal file_id: " + file_id + " | hash: " + hash);
            final UrlQuery query_requestdl = new UrlQuery();
            query_requestdl.appendEncoded("token", this.getApikey(account));
            query_requestdl.appendEncoded("web_id", file_id);
            query_requestdl.appendEncoded("zip", "false");
            final Request req_requestdl = br.createGetRequest(API_BASE + "/webdl/requestdl?" + query_requestdl.toString());
            dllink = this.callAPI(br, req_requestdl, account, link).toString();
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                mhm.handleErrorGeneric(account, link, "Final downloadlink did not lead to file content", 50, 5 * 60 * 1000l);
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
            /* Store directurl so we can re-use it next time. */
            link.setProperty(directlinkproperty, dllink);
        }
        dl.startDownload();
    }

    /**
     * Fixed timestamps given by API so that we got milliseconds instead of nanoseconds. </br>
     * 2024-11-12: Problems have been fixed server side so this workaround should not be needed anymore.
     */
    private String fixDateString(final String dateStr) {
        /* 2024-07-10: They sometimes even return timestamps with 5 digits milli/nanosecs e.g.: 2024-07-05T13:57:33.76273+00:00 */
        // timestamps also have nano seconds?! SimpleDateFormat lenien=true will then parse xxxxxx ms and convert to secs/minutes...
        return dateStr.replaceFirst("(\\.\\d{4,6})", ".000");
    }

    private long parseTimeStamp(final String dateStr) {
        if (dateStr == null) {
            return -1;
        }
        final String dateStrFixed = fixDateString(dateStr);
        if (dateStrFixed.matches("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}.\\d{3}\\+\\d{2}:\\d{2}")) {
            return TimeFormatter.getMilliSeconds(dateStrFixed, "yyyy-MM-dd'T'HH:mm:ss.SSSXXX", Locale.ENGLISH);
        } else if (dateStrFixed.matches("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\+\\d{2}:\\d{2}")) {
            return TimeFormatter.getMilliSeconds(dateStrFixed, "yyyy-MM-dd'T'HH:mm:ssXXX", Locale.ENGLISH);
        } else if (dateStrFixed.matches("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}")) {
            /* 2024-11-12 */
            return TimeFormatter.getMilliSeconds(dateStrFixed, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
        } else if (dateStrFixed.matches("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z")) {
            /* 2024-11-15 */
            return TimeFormatter.getMilliSeconds(dateStrFixed, "yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.ENGLISH);
        } else {
            /* Fallback */
            return TimeFormatter.getMilliSeconds(dateStrFixed, "yyyy-MM-dd'T'HH:mm:ss.SSSX", Locale.ENGLISH);
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> user = login(account, true);
        final String premium_expires_at = (String) user.get("premium_expires_at");
        final Object total_bytes_downloaded = user.get("total_bytes_downloaded");
        final Map<String, Object> user_settings = (Map<String, Object>) user.get("settings");
        /* Use shorter timeout than usually to make notification system work in a better way (see end of this function). */
        account.setRefreshTimeout(5 * 60 * 1000l);
        /**
         * In GUI, used only needs to enter API key so we'll set the username for him here. </br>
         * This is also important to be able to keep the user from adding the same account multiple times.
         */
        account.setUser(user.get("email").toString());
        /* 0 = free, 1 = standard, 2 = pro */
        final int planID = ((Number) user.get("plan")).intValue();
        final String created_at = (String) user.get("created_at");
        if (created_at != null) {
            ai.setCreateTime(parseTimeStamp(created_at));
        }
        long premiumExpireTimestamp = -1;
        if (premium_expires_at != null) {
            premiumExpireTimestamp = parseTimeStamp(premium_expires_at);
        }
        if (premiumExpireTimestamp > System.currentTimeMillis()) {
            account.setType(AccountType.PREMIUM);
            ai.setValidUntil(premiumExpireTimestamp, br);
        } else {
            account.setType(AccountType.FREE);
            ai.setExpired(true);
        }
        final String subscribedStr;
        if (Boolean.TRUE.equals(user.get("is_subscribed"))) {
            subscribedStr = "Yes";
        } else {
            subscribedStr = "No";
        }
        final String totalBytesDownloadedText;
        if (total_bytes_downloaded instanceof Number) {
            final SIZEUNIT maxSizeUnit = (SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue();
            totalBytesDownloadedText = SIZEUNIT.formatValue(maxSizeUnit, ((Number) total_bytes_downloaded).longValue());
        } else {
            totalBytesDownloadedText = "N/A";
        }
        String statusText = account.getType().getLabel() + " | Subscribed: " + subscribedStr + " | Dl so far: " + totalBytesDownloadedText;
        ai.setStatus(statusText);
        /* Obtain list of supported hosts */
        final Request req_hosters = br.createGetRequest(API_BASE + "/webdl/hosters");
        final List<Map<String, Object>> hosterlist = (List<Map<String, Object>>) this.callAPI(br, req_hosters, account, null);
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (final Map<String, Object> hosterlistitem : hosterlist) {
            final String domain = hosterlistitem.get("domain").toString();
            final String name = hosterlistitem.get("name").toString();
            if (domain.equalsIgnoreCase("usenet") || name.equalsIgnoreCase("usenet")) {
                /* Usenet is added/handled down below */
                continue;
            }
            final MultiHostHost mhost = new MultiHostHost(domain);
            mhost.setName(name);
            if (Boolean.FALSE.equals(hosterlistitem.get("status"))) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            supportedhosts.add(mhost);
        }
        final MultiHostHost usenet = new MultiHostHost("usenet");
        if (planID == 2) {
            /* Obtain usenet login credentials if user owns a pro account */
            try {
                final Request req_usenet = br.createGetRequest(API_BASE + "/usenet/provider/account");
                final Map<String, Object> usenet_data = (Map<String, Object>) this.callAPI(br, req_usenet, account, null);
                final int maxConnections = ((Number) usenet_data.get("connections")).intValue();
                account.setProperty(PROPERTY_ACCOUNT_USENET_USERNAME, usenet_data.get("username"));
                account.setProperty(PROPERTY_ACCOUNT_USENET_PASSWORD, usenet_data.get("password"));
                account.setProperty(PROPERTY_ACCOUNT_USENET_SERVER, usenet_data.get("host"));
                account.setProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET, maxConnections); // 25
                usenet.setStatusText(usenet_data.get("limitations").toString());
                // usenet.setMaxChunks(maxConnections);
                usenet.setMaxDownloads(maxConnections);
            } catch (final InterruptedException ie) {
                throw ie;
            } catch (final Exception e) {
                logger.log(e);
                final String reason = e.getMessage();
                String msg = "Failed to obtain Usenet information from API";
                if (reason != null) {
                    msg += " | API error: " + msg;
                }
                usenet.setStatus(MultihosterHostStatus.DEACTIVATED_JDOWNLOADER);
                usenet.setStatusText(msg);
            }
        } else {
            /* Usenet not supported [anymore] */
            account.removeProperty(PROPERTY_ACCOUNT_USENET_USERNAME);
            account.removeProperty(PROPERTY_ACCOUNT_USENET_PASSWORD);
            account.removeProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET);
            usenet.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST_NOT_FOR_THIS_ACCOUNT_TYPE);
            usenet.setStatusText("Not supported in your current plan");
        }
        supportedhosts.add(usenet);
        ai.setMultiHostSupportV2(this, supportedhosts);
        /* Handle notifications */
        notificationHandling: if (true) {
            if (!Boolean.TRUE.equals(user_settings.get("jdownloader_notifications"))) {
                logger.info("User has disabled notifications in web interface of " + getHost());
                break notificationHandling;
            } else if (BubbleNotifyEnabledState.NEVER.equals(CFG_BUBBLE.CFG.getBubbleNotifyEnabledState())) {
                logger.info("User has disabled BubbleNotifications globally in JDownloader settings");
                break notificationHandling;
            }
            long highestNotificationTimestamp = 0;
            try {
                final Request req_notifications = br.createGetRequest(API_BASE + "/notifications/mynotifications");
                final List<Map<String, Object>> notifications = (List<Map<String, Object>>) this.callAPI(br, req_notifications, account, null);
                final long timestampNotificationsDisplayed = account.getLongProperty(PROPERTY_ACCOUNT_NOTIFICATIONS_DISPLAYED_UNTIL_TIMESTAMP, 0);
                /* Note: 2024-06-13: There is no serverside limit of number of notofications that can be returned here. */
                int numberofNotificationsActuallyDisplayed = 0;
                for (final Map<String, Object> notification : notifications) {
                    final long notification_created_at = parseTimeStamp(notification.get("created_at").toString());
                    if (notification_created_at > highestNotificationTimestamp) {
                        highestNotificationTimestamp = notification_created_at;
                    }
                    if (notification_created_at <= timestampNotificationsDisplayed) {
                        /* Assume that this notification has already been displayed in the past. */
                        continue;
                    }
                    logger.info("Displaying notification with ID: " + notification.get("id"));
                    displayBubbleNotification(notification.get("title").toString(), notification.get("message").toString());
                    numberofNotificationsActuallyDisplayed++;
                    if (timestampNotificationsDisplayed == 0 && numberofNotificationsActuallyDisplayed >= 5) {
                        logger.info("First time we are displaying notifications for this account. Let's not spam the user with all past notifications he has.");
                        break;
                    }
                }
                logger.info("Total number of notifications: " + notifications.size() + " | Displayed this run: " + numberofNotificationsActuallyDisplayed);
                if (numberofNotificationsActuallyDisplayed == 0) {
                    /* Zero notifications were displayed to use -> Do not clear notifications */
                    break notificationHandling;
                }
                /* Save this timestamp so we are able to know to which time we've already displayed notifications. */
                account.setProperty(PROPERTY_ACCOUNT_NOTIFICATIONS_DISPLAYED_UNTIL_TIMESTAMP, highestNotificationTimestamp);
                /* Clear all notifications ("mark as read") */
                final Request req_clear_all_notifications = br.createPostRequest(API_BASE + "/notifications/clear", "");
                this.callAPI(br, req_clear_all_notifications, account, null);
                logger.info("Successfully cleared notifications");
            } catch (final InterruptedException ie) {
                throw ie;
            } catch (final Exception e) {
                /*
                 * Ignore exception as the important part [the account-check] was successful and we don't care about a failure at this
                 * point.
                 */
                logger.log(e);
                logger.warning("Exception happened in notification handling");
            }
        }
        account.setConcurrentUsePossible(true);
        return ai;
    }

    @Override
    protected void displayBubbleNotification(String title, final String text) {
        title = "TorBox | " + title;
        super.displayBubbleNotification(title, text);
    }

    private Map<String, Object> login(final Account account, final boolean validateLogins) throws IOException, PluginException, InterruptedException {
        synchronized (account) {
            setLoginHeaders(br, account);
            if (!validateLogins) {
                /* Do not validate logins */
                return null;
            }
            final Request loginreq = br.createGetRequest(API_BASE + "/user/me?settings=true");
            final Map<String, Object> resp = (Map<String, Object>) this.callAPI(br, loginreq, account, null);
            return resp;
        }
    }

    private void setLoginHeaders(final Object obj, final Account account) {
        if (obj instanceof Request) {
            ((Request) obj).getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + getApikey(account));
        } else {
            ((Browser) obj).getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + getApikey(account));
        }
    }

    /* API docs: https://api-docs.torbox.app/ */
    private Object callAPI(final Browser br, final Request req, final Account account, final DownloadLink link) throws IOException, PluginException, InterruptedException {
        setLoginHeaders(req, account);
        br.getPage(req);
        return checkErrors(br, account, link);
    }

    private String getApikey(final Account account) {
        return correctPassword(account.getPass());
    }

    private Object checkErrors(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        try {
            final Object jsonO = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
            if (jsonO == null || !(jsonO instanceof Map)) {
                return jsonO;
            }
            handleErrorMap(account, link, (Map<String, Object>) jsonO);
            if (jsonO instanceof Map) {
                final Map<String, Object> thismap = (Map<String, Object>) jsonO;
                return thismap.get("data");
            }
            return jsonO;
        } catch (final JSonMapperException jme) {
            final String errortext = "Bad API response";
            if (link != null) {
                mhm.handleErrorGeneric(account, link, errortext, 50, 5 * 60 * 1000l);
            } else {
                throw Exceptions.addSuppressed(new AccountUnavailableException(errortext, 1 * 60 * 1000l), jme);
            }
        }
        return null;
    }

    private void handleErrorMap(final Account account, final DownloadLink link, final Map<String, Object> entries) throws PluginException, InterruptedException {
        final Boolean successO = (Boolean) entries.get("success");
        if (Boolean.TRUE.equals(successO)) {
            /* No error */
            return;
        }
        // TODO: Add better errorhandling
        /*
         * List of possible error codes/strings: </br>
         * https://www.postman.com/wamy-dev/workspace/torbox/collection/29572726-4244cdaf-ece6-4b6d-a2ca-463a3af48f54
         */
        // final Object error = entries.get("error");
        final String errormsg = entries.get("detail").toString();
        if (link != null) {
            /* E.g. {"success":false,"detail":"Failed to request web download. Please try again later.","data":null} */
            mhm.handleErrorGeneric(account, link, errormsg, 50, 1 * 60 * 1000l);
        } else {
            throw new AccountInvalidException(errormsg);
        }
    }

    private static String correctPassword(final String pw) {
        if (pw != null) {
            return pw.trim().replace("-", "");
        } else {
            return null;
        }
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/settings";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (correctPassword(str).matches("[a-f0-9]{32}")) {
            return true;
        } else {
            return false;
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

    @Override
    protected UsenetServer getUseNetServer(final Account account) throws Exception {
        final UsenetServer userv = new UsenetServer(account.getStringProperty(PROPERTY_ACCOUNT_USENET_SERVER), 563, true);
        userv.setConnections(account.getIntegerProperty(PROPERTY_ACCOUNT_USENET_SERVER, 1));
        return userv;
    }
    // @Override
    // public List<UsenetServer> getAvailableUsenetServer() {
    // // TODO: Obtain this data from API
    // final List<UsenetServer> ret = new ArrayList<UsenetServer>();
    // ret.addAll(UsenetServer.createServerList("usenet.torbox.app", true, 563));
    // return ret;
    // }

    @Override
    protected int getMaxSimultanDownload(final DownloadLink link, final Account account) {
        if (account != null && link != null && isUsenetLink(link)) {
            return account.getIntegerProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET, 1);
        } else {
            return Integer.MAX_VALUE;
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }
}