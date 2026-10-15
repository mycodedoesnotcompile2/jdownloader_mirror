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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
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
import jd.plugins.components.MultiHosterManagement;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.gui.notify.gui.BubbleNotifyConfig.BubbleNotifyEnabledState;
import org.jdownloader.gui.notify.gui.CFG_BUBBLE;
import org.jdownloader.plugins.components.usenet.UsenetServer;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

@HostPlugin(revision = "$Revision: 53625 $", interfaceVersion = 3, names = { "torbox.app" }, urls = { "" })
public class TorboxApp extends UseNet {
    /* Docs: https://api-docs.torbox.app/ */
    public static final String           API_BASE                                                 = "https://api.torbox.app/v1/api";
    private static final long            WEBDL_RETRY_DELAY                                        = TimeUnit.SECONDS.toMillis(30);
    private static MultiHosterManagement mhm                                                      = new MultiHosterManagement("torbox.app");
    public static final String           PROPERTY_IS_INFECTED                                     = "is_infected";
    public static final String           PROPERTY_DOWNLOAD_TYPE                                   = "download_type";
    public static final String           PROPERTY_DOWNLOAD_ID                                     = "download_download_id";
    public static final String           PROPERTY_DOWNLOAD_FILE_ID                                = "download_file_id";
    public static final String           PROPERTY_DOWNLOAD_FILE_INTERNAL_NAME                     = "download_file_internal_name";
    public static final String           PROPERTY_DOWNLOAD_TYPE_torrents                          = "torrents";
    public static final String           PROPERTY_DOWNLOAD_TYPE_web_downloads                     = "web_downloads";
    public static final String           PROPERTY_DOWNLOAD_TYPE_usenet_downloads                  = "usenet_downloads";
    private static final String          PROPERTY_MULTIHOST_FILE_ID                               = "torbox_handlemultihost_file_id";
    private static final String          PROPERTY_MULTIHOST_HASH                                  = "torbox_handlemultihost_hash";
    private static final String          PROPERTY_MULTIHOST_WEB_DOWNLOAD_FAILED_TIMESTAMP         = "torbox_handlemultihost_web_download_failed_ts";
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
    public String getAGBLink() {
        return "https://" + this.getHost() + "/terms";
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
    public String getPluginContentURL(final DownloadLink link) {
        return "https://" + getHost() + "/download?id=" + link.getStringProperty(PROPERTY_DOWNLOAD_ID) + "&type=" + link.getStringProperty(PROPERTY_DOWNLOAD_TYPE) + "&name=" + Encoding.urlEncode(link.getStringProperty(PROPERTY_DOWNLOAD_FILE_INTERNAL_NAME, "PROPERTY_MISSING"));
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        return this.getHost() + "://type/" + link.getStringProperty(PROPERTY_DOWNLOAD_TYPE) + "/download_id/" + link.getStringProperty(PROPERTY_DOWNLOAD_ID) + "/file_id/" + link.getStringProperty(PROPERTY_DOWNLOAD_FILE_ID);
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
         * 2024-06-12: Max 16 total connections according to admin. </br> We'll be doing it this way right know, knowing that the user can
         * easily try to exceed that limit with JDownloader.
         */
        return -16;
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
            /* Download selfhosted file */
            final String file_id = link.getStringProperty(PROPERTY_DOWNLOAD_FILE_ID);
            final String dl_type = link.getStringProperty(PROPERTY_DOWNLOAD_TYPE);
            final String dl_id = link.getStringProperty(PROPERTY_DOWNLOAD_ID);
            // final String dl_type_api_path = getDownloadTypeAPIPath(link.getStringProperty(PROPERTY_DOWNLOAD_TYPE));
            final Request req_requestdl;
            if (dl_type.equalsIgnoreCase("torrents")) {
                final UrlQuery query_requestdl = new UrlQuery();
                query_requestdl.appendEncoded("token", this.getApikey(account));
                query_requestdl.appendEncoded("torrent_id", dl_id);
                query_requestdl.appendEncoded("file_id", file_id);
                query_requestdl.appendEncoded("zip", "false");
                req_requestdl = br.createGetRequest(API_BASE + "/torrents/requestdl?" + query_requestdl.toString());
            } else if (dl_type.equalsIgnoreCase("web_downloads")) {
                final UrlQuery query_requestdl = new UrlQuery();
                query_requestdl.appendEncoded("token", this.getApikey(account));
                query_requestdl.appendEncoded("web_id", dl_id);
                query_requestdl.appendEncoded("file_id", file_id);
                query_requestdl.appendEncoded("zip", "false");
                req_requestdl = br.createGetRequest(API_BASE + "/webdl/requestdl?" + query_requestdl.toString());
            } else if (dl_type.equalsIgnoreCase("usenet_downloads")) {
                final UrlQuery query_requestdl = new UrlQuery();
                query_requestdl.appendEncoded("token", this.getApikey(account));
                query_requestdl.appendEncoded("usenet_id", dl_id);
                query_requestdl.appendEncoded("file_id", file_id);
                query_requestdl.appendEncoded("zip", "false");
                req_requestdl = br.createGetRequest(API_BASE + "/usenet/requestdl?" + query_requestdl.toString());
            } else {
                throw new IllegalArgumentException("Unexpected download_type");
            }
            final String dllink = this.callAPI(br, req_requestdl, account, link).toString();
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                checkErrors(br, account, link);
                /* This code should never be reached */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "WTF");
            }
            dl.startDownload();
        }
    }

    private String getPropertyKey(final String property) {
        return this.getHost() + "_" + property;
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        if (isUsenetLink(link)) {
            super.handleMultiHost(link, account);
            return;
        }
        this.login(account, false);
        final String directlinkproperty = this.getPropertyKey("directlink");
        String storedDirecturl = link.getStringProperty(directlinkproperty);
        final String accountProperty = getPropertyKey("webdl_account");
        final String accountID = String.valueOf(account.getId().getID());
        final String previousAccount = link.getStringProperty(accountProperty);
        if (previousAccount != null && !StringUtils.equals(previousAccount, accountID)) {
            resetDownloadlink(link);
            storedDirecturl = null;
        }
        link.setProperty(accountProperty, accountID);
        /* Adopt job identifiers from the previous local compatibility patch. */
        if (previousAccount != null && StringUtils.equals(previousAccount, accountID) && link.getStringProperty(PROPERTY_MULTIHOST_FILE_ID) == null) {
            final String legacyID = link.getStringProperty(getPropertyKey("webdl_id"));
            final String legacyHash = link.getStringProperty(getPropertyKey("webdl_hash"));
            if (legacyID != null && legacyHash != null) {
                link.setProperty(PROPERTY_MULTIHOST_FILE_ID, legacyID);
                link.setProperty(PROPERTY_MULTIHOST_HASH, legacyHash);
            }
        }

        final String dllink;
        find_direct_download_link: {
            if (storedDirecturl != null) {
                logger.info("Trying to re-use stored directurl");
                dllink = storedDirecturl;
                break find_direct_download_link;
            }
            String passCode = link.getDownloadPassword();
            if (link.isPasswordProtected() && passCode == null) {
                passCode = getUserInput("Password?", link);
            }
            final String file_id = resolveWebDownload(link, account, passCode);
            logger.info("Requesting the ready TorBox web download");
            final UrlQuery query_requestdl = new UrlQuery();
            query_requestdl.appendEncoded("token", this.getApikey(account));
            query_requestdl.appendEncoded("web_id", file_id);
            query_requestdl.appendEncoded("zip", "false");
            final Request req_requestdl = br.createGetRequest(API_BASE + "/webdl/requestdl?" + query_requestdl.toString());
            dllink = this.callAPI(br, req_requestdl, account, link).toString();
            if (passCode != null && link.getDownloadPassword() == null) {
                logger.info("Storing download password for future retries");
                /* Store password so we can re-use it next time */
                link.setDownloadPassword(passCode);
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                checkErrors(br, account, link);
                /* This code should never be reached */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "WTF");
            }
        } catch (final InterruptedException ie) {
            throw ie;
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

    private String resolveWebDownload(final DownloadLink link, final Account account, final String passCode) throws IOException, PluginException, InterruptedException {
        String fileID = link.getStringProperty(PROPERTY_MULTIHOST_FILE_ID);
        if (fileID == null || link.getStringProperty(PROPERTY_MULTIHOST_HASH) == null) {
            synchronized (account) {
                final String sourceURL = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
                final Object response = callAPI(br, br.createGetRequest(API_BASE + "/webdl/mylist?bypass_cache=true"), account, link);
                Map<String, Object> matching = null;
                for (final Map<String, Object> item : webDownloadItems(response)) {
                    if (sourceURL.equals(item.get("original_url")) && item.get("id") != null && item.get("hash") != null) {
                        matching = item;
                        if (Boolean.TRUE.equals(item.get("download_present"))) {
                            break;
                        }
                    }
                }
                if (matching != null) {
                    fileID = matching.get("id").toString();
                    link.setProperty(PROPERTY_MULTIHOST_FILE_ID, fileID);
                    link.setProperty(PROPERTY_MULTIHOST_HASH, matching.get("hash").toString());
                    link.setProperty(getPropertyKey("webdl_created"), System.currentTimeMillis());
                } else {
                    final long delay = account.getLongProperty(getPropertyKey("webdl_next_create"), 0) - System.currentTimeMillis();
                    if (delay > 0) {
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "TorBox: waiting for the web download creation interval", delay);
                    }
                    /* Shared reservation; failed create calls also consume API quota. */
                    account.setProperty(getPropertyKey("webdl_next_create"), System.currentTimeMillis() + TimeUnit.SECONDS.toMillis(65));
                    final UrlQuery query = new UrlQuery();
                    query.appendEncoded("link", sourceURL);
                    if (passCode != null) {
                        query.appendEncoded("password", passCode);
                    }
                    final Map<String, Object> created = (Map<String, Object>) callAPI(br, br.createPostRequest(API_BASE + "/webdl/createwebdownload", query), account, link);
                    if (created == null || created.get("webdownload_id") == null || created.get("hash") == null) {
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "TorBox: missing remote download identifier", WEBDL_RETRY_DELAY);
                    }
                    fileID = created.get("webdownload_id").toString();
                    link.setProperty(PROPERTY_MULTIHOST_FILE_ID, fileID);
                    link.setProperty(PROPERTY_MULTIHOST_HASH, created.get("hash").toString());
                    link.setProperty(getPropertyKey("webdl_created"), System.currentTimeMillis());
                }
            }
        }
        return waitForWebDownload(link, account, fileID);
    }

    @SuppressWarnings("unchecked")
    private List<Map<String, Object>> webDownloadItems(final Object response) throws PluginException {
        if (response == null) {
            return Collections.emptyList();
        } else if (response instanceof Map && ((Map<?, ?>) response).get("id") != null) {
            return Collections.singletonList((Map<String, Object>) response);
        } else if (response instanceof List) {
            final List<Map<String, Object>> items = new ArrayList<Map<String, Object>>();
            for (final Object item : (List<?>) response) {
                items.add((Map<String, Object>) item);
            }
            return items;
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    private String waitForWebDownload(final DownloadLink link, final Account account, final String fileID) throws IOException, PluginException, InterruptedException {
        final String hash = link.getStringProperty(PROPERTY_MULTIHOST_HASH);
        final Object response = callAPI(br, br.createGetRequest(API_BASE + "/webdl/mylist?bypass_cache=true"), account, link);
        for (final Map<String, Object> item : webDownloadItems(response)) {
            if (fileID.equals(String.valueOf(item.get("id"))) || hash != null && hash.equals(item.get("hash"))) {
                if (item.get("id") == null) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "TorBox: missing remote download identifier", WEBDL_RETRY_DELAY);
                }
                final String id = item.get("id").toString();
                link.setProperty(PROPERTY_MULTIHOST_FILE_ID, id);
                if (Boolean.TRUE.equals(item.get("download_present")) && (Boolean.TRUE.equals(item.get("cached")) || Boolean.TRUE.equals(item.get("download_finished")))) {
                    return id;
                }
                final String state = String.valueOf(item.get("download_state"));
                final Object error = item.get("error");
                if (StringUtils.startsWithCaseInsensitive(state, "failed") || error != null && !StringUtils.isEmpty(error.toString())) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "TorBox web download failed: " + (error != null ? error : state), TimeUnit.HOURS.toMillis(3));
                }
                final Number progress = (Number) item.get("progress");
                final Number eta = (Number) item.get("eta");
                final String progressText = progress != null ? String.format("%.2f%%", progress.doubleValue() * 100) : "Unknown";
                final long delay = eta != null ? Math.max(15, Math.min(60, eta.longValue())) * 1000l : WEBDL_RETRY_DELAY;
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "TorBox is preparing the download | State: " + state + " | Progress: " + progressText, delay);
            }
        }
        final Object queued = callAPI(br, br.createGetRequest(API_BASE + "/queued/getqueued?type=webdl&bypass_cache=true"), account, link);
        for (final Map<String, Object> item : webDownloadItems(queued)) {
            if (hash != null && hash.equals(item.get("hash"))) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "TorBox is waiting for an available account slot", WEBDL_RETRY_DELAY);
            }
        }
        if (System.currentTimeMillis() - link.getLongProperty(getPropertyKey("webdl_created"), 0) < TimeUnit.MINUTES.toMillis(1)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Waiting for TorBox download registration", WEBDL_RETRY_DELAY);
        }
        resetDownloadlink(link);
        throw new PluginException(LinkStatus.ERROR_RETRY, "TorBox job no longer exists");
    }

    /**
     * Fixed timestamps given by API so that we got milliseconds instead of nanoseconds. </br> 2024-11-12: Problems have been fixed server
     * side so this workaround should not be needed anymore.
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
        final Map<String, Object> user = login(account, true);
        final AccountInfo ai = new AccountInfo();
        final String premium_expires_at = (String) user.get("premium_expires_at");
        final Object total_bytes_downloaded = user.get("total_bytes_downloaded");
        final Map<String, Object> user_settings = (Map<String, Object>) user.get("settings");
        /* Use shorter timeout than usually to make notification system work in a better way (see end of this function). */
        account.setRefreshTimeout(TimeUnit.MINUTES.toMillis(5));
        /**
         * In GUI, used only needs to enter API key so we'll set the username for him here. </br> This is also important to be able to keep
         * the user from adding the same account multiple times.
         */
        account.setUser(user.get("email").toString());
        /* 0 = free, 1 = standard, 2 = pro */
        final int planID = ((Number) user.get("plan")).intValue();
        final String created_at = (String) user.get("created_at");
        if (created_at != null) {
            ai.setCreateTime(parseTimeStamp(created_at));
        }
        final long premiumExpireTimestamp = parseTimeStamp(premium_expires_at);
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
            final SIZEUNIT maxSizeUnit = CFG_GUI.MAX_SIZE_UNIT.getValue();
            totalBytesDownloadedText = SIZEUNIT.formatValue(maxSizeUnit, ((Number) total_bytes_downloaded).longValue());
        } else {
            totalBytesDownloadedText = "N/A";
        }
        String statusText = account.getType().getLabel() + " | Subscribed: " + subscribedStr + " | Dl so far: " + totalBytesDownloadedText;
        ai.setStatus(statusText);
        /* Obtain list of supported hosts */
        final Request req_hosters = br.createGetRequest(API_BASE + "/webdl/hosters");
        final List<Map<String, Object>> hosts = (List<Map<String, Object>>) this.callAPI(br, req_hosters, account, null);
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        boolean supports_usenet = false;
        hosterloop: for (final Map<String, Object> host : hosts) {
            List<String> domains = (List<String>) host.get("domains");
            final String name = host.get("name").toString();
            final String note = (String) host.get("note");
            final long daily_link_limit = ((Number) host.get("daily_link_limit")).longValue();
            final long daily_link_used = ((Number) host.get("daily_link_used")).longValue();
            final long daily_bandwidth_limit = ((Number) host.get("daily_bandwidth_limit")).longValue();
            final long daily_bandwidth_used = ((Number) host.get("daily_bandwidth_used")).longValue();
            final MultiHostHost mhost = new MultiHostHost();
            if (domains == null) {
                /* Fallback */
                logger.warning("WTF bad json response for item: " + name);
                domains = new ArrayList<String>();
                domains.add(name);
            }
            for (final String domain : domains) {
                if (domain.equalsIgnoreCase("usenet") || name.equalsIgnoreCase("usenet")) {
                    /* Usenet is added/handled down below */
                    supports_usenet = true;
                    continue hosterloop;
                }
                mhost.addDomain(domain);
            }
            mhost.setName(name);
            if (Boolean.FALSE.equals(host.get("status"))) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            if (!StringUtils.isEmpty(note)) {
                mhost.setStatusText(note);
            }
            if (daily_link_limit != 0) {
                mhost.setLinksMax(daily_link_limit);
                mhost.setLinksLeft(daily_link_limit - daily_link_used);
            }
            if (daily_bandwidth_limit != 0) {
                mhost.setTrafficMax(daily_bandwidth_limit);
                mhost.setTrafficLeft(daily_bandwidth_limit - daily_bandwidth_used);
            }
            supportedhosts.add(mhost);
        }
        if (supports_usenet) {
            /**
             * Looks like usenet is supported -> Obtain login credentials for usenet client <br>
             * 2026-05-19: Looks like a longer time ago, direct-usenet support has been removed so the subsequent request will most likely
             * fail. <br>
             * I'm still leaving it in the code as technically they could re-enable it at any point of time.
             */
            final MultiHostHost usenet = new MultiHostHost("usenet");
            if (planID == 2) {
                /* Obtain usenet login credentials if user owns a pro account */
                try {
                    final Request req_usenet = br.createGetRequest(API_BASE + "/usenet/provider/account");
                    final Map<String, Object> usenet_data = (Map<String, Object>) this.callAPI(br, req_usenet, account, null);
                    final int maxConnections = ((Number) usenet_data.get("connections")).intValue();
                    account.setProperty(PROPERTY_ACCOUNT_USENET_USERNAME, usenet_data.get("username").toString());
                    account.setProperty(PROPERTY_ACCOUNT_USENET_PASSWORD, usenet_data.get("password").toString());
                    account.setProperty(PROPERTY_ACCOUNT_USENET_SERVER, usenet_data.get("host").toString());
                    account.setProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET, maxConnections); // default = 25
                    if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                        usenet.setStatusText("Connections: " + maxConnections);
                    }
                    usenet.setMaxDownloads(maxConnections);
                } catch (final InterruptedException ie) {
                    throw ie;
                } catch (final Exception e) {
                    logger.log(e);
                    final String reason = e.getMessage();
                    String msg = "Failed to obtain Usenet information from API";
                    if (reason != null) {
                        msg += " | API error: " + reason;
                    }
                    usenet.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                    usenet.setStatusText(msg);
                }
            } else {
                /* Usenet not supported [anymore] */
                removeUsenetProperties(account);
                usenet.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST_NOT_FOR_THIS_ACCOUNT_TYPE);
                usenet.setStatusText("Not supported in your current plan");
            }
            supportedhosts.add(usenet);
        } else {
            /* Usenet not supported [anymore] */
            removeUsenetProperties(account);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        /* Handle notifications */
        notificationHandling: if (true) {
            if (BubbleNotifyEnabledState.NEVER.equals(CFG_BUBBLE.CFG.getBubbleNotifyEnabledState())) {
                /* User has disabled BubbleNotifications globally in JDownloader settings */
                break notificationHandling;
            } else if (!Boolean.TRUE.equals(user_settings.get("jdownloader_notifications"))) {
                logger.info("User has disabled notifications in web interface of " + getHost());
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

    private void removeUsenetProperties(final Account account) {
        account.removeProperty(PROPERTY_ACCOUNT_USENET_USERNAME);
        account.removeProperty(PROPERTY_ACCOUNT_USENET_PASSWORD);
        account.removeProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET);
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
    public Object callAPI(final Browser br, final Request req, final Account account, final DownloadLink link) throws IOException, PluginException, InterruptedException {
        final String cooldownKey = getPropertyKey("api_wait_" + new java.net.URL(req.getUrl()).getPath());
        final long wait = account.getLongProperty(cooldownKey, 0) - System.currentTimeMillis();
        if (wait > 0) {
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "TorBox: waiting for API rate limit reset", wait);
            }
            throw new AccountUnavailableException("TorBox: waiting for API rate limit reset", wait);
        }
        setLoginHeaders(req, account);
        br.getPage(req);
        return checkErrors(br, account, link);
    }

    private String getApikey(final Account account) {
        return account.getPass();
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
                mhm.handleErrorGeneric(account, link, errortext, 50, TimeUnit.MINUTES.toMillis(5));
            } else {
                throw new AccountUnavailableException(jme, errortext, TimeUnit.MINUTES.toMillis(1));
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
        // TODO: Add better error handling
        // TODO: Add more error keys
        /*
         * List of possible error codes/strings: </br>
         * https://www.postman.com/wamy-dev/workspace/torbox/collection/29572726-4244cdaf-ece6-4b6d-a2ca-463a3af48f54
         */
        final String errorcode = (String) entries.get("error");
        final String detail = entries.get("detail") instanceof String ? (String) entries.get("detail") : "API request failed";
        final String errormsg = "TorBox" + (errorcode != null ? " [" + errorcode + "]" : "") + ": " + detail;
        final HashSet<String> accountErrorsPermanent = new HashSet<String>();
        accountErrorsPermanent.add("BAD_TOKEN");
        accountErrorsPermanent.add("AUTH_ERROR");
        final HashSet<String> accountErrorsTemporary = new HashSet<String>();
        accountErrorsTemporary.add("ACTIVE_LIMIT");
        final HashSet<String> downloadErrorsHostUnavailable = new HashSet<String>();
        downloadErrorsHostUnavailable.add("UNSUPPORTED_SITE");
        /*
         * E.g. {"success":false,"error":"DOWNLOAD_LIMIT_REACHED",
         * "detail":"You have reached the 10 downloads limit for ddownload.com, ddl.to. Please try again in 24 hours when the limit resets."
         * ,"data":null}
         */
        downloadErrorsHostUnavailable.add("DOWNLOAD_LIMIT_REACHED");
        final HashSet<String> downloadErrorsFileUnavailable = new HashSet<String>();
        /* First check some special cases */
        if (link != null && new Regex(errormsg, ".*This link is password protected.*").patternFind()) {
            link.setPasswordProtected(true);
            if (link.getDownloadPassword() != null) {
                link.setDownloadPassword(null);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
            } else {
                throw new PluginException(LinkStatus.ERROR_RETRY, "Link is password protected");
            }
        }
        if (("RATE_LIMIT_EXCEEDED".equals(errorcode) || "60 per 1 hour".equals(detail))) {
            long retryDelay = detail.contains("hour") ? TimeUnit.HOURS.toMillis(1) : TimeUnit.MINUTES.toMillis(1);
            if (br.getHttpConnection() != null) {
                final String retryAfter = br.getHttpConnection().getHeaderField("Retry-After");
                if (retryAfter != null && retryAfter.matches("\\d{1,8}")) {
                    retryDelay = Math.max(1000l, Long.parseLong(retryAfter) * 1000l);
                }
            }
            if (br.getRequest() != null) {
                try {
                    final String path = new java.net.URL(br.getRequest().getUrl()).getPath();
                    account.setProperty(getPropertyKey("api_wait_" + path), System.currentTimeMillis() + retryDelay);
                } catch (final java.net.MalformedURLException ignored) {
                }
            }
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, retryDelay);
            }
            throw new AccountUnavailableException(errormsg, retryDelay);
        }
        if (link != null && ("ACTIVE_LIMIT".equals(errorcode) || "DOWNLOAD_LIMIT_REACHED".equals(errorcode) && StringUtils.containsIgnoreCase(detail, "active download limit") || "COOLDOWN_LIMIT".equals(errorcode) || "DOWNLOAD_SERVER_ERROR".equals(errorcode))) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, WEBDL_RETRY_DELAY);
        }
        if (link != null && ("DOWNLOAD_TOO_LARGE".equals(errorcode) || "PLAN_RESTRICTED_FEATURE".equals(errorcode))) {
            throw new PluginException(LinkStatus.ERROR_FATAL, errormsg);
        }
        if (link != null && "LINK_OFFLINE".equals(errorcode)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, errormsg);
        }
        if (accountErrorsPermanent.contains(errorcode)) {
            /* This is the only error which allows us to remove the apikey and re-login. */
            throw new AccountInvalidException(errormsg);
        } else if (accountErrorsTemporary.contains(errorcode)) {
            throw new AccountUnavailableException(errormsg, TimeUnit.MINUTES.toMillis(5));
        } else if (downloadErrorsHostUnavailable.contains(errorcode)) {
            mhm.putError(account, link, TimeUnit.MINUTES.toMillis(5), errormsg);
        } else if (downloadErrorsFileUnavailable.contains(errorcode)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg);
        }
        if (link != null) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, TimeUnit.MINUTES.toMillis(5));
        } else {
            throw new AccountUnavailableException(errormsg, TimeUnit.MINUTES.toMillis(5));
        }
    }

    @Override
    public String updateAccountPassword(final Account account, final String password) {
        if (password == null) {
            return null;
        }
        final String ret = password.trim().replace("-", "");
        return super.updateAccountPassword(account, ret);
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/settings";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (updateAccountPassword(null, str).matches("[a-f0-9]{32}")) {
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
        userv.setConnections(account.getIntegerProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET, 1));
        return userv;
    }

    @Override
    protected int getMaxSimultanDownload(final DownloadLink link, final Account account) {
        if (account != null && link != null && isUsenetLink(link)) {
            return account.getIntegerProperty(PROPERTY_ACCOUNT_MAX_DOWNLOADS_USENET, 1);
        } else {
            return Integer.MAX_VALUE;
        }
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        link.removeProperty(PROPERTY_MULTIHOST_FILE_ID);
        link.removeProperty(PROPERTY_MULTIHOST_HASH);
        link.removeProperty(getPropertyKey("webdl_id"));
        link.removeProperty(getPropertyKey("webdl_hash"));
        link.removeProperty(getPropertyKey("webdl_created"));
        link.removeProperty(getPropertyKey("webdl_account"));
        link.removeProperty(getPropertyKey("directlink"));
        super.resetDownloadlink(link);
    }
}