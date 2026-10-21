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
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.PluginTaskID;
import org.jdownloader.plugins.components.config.FakirdebridNetConfig;
import org.jdownloader.plugins.components.config.FakirdebridNetConfig.BlockDownloadSlotsMode;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
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

@HostPlugin(revision = "$Revision: 53691 $", interfaceVersion = 3, names = { "fakirdebrid.net" }, urls = { "" })
public class FakirdebridNet extends PluginForHost {
    private static final String          API_BASE                                 = "https://api.fakirdebrid.net";
    private static MultiHosterManagement mhm                                      = new MultiHosterManagement("fakirdebrid.net");
    private static final boolean         defaultResume                            = true;
    private static final int             defaultMaxchunks                         = -10;
    private final String                 PROPERTY_RESUME                          = "fakirdebrid_resume";
    private final String                 PROPERTY_MAXCHUNKS                       = "fakirdebrid_maxchunks";
    private final String                 PROPERTY_APILINK                         = "fakirdebrid_apilink";
    private final String                 PROPERTY_APILINK_TIMESTAMP               = "fakirdebrid_apilink_timestamp";
    private static final long            APILINK_MAX_AGE_MILLIS                   = 6 * 60 * 60 * 1000l;
    /* No retry/eta field is known in the API -> fixed poll interval. */
    private static final long            SERVERSIDE_DOWNLOAD_POLL_INTERVAL_MILLIS = 5 * 1000l;
    private static final long            SERVERSIDE_DOWNLOAD_MAX_WAIT_MILLIS      = 10 * 60 * 1000l;

    @SuppressWarnings("deprecation")
    public FakirdebridNet(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/showthread.php?tid=184&pid=1370#pid1370");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/konu-terms-of-use.html";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new AccountRequiredException();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        if (!attemptStoredDownloadurlDownload(link, this.getHost() + "directlink", link.getBooleanProperty(PROPERTY_RESUME, defaultResume), link.getIntegerProperty(PROPERTY_MAXCHUNKS, defaultMaxchunks))) {
            Map<String, Object> entries = null;
            String apilink = null;
            boolean resumable = defaultResume;
            int maxChunks = defaultMaxchunks;
            final String storedApilink = link.getStringProperty(PROPERTY_APILINK);
            if (storedApilink != null && System.currentTimeMillis() - link.getLongProperty(PROPERTY_APILINK_TIMESTAMP, 0) < APILINK_MAX_AGE_MILLIS) {
                logger.info("Re-using stored apilink: " + storedApilink);
                br.getPage(storedApilink);
                final Map<String, Object> storedEntries = parseAPIResponse(account, link);
                if (Boolean.FALSE.equals(storedEntries.get("success"))) {
                    logger.info("Stored apilink is no longer valid: " + storedEntries.get("message"));
                    link.removeProperty(PROPERTY_APILINK);
                    link.removeProperty(PROPERTY_APILINK_TIMESTAMP);
                } else {
                    apilink = storedApilink;
                    resumable = link.getBooleanProperty(PROPERTY_RESUME, defaultResume);
                    maxChunks = link.getIntegerProperty(PROPERTY_MAXCHUNKS, defaultMaxchunks);
                    entries = (Map<String, Object>) storedEntries.get("data");
                }
            }
            if (apilink == null) {
                int counter = 0;
                String passCode = link.getDownloadPassword();
                do {
                    String url = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
                    if (counter > 0) {
                        /* 2nd try: First provided password was invalid or no password has been tried on first attempt. */
                        passCode = getUserInput("Password?", link);
                    }
                    if (passCode != null) {
                        /* Append password like: "|<password>" */
                        url += "|" + passCode;
                    }
                    final UrlQuery query = new UrlQuery();
                    query.appendEncoded("url", url);
                    /* Add filename/siue/hash to query for better server-side mirror matching. */
                    query.appendEncoded("filename", link.getName());
                    final long filesize = link.getKnownDownloadSize();
                    if (filesize >= 0) {
                        /* Only send filesize if it is known. */
                        query.appendEncoded("filesize", Long.toString(filesize));
                    }
                    final String hash = getStrongestHash(link);
                    if (hash != null) {
                        query.appendEncoded("hash", hash);
                    }
                    br.postPage(API_BASE + "/generate/" + Encoding.urlEncode(account.getPass()), query);
                    entries = parseAPIResponse(account, link);
                    final Object errorCodeO = entries.get("code");
                    if (errorCodeO != null && errorCodeO instanceof String && errorCodeO.toString().equalsIgnoreCase("Password_Required")) {
                        logger.info("Password required");
                        counter += 1;
                    } else if (errorCodeO != null && errorCodeO instanceof String && errorCodeO.toString().equalsIgnoreCase("Wrong_Password")) {
                        logger.info("Wrong password");
                        counter += 1;
                    } else {
                        /* No password required or correct password entered */
                        break;
                    }
                } while (counter <= 2);
                /* entries is already parsed above – pass directly to avoid re-parsing */
                entries = handleAPIErrors(entries, account, link);
                if (passCode != null) {
                    /* Save password for the next time. */
                    link.setDownloadPassword(passCode);
                }
                entries = (Map<String, Object>) entries.get("data");
                resumable = ((Boolean) entries.get("resumable")).booleanValue();
                maxChunks = ((Number) entries.get("maxchunks")).intValue();
                if (maxChunks > 1) {
                    maxChunks = -maxChunks;
                }
                apilink = (String) entries.get("link");
                /* Store apilink (and the info needed to re-use it) as DownloadLink properties. */
                link.setProperty(PROPERTY_APILINK, apilink);
                link.setProperty(PROPERTY_APILINK_TIMESTAMP, System.currentTimeMillis());
                link.setProperty(PROPERTY_RESUME, resumable);
                link.setProperty(PROPERTY_MAXCHUNKS, maxChunks);
                br.getPage(apilink);
                entries = handleAPIErrors(account, link);
                entries = (Map<String, Object>) entries.get("data");
            }
            if ("processing".equalsIgnoreCase((String) entries.get("state"))) {
                entries = waitForServersideDownload(entries, apilink, account, link);
            }
            final String state = (String) entries.get("state");
            if (!"completed".equals(state)) {
                mhm.handleErrorGeneric(account, link, "Unknown state: " + state, 50);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String dllink = entries.get("link").toString();
            if (dllink == null) {
                mhm.handleErrorGeneric(account, link, "Failed to find final downloadurl", 10, 5 * 60 * 1000l);
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, resumable, maxChunks);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                mhm.handleErrorGeneric(account, link, "Final downloadurl did not lead to file", 10, 5 * 60 * 1000l);
            }
            link.setProperty(this.getHost() + "directlink", dl.getConnection().getURL().toExternalForm());
            link.setProperty(PROPERTY_RESUME, resumable);
            link.setProperty(PROPERTY_MAXCHUNKS, maxChunks);
        }
        this.dl.startDownload();
    }

    /** Returns the strongest hash known for the given link (SHA256 > SHA1 > MD5) or null if none is available. */
    private String getStrongestHash(final DownloadLink link) {
        final String sha256 = link.getSha256Hash();
        if (!StringUtils.isEmpty(sha256)) {
            return sha256;
        }
        final String sha1 = link.getSha1Hash();
        if (!StringUtils.isEmpty(sha1)) {
            return sha1;
        }
        final String md5 = link.getMD5Hash();
        if (!StringUtils.isEmpty(md5)) {
            return md5;
        }
        return null;
    }

    /**
     * Block download slots for files which have to be downloaded to the multihost's servers first? If this returns true, the download stays
     * active (and occupies its slot) while waiting for the serverside download. If false, the link is set temporarily unavailable so other
     * download candidates can be tried.
     */
    protected boolean blockDownloadSlotsForCloudDownloads(final Account account) {
        return get(getConfigInterface()).getBlockDownloadSlotsForCloudDownloads().getMode() == BlockDownloadSlotsMode.BLOCK;
    }

    /**
     * Polls the given API link until the serverside download is finished. Stops as soon as an API error occurs (thrown by handleAPIErrors),
     * the user aborts or {@link #SERVERSIDE_DOWNLOAD_MAX_WAIT_MILLIS} have passed.
     *
     * @return "data" map of the last API response.
     */
    private Map<String, Object> waitForServersideDownload(Map<String, Object> data, final String apilink, final Account account, final DownloadLink link) throws Exception {
        final AtomicReference<String> statusText = new AtomicReference<String>("Waiting for serverside download...");
        final PluginProgress waitProgress = new PluginProgress(0, 100, null) {
            @Override
            public PluginTaskID getID() {
                return PluginTaskID.WAIT;
            }

            @Override
            public String getMessage(Object requestor) {
                return statusText.get();
            }
        };
        waitProgress.setIcon(new AbstractIcon(IconKey.ICON_WAIT, 16));
        waitProgress.setProgressSource(this);
        final long startTimeStamp = Time.systemIndependentCurrentJVMTimeMillis();
        try {
            while (true) {
                /* "completed" is a percentage with decimals e.g. 9.93 */
                final double percent = ((Number) data.get("completed")).doubleValue();
                final String text = "File is being transferred to fakirdebrid servers: " + String.format(Locale.US, "%.1f", percent) + "%";
                if (!blockDownloadSlotsForCloudDownloads(account)) {
                    /* Throw exception right away so other download candidates will be tried. */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, text, SERVERSIDE_DOWNLOAD_POLL_INTERVAL_MILLIS);
                }
                final long waited = Time.systemIndependentCurrentJVMTimeMillis() - startTimeStamp;
                if (waited >= SERVERSIDE_DOWNLOAD_MAX_WAIT_MILLIS) {
                    logger.info("Serverside download wait timeout after ms: " + waited);
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, text, SERVERSIDE_DOWNLOAD_POLL_INTERVAL_MILLIS);
                }
                /*
                 * Do not use sleep(): It adds its own progress which hides ours. Use 1000 as total so the progress bar has a resolution of
                 * 0.1%.
                 */
                final String speedText = data.get("speed_text").toString();
                statusText.set("Serverside download: " + String.format(Locale.US, "%.1f", percent) + "%" + (StringUtils.isEmpty(speedText) ? "" : " (" + speedText + "/s)"));
                waitProgress.updateValues(Math.round(percent * 10), 1000);
                link.addPluginProgress(waitProgress);
                final long pollEnd = Time.systemIndependentCurrentJVMTimeMillis() + SERVERSIDE_DOWNLOAD_POLL_INTERVAL_MILLIS;
                while (Time.systemIndependentCurrentJVMTimeMillis() < pollEnd) {
                    if (this.isAbort()) {
                        throw new InterruptedException();
                    }
                    Thread.sleep(250);
                }
                br.getPage(apilink);
                data = (Map<String, Object>) handleAPIErrors(account, link).get("data");
                if (!"processing".equalsIgnoreCase((String) data.get("state"))) {
                    return data;
                }
            }
        } finally {
            link.removePluginProgress(waitProgress);
        }
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final String directlinkproperty, final boolean resumable, final int maxchunks) throws Exception {
        final String url = link.getStringProperty(directlinkproperty);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, resumable, maxchunks);
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                return true;
            } else {
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            logger.log(e);
            try {
                dl.getConnection().disconnect();
            } catch (Throwable ignore) {
            }
            return false;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> entries = login(account);
        final Map<String, Object> accountData = (Map<String, Object>) entries.get("account");
        final Map<String, Object> trafficData = (Map<String, Object>) accountData.get("traffic");
        final AccountInfo ai = new AccountInfo();
        ai.setTrafficRefill(false);
        /*
         * User only enters apikey as password and could enter anything into the username field --> Make sure it contains an unique value.
         */
        final String username = (String) accountData.get("username");
        if (!StringUtils.isEmpty(username)) {
            account.setUser(username);
        }
        account.setType(AccountType.PREMIUM);
        /* 2026-10-20: Limit according to admin */
        account.setMaxSimultanDownloads(2);
        final String message = (String) entries.get("message");
        final String planStr = (String) accountData.get("plan");
        if (!StringUtils.isEmpty(message) && !StringUtils.isEmpty(planStr)) {
            ai.setStatus(message + " - Plan: " + planStr);
        } else if (!StringUtils.isEmpty(planStr)) {
            ai.setStatus("Plan: " + planStr);
        } else if (!StringUtils.isEmpty(message)) {
            ai.setStatus(message);
        }
        ai.setTrafficLeft(((Number) trafficData.get("left")).longValue());
        ai.setTrafficMax(((Number) trafficData.get("limit")).longValue());
        final long premiumUntil = ((Number) accountData.get("premium_until")).longValue();
        final long serverTime = ((Number) entries.get("server_time")).longValue();
        if (premiumUntil > 0 && serverTime > 0) {
            /* Use server_time to calculate remaining validity, then anchor to local clock */
            final long validForMs = (premiumUntil - serverTime) * 1000L;
            ai.setValidUntil(System.currentTimeMillis() + validForMs);
            account.setType(AccountType.PREMIUM);
        } else {
            ai.setExpired(true);
            account.setType(AccountType.FREE);
        }
        br.getPage(API_BASE + "/hosts/" + Encoding.urlEncode(account.getPass()));
        final Map<String, Object> supportedhosts_root = handleAPIErrors(account, null);
        final List<Map<String, Object>> arrayHoster = (List<Map<String, Object>>) supportedhosts_root.get("supportedhosts");
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (final Map<String, Object> hostermap : arrayHoster) {
            final String domain = hostermap.get("host").toString();
            final boolean active = ((Boolean) hostermap.get("currently_working")).booleanValue();
            final MultiHostHost mhost = new MultiHostHost(domain);
            if (!active) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            mhost.setResumable(((Boolean) hostermap.get("resumable")).booleanValue());
            mhost.setMaxChunks(((Number) hostermap.get("maxChunks")).intValue());
            mhost.setMaxDownloads(((Number) hostermap.get("maxDownloads")).intValue());
            mhost.setTrafficMax(((Number) hostermap.get("traffixmax_daily")).longValue());
            mhost.setTrafficLeft(((Number) hostermap.get("trafficleft")).longValue());
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        return ai;
    }

    private Map<String, Object> login(final Account account) throws Exception {
        synchronized (account) {
            br.getPage(API_BASE + "/account/" + Encoding.urlEncode(account.getPass()));
            return handleAPIErrors(account, null);
        }
    }

    /** Parses the current br response into a Map. Throws on invalid JSON without inspecting the content. */
    private Map<String, Object> parseAPIResponse(final Account account, final DownloadLink link) throws Exception {
        try {
            return restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            final String msg = "Invalid API response";
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, 1 * 60 * 1000l);
            } else {
                throw new AccountUnavailableException(msg, 1 * 60 * 1000l);
            }
        }
    }

    /** Parses the current br response and delegates to {@link #handleAPIErrors(Map, Account, DownloadLink)}. */
    private Map<String, Object> handleAPIErrors(final Account account, final DownloadLink link) throws Exception {
        return handleAPIErrors(parseAPIResponse(account, link), account, link);
    }

    /**
     * CODE1: Unauthorized </br>
     * CODE2: Unauthorized </br>
     * CODE3: Unauthorized </br>
     * CODE5: Account automatically suspended </br>
     * CODE6: Transfer limit reached – upgrade required </br>
     * CODE7: Transfer limit reached – upgrade required </br>
     * CODE8: Daily download limit reached </br>
     * CODE9: Daily link limit reached </br>
     * CODE10: Account permanently banned </br>
     * CODE11: Invalid link </br>
     * CODE12: Invalid link format </br>
     * CODE13: Daily download limit reached for hoster </br>
     * CODE14: Daily link limit reached for hoster </br>
     * CODE15: Weekly download limit reached for hoster </br>
     * CODE16: Weekly link limit reached for hoster </br>
     * CODE17-21/23/26-27/29: Link not supported or temporarily unavailable </br>
     * CODE22/24-25/28: File download link removed or incorrect </br>
     * CODE30: Host not supported </br>
     * CODE31: Unknown error </br>
     * CODE32: File removed by owner </br>
     * CODE33: File ID does not exist </br>
     * CODE34: Invalid PIN </br>
     * CODE35: VIP account required
     */
    private Map<String, Object> handleAPIErrors(final Map<String, Object> entries, final Account account, final DownloadLink link) throws Exception {
        if (!Boolean.FALSE.equals(entries.get("success"))) {
            return entries;
        }
        final String message = (String) entries.get("message");
        final Object errorCodeO = entries.get("code");
        final String errorStr = errorCodeO != null ? errorCodeO.toString() : "";
        if ("CODE1".equals(errorStr) || "CODE2".equals(errorStr) || "CODE3".equals(errorStr) || "CODE34".equals(errorStr)) {
            /* Invalid or missing PIN -> permanent account error */
            throw new AccountInvalidException(message);
        } else if ("CODE5".equals(errorStr) || "CODE10".equals(errorStr)) {
            /* Account suspended or permanently banned -> permanent account error */
            throw new AccountInvalidException(message);
        } else if ("CODE35".equals(errorStr)) {
            /* VIP account required (free account) -> permanent account error */
            throw new AccountInvalidException(message);
        } else if ("CODE6".equals(errorStr) || "CODE7".equals(errorStr)) {
            /* Transfer limit exhausted -> temporary account error */
            throw new AccountUnavailableException(message, 5 * 60 * 1000L);
        } else if ("CODE8".equals(errorStr) || "CODE9".equals(errorStr)) {
            /* Daily download/link limit reached -> temporary account error */
            throw new AccountUnavailableException(message, 5 * 60 * 1000L);
        } else if ("CODE32".equals(errorStr) || "CODE33".equals(errorStr)) {
            /* File not found or removed by owner */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if ("CODE30".equals(errorStr)) {
            /* Host not supported -> put error for this specific hoster */
            mhm.putError(account, link, 5 * 60 * 1000L, message);
            /* Unreachable code */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            /* Default case */
            logger.info("Unhandled API error: " + errorStr + " | " + message);
            if (link == null) {
                throw new AccountUnavailableException(message, 5 * 60 * 1000L);
            } else {
                mhm.handleErrorGeneric(account, link, errorStr, 10);
                /* Unreachable code */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
    }

    @Override
    public Class<? extends FakirdebridNetConfig> getConfigInterface() {
        return FakirdebridNetConfig.class;
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/api/login.php";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("[a-f0-9]{20,}")) {
            /* Do not validate with fixed length as admin wants to change length in the future. */
            return true;
        } else {
            return false;
        }
    }
}