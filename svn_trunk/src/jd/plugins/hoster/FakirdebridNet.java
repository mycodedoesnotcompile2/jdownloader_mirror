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
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
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
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 52802 $", interfaceVersion = 3, names = { "fakirdebrid.net" }, urls = { "" })
public class FakirdebridNet extends PluginForHost {
    // private static final String WEBSITE_BASE = "https://fakirdebrid.net";
    private static final String          API_BASE           = "https://api.fakirdebrid.net";
    private static MultiHosterManagement mhm                = new MultiHosterManagement("fakirdebrid.net");
    private static final boolean         defaultResume      = true;
    private static final int             defaultMaxchunks   = -10;
    private final String                 PROPERTY_RESUME    = "fakirdebrid_resume";
    private final String                 PROPERTY_MAXCHUNKS = "fakirdebrid_maxchunks";

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
            int counter = 0;
            String passCode = link.getDownloadPassword();
            do {
                String postData = "url=" + Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this));
                if (counter > 0) {
                    /* 2nd try: First provided password was invalid or no password has been tried on first attempt. */
                    passCode = getUserInput("Password?", link);
                }
                if (passCode != null) {
                    /* Append password like: "|<password>" */
                    postData += Encoding.urlEncode("|" + passCode);
                }
                br.postPage(API_BASE + "/generate/" + Encoding.urlEncode(account.getPass()), postData);
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
            final boolean resumable = ((Boolean) entries.get("resumable")).booleanValue();
            int maxChunks = ((Number) entries.get("maxchunks")).intValue();
            if (maxChunks > 1) {
                maxChunks = -maxChunks;
            }
            final String apilink = (String) entries.get("link");
            br.getPage(apilink);
            entries = handleAPIErrors(account, link);
            entries = (Map<String, Object>) entries.get("data");
            final String state = (String) entries.get("state");
            if ("processing".equalsIgnoreCase(state)) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "File is being transferred to fakirdebrid servers: " + entries.get("completed") + "%", 10 * 1000l);
            } else if (!"completed".equals(state)) {
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
        switch (errorStr) {
        case "CODE1":
        case "CODE2":
        case "CODE3":
        case "CODE34":
            /* Invalid or missing PIN -> permanent account error */
            throw new AccountInvalidException(message);
        case "CODE5":
        case "CODE10":
            /* Account suspended or permanently banned -> permanent account error */
            throw new AccountInvalidException(message);
        case "CODE35":
            /* VIP account required (free account) -> permanent account error */
            throw new AccountInvalidException(message);
        case "CODE6":
        case "CODE7":
            /* Transfer limit exhausted -> temporary account error */
            throw new AccountUnavailableException(message, 5 * 60 * 1000l);
        case "CODE8":
        case "CODE9":
            /* Daily download/link limit reached -> temporary account error */
            throw new AccountUnavailableException(message, 5 * 60 * 1000l);
        case "CODE32":
        case "CODE33":
            /* File not found or removed by owner */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        case "CODE30":
            /* Host not supported -> put error for this specific hoster */
            mhm.putError(account, link, 5 * 60 * 1000l, message);
            /* Unreachable code */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        default:
            logger.info("Unhandled API error: " + errorStr + " | " + message);
            if (link == null) {
                throw new AccountUnavailableException(message, 5 * 60 * 1000l);
            } else {
                mhm.handleErrorGeneric(account, link, errorStr, 10);
            }
        }
        return entries;
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "Real-Debrid/api/login.php";
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