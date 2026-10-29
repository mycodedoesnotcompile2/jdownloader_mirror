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
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 53802 $", interfaceVersion = 3, names = {}, urls = {})
public class MochaMy extends PluginForHost {
    public MochaMy(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        /* Each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain */
        ret.add(new String[] { "mocha.my" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    /**
     * These are internal URLs which are created by the crawler plugin. The URLs users see in their browser (/share/...) are handled by the
     * crawler.
     */
    public static final String PATTERN_FILE = "/dl/([A-Za-z0-9_-]+)(?:/([a-f0-9-]{36}))?";

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_FILE);
        }
        return ret.toArray(new String[0]);
    }

    public static final String                                TURNSTILE_SITEKEY = "0x4AAAAAADvCUptk-JVN3aZp";
    /* Download tickets by share token. Access is synchronized on this object. */
    private static AtomicReference<Map<String, String>>       TICKET_CACHE      = new AtomicReference<Map<String, String>>(new HashMap<String, String>());

    /* Number of downloads which have passed the start phase and are currently downloading. */
    private static AtomicInteger                              RUNNING_DOWNLOADS = new AtomicInteger(0);
    private static final int                                  MAX_SIMULTAN_DOWNLOADS = 20;

    public static String getAPIBase(final String host) {
        return "https://api." + host + "/api";
    }

    public static Browser prepBrowserAPI(final Browser br) {
        br.getHeaders().put("Accept", "application/json");
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(400, 401, 403, 404, 410, 429);
        return br;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String shareToken = getShareToken(link);
        if (shareToken == null) {
            return super.getLinkID(link);
        }
        final String fileID = getFileID(link);
        if (fileID != null) {
            return "mochamy://" + shareToken + "/" + fileID;
        } else {
            return "mochamy://" + shareToken;
        }
    }

    private String getShareToken(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), PATTERN_FILE).getMatch(0);
    }

    /** Returns the file ID if this file is part of a shared folder. Returns null for single file shares. */
    private String getFileID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), PATTERN_FILE).getMatch(1);
    }

    /**
     * Stores file information found by the crawler so that the hosterplugin does not need to do any requests when only checking links.
     */
    public static void setFileInfo(final DownloadLink link, final String filename, final long filesize) {
        link.setFinalFileName(filename);
        link.setVerifiedFileSize(filesize);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final String shareToken = getShareToken(link);
        final String fileID = getFileID(link);
        if (!link.isNameSet()) {
            link.setName(fileID != null ? fileID : shareToken);
        }
        final Browser br = prepBrowserAPI(createNewBrowserInstance());
        final Map<String, Object> share = loadShare(br, shareToken);
        if (fileID == null) {
            /* Single file share */
            setFileInfo(link, share.get("originalName").toString(), ((Number) share.get("fileSize")).longValue());
        } else if (link.getFinalFileName() != null) {
            /* Information has already been set by crawler. */
        } else {
            final String accessToken = getAccessToken(br, link, share, shareToken);
            final Map<String, Object> entry = findFolderEntry(br, shareToken, fileID, accessToken);
            setFileInfo(link, entry.get("fileName").toString(), ((Number) entry.get("fileSize")).longValue());
        }
        link.setPasswordProtected(Boolean.TRUE.equals(share.get("passwordProtected")));
        return AvailableStatus.TRUE;
    }

    /** Loads share information. Throws exception if share is offline. */
    private Map<String, Object> loadShare(final Browser br, final String shareToken) throws IOException, PluginException {
        br.getPage(getAPIBase(getHost()) + "/shares/" + shareToken);
        if (br.getHttpConnection().getResponseCode() == 404 || br.getHttpConnection().getResponseCode() == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 429) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Rate limit reached", 60 * 1000l);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> share = (Map<String, Object>) entries.get("share");
        if (!Boolean.TRUE.equals(share.get("isActive"))) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return share;
    }

    private Map<String, Object> findFolderEntry(final Browser br, final String shareToken, final String fileID, final String accessToken) throws IOException, PluginException {
        String url = getAPIBase(getHost()) + "/shares/" + shareToken + "/folder";
        if (accessToken != null) {
            url += "?accessToken=" + Encoding.urlEncode(accessToken);
        }
        br.getPage(url);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> items = (List<Map<String, Object>>) entries.get("entries");
        for (final Map<String, Object> item : items) {
            if (fileID.equals(item.get("id"))) {
                return item;
            }
        }
        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
    }

    /** Returns access token for password protected shares or null if share is not password protected. */
    private String getAccessToken(final Browser br, final DownloadLink link, final Map<String, Object> share, final String shareToken) throws Exception {
        if (!Boolean.TRUE.equals(share.get("passwordProtected"))) {
            return null;
        }
        int attempts = 0;
        do {
            String passCode = link.getDownloadPassword();
            if (passCode == null) {
                passCode = getUserInput("Password?", link);
            }
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("password", passCode);
            br.postPageRaw(getAPIBase(getHost()) + "/shares/" + shareToken + "/verify-password", JSonStorage.serializeToJson(postdata));
            if (br.getHttpConnection().getResponseCode() == 429) {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Rate limit reached", 60 * 1000l);
            } else if (br.getHttpConnection().getResponseCode() == 200) {
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                link.setDownloadPassword(passCode);
                return entries.get("accessToken").toString();
            }
            /* Wrong password -> Ask user again */
            link.setDownloadPassword(null);
            attempts++;
        } while (attempts < 3);
        throw new PluginException(LinkStatus.ERROR_FATAL, "Wrong password");
    }

    /**
     * Returns a download ticket for the given share. Tickets are cached per share because the website also uses one ticket for multiple
     * files. Only one captcha is solved at a time: Parallel downloads wait and then use the ticket of the first one.
     *
     * @param rejectedTicket
     *            Ticket the server has just rejected. It will not be returned again.
     */
    private String getTicket(final Browser br, final DownloadLink link, final String shareToken, final String rejectedTicket) throws Exception {
        synchronized (TICKET_CACHE) {
            final Map<String, String> cache = TICKET_CACHE.get();
            final String cachedTicket = cache.get(shareToken);
            if (cachedTicket != null && !cachedTicket.equals(rejectedTicket)) {
                logger.info("Using cached ticket");
                return cachedTicket;
            }
            /* Cloudflare Turnstile is required to get a download ticket. */
            final Browser brCaptcha = br.cloneBrowser();
            brCaptcha.getPage("https://" + getHost() + "/share/" + shareToken);
            final String turnstileResponse = new CaptchaHelperHostPluginCloudflareTurnstile(this, brCaptcha, TURNSTILE_SITEKEY).getToken();
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("token", turnstileResponse);
            postdata.put("shareToken", shareToken);
            br.postPageRaw(getAPIBase(getHost()) + "/turnstile/verify", JSonStorage.serializeToJson(postdata));
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String ticket = (String) entries.get("ticket");
            if (!Boolean.TRUE.equals(entries.get("success")) || StringUtils.isEmpty(ticket)) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
            cache.put(shareToken, ticket);
            return ticket;
        }
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return true;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        final String shareToken = getShareToken(link);
        final String fileID = getFileID(link);
        final Browser br = prepBrowserAPI(createNewBrowserInstance());
        final Map<String, Object> share = loadShare(br, shareToken);
        final String accessToken = getAccessToken(br, link, share, shareToken);
        String rejectedTicket = null;
        int attempt = 0;
        while (true) {
            final boolean lastAttempt = attempt >= 1;
            attempt++;
            /* A ticket is valid for multiple files of the same share -> Reuse cached ticket if possible. */
            final String ticket = getTicket(br, link, shareToken, rejectedTicket);
            final StringBuilder query = new StringBuilder();
            if (fileID != null) {
                query.append("/folder/download?");
                query.append("fileId=").append(fileID).append("&");
            } else {
                query.append("/download?");
            }
            final String ticketParams = "ticket=" + Encoding.urlEncode(ticket) + (accessToken != null ? "&accessToken=" + Encoding.urlEncode(accessToken) : "");
            final String urlBase = getAPIBase(getHost()) + "/shares/" + shareToken + query.toString();
            /* Same as the website: Check ticket via probe request without downloading anything. */
            final Browser brProbe = br.cloneBrowser();
            brProbe.setFollowRedirects(false);
            brProbe.getPage(urlBase + "probe=1&" + ticketParams);
            if (brProbe.getHttpConnection().getResponseCode() == 403 && !lastAttempt) {
                logger.info("Ticket was rejected by probe request -> Getting a new one");
                rejectedTicket = ticket;
                continue;
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, urlBase + ticketParams, isResumeable(link, null), getMaxChunks(null));
            if (!looksLikeDownloadableContent(dl.getConnection()) && dl.getConnection().getResponseCode() == 403 && !lastAttempt) {
                br.followConnection(true);
                logger.info("Ticket was rejected by download request -> Getting a new one");
                rejectedTicket = ticket;
                continue;
            }
            break;
        }
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (dl.getConnection().getResponseCode() == 429) {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Rate limit reached", 60 * 1000l);
            }
            String message = "Server did not return file content";
            try {
                final Map<String, Object> errorMap = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                if (errorMap.get("error") != null) {
                    message = errorMap.get("error").toString();
                }
            } catch (final Exception ignore) {
            }
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, message, 5 * 60 * 1000l);
        }
        /* Add a download slot: From now on, the next download is allowed to start. */
        controlRunningDownloads(link, +1);
        try {
            dl.startDownload();
        } finally {
            /* Remove download slot */
            controlRunningDownloads(link, -1);
        }
    }

    private void controlRunningDownloads(final DownloadLink link, final int num) {
        synchronized (RUNNING_DOWNLOADS) {
            final int before = RUNNING_DOWNLOADS.get();
            final int after = before + num;
            RUNNING_DOWNLOADS.set(after);
            logger.info("runningDownloads(" + link.getName() + ")|max:" + getMaxSimultanFreeDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
        }
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    /* No @Override */
    public int getMaxChunks(final Account account) {
        /* A new ticket is needed for each download request -> Use one connection only. */
        return 1;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        /*
         * Allow up to 20 simultaneous downloads but let them start one after another: A new download may only start when all other
         * downloads have already reached the download phase. This way only one download at a time solves the captcha and all others
         * re-use its ticket.
         */
        return Math.min(RUNNING_DOWNLOADS.get() + 1, MAX_SIMULTAN_DOWNLOADS);
    }
}
