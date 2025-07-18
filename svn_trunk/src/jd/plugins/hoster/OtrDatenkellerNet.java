//    jDownloader - Downloadmanager
//    Copyright (C) 2011  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.RandomUserAgent;
import jd.nutils.JDHash;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51180 $", interfaceVersion = 3, names = {}, urls = {})
public class OtrDatenkellerNet extends PluginForHost {
    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "otr.datenkeller.net", "otr.datenkeller.at" });
        return ret;
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        /* Filename can be used as global unique identifier. */
        final String filenameFromURL = this.getFilenameFromURL(link);
        if (filenameFromURL != null) {
            return this.getHost() + "://" + filenameFromURL;
        } else {
            return super.getLinkID(link);
        }
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_OLD = Pattern.compile("/\\?(?:file|getFile)=(.+\\.otrkey)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_NEW = Pattern.compile("/queueForFile/(.+\\.otrkey)", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://" + buildHostsPatternPart(domains) + "(" + PATTERN_OLD.pattern() + "|" + PATTERN_NEW.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    public static AtomicReference<String> agent = new AtomicReference<String>(null);

    private String getUserAgent() {
        while (true) {
            String agent = OtrDatenkellerNet.agent.get();
            if (agent == null) {
                OtrDatenkellerNet.agent.compareAndSet(null, RandomUserAgent.generate());
            } else {
                return agent;
            }
        }
    }

    private final String            API_BASE_URL                 = "https://otr.datenkeller.net/api.php";
    private final String            APIVERSION                   = "1";
    private static final AtomicLong timestampLastDownloadStarted = new AtomicLong(0);
    /* Don't touch the following! */
    private static AtomicInteger    freeRunning                  = new AtomicInteger(0);
    private static AtomicInteger    accountRunning               = new AtomicInteger(0);

    public static interface OtrDatenKellerInterface extends PluginConfigInterface {
        public static final TRANSLATION TRANSLATION = new TRANSLATION();

        public static class TRANSLATION {
            public String getMaxWaitMinutesForTicket_label() {
                return "Max wait for ticket (minutes)";
            }
        }

        @AboutConfig
        @DefaultIntValue(600)
        int getMaxWaitMinutesForTicket();

        void setMaxWaitMinutesForTicket(int i);
    }

    public OtrDatenkellerNet(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + this.getHost() + "/payment");
    }

    private String getContentURL(final DownloadLink link) throws MalformedURLException {
        return "https://otr.datenkeller.net/queueForFile/" + this.getFilenameFromURL(link);
    }

    @Override
    public String getAGBLink() {
        return "https://" + this.getHost() + "/";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        checkLinks(new DownloadLink[] { link });
        if (!link.isAvailabilityStatusChecked()) {
            return AvailableStatus.UNCHECKED;
        } else if (!link.isAvailable()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            return AvailableStatus.TRUE;
        }
    }

    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        try {
            api_prepBrowser(br);
            br.setCookiesExclusive(true);
            final StringBuilder sb = new StringBuilder();
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    /* we test up to 100 links at once */
                    if (index == urls.length || links.size() > 100) {
                        break;
                    }
                    links.add(urls[index]);
                    index++;
                }
                sb.delete(0, sb.capacity());
                for (final DownloadLink dl : links) {
                    if (sb.length() > 0) {
                        sb.append("%2C");
                    }
                    sb.append(Encoding.urlEncode(getFilenameFromURL(dl)));
                }
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("api_version", APIVERSION);
                query.appendEncoded("action", "validate");
                query.appendEncoded("file", sb.toString());
                br.postPage(API_BASE_URL, query);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                /* If all checked files are offline, "file_ok" field does not exist. */
                final Map<String, Object> file_okMap = (Map<String, Object>) entries.get("file_ok");
                for (final DownloadLink link : links) {
                    final String filenameFromURL = getFilenameFromURL(link);
                    final Map<String, Object> fileinfomap = file_okMap != null ? (Map<String, Object>) file_okMap.get(filenameFromURL) : null;
                    if (fileinfomap == null) {
                        link.setAvailable(false);
                    } else {
                        link.setAvailable(true);
                        final Object filesizeO = fileinfomap.get("filesize");
                        if (filesizeO instanceof String) {
                            link.setVerifiedFileSize(Long.parseLong(filesizeO.toString()));
                        } else {
                            link.setVerifiedFileSize(((Number) filesizeO).longValue());
                        }
                    }
                    link.setFinalFileName(Encoding.htmlDecode(filenameFromURL));
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            return false;
        }
        return true;
    }

    public String getDllink(final Browser br) throws Exception, PluginException {
        String dllink = br.getRegex("a href=\"(https?://[^\"]+)\"[^>]*class=\"piwik_download download-button\"").getMatch(0);
        if (dllink != null) {
            return dllink;
        }
        final Regex allMatches = br.getRegex("onclick=\"startCount\\(\\d+ +, +\\d+, +\\'([^<>\"\\']+)\\', +\\'([^<>\"\\']+)\\', +\\'([^<>\"\\']+)\\'\\)");
        if (!allMatches.patternFind()) {
            return null;
        }
        String firstPart = allMatches.getMatch(1);
        String secondPart = allMatches.getMatch(0);
        String thirdPart = allMatches.getMatch(2);
        dllink = "http://" + firstPart + "/" + secondPart + "/" + thirdPart;
        return dllink;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        final int max = 5;
        final int running = freeRunning.get();
        final int ret = Math.min(running + 1, max);
        return ret;
    }

    private final String WEBAPI_BASE = "https://waitaws.lastverteiler.net/api.php";

    private void preDownloadWaitHandling(final DownloadLink link) throws PluginException {
        synchronized (timestampLastDownloadStarted) {
            final long waitMillisBetweenDownloads = 30 * 1000l;
            final long passedTimeMillisSinceLastDownload = Time.systemIndependentCurrentJVMTimeMillis() - timestampLastDownloadStarted.get();
            if (passedTimeMillisSinceLastDownload < waitMillisBetweenDownloads) {
                this.sleep(waitMillisBetweenDownloads - passedTimeMillisSinceLastDownload, link);
            }
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        /*
         * Use random User-Agent here because we use website for downloading and they determine the limits based on User-Agent + cookies(?)
         */
        br.getHeaders().put("User-Agent", getUserAgent());
        final String directurlproperty = "free_finallink";
        final String storedDirecturl = link.getStringProperty(directurlproperty);
        String dllink = null;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            final String contenturl = this.getContentURL(link);
            String queuePositionStr = null;
            // final String jscounturl = br.getRegex("(https?://[^\"]+/countMe\\.js\\?\\d+)").getMatch(0);
            // br2.getPage("http://staticaws.lastverteiler.net/otrfuncs/countMe.js");
            link.getLinkStatus().setStatusText("Waiting for ticket...");
            final int userDefinedMaxWaitMinutes = PluginJsonConfig.get(OtrDatenKellerInterface.class).getMaxWaitMinutesForTicket();
            int loops = 0;
            final long timeStart = Time.systemIndependentCurrentJVMTimeMillis();
            String queueID = null;
            String queueStatusString = "Unknown";
            final int refreshSeconds = 20;
            ticketLoop: while (!this.isAbort()) {
                if (queueID == null) {
                    br.getPage(contenturl);
                    dllink = getDllink(br);
                    if (dllink != null) {
                        logger.info("Found directurl: " + dllink);
                        break ticketLoop;
                    }
                    queueID = br.getRegex("queueManager\\.init\\(\\s*'([a-f0-9]+)'").getMatch(0);
                    if (queueID != null) {
                        /* This allows us to use their WebAPI to queck the queue status */
                        logger.info("Found queueID: " + queueID);
                    }
                    // TODO: Check/fix queue position regex
                    queuePositionStr = br.getRegex("Deine Position in der Warteschlange\\s*:\\s*</td><td>~(\\d+)</td>").getMatch(0);
                }
                if (queueID != null) {
                    final Browser brc = br.cloneBrowser();
                    brc.getPage("/queue/api/status/" + queueID);
                    final Map<String, Object> queueStatus = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                    queueStatusString = queueStatus.get("status").toString();
                    if (queueStatusString.equalsIgnoreCase("completed")) {
                        dllink = queueStatus.get("download_link").toString();
                        logger.info("Found queue completed: " + dllink);
                        break ticketLoop;
                    }
                }
                if (Time.systemIndependentCurrentJVMTimeMillis() - timeStart > userDefinedMaxWaitMinutes * 60 * 1000) {
                    logger.info("Stopping because: Did not get a ticket within user defined time");
                    break ticketLoop;
                }
                logger.info("Didn't get a ticket on try " + loops + ". Retrying...Position: " + queuePositionStr);
                sleep(refreshSeconds * 1000l, link);
                final String queuePositionHumanReadable;
                if (queuePositionStr != null) {
                    queuePositionHumanReadable = queuePositionStr;
                } else {
                    queuePositionHumanReadable = "N/A";
                }
                link.getLinkStatus().setStatusText("Warten auf Ticket | Position in der Warteschlange: " + queuePositionHumanReadable);
            }
            if (dllink == null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Didn't get a ticket", 30 * 60 * 1000l);
            }
            link.setProperty(directurlproperty, dllink);
        }
        this.preDownloadWaitHandling(link);
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            checkErrorsAfterDownloadAttempt();
        } catch (final InterruptedException ie) {
            throw ie;
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directurlproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired?", e);
            } else {
                throw e;
            }
        }
        synchronized (timestampLastDownloadStarted) {
            timestampLastDownloadStarted.set(Time.systemIndependentCurrentJVMTimeMillis());
        }
        controlSlot(link, null, +1);
        try {
            dl.startDownload();
        } finally {
            controlSlot(link, null, -1);
        }
    }

    private void checkErrorsAfterDownloadAttempt() throws IOException, PluginException {
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (br.containsHTML(">\\s*Maximale Anzahl an Verbindungen verbraucht")) {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Too many connections", 1 * 60 * 1000l);
            } else if (br.containsHTML("wurde wegen Missbrauch geblockt")) {
                throw new AccountInvalidException("Account wurde wegen Missbrauch geblockt");
            } else if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error");
            }
        }
    }

    private Map<String, Object> login(Account account, boolean force) throws Exception {
        br.setCookiesExclusive(true);
        api_prepBrowser(br);
        final boolean useAPIV2 = false;
        String apikey = getAPIKEY(account);
        if (!force && apikey != null) {
            /* Re-use existing apikey */
            return null;
        }
        Map<String, Object> userinfo = null;
        if (useAPIV2 && DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            // 2023-06-23: Unfinished code. The other API is nicer as it returns json and this one returns line based results.
            final Calendar cal = Calendar.getInstance();
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("jdlAPI", "");
            query.appendEncoded("account", account.getUser());
            query.appendEncoded("password", account.getPass());
            final String str = cal.get(Calendar.DAY_OF_MONTH) + account.getUser() + account.getPass();
            System.out.println("String to hash: " + str);
            query.appendEncoded("auth", JDHash.getMD5(str));
            br.getPage("https://otr.datenkeller.net/?" + query.toString());
            apikey = "TODO";
        } else {
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("api_version", APIVERSION);
            query.appendEncoded("action", "login");
            query.appendEncoded("username", Encoding.urlEncode(account.getUser()));
            query.appendEncoded("password", Encoding.urlEncode(account.getPass()));
            br.postPage(API_BASE_URL, query);
            userinfo = this.checkErrorsAPI(br, null, account);
            apikey = userinfo.get("apikey").toString();
        }
        if (StringUtils.isEmpty(apikey)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        this.setApikey(account, apikey);
        return userinfo;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> userinfo = login(account, true);
        final Object expiresO = userinfo.get("expires");
        if (expiresO == null) {
            /* No expire date given -> Assume that account is expired. */
            ai.setExpired(true);
            return ai;
        }
        if (expiresO instanceof Number) {
            ai.setValidUntil(((Number) expiresO).longValue() * 1000);
        } else {
            /* Field can be given as String. */
            ai.setValidUntil(Long.parseLong(expiresO.toString()) * 1000);
        }
        account.setType(AccountType.PREMIUM);
        account.setConcurrentUsePossible(true);
        ai.setUnlimitedTraffic();
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        login(account, false);
        final Browser brc = br.cloneBrowser();
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("api_version", APIVERSION);
        query.appendEncoded("action", "getpremlink");
        query.appendEncoded("username", account.getUser());
        query.appendEncoded("password", account.getPass());
        query.appendEncoded("apikey", getAPIKEY(account));
        query.appendEncoded("filename", getFilenameFromURL(link));
        brc.postPage(API_BASE_URL, query);
        final Map<String, Object> resp = this.checkErrorsAPI(brc, link, account);
        final String dllink = (String) resp.get("dllink");
        if (StringUtils.isEmpty(dllink)) {
            /* This should never happen. */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to find final downloadurl");
        }
        this.preDownloadWaitHandling(link);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, Encoding.htmlDecode(dllink), this.isResumeable(link, account), this.getMaxChunks(link, account));
        checkErrorsAfterDownloadAttempt();
        synchronized (timestampLastDownloadStarted) {
            timestampLastDownloadStarted.set(Time.systemIndependentCurrentJVMTimeMillis());
        }
        controlSlot(link, account, +1);
        try {
            dl.startDownload();
        } finally {
            controlSlot(link, account, -1);
        }
    }

    private void controlSlot(final DownloadLink link, final Account account, final int num) {
        if (account == null) {
            synchronized (freeRunning) {
                final int before = freeRunning.get();
                final int after = before + num;
                freeRunning.set(after);
                logger.info("freeRunning(" + link.getName() + ")|max:" + getMaxSimultanFreeDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
            }
        } else {
            synchronized (accountRunning) {
                final int before = accountRunning.get();
                final int after = before + num;
                accountRunning.set(after);
                logger.info("accountRunning(" + link.getName() + ")|max:" + getMaxSimultanPremiumDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
            }
        }
    }

    private Map<String, Object> checkErrorsAPI(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException jse) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid API response");
        }
        final String status = (String) entries.get("status");
        final String message = (String) entries.get("message");
        if (!"fail".equals(status)) {
            /* No error */
            return entries;
        }
        if (message.equalsIgnoreCase("failed to login due to wrong credentials, missing or wrong apikey")) {
            throw new AccountInvalidException(message);
        } else if (message.equalsIgnoreCase("failed to login due to wrong credentials or expired account")) {
            throw new AccountInvalidException(message);
        } else if (message.equalsIgnoreCase("wrong filename supplied")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            /* Unknown error */
            if (link == null) {
                throw new AccountUnavailableException(message, 5 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, message);
            }
        }
    }

    private void api_prepBrowser(final Browser br) {
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setCustomCharset("utf-8");
    }

    private void api_Free_prepBrowser(final Browser br) {
        br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        br.getHeaders().put("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
        br.getHeaders().put("Accept-Charset", null);
    }

    final String getFilenameFromURL(final DownloadLink link) {
        final String url = link.getPluginPatternMatcher();
        String filename = new Regex(url, PATTERN_NEW).getMatch(0);
        if (filename != null) {
            return filename;
        }
        filename = new Regex(url, PATTERN_OLD).getMatch(0);
        return filename;
    }

    private void setApikey(final Account account, final String apikey) {
        account.setProperty("apikey_" + account.getUser(), apikey);
    }

    private String getAPIKEY(final Account account) {
        String apikey = account.getStringProperty("apikey_" + account.getUser());
        if (apikey != null) {
            apikey = apikey.replace("\\", "");
        }
        return apikey;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        /*
         * Admin told us limit = 12 but when we checked it, only 7 with a large wait time in between were possible - anyways, works best
         * with only one
         */
        /* 2023-06-20: Changed limit from 1 to 5. */
        final int max = 5;
        final int running = accountRunning.get();
        final int ret = Math.min(running + 1, max);
        return ret;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final jd.plugins.Account acc) {
        return false;
    }
}