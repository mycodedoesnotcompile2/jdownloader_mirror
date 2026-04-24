//    jDownloader - Downloadmanager
//    Copyright (C) 2013  JD-Team support@jdownloader.org
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
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.net.websocket.WebSocketFrame;
import org.appwork.utils.net.websocket.WriteWebSocketFrame;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
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
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.websocket.WebSocketClient;

@HostPlugin(revision = "$Revision: 52710 $", interfaceVersion = 2, names = {}, urls = {})
public class FileFactory extends PluginForHost {
    public FileFactory(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + this.getHost() + "/pricing");
    }

    @Deprecated
    public static final String   PROPERTY_CLASSIC                      = "classic_website";
    @Deprecated
    private static final boolean ALLOW_EXPECT_SPECIAL_CLASSIC_LINKS    = false;
    @Deprecated
    private static final boolean ALLOW_EXPECT_SPECIAL_CLASSIC_ACCOUNTS = false;
    /* Account related patterns */
    private static final String  PROPERTY_ACCOUNT_LOGIN_TOKEN          = "login_token";
    private static final Pattern PATTERN_FILE                          = Pattern.compile("/(?:file|image|preview|stream)/([a-z0-9]+)(/([^/]+))?", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_TRAFFIC_SHARE                 = Pattern.compile("/trafficshare/[a-f0-9]{32}/([a-z0-9]+)/?", Pattern.CASE_INSENSITIVE);
    /* 2026-04-22: Not possible anymore */
    private static final boolean ALLOW_MASS_LINKCHECK_WITHOUT_ACCOUNT  = false;
    /* 2026-04-22: Broken -> Displays all links as offline */
    private static final boolean ALLOW_MASS_LINKCHECK_WITH_ACCOUNT     = false;

    @Override
    public FEATURE[] getFeatures() {
        return new FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL /* , untested: LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL */ };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        // blocking default UA
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, Request.getSuggestedUserAgent("142.0"));
        br.setCookie(getHost(), "filefactory_relaunch", "seen"); // old
        br.setCookie(getHost(), "ff_relaunch_message_seen", "true"); // 2026-04-22
        br.setCookie(getHost(), "filefactory_stream", "seen"); // 2026-04-22
        br.setCookie(getHost(), "cookieConsent", "accepted");
        br.setCookie(getHost(), "locale", "en_US.utf8");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return getWebsiteBase() + "/legal/terms";
    }

    /** Returns true if this link shall be used with the older filefactory.com website accessible via classic.filefactory.com. */
    @Deprecated
    private boolean isClassicFile(final DownloadLink link) throws PluginException {
        if (!ALLOW_EXPECT_SPECIAL_CLASSIC_LINKS) {
            return false;
        }
        return link.hasProperty(PROPERTY_CLASSIC) || link.getPluginPatternMatcher().contains("classic.filefactory.com");
    }

    @Deprecated
    private boolean isAccountUsingClassicWebsite(final Account account) {
        if (!ALLOW_EXPECT_SPECIAL_CLASSIC_ACCOUNTS) {
            return false;
        }
        return account.hasProperty(PROPERTY_CLASSIC);
    }

    private String getContentURL(final DownloadLink link) throws PluginException {
        return getContentURL(link, false);
    }

    private String getContentURL(final DownloadLink link, boolean disableClassic) throws PluginException {
        if (isClassicFile(link) && !disableClassic) {
            return "https://classic." + getHost() + "/file/" + this.getFUID(link);
        } else {
            return getWebsiteBase() + "/file/" + this.getFUID(link);
        }
    }

    private String getWebapiBase() {
        return "https://www." + getHost() + "/api";
    }

    private String getWebsiteBase() {
        return "https://www." + getHost();
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (this.isPremiumAccount(account)) {
            return true;
        } else {
            /* Free(anonymous) or unknown account type */
            return false;
        }
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if (this.isPremiumAccount(account)) {
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFUID(link);
        if (fid != null) {
            return getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "filefactory.com" });
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
            ret.add("https?://(?:www\\.|classic\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_FILE.pattern() + "|" + PATTERN_TRAFFIC_SHARE.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    /** Returns decoded filename from url if url contains a filename, returns null otherwise. */
    private String getFilenameFromURL(final DownloadLink link) {
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), PATTERN_FILE);
        final String filenameFromURL = urlinfo.getMatch(3);
        if (filenameFromURL != null) {
            /* Return url from filename -> Not all URLs contain a filename */
            return URLEncode.decodeURIComponent(filenameFromURL);
        }
        return null;
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        final String filenameFromURL = getFilenameFromURL(link);
        if (filenameFromURL != null) {
            /* Return url from filename -> Not all URLs contain a filename */
            return filenameFromURL;
        }
        return this.getFUID(link);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        try {
            return requestFileInformationWebsite(null, link);
        } catch (final PluginException e) {
            switch (e.getLinkStatus()) {
            case LinkStatus.ERROR_FILE_NOT_FOUND:
                throw e;
            case LinkStatus.ERROR_PLUGIN_DEFECT:
                throw e;
            default:
                /**
                 * Ignore exception since we know that the file is online. <br>
                 * This is especially for file items with errors 257 and 258 so that they get correctly displayed as online during available
                 * check.
                 */
                logger.log(e);
                return AvailableStatus.TRUE;
            }
        }
    }

    private AvailableStatus requestFileInformationWebsite(final Account account, final DownloadLink link) throws Exception {
        setBrowserExclusive();
        if (account != null) {
            this.loginWebsite(br, account, false);
        }
        final String contenturl = this.getContentURL(link);
        final boolean isDownload = PluginEnvironment.DOWNLOAD.equals(this.getPluginEnvironment());
        boolean success = false;
        URLConnectionAdapter con = null;
        try {
            if (isDownload) {
                dl = new jd.plugins.BrowserAdapter().openDownload(br, link, contenturl, this.isResumeable(link, account), this.getMaxChunks(link, account));
                con = dl.getConnection();
            } else {
                con = br.openGetConnection(contenturl);
            }
            if (this.looksLikeDownloadableContent(con)) {
                success = true;
                link.setFinalFileName(Plugin.getFileNameFromConnection(con));
                if (con.getCompleteContentLength() > 0) {
                    if (con.isContentDecoded()) {
                        link.setDownloadSize(con.getCompleteContentLength());
                    } else {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                }
                link.setAvailable(true);
                return AvailableStatus.TRUE;
            }
            br.followConnection();
            final String NEXT_REDIRECT = br.getRegex("NEXT_REDIRECT;replace;(https://classic\\.filefactory\\.com/file/" + Pattern.quote(getFUID(link)) + ")").getMatch(0);
            if (NEXT_REDIRECT != null) {
                br.getPage(NEXT_REDIRECT);
            }
            if (ALLOW_EXPECT_SPECIAL_CLASSIC_LINKS) {
                if ("classic.filefactory.com".equals(br.getHost(true))) {
                    link.setProperty(PROPERTY_CLASSIC, Boolean.TRUE);
                } else {
                    link.removeProperty(PROPERTY_CLASSIC);
                }
            }
            if (isPasswordProtectedFile(br)) {
                link.setPasswordProtected(true);
            } else {
                link.setPasswordProtected(false);
            }
            old_website: {
                /* classic website */
                String htmlFileName = br.getRegex("class\\s*=\\s*\"file-name\"[^>]*>\\s*(.+?)\\s*</").getMatch(0);
                if (htmlFileName == null) {
                    /* For image links via "/preview/..." */
                    htmlFileName = br.getRegex("id=\"file_name\"[^>]*>\\s*<h2>([^<]+)</h2>").getMatch(0);
                }
                if (htmlFileName != null) {
                    // htmlDecode only on html input
                    htmlFileName = Encoding.htmlDecode(htmlFileName).trim();
                    link.setName(htmlFileName);
                }
                /* classic website */
                String htmlFilesizeString = br.getRegex("class\\s*=\\s*\"file-meta\"[^>]*id\\s*=\\s*\"file_info\"[^>]*>\\s*([0-9\\. GMKB]+)").getMatch(0);
                if (htmlFilesizeString == null) {
                    /* For image links via "/preview/..." */
                    htmlFilesizeString = br.getRegex("id=\"file_info\"[^>]*>(\\d+[^<]+) uploaded[^<]*").getMatch(0);
                }
                if (htmlFilesizeString != null) {
                    link.setDownloadSize(SizeFormatter.getSize(htmlFilesizeString));
                }
            }
            String jsonFileName = getString(br, "disp_filename");
            if (jsonFileName != null) {
                link.setFinalFileName(jsonFileName);
            }
            final Number jsonFilesizeBytes = getNumber(br, "size");
            if (jsonFilesizeBytes != null) {
                link.setVerifiedFileSize(jsonFilesizeBytes.longValue());
            }
            checkErrorsWebsite(link, account, br);
        } finally {
            if (isDownload && !success) {
                if (con != null) {
                    con.disconnect();
                }
                this.dl = null;
            }
        }
        return AvailableStatus.TRUE;
    }

    private Boolean getBoolean(Browser br, final String key) {
        final String value = br.getRegex("\"" + Pattern.quote(key) + "(?:\\\\)?\"\\s*:\\s*(true|false)").getMatch(0);
        if (value == null) {
            return null;
        }
        return "true".equals(value);
    }

    /** Returns number from any place in json in browser instance., */
    private Number getNumber(final Browser br, final String key) {
        final String value = br.getRegex("\"" + Pattern.quote(key) + "(?:\\\\)?\"\\s*:\\s*(\\d+)").getMatch(0);
        if (value == null) {
            return null;
        }
        return Long.parseLong(value);
    }

    private String getString(Browser br, final String key) {
        final String value = br.getRegex("\"" + Pattern.quote(key) + "(?:\\\\)?\"\\s*:\\s*(?:\\\\)?\"(.*?)(?:\\\\)?\"").getMatch(0);
        if (value == null) {
            return null;
        }
        return restoreFromString("\"" + value + "\"", TypeRef.STRING);
    }

    /** Checks premium status based on html code. */
    private Boolean isPremium(final Browser br) {
        final Boolean userIsPremium = getBoolean(br, "userIsPremium");
        if (userIsPremium != null) {
            return userIsPremium;
        }
        final Boolean userIsFree = getBoolean(br, "isFree");
        if (userIsFree != null) {
            /* No free == premium */
            return !userIsFree;
        }
        return null;
    }

    /** Checks loggedin status based on html code. */
    private Boolean isLoggedIn(final Browser br) {
        return getBoolean(br, "isLoggedIn");
    }

    private boolean isPasswordProtectedFile(final Browser br) {
        if (Boolean.TRUE.equals(getBoolean(br, "requiresPassword"))) {
            return true;
        } else {
            return false;
        }
    }

    public void checkErrorsWebsite(final DownloadLink link, final Account account, final Browser br) throws PluginException, MalformedURLException {
        // final Boolean isFree = getBoolean(br, "isFree");
        final UrlQuery query = UrlQuery.parse(br.getURL());
        String error_code = null;
        String errorData = br.getRegex("\"errorData\\\\\"\\s*:\\s*(\\{.*?\\})\\s*,").getMatch(0);
        Map<String, Object> errorDataMap = null;
        if (errorData != null) {
            try {
                errorData = "\"" + errorData + "\"";
                errorData = restoreFromString(errorData, TypeRef.STRING);
                errorDataMap = restoreFromString(errorData, TypeRef.MAP);
                error_code = errorDataMap.get("errorCode").toString();
            } catch (Exception e) {
                logger.log(e);
            }
        }
        if (error_code == null) {
            error_code = query == null ? null : query.get("code");
            if (error_code == null) {
                error_code = br.getRegex("\"errorCode(?:\\\\)?\":(?:\\\\)?\"(.*?)(?:\\\\)?\"").getMatch(0);
            }
        }
        if (account != null && Boolean.FALSE.equals(this.isLoggedIn(br))) {
            throw new AccountUnavailableException("Session expired?", 1 * 60 * 1000l);
        }
        if (Boolean.TRUE.equals(getBoolean(br, "requiresPremium")) && !Boolean.TRUE.equals(isPremium(br))) {
            throw new AccountRequiredException();
        }
        if (error_code != null) {
            String msg = br.getRegex("id=\"wp-body-box-message\"[^>]*>\\s*<p>([^<]+)</p>").getMatch(0);
            if (msg != null) {
                msg = Encoding.htmlDecode(msg);
                /* Add error code to error message. */
                msg = "Error " + error_code + ": " + msg;
            } else {
                msg = "Error " + error_code;
            }
            if ("251".equals(error_code)) {
                /* "Invalid Download Link" */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, msg);
            } else if ("252".equals(error_code)) {
                /* "File Unavailable - This file is no longer available due to an unexpected error." */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, msg);
            } else if ("258".equals(error_code)) {
                /* https://www.filefactory.com/error.php?code=258 */
                throw new AccountRequiredException("The owner of this file has restricted it to members with a Premium Account. Please purchase a Premium account in order to download this file.");
            } else if ("257".equals(error_code)) {
                /* https://www.filefactory.com/error.php?code=257 */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server Load Too High");
            } else if ("266".equals(error_code)) {
                // Please wait a moment before downloading. Free users must wait between downloads. This typically takes 5 minutes after
                // your
                // last download completed
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, errorDataMap != null ? StringUtils.valueOfOrNull(errorDataMap.get("message")) : null);
            } else if ("274".equals(error_code)) {
                // File unavailable
                /**
                 * https://www.filefactory.com/error.php?code=274 <br>
                 * This file cannot be downloaded at this time. Please let us know about this issue by using the contact link below.
                 */
                throw new PluginException(LinkStatus.ERROR_FATAL, msg);
            } else if ("300".equals(error_code)) {
                /* Invalid folder link */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, msg);
            } else if ("325".equals(error_code)) {
                /* Invalid Share Link e.g. https://www.filefactory.com/share/fi:xxxyyy */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, msg);
            } else if ("FILE_NOT_FOUND".equals(error_code)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, msg);
            } else {
                logger.info("Unknown error happened: " + error_code);
                throw new PluginException(LinkStatus.ERROR_FATAL, msg);
            }
        }
        /* Handle errors inside url parameters */
        final String error_type = query == null ? null : query.get("type");
        if (error_type != null) {
            long waitMillis = 30 * 60 * 1000;
            final String waitMinutesStr = query.get("minutesLeft");
            if (waitMinutesStr != null && waitMinutesStr.matches("\\d+")) {
                waitMillis = Long.parseLong(waitMinutesStr) * 60 * 1000;
            }
            if (error_type.equalsIgnoreCase("rate_limit")) {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Rate limit reached", waitMillis);
            } else {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Unknown error happened: " + error_type);
            }
        }
    }

    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account == null) {
            /* No account given -> Check links via special method. */
            if (!ALLOW_MASS_LINKCHECK_WITHOUT_ACCOUNT) {
                return false;
            }
            return checkLinks_old_public(urls);
        }
        if (!ALLOW_MASS_LINKCHECK_WITH_ACCOUNT) {
            return false;
        }
        if (isAccountUsingClassicWebsite(account)) {
            return this.checkLinks_old(urls, account);
        }
        final Browser br = this.createNewBrowserInstance();
        try {
            loginWebsite(br, account, false);
        } catch (final Exception e) {
            logger.log(e);
            logger.info("Login failed -> Cannot use mass-linkchecking");
            return false;
        }
        try {
            final StringBuilder sb = new StringBuilder();
            final Map<String, DownloadLink> links = new HashMap<String, DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    /* Check up to 100 items with one request */
                    if (index == urls.length || links.size() == 100) {
                        break;
                    }
                    final DownloadLink link = urls[index];
                    final String fid = getFUID(link);
                    this.setFallbackFilename(link, fid);
                    links.put(fid, link);
                    index++;
                }
                sb.delete(0, sb.capacity());
                for (final DownloadLink link : links.values()) {
                    if (sb.length() > 0) {
                        sb.append("\n");
                    }
                    sb.append(this.getContentURL(link, true));
                }
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("links", sb.toString());
                br.postPageRaw(this.getWebapiBase() + "/tools/link-checker", JSonStorage.serializeToJson(postdata));
                /* Returns http response 401 when not logged in along with response {"error":"Unauthorized"} */
                final Map<String, Object> entries = checkErrorsWebapi(br, account, null);
                final List<Map<String, Object>> items = (List<Map<String, Object>>) entries.get("results");
                for (final Map<String, Object> item : items) {
                    /* If "importEligible" equals "owned", this file is owned by the currently logged in user. */
                    // final String importEligible = (String) linkinfo.get("importEligible");
                    final Map<String, Object> fileDetails = (Map<String, Object>) item.get("fileDetails");
                    if (fileDetails == null) {
                        continue;
                    }
                    final String viewhash = (String) fileDetails.get("viewhash");
                    final DownloadLink link = links.remove(viewhash);
                    if (link == null) {
                        continue;
                    }
                    /* Assume that status equals "invalid" with error message "File not found or has been deleted.". */
                    final String status = item.get("status").toString();
                    link.setAvailable("valid".equalsIgnoreCase(status));
                    link.setFinalFileName(fileDetails.get("name").toString());
                    link.setDownloadSize(SizeFormatter.getSize(fileDetails.get("size").toString()));
                }
                if (links.size() > 0) {
                    /* Assume that all leftover items are offline. */
                    for (final DownloadLink link : links.values()) {
                        link.setAvailable(false);
                    }
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            logger.log(e);
            return false;
        }
        return true;
    }

    private boolean checkLinks_old(final DownloadLink[] urls, final Account account) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        final Browser br = this.createNewBrowserInstance();
        try {
            loginWebsite(br, account, false);
        } catch (final Exception e) {
            logger.log(e);
            logger.info("Login failed -> Cannot use mass-linkchecking");
            return false;
        }
        try {
            final StringBuilder sb = new StringBuilder();
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    if (index == urls.length || links.size() == 100) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                sb.delete(0, sb.capacity());
                for (final DownloadLink link : links) {
                    this.setFallbackFilename(link, null);
                    if (sb.length() > 0) {
                        sb.append("\r\n");
                    }
                    sb.append(this.getContentURL(link, true));
                }
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("links", sb.toString());
                query.appendEncoded("Submit", "Check Links");
                /* Set referer */
                final String url_mass_linkchecker = getWebsiteBase() + "/account/tools/link-checker.php";
                br.setCurrentURL(url_mass_linkchecker);
                br.postPage(url_mass_linkchecker, query);
                final String trElements[] = br.getRegex("<tr>(.*?)</tr>").getColumn(0);
                if (trElements == null || trElements.length == 0) {
                    logger.warning("Mass linkcheck failed");
                    return false;
                }
                for (final DownloadLink link : links) {
                    final String fileID = getFUID(link);
                    /* Search html snippet belonging to the link we are working on to determine online status. */
                    String filehtml = null;
                    for (final String trElement : trElements) {
                        if (new Regex(trElement, ">\\s*(" + fileID + ".*?</small>\\s*</span>)").getMatch(0) != null) {
                            filehtml = trElement;
                            break;
                        }
                    }
                    if (filehtml == null) {
                        /* Assume that this item is offline */
                        link.setAvailable(false);
                    } else {
                        /* Find file information */
                        /* 2025-10-09: File size unit starts from "KB", there is no "bytes". */
                        final String filesizeStr = new Regex(filehtml, ">\\s*Size:\\s*([\\d\\.]+\\s*(KB|MB|GB|TB))").getMatch(0);
                        if (filesizeStr != null) {
                            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                        }
                        String filenameFromHTML = new Regex(filehtml, "Filename:([^<]+)<br>").getMatch(0);
                        if (filenameFromHTML != null) {
                            filenameFromHTML = Encoding.htmlDecode(filenameFromHTML).trim();
                            link.setName(filenameFromHTML);
                        }
                        /* Check if file is offline: Items can be offline but website can still provide filename or size. */
                        if (filehtml.matches("(?s).*>\\s*(Gültig|Valid)\\s*</abbr>.*")) {
                            link.setAvailable(true);
                            if (filesizeStr == null) {
                                logger.warning("Failed to find filesize");
                            }
                            if (filenameFromHTML == null) {
                                logger.warning("Failed to find filename");
                            }
                        } else {
                            link.setAvailable(false);
                        }
                    }
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            logger.log(e);
            return false;
        }
        return true;
    }

    /* Checks links in batches without the need to be logged in. */
    @Deprecated
    private boolean checkLinks_old_public(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        final Browser br = this.createNewBrowserInstance();
        try {
            final StringBuilder sb = new StringBuilder();
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    if (index == urls.length || links.size() == 100) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                sb.delete(0, sb.capacity());
                for (final DownloadLink link : links) {
                    final String fid = this.getFUID(link);
                    setFallbackFilename(link, fid);
                    if (sb.length() > 0) {
                        sb.append(",");
                    }
                    sb.append("fi:" + fid);
                }
                check_this_batch_of_links: {
                    br.getPage(getWebsiteBase() + "/share/" + sb.toString());
                    try {
                        this.checkErrorsWebsite(null, null, br);
                    } catch (final Exception e) {
                        /* Typically https://www.filefactory.com/error.php?code=325 -> "Invalid Share Link" */
                        logger.log(e);
                        logger.info("All items are invalid/offline");
                        for (final DownloadLink link : links) {
                            link.setAvailable(false);
                        }
                        /* Break out of this label to allow to continue in this loop down below. */
                        break check_this_batch_of_links;
                    }
                    final String trElements[] = br.getRegex("<tr id=\"row_[a-z0-9]*\">(.*?)</tr>").getColumn(0);
                    if (trElements == null || trElements.length == 0) {
                        /* This should never happen */
                        logger.warning("Mass linkcheck failed");
                        return false;
                    }
                    int numberof_offline_items = 0;
                    for (final DownloadLink link : links) {
                        final String fid = getFUID(link);
                        /* Search html snippet belonging to the link we are working on to determine online status. */
                        String filehtml = null;
                        for (final String trElement : trElements) {
                            if (trElement.contains("/file/" + fid)) {
                                filehtml = trElement;
                                break;
                            }
                        }
                        if (filehtml == null) {
                            /* Assume that this item is offline */
                            link.setAvailable(false);
                            numberof_offline_items++;
                        } else {
                            /* Find/parse file information */
                            /* 2025-10-09: File size unit starts from "KB", there is no "bytes". */
                            final String filesizeStr = new Regex(filehtml, ">\\s*Size:\\s*([\\d\\.]+\\s*(KB|MB|GB|TB))").getMatch(0);
                            if (filesizeStr != null) {
                                link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                            }
                            String filenameFromHTML = new Regex(filehtml, "/file/" + fid + "/([^/\"]+)").getMatch(0);
                            if (filenameFromHTML != null) {
                                filenameFromHTML = Encoding.htmlDecode(filenameFromHTML).trim();
                                link.setName(filenameFromHTML);
                            }
                            /* Assume that item is online */
                            link.setAvailable(true);
                        }
                    }
                    if (numberof_offline_items == links.size()) {
                        /*
                         * html contains id="row_" exactly the number of times we have links -> All items are offline / invalid file_id.
                         */
                        logger.info("All items of the current batch of links are offline -> Possible website shows a table with dummy items, all with a file size of '0.00 KB'");
                    }
                }
                if (index == urls.length) {
                    break;
                }
            }
        } catch (final Exception e) {
            logger.log(e);
            return false;
        }
        return true;
    }

    private void setFallbackFilename(final DownloadLink link, String fileID) {
        if (link.isNameSet()) {
            /* Do nothing */
            return;
        }
        /* Set fallback name */
        final String filenameFromURL = this.getFilenameFromURL(link);
        if (filenameFromURL != null) {
            link.setName(filenameFromURL);
            return;
        }
        /* Final fallback */
        if (fileID == null) {
            fileID = this.getFUID(link);
        }
        link.setName(fileID);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = loginWebsite(br, account, true);
        if (ai != null) {
            return ai;
        }
        if (isAccountUsingClassicWebsite(account)) {
            return fetchAccountInfo_old(account);
        }
        return fetchAccountInfo(br, account, account.getStringProperty(PROPERTY_ACCOUNT_LOGIN_TOKEN));
    }

    public boolean isTokenValid(final String token) {
        if (token == null) {
            return false;
        }
        try {
            String jwtContent = new Regex(token, ".*?\\.(.*?)\\.").getMatch(0);
            final int pad = jwtContent.length() % 4;
            switch (pad) {
            case 1:
                jwtContent += "===";
                break;
            case 2:
                jwtContent += "==";
                break;
            case 3:
                jwtContent += "=";
                break;
            case 0:
                break;
            }
            final String jwt = Base64.decodeToString(jwtContent);
            final String exp = new Regex(jwt, "\"exp\"\\s*:\\s*(\\d+)").getMatch(0);
            final long expireOn = Long.parseLong(exp) * 1000;
            if (System.currentTimeMillis() + (5 * 60 * 1000) < expireOn) {
                // only use if still valid for at last 5 minutes
                return true;
            } else {
                return false;
            }
        } catch (Exception e) {
            logger.log(e);
            return false;
        }
    }

    private AccountInfo fetchAccountInfo(final Browser br, final Account account, final String token) throws Exception {
        final WebSocketClient wsc = new WebSocketClient(br, new URL("https://tacit-mammoth-55.eu-west-1.convex.cloud/api/1.31.7/sync")) {
            @Override
            public void writeFrame(WriteWebSocketFrame webSocketFrame) throws IOException {
                super.writeFrame(webSocketFrame);
                try {
                    // required else we will trigger fast CLOSE
                    Thread.sleep(200);
                } catch (InterruptedException e) {
                    throw new IOException(e);
                }
            }
        };
        wsc.connect();
        boolean closedFlag = false;
        try {
            wsc.writeFrame(wsc.buildUTF8TextFrame("{\"connectionCount\":0,\"lastCloseReason\":\"InitialConnect\",\"clientTs\":" + System.currentTimeMillis() + ",\"type\":\"Connect\",\"sessionId\":\"" + UUID.randomUUID() + "\"}"));
            wsc.writeFrame(wsc.buildUTF8TextFrame("{\"type\":\"Authenticate\",\"baseVersion\":0,\"tokenType\":\"User\",\"value\":\"" + token + "\"}"));
            wsc.writeFrame(wsc.buildUTF8TextFrame("{\"type\":\"ModifyQuerySet\",\"baseVersion\":0,\"newVersion\":1,\"modifications\":[]}"));
            wsc.writeFrame(wsc.buildUTF8TextFrame("{\"type\":\"ModifyQuerySet\",\"baseVersion\":1,\"newVersion\":2,\"modifications\":[{\"type\":\"Add\",\"queryId\":0,\"udfPath\":\"users:me\",\"args\":[{}]}]}"));
            nextFrame: while (true) {
                final WebSocketFrame frame = wsc.readNextFrame();
                if (frame == null) {
                    break;
                }
                switch (frame.getOpCode()) {
                case CLOSE:
                    closedFlag = true;
                    break nextFrame;
                case UTF8_TEXT:
                    payload: if (frame.hasPayLoad()) {
                        final Map<String, Object> response = restoreFromString(new String(frame.getPayload(), "UTF-8"), TypeRef.MAP);
                        if ("AuthError".equals(response.get("type"))) {
                            throw new AccountInvalidException();
                        }
                        final List<Map<String, Object>> modifications = (List<Map<String, Object>>) response.get("modifications");
                        if (modifications == null) {
                            break payload;
                        }
                        for (final Map<String, Object> modification : modifications) {
                            if (((Number) modification.get("queryId")).intValue() == 0) {
                                final Map<String, Object> value = (Map<String, Object>) modification.get("value");
                                final AccountInfo ai = new AccountInfo();
                                final String email = (String) value.get("email");
                                if (email != null) {
                                    account.setUser(email);
                                }
                                final Number premiumUntil = (Number) value.get("premiumUntil");
                                if (Boolean.TRUE.equals(value.get("isLifetime"))) {
                                    account.setType(AccountType.LIFETIME);
                                    ai.setValidUntil(-1);
                                } else if (Boolean.TRUE.equals(value.get("isPremium")) && premiumUntil != null) {
                                    account.setType(AccountType.PREMIUM);
                                    ai.setValidUntil(premiumUntil.longValue());
                                } else {
                                    account.setType(AccountType.FREE);
                                    ai.setValidUntil(-1);
                                }
                                return ai;
                            }
                        }
                    }
                    break;
                default:
                    break;
                }
            }
        } finally {
            try {
                if (!closedFlag) {
                    wsc.close();
                } else {
                    wsc.disconnect();
                }
            } catch (IOException e) {
                logger.log(e);
            }
        }
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    /** Obtains account information from old website */
    @Deprecated
    private AccountInfo fetchAccountInfo_old(final Account account) throws Exception {
        if (br.getURL() == null || !StringUtils.endsWithCaseInsensitive(br.getURL(), "/account/")) {
            br.getPage(getWebsiteBase() + "/account/");
        }
        find_and_set_real_user_email: if (account.loadUserCookies() != null) {
            /* Users using cookie login could enter whatever they want into the username field -> Correct that. */
            final String email = br.getRegex("\"email\":\"([^\"]+)").getMatch(0);
            if (email != null && email.contains("@")) {
                account.setUser(email);
            } else {
                logger.warning("Failed to find users' email in html code");
            }
        }
        final AccountInfo ai = new AccountInfo();
        AccountType accountType = null;
        accountType: {
            String accountTypeStr = br.getRegex("member_type\"?\\s*:\\s*\"([^\"]+)").getMatch(0);
            if (accountTypeStr == null) {
                accountTypeStr = br.getRegex("<div\\s*class\\s*=\\s*\"number\"\\s*>\\s*(.*?)\\s*</div>\\s*<[^>]*>\\s*Member").getMatch(0);
            }
            if (accountTypeStr != null) {
                if (accountTypeStr.equalsIgnoreCase("lifetime")) {
                    accountType = AccountType.LIFETIME;
                } else if (accountTypeStr.equalsIgnoreCase("premium")) {
                    accountType = AccountType.PREMIUM;
                }
                /**
                 * Other possible values: </br>
                 * "expired" -> Free Account
                 */
            }
            if (accountType == null) {
                /* Fallback/Old handling */
                if (br.containsHTML("<strong>\\s*(Lebenszeit|Lifetime|Livstid|Levenslang|À vie|生涯|Vitalício|De por vida)\\s*</strong>") || br.containsHTML(">\\s*Lifetime Member\\s*<")) {
                    accountType = AccountType.LIFETIME;
                    // <li class="tooltipster" title="Premium valid until: <strong>30th Jan, 2014</strong>">
                } else if (br.containsHTML("(>|\")\\s*Premium valid until\\s*(<|:)")) {
                    accountType = AccountType.PREMIUM;
                }
            }
        }
        long expireTimestamp = 0;
        expireDate: {
            final String expireTimestampStr = br.getRegex("premium_ends\"?\\s*:\\s*\"?(\\d+)").getMatch(0);
            if (expireTimestampStr != null) {
                expireTimestamp = Long.parseLong(expireTimestampStr) * 1000;
            } else {
                /* Fallback/Old handling */
                final String expireDateStr = br.getRegex("Premium valid until\\s*:\\s*<strong>(.*?)</strong>").getMatch(0);
                if (expireDateStr != null) {
                    expireTimestamp = TimeFormatter.getMilliSeconds(expireDateStr.replaceFirst("(st|nd|rd|th)", ""), "dd MMM, yyyy", Locale.ENGLISH);
                }
            }
            if (accountType == null && expireTimestamp > System.currentTimeMillis()) {
                accountType = AccountType.PREMIUM;
            }
        }
        ai.setUnlimitedTraffic();
        if (accountType != null) {
            account.setType(accountType);
        } else {
            account.setType(AccountType.FREE);
        }
        space: {
            String space = br.getRegex("<strong>\\s*([0-9\\.]+ ?(KB|MB|GB|TB))\\s*</strong>\\s*Free Space").getMatch(0);
            if (space == null) {
                space = br.getRegex("<div\\s*class\\s*=\\s*\"number\"\\s*>\\s*([0-9\\.]+ ?(KB|MB|GB|TB))\\s*</div>\\s*<[^>]*>\\s*Used Space").getMatch(0);
            }
            if (space != null) {
                ai.setUsedSpace(SizeFormatter.getSize(space));
            }
        }
        if (AccountType.PREMIUM.equals(accountType)) {
            if (expireTimestamp > System.currentTimeMillis()) {
                ai.setValidUntil(expireTimestamp);
            }
            // TODO: is this download quota?
            // <li class="tooltipster"
            // title="TrafficShare: 0.00 KB / 1.00 TB<br>Data Packs: 0.00 KB / 0.00 KB<br><strong>Total: 0.00 KB / 1.00 TB</strong>">
            final String traffic = br.getRegex("donoyet(.*?)xyz").getMatch(0);
            if (traffic != null) {
                // OLD SHIT
                String loaded = br.getRegex("You have used (.*?) out").getMatch(0);
                String max = br.getRegex("limit of (.*?)\\. ").getMatch(0);
                if (max != null && loaded != null) {
                    // you don't need to strip characters or reorder its structure. The source is fine!
                    ai.setTrafficMax(SizeFormatter.getSize(max));
                    ai.setTrafficLeft(ai.getTrafficMax() - SizeFormatter.getSize(loaded));
                } else {
                    max = br.getRegex("You can now download up to (.*?) in").getMatch(0);
                    if (max != null) {
                        ai.setTrafficLeft(SizeFormatter.getSize(max));
                    }
                }
            }
        }
        final String createTimestampStr = br.getRegex("created_at\"?\\s*:\\s*\"?(\\d+)").getMatch(0);
        if (createTimestampStr != null) {
            ai.setCreateTime(Long.parseLong(createTimestampStr) * 1000);
        }
        return ai;
    }

    private AccountInfo loginWebsite(final Browser br, final Account account, final boolean validateCookies) throws Exception {
        synchronized (account) {
            setBrowserExclusive();
            final Cookies cookies = account.loadCookies("");
            final Cookies userCookies = account.loadUserCookies();
            verify: if (cookies != null || userCookies != null) {
                if (userCookies != null) {
                    br.setCookies(userCookies);
                } else {
                    br.setCookies(cookies);
                }
                final String token = account.getStringProperty(PROPERTY_ACCOUNT_LOGIN_TOKEN);
                if (!isTokenValid(token)) {
                    break verify;
                } else if (!validateCookies) {
                    /* Do not verify cookies/token */
                    return null;
                }
                /* Verify cookies */
                try {
                    final AccountInfo ai = fetchAccountInfo(br, account, token);
                    logger.info("Token login successful");
                    if (userCookies == null) {
                        account.saveCookies(br.getCookies(br.getHost()), "");
                    }
                    return ai;
                } catch (final AccountInvalidException e) {
                    logger.info("Token login failed");
                    br.clearCookies(null);
                    if (userCookies != null) {
                        /* Dead end */
                        logger.info("User Cookie login failed");
                        if (account.hasEverBeenValid()) {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                        } else {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                        }
                    }
                }
            }
            logger.info("Performing full login");
            br.getPage(getWebsiteBase() + "/signin");
            br.setCookie(getHost(), "recaptcha-verified", "true"); // Skips login captcha
            {
                final Map<String, Object> loginJson = new HashMap<String, Object>();
                loginJson.put("action", "auth:signIn");
                final Map<String, Object> args = new HashMap<String, Object>();
                loginJson.put("args", args);
                args.put("provider", "password");
                final Map<String, Object> params = new HashMap<String, Object>();
                args.put("params", params);
                params.put("email", account.getUser());
                params.put("flow", "signIn");
                params.put("password", account.getPass());
                final Request request = br.createJSonPostRequest(getWebapiBase() + "/auth", loginJson);
                br.getPage(request);
                // response contains token json
            }
            /**
             * 2026-04-22: Also always answers with: "refreshToken": "dummy" <br>
             * -> lol
             */
            final Map<String, Object> entries = checkErrorsWebapi(br, account, null);
            String token = (String) entries.get("token");
            if (token == null) {
                final Map<String, Object> tokens = (Map<String, Object>) entries.get("tokens");
                token = tokens.get("token").toString();
            }
            final AccountInfo ai = fetchAccountInfo(br, account, token);
            account.saveCookies(br.getCookies(br.getHost()), "");
            account.setProperty(PROPERTY_ACCOUNT_LOGIN_TOKEN, token);
            return ai;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownloadWebsite(link, null);
    }

    public void handleDownloadWebsite(final DownloadLink link, final Account account) throws Exception {
        requestFileInformationWebsite(account, link);
        if (this.dl == null) {
            if (link.isPasswordProtected()) {
                /*
                 * 2025-09-10: Website json implies that password protected links exist but I was unable to create- or find such test-links.
                 */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Password protected links are not yet supported");
            }
            final boolean isPremium = this.isPremiumAccount(account) || this.isPremium(br);
            if (!isPremium) {
                // JDownloader ads
                if (checkShowFreeDialog(getHost())) {
                    showFreeDialog(getHost());
                }
            }
            String finallink = null;
            old_website: {
                /* Download via old/legacy/classic website */
                final boolean isSpecialImageDownload = br.getURL().matches("(?i)https?://[^/]+/preview/.+");
                final boolean isSpecialTrafficShareDownload = br.getURL().matches("(?i)https?://[^/]+/trafficshare/.+");
                if (isSpecialTrafficShareDownload) {
                    /* Trafficshare link = Can be downloaded like a premium user, even by free users */
                    finallink = regexTrafficShareDirecturl_old_website(br);
                } else {
                    finallink = br.getRegex("data-href\\s*=\\s*\"([^\"]*)\"\\s*>\\Slow Download").getMatch(0);
                    if (finallink == null && isSpecialImageDownload) {
                        finallink = br.getRegex("id=\"image_info_dl\"[^>]*href=\"(https?://[^\"]+)\"").getMatch(0);
                    }
                    if (finallink == null) {
                        /* Premium download */
                        finallink = br.getRegex("href=\"(https?://[^\"]+)\"[^>]*>\\s*Start Download").getMatch(0);
                        if (finallink == null) {
                            /* Premium traffic share download */
                            finallink = regexTrafficShareDirecturl_old_website(br);
                        }
                    }
                }
                if (finallink == null) {
                    if (!this.isClassicFile(link)) {
                        /* Try download via new website */
                        break old_website;
                    }
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                /* No pre download wait time needed for special image links and premium users */
                if (!isSpecialImageDownload && !isPremium && !isSpecialTrafficShareDownload) {
                    final String delaySecondsStr = br.getRegex("countdown_clock\"\\s*data-delay\\s*=\\s*\"(\\d+)\"").getMatch(0);
                    long delayMillis = TimeUnit.SECONDS.toMillis(60);
                    if (delaySecondsStr != null) {
                        delayMillis = TimeUnit.SECONDS.toMillis(Integer.parseInt(delaySecondsStr));
                    } else {
                        logger.warning("Failed to find pre download wait");
                    }
                    sleep(delayMillis, link);
                }
            }
            if (finallink == null) {
                final String dltoken = getString(br, "requestToken");
                final String fid = this.getFUID(link);
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("hash", fid);
                if (dltoken != null) {
                    /**
                     * Not all items require a "download token". <br>
                     * For example small images can be downloaded without token.
                     */
                    postdata.put("token", dltoken);
                }
                if (isPremium) {
                    postdata.put("type", "premium");
                } else {
                    postdata.put("type", "free");
                }
                /* Website has 45-60 seconds of pre download wait time for free (& free-account) users which can be skipped. */
                br.postPageRaw(this.getWebapiBase() + "/download/initiate", JSonStorage.serializeToJson(postdata));
                final Map<String, Object> entries = checkErrorsWebapi(br, account, link);
                finallink = (String) entries.get("url");
                if (finallink == null) {
                    /* 2025-10-01 */
                    finallink = (String) entries.get("downloadUrl");
                }
                if (StringUtils.isEmpty(finallink)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find final downloadurl");
                }
                Object fileSizeEntry = entries.get("fileSize");
                if (fileSizeEntry == null) {
                    /* 2025-10-01: atm fields "fileSize" and "filesize" both exist at the same time. */
                    fileSizeEntry = entries.get("filesize");
                }
                if (fileSizeEntry != null && fileSizeEntry.toString().matches("\\d+")) {
                    link.setVerifiedFileSize(Long.parseLong(fileSizeEntry.toString()));
                }
                final String filename = (String) entries.get("filename");
                if (filename != null) {
                    link.setFinalFileName(filename);
                }
                if (!Boolean.TRUE.equals(entries.get("directDownload")) && AccountType.PREMIUM.is(account)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, finallink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                checkErrorsWebsite(link, account, br);
                throwConnectionExceptions(br, dl.getConnection());
                if (br.getRequest().getHtmlCode().length() == 0) {
                    final String errormessage = "Got blank page";
                    throw new PluginException(LinkStatus.ERROR_FATAL, errormessage);
                } else if (br.getRequest().getHtmlCode().length() <= 100 && !br.containsHTML("<html")) {
                    /* Assume that we got a small plaintext error response */
                    /* e.g. 2025-10-24: Couldn't get valid connection to DB */
                    final String plaintextError = br.getRequest().getHtmlCode().trim();
                    throw new PluginException(LinkStatus.ERROR_FATAL, plaintextError);
                }
                logger.warning("Unknown error happened");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    private String regexTrafficShareDirecturl_old_website(final Browser br) {
        return br.getRegex("id=\"download-premium\"[^>]*>\\s*<a href=\"(https?://[^\"]+)\"").getMatch(0);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownloadWebsite(link, account);
    }

    private Map<String, Object> checkErrorsWebapi(final Browser br, final Account account) throws PluginException, InterruptedException {
        return checkErrorsWebapi(br, account, null);
    }

    private Map<String, Object> checkErrorsWebapi(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        try {
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            checkErrorsWebapi(br, account, link, entries);
            return entries;
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String msg = "Invalid API response";
            final long waitMillis = 60 * 1000l;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, waitMillis, ignore);
            } else {
                throw new AccountUnavailableException(ignore, msg, 60 * 1000);
            }
        }
    }

    private void checkErrorsWebapi(final Browser br, final Account account, final DownloadLink link, final Map<String, Object> entries) throws PluginException, InterruptedException {
        // TODO
        if (StringUtils.containsIgnoreCase(br.getURL(), "/api/auth/session")) {
            if (entries == null || entries.size() == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else if (entries.get("user") == null) {
                throw new AccountInvalidException();
            }
        }
        if (StringUtils.containsIgnoreCase(br.getURL(), "/api/auth/csrf ")) {
            if (entries == null || entries.size() == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else if (entries.get("csrfToken") == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        if (StringUtils.containsIgnoreCase(br.getURL(), "signin?error=CredentialsSignin&code=credentials")) {
            throw new AccountInvalidException();
        }
        if (br.getHttpConnection().getResponseCode() == 400) {
            /* 2026-04-22: POST on "/api/auth" with invalid credentials */
            throw new AccountInvalidException();
        }
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account account) {
        return false;
    }

    @Override
    public boolean hasAutoCaptcha() {
        return false;
    }

    private String getFUID(final DownloadLink link) {
        String fuid = new Regex(link.getPluginPatternMatcher(), PATTERN_FILE).getMatch(0);
        if (fuid != null) {
            return fuid;
        }
        fuid = new Regex(link.getPluginPatternMatcher(), PATTERN_TRAFFIC_SHARE).getMatch(0);
        return fuid;
    }

    private boolean isPremiumAccount(final Account account) {
        return AccountType.PREMIUM.is(account);
    }
}