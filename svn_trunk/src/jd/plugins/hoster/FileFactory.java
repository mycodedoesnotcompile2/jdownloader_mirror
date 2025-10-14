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
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
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

@HostPlugin(revision = "$Revision: 51662 $", interfaceVersion = 2, names = {}, urls = {})
public class FileFactory extends PluginForHost {
    public FileFactory(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + this.getHost() + "/pricing");
    }

    @Override
    public FEATURE[] getFeatures() {
        return new FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL, LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        // blocking default UA
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, Request.getSuggestedUserAgent("142.0"));
        br.setCookie(getHost(), "filefactory_relaunch", "seen");
        br.setCookie(getHost(), "cookieConsent", "accepted");
        br.setCookie(getHost(), "locale", "en_US.utf8");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + this.getHost() + "/legal/terms";
    }

    public static final String PROPERTY_CLASSIC = "classic_website";

    /** Returns true if this link shall be used with the older filefactory.com website accessible via classic.filefactory.com. */
    private boolean isClassicFile(final DownloadLink link) throws PluginException {
        return link.hasProperty(PROPERTY_CLASSIC) || (false/* disabled because old classic links do redirect to www again */ && link.getPluginPatternMatcher().contains("classic.filefactory.com"));
    }

    private String getContentURL(final DownloadLink link) throws PluginException {
        return getContentURL(link, false);
    }

    private String getContentURL(final DownloadLink link, boolean disableClassic) throws PluginException {
        if (isClassicFile(link) && !disableClassic) {
            return "https://classic." + getHost() + "/file/" + this.getFUID(link);
        } else {
            return "https://www." + getHost() + "/file/" + this.getFUID(link);
        }
    }

    private String getWebapiBase() {
        return "https://www." + getHost() + "/api";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (this.isPremiumAccount(account)) {
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
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

    private static final Pattern PATTERN_FILE = Pattern.compile("/(?:file|image|preview|stream)/([a-z0-9]+)(/([^/]+))?", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.|classic\\.)?" + buildHostsPatternPart(domains) + PATTERN_FILE.pattern());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), PATTERN_FILE);
        final String filenameFromURL = urlinfo.getMatch(2);
        if (filenameFromURL != null) {
            return URLEncode.decodeURIComponent(filenameFromURL);
        }
        final String file_id = urlinfo.getMatch(0);
        return file_id;
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
            if ("classic.filefactory.com".equals(br.getHost(true))) {
                link.setProperty(PROPERTY_CLASSIC, Boolean.TRUE);
            } else {
                link.removeProperty(PROPERTY_CLASSIC);
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
            String jsonFileName = getString(br, "disp_filename");// json
            if (jsonFileName != null) {
                link.setFinalFileName(jsonFileName);
            }
            final String jsonFilesizeBytesStr = getString(br, "size");// json
            if (jsonFilesizeBytesStr != null) {
                link.setVerifiedFileSize(Long.parseLong(jsonFilesizeBytesStr));
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

    private String getString(Browser br, final String key) {
        final String value = br.getRegex("\"" + Pattern.quote(key) + "(?:\\\\)?\"\\s*:\\s*(?:\\\\)?\"(.*?)(?:\\\\)?\"").getMatch(0);
        if (value == null) {
            return null;
        }
        return restoreFromString("\"" + value + "\"", TypeRef.STRING);
    }

    public void checkErrorsWebsite(final DownloadLink link, final Account account, final Browser br) throws PluginException, MalformedURLException {
        // TODO: Add more error handling
        final Boolean requiresPremium = getBoolean(br, "requiresPremium");
        // final Boolean isFree = getBoolean(br, "isFree");
        final Boolean userIsPremium = getBoolean(br, "userIsPremium");
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
        if (Boolean.FALSE.equals(userIsPremium) && Boolean.TRUE.equals(requiresPremium)) {
            throw new AccountRequiredException();
        }
        if (error_code != null) {
            /* "Invalid Download Link" */
            if ("251".equals(error_code)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
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
                throw new PluginException(LinkStatus.ERROR_FATAL, error_code);
            } else if ("300".equals(error_code)) {
                /* Invalid folder link */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if ("325".equals(error_code)) {
                /* Invalid Share Link e.g. https://www.filefactory.com/share/fi:xxxyyy */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if ("FILE_NOT_FOUND".equals(error_code)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                logger.info("Unknown error happened: " + error_code);
                throw new PluginException(LinkStatus.ERROR_FATAL, error_code);
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
                logger.info("Unknown error happened: " + error_type);
                throw new PluginException(LinkStatus.ERROR_FATAL, error_type);
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
            return checkLinks_old_public(urls);
        }
        if (account.hasProperty(PROPERTY_CLASSIC)) {
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
                final String url_mass_linkchecker = "https://www." + getHost() + "/account/tools/link-checker.php";
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
                br.getPage("https://www." + getHost() + "/share/" + sb.toString());
                try {
                    this.checkErrorsWebsite(null, null, br);
                } catch (final Exception e) {
                    /* e.g. https://www.filefactory.com/error.php?code=325 -> "Invalid Share Link" */
                    logger.log(e);
                    return false;
                }
                final String trElements[] = br.getRegex("<tr id=\"row_[a-z0-9]*\">(.*?)</tr>").getColumn(0);
                if (trElements == null || trElements.length == 0) {
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
                        /* Find file information */
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
                    /* html contains id="row_" exactly the number of times we have links -> All items are offline / invalid file_id. */
                    logger.info("All items of the current batch of links are offline -> Possible website shows a table with dummy items, all with a file size of '0.00 KB'");
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
        final String filenameFromURL = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(2);
        if (filenameFromURL != null) {
            link.setName(URLEncode.decodeURIComponent(filenameFromURL));
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
        loginWebsite(br, account, true);
        if (account.hasProperty(PROPERTY_CLASSIC)) {
            return fetchAccountInfo_old(account);
        }
        final AccountInfo ai = new AccountInfo();
        br.getPage(this.getWebapiBase() + "/dashboard");
        final Map<String, Object> entries = this.checkErrorsWebapi(br, account);
        final Map<String, Object> settings = (Map<String, Object>) entries.get("settings");
        if (Boolean.TRUE.equals(entries.get("isLifetime"))) {
            account.setType(AccountType.LIFETIME);
        } else if (Boolean.TRUE.equals(entries.get("isPremium"))) {
            account.setType(AccountType.PREMIUM);
            String nextRebillDate = "Never";
            if (Boolean.TRUE.equals(entries.get("hasActiveRebill"))) {
                nextRebillDate = entries.get("nextRebillDate").toString();
            }
            final int daysRemaining = ((Number) entries.get("daysRemaining")).intValue();
            ai.setValidUntil(System.currentTimeMillis() + TimeUnit.DAYS.toMillis(daysRemaining), br);
            ai.setStatus(AccountType.PREMIUM.getLabel() + " | Next rebill date: " + nextRebillDate);
        } else {
            /* Expired/free account */
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(getMaxSimultanFreeDownloadNum());
        }
        final String usedSpaceGB_Str = (String) entries.get("usedSpace");
        if (usedSpaceGB_Str != null && usedSpaceGB_Str.matches("\\d+")) {
            ai.setUsedSpace(Long.parseLong(usedSpaceGB_Str) * 1024 * 1024 * 1024);
        }
        if (account.loadUserCookies() != null) {
            /* Ensure to have unique usernames when user is using cookie login */
            final String email = (String) settings.get("email");
            if (email != null) {
                account.setUser(email);
            } else {
                /* Possibly Google login user? */
                logger.warning("WTF account has no email?");
            }
        }
        return ai;
    }

    /** Obtains account information from old website */
    private AccountInfo fetchAccountInfo_old(final Account account) throws Exception {
        if (br.getURL() == null || !StringUtils.endsWithCaseInsensitive(br.getURL(), "/account/")) {
            br.getPage("https://www." + this.getHost() + "/account/");
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
                ai.setUsedSpace(space);
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

    private void loginWebsite(final Browser br, final Account account, final boolean validateCookies) throws Exception {
        synchronized (account) {
            setBrowserExclusive();
            final Cookies cookies = account.loadCookies("");
            final Cookies userCookies = account.loadUserCookies();
            if (cookies != null || userCookies != null) {
                if (userCookies != null) {
                    br.setCookies(userCookies);
                } else {
                    br.setCookies(cookies);
                }
                if (!validateCookies) {
                    /* Do not verify cookies */
                    return;
                }
                /* Verify cookies */
                try {
                    this.checkLoginStatus(br, account);
                    logger.info("Cookie login successful");
                    if (userCookies == null) {
                        account.saveCookies(br.getCookies(br.getHost()), "");
                    }
                    return;
                } catch (final AccountInvalidException e) {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    if (userCookies != null) {
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
            br.getPage("https://www." + getHost() + "/signin?from=%2Fdashboard");
            final Form loginform_old_website = br.getFormbyProperty("id", "sign_in");
            old_website: if (loginform_old_website != null) {
                logger.info("Full login via old website");
                if (loginform_old_website.containsHTML("id=\"google-recaptcha\"")) {
                    final String recaptchaV2Response = new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken();
                    loginform_old_website.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                }
                loginform_old_website.put("loginEmail", Encoding.urlEncode(account.getUser()));
                loginform_old_website.put("loginPassword", Encoding.urlEncode(account.getPass()));
                // br.getHeaders().put("Referer", br.getURL("/account/").toExternalForm());
                br.submitForm(loginform_old_website);
                if (!this.isLoggedin_old(br)) {
                    throw new AccountInvalidException();
                }
                account.setProperty(PROPERTY_CLASSIC, true);
            } else {
                logger.info("Full login via new website");
                if (false) {
                    // not required at the moment
                    br.setCookie(br.getHost(), "cookieConsent", "accepted");
                    br.setCookie(br.getHost(), "cookieConsentTimestamp", "2025-09-09T14:12:12.097Z");
                }
                if (false) {
                    // not required at the moment?! only sets the recaptcha-verified cookie, LOL
                    final CaptchaHelperHostPluginRecaptchaV2 rc = new CaptchaHelperHostPluginRecaptchaV2(this, br, "6Le6wT0rAAAAAEOzVh77jsWDtqGkwbXcvuPdmaeW") {
                        @Override
                        protected boolean isEnterprise() {
                            return true;
                        }

                        @Override
                        protected String getSiteUrl() {
                            return "https://www.filefactory.com/signin";
                        }

                        @Override
                        protected Map<String, Object> getV3Action() {
                            final Map<String, Object> ret = new HashMap<String, Object>();
                            ret.put("action", "SIGNIN");
                            return ret;
                        }
                    };
                    final String recaptchaV2Response = rc.getToken();
                    final Map<String, Object> postdata1 = new HashMap<String, Object>();
                    postdata1.put("action", "SIGNIN");
                    postdata1.put("captchaToken", recaptchaV2Response);
                    final Browser brc = br.cloneBrowser();
                    brc.postPageRaw(this.getWebapiBase() + "/auth/pre-auth-check", JSonStorage.serializeToJson(postdata1));
                    final Map<String, Object> entries1 = checkErrorsWebapi(brc, account);
                    if (!Boolean.TRUE.equals(entries1.get("success"))) {
                        // throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                    }
                }
                br.setCookie(getHost(), "recaptcha-verified", "true");
                br.setCookie(br.getHost(), "__Secure-authjs.callback-url", "https%3A%2F%2Fwww." + getHost());
                Browser brc = br.cloneBrowser();
                brc.getPage(this.getWebapiBase() + "/auth/csrf");
                final Map<String, Object> entries2 = checkErrorsWebapi(brc, account);
                final String csrfToken = entries2.get("csrfToken").toString();
                final Form loginForm = new Form();
                loginForm.setMethod(MethodType.POST);
                loginForm.setAction(this.getWebapiBase() + "/auth/callback/credentials");
                loginForm.put("email", URLEncoder.encode(account.getUser(), "UTF-8"));
                loginForm.put("password", URLEncoder.encode(account.getPass(), "UTF-8"));
                loginForm.put("csrfToken", csrfToken);
                loginForm.put("callbackUrl", "%2Fdashboard");
                br.submitForm(loginForm);
                checkErrorsWebapi(br, account, null, null);
                checkLoginStatus(br, account);
                account.removeProperty(PROPERTY_CLASSIC);
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
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
            final boolean isPremium = this.isPremiumAccount(account);
            if (!isPremium) {
                // ads
                if (checkShowFreeDialog(getHost())) {
                    showFreeDialog(getHost());
                }
            }
            String finallink = null;
            old_website: {
                /* Download via old/legacy/classic website */
                final boolean isSpecialImageDownload = br.getURL().matches("https?://[^/]+/preview/.+");
                finallink = br.getRegex("data-href\\s*=\\s*\"([^\"]*)\"\\s*>\\Slow Download").getMatch(0);
                if (finallink == null && isSpecialImageDownload) {
                    finallink = br.getRegex("id=\"image_info_dl\"[^>]*href=\"(https?://[^\"]+)\"").getMatch(0);
                }
                if (finallink == null) {
                    /* Premium download */
                    finallink = br.getRegex("href=\"(https?://[^\"]+)\"[^>]*>\\s*Start Download").getMatch(0);
                    if (finallink == null) {
                        /* Premium traffic share download */
                        finallink = br.getRegex("<a href=\"(https?://\\w+\\.filefactory\\.com/get/t/[^\"]+)\"[^\r\n]*Download with FileFactory TrafficShare").getMatch(0);
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
                if (!isSpecialImageDownload && !isPremium) {
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
                     * For example small images can be downloaded without that token.
                     */
                    postdata.put("token", dltoken);
                }
                // if (isPremium) {
                // postdata.put("type", "premium");
                // } else {
                // postdata.put("type", "free");
                // }
                /* 2025-09-09: Even in premium mode, the value of this is "free". */
                postdata.put("type", "free");
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
                final long fileSize = JavaScriptEngineFactory.toLong(fileSizeEntry, -1l);
                if (fileSize != -1) {
                    link.setVerifiedFileSize(fileSize);
                }
                final String filename = (String) entries.get("filename");
                if (filename != null) {
                    link.setFinalFileName(filename);
                }
                if (!Boolean.TRUE.equals(entries.get("directDownload"))) {
                    // TODO
                    if (AccountType.PREMIUM.is(account)) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
            }
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, finallink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                handleConnectionErrors(br, dl.getConnection());
                checkErrorsWebsite(link, account, br);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    @Override
    protected void throwFinalConnectionException(Browser br, URLConnectionAdapter con) throws PluginException, IOException {
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownloadWebsite(link, account);
    }

    /**
     * Checks if we're logged in via Web-API. <br>
     * Throws AccountInvalidException on invalid login.
     */
    private Map<String, Object> checkLoginStatus(final Browser br, final Account account) throws IOException, PluginException, InterruptedException {
        final class OldWebsite {
            private Map<String, Object> checkLoginStatus(final Browser br, final Account account) throws IOException, PluginException, InterruptedException {
                br.getPage("https://www." + FileFactory.this.getHost() + "/account/");
                if (!checkLoginStatus_old(br, account)) {
                    account.removeProperty(PROPERTY_CLASSIC);
                    throw new AccountInvalidException("Session expired");
                }
                logger.info("Successfully logged in via classic website");
                account.setProperty(PROPERTY_CLASSIC, true);
                return null;
            }
        }
        final class NewWebsite {
            private Map<String, Object> checkLoginStatus(final Browser br, final Account account) throws IOException, PluginException, InterruptedException {
                br.getPage(FileFactory.this.getWebapiBase() + "/auth/session");
                if (br.getHttpConnection().getResponseCode() == 404) {
                    return null;
                }
                try {
                    final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    account.removeProperty(PROPERTY_CLASSIC);
                    checkErrorsWebapi(br, account, null, entries);
                    return entries;
                } catch (final JSonMapperException ignore) {
                    logger.log(ignore);
                }
                return null;
            }
        }
        if (account.hasProperty(PROPERTY_CLASSIC)) {
            return new OldWebsite().checkLoginStatus(br, account);
        }
        final Map<String, Object> ret = new NewWebsite().checkLoginStatus(br, account);
        if (ret != null) {
            return ret;
        }
        return new OldWebsite().checkLoginStatus(br, account);
    }

    private boolean checkLoginStatus_old(final Browser br, final Account account) throws IOException {
        br.getPage("https://www." + getHost() + "/account/");
        if (isLoggedin_old(br)) {
            // logger.info("Successfully logged in via classic website");
            return true;
        } else {
            return false;
        }
    }

    private boolean isLoggedin_old(final Browser br) {
        return br.containsHTML("/member/signout\"");
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
    }

    private boolean isPasswordProtectedFile(final Browser br) {
        if (Boolean.TRUE.equals(getBoolean(br, "requiresPassword"))) {
            return true;
        } else {
            return false;
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
        return new Regex(link.getPluginPatternMatcher(), PATTERN_FILE).getMatch(0);
    }

    private boolean isPremiumAccount(final Account account) {
        return AccountType.PREMIUM.is(account);
    }
}