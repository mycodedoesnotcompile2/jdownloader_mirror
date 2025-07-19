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

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.config.SankakucomplexComConfig;
import org.jdownloader.plugins.components.config.SankakucomplexComConfig.AccessMode;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.SankakucomplexComCrawler;

@HostPlugin(revision = "$Revision: 51232 $", interfaceVersion = 2, names = { "sankakucomplex.com" }, urls = { "https?://(?:beta|chan|idol|www)\\.sankakucomplex\\.com/(?:[a-z]{2}/)?(?:post/show|posts)/([A-Za-z0-9]+)" })
public class SankakucomplexCom extends PluginForHost {
    public SankakucomplexCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://chan.sankakucomplex.com/users/signup");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        setDefaultCookies(br, getHost());
        br.setFollowRedirects(true);
        return br;
    }

    private static void setDefaultCookies(final Browser br, final String host) {
        br.setCookie(host, "auto_page", "1");
        br.setCookie(host, "hide_carousel_ai", "1");
        br.setCookie(host, "hide_resized_notice", "1");
        br.setCookie(host, "hide-news-ticker", "1");
        br.setCookie(host, "locale", "en");
        br.setCookie(host, "lang", "en");
        // br.setCookie(host, "blacklisted_tags", "");
        br.setCookie(host, "mode", "view");
        br.setCookie(host, "v", "0");
    }

    private static final boolean ACCESS_MODE_AUTO_PREFER_API_MODE      = false;
    public static final String   PROPERTY_UPLOADER                     = "uploader";
    public static final String   PROPERTY_DIRECTURL                    = "directurl";
    public static final String   PROPERTY_DIRECTURL_FILENAME           = "directurl_filename";
    public static final String   PROPERTY_BOOK_TITLE                   = "book_title";
    public static final String   PROPERTY_TAGS_COMMA_SEPARATED         = "tags_comma_separated";
    public static final String   PROPERTY_IS_PREMIUMONLY               = "is_premiumonly";
    public static final String   PROPERTY_POSITION_NUMBER              = "position_number";
    public static final String   PROPERTY_PAGE_NUMBER                  = "page_number";
    public static final String   PROPERTY_PAGE_NUMBER_MAX              = "page_number_max";
    public static final String   PROPERTY_SOURCE                       = "source";
    public static final String   PROPERTY_DATE_PUBLISHED               = "date_published";
    /* Contains file extension hint. */
    public static final String   PROPERTY_EXT_HINT                     = "ext_hint";
    private final String         TIMESTAMP_LAST_TIME_FILE_MAYBE_BROKEN = "timestamp_last_time_file_maybe_broken";
    private static final String  PROPERTY_ACCOUNT_ACCESS_TOKEN         = "access_token";
    /* 2024-04-26: Refresh-token is currently not used. */
    private static final String  PROPERTY_ACCOUNT_REFRESH_TOKEN        = "refresh_token";
    /* Don't touch the following! */
    private static AtomicInteger freeRunning                           = new AtomicInteger(0);
    private static AtomicInteger premiumRunning                        = new AtomicInteger(0);

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/";
    }

    @Override
    public void init() {
        try {
            Browser.setBurstRequestIntervalLimitGlobal(this.getHost(), 3000, 20, 60000);
        } catch (Exception e) {
        }
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

    public int getMaxChunks(final DownloadLink link, final Account account) {
        /* 2024-06-25: Set to 1 based on logs. */
        return 1;
    }

    @Override
    public String getPluginContentURL(DownloadLink link) {
        final String fileID = this.getFID(link);
        return "https://chan." + getHost() + "/posts/" + fileID;
    }

    private boolean allowUseAPI(final Account account) {
        final SankakucomplexComConfig cfg = PluginJsonConfig.get(SankakucomplexComConfig.class);
        final AccessMode mode = cfg.getLinkcheckAccessMode();
        if (mode == AccessMode.API) {
            return true;
        } else if (mode == AccessMode.API && ACCESS_MODE_AUTO_PREFER_API_MODE) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final AvailableStatus status = requestFileInformation(link, null);
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (status == AvailableStatus.TRUE && link.hasProperty(PROPERTY_IS_PREMIUMONLY) && allowUseAPI(null) && !link.isSizeSet() && account != null) {
            /* Workaround for when some file information is missing when link leads to account-only content and is checked via API. */
            logger.info("Failed to find file size via API and item is only available via account while we have an account -> Checking status again via website in hope to obtain all information");
            return requestFileInformationWebsite(link, account, false);
        } else {
            return status;
        }
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        final String fileID = this.getFID(link);
        final boolean fileIDIsAPICompatible;
        if (fileID.matches("[A-Fa-f0-9]{32}")) {
            fileIDIsAPICompatible = false;
        } else {
            fileIDIsAPICompatible = true;
        }
        if (fileIDIsAPICompatible && allowUseAPI(account)) {
            return requestFileInformationAPI(link, account, false);
        } else {
            return requestFileInformationWebsite(link, account, false);
        }
    }

    private void setWeakFilename(final DownloadLink link) {
        if (link.isNameSet()) {
            return;
        }
        final String fileID = this.getFID(link);
        final String assumedExt = link.getStringProperty(PROPERTY_EXT_HINT, "png");
        link.setName(fileID + "." + assumedExt);
    }

    private AvailableStatus requestFileInformationWebsite(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        setWeakFilename(link);
        final String fileID = this.getFID(link);
        final String host = new URL(link.getPluginPatternMatcher()).getHost();
        setDefaultCookies(br, host);
        final String contenturl = getPluginContentURL(link);
        if (account != null) {
            login(account, contenturl, true);
        } else {
            br.getPage(contenturl);
        }
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("<title>\\s*404: Page Not Found\\s*<")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.canHandle(br.getURL())) {
            /* E.g. redirect to https://chan.sankakucomplex.com/de/posts */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (StringUtils.endsWithCaseInsensitive(br.getURL(), "/show_empty")) {
            /* E.g. redirect to https://chan.sankakucomplex.com/de/posts/show_empty */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (br.containsHTML(">\\s*You lack the access rights required to view this content") || br.containsHTML(">\\s*Nothing is visible to you here")) {
            /* Content can only be downloaded by premium users or mature content which can only be downloaded by logged in users. */
            link.setProperty(PROPERTY_IS_PREMIUMONLY, true);
        } else {
            link.removeProperty(PROPERTY_IS_PREMIUMONLY);
        }
        String dllink = br.getRegex("Original:\\s*(?:</span>\\s*)?<a[^>]*href=\"(//[^<>\"]*?)\"").getMatch(0);
        if (dllink == null) {
            dllink = br.getRegex("<a href=\"(//[^<>\"]*?)\">\\s*Save this file").getMatch(0);
            if (dllink == null) {
                /* 2024-04-26: First = sample, last = full/original */
                final String[] fulldlurls = br.getRegex("Post\\.prepare_download\\(\\&#39;(//[^\"<>]+)&#39;,").getColumn(0);
                if (fulldlurls != null && fulldlurls.length > 0) {
                    dllink = fulldlurls[fulldlurls.length - 1];
                }
            }
        }
        String ext = null;
        if (dllink == null) {
            /* 2021-02-23: Image download - if the upper handling fails on videos, this may make us download an image vs a video */
            logger.info("Download of original file is not possible");
            dllink = br.getRegex("<meta content=\"(//[^<>\"]+)\" property=og:image>").getMatch(0);
        }
        if (dllink != null) {
            dllink = Encoding.htmlOnlyDecode(dllink);
            dllink = br.getURL(dllink).toExternalForm();
        }
        if (dllink != null) {
            ext = Plugin.getFileNameExtensionFromURL(dllink);
        }
        link.setFinalFileName(this.applyFilenameExtension(fileID, ext));
        final String filesizeBytesStr = br.getRegex("([0-9,]+) bytes").getMatch(0);
        if (filesizeBytesStr != null) {
            link.setDownloadSize(Long.parseLong(filesizeBytesStr.replace(",", "")));
        } else {
            /* Look for rough file size aka "177 KB". */
            final String filesizeRoughStr = br.getRegex("\\d+x\\d+ \\((\\d+ [A-Za-z]{2,5}) [^\\)]*\\)").getMatch(0);
            if (filesizeRoughStr != null) {
                link.setDownloadSize(SizeFormatter.getSize(filesizeRoughStr));
            }
        }
        /* Crawl tags */
        String tagsNewlineSeparatedStr = br.getRegex("<textarea[^>]*id=\"post_tags\"[^>]*>([^<]+)</textarea>").getMatch(0);
        if (!StringUtils.isEmpty(tagsNewlineSeparatedStr)) {
            tagsNewlineSeparatedStr = tagsNewlineSeparatedStr.trim();
            String tagsCommaSeparated = tagsNewlineSeparatedStr.replaceAll("\n", ",");
            link.setProperty(PROPERTY_TAGS_COMMA_SEPARATED, tagsCommaSeparated);
            if (PluginJsonConfig.get(SankakucomplexComConfig.class).isSetCommaSeparatedTagsOfPostsAsComment()) {
                link.setComment(tagsCommaSeparated);
            }
        }
        final String datePublished = br.getRegex("Posted\\s*:?\\s*</span>\\s*<a[^>]*title=\"(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        if (datePublished != null) {
            link.setProperty(datePublished, PROPERTY_DATE_PUBLISHED);
        } else {
            logger.warning("Failed to find publish date");
        }
        if (dllink != null) {
            this.storeDirecturl(link, dllink);
            if (!isDownload && !link.isSizeSet()) {
                /* Obtain file size from header */
                try {
                    final Browser brc = br.cloneBrowser();
                    prepareDownloadHeaders(brc);
                    basicLinkCheck(brc, br.createHeadRequest(dllink), link, fileID, ext);
                } catch (final Exception e) {
                    logger.log(e);
                    logger.info("Final downloadurl did not lead to file -> File broken/unavailable serverside?");
                    return AvailableStatus.TRUE;
                }
            }
        }
        return AvailableStatus.TRUE;
    }

    private AvailableStatus requestFileInformationAPI(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        setWeakFilename(link);
        final String fileID = this.getFID(link);
        if (account != null) {
            this.login(account, false);
        }
        final Browser brc = this.createNewBrowserInstance();
        brc.setAllowedResponseCodes(400);
        brc.getPage(SankakucomplexComCrawler.API_BASE + "/posts?lang=en&page=1&limit=1&tags=id_range:" + fileID);
        if (brc.getHttpConnection().getResponseCode() == 400) {
            /* {"success":false,"code":"invalid id","error":"invalid id","errorId":"error_<someHash>"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Object obj = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.OBJECT);
        if (obj instanceof Map) {
            /* We only get a map if something went wrong. */
            final Map<String, Object> errormap = (Map<String, Object>) obj;
            final String errorcode = (String) errormap.get("code");
            if (StringUtils.equalsIgnoreCase(errorcode, "snackbar__content-belongs-to-premium-client")) {
                link.setProperty(PROPERTY_IS_PREMIUMONLY, true);
                return AvailableStatus.TRUE;
            } else if (brc.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                throw new PluginException(LinkStatus.ERROR_FATAL, "API error: " + errorcode);
            }
        }
        final List<Object> ressourcelist = (List<Object>) obj;
        if (ressourcelist.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> item = (Map<String, Object>) ressourcelist.get(0);
        parseFileInfoAndSetFilenameAPI(this, link, item);
        return AvailableStatus.TRUE;
    }

    public static void parseFileInfoAndSetFilenameAPI(final Plugin plugin, final DownloadLink link, final Map<String, Object> item) {
        final Map<String, Object> author = (Map<String, Object>) item.get("author");
        link.setProperty(PROPERTY_UPLOADER, author.get("name"));
        // final boolean isActive = StringUtils.equalsIgnoreCase(item.get("status").toString(), "active");
        final String mimeType = item.get("file_type").toString();
        final String ext = plugin.getExtensionFromMimeType(mimeType);
        final Number file_size = (Number) item.get("file_size");
        if (file_size != null) {
            /*
             * Do not set verifiedFileSize since we don't know if we will download the original file in the end or a lower quality version.
             */
            // link.setVerifiedFileSize(file_size.longValue());
            link.setDownloadSize(file_size.longValue());
        }
        if ((Boolean) item.get("is_premium")) {
            // throw new AccountRequiredException();
            link.setProperty(PROPERTY_IS_PREMIUMONLY, true);
        } else {
            link.removeProperty(PROPERTY_IS_PREMIUMONLY);
        }
        final List<Map<String, Object>> tags = (List<Map<String, Object>>) item.get("tags");
        if (tags != null && tags.size() > 0) {
            String tagsCommaSeparated = "";
            for (final Map<String, Object> tagInfo : tags) {
                String tag = (String) tagInfo.get("name_en");
                if (StringUtils.isEmpty(tag)) {
                    tag = (String) tagInfo.get("name_ja");
                }
                if (tagsCommaSeparated.length() > 0) {
                    tagsCommaSeparated += ",";
                }
                tagsCommaSeparated += tag;
            }
            link.setProperty(PROPERTY_TAGS_COMMA_SEPARATED, tagsCommaSeparated);
            if (PluginJsonConfig.get(SankakucomplexComConfig.class).isSetCommaSeparatedTagsOfPostsAsComment()) {
                link.setComment(tagsCommaSeparated);
            }
        }
        /* 2022-12-20: We can't trust this hash for all items. */
        final String md5hash = (String) item.get("md5");
        if (!StringUtils.isEmpty(md5hash)) {
            /*
             * Do not set md5 hash since we don't know if we will download the original file in the end or a lower quality version.
             */
            // link.setMD5Hash(md5hash);
        }
        storeDirecturl(link, item.get("file_url").toString());
        link.setProperty(PROPERTY_SOURCE, item.get("source"));
        final String bookTitle = link.getStringProperty(PROPERTY_BOOK_TITLE);
        final int pageNumber = link.getIntegerProperty(PROPERTY_PAGE_NUMBER, 0) + 1;
        final int pageNumberMax = link.getIntegerProperty(PROPERTY_PAGE_NUMBER_MAX, 0) + 1;
        if (pageNumberMax > 1 && bookTitle != null) {
            link.setFinalFileName(StringUtils.formatByPadLength(StringUtils.getPadLength(pageNumberMax), pageNumber) + "_" + item.get("id") + "." + ext);
        } else {
            link.setFinalFileName(item.get("id") + "." + ext);
        }
        final Map<String, Object> created_at_map = (Map<String, Object>) item.get("created_at");
        if (created_at_map != null) {
            final long created_at_timestamp = ((Number) created_at_map.get("s")).longValue();
            final SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
            final String dateFormatted = formatter.format(new Date(created_at_timestamp * 1000));
            link.setProperty(PROPERTY_DATE_PUBLISHED, dateFormatted);
        }
    }

    private static void storeDirecturl(final DownloadLink link, final String url) {
        if (url == null) {
            return;
        }
        try {
            final String filenameFromURL = Plugin.getFileNameFromURL(new URL(url));
            link.setProperty(PROPERTY_DIRECTURL_FILENAME, filenameFromURL);
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }
        link.setProperty(PROPERTY_DIRECTURL, url);
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownload(link, null);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception {
        if (account != null) {
            /* Ensure that we are always logged in, even if we are using a stored direct URL down below. */
            login(account, false);
        }
        String dllink = link.getStringProperty(PROPERTY_DIRECTURL);
        if (dllink == null) {
            requestFileInformationWebsite(link, account, true);
            dllink = link.getStringProperty(PROPERTY_DIRECTURL);
            if (dllink == null) {
                if (link.hasProperty(PROPERTY_IS_PREMIUMONLY) && account == null) {
                    throw new AccountRequiredException();
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
        prepareDownloadHeaders(br);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, isResumeable(link, account), 1);
        this.handleConnectionErrors(br, dl.getConnection());
        /* Add a download slot */
        controlMaxDownloads(account, link, +1);
        try {
            /* Start download */
            dl.startDownload();
        } finally {
            /* Remove download slot */
            controlMaxDownloads(account, link, -1);
        }
    }

    private static void prepareDownloadHeaders(final Browser br) {
        /**
         * 2024-11-12: Do not send a referer header! </br>
         * This is really important else we may get redirected to a dummy image. Looks to be some kind of pseudo protection.
         */
        br.getHeaders().put("Referer", "");
        // br.setCurrentURL(null);
    }

    @Override
    protected void handleConnectionErrors(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        if (!isValidFinalURL(dl.getConnection().getURL().toExternalForm())) {
            errorNoFileContent(this.getDownloadLink());
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (this.looksLikeDownloadableContent(con)) {
            /* No error */
            return;
        }
        br.followConnection(true);
        errorNoFileContent(this.getDownloadLink());
        /* This code should never be reached */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    private void errorNoFileContent(final DownloadLink link) throws PluginException {
        /* Force generation of new directurl next time */
        link.removeProperty(PROPERTY_DIRECTURL);
        final long timestampLastTimeFileMaybeBroken = link.getLongProperty(TIMESTAMP_LAST_TIME_FILE_MAYBE_BROKEN, 0);
        final String errortext = "Broken or temporarily unavailable file";
        if (System.currentTimeMillis() - timestampLastTimeFileMaybeBroken <= 5 * 60 * 1000l) {
            /**
             * Failed again in a short time even with fresh direct URL: </br>
             * Wait longer time before retry as we've just recently tried and it failed again.
             */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errortext, 5 * 60 * 1000l);
        } else {
            /* Retry soon */
            link.setProperty(TIMESTAMP_LAST_TIME_FILE_MAYBE_BROKEN, System.currentTimeMillis());
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errortext, 30 * 1000l);
        }
    }

    @Override
    protected boolean looksLikeDownloadableContent(final URLConnectionAdapter con) {
        final String etag = con.getHeaderField("etag");
        if (StringUtils.equalsIgnoreCase(etag, "\"657c7197-327a\"")) {
            /* 2024-07-25: etag of: https://chan.sankakucomplex.com/redirect.png */
            return false;
        } else if (!isValidFinalURL(con.getURL().toExternalForm())) {
            /* 2024-07-25: Dummy image: https://chan.sankakucomplex.com/redirect.png */
            return false;
        } else {
            return super.looksLikeDownloadableContent(con);
        }
    }

    private boolean isValidFinalURL(final String url) {
        if (StringUtils.containsIgnoreCase(url, "/redirect.png")) {
            /* 2024-07-25: Dummy image: https://chan.sankakucomplex.com/redirect.png */
            return false;
        } else if (StringUtils.containsIgnoreCase(url, "expired.png")) {
            return false;
        } else {
            return true;
        }
    }

    public boolean login(final Account account, final boolean force) throws Exception {
        return login(account, null, force);
    }

    public boolean login(final Account account, String logincheckurl, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (logincheckurl == null) {
                logincheckurl = "https://chan." + this.getHost() + "/en/users/home";
            }
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
                if (!force) {
                    /* Do not validate cookies */
                    return false;
                }
                br.getPage(logincheckurl);
                if (isLoggedin(br)) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return true;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    account.clearCookies("");
                }
            }
            logger.info("Performing full login");
            setDefaultCookies(br, this.getHost());
            /*
             * 2024-04-26: It is really important to have the right URL here in order to properly login in a way which also works for 'old'
             * "chan." subdomain.
             */
            br.getPage("https://login." + getHost() + "/oidc/auth?response_type=code&scope=openid&client_id=sankaku-channel-legacy&redirect_uri=https%3A%2F%2Fchan.sankakucomplex.com%2Fsso%2Fcallback&route=login");
            final String _grant = br.getCookie(br.getHost(), "_grant", Cookies.NOTDELETEDPATTERN);
            if (StringUtils.isEmpty(_grant)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final Map<String, Object> loginpost = new HashMap<String, Object>();
            final Map<String, Object> loginpost_mfaParams = new HashMap<String, Object>();
            loginpost_mfaParams.put("login", account.getUser());
            loginpost.put("login", account.getUser());
            loginpost.put("mfaParams", loginpost_mfaParams);
            loginpost.put("password", account.getPass());
            br.postPageRaw("/auth/token", JSonStorage.serializeToJson(loginpost));
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if (!Boolean.TRUE.equals(entries.get("success"))) {
                /* E.g. {"success":false,"code":"snackbar-message__not_found"} */
                throw new AccountInvalidException();
            }
            final String access_token = entries.get("access_token").toString();
            final String refresh_token = entries.get("refresh_token").toString();
            if (StringUtils.isEmpty(access_token)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            br.setCookie(br.getHost(), "accessToken", access_token);
            br.setCookie(br.getHost(), "position", "0");
            br.setCookie(br.getHost(), "refreshToken", refresh_token);
            // br.setCookie(br.getHost(), "ssoLoginValid", System.currentTimeMillis() + "");
            final UrlQuery query = new UrlQuery();
            query.add("access_token", Encoding.urlEncode(access_token));
            query.add("state", "lang=en&theme=white");
            br.postPage("/oidc/interaction/" + _grant + "/login", query);
            /* Double-check */
            br.getPage(logincheckurl);
            if (!isLoggedin(br)) {
                throw new AccountInvalidException("Unknown login failure");
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
            account.setProperty(PROPERTY_ACCOUNT_ACCESS_TOKEN, access_token);
            account.setProperty(PROPERTY_ACCOUNT_REFRESH_TOKEN, refresh_token);
            return true;
        }
    }

    public boolean isLoggedin(final Browser br) {
        if (br.containsHTML("/users/logout")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        final AccountInfo ai = new AccountInfo();
        ai.setUnlimitedTraffic();
        if (br.containsHTML(">\\s*Subscription Level\\s*:\\s*<a href=\"[^\"]+\">\\s*Plus\\s*<")) {
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
        }
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload(link, account);
    }

    protected void controlMaxDownloads(final Account account, final DownloadLink link, final int num) {
        if (account != null) {
            synchronized (premiumRunning) {
                final int before = premiumRunning.get();
                final int after = before + num;
                premiumRunning.set(after);
                logger.info("premiumRunning(" + link.getName() + ")|max:" + getMaxSimultanPremiumDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
            }
        } else {
            synchronized (freeRunning) {
                final int before = freeRunning.get();
                final int after = before + num;
                freeRunning.set(after);
                logger.info("freeRunning(" + link.getName() + ")|max:" + getMaxSimultanFreeDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
            }
        }
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return premiumRunning.get() + 1;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return freeRunning.get() + 1;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        if (link == null) {
            return;
        }
        /* Delete cached direct URL. */
        link.removeProperty(PROPERTY_DIRECTURL);
        link.removeProperty(TIMESTAMP_LAST_TIME_FILE_MAYBE_BROKEN);
    }

    @Override
    public Class<? extends SankakucomplexComConfig> getConfigInterface() {
        return SankakucomplexComConfig.class;
    }
}
