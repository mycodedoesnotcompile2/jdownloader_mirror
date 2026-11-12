//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
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
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Hash;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.download.DownloadLinkDownloadable;
import jd.plugins.download.Downloadable;
import jd.plugins.download.HashInfo;
import jd.plugins.download.raf.HTTPDownloader.ConnectionHashInfo;

/**
 * Hosterplugin for shared files of PikPak (mypikpak.com). Share links themselves are handled by the crawler plugin MyPikpakComCrawler which
 * also uses the API helper methods of this class.
 */
@HostPlugin(revision = "$Revision: 53969 $", interfaceVersion = 3, names = {}, urls = {})
public class MyPikpakCom extends PluginForHost {
    /* Properties set by the crawler */
    public static final String               PROPERTY_SHARE_ID      = "share_id";
    public static final String               PROPERTY_FILE_ID       = "file_id";
    /* API related constants. The API is always accessed via the main domain no matter via which domain the user accessed the share. */
    private static final String              API_DRIVE_BASE         = "https://api-drive.mypikpak.com";
    private static final String              API_USER_BASE          = "https://user.mypikpak.com";
    private static final String              API_CLIENT_ID          = "YUMx5nI8ZU8Ap8pm";
    private static final String              API_CAPTCHA_ACTION     = "POST:/config/v1/basic";
    /**
     * Page of the web client which is used to find the (frequently changing) script files containing the parameters for the captcha
     * signature. Any share ID will do.
     */
    private static final String              URL_WEBCLIENT_PAGE     = "https://mypikpak.com/s/jdownloader";
    private static final String              PATH_WEBCLIENT_SCRIPTS = "/s/pikcloud/";
    private static final String              URL_WEBCLIENT_SCRIPTS  = "https://mypikpak.com" + PATH_WEBCLIENT_SCRIPTS;
    /* Shared state across plugin instances (see CLAUDE.md "Shared Plugin State"). */
    private static AtomicReference<String>   DEVICE_ID              = new AtomicReference<String>();
    private static AtomicReference<String>   CAPTCHA_TOKEN          = new AtomicReference<String>();
    private static AtomicLong                CAPTCHA_TOKEN_EXPIRES  = new AtomicLong(-1);
    /* Parameters for "captcha_sign" which are read from the web client at runtime. */
    private static AtomicReference<String[]> SIGN_SALTS             = new AtomicReference<String[]>();
    private static AtomicReference<String>   SIGN_CLIENT_VERSION    = new AtomicReference<String>();
    private static AtomicReference<String>   SIGN_PACKAGE_NAME      = new AtomicReference<String>();
    private static AtomicLong                SIGN_CONFIG_LOADED     = new AtomicLong(-1);

    public MyPikpakCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/policy/user-agreement";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "mypikpak.com", "mypikpak.net", "pickpackapp.com" });
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

    /**
     * Internal URL format created by the crawler plugin: https://mypikpak.com/s/<shareID>?fileid=<fileID> </br>
     * Normal share URLs (https://mypikpak.com/s/<shareID>) are handled by the crawler.
     */
    public static final Pattern PATTERN_FILE = Pattern.compile("/s/([A-Za-z0-9_\\-]+)\\?fileid=([A-Za-z0-9_\\-]+)");

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_FILE.pattern());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return "mypikpak://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        final Regex regex = new Regex(link.getPluginPatternMatcher(), PATTERN_FILE);
        if (!regex.patternFind()) {
            return null;
        }
        return regex.getMatch(0) + "/" + regex.getMatch(1);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Map<String, Object> fileinfo = fetchFileInfo(this, br, link);
        final String filename = (String) fileinfo.get("name");
        if (!StringUtils.isEmpty(filename)) {
            link.setFinalFileName(filename);
        }
        final String filesize = (String) fileinfo.get("size");
        if (filesize != null && filesize.matches("\\d+")) {
            link.setVerifiedFileSize(Long.parseLong(filesize));
        }
        link.setAvailable(true);
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        final Map<String, Object> fileinfo = fetchFileInfo(this, br, link);
        String dllink = (String) fileinfo.get("web_content_link");
        if (StringUtils.isEmpty(dllink)) {
            /*
             * Videos have an empty "web_content_link". Their original file is listed in "medias" (is_origin=true). Transcoded versions are
             * ignored because they differ from the original file.
             */
            final List<Map<String, Object>> medias = (List<Map<String, Object>>) fileinfo.get("medias");
            if (medias != null) {
                for (final Map<String, Object> media : medias) {
                    if (Boolean.TRUE.equals(media.get("is_origin"))) {
                        final Map<String, Object> mediaLink = (Map<String, Object>) media.get("link");
                        if (mediaLink != null) {
                            dllink = (String) mediaLink.get("url");
                        }
                        break;
                    }
                }
            }
        }
        if (StringUtils.isEmpty(dllink)) {
            /* No direct downloadlink available e.g. because the file is not downloadable without account or the file is still processed. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find downloadurl");
        }
        /* Do not send any API headers to the file server. */
        dl = jd.plugins.BrowserAdapter.openDownload(createNewBrowserInstance(), link, dllink, this.isResumeable(link, null), this.getMaxChunks(null));
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public Downloadable newDownloadable(DownloadLink downloadLink, Browser br) {
        return new DownloadLinkDownloadable(downloadLink, br) {
            @Override
            public void setHashInfo(HashInfo hashInfo) {
                if (hashInfo instanceof ConnectionHashInfo) {
                    /* The server sends a "Content-MD5" header which contains wrong values -> Do not use hashes of the headers. */
                    return;
                }
                super.setHashInfo(hashInfo);
            }
        };
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    /* No @Override: Cannot be overridden at the moment. */
    public int getMaxChunks(final Account account) {
        return 0;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    /** Returns the "file_info" object of the given file. */
    private static Map<String, Object> fetchFileInfo(final Plugin plugin, final Browser br, final DownloadLink link) throws Exception {
        final Regex regex = new Regex(link.getPluginPatternMatcher(), PATTERN_FILE);
        if (!regex.matches()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String shareID = regex.getMatch(0);
        final String fileID = regex.getMatch(1);
        final String passCode = link.getDownloadPassword();
        /* The "pass_code_token" is only available as a result of a password check -> Always do it first. */
        final UrlQuery shareQuery = new UrlQuery();
        shareQuery.appendEncoded("limit", "1");
        shareQuery.appendEncoded("share_id", shareID);
        if (passCode != null) {
            shareQuery.appendEncoded("pass_code", passCode);
        }
        final Map<String, Object> shareInfo = apiGet(plugin, br, "/drive/v1/share", shareQuery);
        checkShareStatus(shareInfo, link);
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("share_id", shareID);
        query.appendEncoded("file_id", fileID);
        final String passCodeToken = (String) shareInfo.get("pass_code_token");
        if (!StringUtils.isEmpty(passCodeToken)) {
            query.appendEncoded("pass_code_token", passCodeToken);
        }
        final Map<String, Object> response = apiGet(plugin, br, "/drive/v1/share/file_info", query);
        checkShareStatus(response, link);
        return (Map<String, Object>) response.get("file_info");
    }

    private static void checkShareStatus(final Map<String, Object> response, final DownloadLink link) throws PluginException {
        final String status = (String) response.get("share_status");
        if (status == null || status.equals("OK")) {
            /* "share_status" is not present in error responses -> Those are handled in apiGet(). */
            return;
        }
        switch (status) {
        case "PASS_CODE_EMPTY":
        case "PASS_CODE_ERROR":
            link.setDownloadPassword(null);
            throw new PluginException(LinkStatus.ERROR_FATAL, "Wrong password");
        default:
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, (String) response.get("share_status_text"));
        }
    }

    /**
     * Performs a GET request against the PikPak drive API and returns the parsed JSON response. Handles the anonymous captcha token which
     * is required for all API requests.
     *
     * @param path
     *            e.g. "/drive/v1/share"
     */
    public static Map<String, Object> apiGet(final Plugin plugin, final Browser br, final String path, final UrlQuery query) throws Exception {
        final Browser brc = br.cloneBrowser();
        brc.setAllowedResponseCodes(400, 401, 403, 404, 429);
        boolean captchaRefreshed = false;
        while (true) {
            final String captchaToken = getCaptchaToken(plugin, brc, captchaRefreshed);
            final Request req = brc.createGetRequest(API_DRIVE_BASE + path + "?" + query.toString());
            req.getHeaders().put("x-captcha-token", captchaToken);
            req.getHeaders().put("x-client-id", API_CLIENT_ID);
            req.getHeaders().put("x-device-id", DEVICE_ID.get());
            req.getHeaders().put("x-user-id", "");
            req.getHeaders().put("x-global-file-token", "");
            req.getHeaders().put(HTTPConstants.HEADER_REQUEST_CONTENT_TYPE, "application/json");
            req.getHeaders().put(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://mypikpak.com");
            req.getHeaders().put(HTTPConstants.HEADER_REQUEST_REFERER, "https://mypikpak.com/");
            brc.getPage(req);
            final Map<String, Object> response = plugin.restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            final String error = (String) response.get("error");
            if (error == null) {
                return response;
            } else if ("captcha_invalid".equals(error) && !captchaRefreshed) {
                /* Captcha token expired or was rejected -> Get a new one and try again (once). */
                captchaRefreshed = true;
                continue;
            } else {
                /* E.g. invalid/deleted share. */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, (String) response.get("error_description"));
            }
        }
    }

    /**
     * Reads the parameters for the captcha signature (salts, client version, package name) from the script files of the web client. They
     * are refreshed once per hour or if forced. If a refresh fails, previously loaded values are kept.
     */
    private static void loadSignConfig(final Plugin plugin, final Browser br, final boolean force) throws Exception {
        final boolean hasConfig = SIGN_SALTS.get() != null;
        if (hasConfig && !force && Time.systemIndependentCurrentJVMTimeMillis() - SIGN_CONFIG_LOADED.get() < TimeUnit.HOURS.toMillis(1)) {
            return;
        }
        try {
            final Browser brc = br.cloneBrowser();
            brc.getPage(URL_WEBCLIENT_PAGE);
            final String[] scripts = brc.getRegex(PATH_WEBCLIENT_SCRIPTS + "([A-Za-z0-9_.\\-]+\\.js)").getColumn(0);
            /* Step 1: Find the script that lazy-loads the web client ("xBase") and thereby tells us the name of the script we need. */
            String configScript = null;
            final Set<String> checked = new HashSet<String>();
            for (final String script : scripts) {
                if (!checked.add(script)) {
                    continue;
                }
                brc.getPage(URL_WEBCLIENT_SCRIPTS + script);
                configScript = brc.getRegex("\\{xBase:\\w+\\}\\s*=\\s*await import\\(`\\./([A-Za-z0-9_.\\-]+\\.js)`\\)").getMatch(0);
                if (configScript != null) {
                    break;
                }
            }
            if (configScript == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find web client script");
            }
            /* Step 2: Parse the parameters. */
            brc.getPage(URL_WEBCLIENT_SCRIPTS + configScript);
            final String algorithms = brc.getRegex("algorithms:\\[(.*?)\\]").getMatch(0);
            final String clientVersion = brc.getRegex("clientVersion:`([^`]*)`").getMatch(0);
            final String packageName = brc.getRegex("packageName:`([^`]*)`").getMatch(0);
            if (algorithms == null || clientVersion == null || packageName == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to parse captcha sign parameters");
            }
            if (new Regex(algorithms, "alg:`(?!md5`)").matches()) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported captcha sign algorithm");
            }
            final String[] salts = new Regex(algorithms, "salt:`([^`]*)`").getColumn(0);
            if (salts.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find captcha sign salts");
            }
            SIGN_SALTS.set(salts);
            SIGN_CLIENT_VERSION.set(clientVersion);
            SIGN_PACKAGE_NAME.set(packageName);
            SIGN_CONFIG_LOADED.set(Time.systemIndependentCurrentJVMTimeMillis());
        } catch (final Exception e) {
            if (!hasConfig) {
                throw e;
            }
            plugin.getLogger().log(e);
            plugin.getLogger().info("Failed to refresh captcha sign parameters -> Keep using old ones");
        }
    }

    /** Returns the (anonymous) captcha token needed for API requests. A new one is requested if the current one is (almost) expired. */
    private static String getCaptchaToken(final Plugin plugin, final Browser br, final boolean forceRefresh) throws Exception {
        synchronized (CAPTCHA_TOKEN) {
            if (DEVICE_ID.get() == null) {
                DEVICE_ID.set(UUID.randomUUID().toString().replace("-", ""));
            }
            final String deviceID = DEVICE_ID.get();
            final String existingToken = CAPTCHA_TOKEN.get();
            if (!forceRefresh && existingToken != null && Time.systemIndependentCurrentJVMTimeMillis() < CAPTCHA_TOKEN_EXPIRES.get()) {
                return existingToken;
            }
            loadSignConfig(plugin, br, forceRefresh);
            final String clientVersion = SIGN_CLIENT_VERSION.get();
            final String packageName = SIGN_PACKAGE_NAME.get();
            final String timestamp = Long.toString(System.currentTimeMillis());
            /* captcha_sign = "1." + md5(...md5(md5(clientID + clientVersion + packageName + deviceID + timestamp + salt1) + salt2)...) */
            String sign = API_CLIENT_ID + clientVersion + packageName + deviceID + timestamp;
            for (final String salt : SIGN_SALTS.get()) {
                sign = Hash.getMD5(sign + salt);
            }
            final Map<String, Object> meta = new HashMap<String, Object>();
            meta.put("captcha_sign", "1." + sign);
            meta.put("client_version", clientVersion);
            meta.put("package_name", packageName);
            meta.put("user_id", "");
            meta.put("timestamp", timestamp);
            final Map<String, Object> postdata = new LinkedHashMap<String, Object>();
            postdata.put("client_id", API_CLIENT_ID);
            postdata.put("action", API_CAPTCHA_ACTION);
            postdata.put("device_id", deviceID);
            postdata.put("meta", meta);
            final Browser brc = br.cloneBrowser();
            brc.setAllowedResponseCodes(400, 401, 403, 404, 429);
            final Request req = brc.createJSonPostRequest(API_USER_BASE + "/v1/shield/captcha/init", postdata);
            req.getHeaders().put("x-client-id", API_CLIENT_ID);
            req.getHeaders().put("x-client-version", "1.0.0");
            req.getHeaders().put("x-device-id", deviceID);
            req.getHeaders().put("x-device-model", "chrome%2F157.0.0.0");
            req.getHeaders().put("x-device-name", "PC-Chrome");
            req.getHeaders().put("x-device-sign", "wdi10." + deviceID + "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
            req.getHeaders().put("x-net-work-type", "NONE");
            req.getHeaders().put("x-os-version", "Win32");
            req.getHeaders().put("x-platform-version", "1");
            req.getHeaders().put("x-protocol-version", "301");
            req.getHeaders().put("x-provider-name", "NONE");
            req.getHeaders().put("x-sdk-version", "8.1.4");
            req.getHeaders().put(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://mypikpak.com");
            req.getHeaders().put(HTTPConstants.HEADER_REQUEST_REFERER, "https://mypikpak.com/");
            brc.getPage(req);
            final Map<String, Object> response = plugin.restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            final String token = (String) response.get("captcha_token");
            if (token == null) {
                /* E.g. a visual captcha is required ("url" field) or the signature algorithm has changed. */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to obtain captcha token");
            }
            final long expiresInSeconds = ((Number) response.get("expires_in")).longValue();
            CAPTCHA_TOKEN.set(token);
            /* Renew 30 seconds before the token expires. */
            CAPTCHA_TOKEN_EXPIRES.set(Time.systemIndependentCurrentJVMTimeMillis() + TimeUnit.SECONDS.toMillis(expiresInSeconds - 30));
            return token;
        }
    }
}
