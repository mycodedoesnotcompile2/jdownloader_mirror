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
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.jdownloader.plugins.components.config.OneFichierConfigInterface;
import org.jdownloader.plugins.components.config.OneFichierConfigInterface.LinkcheckMode;
import org.jdownloader.plugins.components.config.OneFichierConfigInterface.SSLMode;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.download.HashInfo;
import jd.plugins.download.HashInfo.TYPE;

@HostPlugin(revision = "$Revision: 51244 $", interfaceVersion = 3, names = {}, urls = {})
public class OneFichierCom extends PluginForHost {
    /* Account properties */
    private final String       PROPERTY_ACCOUNT_USE_CDN_CREDITS             = "use_cdn_credits";
    private final String       PROPERTY_ACCOUNT_CDN_CREDITS_BYTES           = "cdn_credits_bytes";
    private final String       PROPERTY_ACCOUNT_IS_GOLD_ACCOUNT             = "is_gold_account";
    private final String       PROPERTY_ACCOUNT_TIMESTAMP_VPN_DETECTED      = "timestamp_vpn_detected";
    private final String       PROPERTY_ACCOUNT_HAS_SHOWN_VPN_LOGIN_WARNING = "has_shown_vpn_login_warning";
    /* DownloadLink properties */
    private final String       PROPERTY_HOTLINK                             = "hotlink";
    /** URLs can be restricted for various reason: https://1fichier.com/console/acl.pl */
    public static final String PROPERTY_ACL_ACCESS_CONTROL_LIMIT            = "acl_access_control_limit";
    /** 2019-04-04: Documentation: https://1fichier.com/api.html */
    public static final String API_BASE                                     = "https://api.1fichier.com/v1";
    private final boolean      allowFreeAccountDownloadsViaAPI              = false;

    @Override
    public String[] siteSupportedNames() {
        /* 1st domain = current domain! */
        final String[] supportedDomains = buildSupportedNames(getPluginDomains());
        final List<String> ret = new ArrayList<String>(Arrays.asList(supportedDomains));
        ret.add("1fichier");
        return ret.toArray(new String[0]);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "1fichier.com", "alterupload.com", "cjoint.net", "desfichiers.net", "dfichiers.com", "megadl.fr", "mesfichiers.org", "piecejointe.net", "pjointe.com", "tenvoi.com", "dl4free.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            /* URL format according to API page --> General: https://1fichier.com/api.html */
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/\\?([a-z0-9]{5,20})");
        }
        return ret.toArray(new String[0]);
    }

    public OneFichierCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/en/register.pl");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static Browser prepareBrowserWebsite(final Browser br) {
        br.setConnectTimeout(3 * 60 * 1000);
        br.setReadTimeout(3 * 60 * 1000);
        br.getHeaders().put("User-Agent", "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.103 Safari/537.36");
        br.getHeaders().put("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8");
        br.getHeaders().put("Accept-Language", "en-us,en;q=0.5");
        br.getHeaders().put("Pragma", null);
        br.getHeaders().put("Cache-Control", null);
        br.setCustomCharset("utf-8");
        br.setCookie("1fichier.com", "LG", "en");
        br.setAllowedResponseCodes(new int[] { 403, 503 });
        return br;
    }

    public static Browser prepareBrowserAPI(final Browser br, final Account account) throws Exception {
        br.setConnectTimeout(3 * 60 * 1000);
        br.setReadTimeout(3 * 60 * 1000);
        br.getHeaders().put("User-Agent", "JDownloader");
        br.getHeaders().put("Content-Type", "application/json");
        br.setAllowedResponseCodes(new int[] { 401, 403, 503 });
        setPremiumAPIHeaders(br, account);
        return br;
    }

    public String getDirectlinkproperty(final Account account) {
        if (account == null) {
            /* no account, yes we can expect captcha */
            return "freeLink";
        } else {
            return "account_ " + account.getType() + "_directurl";
        }
    }

    /** Required to authenticate via API. Wrapper for setPremiumAPIHeaders(String). */
    private static void setPremiumAPIHeaders(final Browser br, final Account account) throws PluginException {
        final String apiKey = account.getPass();
        if (apiKey == null || !looksLikeValidAPIKeySTATIC(apiKey)) {
            errorInvalidAPIKey(account);
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        setPremiumAPIHeaders(br, apiKey);
    }

    /* 2024-04-26: Removed this as user can switch between API-key and website login. E-Mail is not given in API-Key login */
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        if (PluginJsonConfig.get(OneFichierConfigInterface.class).isUsePremiumAPIEnabled()) {
            return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.API_KEY_LOGIN };
        } else {
            return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
        }
    }

    @Override
    public void init() {
        setRequestIntervalLimits();
    }

    public static void setRequestIntervalLimits() {
        /** 2021-02-10: We use 2500ms as default, 1 request per second is also fine according to admin. */
        final OneFichierConfigInterface cfg = PluginJsonConfig.get(OneFichierConfigInterface.class);
        Browser.setRequestIntervalLimitGlobal("1fichier.com", cfg.getGlobalRequestIntervalLimit1fichierComMilliseconds());
        Browser.setRequestIntervalLimitGlobal("api.1fichier.com", true, cfg.getGlobalRequestIntervalLimitAPI1fichierComMilliseconds());
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if ((account != null && AccountType.PREMIUM.equals(account.getType())) || link.hasProperty(PROPERTY_HOTLINK)) {
            /* Premium download */
            /*
             * Max total connections for premium = 30 (RE: admin, updated 07.03.2019) --> See also their FAQ:
             * https://1fichier.com/hlp.html#dllent
             */
            int maxChunks = PluginJsonConfig.get(OneFichierConfigInterface.class).getMaxPremiumChunks();
            if (maxChunks == 1) {
                maxChunks = 1;
            } else if (maxChunks < 1 || maxChunks >= 20) {
                maxChunks = 0;
            } else {
                maxChunks = -maxChunks;
            }
            return maxChunks;
        } else if (link.hasProperty(PROPERTY_HOTLINK)) {
            return -3;
        } else {
            return 1;
        }
    }

    private String getURLWithPreferredProtocol(String url) {
        final OneFichierConfigInterface cfg = PluginJsonConfig.get(OneFichierConfigInterface.class);
        final SSLMode sslmode = cfg.getSSLMode();
        if (sslmode == SSLMode.AUTO) {
            /* Do not modify URL */
            return url;
        } else if (sslmode == SSLMode.FORCE_HTTPS) {
            return url.replaceFirst("(?i)http://", "https://");
        } else {
            return url.replaceFirst("(?i)https://", "http://");
        }
    }

    private String getContentURL(final DownloadLink link) {
        /**
         * 2019-04-24: Do NOT change domains here! Uploaders can decide which domain is the only valid domain for their files e.g.
         * "alterupload.com". Using their main domain (1fichier.com) will result in OFFLINE URLs!
         */
        final String fid = getFID(link);
        if (fid != null) {
            /* Use new/current linktype and keep original domain of inside added URL! */
            final String domainFromURL = Browser.getHost(link.getPluginPatternMatcher());
            return "https://" + domainFromURL + "/?" + fid;
        } else {
            /* Fallback, should not be needed. */
            return link.getPluginPatternMatcher();
        }
    }

    /** Returns content-URL for website requests */
    private String getContentURLWebsite(final DownloadLink link) {
        String contentURL = this.getContentURL(link);
        if (contentURL.contains("?") && !contentURL.contains("&")) {
            /* Attempt to force English language as only setting the cookie may not be enough. */
            contentURL += "&lg=en";
        }
        return contentURL;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return this.getHost() + "://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    /** Returns the unique file/link-ID of given downloadLink. */
    private String getFID(final DownloadLink link) {
        if (link.getPluginPatternMatcher() == null) {
            return null;
        } else {
            return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
        }
    }

    private boolean preferSingleLinkcheckViaAPI() {
        final OneFichierConfigInterface cfg = PluginJsonConfig.get(OneFichierConfigInterface.class);
        final LinkcheckMode mode = cfg.getLinkcheckMode();
        if (mode == LinkcheckMode.AUTO) {
            return true;
        } else if (mode == LinkcheckMode.PREFER_SINGLE_LINKCHECK) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Returns true if we allow upper handling to check this link via API. <br>
     * This is mostly used for password protected links if we have the [correct] download password because the API is returning filename and
     * filesize while website and old mass-linkcheck does not provide this information. <br>
     * The official linkcheck API is very rate limited though so we should really only use it if needed.
     */
    private boolean allowSingleFileAPILinkcheck(final DownloadLink link, final Account account) {
        if (isaccessControlLimited(link) && !link.isSizeSet() && link.getDownloadPassword() != null && canUseAPI(account)) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        final Account account;
        if (preferSingleLinkcheckViaAPI() && urls.length == 1 && (account = AccountController.getInstance().getValidAccount(this.getHost())) != null && allowSingleFileAPILinkcheck(urls[0], account)) {
            /* Returning false triggers fallback to single linkcheck */
            logger.info("Special handling for single password protected links: Returning false here to trigger single linkcheck via API");
            return false;
        }
        try {
            prepareBrowserWebsite(br);
            final StringBuilder sb = new StringBuilder();
            final List<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    /* We check 100 links at once */
                    if (index == urls.length || links.size() == 100) {
                        break;
                    } else {
                        links.add(urls[index]);
                        index++;
                    }
                }
                sb.delete(0, sb.capacity());
                for (final DownloadLink link : links) {
                    sb.append("links[]=");
                    sb.append(Encoding.urlEncode(this.getContentURL(link)));
                    sb.append("&");
                }
                // remove last "&"
                sb.deleteCharAt(sb.length() - 1);
                /**
                 * This method is server side deprecated but we're still using it because: </br>
                 * 1. It is still working. </br>
                 * 2. It is the only method that can be used to check multiple items with one request.
                 */
                br.postPageRaw("https://" + this.getHost() + "/check_links.pl", sb.toString());
                for (final DownloadLink link : links) {
                    final String file_id = this.getFID(link);
                    if (!link.isNameSet()) {
                        /* Set weak filename */
                        link.setName(file_id);
                    }
                    final String file_id_string = new Regex(br.getRequest().getHtmlCode(), Pattern.compile("(" + file_id + ".+)", Pattern.MULTILINE)).getMatch(0);
                    if (file_id_string == null) {
                        /* This should not happen but let's treat such links as offline */
                        logger.warning("Failed to find fileID information for fileID: " + file_id);
                        link.setAvailable(false);
                        continue;
                    }
                    final String[] file_id_properties = file_id_string.split(";");
                    if (file_id_properties.length == 3) {
                        /* Success: FileID;filename;filesize_bytes */
                        link.removeProperty(PROPERTY_ACL_ACCESS_CONTROL_LIMIT);
                        link.setAvailable(true);
                        /* Trust API information. */
                        link.setFinalFileName(Encoding.htmlDecode(file_id_properties[1]));
                        link.setVerifiedFileSize(Long.parseLong(file_id_properties[2]));
                    } else if (file_id_properties.length == 4) {
                        /* Evaluate error status. */
                        final String error_status = file_id_properties[3];
                        if (error_status.equalsIgnoreCase("NOT FOUND")) {
                            /* Item has been deleted */
                            link.setAvailable(false);
                        } else if (error_status.equalsIgnoreCase("BAD LINK")) {
                            /* Items' file_id is invalid (never existed) */
                            link.setAvailable(false);
                        } else if (error_status.equalsIgnoreCase("PRIVATE")) {
                            /**
                             * Item is online but access is limited thus API does not provide filename/size. <br>
                             * Example when this can happen: <br>
                             * - File is private <br>
                             * - File is password protected <br>
                             * - File can only be downloaded by premium users <br>
                             * - File can only be downloaded by IPs of a specific country <br>
                             * - File can only be downloaded by a specific 1fichier.com user <br>
                             * - Multiple of the above "access control restrictions" may apply at the same time
                             */
                            link.setAvailable(true);
                            link.setProperty(PROPERTY_ACL_ACCESS_CONTROL_LIMIT, true);
                        } else {
                            /* This should never happen */
                            logger.warning("Found unknown error_status: " + error_status);
                        }
                    } else {
                        /* This should never happen */
                        logger.warning("Linkchecker is broken!");
                        return false;
                    }
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

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        this.setBrowserExclusive();
        final Account account;
        if (!link.isNameSet()) {
            link.setName(this.getFID(link));
        }
        if (preferSingleLinkcheckViaAPI() && (account = AccountController.getInstance().getValidAccount(this.getHost())) != null && allowSingleFileAPILinkcheck(link, account)) {
            /* API */
            /*
             * Advantage when doing this: We can get the file information even for password protected files (if we got the correct
             * password).
             */
            return requestFileInformationAPI(br.cloneBrowser(), link, account);
        } else {
            /* Website */
            checkLinks(new DownloadLink[] { link });
            if (!link.isAvailabilityStatusChecked()) {
                return AvailableStatus.UNCHECKED;
            } else if (!link.isAvailable()) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                return AvailableStatus.TRUE;
            }
        }
    }

    /**
     * 2022-12-10: Do not use this frequently as it will cause IP-blocks!! <br>
     * Checks single URLs via API.
     *
     * @throws Exception
     */
    public AvailableStatus requestFileInformationAPI(final Browser br, final DownloadLink link, final Account account) throws Exception {
        prepareBrowserAPI(br, account);
        final Map<String, Object> postData = new HashMap<String, Object>();
        postData.put("url", this.getContentURL(link));
        postData.put("pass", link.getDownloadPassword());
        performAPIRequest(br, API_BASE + "/file/info.cgi", JSonStorage.serializeToJson(postData));
        final Map<String, Object> entries = this.parseAPIResponse(br, account);
        final String errorMsg = (String) entries.get("message");
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* E.g. message": "Resource not found #469" */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (errorMsg != null && errorMsg.matches("(?i).*Resource not allowed.*")) {
            /* 2020-01-30: e.g. {"status":"KO","message":"Resource not allowed #631"} */
            /**
             * Password-protected or private file: No information given at all but we know that file is online. Example reasons: file is not
             * allowed to be downloaded in current country, by current user, file is private, file is password protected. <br>
             * If we already know that file file is password protected, we now also know that it is not access control limited :)
             */
            if (!link.isPasswordProtected()) {
                link.setProperty(PROPERTY_ACL_ACCESS_CONTROL_LIMIT, true);
            }
            /* File is online but we might not be able to download it. */
            return AvailableStatus.TRUE;
        }
        this.handleErrorsAPI(entries, account);
        link.removeProperty(PROPERTY_ACL_ACCESS_CONTROL_LIMIT);
        if (((Number) entries.get("pass")).shortValue() == 1) {
            link.setPasswordProtected(true);
        } else {
            link.setPasswordProtected(false);
        }
        link.setFinalFileName((String) entries.get("filename"));
        link.setVerifiedFileSize(((Number) entries.get("size")).longValue());
        final String description = (String) entries.get("description");
        if (!StringUtils.isEmpty(description) && StringUtils.isEmpty(link.getComment())) {
            link.setComment(description);
        }
        final String checksum = (String) entries.get("checksum");
        if (checksum != null) {
            link.setHashInfo(HashInfo.parse(checksum, TYPE.WHIRLPOOL));
        }
        link.setAvailable(true);
        return AvailableStatus.TRUE;
    }

    @Override
    protected int getMaxSimultanDownload(final DownloadLink link, final Account account) {
        if (account == null && (link != null && link.hasProperty(PROPERTY_HOTLINK))) {
            return Integer.MAX_VALUE;
        } else {
            return super.getMaxSimultanDownload(link, account);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        if (checkShowFreeDialog(getHost())) {
            showFreeDialog(getHost());
        }
        handleDownloadWebsite(link, null);
    }

    private boolean isPasswordProtectedFileWebsite(final Browser br) throws Exception {
        final Form ret = br.getFormbyKey("pass");
        if (ret != null && this.canHandle(ret.getAction())) {
            return true;
        } else {
            return false;
        }
    }

    public void handleDownloadWebsite(final DownloadLink link, final Account account) throws Exception, PluginException {
        if (account != null) {
            this.loginWebsite(account, false);
        }
        /* retry/resume of cached free link! */
        final String directurlproperty = getDirectlinkproperty(account);
        String storedDirecturl = link.getStringProperty(directurlproperty);
        if (storedDirecturl != null) {
            storedDirecturl = this.getURLWithPreferredProtocol(storedDirecturl);
            logger.info("Attempting to download stored directurl: " + storedDirecturl);
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, storedDirecturl, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                /* resume download */
                link.setProperty(directurlproperty, storedDirecturl);
                dl.startDownload();
                return;
            }
            logger.info("Stored directurl is invalid");
            br.followConnection(true);
            link.removeProperty(directurlproperty);
        }
        prepareBrowserWebsite(br);
        final String contentURL = this.getURLWithPreferredProtocol(getContentURLWebsite(link));
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, contentURL, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (this.looksLikeDownloadableContent(dl.getConnection())) {
            link.setProperty(directurlproperty, dl.getConnection().getURL().toExternalForm());
            if (account == null || !AccountType.PREMIUM.equals(account.getType())) {
                /* Link is direct-downloadable without premium account -> Hotlink which means another user is paying for the traffic. */
                logger.info("Link is hotlink");
                link.setProperty(PROPERTY_HOTLINK, true);
            }
            dl.startDownload();
            return;
        }
        logger.info("Link is not a hotlink or premium user has direct downloads disabled via Website -> /console/params.pl -> Force download Menu -> Enabled [enabled = disables direct downloads]");
        link.removeProperty(PROPERTY_HOTLINK);
        br.followConnection();
        /* 404 error here means the file is offline */
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        errorHandlingWebsite(link, account, br);
        final Form form = br.getForm(0);
        String passCode = link.getDownloadPassword();
        if (isPasswordProtectedFileWebsite(br)) {
            logger.info("Handling password protected link...");
            /*
             * Set pw protected flag so in case this downloadlink is ever tried to be downloaded via API, we already know that it is
             * password protected!
             */
            link.setPasswordProtected(true);
            /** Try passwords in this order: 1. DownloadLink stored password, 2. Last used password, 3. Ask user */
            if (passCode == null) {
                passCode = getUserInput("Password?", link);
            }
            form.put("pass", Encoding.urlEncode(passCode));
        }
        /* Remove form fields we don't want. */
        form.remove("save");
        form.put("did", "1");
        if (PluginJsonConfig.get(OneFichierConfigInterface.class).getSSLMode() == SSLMode.FORCE_HTTP) {
            form.put("dl_no_ssl", "on");
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, form, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (this.looksLikeDownloadableContent(dl.getConnection())) {
            if (link.isPasswordProtected()) {
                logger.info("User entered valid download password: " + passCode);
                /* Save download-password */
                link.setDownloadPassword(passCode);
            }
            dl.startDownload();
            return;
        }
        br.followConnection();
        if (isPasswordProtectedFileWebsite(br)) {
            this.errorWrongPassword(link);
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        errorHandlingWebsite(link, account, br);
        if (link.isPasswordProtected()) {
            logger.info("User entered valid download password: " + passCode);
            /* Save download-password */
            link.setDownloadPassword(passCode);
        }
        String dllink = br.getRegex("align:middle\">\\s+<a href=(\"|')(https?://[a-zA-Z0-9_\\-]+\\.(1fichier|desfichiers)\\.com/[a-zA-Z0-9]+.*?)\\1").getMatch(1);
        if (dllink == null) {
            dllink = br.getRegex("<a href=\"([^\"]*?)\"[^<]*>\\s*Click here to download").getMatch(0);
        }
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dllink = this.getURLWithPreferredProtocol(dllink);
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            logger.warning("The final dllink seems not to be a file!");
            br.followConnection(true);
            errorHandlingWebsite(link, account, br);
            this.handleErrorsLastResortWebsite(link, account);
        }
        link.setProperty(directurlproperty, dllink);
        dl.startDownload();
    }

    private void errorHandlingWebsite(final DownloadLink link, final Account account, final Browser br) throws Exception {
        final int responsecode = br.getHttpConnection().getResponseCode();
        if (br.containsHTML(">\\s*File not found")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Software error:<")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 'Software error'", 10 * 60 * 1000l);
        } else if (br.containsHTML(">\\s*Connexion à la base de données impossible<|>Can\\'t connect DB")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Internal database error", 5 * 60 * 1000l);
        } else if (br.containsHTML("not possible to free unregistered users")) {
            throw new AccountRequiredException();
        } else if (br.containsHTML("Your account will be unlock")) {
            if (account != null) {
                throw new AccountUnavailableException("Locked for security reasons", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "IP blocked for security reasons", 60 * 60 * 1000l);
            }
        } else if (br.containsHTML(">\\s*Access to this file is protected|>\\s*This file is protected")) {
            /* Access restricted by IP / only registered users / only premium users / only owner */
            if (br.containsHTML(">\\s*The owner of this file has reserved access to the subscribers of our services")) {
                throw new AccountRequiredException();
            } else {
                errorAccessControlLimit(link);
                /* This code should never be reached */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } else if (br.containsHTML(">\\s*Your requests are too fast")) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Rate limit reached", 30 * 1000l);
        } else if (br.getURL().contains("/?c=DB")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Internal database error", 5 * 60 * 1000l);
        } else if (responsecode == 403) {
            if (br.containsHTML(">\\s*Premium status must not be used on professional services")) {
                errorVPNUsed(account);
                /* This code should never be reached */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else if (br.containsHTML(">\\s*Premium status is only allowed to be used on residential private and dedicated")) {
                errorVPNUsed(account);
                /* This code should never be reached */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 15 * 60 * 1000l);
            }
        } else if (responsecode == 404) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 30 * 60 * 1000l);
        } else if (br.getHttpConnection().getResponseCode() == 503 && br.containsHTML(">\\s*Our services are in maintenance\\.\\s*Please come back after")) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Hoster is in maintenance mode!", 20 * 60 * 1000l);
        } else if (br.containsHTML(">\\s*You can use your account only for downloading from") || br.containsHTML(">\\s*Our services are not compatible with massively shared internet access") || br.containsHTML(">\\s*Be carrefull? to not use simultaneously your IPv4 and IPv6 IP")) {
            throw new AccountUnavailableException("You are trying to use this account on multiple IP addresses at once", 10 * 60 * 1000l);
        } else if (br.containsHTML("\">Warning \\! Without premium status, you can download only")) {
            logger.info("Seems like this is no premium account or it's vot valid anymore -> Disabling it");
            throw new AccountInvalidException("Account is no premium anymore");
        } else if (account != null && br.containsHTML(">\\s*Usage of professional services is restricted and requires usage of CDN credits") && !this.isUsingCDNCredits(account)) {
            errorVPNUsed(account);
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Check for blocked IP */
        String waittimeMinutesStr = br.getRegex("you must wait (at least|up to)\\s*(\\d+)\\s*minutes between each downloads").getMatch(1);
        if (waittimeMinutesStr == null) {
            waittimeMinutesStr = br.getRegex(">\\s*You must wait\\s*(\\d+)\\s*minutes").getMatch(0);
            if (waittimeMinutesStr == null) {
                waittimeMinutesStr = br.getRegex(">\\s*Vous devez attendre encore\\s*(\\d+)\\s*minutes").getMatch(0);
            }
        }
        if (br.containsHTML(">\\s*IP Locked|>\\s*Will be unlocked within 1h\\.")) {
            waittimeMinutesStr = "60";
        }
        final int defaultWaitMinutes = 5;
        boolean isBlocked = waittimeMinutesStr != null;
        isBlocked |= br.containsHTML("/>\\s*Téléchargements en cours");
        isBlocked |= br.containsHTML("En téléchargement standard, vous ne pouvez télécharger qu\\'un seul fichier");
        isBlocked |= br.containsHTML(">\\s*veuillez patienter avant de télécharger un autre fichier");
        isBlocked |= br.containsHTML(">\\s*You already downloading (some|a) file");
        isBlocked |= br.containsHTML(">\\s*You can download only one file at a time");
        isBlocked |= br.containsHTML(">\\s*Please wait a few seconds before downloading new ones");
        isBlocked |= br.containsHTML(">\\s*You must wait for another download");
        isBlocked |= br.containsHTML("Without premium status, you can download only one file at a time");
        isBlocked |= br.containsHTML("Without Premium, you can only download one file at a time");
        isBlocked |= br.containsHTML("Without Premium, you must wait between downloads");
        isBlocked |= br.containsHTML("Warning ! Without subscription, you can only download one file at|<span style=\"color:red\">Warning\\s*!\\s*</span>\\s*<br/>Without subscription, you can only download one file at a time\\.\\.\\.");
        isBlocked |= br.containsHTML(">\\s*Votre adresse IP ouvre trop de connexions vers le serveur");
        if (isBlocked) {
            if (account != null) {
                final long waitMilliseconds;
                if (waittimeMinutesStr != null) {
                    waitMilliseconds = Integer.parseInt(waittimeMinutesStr) * 60 * 1001l;
                } else {
                    waitMilliseconds = defaultWaitMinutes * 60 * 1000l;
                }
                throw new AccountUnavailableException("Wait between downloads", waitMilliseconds);
            } else {
                final boolean preferReconnect = PluginJsonConfig.get(OneFichierConfigInterface.class).isPreferReconnectEnabled();
                if (waittimeMinutesStr != null && preferReconnect) {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, Integer.parseInt(waittimeMinutesStr) * 60 * 1001l);
                } else if (waittimeMinutesStr != null && Integer.parseInt(waittimeMinutesStr) >= 10) {
                    /* High waittime --> Reconnect */
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, Integer.parseInt(waittimeMinutesStr) * 60 * 1001l);
                } else if (preferReconnect) {
                    /* User prefers reconnect --> Throw Exception with LinkStatus to trigger reconnect */
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, defaultWaitMinutes * 60 * 1000l);
                } else if (waittimeMinutesStr != null) {
                    throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Wait between download, Reconnect is disabled in plugin settings", Integer.parseInt(waittimeMinutesStr) * 60 * 1001l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Wait between download, Reconnect is disabled in plugin settings", defaultWaitMinutes * 60 * 1001);
                }
            }
        }
    }

    /**
     * Access restricted by IP / only registered users / only premium users / only owner. </br>
     * See here for all possible reasons (login required): https://1fichier.com/console/acl.pl
     *
     * @throws PluginException
     */
    private static void errorAccessControlLimit(final DownloadLink link) throws PluginException {
        if (link != null) {
            link.setProperty(PROPERTY_ACL_ACCESS_CONTROL_LIMIT, true);
        }
        throw new PluginException(LinkStatus.ERROR_FATAL, "Access to this file has been restricted");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        /* Clear this property on every account check so it will get refreshed. */
        account.removeProperty(PROPERTY_ACCOUNT_TIMESTAMP_VPN_DETECTED);
        if (canUseAPI(account)) {
            return fetchAccountInfoAPI(account);
        } else {
            return fetchAccountInfoWebsite(account);
        }
    }

    public AccountInfo fetchAccountInfoWebsite(final Account account) throws Exception {
        loginWebsite(account, true);
        final AccountInfo ai = new AccountInfo();
        if (!StringUtils.endsWithCaseInsensitive(br.getURL(), "/console/abo.pl")) {
            br.getPage("/console/abo.pl");
        }
        final Regex accountInfoRegex = br.getRegex("Your (Premium|Premium GOLD) subscription is valid until\\s*<[^>]*>(\\d{4}-\\d{2}-\\d{2})");
        final String premiumAccountTypeStr = accountInfoRegex.getMatch(0);
        String validUntil = accountInfoRegex.getMatch(1);
        if (validUntil == null) {
            /* Wider attempt */
            validUntil = br.getRegex("(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        }
        if (validUntil != null) {
            final long validUntilTimestamp = TimeFormatter.getMilliSeconds(validUntil, "yyyy'-'MM'-'dd", Locale.FRANCE);
            setValidUntil(ai, validUntilTimestamp);
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(getMaxSimultanPremiumDownloadNum());
            account.setConcurrentUsePossible(true);
        } else {
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(getMaxSimultanFreeDownloadNum());
            account.setConcurrentUsePossible(false);
        }
        if (StringUtils.containsIgnoreCase(premiumAccountTypeStr, "Gold")) {
            account.setProperty(PROPERTY_ACCOUNT_IS_GOLD_ACCOUNT, true);
        } else {
            account.removeProperty(PROPERTY_ACCOUNT_IS_GOLD_ACCOUNT);
        }
        ai.setUnlimitedTraffic();
        /* Credits are only relevant if usage of credits for downloads is enabled: https://1fichier.com/console/params.pl */
        br.getPage("/console/params.pl");
        final String cdnCreditCheckedStatus = br.getRegex("<input\\s*type=\"checkbox\"\\s*checked=\"([^\"]+)\"[^>]*name=\"own_credit\"").getMatch(0);
        if (StringUtils.equalsIgnoreCase("checked", cdnCreditCheckedStatus)) {
            logger.info("User has enabled usage of CDN credits");
            account.setProperty(PROPERTY_ACCOUNT_USE_CDN_CREDITS, true);
        } else {
            logger.info("User has disabled usage of CDN credits");
            account.removeProperty(PROPERTY_ACCOUNT_USE_CDN_CREDITS);
        }
        /*
         * Page "/console/cdn.pl" also contains the current value of CDN credits but doesn't contain the information about the CDN credits
         * usage boolean setting.
         */
        // br.getPage("/console/cdn.pl");
        String creditsGBStr = br.getRegex("Your account have ([0-9.]+) GB of").getMatch(0);
        if (creditsGBStr == null) {
            /* French version */
            creditsGBStr = br.getRegex("compte a ([0-9.]+) Go de crédits").getMatch(0);
        }
        long creditsAsBytes = -1;
        if (creditsGBStr != null) {
            creditsAsBytes = (long) Double.parseDouble(creditsGBStr) * 1024 * 1024 * 1024;
        } else {
            logger.warning("Failed to find CDN credits value");
        }
        this.setCdnCreditsStatus(account, ai, creditsAsBytes);
        return ai;
    }

    /** Sets end of the day of given timestamp as validUntil date. */
    private void setValidUntil(final AccountInfo ai, final long originalValidUntilTimestamp) {
        final Calendar calendar = Calendar.getInstance();
        calendar.setTimeInMillis(originalValidUntilTimestamp);
        calendar.set(Calendar.HOUR_OF_DAY, 23);
        calendar.set(Calendar.MINUTE, 59);
        calendar.set(Calendar.SECOND, 59);
        ai.setValidUntil(calendar.getTimeInMillis());
    }

    /**
     * 2019-04-04: This API can only be used by premium users! It might still work when a premium account expires and the key stays valid
     * but we don't know this yet!
     */
    public AccountInfo fetchAccountInfoAPI(final Account account) throws Exception {
        prepareBrowserAPI(br, account);
        /*
         * This request can only be used every ~5 minutes - using it more frequently will e.g. cause response:
         * {"status":"KO","message":"Flood detected: IP Locked #38"} [DOWNLOADS VIA API WILL STILL WORK!!]
         */
        performAPIRequest(API_BASE + "/user/info.cgi", "");
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> entries = this.parseAPIResponse(account);
        final String apierror = (String) entries.get("message");
        final boolean apiTempBlocked = apierror != null && apierror.matches("(?i)Flood detected: (User|User APK|IP) Locked.*?");
        if (apiTempBlocked) {
            final long cachedCdnCreditsBytes = account.getLongProperty(PROPERTY_ACCOUNT_CDN_CREDITS_BYTES, -1);
            AccountType type = null;
            if (account.lastUpdateTime() > 0) {
                logger.info("Cannot get account details because of API limits but account has been checked before and is ok");
                final AccountType oldAccountType = account.getType();
                if (AccountType.FREE.equals(oldAccountType) && !allowFreeAccountDownloadsViaAPI) {
                    errorPremiumNeededForAPIDownloading(account);
                    /* This code should never be reached */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                if (oldAccountType != null && oldAccountType != AccountType.UNKNOWN) {
                    final AccountInfo existing = account.getAccountInfo();
                    if (existing != null) {
                        /* return previously set AccountInfo */
                        final long lastValidUntil = existing.getLastValidUntil();
                        if (lastValidUntil > 0) {
                            /* keep previously set valid until date */
                            existing.setValidUntil(lastValidUntil);
                        }
                        setCdnCreditsStatus(account, existing, cachedCdnCreditsBytes);
                        return existing;
                    }
                    type = oldAccountType;
                }
            } else {
                /*
                 * Account got added for the first time but API is blocked at the moment. We know the account must be premium because only
                 * premium users can generate APIKeys but we cannot get any information at the moment ...
                 */
                logger.info("Cannot get account details because of API limits and account has never been checked before --> Adding account without info");
            }
            if (type == null) {
                /* If in doubt, treat account as premium. */
                type = AccountType.PREMIUM;
            }
            account.setType(type);
            ai.setStatus(type.getLabel() + " | Try account-check again later, downloads are not affected by this message!");
            account.setMaxSimultanDownloads(getMaxSimultanPremiumDownloadNum());
            account.setConcurrentUsePossible(true);
            if (type == AccountType.PREMIUM) {
                ai.setUnlimitedTraffic();
            } else {
                /*
                 * Free accounts cannot be used for downloading in API mode [unless they got CDN credits and they are in use, see down
                 * below].
                 */
                ai.setTrafficLeft(0);
            }
            setCdnCreditsStatus(account, ai, cachedCdnCreditsBytes);
            return ai;
        }
        this.handleErrorsAPI(entries, account);
        final String email = (String) entries.get("email");
        if (!StringUtils.isEmpty(email) && !StringUtils.equals(account.getUser(), email)) {
            account.setUser(email);
        }
        final Number cold_storage_used = (Number) entries.get("cold_storage");
        // final Number available_cold_storage = (Number)entries.get("available_cold_storage");
        final Number hot_storage_used = (Number) entries.get("hot_storage");
        final String subscription_end = (String) entries.get("subscription_end");
        final int accountType = Integer.parseInt(entries.get("offer").toString()); // 0=Free, 1=Premium, 2=Access
        final Object available_credits_in_gigabyteO = entries.get("cdn");
        long creditsAsBytes = -1;
        if (available_credits_in_gigabyteO != null) {
            if (available_credits_in_gigabyteO instanceof Number) {
                creditsAsBytes = (long) ((Number) available_credits_in_gigabyteO).doubleValue() * 1024 * 1024 * 1024;
            } else {
                creditsAsBytes = (long) Double.parseDouble(available_credits_in_gigabyteO.toString()) * 1024 * 1024 * 1024;
            }
        }
        final boolean useCDNCredits = JavaScriptEngineFactory.toLong(entries.get("use_cdn"), 0) == 1 ? true : false;
        if (useCDNCredits) {
            account.setProperty(PROPERTY_ACCOUNT_USE_CDN_CREDITS, true);
        } else {
            account.removeProperty(PROPERTY_ACCOUNT_USE_CDN_CREDITS);
        }
        if (accountType == 0) {
            /* Free --> 2019-07-18: API Keys are only available for premium users so this should never happen! */
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(getMaxSimultanFreeDownloadNum());
            if (!allowFreeAccountDownloadsViaAPI) {
                errorPremiumNeededForAPIDownloading(account);
                /* This code should never be reached */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } else {
            /* Premium or "Access" account */
            long validuntil = 0;
            if (!StringUtils.isEmpty(subscription_end)) {
                validuntil = TimeFormatter.getMilliSeconds(subscription_end, "yyyy-MM-dd HH:mm:ss", Locale.FRANCE);
            }
            /* Premium */
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(getMaxSimultanPremiumDownloadNum());
            setValidUntil(ai, validuntil);
            ai.setUnlimitedTraffic();
        }
        /* Set/remove gold status */
        if (accountType == 3) {
            account.setProperty(PROPERTY_ACCOUNT_IS_GOLD_ACCOUNT, true);
        } else {
            account.removeProperty(PROPERTY_ACCOUNT_IS_GOLD_ACCOUNT);
        }
        account.setConcurrentUsePossible(true);
        if (cold_storage_used != null || hot_storage_used != null) {
            long space_used_bytes = 0;
            if (cold_storage_used != null) {
                space_used_bytes += cold_storage_used.longValue();
            }
            if (hot_storage_used != null) {
                space_used_bytes += hot_storage_used.longValue();
            }
            ai.setUsedSpace(space_used_bytes);
        }
        setCdnCreditsStatus(account, ai, creditsAsBytes);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && this.isGoldAccount(account) && account.hasProperty(PROPERTY_ACCOUNT_TIMESTAMP_VPN_DETECTED)) {
            /* TODO: Check if this handling is correct, then add a nicer error message dialog + translations. */
            throw new AccountUnavailableException("Premium GOLD account + VPN can only be used in website mode", 5 * 60 * 1000l);
        }
        return ai;
    }

    private boolean isUsingCDNCredits(final Account account) {
        return account.hasProperty(PROPERTY_ACCOUNT_USE_CDN_CREDITS);
    }

    /**
     * Returns true if account is a gold account. <br>
     * Gold accounts are allowed to download via VPN or datacenter IPs without the need of CDN credits. <br>
     * See: https://1fichier.com/tarifs.html
     */
    private boolean isGoldAccount(final Account account) {
        if (AccountType.PREMIUM == account.getType() && account.hasProperty(PROPERTY_ACCOUNT_IS_GOLD_ACCOUNT)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Sets CDN credit status and account status text
     *
     * @throws PluginException
     * @throws IOException
     */
    private void setCdnCreditsStatus(final Account account, final AccountInfo ai, final long creditsInBytes) throws PluginException, IOException {
        account.setProperty(PROPERTY_ACCOUNT_CDN_CREDITS_BYTES, creditsInBytes);
        Boolean cdnCreditsUsageEnforced = null;
        /**
         * Check if CDN credits are required <br>
         * For gold accounts this is not needed since they do not have any VPN limitations.
         */
        logger.info("Checking for forced CDN credits usage");
        br.getPage("https://" + getHost() + "/network.html");
        if (br.containsHTML(">\\s*VPN detected|>\\s*Requires the use of CDN credits or")) {
            logger.info("CDN credits usage is forced");
            account.setProperty(PROPERTY_ACCOUNT_TIMESTAMP_VPN_DETECTED, System.currentTimeMillis());
            cdnCreditsUsageEnforced = true;
        } else {
            logger.info("CDN credits usage is not forced");
            account.removeProperty(PROPERTY_ACCOUNT_TIMESTAMP_VPN_DETECTED);
            cdnCreditsUsageEnforced = false;
        }
        if (Boolean.TRUE.equals(cdnCreditsUsageEnforced) && creditsInBytes <= 0) {
            /* CDN credits are needed for downloading but user has no CDN credits. */
            errorVPNUsed(account);
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final SIZEUNIT maxSizeUnit = (SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue();
        final String available_credits_human_readable;
        if (creditsInBytes == -1) {
            /* Credits number is not known */
            available_credits_human_readable = "N/A";
        } else {
            available_credits_human_readable = SIZEUNIT.formatValue(maxSizeUnit, creditsInBytes);
        }
        String cdnCreditsStatusText = "CDN credits: " + available_credits_human_readable + " | Used: ";
        if ((AccountType.FREE.equals(account.getType()) && this.isUsingCDNCredits(account)) || Boolean.TRUE.equals(cdnCreditsUsageEnforced)) {
            cdnCreditsStatusText += "Yes";
            if (Boolean.TRUE.equals(cdnCreditsUsageEnforced)) {
                cdnCreditsStatusText += " (forced)";
            }
            if (account.getType() == AccountType.FREE) {
                /* Treat Free accounts like a premium account if credits are used. */
                account.setMaxSimultanDownloads(getMaxSimultanPremiumDownloadNum());
            }
            ai.setTrafficLeft(creditsInBytes);
            ai.setTrafficRefill(false);
            /**
             * Display traffic but do not care about how much is actually left. <br>
             * If the credits are used up, normal free downloads can be performed.
             */
            ai.setSpecialTraffic(true);
        } else {
            cdnCreditsStatusText += "No";
        }
        final String accountTypeText;
        if (this.isGoldAccount(account)) {
            accountTypeText = "Premium GOLD Account";
        } else {
            accountTypeText = account.getType().getLabel();
        }
        ai.setStatus(accountTypeText + " " + cdnCreditsStatusText);
    }

    private Map<String, Object> parseAPIResponse(final Account account) throws PluginException {
        return parseAPIResponse(this.br, account);
    }

    private Map<String, Object> parseAPIResponse(final Browser br, final Account account) throws PluginException {
        try {
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            return entries;
        } catch (final Exception ignore) {
            throw new AccountUnavailableException("Invalid API response", 60 * 1000);
        }
    }

    /**
     * Check- and handle API errors
     *
     * @throws PluginException
     */
    private Map<String, Object> handleErrorsAPI(final Account account) throws PluginException {
        final Map<String, Object> entries = parseAPIResponse(account);
        return handleErrorsAPI(entries, account);
    }

    private Map<String, Object> handleErrorsAPI(final Map<String, Object> entries, final Account account) throws PluginException {
        final Object statusO = entries.get("status");
        if (statusO == null) {
            /* No error */
            return entries;
        } else if (!"KO".equalsIgnoreCase((String) statusO)) {
            /* No error */
            return entries;
        }
        final String message = (String) entries.get("message");
        if (StringUtils.isEmpty(message)) {
            /* This should never happen! */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown API error", 5 * 60 * 1000l);
        } else if (message.matches("(?i)Flood detected: IP Locked #\\d+")) {
            /*
             * 2019-07-18: This may even happen on the first login attempt. When this happens we cannot know whether the account is valid or
             * not!
             */
            throw new AccountUnavailableException("API flood detection has been triggered", 5 * 60 * 1000l);
        } else if (message.matches("(?i)Flood detected: User Locked #\\d+")) {
            throw new AccountUnavailableException("API flood detection has been triggered", 5 * 60 * 1000l);
        } else if (message.matches("(?i)Not authenticated #\\d+")) {
            /* Login required but not logged in (this should never happen) */
            errorInvalidAPIKey(account);
        } else if (message.matches("(?i)No such user\\s*#\\d+")) {
            errorInvalidAPIKey(account);
        } else if (message.matches("(?i)Owner locked\\s*#\\d+")) {
            /* 2021-01-29 */
            throw new AccountInvalidException("Account banned: " + message);
        } else if (message.matches("(?i)IP Locked\\s*#\\d+")) {
            throw new AccountUnavailableException(message, 60 * 60 * 1000l);
        } else if (message.matches("(?i).*Must be a customer.*")) {
            /* 2020-06-09: E.g. {"message":"Must be a customer (Premium, Access) #200","status":"KO"} */
            /* Free account api key entered by user --> API can only be used by premium users */
            errorPremiumNeededForAPIDownloading(account);
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (isAPIErrorPassword(message)) {
            this.errorWrongPassword(this.getDownloadLink());
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (message.matches("(?i).*Resource not allowed #\\d+")) {
            errorAccessControlLimit(this.getDownloadLink());
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (message.matches("(?i).*Resource not found #\\d+")) {
            /* Usually goes along with http response 404 */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (message.matches("(?i).*Only \\d+ locations? allowed at a time\\s*#\\d+")) {
            /* 2021-03-17: Tmp. account ban e.g. because of account sharing or user is just using it with too many IPs. */
            throw new AccountUnavailableException(message, 5 * 60 * 1000l);
        } else if (message.matches("(?i).*professional equipment, must have CDN.*")) {
            errorVPNUsed(account);
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            /* Unknown/unhandled error */
            logger.warning("Handling unknown API error: " + message);
            if (this.getPluginEnvironment() == PluginEnvironment.ACCOUNT_CHECK) {
                /* Account error */
                throw new AccountUnavailableException(message, 5 * 60 * 1000l);
            } else {
                /* Error during download/linkcheck */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, message, 5 * 60 * 1000l);
            }
        }
        /* This code should never be reached! */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    private boolean isAPIErrorPassword(final String errorMsg) {
        if (errorMsg == null) {
            return false;
        } else if (errorMsg.matches("(?i).*(Invalid password\\.|Password not provided\\.).*Resource not allowed.*")) {
            return true;
        } else {
            return false;
        }
    }

    private void errorPremiumNeededForAPIDownloading(final Account account) throws AccountInvalidException {
        throw new AccountInvalidException("Only premium users can use the 1fichier API for downloading!");
    }

    private void errorWrongPassword(final DownloadLink link) throws PluginException {
        link.setDownloadPassword(null);
        if (link.isPasswordProtected()) {
            throw new PluginException(LinkStatus.ERROR_RETRY, "Password wrong!");
        } else {
            link.setPasswordProtected(true);
            throw new PluginException(LinkStatus.ERROR_RETRY, "Password required!");
        }
    }

    private static void errorInvalidAPIKey(final Account account) throws PluginException {
        if (account == null) {
            /* This should never happen */
            throw new AccountRequiredException();
        }
        /* Assume APIKey is invalid or simply not valid anymore (e.g. user disabled or changed APIKey) */
        final StringBuilder sb = new StringBuilder();
        String language = System.getProperty("user.language").toLowerCase();
        if ("de".equals(language)) {
            sb.append("Ungültiger API-Schlüssel!");
            sb.append("\r\nDu findest Deinen API-Schlüssel hier: 1fichier.com/console/params.pl");
            sb.append("\r\nWenn Du einen 'Premium-Schlüssel'/'Gutschein'/'Aktivierungsschlüssel' von einem Reseller gekauft hast, musst Du ein 1fichier-Konto erstellen, Dich anmelden und diesen Code hier einlösen: 1fichier.com/console/vu.pl");
            sb.append("\r\nBitte beachte, dass API-Schlüssel nur von Premium-Kunden zum Herunterladen verwendet werden können.");
            sb.append("\r\nInformationen für 1fichier FREE-Kontonutzer:");
            sb.append("\r\nUm kostenlose Konten in JD zu verwenden, gehe zu Einstellungen -> Plugins -> 1fichier.com -> 'Premium-API verwenden' -> Deaktiviere dies -> Versuche jetzt, Dich erneut anzumelden, diesmal mit Deiner E-Mail & Passwort.");
        } else if ("es".equals(language)) {
            sb.append("¡Clave API inválida!");
            sb.append("\r\nPuedes encontrar tu clave API aquí: 1fichier.com/console/params.pl");
            sb.append("\r\nSi compraste una 'clave premium'/'Cupón'/'Clave de activación' de un Reseller, necesitas crear una cuenta de 1fichier, iniciar sesión y canjear ese código aquí: 1fichier.com/console/vu.pl");
            sb.append("\r\nPor favor ten en cuenta que las claves API solo pueden ser utilizadas para descargar por clientes premium.");
            sb.append("\r\nInformación para usuarios de cuentas GRATUITAS de 1fichier:");
            sb.append("\r\nPara usar cuentas gratuitas en JD, ve a Configuración -> Plugins -> 1fichier.com -> 'Usar API premium' -> Desactiva esto -> Ahora intenta iniciar sesión de nuevo, esta vez con tu correo electrónico y contraseña.");
        } else if ("fr".equals(language)) {
            sb.append("Clé API invalide !");
            sb.append("\r\nTu peux trouver ta clé API ici : 1fichier.com/console/params.pl");
            sb.append("\r\nSi tu as acheté une 'clé premium'/'Coupon'/'Clé d'activation' auprès d'un Reseller, tu dois créer un compte 1fichier, te connecter et échanger ce code ici : 1fichier.com/console/vu.pl");
            sb.append("\r\nGarde à l'esprit que les clés API ne peuvent être utilisées pour télécharger que par les clients premium.");
            sb.append("\r\nInformations pour les utilisateurs de comptes GRATUITS 1fichier :");
            sb.append("\r\nPour utiliser des comptes gratuits dans JD, va dans Paramètres -> Plugins -> 1fichier.com -> 'Utiliser l'API premium' -> Désactive cette option -> Maintenant essaie de te connecter à nouveau, cette fois avec ton e-mail et mot de passe.");
        } else {
            sb.append("Invalid API Key!");
            sb.append("\r\nYou can find your API Key here: 1fichier.com/console/params.pl");
            sb.append("\r\nIf you bought a 'premium key'/'Voucher'/'Activation key' from a Reseller, you need to create a 1fichier account, login and redeem that code here: 1fichier.com/console/vu.pl");
            sb.append("\r\nPlease keep in mind that API Keys can only be used for downloading by premium customers.");
            sb.append("\r\nInformation for 1fichier FREE account users:");
            sb.append("\r\nTo use free accounts in JD, Go to Settings -> Plugins -> 1fichier.com -> 'Use premium API' -> Disable this -> Now try to login again, this time with your e-mail & password.");
        }
        throw new AccountInvalidException(sb.toString());
    }

    /** Checks whether we're logged in via website. */
    private boolean isLoggedinWebsite(final Browser br) {
        if (isLoginCookieExists(br) && br.containsHTML("/logout\\.pl")) {
            return true;
        } else {
            return false;
        }
    }

    private boolean isLoginCookieExists(final Browser br) {
        if (br.getCookie(this.getHost(), "SID", Cookies.NOTDELETEDPATTERN) != null) {
            return true;
        } else {
            return false;
        }
    }

    private void loginWebsite(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            /* Load cookies */
            prepareBrowserWebsite(br);
            final Cookies cookies = account.loadCookies("");
            final String pathAccountOverview = "/console/abo.pl";
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
                setBasicAuthHeader(br, account);
                if (!force) {
                    /* Do not validate cookies */
                    return;
                }
                br.getPage("https://" + this.getHost() + pathAccountOverview);
                if (isLoggedinWebsite(br)) {
                    logger.info("Cookie login successful");
                    account.saveCookies(br.getCookies(getHost()), "");
                    return;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    prepareBrowserWebsite(this.br);
                }
            }
            logger.info("Performing full website login");
            br.getPage("https://" + getHost() + "/login.pl");
            final String username = account.getUser();
            final String password = account.getPass();
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("mail", username);
            query.appendEncoded("pass", password);
            /* Now add the "Advanced options" parameters */
            /* do not purge older sessions by no adding the param down below */
            // query.appendEncoded("purge", "on");
            query.appendEncoded("lt", "on"); // long term session
            query.appendEncoded("other", "on"); // set cookies also on other 1fichier domains
            query.appendEncoded("valider", "OK");
            br.postPage("/login.pl", query);
            if (!isLoggedinWebsite(this.br)) {
                logger.info("We are not yet logged in -> Looking for 2FA login form");
                final Form[] forms = br.getForms();
                if (forms == null || forms.length == 0) {
                    /* No 2FA form found -> Account looks to be invalid */
                    errorNotLoggedIn(account);
                    /* Thi code should never be reached */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                Form twoFAForm = null;
                final String formKey2FA = "tfa";
                for (final Form form : forms) {
                    if (form.hasInputFieldByName(formKey2FA)) {
                        logger.info("Found 2FA login form");
                        twoFAForm = form;
                        break;
                    }
                }
                if (twoFAForm == null) {
                    /* No 2FA form found -> Account looks to be invalid */
                    errorNotLoggedIn(account);
                    /* Thi code should never be reached */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                logger.info("2FA code required");
                final String twoFACode = this.getTwoFACode(account, "\\d{6}");
                logger.info("Submitting 2FA code");
                twoFAForm.put(formKey2FA, twoFACode);
                br.submitForm(twoFAForm);
                if (!isLoggedinWebsite(br)) {
                    /* Still not logged in -> User must have entered invalid 2FA code */
                    throw new AccountInvalidException(org.jdownloader.gui.translate._GUI.T.jd_gui_swing_components_AccountDialog_2FA_login_invalid());
                }
            }
            setBasicAuthHeader(br, account);
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    /** Call this if login failed for no specific reason. */
    private void errorNotLoggedIn(final Account account) throws PluginException {
        final String errorTooManyLoginAttempts = br.getRegex(">\\s*(More than \\d+ login try per \\d+ minutes is not allowed)").getMatch(0);
        if (errorTooManyLoginAttempts != null) {
            throw new AccountUnavailableException(errorTooManyLoginAttempts, 1 * 60 * 1000l);
        }
        if (br.containsHTML("Your account will be unlock")) {
            throw new AccountUnavailableException("Your account will be unlocked within 1 hour", 10 * 60 * 1000l);
        } else if (br.containsHTML("your IP address") && br.containsHTML("is temporarily locked")) {
            throw new AccountUnavailableException("For security reasons, following many identification errors, your IP address is temporarily locked.", 15 * 60 * 1000l);
        } else {
            throw new AccountInvalidException();
        }
    }

    @Override
    protected long getStartInterval(final DownloadLink link, final Account account) {
        if (account == null || !AccountType.PREMIUM.equals(account.getType()) || link == null) {
            return super.getStartInterval(link, account);
        } else {
            final long knownDownloadSize = link.getKnownDownloadSize();
            if (knownDownloadSize <= 50 * 1024 * 1024) {
                final int wait = PluginJsonConfig.get(OneFichierConfigInterface.class).getSmallFilesWaitInterval();
                /* Small file or big file but only some bytes remaining -> Avoid IP block because of too many downloads in short time */
                return Math.max(0, wait * 1000);
            } else {
                return super.getStartInterval(link, account);
            }
        }
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (canUseAPIForPremiumDownloads(account)) {
            final String directurlproperty = getDirectlinkproperty(account);
            String storedDirecturl = link.getStringProperty(directurlproperty);
            if (storedDirecturl != null) {
                storedDirecturl = this.getURLWithPreferredProtocol(storedDirecturl);
                logger.info("Trying to re-use stored directurl: " + storedDirecturl);
                dl = new jd.plugins.BrowserAdapter().openDownload(br, link, storedDirecturl, this.isResumeable(link, account), this.getMaxChunks(link, account));
                if (this.looksLikeDownloadableContent(dl.getConnection())) {
                    logger.info("Re-using stored directurl");
                    dl.startDownload();
                    return;
                }
                logger.info("Stored directurl is invalid");
                br.followConnection();
                link.removeProperty(directurlproperty);
            }
            String dllink = getDllinkPremiumAPI(link, account);
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dllink = this.getURLWithPreferredProtocol(dllink);
            link.setProperty(directurlproperty, dllink);
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                logger.warning("The final dllink seems not to be a file!");
                br.followConnection(true);
                errorHandlingWebsite(link, account, br);
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error", 5 * 60 * 1000l);
            }
            dl.startDownload();
        } else {
            /**
             * Website mode is required for free (anonymous) and free account downloads
             */
            handleDownloadWebsite(link, account);
        }
    }

    private String getDllinkPremiumAPI(final DownloadLink link, final Account account) throws Exception {
        /**
         * 2019-04-05: At the moment there are no benefits for us when using this. </br>
         * 2021-01-29: Removed this because if login is blocked because of "flood control" this won't work either!
         */
        boolean checkFileInfoBeforeDownloadAttempt = false;
        final boolean dev_mode_check_via_api = false;
        if (dev_mode_check_via_api && DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            checkFileInfoBeforeDownloadAttempt = true;
        }
        if (checkFileInfoBeforeDownloadAttempt) {
            requestFileInformationAPI(br, link, account);
        }
        setPremiumAPIHeaders(br, account);
        /* Do NOT trust pwProtected as this is obtained via website or old mass-linkcheck API!! */
        String passCode = link.getDownloadPassword();
        /* Check if we already know that this file is password protected ... */
        /** Try passwords in this order: 1. DownloadLink stored password, 2. Last used password, 3. Ask user */
        if (link.isPasswordProtected() && passCode == null) {
            passCode = getUserInput("Password?", link);
        }
        /** Description of optional parameters: cdn=0/1 - use download-credits, */
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("url", this.getContentURL(link));
        postdata.put("pass", passCode);
        postdata.put("no_ssl", PluginJsonConfig.get(OneFichierConfigInterface.class).getSSLMode() == SSLMode.FORCE_HTTP ? 1 : 0);
        performAPIRequest(API_BASE + "/download/get_token.cgi", JSonStorage.serializeToJson(postdata));
        final Map<String, Object> entries = handleErrorsAPI(account);
        /* 2019-04-04: Downloadlink is officially only valid for 5 minutes */
        String dllink = entries.get("url").toString();
        if (passCode != null) {
            link.setDownloadPassword(passCode);
        } else {
            /* File is not password protected (anymore) */
            link.setPasswordProtected(false);
        }
        if (StringUtils.isEmpty(dllink)) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to find final downloadurl");
        }
        return dllink;
    }

    private void performAPIRequest(final String url, final String json_postdata) throws IOException {
        performAPIRequest(br, url, json_postdata);
    }

    private void performAPIRequest(final Browser br, final String url, final String json_string) throws IOException {
        final PostRequest downloadReq = br.createJSonPostRequest(url, json_string);
        downloadReq.setContentType("application/json");
        br.openRequestConnection(downloadReq);
        br.loadConnection(null);
    }

    public static boolean canUseAPI(final Account account) {
        /**
         * true = use premium API, false = use combination of website + OLD basic auth API - ONLY RELEVANT FOR PREMIUM USERS; IF ENABLED,
         * USER HAS TO ENTER API_KEY INSTEAD OF USERNAME:PASSWORD (or APIKEY:APIKEY)!!
         */
        if (account != null && looksLikeValidAPIKeySTATIC(account.getPass())) {
            return PluginJsonConfig.get(OneFichierConfigInterface.class).isUsePremiumAPIEnabled();
        } else {
            return false;
        }
    }

    public static boolean canUseAPIForPremiumDownloads(final Account account) {
        /**
         * true = use premium API, false = use combination of website + OLD basic auth API - ONLY RELEVANT FOR PREMIUM USERS; IF ENABLED,
         * USER HAS TO ENTER API_KEY INSTEAD OF USERNAME:PASSWORD (or APIKEY:APIKEY)!!
         */
        if (canUseAPI(account) && (account.getType() == null || account.getType() == AccountType.PREMIUM || account.getType() == AccountType.UNKNOWN)) {
            return PluginJsonConfig.get(OneFichierConfigInterface.class).isUsePremiumAPIEnabled();
        } else {
            return false;
        }
    }

    /** Required to authenticate via API. */
    public static void setPremiumAPIHeaders(final Browser br, final String apiKey) {
        br.getHeaders().put("Authorization", "Bearer " + apiKey);
    }

    @Deprecated
    /** 2025-04-01: Internally deprecated but website still accepts user login via basic auth. */
    private void setBasicAuthHeader(final Browser br, final Account account) {
        br.getHeaders().put("Authorization", "Basic " + Encoding.Base64Encode(account.getUser() + ":" + account.getPass()));
    }

    private boolean isaccessControlLimited(final DownloadLink link) {
        if (link.hasProperty(PROPERTY_ACL_ACCESS_CONTROL_LIMIT)) {
            return true;
        } else {
            return false;
        }
    }

    /** Ends up in PluginException(LinkStatus.ERROR_PLUGIN_DEFECT). */
    private void handleErrorsLastResortWebsite(final DownloadLink link, final Account account) throws PluginException {
        if (account != null && !this.isLoggedinWebsite(this.br)) {
            throw new AccountUnavailableException("Session expired?", 5 * 60 * 1000l);
        } else if (this.isaccessControlLimited(link)) {
            // throw new PluginException(LinkStatus.ERROR_FATAL, "This link is private. You're not authorized to download it!");
            /*
             * 2021-02-10: Not sure - seems like this could be multiple reasons: registered only, premium only, IP/country only or private
             * file --> Owner only. See https://1fichier.com/console/acl.pl
             */
            throw new PluginException(LinkStatus.ERROR_FATAL, "Access to this file has been restricted");
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    @Override
    public Class<OneFichierConfigInterface> getConfigInterface() {
        return OneFichierConfigInterface.class;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/en/cgu.html";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 10;
    }

    @Override
    public boolean enoughTrafficFor(final DownloadLink link, final Account account) throws Exception {
        final String directlinkproperty = getDirectlinkproperty(account);
        final String dllink = link.getStringProperty(directlinkproperty, null);
        /*
         * E.g. account doesn't have enough traffic left but we still got a stored directurl from a previous downloadstart --> Allow
         * download attempt anyways as we can be quite sure that it will still be valid.
         */
        if (StringUtils.isNotEmpty(dllink)) {
            return true;
        } else {
            return super.enoughTrafficFor(link, account);
        }
    }

    /** Call this whenever an account was attempted to be used with a VPN/proxy/datacenter IP. */
    private void errorVPNUsed(final Account account) throws AccountUnavailableException {
        this.displayVPNWarning(account);
        throw new AccountUnavailableException("VPN/proxy/datacenter IP not allowed; CDN credits needed for downloading", 5 * 60 * 1000l);
    }

    private void displayVPNWarning(final Account account) {
        if (account == null) {
            throw new IllegalArgumentException();
        }
        synchronized (account) {
            if (account.hasProperty(PROPERTY_ACCOUNT_HAS_SHOWN_VPN_LOGIN_WARNING)) {
                /* Message has already been displayed for this account */
                return;
            }
            account.setProperty(PROPERTY_ACCOUNT_HAS_SHOWN_VPN_LOGIN_WARNING, System.currentTimeMillis());
        }
        /* TODO: Maybe add extra errorhandling for "Premium GOLD" account owners in API mode. */
        final Thread thread = new Thread() {
            public void run() {
                try {
                    String message = "";
                    final String title;
                    String language = System.getProperty("user.language").toLowerCase();
                    if ("de".equals(language)) {
                        title = "VPN/Proxy oder Rechenzentrum-IP erkannt!";
                        message += "\r\nEs scheint, dass Du eine VPN/Proxy oder Rechenzentrum-IP verwendest, die laut den Nutzungsbedingungen von 1fichier nicht für Premium-Nutzer erlaubt ist";
                        message += "\r\nWas kann dagegen getan werden?";
                        message += "\r\n1. Melde Dich über die 1fichier-Website an '1fichier.com/login.pl'.";
                        message += "\r\n2. Gehe zu '1fichier.com/network.html'\r\nWenn Du eine rote oder gelbe Warnmeldung wie \"VPN erkannt\" oder \"Erfordert die Nutzung von CDN-Guthaben oder das Premium GOLD Angebot\" siehst, wurde Deine IP aus einem bestimmten Grund markiert.";
                        message += "\r\n3. Stelle sicher, dass Du eine normale Heim-IP verwendest, deaktiviere alle Proxys/VPNs und versuche es erneut.";
                        message += "\r\n4. Wenn Du bewusst einen VPN/Proxy/Rechenzentrum-IP verwendest, musst Du ein 'Premium GOLD' Konto und/oder CDN-Guthaben kaufen und zum Herunterladen verwenden, siehe 1fichier.com/tarifs.html";
                        message += "\r\n";
                        message += "\r\nWenn das Problem weiterhin besteht:";
                        message += "\r\nWenn Du denkst, dass dies ein Fehler ist, kontaktiere den 1fichier Support: 1fichier.com/contact.html";
                        message += "\r\nDies ist kein JDownloader-Fehler, sondern eine serverseitige Fehlermeldung, die wir Dir hier einfach weiterleiten.";
                        message += "\r\nWeitere Informationen zu diesem Thema: https://support.jdownloader.org/knowledgebase/article/plugins-1fichiercom-settings-and-troubleshooting";
                    } else if ("es".equals(language)) {
                        title = "¡VPN/proxy o IP de centro de datos detectada!";
                        message += "\r\nParece que estás usando una VPN/proxy o IP de centro de datos que los usuarios premium no pueden usar según los términos de servicio de 1fichier";
                        message += "\r\n¿Qué se puede hacer al respecto?";
                        message += "\r\n1. Inicia sesión a través del sitio web de 1fichier '1fichier.com/login.pl'.";
                        message += "\r\n2. Ve a '1fichier.com/network.html'\r\nSi puedes ver un mensaje de advertencia rojo o amarillo similar a \"VPN detectada\" o \"Requiere el uso de créditos CDN o la oferta Premium GOLD\", tu IP ha sido marcada por algún motivo.";
                        message += "\r\n3. Asegúrate de usar una IP doméstica normal, desactiva cualquier proxy/VPN e intenta nuevamente.";
                        message += "\r\n4. Si estás usando conscientemente una VPN/proxy/IP de centro de datos, debes comprar una cuenta 'Premium GOLD' y/o créditos CDN y usarlos para descargar, ve a 1fichier.com/tarifs.html";
                        message += "\r\n";
                        message += "\r\nSi el problema no desaparece:";
                        message += "\r\nSi crees que esto es un error, contacta al soporte de 1fichier: 1fichier.com/contact.html";
                        message += "\r\nEsto no es un error de JDownloader sino un mensaje de error del servidor que simplemente te reenviamos aquí.";
                        message += "\r\nMás información sobre este tema: https://support.jdownloader.org/knowledgebase/article/plugins-1fichiercom-settings-and-troubleshooting";
                    } else if ("fr".equals(language)) {
                        title = "VPN/proxy ou IP de centre de données détecté !";
                        message += "\r\nIl semble que tu utilises un VPN/proxy ou une IP de centre de données que les utilisateurs premium ne sont pas autorisés à utiliser selon les conditions d'utilisation de 1fichier";
                        message += "\r\nQue peut-on faire à ce sujet ?";
                        message += "\r\n1. Connecte-toi via le site web de 1fichier '1fichier.com/login.pl'.";
                        message += "\r\n2. Va sur '1fichier.com/network.html'\r\nSi tu peux voir un message d'avertissement rouge ou jaune similaire à \"VPN détecté\" ou \"Nécessite l'utilisation de crédits CDN ou l'offre Premium GOLD\", ton IP a été signalée pour une raison quelconque.";
                        message += "\r\n3. Assure-toi d'utiliser une IP domestique régulière, désactive tout proxy/VPN et réessaie.";
                        message += "\r\n4. Si tu utilises sciemment un VPN/proxy/IP de centre de données, tu dois acheter un compte 'Premium GOLD' et/ou des crédits CDN et les utiliser pour télécharger, voir 1fichier.com/tarifs.html";
                        message += "\r\n";
                        message += "\r\nSi le problème ne disparaît pas :";
                        message += "\r\nSi tu penses qu'il s'agit d'une erreur, contacte le support de 1fichier : 1fichier.com/contact.html";
                        message += "\r\nCe n'est pas un bug de JDownloader mais un message d'erreur côté serveur que nous te transmettons simplement ici.";
                        message += "\r\nPlus d'informations sur ce sujet : https://support.jdownloader.org/knowledgebase/article/plugins-1fichiercom-settings-and-troubleshooting";
                    } else {
                        title = "VPN/proxy or datacenter IP detected!";
                        message += "\r\nIt looks like you are using a VPN/proxy or datacenter IP which premium users are not allowed to use according to the terms of service of 1fichier";
                        message += "\r\nWhat can be done about this?";
                        message += "\r\n1. Login via the 1fichier website '1fichier.com/login.pl'.";
                        message += "\r\n2. Go to '1fichier.com/network.html'\r\nIf you can see a red or yellow warning message similar to \"VPN detected\" or \"Requires the use of CDN credits or the Premium GOLD offer\", your IP was flagged for some reason.";
                        message += "\r\n3. Ensure that you are using a regular home IP, disable any proxy/VPN and try again.";
                        message += "\r\n4. If you are knowingly using a VPN/proxy/datacenter IP, you need to purchase a 'Premium GOLD' account and/or CDN credits and use them for downloading, see 1fichier.com/tarifs.html";
                        message += "\r\n";
                        message += "\r\nIf the problem does not disappear:";
                        message += "\r\nIf you think this is a mistake, contact the 1fichier support: 1fichier.com/contact.html";
                        message += "\r\nThis is not a JDownloader bug but a server side error message which we are simply forwarding to you here.";
                        message += "\r\nMore information about this topic: https://support.jdownloader.org/knowledgebase/article/plugins-1fichiercom-settings-and-troubleshooting";
                    }
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, title, message);
                    dialog.setTimeout(300 * 1000);
                    final ConfirmDialogInterface ret = UIOManager.I().show(ConfirmDialogInterface.class, dialog);
                    ret.throwCloseExceptions();
                } catch (final Throwable e) {
                    getLogger().log(e);
                }
            };
        };
        thread.setDaemon(true);
        thread.start();
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/console/params.pl";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        return looksLikeValidAPIKeySTATIC(str);
    }

    public static boolean looksLikeValidAPIKeySTATIC(final String str) {
        if (str != null && str.matches("[A-Za-z0-9\\-_=]{32}")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}