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
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.jdownloader.plugins.components.config.OneFichierConfigInterface;
import org.jdownloader.plugins.components.config.OneFichierConfigInterface.LinkcheckMode;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.requests.GetRequest;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.InputField;
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

@HostPlugin(revision = "$Revision: 50860 $", interfaceVersion = 3, names = {}, urls = {})
public class OneFichierCom extends PluginForHost {
    private final String         PROPERTY_FREELINK                 = "freeLink";
    private final String         PROPERTY_HOTLINK                  = "hotlink";
    private final String         PROPERTY_PREMLINK                 = "premLink";
    /** URLs can be restricted for various reason: https://1fichier.com/console/acl.pl */
    public static final String   PROPERTY_ACL_ACCESS_CONTROL_LIMIT = "acl_access_control_limit";
    /** 2019-04-04: Documentation: https://1fichier.com/api.html */
    public static final String   API_BASE                          = "https://api.1fichier.com/v1";
    /*
     * Max total connections for premium = 30 (RE: admin, updated 07.03.2019) --> See also their FAQ: https://1fichier.com/hlp.html#dllent
     */
    private static final boolean resume_account_premium            = true;
    /* 2015-07-10: According to admin, resume in free mode is not possible anymore. On attempt this will lead to 404 server error! */
    private static final int     maxchunks_free                    = 1;
    private static final boolean resume_free                       = true;
    /*
     * Settings for hotlinks - basically such links are created by premium users so free users can download them without limits (same limits
     * as premium users).
     */
    private static final boolean resume_free_hotlink               = true;
    private static final int     maxchunks_free_hotlink            = -3;

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

    private Browser prepareBrowserWebsite(final Browser br) {
        br.setConnectTimeout(3 * 60 * 1000);
        br.setReadTimeout(3 * 60 * 1000);
        br.getHeaders().put("User-Agent", "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.103 Safari/537.36");
        br.getHeaders().put("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8");
        br.getHeaders().put("Accept-Language", "en-us,en;q=0.5");
        br.getHeaders().put("Pragma", null);
        br.getHeaders().put("Cache-Control", null);
        br.setCustomCharset("utf-8");
        /* we want ENGLISH! */
        br.setCookie(this.getHost(), "LG", "en");
        br.setAllowedResponseCodes(new int[] { 403, 503 });
        return br;
    }

    private Browser prepareBrowserAPI(final Browser br, final Account account) throws Exception {
        if (br == null) {
            return null;
        }
        br.setConnectTimeout(3 * 60 * 1000);
        br.setReadTimeout(3 * 60 * 1000);
        br.getHeaders().put("User-Agent", "JDownloader");
        br.getHeaders().put("Content-Type", "application/json");
        br.setAllowedResponseCodes(new int[] { 401, 403, 503 });
        setPremiumAPIHeaders(br, account);
        return br;
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

    private String correctProtocol(final String input) {
        return input.replaceFirst("(?i)http://", "https://");
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
            /* 2021-07-27: Another attempt to force English language as only setting the cookie may not be enough. */
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
            /* Trigger fallback to single linkcheck */
            // requestFileInformationAPI(br, urls[0], account);
            /* Trigger single linkcheck */
            return false;
        }
        try {
            final Browser br = createNewBrowserInstance();
            prepareBrowserWebsite(br);
            br.getHeaders().put("User-Agent", "");
            br.getHeaders().put("Accept", "");
            br.getHeaders().put("Accept-Language", "");
            br.setCookiesExclusive(true);
            final StringBuilder sb = new StringBuilder();
            final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
            int index = 0;
            while (true) {
                links.clear();
                while (true) {
                    /* we test 100 links at once */
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
                // remove last &
                sb.deleteCharAt(sb.length() - 1);
                /**
                 * This method is serverside deprecated but we're still using it because: </br>
                 * 1. It is still working. </br>
                 * 2. It is the only method that can be used to check multiple items with one request.
                 */
                br.postPageRaw(correctProtocol("http://" + this.getHost() + "/check_links.pl"), sb.toString());
                for (final DownloadLink link : links) {
                    // final String addedLink = dllink.getDownloadURL();
                    final String file_id = this.getFID(link);
                    /* Set fallback-filename */
                    if (!link.isNameSet()) {
                        link.setName(file_id);
                    }
                    if (br.containsHTML(file_id + "[^;]*;;;(NOT FOUND|BAD LINK)")) {
                        link.setAvailable(false);
                    } else if (br.containsHTML(file_id + "[^;]*;;;PRIVATE")) {
                        /**
                         * Private or password protected file. </br>
                         * Admin was asked to change this to return a more precise status instead but declined that suggestion.
                         */
                        link.setProperty(PROPERTY_ACL_ACCESS_CONTROL_LIMIT, true);
                        link.setAvailable(true);
                    } else {
                        final String[] linkInfo = br.getRegex(file_id + "[^;]*;([^;]+);(\\d+)").getRow(0);
                        if (linkInfo.length != 2) {
                            logger.warning("Linkchecker for 1fichier.com is broken!");
                            return false;
                        }
                        link.removeProperty(PROPERTY_ACL_ACCESS_CONTROL_LIMIT);
                        link.setAvailable(true);
                        /* Trust API information. */
                        link.setFinalFileName(Encoding.htmlDecode(linkInfo[0]));
                        link.setVerifiedFileSize(Long.parseLong(linkInfo[1]));
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
            /*
             * Advantage when doing this: We can get the file information even for password protected files (if we got the correct
             * password).
             */
            return requestFileInformationAPI(br, link, account);
        } else {
            checkLinks(new DownloadLink[] { link });
            prepareBrowserWebsite(br);
            if (!link.isAvailabilityStatusChecked()) {
                return AvailableStatus.UNCHECKED;
            } else if (link.isAvailabilityStatusChecked() && !link.isAvailable()) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                return AvailableStatus.TRUE;
            }
        }
    }

    /**
     * 2022-12-10: Do not use this as it will cause IP-blocks!!
     */
    /**
     * Checks single URLs via API.
     *
     * @throws Exception
     */
    public AvailableStatus requestFileInformationAPI(final Browser br, final DownloadLink link, final Account account) throws Exception {
        prepareBrowserAPI(br, account);
        final Map<String, Object> postData = new HashMap<String, Object>();
        postData.put("url", this.getContentURL(link));
        postData.put("pass", link.getDownloadPassword());
        performAPIRequest(API_BASE + "/file/info.cgi", JSonStorage.serializeToJson(postData));
        final Map<String, Object> entries = this.parseAPIResponse(account);
        final String errorMsg = (String) entries.get("message");
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* E.g. message": "Resource not found #469" */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (errorMsg != null && errorMsg.matches("(?i).*Resource not allowed.*")) {
            /* 2020-01-30: e.g. {"status":"KO","message":"Resource not allowed #631"} */
            /*
             * Password-protected or private file: No information given at all but we know that file is online. Example reasons: file is not
             * allowed to be downloaded in current country, by current user, file is private, file is password protected.
             */
            link.setProperty(PROPERTY_ACL_ACCESS_CONTROL_LIMIT, true);
            /* Else all is fine - URL is online but we might not be able to download it. */
            return AvailableStatus.TRUE;
        }
        this.handleErrorsAPI(entries, account);
        link.removeProperty(PROPERTY_ACL_ACCESS_CONTROL_LIMIT);
        final short passwordProtected = ((Number) entries.get("pass")).shortValue();
        if (passwordProtected == 1) {
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
        /* 2020-01-30: We cannot work with this checksum (512 bit "Whirlpool" checksum). */
        final String checksum = (String) entries.get("checksum");
        if (checksum != null) {
            link.setHashInfo(HashInfo.parse(checksum, TYPE.WHIRLPOOL));
        }
        link.setAvailable(true);
        return AvailableStatus.TRUE;
    }

    @Override
    protected int getMaxSimultanDownload(final DownloadLink link, final Account account) {
        if (account == null && (link != null && link.getProperty(PROPERTY_HOTLINK, null) != null)) {
            return Integer.MAX_VALUE;
        } else {
            return super.getMaxSimultanDownload(link, account);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        /* Do not perform availablecheck here to save requests */
        // requestFileInformation(link);
        if (false && checkShowFreeDialog(getHost())) {
            showFreeDialog(getHost());
        }
        doFree(null, link);
    }

    private String regex_dllink_middle = "align:middle\">\\s+<a href=(\"|')(https?://[a-zA-Z0-9_\\-]+\\.(1fichier|desfichiers)\\.com/[a-zA-Z0-9]+.*?)\\1";

    public void doFree(final Account account, final DownloadLink link) throws Exception, PluginException {
        /* The following code will cover saved hotlinks */
        String dllink = link.getStringProperty(PROPERTY_HOTLINK);
        if (dllink != null) {
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, resume_free_hotlink, maxchunks_free_hotlink);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                // link has expired... but it could be for any reason! dont care!
                // clear saved final link
                link.removeProperty(PROPERTY_HOTLINK);
                br = createNewBrowserInstance();
                prepareBrowserWebsite(br);
            } else {
                /* resume download */
                logger.info("Hotlink download active");
                link.setProperty(PROPERTY_HOTLINK, dllink);
                dl.startDownload();
                return;
            }
        }
        /* retry/resume of cached free link! */
        dllink = link.getStringProperty(PROPERTY_FREELINK);
        if (dllink != null) {
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, resume_free, maxchunks_free);
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                /* resume download */
                link.setProperty(PROPERTY_FREELINK, dllink);
                dl.startDownload();
                return;
            } else {
                br.followConnection(true);
                /* link has expired... but it could be for any reason! dont care! */
                /* Clear saved final link */
                link.removeProperty(PROPERTY_FREELINK);
                br.clearAll();
                prepareBrowserWebsite(br);
            }
        }
        final String contentURL = getContentURLWebsite(link);
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, contentURL, resume_free_hotlink, maxchunks_free_hotlink);
        if (this.looksLikeDownloadableContent(dl.getConnection())) {
            /* Hotlink or user is using CDN credits for downloading */
            link.setProperty(PROPERTY_HOTLINK, dl.getConnection().getURL().toString());
            dl.startDownload();
            return;
        }
        /* Not hotlinkable.. standard free link... */
        br.followConnection();
        dllink = null;
        boolean retried = false;
        int i = 0;
        while (true) {
            i++;
            if (i > 1) {
                br.getPage(contentURL);
            }
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            errorHandlingWebsite(link, account, br);
            if (this.getDownloadPasswordForm() != null) {
                handleDownloadPasswordWebsite(link);
                dllink = br.getRedirectLocation();
                if (dllink == null) {
                    dllink = br.getRegex(regex_dllink_middle).getMatch(1);
                    if (dllink == null) {
                        logger.warning("Failed to find final downloadlink after password handling success");
                        this.handleErrorsLastResortWebsite(link, account);
                    }
                }
                logger.info("Successfully went through the password handling");
                break;
            } else {
                // base > submit:Free Download > submit:Show the download link + t:35140198 == link
                final Browser br2 = br.cloneBrowser();
                br2.getHeaders().put("Content-Type", "application/x-www-form-urlencoded");
                sleep(2000, link);
                br2.postPageRaw(br.getURL(), "");
                errorHandlingWebsite(link, account, br2);
                dllink = br2.getRedirectLocation();
                if (dllink == null) {
                    dllink = br2.getRegex(regex_dllink_middle).getMatch(1);
                }
                if (dllink == null) {
                    final Form a2 = br2.getForm(0);
                    if (a2 == null) {
                        this.handleErrorsLastResortWebsite(link, account);
                    }
                    a2.remove("save");
                    final Browser br3 = br.cloneBrowser();
                    br3.getHeaders().put("Content-Type", "application/x-www-form-urlencoded");
                    sleep(2000, link);
                    br3.submitForm(a2);
                    errorHandlingWebsite(link, account, br3);
                    if (dllink == null) {
                        dllink = br3.getRedirectLocation();
                    }
                    if (dllink == null) {
                        dllink = br3.getRegex("<a href=\"([^<>\"]*?)\"[^<>]*?>\\s*Click here to download").getMatch(0);
                    }
                    if (dllink == null) {
                        dllink = br3.getRegex("window\\.location\\s*=\\s*('|\")(https?://[a-zA-Z0-9_\\-]+\\.(1fichier|desfichiers)\\.com/[a-zA-Z0-9]+/.*?)\\1").getMatch(1);
                    }
                    if (dllink == null) {
                        String wait = br3.getRegex("var count = (\\d+);").getMatch(0);
                        if (wait != null && retried == false) {
                            retried = true;
                            sleep(1000 * Long.parseLong(wait), link);
                            continue;
                        }
                        this.handleErrorsLastResortWebsite(link, account);
                    }
                }
            }
            if (dllink != null) {
                break;
            }
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, resume_free, maxchunks_free);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            logger.warning("The final dllink seems not to be a file!");
            br.followConnection(true);
            errorHandlingWebsite(link, account, br);
            this.handleErrorsLastResortWebsite(link, account);
        }
        link.setProperty(PROPERTY_FREELINK, dllink);
        dl.startDownload();
    }

    private void errorHandlingWebsite(final DownloadLink link, final Account account, final Browser ibr) throws Exception {
        long responsecode = 200;
        if (ibr.getHttpConnection() != null) {
            responsecode = ibr.getHttpConnection().getResponseCode();
        }
        if (ibr.containsHTML(">\\s*File not found")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (ibr.containsHTML(">\\s*Software error:<")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 'Software error'", 10 * 60 * 1000l);
        } else if (ibr.containsHTML(">\\s*Connexion à la base de données impossible<|>Can\\'t connect DB")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Internal database error", 5 * 60 * 1000l);
        } else if (ibr.containsHTML("not possible to free unregistered users")) {
            throw new AccountRequiredException();
        } else if (ibr.containsHTML("Your account will be unlock")) {
            if (account != null) {
                throw new AccountUnavailableException("Locked for security reasons", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "IP blocked for security reasons", 60 * 60 * 1000l);
            }
        } else if (ibr.containsHTML(">\\s*Access to this file is protected|>\\s*This file is protected")) {
            /* Access restricted by IP / only registered users / only premium users / only owner */
            if (ibr.containsHTML(">\\s*The owner of this file has reserved access to the subscribers of our services")) {
                throw new AccountRequiredException();
            } else {
                errorAccessControlLimit(link);
            }
        } else if (ibr.containsHTML(">\\s*Your requests are too fast")) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Rate limit reached", 30 * 1000l);
        } else if (ibr.getURL().contains("/?c=DB")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Internal database error", 5 * 60 * 1000l);
        } else if (responsecode == 403) {
            if (ibr.containsHTML(">\\s*Premium status must not be used on professional services")) {
                errorVPNUsed(account);
                /* This code should never be reached */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            if (ibr.containsHTML(">\\s*Premium status is only allowed to be used on residential private and dedicated")) {
                errorVPNUsed(account);
                /* This code should never be reached */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 15 * 60 * 1000l);
        } else if (responsecode == 404) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 30 * 60 * 1000l);
        } else if (ibr.getHttpConnection().getResponseCode() == 503 && ibr.containsHTML(">\\s*Our services are in maintenance\\.\\s*Please come back after")) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Hoster is in maintenance mode!", 20 * 60 * 1000l);
        } else {
            ipBlockedErrorHandling(account, ibr);
        }
    }

    /** Call this whenever an account was attempted to be used with a VPN/proxy/datacenter IP. */
    private void errorVPNUsed(final Account account) throws AccountUnavailableException {
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            this.displayVPNWarning(account);
        }
        throw new AccountUnavailableException("VPN/proxy/datacenter IP not allowed", 5 * 60 * 1000l);
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

    /** Contains all errorhandling for IP related limits/errormessages. */
    private static void ipBlockedErrorHandling(final Account account, final Browser br) throws PluginException {
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
        // jdlog://3278035891641 jdlog://7543779150841
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

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        if (canUseAPI(account)) {
            return fetchAccountInfoAPI(account);
        } else {
            return fetchAccountInfoWebsite(account);
        }
    }

    public AccountInfo fetchAccountInfoWebsite(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        br.setAllowedResponseCodes(new int[] { 403, 503 });
        loginWebsite(account, true);
        br.getPage("/console/abo.pl");
        final String validUntil = br.getRegex("subscription is valid until\\s*<[^<]*>(\\d+-\\d+-\\d+)").getMatch(0);
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
            final GetRequest get = new GetRequest("https://" + this.getHost() + "/en/console/params.pl");
            get.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            br.getPage(get);
        }
        ai.setUnlimitedTraffic();
        /* Credits are only relevant if usage of credits for downloads is enabled: https://1fichier.com/console/params.pl */
        final String creditsStr = br.getRegex(">\\s*Your account have ([^<>\"]*?) of[^<]*credits").getMatch(0);
        final boolean useOwnCredits = StringUtils.equalsIgnoreCase("checked", br.getRegex("<input\\s*type=\"checkbox\"\\s*checked=\"(.*?)\"[^>]*name=\"own_credit\"").getMatch(0));
        long creditsAsFilesize = 0;
        if (creditsStr != null) {
            creditsAsFilesize = SizeFormatter.getSize(creditsStr);
        }
        this.setCdnCreditsStatus(account, ai, creditsAsFilesize, useOwnCredits);
        if (account.getType() == AccountType.FREE && creditsAsFilesize > 0) {
            /* Display traffic but do not care about how much is actually left. */
            ai.setSpecialTraffic(true);
        }
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
        final boolean apiTempBlocked = !StringUtils.isEmpty(apierror) && apierror.matches("(?i)Flood detected: (User|IP) Locked.*?");
        if (apiTempBlocked) {
            AccountType type = null;
            if (account.lastUpdateTime() > 0) {
                logger.info("Cannot get account details because of API limits but account has been checked before and is ok");
                // /* Return last accountInfo if available */
                // if (account.getAccountInfo() != null) {
                // logger.info("Returning last accountInfo");
                // ai = account.getAccountInfo();
                // ai.setStatus("Premium account (can't display more detailed info at this moment)");
                // return ai;
                // }
                final AccountType oldAccountType = account.getType();
                if (oldAccountType != null && oldAccountType != AccountType.UNKNOWN) {
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
                type = AccountType.PREMIUM;
            }
            account.setType(type);
            ai.setStatus(type.getLabel() + " | Can't display more detailed info at this moment due to 'API flood' -> Try account-check again later, downloads are not affected by this message!");
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
            return ai;
        }
        this.handleErrorsAPI(entries, account);
        final String email = (String) entries.get("email");
        if (!StringUtils.isEmpty(email) && !StringUtils.equals(account.getUser(), email)) {
            account.setUser(email);
        }
        final String subscription_end = (String) entries.get("subscription_end");
        final Object available_credits_in_gigabyteO = entries.get("cdn");
        double available_credits_in_gigabyte = 0;
        if (available_credits_in_gigabyteO != null) {
            if (available_credits_in_gigabyteO instanceof Number) {
                available_credits_in_gigabyte = ((Number) available_credits_in_gigabyteO).doubleValue();
            } else {
                available_credits_in_gigabyte = Double.parseDouble(available_credits_in_gigabyteO.toString());
            }
        }
        final boolean useCDNCredits = JavaScriptEngineFactory.toLong(entries.get("use_cdn"), 0) == 1 ? true : false;
        long validuntil = 0;
        if (!StringUtils.isEmpty(subscription_end)) {
            validuntil = TimeFormatter.getMilliSeconds(subscription_end, "yyyy-MM-dd HH:mm:ss", Locale.FRANCE);
        }
        if (validuntil > System.currentTimeMillis()) {
            /* Premium */
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(getMaxSimultanPremiumDownloadNum());
            account.setConcurrentUsePossible(true);
            setValidUntil(ai, validuntil);
            ai.setUnlimitedTraffic();
        } else {
            /* Free --> 2019-07-18: API Keys are only available for premium users so this should never happen! */
            ai.setExpired(true);
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(getMaxSimultanFreeDownloadNum());
            account.setConcurrentUsePossible(false);
            /*
             * Free accounts cannot be used for downloading in API mode [unless they got CDN credits and they are in use, see down below].
             */
            ai.setTrafficLeft(0);
        }
        final long available_credits_in_bytes = (long) available_credits_in_gigabyte;
        if (AccountType.FREE.equals(account.getType()) && (available_credits_in_gigabyte <= 0 || !useCDNCredits)) {
            /* Free accounts cannot be used for normal downloading via API Key */
            ai.setExpired(true);
            throw new AccountInvalidException("Free accounts cannot be used for normal downloading via API (only for CDN credits downloading)");
        }
        setCdnCreditsStatus(account, ai, available_credits_in_bytes, useCDNCredits);
        return ai;
    }

    /** Sets CDN credit status and account status text */
    private void setCdnCreditsStatus(final Account account, final AccountInfo ai, final long creditsInBytes, final boolean useCDNCredits) {
        final SIZEUNIT maxSizeUnit = (SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue();
        final String available_credits_human_readable = SIZEUNIT.formatValue(maxSizeUnit, creditsInBytes);
        String cdnCreditsStatus = "CDN credits: " + available_credits_human_readable + " | Used: ";
        if (AccountType.FREE.equals(account.getType()) && useCDNCredits) {
            cdnCreditsStatus += "Yes";
            if (account.getType() == AccountType.FREE) {
                /* Treat Free accounts like a premium account if credits are used. */
                account.setMaxSimultanDownloads(getMaxSimultanPremiumDownloadNum());
            }
            ai.setTrafficLeft(creditsInBytes);
            ai.setTrafficRefill(false);
        } else {
            cdnCreditsStatus += "No";
        }
        final String accountStatus = account.getType().getLabel() + " " + cdnCreditsStatus;
        ai.setStatus(accountStatus);
    }

    private Map<String, Object> parseAPIResponse(final Account account) throws PluginException {
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
        final Object statusO = entries.get("status");
        if (statusO == null) {
            return entries;
        } else if (!"KO".equalsIgnoreCase((String) statusO)) {
            return entries;
        }
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
            /* Free account (most likely expired premium) apikey entered by user --> API can only be used by premium users */
            throw new AccountInvalidException("Premium expired: Only premium users can use the 1fichier API");
        } else if (isAPIErrorPassword(message)) {
            /* 2021-02-10: This will usually be handled outside of this errorhandling! */
            if (this.getDownloadLink() != null && this.getDownloadLink().getDownloadPassword() != null) {
                this.getDownloadLink().setDownloadPassword(null);
                if (lastSessionPassword.get() != null && lastSessionPassword.get().equals(this.getDownloadLink().getDownloadPassword())) {
                    lastSessionPassword.set(null);
                }
            } else {
                lastSessionPassword.set(null);
            }
            if (this.getDownloadLink().isPasswordProtected()) {
                throw new PluginException(LinkStatus.ERROR_RETRY, "Password wrong!");
            } else {
                this.getDownloadLink().setPasswordProtected(true);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Password required!");
            }
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

    private static void errorInvalidAPIKey(final Account account) throws PluginException {
        if (account == null) {
            /* This should never happen */
            throw new AccountRequiredException();
        }
        /* Assume APIKey is invalid or simply not valid anymore (e.g. user disabled or changed APIKey) */
        final StringBuilder sb = new StringBuilder();
        sb.append("Invalid API Key!");
        sb.append("\r\nYou can find your API Key here: 1fichier.com/console/params.pl");
        sb.append("\r\nIf you bought a 'premium key'/'Voucher'/'Activation key' from a reseller, you first need to create a 1fichier account and redeem that code here: 1fichier.com/console/vu.pl");
        sb.append("\r\nPlease keep in mind that API Keys are only available for premium customers.");
        sb.append("\r\nInformation for 1fichier FREE-account users:");
        sb.append("\r\nTo use free accounts in JD, Go to Settings -> Plugins -> 1fichier.com -> 'Use premium API' -> Disable this -> Now try to login again, this time with your e-mail & password.");
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
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
                setBasicAuthHeader(br, account);
                if (!force) {
                    /* Do not validate cookies */
                    return;
                }
                br.getPage("https://" + this.getHost() + "/console/index.pl");
                if (isLoggedinWebsite(br)) {
                    logger.info("Cookie login successful");
                    account.saveCookies(br.getCookies(getHost()), "");
                    return;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    this.prepareBrowserWebsite(this.br);
                }
            }
            logger.info("Performing full website login");
            final String username = account.getUser();
            final String password = account.getPass();
            final UrlQuery query = new UrlQuery();
            query.add("mail", Encoding.urlEncode(username));
            query.add("pass", Encoding.urlEncode(password));
            query.add("lt", "on"); // long term session
            query.add("other", "on"); // set cookies also on other 1fichier domains
            query.add("valider", "ok");
            br.postPage("https://" + this.getHost() + "/login.pl", query);
            Form twoFAForm = null;
            final String formKey2FA = "tfa";
            final Form[] forms = br.getForms();
            for (final Form form : forms) {
                final InputField twoFAField = form.getInputField(formKey2FA);
                if (twoFAField != null) {
                    twoFAForm = form;
                    break;
                }
            }
            if (!isLoggedinWebsite(this.br) && twoFAForm != null) {
                logger.info("2FA code required");
                final String twoFACode = this.getTwoFACode(account, "\\d{6}");
                logger.info("Submitting 2FA code");
                twoFAForm.put(formKey2FA, twoFACode);
                br.submitForm(twoFAForm);
                if (!isLoggedinWebsite(this.br)) {
                    throw new AccountInvalidException(org.jdownloader.gui.translate._GUI.T.jd_gui_swing_components_AccountDialog_2FA_login_invalid());
                }
            }
            if (!isLoggedinWebsite(this.br)) {
                final String errorTooManyLoginAttempts = br.getRegex(">\\s*(More than \\d+ login try per \\d+ minutes is not allowed)").getMatch(0);
                if (errorTooManyLoginAttempts != null) {
                    throw new AccountUnavailableException(errorTooManyLoginAttempts, 1 * 60 * 1000l);
                }
                if (br.containsHTML("following many identification errors")) {
                    if (br.containsHTML("Your account will be unlock")) {
                        throw new AccountUnavailableException("Your account will be unlocked within 1 hour", 10 * 60 * 1000l);
                    } else if (br.containsHTML("your IP address") && br.containsHTML("is temporarily locked")) {
                        throw new AccountUnavailableException("For security reasons, following many identification errors, your IP address is temporarily locked.", 15 * 60 * 1000l);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
                throw new AccountInvalidException();
            }
            setBasicAuthHeader(br, account);
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
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
    protected long getStartIntervall(final DownloadLink link, final Account account) {
        if (account == null || !AccountType.PREMIUM.equals(account.getType()) || link == null) {
            return super.getStartIntervall(link, account);
        } else {
            final long knownDownloadSize = link.getKnownDownloadSize();
            if (knownDownloadSize <= 50 * 1024 * 1024) {
                final int wait = PluginJsonConfig.get(OneFichierConfigInterface.class).getSmallFilesWaitInterval();
                // avoid IP block because of too many downloads in short time
                return Math.max(0, wait * 1000);
            } else {
                return super.getStartIntervall(link, account);
            }
        }
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (AccountType.FREE.equals(account.getType())) {
            /**
             * Website mode is required for free account downloads
             */
            loginWebsite(account, false);
            doFree(account, link);
            return;
        } else {
            int maxChunks = PluginJsonConfig.get(OneFichierConfigInterface.class).getMaxPremiumChunks();
            if (maxChunks == 1) {
                maxChunks = 1;
            } else if (maxChunks < 1 || maxChunks >= 20) {
                maxChunks = 0;
            } else {
                maxChunks = -maxChunks;
            }
            logger.info("Max Chunks:" + maxChunks);
            if (!this.attemptStoredDownloadurlDownload(link, PROPERTY_PREMLINK, resume_account_premium, maxChunks)) {
                /**
                 * 2021-02-11: Don't do availablecheck in premium mode to reduce requests. </br>
                 * According to their admin, using the public availablecheck call just before downloading via API can be troublesome
                 */
                String dllink;
                if (canUseAPIForPremiumDownloads(account)) {
                    dllink = getDllinkPremiumAPI(link, account);
                } else {
                    dllink = getDllinkPremiumWebsite(link, account);
                }
                if (StringUtils.isEmpty(dllink)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                if (PluginJsonConfig.get(OneFichierConfigInterface.class).isPreferSSLEnabled()) {
                    dllink = dllink.replace("http://", "https://");
                } else {
                    dllink = dllink.replaceFirst("https://", "http://");
                }
                link.setProperty(PROPERTY_PREMLINK, dllink);
                dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, resume_account_premium, maxChunks);
                if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                    logger.warning("The final dllink seems not to be a file!");
                    br.followConnection(true);
                    errorHandlingWebsite(link, account, br);
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error", 5 * 60 * 1000l);
                }
            }
            dl.startDownload();
        }
    }

    private String getDllinkPremiumAPI(final DownloadLink link, final Account account) throws Exception {
        /**
         * 2019-04-05: At the moment there are no benefits for us when using this. </br>
         * 2021-01-29: Removed this because if login is blocked because of "flood control" this won't work either!
         */
        final boolean checkFileInfoBeforeDownloadAttempt = false;
        if (checkFileInfoBeforeDownloadAttempt) {
            requestFileInformationAPI(br, link, account);
        }
        setPremiumAPIHeaders(br, account);
        /* Do NOT trust pwProtected as this is obtained via website or old mass-linkcheck API!! */
        String dllink = null;
        synchronized (lastSessionPassword) {
            String passCode = link.getDownloadPassword();
            /* Check if we already know that this file is password protected ... */
            /** Try passwords in this order: 1. DownloadLink stored password, 2. Last used password, 3. Ask user */
            if (link.isPasswordProtected()) {
                if (passCode == null) {
                    if (lastSessionPassword.get() != null) {
                        passCode = lastSessionPassword.get();
                    } else {
                        passCode = getUserInput("Password?", link);
                    }
                }
            }
            /** Description of optional parameters: cdn=0/1 - use download-credits, */
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("url", this.getContentURL(link));
            postdata.put("pass", link.getDownloadPassword());
            postdata.put("no_ssl", PluginJsonConfig.get(OneFichierConfigInterface.class).isPreferSSLEnabled() == false ? 1 : 0);
            performAPIRequest(API_BASE + "/download/get_token.cgi", JSonStorage.serializeToJson(postdata));
            // TODO: Make use of this parsed json response down below!
            final Map<String, Object> entries = handleErrorsAPI(account);
            /* 2019-04-04: Downloadlink is officially only valid for 5 minutes */
            dllink = entries.get("url").toString();
            if (passCode != null) {
                lastSessionPassword.set(passCode);
                link.setDownloadPassword(passCode);
            } else {
                /* File is not password protected (anymore) */
                link.setPasswordProtected(false);
            }
        }
        if (StringUtils.isEmpty(dllink)) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to find final downloadurl");
        }
        return dllink;
    }

    private String getDllinkPremiumWebsite(final DownloadLink link, final Account account) throws Exception {
        String dllink = null;
        loginWebsite(account, false);
        final boolean isFollowingRedirects = br.isFollowingRedirects();
        br.setFollowRedirects(false);
        try {
            br.getPage(this.getContentURL(link));
            // error checking, offline links can happen here.
            errorHandlingWebsite(link, account, br);
            dllink = br.getRedirectLocation();
            if (this.getDownloadPasswordForm() != null) {
                handleDownloadPasswordWebsite(link);
                /*
                 * The users' 'direct download' setting has no effect on the password handling so we should always get a redirect to the
                 * final downloadlink after having entered the correct download password (for premium users).
                 */
                dllink = br.getRedirectLocation();
                if (dllink == null) {
                    dllink = br.getRegex(regex_dllink_middle).getMatch(1);
                    if (dllink == null) {
                        logger.warning("After successful password handling: Final downloadlink 'dllink' is null");
                        this.handleErrorsLastResortWebsite(link, account);
                    }
                }
            }
            ipBlockedErrorHandling(account, br);
            if (dllink == null) {
                /* The link is always SSL - based on user setting it will redirect to either https or http. */
                String postData = "did=0&";
                /* Returns postPage key + data based on the users' SSL preference. */
                if (PluginJsonConfig.get(OneFichierConfigInterface.class).isPreferSSLEnabled()) {
                    // logger.info("User prefers download with SSL");
                    postData += "dlssl=SSL+Download";
                } else {
                    logger.info("User prefers download without SSL");
                    postData += "dl=Download";
                }
                br.postPage(getContentURLWebsite(link), postData);
                dllink = br.getRedirectLocation();
                if (dllink == null) {
                    if (br.containsHTML("\">Warning \\! Without premium status, you can download only")) {
                        logger.info("Seems like this is no premium account or it's vot valid anymore -> Disabling it");
                        throw new AccountInvalidException("Account is no premium anymore");
                    } else if (br.containsHTML(">\\s*You can use your account only for downloading from") || br.containsHTML(">\\s*Our services are not compatible with massively shared internet access") || br.containsHTML(">\\s*Be carrefull? to not use simultaneously your IPv4 and IPv6 IP")) {
                        throw new AccountUnavailableException("You are trying to use this account on multiple IP addresses at once", 10 * 60 * 1000l);
                    } else {
                        logger.warning("Final downloadlink 'dllink' is null");
                        this.handleErrorsLastResortWebsite(link, account);
                    }
                }
            }
        } finally {
            br.setFollowRedirects(isFollowingRedirects);
        }
        return dllink;
    }

    private void performAPIRequest(final String url, final String json_string) throws IOException {
        final PostRequest downloadReq = br.createJSonPostRequest(url, json_string);
        downloadReq.setContentType("application/json");
        br.openRequestConnection(downloadReq);
        br.loadConnection(null);
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final String property, final boolean resume, final int maxchunks) throws Exception {
        String url = link.getStringProperty(property);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        if (PluginJsonConfig.get(OneFichierConfigInterface.class).isPreferSSLEnabled()) {
            url = url.replaceFirst("(?i)http://", "https://");
        } else {
            url = url.replaceFirst("(?i)https://", "http://");
        }
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, resume, maxchunks);
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                return true;
            } else {
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            link.removeProperty(property);
            logger.log(e);
            try {
                dl.getConnection().disconnect();
            } catch (Throwable ignore) {
            }
            return false;
        }
    }

    /** Required to authenticate via API. Wrapper for setPremiumAPIHeaders(String). */
    public static void setPremiumAPIHeaders(final Browser br, final Account account) throws PluginException {
        final String apiKey = account.getPass();
        if (apiKey == null || !looksLikeValidAPIKeySTATIC(apiKey)) {
            errorInvalidAPIKey(account);
        } else {
            setPremiumAPIHeaders(br, apiKey);
        }
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

    private void setBasicAuthHeader(final Browser br, final Account account) {
        br.getHeaders().put("Authorization", "Basic " + Encoding.Base64Encode(account.getUser() + ":" + account.getPass()));
    }

    private static AtomicReference<String> lastSessionPassword = new AtomicReference<String>(null);

    private Form getDownloadPasswordForm() throws Exception {
        final Form ret = br.getFormbyKey("pass");
        if (ret != null && this.canHandle(ret.getAction())) {
            return ret;
        } else {
            return null;
        }
    }

    @Override
    public Class<OneFichierConfigInterface> getConfigInterface() {
        return OneFichierConfigInterface.class;
    }

    private void handleDownloadPasswordWebsite(final DownloadLink link) throws Exception {
        synchronized (lastSessionPassword) {
            logger.info("Handling supposedly password protected link...");
            final Form pwform = getDownloadPasswordForm();
            /** Try passwords in this order: 1. DownloadLink stored password, 2. Last used password, 3. Ask user */
            boolean usedLastPassword = false;
            String passCode = link.getDownloadPassword();
            if (passCode == null) {
                if (lastSessionPassword.get() != null) {
                    usedLastPassword = true;
                    passCode = lastSessionPassword.get();
                } else {
                    passCode = getUserInput("Password?", link);
                }
            }
            pwform.put("pass", Encoding.urlEncode(passCode));
            /*
             * Set pw protected flag so in case this downloadlink is ever tried to be downloaded via API, we already know that it is
             * password protected!
             */
            link.setPasswordProtected(true);
            /** That is a multi purpose Form containing some default fields which we don't want or and to correct. */
            pwform.remove("save");
            pwform.put("did", "1");
            br.submitForm(pwform);
            if (getDownloadPasswordForm() != null) {
                if (usedLastPassword) {
                    lastSessionPassword.set(null);
                } else {
                    link.setDownloadPassword(null);
                }
                throw new PluginException(LinkStatus.ERROR_RETRY, "Password wrong!");
            } else {
                /* Save download-password */
                lastSessionPassword.set(passCode);
                link.setDownloadPassword(passCode);
            }
        }
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
             * 2021-02-10: Not sure - seems like thi could be multiple reasons: registered only, premium only, IP/country only or private
             * file --> Owner only. See https://1fichier.com/console/acl.pl
             */
            throw new PluginException(LinkStatus.ERROR_FATAL, "Access to this file has been restricted");
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    private final String PROPERTY_HAS_SHOWN_VPN_LOGIN_WARNING = "has_shown_vpn_login_warning";

    private void displayVPNWarning(final Account account) {
        if (account == null) {
            throw new IllegalArgumentException();
        }
        if (account.getBooleanProperty(PROPERTY_HAS_SHOWN_VPN_LOGIN_WARNING, false)) {
            /* Message has already been displayed for this account */
            return;
        }
        final Thread thread = new Thread() {
            public void run() {
                try {
                    String message = "";
                    final String title;
                    String language = System.getProperty("user.language").toLowerCase();
                    if ("de".equals(language)) {
                        title = "VPN/Proxy oder Rechenzentrum-IP erkannt!";
                        message += "Anmeldung fehlgeschlagen :(";
                        message += "\r\nEs scheint, dass Sie eine VPN/Proxy oder Rechenzentrum-IP verwenden, die laut den Nutzungsbedingungen von 1fichier nicht für Premium-Nutzer erlaubt ist";
                        message += "\r\nWas kann dagegen getan werden?";
                        message += "\r\n1. Prüfen Sie, ob Sie sich über die 1fichier-Website anmelden und herunterladen können.\r\nWenn Sie eine Fehlermeldung wie \"Must have CDN\" erhalten, wurde Ihre IP aus einem bestimmten Grund markiert.";
                        message += "\r\n2. Stellen Sie sicher, dass Sie eine normale Heim-IP verwenden, deaktivieren Sie alle Proxys/VPNs und versuchen Sie es erneut.";
                        message += "\r\n3. Wenn Sie bewusst einen VPN/Proxy/Rechenzentrum-IP verwenden, müssen Sie CDN-Guthaben kaufen und zum Herunterladen verwenden, siehe '1fichier.com/tarifs.html' -> Scrollen Sie nach unten zu 'CDN Angebot'";
                        message += "\r\n";
                        message += "\r\nWenn das Problem weiterhin besteht:";
                        message += "\r\nWenn Sie denken, dass dies ein Fehler ist, kontaktieren Sie den 1fichier Support: 1fichier.com/contact.html";
                        message += "\r\nDies ist kein JDownloader-Fehler, sondern eine serverseitige Fehlermeldung, die wir Ihnen hier einfach weiterleiten.";
                        message += "\r\nWeitere Informationen zu diesem Thema: https://support.jdownloader.org/knowledgebase/article/plugins-1fichiercom-settings-and-troubleshooting";
                    } else if ("es".equals(language)) {
                        title = "¡VPN/proxy o IP de centro de datos detectada!";
                        message += "Inicio de sesión fallido :(";
                        message += "\r\nParece que está usando una VPN/proxy o IP de centro de datos que los usuarios premium no pueden usar según los términos de servicio de 1fichier";
                        message += "\r\n¿Qué se puede hacer al respecto?";
                        message += "\r\n1. Compruebe si puede iniciar sesión y descargar a través del sitio web de 1fichier.\r\nSi recibe un mensaje de error similar a \"Must have CDN\", su IP ha sido marcada por algún motivo.";
                        message += "\r\n2. Asegúrese de usar una IP doméstica normal, desactive cualquier proxy/VPN e intente nuevamente.";
                        message += "\r\n3. Si está usando conscientemente una VPN/proxy/IP de centro de datos, debe comprar créditos CDN y usarlos para descargar, vea '1fichier.com/tarifs.html' -> Desplácese hacia abajo hasta 'Oferta CDN'";
                        message += "\r\n";
                        message += "\r\nSi el problema no desaparece:";
                        message += "\r\nSi cree que esto es un error, contacte al soporte de 1fichier: 1fichier.com/contact.html";
                        message += "\r\nEsto no es un error de JDownloader sino un mensaje de error del servidor que simplemente le reenviamos aquí.";
                        message += "\r\nMás información sobre este tema: https://support.jdownloader.org/knowledgebase/article/plugins-1fichiercom-settings-and-troubleshooting";
                    } else if ("fr".equals(language)) {
                        title = "VPN/proxy ou IP de centre de données détecté !";
                        message += "Échec de connexion :(";
                        message += "\r\nIl semble que vous utilisez un VPN/proxy ou une IP de centre de données que les utilisateurs premium ne sont pas autorisés à utiliser selon les conditions d'utilisation de 1fichier";
                        message += "\r\nQue peut-on faire à ce sujet ?";
                        message += "\r\n1. Vérifiez si vous pouvez vous connecter et télécharger via le site web 1fichier.\r\nSi vous recevez un message d'erreur similaire à \"Must have CDN\", votre IP a été signalée pour une raison quelconque.";
                        message += "\r\n2. Assurez-vous d'utiliser une IP domestique régulière, désactivez tout proxy/VPN et réessayez.";
                        message += "\r\n3. Si vous utilisez sciemment un VPN/proxy/IP de centre de données, vous devez acheter des crédits CDN et les utiliser pour télécharger, voir '1fichier.com/tarifs.html' -> Faites défiler jusqu'à 'Offre CDN'";
                        message += "\r\n";
                        message += "\r\nSi le problème ne disparaît pas :";
                        message += "\r\nSi vous pensez qu'il s'agit d'une erreur, contactez le support de 1fichier : 1fichier.com/contact.html";
                        message += "\r\nCe n'est pas un bug de JDownloader mais un message d'erreur côté serveur que nous vous transmettons simplement ici.";
                        message += "\r\nPlus d'informations sur ce sujet : https://support.jdownloader.org/knowledgebase/article/plugins-1fichiercom-settings-and-troubleshooting";
                    } else {
                        title = "VPN/proxy or datacenter IP detected!";
                        message += "Login failed :(";
                        message += "\r\nIt looks like you are using a VPN/proxy or datacenter IP which is premium users are not allowed to use according to the terms of service of 1fichier";
                        message += "\r\nWhat can be done about this?";
                        message += "\r\n1. Check if you can login and download via the 1fichier website.\r\nIf you are getting an error message similar to \"Must have CDN\", your IP was flagged for some reason.";
                        message += "\r\n2. Ensure that you are using a regular home IP, disable any proxy/VPN and try again.";
                        message += "\r\n3. If you are knowingly using a VPN/proxy/datacenter IP, you need to purchase CDN credits and use them for downloading, see '1fichier.com/tarifs.html' -> Scroll down to 'CDN Offer'";
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
        account.setProperty(PROPERTY_HAS_SHOWN_VPN_LOGIN_WARNING, true);
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