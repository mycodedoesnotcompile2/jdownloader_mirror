//jDownloader - Downloadmanager
//Copyright (C) 2017  JD-Team support@jdownloader.org
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
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
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

@HostPlugin(revision = "$Revision: 51437 $", interfaceVersion = 3, names = {}, urls = {})
public class ModelKarteiDe extends PluginForHost {
    public ModelKarteiDe(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/vip/");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX, LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        for (final String[] domains : getPluginDomains()) {
            for (final String domain : domains) {
                br.setCookie(domain, "mk4_language", "2"); // English
            }
        }
        return br;
    }

    private String dllink = null;

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if (isVideo(link)) {
            return -5;
        } else {
            /* 2024-06-04: Set to 1 as we are only downloading small files. */
            return 1;
        }
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "model-kartei.de" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static Pattern TYPE_PHOTO = Pattern.compile("/(?:fotos|photos)/(?:foto|photo)/(\\d+)/?", Pattern.CASE_INSENSITIVE);
    private static Pattern TYPE_VIDEO = Pattern.compile("/videos?/video/(\\d+)/?", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_PHOTO.pattern() + "|" + TYPE_VIDEO.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/agb/";
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

    public String getFID(final DownloadLink link) {
        String fid = new Regex(link.getPluginPatternMatcher(), TYPE_PHOTO).getMatch(0);
        if (fid == null) {
            fid = new Regex(link.getPluginPatternMatcher(), TYPE_VIDEO).getMatch(0);
        }
        return fid;
    }

    private boolean isVideo(final DownloadLink link) {
        if (new Regex(link.getPluginPatternMatcher(), TYPE_VIDEO).patternFind()) {
            return true;
        } else {
            return false;
        }
    }

    public String getExtDefault(final DownloadLink link) {
        if (isVideo(link)) {
            /* Can be flv, webm, mp4 but mostly mp4 */
            return ".mp4";
        } else {
            return ".jpg";
        }
    }

    public void setWeakFilename(final DownloadLink link) {
        final String extDefault;
        if (isVideo(link)) {
            /* Can be flv, webm, mp4 but mostly mp4 */
            extDefault = ".mp4";
        } else {
            extDefault = ".jpg";
        }
        final String contentID = this.getFID(link);
        if (!link.isNameSet()) {
            /* Set weak filename */
            link.setName(contentID + extDefault);
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, null, false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        dllink = null;
        final String extDefault = getExtDefault(link);
        final String contentID = this.getFID(link);
        if (!link.isNameSet()) {
            setWeakFilename(link);
        }
        this.setBrowserExclusive();
        if (account != null) {
            this.login(account, false);
        }
        getPageEnsureEnglish(br, link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*The video does not exist")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*The video is not active")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("class=\"p-title\">\\s*<a href=\"[^\"]+\"[^<]*title=\"([^\"]+)\"").getMatch(0); // photo
        if (title == null) {
            // video
            title = br.getRegex("class=\"col c-3-2 vidHeader\"[^>]*>\\s*<h1>([^<]+)</h1>").getMatch(0);
        }
        if (title != null) {
            title = Encoding.htmlOnlyDecode(title).trim();
        } else {
            logger.warning("Failed to find title");
        }
        final String dateStr = br.getRegex("(\\d{8})/\">\\s*<span[^>]*class=\"date\"").getMatch(0);
        String dateFormatted = null;
        if (dateStr != null) {
            dateFormatted = dateStr.substring(0, 4) + "_" + dateStr.substring(4, 6) + "_" + dateStr.substring(6, 8);
        } else {
            logger.warning("Failed to find date in html code");
        }
        dllink = br.getRegex("id=\"gofullscreen\"[^<]*src=\"([^\"]+)").getMatch(0); // photo
        if (dllink == null) {
            dllink = br.getRegex("type=\"video/mp4\"[^>]*src=\"(https?://[^\"]+)").getMatch(0); // video MP4
            if (dllink == null) {
                dllink = br.getRegex("type=\"video/webm\"[^>]*src=\"(https?://[^\"]+)").getMatch(0); // video WEBM
                if (dllink == null) {
                    /* Super old flash player .flv videos e.g. /videos/video/1370/ */
                    dllink = br.getRegex("\"(https?://[^/]+/[^\"]+\\.flv)\"").getMatch(0); // video FLV
                }
            }
        }
        String ext = extDefault;
        if (dllink != null) {
            ext = getFileNameExtensionFromURL(dllink, extDefault);
        }
        try {
            findDateViaHeader: if (!StringUtils.isEmpty(dllink) && (isDownload == false || dateFormatted == null)) {
                /* Find file size */
                final URLConnectionAdapter con = basicLinkCheck(br.cloneBrowser(), br.createHeadRequest(dllink), link, null, null);
                final String lastModifiedHeader = con.getHeaderField(HTTPConstants.HEADER_RESPONSE_LAST_MODFIED);
                if (StringUtils.isEmpty(lastModifiedHeader)) {
                    logger.warning("Last modified header is missing");
                    break findDateViaHeader;
                }
                final Date lastModifiedDate = TimeFormatter.parseDateString(lastModifiedHeader);
                dateFormatted = new SimpleDateFormat("yyyy-MM-dd").format(lastModifiedDate);
                /* Now that the connection has been opened, we can also obtain the */
                final String extensionFromMimeType = Plugin.getExtensionFromMimeTypeStatic(con.getContentType());
                if (extensionFromMimeType != null) {
                    ext = extensionFromMimeType;
                }
            }
        } finally {
            /* Always set filename, even if exception has happened */
            if (!ext.startsWith(".")) {
                ext = "." + ext;
            }
            if (dateFormatted != null && title != null) {
                link.setFinalFileName(dateFormatted + "_" + contentID + "_" + title + ext);
            } else if (title != null) {
                link.setFinalFileName(contentID + "_" + title + ext);
            } else if (dateFormatted != null) {
                link.setFinalFileName(dateFormatted + "_" + contentID + ext);
            } else {
                link.setFinalFileName(contentID + ext);
            }
        }
        return AvailableStatus.TRUE;
    }

    public void getPageEnsureEnglish(final Browser br, final String url) throws IOException {
        /* Ensure English language */
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_REFERER, url);
        final String targetPath = new URL(url).getPath();
        /**
         * Setting only the English language cookie wasn't enough so this should redirect us to our target-URL and enforce English language.
         */
        br.getPage("https://www." + getHost() + "/l.php?l=en");
        /* Double-check: If we're not on our target-URL, navigate to it. */
        if (!br.getURL().endsWith(targetPath)) {
            logger.warning("Expected redirect from language switcher to final URL did not happen");
            br.getPage(url);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownload(link, null);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload(link, account);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link, account, true);
        if (br.containsHTML("assets/images/no\\.jpg")) {
            /* Account needed to view this image */
            throw new AccountRequiredException("Account needed to download this image");
        } else if (br.containsHTML(">\\s*(Your are not authorized to see the video|Deine Nutzerrechte reichen leider nicht aus um das Video zu sehen)")) {
            /* Account needed to view this video */
            throw new AccountRequiredException("Account needed to download this video");
        } else if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(this.br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    public boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            final Cookies storedCookies = account.loadCookies("");
            final Cookies userCookies = account.loadUserCookies();
            final Cookies cookiesToUse;
            if (userCookies != null) {
                cookiesToUse = userCookies;
            } else {
                cookiesToUse = storedCookies;
            }
            if (cookiesToUse != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookiesToUse);
                if (!force) {
                    /* Don't validate cookies */
                    return false;
                }
                br.getPage("https://www." + this.getHost());
                if (this.isLoggedin(br)) {
                    logger.info("Cookie login successful");
                    if (userCookies != null) {
                        /* User can put anything into the username field but we are trying to find the 100% correct value in html code. */
                        final String username = br.getRegex("class=\"hide4\"[^>]*>([^<]+)</span>").getMatch(0);
                        if (username != null) {
                            account.setUser(Encoding.htmlDecode(username).trim());
                        } else {
                            logger.warning("Failed to find username");
                        }
                    } else {
                        /* Save cookies */
                        account.saveCookies(br.getCookies(br.getHost()), "");
                    }
                    return true;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                }
                if (userCookies != null) {
                    /* Dead end */
                    if (account.hasEverBeenValid()) {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                    } else {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                    }
                }
            }
            logger.info("Performing full login");
            br.getPage("https://login." + getHost());
            final Form loginform = br.getFormbyActionRegex(".*/in/?$");
            if (loginform == null) {
                logger.warning("Failed to find loginform");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            loginform.put("lID", Encoding.urlEncode(account.getUser()));
            loginform.put("lPW", Encoding.urlEncode(account.getPass()));
            loginform.put("stay", "1");
            loginform.remove("goto");
            loginform.remove("goto");
            loginform.put("goto", "1");
            br.submitForm(loginform);
            if (!isLoggedin(br)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
            return true;
        }
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("class=\"logout\"");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        ai.setUnlimitedTraffic();
        if (br.containsHTML("VIP-Account")) {
            /* Premium accounts do not have an expire date. */
            account.setType(AccountType.PREMIUM);
            ai.setStatus("VIP-Account");
        } else {
            account.setType(AccountType.FREE);
        }
        return ai;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}