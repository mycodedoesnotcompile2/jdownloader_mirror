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
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.config.GelbooruComConfig;
import org.jdownloader.plugins.components.config.GelbooruComConfig.FilenameScheme;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.SiteType.SiteTemplate;

@HostPlugin(revision = "$Revision: 51051 $", interfaceVersion = 3, names = {}, urls = {})
public class GelbooruCom extends PluginForHost {
    public GelbooruCom(PluginWrapper wrapper) {
        super(wrapper);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            this.enablePremium("https://" + getHost() + "/index.php?page=account&s=reg");
        }
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setCookie(getHost(), "fringeBenefits", "yup");
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY, LazyPlugin.FEATURE.IMAGE_HOST };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "gelbooru.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/index\\.php\\?page=post\\&s=view\\&id=(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    private String dllink = null;

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/tos.php";
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
        return false;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return requestFileInformation(link, false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws IOException, PluginException {
        dllink = null;
        this.setBrowserExclusive();
        final String extDefault = ".jpg";
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            link.setName(fid + extDefault);
        }
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.br.getURL().contains(fid)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("(?i)<title>([^<>\"]+) - Image View -.*?</title>").getMatch(0);
        if (title != null && false) {
            // filename can be too long
            title = fid + "_" + title;
        } else {
            title = fid;
        }
        dllink = br.getRegex("<a href=\"([^<>\"\\']+)\"[^<>]+>Original image</a>").getMatch(0);
        if (dllink == null) {
            dllink = br.getRegex("\"(https?[^<>\"]+)\" id=\"image\"").getMatch(0);
            if (dllink == null) {
                dllink = br.getRegex("(gelbooru\\.com//images/[^<>\"]+)\"").getMatch(0);
            }
            if (dllink == null) {
                /* 2017-02-18 */
                String imglink = br.getRegex("Resize image.*?<a href=(\"|'|)(.*?)\\1").getMatch(1);
                if (imglink == null) {
                    imglink = br.getRegex("<img alt=.*?src=(\"|'|)(.*?)\\1").getMatch(1);
                }
            }
            if (dllink == null) {
                // can be a video!
                dllink = br.getRegex("<\\s*source\\s+[^>]*src\\s*=\\s*(\"|'|)(.*?)\\1").getMatch(1);
            }
        }
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
        }
        final GelbooruComConfig cfg = PluginJsonConfig.get(GelbooruComConfig.class);
        final FilenameScheme scheme = cfg.getPreferredFilenameScheme();
        final String originalFilename = dllink != null ? getFileNameFromURL(new URL(dllink)) : null;
        String ext = extDefault;
        if (scheme == FilenameScheme.SERVER_FILENAME && originalFilename != null) {
            link.setFinalFileName(originalFilename);
        } else if (title != null) {
            ext = getFileNameExtensionFromString(dllink, ext);
            link.setFinalFileName(this.applyFilenameExtension(title, ext));
        }
        if (!StringUtils.isEmpty(dllink) && !isDownload) {
            dllink = Encoding.htmlOnlyDecode(dllink);
            dllink = br.getURL(dllink).toExternalForm();
            if (!isDownload) {
                basicLinkCheck(br.cloneBrowser(), br.createHeadRequest(dllink), link, scheme == FilenameScheme.SERVER_FILENAME && originalFilename != null ? originalFilename : title, ext);
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), 1);
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    public boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final String path_account_overview = "/index.php?page=account&s=home";
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
                if (!force) {
                    /* Don't validate cookies */
                    return false;
                }
                br.getPage("https://" + this.getHost() + path_account_overview);
                if (this.isLoggedin(br)) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return true;
                } else {
                    logger.info("Cookie login failed");
                }
            }
            logger.info("Performing full login");
            br.getPage("https://" + this.getHost() + "/index.php?page=account&s=login&code=00");
            final Form loginform = br.getFormbyKey("user");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find loginform");
            }
            loginform.put("user", Encoding.urlEncode(account.getUser()));
            loginform.put("pass", Encoding.urlEncode(account.getPass()));
            br.submitForm(loginform);
            if (!isLoggedin(br)) {
                throw new AccountInvalidException();
            }
            // br.getPage(path_account_overview);
            account.saveCookies(br.getCookies(br.getHost()), "");
            return true;
        }
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("code=01\"|>\\s*Logout\\s*</a>");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        final AccountInfo ai = new AccountInfo();
        // final String user_id = br.getCookie(br.getHost(), "user_id", Cookies.NOTDELETEDPATTERN);
        /* Detailed account information can be found here: https://gelbooru.com/index.php?page=account&s=profile&id=<user_id> */
        account.setType(AccountType.FREE);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        /* TODO: 2025-05-07: Check if login is needed for downloading. Afaik it is only needed for crawling. */
        this.handleFree(link);
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
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.Danbooru;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
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

    @Override
    public Class<? extends GelbooruComConfig> getConfigInterface() {
        return GelbooruComConfig.class;
    }
}
