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

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
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
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.HentaiFoundryComGallery;

@HostPlugin(revision = "$Revision: 52987 $", interfaceVersion = 2, names = {}, urls = {})
@PluginDependencies(dependencies = { HentaiFoundryComGallery.class })
public class HentaiFoundryCom extends PluginForHost {
    public HentaiFoundryCom(PluginWrapper wrapper) {
        super(wrapper);
        setConfigElements();
        this.enablePremium(getBaseURL() + "/users/create");
    }

    public static List<String[]> getPluginDomains() {
        return HentaiFoundryComGallery.getPluginDomains();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_PICTURE   = Pattern.compile("/pictures/user/([A-Za-z0-9\\-_]+)/(\\d+)(/([A-Za-z0-9\\-_]+))?", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_STORY_PDF = Pattern.compile("/stories/user/([A-Za-z0-9\\-_]+)/(\\d+)/([A-Za-z0-9\\-_]+\\.pdf)", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://www\\." + buildHostsPatternPart(domains) + "/(" + PATTERN_PICTURE.pattern().substring(1) + "|" + PATTERN_STORY_PDF.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    private String getBaseURL() {
        return "https://www." + getHost();
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX, LazyPlugin.FEATURE.IMAGE_HOST, LazyPlugin.FEATURE.IMAGE_GALLERY };
    }

    private String dllink = null;

    @Override
    public String getAGBLink() {
        return getBaseURL() + "/";
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "Filename_id", "Choose file name + id?").setDefaultValue(false));
    }

    public static String getFID(final String url) {
        return new Regex(url, "(?i)/user/[A-Za-z0-9\\-_]+/(\\d+)").getMatch(0);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link.getPluginPatternMatcher());
        if (fid != null) {
            return getHost() + "://" + fid;
        }
        return super.getLinkID(link);
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        if (PATTERN_STORY_PDF.matcher(link.getPluginPatternMatcher()).find()) {
            return getFID(link.getPluginPatternMatcher()) + ".pdf";
        } else {
            return getFID(link.getPluginPatternMatcher()) + ".jpg";
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, null);
    }

    @SuppressWarnings("deprecation")
    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        dllink = null;
        String title = null;
        String ext = null;
        final String fid = getFID(link.getPluginPatternMatcher());
        br.setFollowRedirects(true);
        if (account != null) {
            login(account, false);
        }
        if (PATTERN_STORY_PDF.matcher(link.getPluginPatternMatcher()).find()) {
            dllink = link.getDownloadURL() + "?enterAgree=1&size=0";
            ext = ".pdf";
            title = fid + "_" + new Regex(link.getDownloadURL(), "([A-Za-z0-9\\-_]+\\.pdf)$").getMatch(0);
        } else {
            br.getPage(link.getDownloadURL() + "?enterAgree=1&size=0");
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            title = new Regex(br.getURL(), PATTERN_PICTURE).getMatch(3);
            dllink = br.getRegex("\"(//pictures\\.hentai-foundry\\.com/{1,}/[^\"]+)\"").getMatch(0);
            if (title == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            if (dllink != null) {
                dllink = Request.getLocation(Encoding.htmlDecode(dllink), br.getRequest());
            }
            if (getPluginConfig().getBooleanProperty("Filename_id", true)) {
                title = Encoding.htmlDecode(title) + "_" + fid;
            } else {
                title = fid + "_" + Encoding.htmlDecode(title);
            }
            title = title.trim();
        }
        if (ext == null && dllink != null) {
            ext = getFileNameExtensionFromString(dllink, ".png");
            /*
             * 2017-01-30: Fallback for some pictures - their urls end with "." and they do not even have an extensions via browser -->
             * Usually these are .jpg files.
             */
            if (!ext.matches("\\.[A-Za-z]{3,5}")) {
                ext = ".jpg";
            }
        } else if (ext == null) {
            ext = ".jpg";
        }
        if (title != null) {
            link.setFinalFileName(this.applyFilenameExtension(title, ext));
        }
        if (!StringUtils.isEmpty(dllink) && !link.isSizeSet()) {
            basicLinkCheck(br.cloneBrowser(), br.createHeadRequest(dllink), link, title, ext);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownload(link, null);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link, account);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, false, 1);
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 5;
    }

    public void login(final Account account, final boolean validate) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                br.setCookies(cookies);
                if (!validate) {
                    return;
                }
                br.getPage("https://www." + getHost() + "/");
                if (this.isLoggedin(br)) {
                    logger.info("Cookie login successful");
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return;
                }
                logger.info("Cookie login failed");
            }
            br.setFollowRedirects(true);
            br.getPage("https://www." + getHost() + "/site/login?enterAgree=1&size=0");
            final Form loginform = br.getFormbyActionRegex("/site/login.*");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            loginform.put("LoginForm[username]", Encoding.urlEncode(account.getUser()));
            loginform.put("LoginForm[password]", Encoding.urlEncode(account.getPass()));
            loginform.put("LoginForm[rememberMe]", "1");
            br.submitForm(loginform);
            if (!this.isLoggedin(br)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("site/logout'>\\s*Logout\\s*</a>");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        ai.setUnlimitedTraffic();
        account.setType(AccountType.FREE);
        /* free accounts can still have captcha */
        account.setMaxSimultanDownloads(this.getMaxSimultanFreeDownloadNum());
        account.setConcurrentUsePossible(false);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }
}
