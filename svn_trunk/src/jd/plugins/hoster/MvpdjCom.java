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
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.AccountController;
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
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 52241 $", interfaceVersion = 3, names = {}, urls = {})
public class MvpdjCom extends PluginForHost {
    public MvpdjCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/user/register");
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/about";
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.AUDIO_STREAMING };
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "dj1234.com", "mvpdj.com", "mvpdj.cn" });
        return ret;
    }

    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("mvpdj.cn");
        deadDomains.add("mvpdj.com"); // 2026-02-03
        return deadDomains;
    }

    private String getContenturl(final DownloadLink link) {
        // String url = link.getPluginPatternMatcher();
        // for (final String deaddomain : this.getDeadDomains()) {
        // url = url.replace(deaddomain, this.getHost());
        // }
        // return url;
        /* Old url-formats are not supported anymore but old file-ids may still be online. */
        return "https://www." + getHost() + "/music/info-" + this.getFID(link);
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2026-02-03: Main domain has changed from mvpdj.com to dj1234.com */
        return this.rewriteHost(getPluginDomains(), host);
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_SONG_OLD = Pattern.compile("/song/player/(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_SONG_NEW = Pattern.compile("/music/info-(\\d+)", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_SONG_OLD.pattern() + "|" + PATTERN_SONG_NEW.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return "mvpdj_com://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        String fid = new Regex(link.getPluginPatternMatcher(), PATTERN_SONG_NEW).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), PATTERN_SONG_OLD).getMatch(0);
        return fid;
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private String dllink = null;

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dllink = null;
        final String extDefault = ".mp3";
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            link.setName(fid + extDefault);
        }
        boolean loggedIN = false;
        final Account aa = AccountController.getInstance().getValidAccount(this);
        if (aa != null) {
            this.login(aa, false);
            loggedIN = true;
        }
        String title = null;
        String filesizeStr = null;
        if (loggedIN) {
            /* Download via download button --> Higher quality */
            br.postPage("https://www." + getHost() + "/song/download", "id=" + fid);
            title = br.getRegex("/info-" + fid + "\">([^<]+)</a>").getMatch(0);
            if (br.containsHTML(">\\s*账户余额不足，请先充值")) {
                /*
                 * Hmm something like "No traffic left" --> But let's not temp-disable the account - let's simply download the stream then!
                 */
                logger.info("Account traffic exhausted or track not downloadable!");
            } else {
                /* Number at the end seems to be a server/mirror number. Possibilities: 1,2 */
                logger.info("Track should be downloadable fine via account");
                dllink = "https://www." + getHost() + "/song/purchase/" + fid + "/2";
            }
            if (title != null) {
                title = Encoding.htmlDecode(title).trim();
            }
        }
        if (dllink == null) {
            if (loggedIN) {
                logger.info("Official track download via account failed --> Trying stream download");
            } else {
                logger.info("Trying stream download");
            }
            br.getPage(getContenturl(link));
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (!br.containsHTML("data-id=\"" + fid)) {
                /* E.g. https://www.mvpdj.com/song/player/111222333 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            if (title == null) {
                title = br.getRegex("/info-" + fid + "\">([^<]+)</a>").getMatch(0);
            }
            if (filesizeStr == null) {
                filesizeStr = br.getRegex("class=\"iconfont icon-file [^\"]+\"></i>([^<]+)</span>").getMatch(0);
            }
            dllink = PluginJSonUtils.getJsonValue(br, "url");
            if (StringUtils.isEmpty(dllink)) {
                /* 2023-01-23 */
                dllink = br.getRegex("<audio[^<]*src=\"([^\"]+)").getMatch(0);
            }
        }
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            final String ext = Plugin.getFileNameExtensionFromURL(dllink, extDefault);
            link.setFinalFileName(this.applyFilenameExtension(title, ext));
        }
        if (filesizeStr != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
        }
        if (dllink != null) {
            dllink = br.getURL(dllink).toExternalForm();
            if (this.getPluginEnvironment() != PluginEnvironment.DOWNLOAD && !link.isSizeSet()) {
                /* Find file size */
                URLConnectionAdapter con = null;
                try {
                    con = br.openHeadConnection(dllink);
                    handleConnectionErrors(br, con);
                    /* Especially for official account-downloads, server-filenames might be crippled! */
                    final String filename_connection = getFileNameFromConnection(con);
                    if (filename_connection != null) {
                        link.setFinalFileName(filename_connection);
                    }
                    if (con.getCompleteContentLength() > 0) {
                        if (con.isContentDecoded()) {
                            link.setDownloadSize(con.getCompleteContentLength());
                        } else {
                            link.setVerifiedFileSize(con.getCompleteContentLength());
                        }
                    }
                } finally {
                    try {
                        con.disconnect();
                    } catch (Throwable e) {
                    }
                }
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownload(link);
    }

    public void handleDownload(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (StringUtils.isEmpty(this.dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        ai.setUnlimitedTraffic();
        account.setType(AccountType.FREE);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link);
        /* No need to log in as we're already logged in */
        handleDownload(link);
    }

    private void login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                br.setCookies(this.getHost(), cookies);
                br.getPage("https://www." + this.getHost() + "/user/useraccount");
                if (!br.toString().equals("0")) {
                    logger.info("Cookie login successful");
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    account.clearCookies("");
                }
            }
            logger.info("Performing full login");
            br.getPage("https://www." + this.getHost() + "/user/login");
            final Form loginform = br.getFormbyActionRegex(".*login/save");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            loginform.put("username", Encoding.urlEncode(account.getUser()));
            loginform.put("userpass", Encoding.urlEncode(account.getPass()));
            /* 2024-04-08: Captcha not needed anymore */
            // final DownloadLink dummyLink = new DownloadLink(this, "Account", this.getHost(), "https://" + this.getHost(), true);
            // final String code = this.getCaptchaCode("https://www.mvpdj.com/captcha/number2.php", dummyLink);
            // br.postPage("/user/useraccount", "");
            br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
            br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            br.getHeaders().put("Referer", "https://www." + this.getHost() + "/");
            br.getHeaders().put("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
            br.submitForm(loginform);
            final String statuscode = PluginJSonUtils.getJsonValue(br, "code");
            if (!"200".equals(statuscode)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
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
}
