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
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.UpdateRequiredClassNotFoundException;

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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.SiteType.SiteTemplate;

@HostPlugin(revision = "$Revision: 51351 $", interfaceVersion = 2, names = {}, urls = {})
public class OneHundretSixteenPanCom extends PluginForHost {
    public OneHundretSixteenPanCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/vip.php");
    }

    private static final boolean SKIP_FREE_DOWNLOAD_CAPTCHA = true;

    @Override
    public String getAGBLink() {
        return "https://www." + this.getHost() + "/terms.html";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if (account != null && AccountType.PREMIUM.equals(account.getType())) {
            return 0;
        } else {
            return -2;
        }
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "116pan.com" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:download|viewfile)\\.php\\?file_id=(\\d+)");
        }
        return ret.toArray(new String[0]);
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

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            link.setName(fid);
        }
        br.getPage("https://www." + getHost() + "/viewfile.php?file_id=" + fid);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*文件不存在或已删除")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("<h1>([^<]+)</h1>").getMatch(0);
        String filesize = br.getRegex(">\\s*文件大小：([^<]+)").getMatch(0);
        if (br.getHost().equals("116pan.com")) {
            /**
             * 2025-07-15: Some migration helper code for migration from 116pan.com to 116pan.xyz <br>
             * See: https://board.jdownloader.org/showthread.php?t=96948
             */
            final String newLink = br.getRegex("window\\.location\\.href = '(https?://(?:www\\.)?116pan\\.xyz/f/[a-zA-Z0-9]+)';").getMatch(0);
            if (newLink != null) {
                if (filesize == null) {
                    /* Grab filesize from html tag "keywords" or "description" */
                    filesize = br.getRegex("(\\d+.\\d{1,2} M)").getMatch(0);
                }
                if (filename == null) {
                    /* Grab filename from html tag "description" */
                    filename = br.getRegex("name=\"description\" content=\"([^\"]+)免费高速网盘下载，").getMatch(0);
                }
                // if (StringUtils.isEmpty(link.getComment())) {
                // link.setComment("New link: " + newLink);
                // }
                if (this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD) {
                    // throw new PluginException(LinkStatus.ERROR_FATAL, "Unsupported redirect to new system: " + newLink + " -> Manually
                    // re-add this link to JDownloader to be able to download this file");
                    try {
                        final PluginForHost newPlugin = getNewPluginForHostInstance("116pan.xyz").getLazyP().getPrototype(null);
                        link.setDefaultPlugin(newPlugin);
                        link.setHost(newPlugin.getHost());
                        link.setPluginPatternMatcher(newLink);
                        link.setDomainInfo(null);
                        throw new PluginException(LinkStatus.ERROR_RETRY, "Retry link that has been migrated from 116pan.com to 116pan.xyz");
                    } catch (UpdateRequiredClassNotFoundException e) {
                        getLogger().log(e);
                    }
                }
            }
        }
        if (filename != null) {
            link.setName(Encoding.htmlDecode(filename).trim());
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize + "b"));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String directlinkproperty;
        if (account != null) {
            directlinkproperty = "directurl_account_" + account.getType();
        } else {
            directlinkproperty = "directurl_free";
        }
        String dllink = checkDirectLink(link, directlinkproperty);
        if (dllink == null) {
            requestFileInformation(link);
            final String fid = this.getFID(link);
            final String downlink = br.getRegex("(download\\.php\\?file_id=" + fid + ")").getMatch(0);
            if (downlink != null) {
                /* 2025-01-17: Wait time is skippable */
                final boolean skipWait = true;
                if (!skipWait) {
                    final String waitSecondsStr = br.getRegex("var secs = (\\d+);").getMatch(0);
                    if (waitSecondsStr != null) {
                        this.sleep(Integer.parseInt(waitSecondsStr) * 1000l, link);
                    } else {
                        logger.info("Failed to find pre download wait time");
                    }
                }
                br.getPage(downlink);
                if (StringUtils.containsIgnoreCase(br.getURL(), "action=login")) {
                    /* Free or premium account needed to be able to download this file. */
                    throw new AccountRequiredException();
                }
            }
            final boolean isPremium = account != null && AccountType.PREMIUM.equals(account.getType());
            final Browser ajax = br.cloneBrowser();
            ajax.getHeaders().put("X-Requested-With", "XMLHttpRequest");
            final String code;
            if (isPremium || SKIP_FREE_DOWNLOAD_CAPTCHA) {
                /* No captcha required */
                code = "";
            } else {
                /* Captcha required */
                code = getCaptchaCode("/imagecode.php?t=" + System.currentTimeMillis(), link);
            }
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("action", "check_code");
            query.appendEncoded("code", code);
            query.appendEncoded("file_id", fid);
            br.postPage("/ajax.php", query);
            if (StringUtils.equalsIgnoreCase(br.getRequest().getHtmlCode(), "false")) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
            dllink = br.getRegex("down_process2\\('\\d+',\\s*'(https?://[^\\']+)").getMatch(0);
            if (dllink == null) {
                /* 2025-02-13 */
                dllink = br.getRegex("href=\"(http[^\"]+)\"[^>]*down_process2").getMatch(0);
            }
            if (dllink == null) {
                dllink = br.getRegex("true\\|(http[^<>\"]+)").getMatch(0);
            }
            if (dllink == null) {
                if (br.containsHTML("vip\\.php")) {
                    // throw new AccountRequiredException();
                    /* 2023-12-22: New */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Free download for this file is not possible at this moment", 5 * 60 * 1000l);
                } else if (br.containsHTML(">\\s*抱歉，您的帐号权限当天请求数已满")) {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Daily downloadlimit reached or account required to download");
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            } else if (StringUtils.contains(dllink, "account.php") || StringUtils.contains(dllink, "vip.php")) {
                throw new AccountRequiredException();
            }
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        this.handleConnectionErrors(br, dl.getConnection());
        link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        dl.startDownload();
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        String dllink = link.getStringProperty(property);
        if (dllink == null) {
            return null;
        }
        URLConnectionAdapter con = null;
        try {
            final Browser br2 = br.cloneBrowser();
            br2.setFollowRedirects(true);
            con = br2.openHeadConnection(dllink);
            if (this.looksLikeDownloadableContent(con)) {
                return dllink;
            }
        } catch (final Exception e) {
            logger.log(e);
            return null;
        } finally {
            if (con != null) {
                con.disconnect();
            }
        }
        return null;
    }

    private boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            /*
             * 2021-11-09: Warning: They only allow one active session per account so user logging in via browser may end JDs session!
             */
            final String url_relative_account_info = "/mydisk.php?item=profile&menu=cp";
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
                if (!force) {
                    /* Do not verify cookies */
                    return false;
                }
                br.getPage("https://www." + this.getHost() + url_relative_account_info);
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
            br.getPage("https://www." + this.getHost() + "/account.php?action=login");
            final Form loginform = br.getFormbyProperty("name", "login_form");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find loginform");
            }
            final String captchaFieldKey = "verycode";
            if (loginform.hasInputFieldByName(captchaFieldKey)) {
                final DownloadLink dummyLink = new DownloadLink(this, "Account", account.getHoster(), "https://" + account.getHoster(), true);
                final String code = getCaptchaCode("/includes/imgcode.inc.php?verycode_type=2&t=0." + System.currentTimeMillis(), dummyLink);
                loginform.put(captchaFieldKey, Encoding.urlEncode(code));
            }
            loginform.put("username", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            loginform.put("remember", "1");
            br.submitForm(loginform);
            br.getPage(url_relative_account_info);
            if (!isLoggedin(br)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
            return true;
        }
    }

    private boolean isLoggedin(final Browser br) {
        if (br.containsHTML("action=logout")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        ai.setUnlimitedTraffic();
        final String premiumExpire = br.getRegex("VIP结束时间\\s*</b>[^<]*<span[^>]*>(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        final boolean premiumExpired = br.containsHTML("/no_vip\\.gif");
        if (premiumExpire == null || premiumExpired) {
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(this.getMaxSimultanPremiumDownloadNum());
        } else {
            account.setType(AccountType.PREMIUM);
            ai.setValidUntil(TimeFormatter.getMilliSeconds(premiumExpire, "yyyy-MM-dd", Locale.ENGLISH), br);
            account.setMaxSimultanDownloads(this.getMaxSimultanFreeDownloadNum());
            account.setConcurrentUsePossible(true);
        }
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        login(account, false);
        this.handleDownload(link, account);
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.PhpDisk;
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        if (acc != null && AccountType.PREMIUM.equals(acc.getType())) {
            return false;
        } else if (SKIP_FREE_DOWNLOAD_CAPTCHA) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}