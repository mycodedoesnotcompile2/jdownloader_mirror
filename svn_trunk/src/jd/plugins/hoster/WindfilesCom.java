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

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;

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
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51437 $", interfaceVersion = 3, names = {}, urls = {})
public class WindfilesCom extends PluginForHost {
    public WindfilesCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/vip/get");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookie(getHost(), "lang", "us_en");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/services/agreement";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "windfiles.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/share/([a-z0-9]{14})");
        }
        return ret.toArray(new String[0]);
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
        return 0;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, null);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(this.getFID(link));
        }
        this.setBrowserExclusive();
        if (account != null) {
            this.login(account, false);
        }
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*分享不存在")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex(">\\s*(?:文件名称|File Name)\\s*</th>\\s*<th class=\"encn\"[^>]*>([^<]+)</th>").getMatch(0);
        String filesize = br.getRegex("(?:文件大小|File Size)\\s*</th>\\s*<th>([^<]+)</th>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            filename = decodeString(filename);
            link.setName(filename);
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String directlinkproperty = "directurl";
        if (!attemptStoredDownloadurlDownload(link, directlinkproperty)) {
            requestFileInformation(link, account);
            final String waitSecondsStr = br.getRegex("const counterTime = (\\d+);").getMatch(0);
            final String nextStep = br.getRegex("\"(/download/slow/[^\"]+)").getMatch(0);
            if (StringUtils.isEmpty(nextStep)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final boolean skipWait = true;
            if (!skipWait) {
                this.sleep(Integer.parseInt(waitSecondsStr), link);
            }
            br.getPage(nextStep);
            final Regex ipWait = br.getRegex(">(\\d+) Mins? (\\d+) Second");
            if (ipWait.patternFind()) {
                final int min = Integer.parseInt(ipWait.getMatch(0));
                final int sec = Integer.parseInt(ipWait.getMatch(1));
                final long waitMillis = ((min * 60) + sec) * 1000;
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, waitMillis);
            }
            final Form dlform = br.getFormbyActionRegex("/download/jump");
            if (dlform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dlform, this.isResumeable(link, account), this.getMaxChunks(link, account));
            this.handleConnectionErrors(br, dl.getConnection());
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    protected void handleConnectionErrors(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            final String ipLimit = br.getRegex("(You have downloaded.*?)</h5>\\s*</div>").getMatch(0);
            if (ipLimit != null) {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, ipLimit);
            }
            throwConnectionExceptions(br, con);
            throwFinalConnectionException(br, con);
        }
    }

    /* See https://windfiles.com/static/js/notice.js?v=0.3.5 */
    public static String decodeString(String str) {
        try {
            if (StringUtils.isEmpty(str)) {
                return "";
            }
            // Define the key for shifting
            int key = 3;
            // Decode from Base64
            String decoded = Encoding.Base64Decode(str);
            // Apply character shift
            StringBuilder result = new StringBuilder();
            for (int i = 0; i < decoded.length(); i++) {
                result.append((char) (decoded.charAt(i) - key));
            }
            return result.toString();
        } catch (Exception e) {
            return str;
        }
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final String directlinkproperty) throws Exception {
        final String url = link.getStringProperty(directlinkproperty);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, isResumeable(link, null), getMaxChunks(link, null));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                return true;
            } else {
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            logger.log(e);
            try {
                dl.getConnection().disconnect();
            } catch (Throwable ignore) {
            }
            return false;
        }
    }

    private boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final String path_account_info = "/user/my";
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
                if (!force) {
                    /* Don't validate cookies */
                    return false;
                }
                br.getPage("https://" + this.getHost() + path_account_info);
                if (this.isLoggedin(br)) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return true;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                }
            }
            logger.info("Performing full login");
            br.getPage("https://" + this.getHost() + "/user/login");
            final Form loginform = br.getFormbyActionRegex("user/login");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find loginform");
            }
            loginform.put("email", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            br.submitForm(loginform);
            if (!isLoggedin(br) && StringUtils.containsIgnoreCase(br.getURL(), path_account_info)) {
                br.getPage(path_account_info);
                if (!isLoggedin(br)) {
                    throw new AccountInvalidException();
                }
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
            return true;
        }
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("user/logout\"");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        br.getPage("/user/my");
        final String space = br.getRegex("Space Used (\\d[^<]+)</div>").getMatch(0);
        if (space != null) {
            ai.setUsedSpace(space.trim());
        } else {
            logger.info("Failed to find used account storage space");
        }
        /* TODO: Add detection of different account types such as premium accounts */
        // ai.setUnlimitedTraffic();
        /**
         * Hardcoded free account limits; you can find those in the download mode overview when trying to download any file in browser. <br>
         * Screenshot 2025-05-06: https://snipboard.io/Hpcdv0.jpg
         */
        final long account_free_traffic_max = 1024l * 1024 * 1024;
        ai.setTrafficLeft(account_free_traffic_max);
        ai.setTrafficMax(account_free_traffic_max);
        account.setType(AccountType.FREE);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
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
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}