//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;

import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 48630 $", interfaceVersion = 2, names = { "hyperfileshare.com" }, urls = { "https?://[\\w\\.]*?hyperfileshare\\.com/(?:d/|download\\.php\\?code=)([a-fA-F0-9]+)" })
public class HyperFileShareCom extends PluginForHost {
    public HyperFileShareCom(PluginWrapper wrapper) {
        super(wrapper);
        // Actually we only got support for free accounts, not for premium!
        this.enablePremium("http://www.hyperfileshare.com/register.php");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account);
        account.setType(AccountType.FREE);
        ai.setUnlimitedTraffic();
        return ai;
    }

    @Override
    public String getAGBLink() {
        return "http://download.hyperfileshare.com/terms.php";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 5;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return "hyperfilesharecom://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    private static final String MAINTENANCE = ">\\s*Servers Maintenance\\s*<";

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        br.setCookiesExclusive(true);
        br.clearCookies(getHost());
        br.getPage(link.getPluginPatternMatcher());
        br.setFollowRedirects(true);
        final String properlink = br.getRegex("The document has moved <a href=\"(.*?)\"").getMatch(0);
        if (properlink != null) {
            br.getPage(properlink);
        }
        if (br.containsHTML(MAINTENANCE)) {
            return AvailableStatus.UNCHECKABLE;
        }
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("Download URL is incorrect") || br.containsHTML("Not Found")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("<title>Download (.*?)</title>").getMatch(0);
        if (filename == null) {
            filename = br.getRegex("<span>Download(.*?)</span></div>").getMatch(0);
        }
        String size = br.getRegex("File size:.*?strong>(.*?)</strong>").getMatch(0);
        if (filename != null) {
            link.setName(Encoding.htmlDecode(filename).trim());
        }
        if (size != null) {
            link.setDownloadSize(SizeFormatter.getSize(size + "B"));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (br.containsHTML(MAINTENANCE)) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server is in maintenance mode!", 3 * 60 * 60 * 1000l);
        } else if (br.containsHTML(">We are sorry, requested file is temporarily not available")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE);
        }
        br.setFollowRedirects(true);
        String url = br.getRegex("href=\"(download\\.php\\?code=[a-f0-9]+&sid=[a-f0-9]+&s=\\d)\"").getMatch(0);
        if (url == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        url = "http://download.hyperfileshare.com/" + url;
        br.getPage(url);
        url = br.getRegex("href=\"(download\\.php\\?code=[a-f0-9]+\\&sid=[a-f0-9]+\\&s=\\d)\"").getMatch(0);
        if (url == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        url = "http://download.hyperfileshare.com/" + url;
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, url, true, 1);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (br.containsHTML("(You exceeded your download size limit|>You exceeded your download quota)")) {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, 1 * 60 * 60 * 1000l);
            } else if (br.containsHTML(">We are sorry, requested file is temporarily not available")) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "We are sorry, requested file is temporarily not available", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link);
        login(account);
        br.getPage(link.getPluginPatternMatcher());
        String url = br.getRegex("href=\"(download\\.php\\?code=[a-f0-9]+&sid=[a-f0-9]+&s=\\d)\"").getMatch(0);
        if (url == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        url = "http://download.hyperfileshare.com/" + url;
        br.getPage(url);
        url = null;
        url = br.getRegex("href=\"(download\\.php\\?code=[a-f0-9]+&sid=[a-f0-9]+&s=\\d)\"").getMatch(0);
        if (url == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        url = "http://download.hyperfileshare.com/" + url;
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, url, true, 0);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (br.containsHTML("You exceeded your download size limit")) {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    private void login(final Account account) throws Exception {
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage("http://www.hyperfileshare.com/index.php");
        if (br.containsHTML(MAINTENANCE)) {
            if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nKann Account nicht prüfen!\r\nAnbieter befindet sich momentan im Wartungsmodus!", PluginException.VALUE_ID_PREMIUM_DISABLE);
            } else {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nCannot check account!\r\nServer is in maintenance mode!", PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
        }
        br.postPage("http://www.hyperfileshare.com/index.php", "login=" + Encoding.urlEncode(account.getUser()) + "&psw=" + Encoding.urlEncode(account.getPass()) + "&rem_me=1");
        if (br.getCookie("http://www.hyperfileshare.com", "sid", Cookies.NOTDELETEDPATTERN) == null) {
            if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nUngültiger Benutzername oder ungültiges Passwort!\r\nDu bist dir sicher, dass dein eingegebener Benutzername und Passwort stimmen? Versuche folgendes:\r\n1. Falls dein Passwort Sonderzeichen enthält, ändere es (entferne diese) und versuche es erneut!\r\n2. Gib deine Zugangsdaten per Hand (ohne kopieren/einfügen) ein.", PluginException.VALUE_ID_PREMIUM_DISABLE);
            } else {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nInvalid username/password!\r\nYou're sure that the username and password you entered are correct? Some hints:\r\n1. If your password contains special characters, change it (remove them) and try again!\r\n2. Type in your username/password by hand without copy & paste.", PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }

    @Override
    public void resetPluginGlobals() {
    }
}