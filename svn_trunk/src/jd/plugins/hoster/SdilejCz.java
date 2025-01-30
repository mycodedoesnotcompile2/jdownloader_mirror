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

import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.controlling.AccountController;
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

@HostPlugin(revision = "$Revision: 50530 $", interfaceVersion = 3, names = { "sdilej.cz" }, urls = { "https?://(?:www\\.)?sdilej\\.cz/([A-Fa-f0-9]+)(/([a-z0-9-\\.]+))?" })
public class SdilejCz extends PluginForHost {
    /** Former czshare.com */
    private static AtomicInteger SIMULTANEOUS_PREMIUM                        = new AtomicInteger(-1);
    private static final String  CAPTCHATEXT                                 = "captcha\\.php";
    public static final String   PROPERTY_INTERNAL_FILE_ID                   = "file_id";
    private final boolean        ACCOUNT_REQUIRED_FOR_LINKCHECK_AND_DOWNLOAD = true;                 // 2025-02-29

    public SdilejCz(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/registrace");
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/vop";
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCustomCharset("utf-8");
        br.setLoadLimit(br.getLoadLimit() * 5);
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return true;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return true;
        }
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 1;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        return false;
    }

    @Override
    public boolean hasAutoCaptcha() {
        return false;
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
        final String storedInternalFileID = link.getStringProperty(PROPERTY_INTERNAL_FILE_ID);
        if (storedInternalFileID != null) {
            return storedInternalFileID;
        }
        /* Return ID from URL. */
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    private boolean isInternalFileID(final String fileID) {
        if (fileID == null) {
            return false;
        } else if (!fileID.matches("\\d+")) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        if (ACCOUNT_REQUIRED_FOR_LINKCHECK_AND_DOWNLOAD) {
            final Account account = AccountController.getInstance().getValidAccount(this.getHost());
            return requestFileInformation(link, account);
        } else {
            return requestFileInformation(link, null);
        }
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        if (ACCOUNT_REQUIRED_FOR_LINKCHECK_AND_DOWNLOAD && account == null) {
            return AvailableStatus.UNCHECKABLE;
        }
        if (account != null) {
            this.login(account, false);
        }
        final String fileID = this.getFID(link);
        if (!link.isNameSet()) {
            final String urlTitle = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(2);
            if (urlTitle != null) {
                link.setName(urlTitle.replace("-", " ").trim());
            } else {
                link.setName(fileID);
            }
        }
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("Omluvte, prosím, výpadek databáze\\. Na opravě pracujeme")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.canHandle(br.getURL())) {
            /* E.g. /error.php?co=4 */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String filename = br.getRegex("<h1[^>]*>([^<]+)<").getMatch(0);
        String filesize = br.getRegex("Velikost:?\\s*</b>([^<]+)<").getMatch(0);
        /* Set final filename here because server sends html encoded filenames */
        if (filename != null) {
            link.setFinalFileName(Encoding.htmlDecode(filename).trim());
        } else {
            logger.warning("Failed to find filename");
        }
        if (filesize != null) {
            filesize = filesize.trim();
            if (filesize.equals("0 B")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            link.setDownloadSize(SizeFormatter.getSize(filesize.replace(",", ".").replace(" ", "")));
        } else {
            logger.warning("Failed to find filesize");
        }
        if (!isInternalFileID(fileID)) {
            final String internalFileID = br.getRegex("name=\"file_id\" value=\"(\\d+)").getMatch(0);
            if (internalFileID != null) {
                link.setProperty(PROPERTY_INTERNAL_FILE_ID, internalFileID);
            } else {
                logger.warning("Failed to find internalFileID");
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return SIMULTANEOUS_PREMIUM.get();
    }

    private void handleErrors(final Browser br) throws PluginException {
        if (br.containsHTML("Soubor byl smazan")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("Soubor je dočasně nedostupný\\.")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error");
        } else if (br.containsHTML("Z Vaší IP adresy momentálně probíhá jiné stahování\\. Využijte PROFI")) {
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "IP already downloading", 12 * 60 * 1000);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        handleDownload(link, null);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link, account);
        handleErrors(br);
        String dllink = null;
        if (account != null && AccountType.PREMIUM == account.getType()) {
            dllink = br.getRegex("\"(https?://[^/]+/sdilej_profi\\.php\\?id=\\d+[^\"]+)").getMatch(0);
            if (dllink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        if (dllink == null) {
            dllink = br.getRegex("<a href=\"([^<>\"]+?)\"[^<>]+?>Stáhnout FREE\\s*<").getMatch(0);
        }
        final String fileID = this.getFID(link);
        if (dllink == null) {
            String freeLink = br.getRegex("allowTransparency=\"true\"></iframe><a href=\"(/.*?)\"").getMatch(0);
            if (freeLink == null) {
                freeLink = br.getRegex("\"(/download\\.php\\?id=\\d+.*?code=.*?)\"").getMatch(0);
            }
            if (freeLink != null) {
                br.getPage(Encoding.htmlDecode(freeLink));
                handleErrors(br);
                String file = br.getRegex("name=\"file\" value=\"(.*?)\"").getMatch(0);
                String size = br.getRegex("name=\"size\" value=\"(\\d+)\"").getMatch(0);
                String server = br.getRegex("name=\"server\" value=\"(.*?)\"").getMatch(0);
                if (!br.containsHTML(CAPTCHATEXT) || file == null || size == null || server == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                String code = getCaptchaCode("/captcha.php", link);
                br.postPage("/download.php", "id=" + fileID + "&file=" + file + "&size=" + size + "&server=" + server + "&captchastring2=" + Encoding.urlEncode(code) + "&freedown=Ov%C4%9B%C5%99it+a+st%C3%A1hnout");
                if (br.containsHTML("Chyba 6 / Error 6")) {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, 60 * 60 * 1000);
                } else if (br.containsHTML(">\\s*Zadaný ověřovací kód nesouhlasí") || br.containsHTML(CAPTCHATEXT)) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
                dllink = br.getRegex("<p class=\"button2\" id=\"downloadbtn\" style=\"display:none\">[\t\n\r ]+<a href=\"(https?://[^<>\"]*?)\"").getMatch(0);
                if (dllink == null) {
                    dllink = br.getRegex("\"(https?://www\\d+\\.sdilej\\.cz/download\\.php\\?id=[^<>\"]*?)\"").getMatch(0);
                }
                /** Waittime can be skipped */
                // int wait = 50;
                // final String waittime =
                // br.getRegex("countdown_number = (\\d+);").getMatch(0);
                // if (waittime != null) wait = Integer.parseInt(waittime);
                // sleep(wait * 1001l, downloadLink);
            }
        }
        if (dllink == null) {
            if (!isInternalFileID(fileID)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dllink = "/free/index.php?id=" + fileID;
            logger.info("Using static fallback: " + dllink);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            this.handleErrors(br);
            this.throwConnectionExceptions(br, dl.getConnection());
            if (br.containsHTML("/free-stahovani")) {
                throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Too many concurrent free downloads", 1 * 60 * 1000);
            }
            this.throwFinalConnectionException(br, dl.getConnection());
        }
        dl.startDownload();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    private boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(this.getHost(), cookies);
                if (!force) {
                    /* Don't validate cookies */
                    return false;
                }
                br.getPage("https://" + this.getHost() + "/");
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
            br.getPage("https://" + this.getHost() + "/prihlasit");
            final Form loginform = br.getFormbyProperty("id", "loginform");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find loginform");
            }
            loginform.put("login", Encoding.urlEncode(account.getUser()));
            loginform.put("heslo", Encoding.urlEncode(account.getPass()));
            br.submitForm(loginform);
            if (!isLoggedin(br)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
            return true;
        }
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("logout\\.php\"");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        br.getPage("/predplatne");
        final String trafficleftStr = br.getRegex("\\(kredit:?\\s*(\\d+[^<\\)]+)\\)").getMatch(0);
        final boolean isUnlimited = br.containsHTML("Neomezené předplatné") && br.containsHTML(">\\s*Aktivní\\s*<");
        if (trafficleftStr != null) {
            final long trafficleftBytes = SizeFormatter.getSize(trafficleftStr.replace(",", "."));
            if (trafficleftBytes <= 0) {
                logger.info("Detected free account without credits -> Downloads are still magically possible so traffic will be set as special traffic!");
                account.setType(AccountType.FREE);
                ai.setSpecialTraffic(true);
            } else {
                account.setType(AccountType.PREMIUM);
            }
            ai.setTrafficLeft(trafficleftBytes);
            ai.setTrafficRefill(false);
        } else if (isUnlimited) {
            account.setType(AccountType.PREMIUM);
            ai.setUnlimitedTraffic();
        } else {
            account.setType(AccountType.FREE);
        }
        return ai;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (ACCOUNT_REQUIRED_FOR_LINKCHECK_AND_DOWNLOAD && account == null) {
            /* Without account its not possible to download any link from this host. */
            return false;
        }
        return super.canHandle(link, account);
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