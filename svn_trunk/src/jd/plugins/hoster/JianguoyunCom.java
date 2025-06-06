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

import java.util.Map;

import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51090 $", interfaceVersion = 2, names = { "jianguoyun.com" }, urls = { "http://jianguoyundecrypted\\.com/\\d+" })
public class JianguoyunCom extends PluginForHost {
    public JianguoyunCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www.jianguoyun.com/d/signup");
    }

    @Override
    public String getAGBLink() {
        return "http://help.jianguoyun.com/?page_id=490";
    }

    private String       dllink                 = null;
    private String       folderid               = null;
    private String       passCode               = null;
    private boolean      possiblePremiumonly    = false;
    private final String html_passwordprotected = "id=\"pwd\\-verify\\-view\"";

    @Override
    public String getLinkID(final DownloadLink link) {
        final String folderid = link.getStringProperty("folderid");
        final String relPath = link.getStringProperty("relPath");
        if (folderid != null && relPath != null) {
            return "jianguoyun_com://file/folderid" + folderid + "/relPath" + relPath;
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        prepBR(br);
        dllink = null;
        folderid = link.getStringProperty("folderid", null);
        passCode = link.getDownloadPassword();
        final String relPath = link.getStringProperty("relPath", null);
        final String mainlink = link.getStringProperty("mainlink", null);
        final boolean singlefile = link.getBooleanProperty("singlefile", false);
        /* No folderid --> We have no downloadable link */
        if (folderid == null || relPath == null || mainlink == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (singlefile) {
            accessMainlink(mainlink);
            if (br.getHttpConnection().getResponseCode() == 404 || br.containsHTML("class=\"owner\"><")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            if (!scanFileinfoFromWebsite(br, link)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final boolean download_need_signin = link.getBooleanProperty("download_need_signin", false);
            if (download_need_signin) {
                /* Only available for signed-in users but if its a photo we might be able to download it anways! */
                possiblePremiumonly = true;
                this.dllink = br.getRegex("photoURL:[\t\n\r ]*?\\'([^<>\"\\']+)\\'").getMatch(0);
            }
        } else {
            br.getPage("https://www.jianguoyun.com/d/ajax/dirops/pubDIRLink?k=" + folderid + "&dn=null&p=" + Encoding.urlEncode(relPath) + "&forwin=1&_=" + System.currentTimeMillis());
            if (br.getHttpConnection().getResponseCode() == 401) {
                /* 2021-02-18: {"errorCode":"UnAuthorized","detailMsg":"Try to sign in to access it","payload":null} */
                throw new AccountRequiredException();
            }
            getDllink();
        }
        // final String filename = getJson(br.toString(), "n");
        // final String filesize = getJson(br.toString(), "real_size");
        // if (filename == null || filesize == null) {
        // throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        // }
        // link.setName(unescape(Encoding.htmlDecode(filename.trim())));
        // link.setDownloadSize(Long.parseLong(filesize));
        return AvailableStatus.TRUE;
    }

    @SuppressWarnings("unchecked")
    private void getDllink() throws Exception {
        if (br.getHttpConnection().getResponseCode() == 404) {
            /*
             * {"errorCode":"ObjectNotFound","detailMsg":"the Object /P802 Release File/Important No56757467ce.jpg doesn't
             * exist","payload":null}
             */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(br.toString());
        this.dllink = (String) entries.get("url");
    }

    public static boolean scanFileinfoFromWebsite(final Browser br, final DownloadLink dl) {
        if (br.getHttpConnection().getResponseCode() == 404 || br.containsHTML("class=\"owner\"><")) {
            dl.setAvailable(false); // Suitable for decrypter plugin only, hoster plugin needs PluginException
            return false;
        }
        final String pageJson = getWebsiteJson(br);
        final String isdeleted = new Regex(pageJson, "isdeleted[\t\n\r ]*?:[\t\n\r ]*?(true|false)").getMatch(0);
        final String isrevoked = new Regex(pageJson, "isrevoked[\t\n\r ]*?:[\t\n\r ]*?(true|false)").getMatch(0);
        final String canNotAccess = new Regex(pageJson, "canNotAccess[\t\n\r ]*?:[\t\n\r ]*?(true|false)").getMatch(0);
        String filename = new Regex(pageJson, "name[\t\n\r ]*?:[\t\n\r ]*?\\'([^<>\"\\']+)\\'").getMatch(0);
        final String filesize = new Regex(pageJson, "size[\t\n\r ]*?:[\t\n\r ]*?\\'(\\d+)\\'").getMatch(0);
        final String download_need_signin = new Regex(pageJson, "download_need_signin[\t\n\r ]*?:[\t\n\r ]*?(true|false)").getMatch(0);
        if (download_need_signin != null) {
            dl.setProperty("download_need_signin", Boolean.parseBoolean(download_need_signin));
        }
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            dl.setName(filename);
        }
        if (filesize != null) {
            dl.setDownloadSize(Long.parseLong(filesize));
        }
        if ("true".equalsIgnoreCase(isdeleted)) {
            dl.setAvailable(false);
            return false;
        } else {
            dl.setAvailable(true);
            return true;
        }
    }

    public static String getWebsiteJson(final Browser br) {
        String pageJson = br.getRegex("var PageInfo = (\\{.*?\\});").getMatch(0);
        if (pageJson == null) {
            pageJson = br.toString();
        }
        return pageJson;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        doFree(link);
    }

    public void doFree(final DownloadLink link) throws Exception, PluginException {
        if (dllink == null) {
            dllink = checkDirectLink(link, "directlink");
        }
        if (dllink == null) {
            if (handlePasswordProtected(link)) {
                /* Make sure we get the correct filename after user entered download password. */
                requestFileInformation(link);
            }
            br.getPage("https://www.jianguoyun.com/d/ajax/fileops/pubFileLink?k=" + this.folderid + "&name=" + Encoding.urlEncode(link.getName()) + "&forwin=1&_=" + System.currentTimeMillis());
            getDllink();
        }
        if (dllink == null) {
            if (possiblePremiumonly) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, false, 1);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            logger.warning("Finallink does not lead to a file...");
            br.followConnection(true);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        link.setProperty("directlink", dllink);
        dl.startDownload();
    }

    private boolean handlePasswordProtected(final DownloadLink dl) throws Exception {
        if (br.containsHTML(html_passwordprotected)) {
            if (this.passCode == null) {
                passCode = getUserInput("Password?", dl);
                br.postPage("/d/ajax/pubops/unlockPubObject", "sp=" + Encoding.urlEncode(passCode) + "&key=" + this.folderid);
                if (br.getHttpConnection().getResponseCode() != 200) {
                    dl.setDownloadPassword(null);
                    throw new PluginException(LinkStatus.ERROR_RETRY, "Wrong password entered");
                }
                dl.setDownloadPassword(this.passCode);
                final Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(br.toString());
                final String continue_url = (String) entries.get("url");
                if (continue_url != null) {
                    br.getPage(continue_url);
                }
            }
            return true;
        }
        return false;
    }

    private void accessMainlink(String mainlink) throws Exception {
        if (this.passCode != null) {
            mainlink += "?pd=" + Encoding.urlEncode(this.passCode);
        }
        br.getPage(mainlink);
    }

    private Browser prepBR(final Browser br) {
        br.setAllowedResponseCodes(401);
        br.setFollowRedirects(true);
        return br;
    }

    private String checkDirectLink(final DownloadLink link, final String property) {
        String dllink = link.getStringProperty(property);
        if (dllink != null) {
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
        }
        return null;
    }

    private void login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            try {
                prepBR(br);
                final Cookies cookies = account.loadCookies("");
                if (cookies != null && !force) {
                    br.setCookies(this.getHost(), cookies);
                    br.getPage("https://www." + this.getHost() + "/d/ajax/userop/getUserInfo?start=1&_=" + System.currentTimeMillis());
                    if (br.getHttpConnection().getResponseCode() == 200) {
                        /* 401 */
                        return;
                    }
                    /* Usually 401 == Not logged in */
                    br = this.prepBR(this.createNewBrowserInstance());
                }
                br.getPage("https://www." + this.getHost() + "/d/login");
                br.postPage(br.getURL(), "login_email=" + Encoding.urlEncode(account.getUser()) + "&login_password=" + Encoding.urlEncode(account.getPass()) + "&remember_me=on");
                if (br.getCookie(this.getHost(), "umn") == null) {
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nUngültiger Benutzername oder ungültiges Passwort!\r\nSchnellhilfe: \r\nDu bist dir sicher, dass dein eingegebener Benutzername und Passwort stimmen?\r\nFalls dein Passwort Sonderzeichen enthält, ändere es und versuche es erneut!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nInvalid username/password!\r\nQuick help:\r\nYou're sure that the username and password you entered are correct?\r\nIf your password contains special characters, change it (remove them) and try again!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    }
                }
                account.saveCookies(br.getCookies(this.getHost()), "");
            } catch (final PluginException e) {
                account.clearCookies("");
                throw e;
            }
        }
    }

    @SuppressWarnings("deprecation")
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        if (!account.getUser().matches(".+@.+\\..+")) {
            if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nBitte gib deine E-Mail Adresse ins Benutzername Feld ein!", PluginException.VALUE_ID_PREMIUM_DISABLE);
            } else {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nPlease enter your e-mail address in the username field!", PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
        }
        try {
            login(account, true);
        } catch (PluginException e) {
            account.setValid(false);
            throw e;
        }
        br.getPage("/d/ajax/userop/getUserInfo?start=1&_=" + System.currentTimeMillis());
        @SuppressWarnings("unused")
        final Map<String, Object> entries = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(br.toString());
        final long accountExpireLeftTime = JavaScriptEngineFactory.toLong("accountExpireLeftTime", 0);
        if (accountExpireLeftTime <= 0) {
            account.setType(AccountType.FREE);
        } else {
            /* TODO: Check this account type */
            account.setType(AccountType.PREMIUM);
            ai.setValidUntil(accountExpireLeftTime);
        }
        account.setValid(true);
        ai.setUnlimitedTraffic();
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link);
        login(account, false);
        doFree(link);
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}