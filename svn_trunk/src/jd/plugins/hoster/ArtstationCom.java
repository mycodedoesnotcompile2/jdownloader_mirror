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

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
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

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.net.httpconnection.HTTPConnection;
import org.appwork.utils.net.httpconnection.SSLSocketStreamOptions;
import org.appwork.utils.net.httpconnection.SSLSocketStreamOptionsModifier;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.config.ArtstationComConfig;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 50525 $", interfaceVersion = 3, names = { "artstation.com" }, urls = { "https?://[a-z0-9\\-\\.]+\\.artstation\\.com/p/assets/.+" })
public class ArtstationCom extends PluginForHost {
    public ArtstationCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www.artstation.com/users/sign_up");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        if (cookieLoginOnly) {
            return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL, LazyPlugin.FEATURE.COOKIE_LOGIN_ONLY };
        } else {
            return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL, LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL };
        }
    }

    private final boolean      cookieLoginOnly   = true;
    /* DEV NOTES */
    // Tags:
    // protocol: no https
    // other: image links do not use cloudflare service.
    /* Extension which will be used if no correct extension is found */
    public static final String default_Extension = ".jpg";
    /* Connection stuff */
    private static final int   free_maxchunks    = 1;
    private static final int   free_maxdownloads = -1;
    private String             dllink            = null;

    @Override
    public String getAGBLink() {
        return "https://artstation.com/";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dllink = null;
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        // filename NOT coming from decrypter wont have filename set!!! this is not a plugin defect! -raztoki20160202
        String filename = link.getStringProperty("decrypterfilename");
        dllink = link.getPluginPatternMatcher();
        if (dllink.contains("/large/") && PluginJsonConfig.get(ArtstationComConfig.class).isForce4kWorkaroundForImages()) {
            dllink = largeTo4k(dllink);
        }
        if (filename != null) {
            link.setFinalFileName(filename);
        }
        final Browser br2 = br.cloneBrowser();
        // In case the link redirects to the finallink
        br2.setFollowRedirects(true);
        URLConnectionAdapter con = null;
        try {
            br2.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING, "identity");
            con = br2.openHeadConnection(dllink);
            if (looksLikeDownloadableContent(con)) {
                if (con.getCompleteContentLength() > 0) {
                    link.setDownloadSize(con.getCompleteContentLength());
                }
                if (filename == null) {
                    filename = getFileNameFromConnection(con);
                    link.setFinalFileName(filename);
                }
                link.setProperty("directlink", dllink);
            } else {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        } finally {
            try {
                con.disconnect();
            } catch (final Throwable e) {
            }
        }
        return AvailableStatus.TRUE;
    }

    public static String largeTo4k(final String url) {
        return url.replaceFirst("/large/", "/4k/");
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        doDownload(null, link, dllink);
    }

    private void doDownload(final Account account, final DownloadLink link, final String url) throws Exception {
        this.br.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING, "identity");
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, url, this.isResumeable(link, account), free_maxchunks);
        if (!looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        if (account == null || AccountType.FREE.equals(account.getType())) {
            link.setProperty("free_directlink", dllink);
        } else {
            link.setProperty("premium_directlink", dllink);
        }
        dl.startDownload();
    }

    /** Sets correct json headers */
    public static void setHeaders(final Browser br) {
        String token = br.getRegex("name\\s*=\\s*\"authenticity_token\"\\s*type\\s*=\\s*\"hidden\"\\s*value\\s*=\\s*\"([^<>\"]*?)\"").getMatch(0);
        if (token == null) {
            token = br.getRegex("type\\s*=\\s*\"hidden\"\\s*name\\s*=\\s*\"authenticity_token\"\\s*value\\s*=\\s*\"([^<>\"]*?)\"").getMatch(0);
        }
        br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
        if (token != null) {
            br.getHeaders().put("X-CSRF-Token", token);
        }
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        /* Not necessarily needed! */
        // br.getHeaders().put("X-NewRelic-ID", "VgMBWFFQGwEIUVFQAgE=");
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return free_maxdownloads;
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser ret = super.createNewBrowserInstance();
        ret.setSSLSocketStreamOptions(new SSLSocketStreamOptionsModifier() {
            @Override
            public SSLSocketStreamOptions modify(SSLSocketStreamOptions sslSocketStreamOptions, HTTPConnection httpConnection) {
                // may avoid cloudflare
                sslSocketStreamOptions.getDisabledCipherSuites().clear();
                sslSocketStreamOptions.getCustomFactorySettings().add("JSSE_TLS1.3_ENABLED");
                sslSocketStreamOptions.getCustomFactorySettings().add("BC_TLS1.3_ENABLED");
                return sslSocketStreamOptions;
            }
        });
        return ret;
    }

    public void login(final Account account, boolean verify) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            final Cookies userCookies = account.loadUserCookies();
            if (cookies != null || userCookies != null) {
                if (userCookies != null) {
                    br.setCookies(userCookies);
                } else {
                    br.setCookies(cookies);
                }
                if (!verify) {
                    /* Do not verify cookies */
                    return;
                }
                br.getPage("https://www." + account.getHoster() + "/");
                if (br.containsHTML("\"sign_out")) {
                    logger.info("Cookie login successful");
                    if (userCookies == null) {
                        account.saveCookies(br.getCookies(br.getHost()), "");
                    }
                    return;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    if (userCookies != null) {
                        if (account.hasEverBeenValid()) {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                        } else {
                            throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                        }
                    }
                }
            }
            if (cookieLoginOnly) {
                showCookieLoginInfo();
                throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_required());
            }
            logger.info("Performing full login");
            br.getPage("https://www." + this.getHost() + "/");
            Form loginform = br.getFormbyKey("authenticity_token");
            if (loginform == null) {
                loginform = br.getFormbyActionRegex(".*/session/login");
            }
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            loginform.put(loginform.getBestVariable("email"), Encoding.urlEncode(account.getUser()));
            loginform.put(loginform.getBestVariable("password"), Encoding.urlEncode(account.getPass()));
            final Browser brc = br.cloneBrowser();
            setHeaders(brc);
            brc.submitForm(loginform);
            if (br.getCookie(account.getHoster(), "ArtStationSessionCookie", Cookies.NOTDELETEDPATTERN) == null) {
                if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nUngültiger Benutzername oder ungültiges Passwort!\r\nSchnellhilfe: \r\nDu bist dir sicher, dass dein eingegebener Benutzername und Passwort stimmen?\r\nFalls dein Passwort Sonderzeichen enthält, ändere es und versuche es erneut!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nInvalid username/password!\r\nQuick help:\r\nYou're sure that the username and password you entered are correct?\r\nIf your password contains special characters, change it (remove them) and try again!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                }
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    @SuppressWarnings("deprecation")
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        final AccountInfo ai = new AccountInfo();
        ai.setUnlimitedTraffic();
        account.setType(AccountType.FREE);
        /* free accounts can still have captcha */
        account.setMaxSimultanDownloads(free_maxdownloads);
        account.setConcurrentUsePossible(false);
        account.setValid(true);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link);
        login(account, false);
        br.getPage(link.getDownloadURL());
        doDownload(account, link, dllink);
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return free_maxdownloads;
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
    public Class<? extends ArtstationComConfig> getConfigInterface() {
        return ArtstationComConfig.class;
    }
}
