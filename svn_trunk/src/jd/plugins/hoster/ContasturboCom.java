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

import java.util.Arrays;
import java.util.concurrent.TimeUnit;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
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
import jd.plugins.components.MultiHosterManagement;

import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 51606 $", interfaceVersion = 3, names = { "contasturbo.com" }, urls = { "" })
public class ContasturboCom extends PluginForHost {
    private static MultiHosterManagement mhm = new MultiHosterManagement("contasturbo.com");

    public ContasturboCom(PluginWrapper wrapper) {
        super(wrapper);
        setStartIntervall(1 * 1000l);
        this.enablePremium("https://www." + getHost() + "/planos/");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost();
    }

    private static final boolean ACCOUNT_PREMIUM_RESUME    = true;
    private static final int     ACCOUNT_PREMIUM_MAXCHUNKS = -2;
    private static final int     ACCOUNT_PREMIUM_MAXDLS    = 8;

    private boolean login(final Account account, final boolean validateCookies) throws Exception {
        synchronized (account) {
            br.setCustomCharset("utf-8");
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            br.setFollowRedirects(true);
            final Cookies cookies = account.loadCookies("");
            final Cookies userCookies = account.loadUserCookies();
            if (userCookies != null) {
                logger.info("Attempting user cookie login");
                br.setCookies(userCookies);
                if (!validateCookies) {
                    /* Do not validate cookies */
                    return false;
                }
                if (this.verifyCookies(account, userCookies)) {
                    logger.info("User cookie login successful");
                    return true;
                } else {
                    if (account.hasEverBeenValid()) {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                    } else {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                    }
                }
            }
            if (cookies != null) {
                br.setCookies(cookies);
                if (!validateCookies) {
                    /* Do not validate cookies */
                    return false;
                }
                if (this.verifyCookies(account, cookies)) {
                    account.saveCookies(br.getCookies(this.getHost()), "");
                    return true;
                }
            }
            logger.info("Performing full login");
            br.clearCookies(null);
            br.setFollowRedirects(true);
            br.getPage("https://www." + account.getHoster() + "/login/");
            final Form loginform = br.getFormbyActionRegex(".*?login.*?");
            if (loginform == null) {
                logger.warning("Failed to find loginform");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            loginform.put("email", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            /* Makes cookies last longer */
            loginform.put("remember", "1");
            br.submitForm(loginform);
            if (!isLoggedIN(br)) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
            account.saveCookies(br.getCookies(this.getHost()), "");
            return true;
        }
    }

    protected boolean verifyCookies(final Account account, final Cookies cookies) throws Exception {
        br.setCookies(this.getHost(), cookies);
        br.getPage("https://www." + this.getHost() + "/gerador/");
        if (isLoggedIN(this.br)) {
            logger.info("Successfully logged in via cookies");
            return true;
        } else {
            logger.info("Cookie login failed");
            br.clearCookies(br.getHost());
            return false;
        }
    }

    private boolean isLoggedIN(final Browser br) {
        if (br.getCookie(br.getHost(), "ct_auth", Cookies.NOTDELETEDPATTERN) != null && br.getCookie(br.getHost(), "ct_user", Cookies.NOTDELETEDPATTERN) != null) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        AccountInfo ai = new AccountInfo();
        this.login(account, true);
        if (br.getURL() == null || !br.getURL().contains("/gerador")) {
            br.getPage("/gerador/");
        }
        final String expireDays = br.getRegex("Premium válida por (\\d+) dias").getMatch(0);
        final String expireExtraHours = br.getRegex("Premium válida por \\d+ dias e (\\d+) horas").getMatch(0);
        if (expireDays != null) {
            account.setType(AccountType.PREMIUM);
            account.setMaxSimultanDownloads(ACCOUNT_PREMIUM_MAXDLS);
            ai.setStatus("Premium Account");
            ai.setUnlimitedTraffic();
            long hours_total = Long.parseLong(expireDays) * 24;
            if (expireExtraHours != null) {
                hours_total += Long.parseLong(expireExtraHours);
            }
            ai.setValidUntil(System.currentTimeMillis() + TimeUnit.HOURS.toMillis(hours_total), br);
        } else {
            account.setType(AccountType.FREE);
            ai.setTrafficLeft(0);
        }
        final String[] hosts = br.getRegex("class=\"logo_servers\">\\s*<div[^>]*?><span>([^<>\"]+)</span>").getColumn(0);
        ai.setMultiHostSupport(this, Arrays.asList(hosts));
        return ai;
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        login(account, false);
        final String url = Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this));
        br.postPage("https://www." + this.getHost() + "/api/ext/linkRequest/", "links=" + url);
        final String dllink = br.getRegex("\"(http?://cdn\\.contasturbo\\.com/dl/[^<>\"]*?)\"").getMatch(0);
        if (dllink == null) {
            mhm.handleErrorGeneric(account, link, "dllinknull", 50);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, ACCOUNT_PREMIUM_RESUME, ACCOUNT_PREMIUM_MAXCHUNKS);
        if (dl.getConnection().getContentType().contains("html")) {
            br.followConnection();
            mhm.handleErrorGeneric(account, link, "unknown_dl_error", 50);
        }
        dl.startDownload();
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception {
    }
}