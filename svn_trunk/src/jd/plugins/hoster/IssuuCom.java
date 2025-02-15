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
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.config.IssuuComConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 50517 $", interfaceVersion = 3, names = { "issuu.com" }, urls = { "https?://issuu\\.com/([a-z0-9\\-_\\.]+)/docs/([a-z0-9\\-_]+)" })
public class IssuuCom extends PluginForHost {
    public IssuuCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/signup");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.COOKIE_LOGIN_ONLY };
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/acceptterms";
    }

    public static final String PROPERTY_FINAL_NAME  = "finalname";
    public static final String PROPERTY_DOCUMENT_ID = "document_id";

    private String getUsername(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    private String getDocumentSlug(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(1);
    }

    /** Using oembed API: http://developers.issuu.com/api/oembed.html */
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        final String filename = link.getStringProperty("finalname");
        if (filename != null) {
            link.setFinalFileName(filename);
        }
        final Browser brc = br.cloneBrowser();
        brc.setAllowedResponseCodes(501);
        brc.getPage("https://issuu.com/oembed?format=json&url=" + Encoding.urlEncode(link.getPluginPatternMatcher()));
        if (brc.getHttpConnection().getResponseCode() != 200) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (filename == null) {
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            link.setFinalFileName(entries.get("title") + ".pdf");
        }
        /* Small hack */
        br.setRequest(brc.getRequest());
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    private void login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies userCookies = account.loadUserCookies();
            br.setCookies(userCookies);
            if (!force) {
                /* Do not check cookies */
                return;
            }
            br.setFollowRedirects(true);
            br.getPage("https://" + this.getHost() + "/home/publisher");
            if (!isLoggedIn(br)) {
                logger.info("User cookie login failed");
                if (account.hasEverBeenValid()) {
                    throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                } else {
                    throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                }
            }
            logger.info("User cookie login successful");
        }
    }

    private boolean isLoggedIn(final Browser br) {
        if (br.containsHTML("<a[^>]*href\\s*=\\s*\"/logout\"")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        final AccountInfo ai = new AccountInfo();
        ai.setUnlimitedTraffic();
        account.setType(AccountType.FREE);
        final String htmlUnescaped = PluginJSonUtils.unescape(br.getRequest().getHtmlCode());
        final String email = new Regex(htmlUnescaped, "\"email\":\"([^\"]+)").getMatch(0);
        if (email == null) {
            logger.info("Failed to find unique user-ID in html source");
        } else if (!htmlUnescaped.contains(account.getUser() + "\"") && !email.equalsIgnoreCase(account.getUser())) {
            /* User may have used username or email in JDownloaders' "username" field. */
            account.setUser(email);
        }
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload(link, account);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link);
        String documentID = this.br.getRegex("\"thumbnail_url\":\"https?://image\\.issuu\\.com/([^<>\"/]*?)/").getMatch(0);
        if (documentID == null) {
            br.getPage(link.getPluginPatternMatcher());
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML(">\\s*We can\\'t find what you\\'re looking for")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            documentID = PluginJSonUtils.getJsonValue(br, "documentId");
        }
        if (documentID == null) {
            /* Assume that this document is not downloadable. */
            /* Example: https://issuu.com/popcornposters/docs/cars_fccb2b4de13534 */
            throw new PluginException(LinkStatus.ERROR_FATAL, "Document is not downloadable");
        }
        if (account != null) {
            login(account, true);
        }
        br.getPage("/call/document-page/document-download/" + getUsername(link) + "/" + this.getDocumentSlug(link));
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException e) {
            if (br.getHttpConnection().getResponseCode() == 403) {
                /* No json response --> Document not downloadable --> Check for errormessage in plaintext */
                final String text = br.getRequest().getHtmlCode();
                if (text.length() <= 100 && br.getRequest().getResponseHeader("Content-Type").equalsIgnoreCase("text/plain")) {
                    /* E.g. "The publisher does not have the license to enable download" */
                    throw new PluginException(LinkStatus.ERROR_FATAL, text);
                } else {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Error 403: Document is not downloadable");
                }
            } else {
                throw e;
            }
        }
        final Number code = (Number) entries.get("code");
        final String message = (String) entries.get("message");
        if ("Download limit reached".equals(message) || (code != null && code.intValue() == 15)) {
            throw new AccountUnavailableException("Downloadlimit reached", 5 * 60 * 1000);
        } else if ("Document access denied".equals(message)) {
            /* TODO: Find errorcode for this */
            throw new PluginException(LinkStatus.ERROR_FATAL, "This document is not downloadable");
        }
        final String dllink = (String) entries.get("url");
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        // We have to wait here, otherwise we might get an empty file!
        sleep(3 * 1000l, link);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
        this.handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
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

    @Override
    public Class<? extends IssuuComConfig> getConfigInterface() {
        return IssuuComConfig.class;
    }
}