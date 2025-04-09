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

import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;

import jd.PluginWrapper;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 50941 $", interfaceVersion = 3, names = {}, urls = {})
public class WrzutaNet extends PluginForHost {
    public WrzutaNet(PluginWrapper wrapper) {
        super(wrapper);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            this.enablePremium("https://" + getHost() + "/premium");
        }
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/page/privacy";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "wrzuta.net" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/file/([A-Za-z0-9]{20})(/([^/]+))?");
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
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return true;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
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
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
            final String fileID = urlinfo.getMatch(0);
            final String filenameFromURL = urlinfo.getMatch(2);
            if (filenameFromURL != null) {
                link.setName(Encoding.htmlDecode(filenameFromURL));
            } else {
                link.setName(fileID);
            }
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Strona nie istnieje")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex(">([^<]+)</h6>").getMatch(0);
        String filesize = br.getRegex("Rozmiar:\\s*</span>\\s*<span[^>]*>([^<]+)<").getMatch(0);
        if (filename != null) {
            link.setName(Encoding.htmlDecode(filename.trim()));
        } else {
            logger.warning("Failed to find filename");
        }
        if (!StringUtils.isEmpty(filesize)) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        handleDownload(link, null);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String directlinkproperty = "directurl_" + (account != null ? account.getType().getLabel() : null);
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        Request dlreq;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dlreq = br.createGetRequest(storedDirecturl);
        } else {
            // TODO: Implement premium account support
            /* TODO: Implement [audio-] stream download as this can be used to skip the captcha */
            // final String streamDownloadurl = br.getRegex("src=\"(https?://[^\"]+)\" type=\"audio/mp3\"").getMatch(0);
            final Form dlform = br.getFormbyKey("download_file");
            if (dlform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final CaptchaHelperHostPluginCloudflareTurnstile ts = new CaptchaHelperHostPluginCloudflareTurnstile(this, br);
            logger.info("Detected captcha method \"CloudflareTurnstileCaptcha\" for this host");
            dlform.put("cf-turnstile-response", Encoding.urlEncode(ts.getToken()));
            dlreq = br.createFormRequest(dlform);
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dlreq, this.isResumeable(link, null), this.getMaxChunks(link, null));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
                }
                try {
                    br.followConnection(true);
                } catch (final IOException e) {
                    logger.log(e);
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirecturl == null) {
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    private boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
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
            br.getPage("https://" + this.getHost() + "/login.php");
            final Form loginform = br.getFormbyProperty("name", "login");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find loginform");
            }
            loginform.put("username", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            br.postPage("", "email=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
            if (!isLoggedin(br)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
            return true;
        }
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("/logout");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        String space = br.getRegex("").getMatch(0);
        if (space != null) {
            ai.setUsedSpace(space.trim());
        }
        ai.setUnlimitedTraffic();
        if (br.containsHTML("")) {
            account.setType(AccountType.FREE);
            /* free accounts can still have captcha */
            account.setConcurrentUsePossible(false);
        } else {
            final String expire = br.getRegex("").getMatch(0);
            if (expire == null) {
                throw new AccountInvalidException();
            } else {
                ai.setValidUntil(TimeFormatter.getMilliSeconds(expire, "dd MMMM yyyy", Locale.ENGLISH));
            }
            account.setType(AccountType.PREMIUM);
            account.setConcurrentUsePossible(true);
        }
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
        if (acc == null) {
            /* no account, yes we can expect captcha */
            return true;
        } else if (acc.getType() == AccountType.FREE) {
            /* Free accounts can have captchas */
            return true;
        } else {
            /* Premium accounts do not have captchas */
            return false;
        }
    }
}