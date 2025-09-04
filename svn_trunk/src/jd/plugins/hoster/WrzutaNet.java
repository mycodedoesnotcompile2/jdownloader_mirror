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
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperHostPluginCloudflareTurnstile;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
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

@HostPlugin(revision = "$Revision: 51437 $", interfaceVersion = 3, names = {}, urls = {})
public class WrzutaNet extends PluginForHost {
    public WrzutaNet(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/premium");
    }

    private final String API_BASE = "https://api.wrzuta.net/apiJD2";

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
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return requestFileInformation(link, null);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws IOException, PluginException {
        final boolean useAPI = true;
        if (useAPI) {
            return this.requestFileInformationAPI(link, account);
        } else {
            return this.requestFileInformationWebsite(link, account);
        }
    }

    private AvailableStatus requestFileInformationWebsite(final DownloadLink link, final Account account) throws IOException, PluginException {
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
        String filesize = br.getRegex("Rozmiar:?\\s*</span>\\s*<span[^>]*>([^<]+)<").getMatch(0);
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

    private AvailableStatus requestFileInformationAPI(final DownloadLink link, final Account account) throws IOException, PluginException {
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
        br.getPage(API_BASE + "/link.php?link=" + Encoding.urlEncode(link.getPluginPatternMatcher()));
        if (br.getHttpConnection().getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = checkErrorsAPI(link, account);
        final String status = entries.get("status").toString();
        if (!status.equalsIgnoreCase("ok")) {
            /* {"status":"Not found"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = null; // TODO
        String filesizeStr = entries.get("size").toString();
        if (filename != null) {
            link.setName(Encoding.htmlDecode(filename.trim()));
        } else {
            logger.warning("Failed to find filename");
        }
        if (!StringUtils.isEmpty(filesizeStr)) {
            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        final String directlinkproperty = "directurl_" + (account != null ? account.getType().getLabel() : null);
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        Request dlreq;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dlreq = br.createGetRequest(storedDirecturl);
        } else if (account != null && AccountType.PREMIUM.equals(account.getType())) {
            final Map<String, Object> dlmap = this.loginAndGetDirectDownloadlink(account, link);
            // final Number download_rate = (Number) dlmap.get("download_rate");
            final String dllink = dlmap.get("download_link").toString();
            dlreq = br.createGetRequest(dllink);
        } else {
            /* TODO: Implement [audio-] stream download as this can be used to skip the captcha */
            // final String streamDownloadurl = br.getRegex("src=\"(https?://[^\"]+)\" type=\"audio/mp3\"").getMatch(0);
            requestFileInformationWebsite(link, account);
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
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dlreq, this.isResumeable(link, account), this.getMaxChunks(link, account));
            this.handleConnectionErrors(br, dl.getConnection());
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

    private Map<String, Object> loginAndGetDirectDownloadlink(final Account account, final DownloadLink link) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            logger.info("Performing full login");
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("login", account.getUser());
            query.appendEncoded("password", account.getPass());
            if (link != null) {
                query.appendEncoded("link", link.getPluginPatternMatcher());
            }
            br.postPage(this.API_BASE + "/jd2.php", query);
            return checkErrorsAPI(link, account);
        }
    }

    protected Map<String, Object> checkErrorsAPI(final DownloadLink link, final Account account) throws NumberFormatException, PluginException {
        final long defaultWaitAccount = 3 * 60 * 1000;
        final long defaultWaitLink = 3 * 60 * 1000;
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException e) {
            logger.log(e);
            final String errormessage = "Invalid API response";
            if (link == null) {
                throw new AccountUnavailableException(errormessage, defaultWaitAccount);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormessage, defaultWaitLink);
            }
        }
        final Boolean success = (Boolean) entries.get("success");
        if (Boolean.TRUE.equals(success)) {
            /* No error */
            return entries;
        }
        String errormsg = (String) entries.get("message");
        if (errormsg == null) {
            /* Linkcheck */
            errormsg = (String) entries.get("status");
        }
        if (errormsg == null || errormsg.equalsIgnoreCase("ok")) {
            /* No error */
            return entries;
        }
        if (errormsg.equalsIgnoreCase("Not found")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (errormsg.equalsIgnoreCase("Login ERROR")) {
            throw new AccountInvalidException();
        } else {
            logger.info("Unknown API error: " + errormsg);
            if (link == null) {
                throw new AccountUnavailableException(errormsg, defaultWaitAccount);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, errormsg, defaultWaitLink);
            }
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> userinfo = loginAndGetDirectDownloadlink(account, null);
        final String premium_date_expire = (String) userinfo.get("premium_date_expire");
        if (premium_date_expire != null) {
            ai.setValidUntil(TimeFormatter.getMilliSeconds(premium_date_expire, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH), br);
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
        }
        final String transfer_leftStr = (String) userinfo.get("transfer_left");
        if (transfer_leftStr != null) {
            ai.setTrafficLeft(SizeFormatter.getSize(transfer_leftStr));
        } else {
            ai.setUnlimitedTraffic();
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
        if (acc != null && acc.getType() == AccountType.PREMIUM) {
            return false;
        } else {
            return true;
        }
    }
}