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
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 50518 $", interfaceVersion = 3, names = {}, urls = {})
public class SavefilesCom extends PluginForHost {
    public SavefilesCom(PluginWrapper wrapper) {
        super(wrapper);
        // this.enablePremium("https://" + getHost() + "/pricing");
    }

    private final String API_BASE = "https://api.savefiles.com";

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setAllowedResponseCodes(400);
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms-and-condition";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "savefiles.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/v/([A-Za-z0-9\\-_]{10,})");
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
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        this.setBrowserExclusive();
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            /* Expect video files only */
            link.setName(fid + ".mp4");
        }
        final Map<String, Object> data = this.callAPI("/user/getFileUrl/" + fid, link);
        final Number fileSizeMB = (Number) data.get("fileSize");
        link.setFinalFileName(data.get("fileName").toString());
        if (fileSizeMB != null) {
            link.setDownloadSize(fileSizeMB.longValue() * 1024 * 1024);
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
        String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            requestFileInformation(link);
            final String fid = this.getFID(link);
            /* Get file details to obtain internal filename */
            final Map<String, Object> fileDetails = this.callAPI("/user/getFileDetails/" + fid, link);
            final String internalFilename = fileDetails.get("name").toString();
            final Map<String, Object> data = this.callAPI("/user/getFileUrlV2?filename=" + Encoding.urlEncode(internalFilename) + "&key=" + fid, link);
            dllink = data.get("downloadUrl").toString();
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        /* Important else we will get http response code 403. */
        br.getHeaders().put("Referer", "https://" + getHost() + "/");
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
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

    private Map<String, Object> callAPI(final String path, final DownloadLink link) throws Exception {
        return callAPI(path, null, link);
    }

    private Map<String, Object> callAPI(final String path, final Account account, final DownloadLink link) throws Exception {
        br.getPage(API_BASE + path);
        return handleErrors(account, link);
    }

    /** See https://docs.alldebrid.com/#all-errors */
    private Map<String, Object> handleErrors(final Account account, final DownloadLink link) throws PluginException, Exception {
        /* 2020-03-25: E.g. {"status": "error", "error": {"code": "AUTH_BAD_APIKEY","message": "The auth apikey is invalid"}} */
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException jme) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Bad API answer");
        }
        if (Boolean.TRUE.equals(entries.get("success"))) {
            /* No error */
            final Map<String, Object> data = (Map<String, Object>) entries.get("data");
            return data;
        }
        final String message = entries.get("message").toString();
        if (message.equalsIgnoreCase("")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            throw new PluginException(LinkStatus.ERROR_FATAL, message);
        }
    }
    // private boolean login(final Account account, final boolean force) throws Exception {
    // synchronized (account) {
    // br.setFollowRedirects(true);
    // br.setCookiesExclusive(true);
    // final Cookies cookies = account.loadCookies("");
    // if (cookies != null) {
    // logger.info("Attempting cookie login");
    // this.br.setCookies(this.getHost(), cookies);
    // if (!force) {
    // /* Don't validate cookies */
    // return false;
    // }
    // br.getPage("https://" + this.getHost() + "/");
    // if (this.isLoggedin(br)) {
    // logger.info("Cookie login successful");
    // /* Refresh cookie timestamp */
    // account.saveCookies(br.getCookies(br.getHost()), "");
    // return true;
    // } else {
    // logger.info("Cookie login failed");
    // br.clearCookies(null);
    // }
    // }
    // logger.info("Performing full login");
    // br.getPage("https://" + this.getHost() + "/login.php");
    // final Form loginform = br.getFormbyProperty("name", "login");
    // if (loginform == null) {
    // throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find loginform");
    // }
    // loginform.put("username", Encoding.urlEncode(account.getUser()));
    // loginform.put("password", Encoding.urlEncode(account.getPass()));
    // br.postPage("", "email=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
    // if (!isLoggedin(br)) {
    // throw new AccountInvalidException();
    // }
    // account.saveCookies(br.getCookies(br.getHost()), "");
    // return true;
    // }
    // }
    //
    // private boolean isLoggedin(final Browser br) {
    // return br.containsHTML("/logout");
    // }
    //
    // @Override
    // public AccountInfo fetchAccountInfo(final Account account) throws Exception {
    // final AccountInfo ai = new AccountInfo();
    // login(account, true);
    // String space = br.getRegex("").getMatch(0);
    // if (space != null) {
    // ai.setUsedSpace(space.trim());
    // }
    // ai.setUnlimitedTraffic();
    // if (br.containsHTML("")) {
    // account.setType(AccountType.FREE);
    // /* free accounts can still have captcha */
    // account.setConcurrentUsePossible(false);
    // } else {
    // final String expire = br.getRegex("").getMatch(0);
    // if (expire == null) {
    // throw new AccountInvalidException();
    // } else {
    // ai.setValidUntil(TimeFormatter.getMilliSeconds(expire, "dd MMMM yyyy", Locale.ENGLISH));
    // }
    // account.setType(AccountType.PREMIUM);
    // account.setConcurrentUsePossible(true);
    // }
    // return ai;
    // }
    //
    // @Override
    // public void handlePremium(final DownloadLink link, final Account account) throws Exception {
    // this.handleDownload(link, account);
    // }
    //
    // @Override
    // public int getMaxSimultanPremiumDownloadNum() {
    // return Integer.MAX_VALUE;
    // }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}