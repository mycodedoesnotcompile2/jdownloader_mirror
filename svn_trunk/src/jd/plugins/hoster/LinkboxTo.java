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

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 50921 $", interfaceVersion = 3, names = {}, urls = {})
public class LinkboxTo extends PluginForHost {
    public LinkboxTo(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/signup");
    }

    public static final String   PROPERTY_DIRECTURL              = "directlink";
    private static final String  PROPERTY_ACCOUNT_TOKEN          = "token";
    /* 2025-03-31: For downloading, an account is needed. */
    private static final boolean ALLOW_DOWNLOADS_WITHOUT_ACCOUNT = false;

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/terms-of-service";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "linkbox.to", "sharezweb.com", "linkbox.cloud" });
        return ret;
    }

    @Override
    public String rewriteHost(final String host) {
        /* 2023-05-10: Main domain has changed from sharezweb.com to linkbox.to. */
        return this.rewriteHost(getPluginDomains(), host);
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
            ret.add("https?://(?:(?:www|[a-z]{2})\\.)?" + buildHostsPatternPart(domains) + "/(?:file|f-detail)/([a-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 1;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return 1;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
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
        String ret = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
        // e=>{if(!e||e.startsWith("fp"))return"";const t=e.slice(0,e.length-1);e.slice(e.length-1);return window.atob(t)}
        if (ret == null || ret.startsWith("fp")) {
            return ret;
        }
        ret = ret.substring(0, ret.length() - 1);
        ret = Base64.decodeToString(ret);
        return ret;
    }

    private String getWebapiBase() {
        return "https://www." + getHost() + "/api";
    }

    public static UrlQuery getBaseQuery() {
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("platform", "web");
        query.appendEncoded("pf", "web");
        query.appendEncoded("lan", "en");
        return query;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return requestFileInformation(link, null);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws IOException, PluginException {
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(fid);
        }
        final String token = account != null ? account.getStringProperty(PROPERTY_ACCOUNT_TOKEN) : "";
        final UrlQuery query = getBaseQuery();
        query.appendEncoded("itemId", fid);
        query.appendEncoded("needUser", "1");
        query.appendEncoded("needTpInfo", "1");
        query.appendEncoded("token", token);
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Accept", "application/json, text/plain, */*");
        brc.getHeaders().put("Referer", "https://www.linkbox.cloud/f-detail/" + fid);
        brc.getPage(getWebapiBase() + "/file/detail?" + query.toString());
        if (brc.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> data = (Map<String, Object>) entries.get("data");
        /* E.g. when invalid fileID is used: {"data":null,"status":1} */
        if (data == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> itemInfo = (Map<String, Object>) data.get("itemInfo");
        parseFileInfoAndSetFilename(link, account, itemInfo);
        return AvailableStatus.TRUE;
    }

    public static void parseFileInfoAndSetFilename(final DownloadLink link, final Account account, final Map<String, Object> ressource) {
        link.setVerifiedFileSize(((Number) ressource.get("size")).longValue());
        link.setFinalFileName(ressource.get("name").toString());
        link.setProperty(getDirectlinkproperty(account), ressource.get("url"));
        link.setAvailable(true);
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        if (!ALLOW_DOWNLOADS_WITHOUT_ACCOUNT) {
            throw new AccountRequiredException();
        }
        handleDownload(link, null);
    }

    private static String getDirectlinkproperty(final Account acc) {
        if (acc == null) {
            /* no account, yes we can expect captcha */
            return "free_directurl";
        } else {
            return "account_ " + acc.getType() + "_directurl";
        }
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        if (account != null) {
            this.login(account, false);
        }
        final String directlinkproperty = getDirectlinkproperty(account);
        if (!attemptStoredDownloadurlDownload(link, directlinkproperty)) {
            requestFileInformation(link, account);
            final String dllink = link.getStringProperty(directlinkproperty);
            if (StringUtils.isEmpty(dllink)) {
                logger.warning("Failed to find final downloadurl");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(null));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getCompleteContentLength() == 0) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Corrupt or empty file");
                } else if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
                } else {
                    /* 2023-11-13: API does not necessarily report abused files as offline. */
                    throw new PluginException(LinkStatus.ERROR_FATAL, "File offline or broken");
                }
            }
            preDownloadErrorCheck(dl.getConnection());
            link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        }
        dl.startDownload();
    }

    private void preDownloadErrorCheck(final URLConnectionAdapter con) throws PluginException {
        final String etag = con.getRequest().getResponseHeader("etag");
        if (StringUtils.equalsIgnoreCase(etag, "\"28a14757bfe1522e447b544b7d7e5885\"")) {
            /* 2023-11-13: Dummy video-file for abused video content. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link, final String directlinkproperty) throws Exception {
        final String url = link.getStringProperty(directlinkproperty);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, this.isResumeable(link, null), this.getMaxChunks(null));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                preDownloadErrorCheck(dl.getConnection());
                return true;
            } else {
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            link.removeProperty(directlinkproperty);
            logger.log(e);
            try {
                dl.getConnection().disconnect();
            } catch (Throwable ignore) {
            }
            return false;
        }
    }

    private Map<String, Object> login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            final String storedToken = account.getStringProperty(PROPERTY_ACCOUNT_TOKEN);
            if (cookies != null && storedToken != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
                if (!force) {
                    /* Don't validate cookies */
                    return null;
                }
                br.getPage(this.getWebapiBase() + "/user/info?token=" + Encoding.urlEncode(storedToken));
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final int status = ((Number) entries.get("status")).intValue();
                if (status == 1) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return (Map<String, Object>) entries.get("data");
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                    /* Token is invalid -> Do not use it again */
                    account.removeProperty(PROPERTY_ACCOUNT_TOKEN);
                }
            }
            logger.info("Performing full login");
            final UrlQuery query = getBaseQuery();
            query.appendEncoded("email", account.getUser());
            query.appendEncoded("pwd", account.getPass());
            br.getHeaders().put("Referer", "https://www." + getHost() + "/email");
            br.getPage(this.getWebapiBase() + "/user/login_email?" + query.toString());
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final int status = ((Number) entries.get("status")).intValue();
            if (status != 1) {
                final String msg = (String) entries.get("msg");
                /* E.g.: {"msg":"LoginEmailNoAccount","status":50001} */
                /* {"msg":"LoginEmailErrAccount","status":50002} */
                // 5001 = Invalid email
                // 5002 = Invalid password
                throw new AccountInvalidException(msg);
            }
            final Map<String, Object> data = (Map<String, Object>) entries.get("data");
            final String token = (String) data.get("token");
            if (StringUtils.isEmpty(token) && StringUtils.isEmpty(storedToken)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            account.setProperty(PROPERTY_ACCOUNT_TOKEN, token);
            account.saveCookies(br.getCookies(br.getHost()), "");
            return (Map<String, Object>) data.get("userInfo");
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        final Map<String, Object> userInfo = login(account, true);
        final String email = (String) userInfo.get("email");
        if (account.loadUserCookies() != null && email != null) {
            account.setUser(email);
        }
        final Number spaceUsedBytes = (Number) userInfo.get("size_curr");
        if (spaceUsedBytes != null) {
            ai.setUsedSpace(spaceUsedBytes.longValue());
        }
        final long vip_end = ((Number) userInfo.get("vip_end")).longValue();
        if (vip_end > System.currentTimeMillis()) {
            ai.setValidUntil(vip_end * 1000, br);
            /* As long as the account is not expired, it is considered to be a premium account. */
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
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
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        /* 2022-12-20: No captchas needed at all. */
        return false;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (!ALLOW_DOWNLOADS_WITHOUT_ACCOUNT && account == null) {
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
}