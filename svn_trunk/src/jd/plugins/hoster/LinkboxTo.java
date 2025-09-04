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
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

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
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51437 $", interfaceVersion = 3, names = {}, urls = {})
public class LinkboxTo extends PluginForHost {
    public LinkboxTo(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/signup");
    }

    public static final String  PROPERTY_DIRECTURL                 = "directlink";
    private static final String PROPERTY_ACCOUNT_TOKEN             = "token";
    private static final String PROPERTY_ACCOUNT_STORAGE_MAX_BYTES = "storage_max_bytes";

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        /* 2025-04-04: There are problems with downloading in ipv4/6 mixed mode -> Use IPV4 only */
        br.setIPVersion(IPVERSION.IPV4_ONLY);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/terms-of-service";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "linkbox.to", "sharezweb.com", "linkbox.cloud", "linkbox.fun", "lbx.mobi" });
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
        final String fid = getInternalFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public String getPluginContentURL(DownloadLink link) {
        final String publicFID = getPublicFID(link);
        if (publicFID == null) {
            return null;
        }
        return "https://www." + getHost() + "/f-detail/" + publicFID;
    }

    @Override
    public String buildExternalDownloadURL(DownloadLink downloadLink, PluginForHost buildForThisPlugin) {
        return getPluginContentURL(downloadLink);
    }

    private String getPublicFID(final DownloadLink link) {
        String ret = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
        if (ret == null || !ret.startsWith("fp")) {
            return ret;
        }
        return Base64.encode(ret) + "0";
    }

    /** Returns fileID as is in the added URL. */
    private String getFIDRaw(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    private String getInternalFID(final DownloadLink link) {
        String ret = getFIDRaw(link);
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
        final String fid = this.getInternalFID(link);
        if (!link.isNameSet()) {
            /**
             * Fallback <br>
             * Set raw fileID so for offline items, the ID the user sees in GUI is exactly the one from the link added by the user.
             */
            link.setName(getFIDRaw(link));
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
        final Map<String, Object> data = this.callAPI(link, account, "/file/detail", query);
        /* E.g. when invalid fileID is used: {"data":null,"status":1} */
        if (data == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> itemInfo = (Map<String, Object>) data.get("itemInfo");
        parseFileInfoAndSetFilename(this, link, account, itemInfo);
        return AvailableStatus.TRUE;
    }

    public static void parseFileInfoAndSetFilename(final Plugin plg, final DownloadLink link, final Account account, final Map<String, Object> ressource) {
        link.setVerifiedFileSize(((Number) ressource.get("size")).longValue());
        String filename = ressource.get("name").toString();
        final String ext = (String) ressource.get("sub_type");
        if (!StringUtils.isEmpty(ext)) {
            filename = filename.trim();
            filename = plg.applyFilenameExtension(filename, ext);
        }
        link.setFinalFileName(filename);
        link.setProperty(getDirectlinkproperty(account), ressource.get("url"));
        link.setAvailable(true);
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
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
            if (account == null) {
                /* Account required to download this item */
                throw new AccountRequiredException();
            }
            requestFileInformation(link, account);
            String dllink = link.getStringProperty(directlinkproperty);
            if (StringUtils.isEmpty(dllink) && account != null) {
                /* File needs to be imported into account to be able to download it -> Let's try that */
                /* First check if file woult fit in account storage */
                final long storage_max_bytes = account.getLongProperty(PROPERTY_ACCOUNT_STORAGE_MAX_BYTES, 5368709120l); // Default = 5GB
                if (link.getVerifiedFileSize() > storage_max_bytes) {
                    /* E.g. free account has max storage size of 5GB but file is bigger than 5GB. */
                    errorInsufficientStorageSpaceAvailable(link.getVerifiedFileSize() - storage_max_bytes);
                    /* This code should never be reached */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final long expectedSize = link.getVerifiedFileSize();
                final AccountInfo ai = account.getAccountInfo();
                if (ai != null && expectedSize > 0) {
                    final long space_left_bytes = storage_max_bytes - ai.getUsedSpace();
                    if (link.getVerifiedFileSize() > space_left_bytes) {
                        final long requiredSpaceBytes = link.getVerifiedFileSize() - space_left_bytes;
                        errorInsufficientStorageSpaceAvailable(requiredSpaceBytes);
                        /* This code should never be reached */
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
                synchronized (account) {
                    final String fid = this.getInternalFID(link);
                    final UrlQuery queryImport = getBaseQuery();
                    queryImport.appendEncoded("isImport", "1");
                    queryImport.appendEncoded("pid", "0");
                    queryImport.appendEncoded("itemIds", fid);
                    queryImport.appendEncoded("shareToken", "");
                    this.callAPI(link, account, "/file/repost", queryImport);
                    /* Give the server a second to "copy" the file into our account */
                    this.sleep(1000, link);
                    /* E.g. {"data":{"sizeDiff":0,"sizeNeed":0},"msg":"suc","status":1} */
                    final UrlQuery listFilesQuery = getBaseQuery();
                    listFilesQuery.appendEncoded("sortField", "utime");
                    /* Sort from new to old so that hopefully our imported item is on the first page */
                    listFilesQuery.appendEncoded("sortAsc", "0");
                    listFilesQuery.appendEncoded("pageNo", "1");
                    listFilesQuery.appendEncoded("pageSize", "50");
                    listFilesQuery.appendEncoded("pid", "0");
                    final Map<String, Object> entries = this.callAPI(link, account, "/file/my_file_list/web", listFilesQuery);
                    /* When user has zero files in account, "list" will be null. */
                    final List<Map<String, Object>> filelist = (List<Map<String, Object>>) entries.get("list");
                    String clonedItemID = null;
                    if (filelist != null) {
                        for (final Map<String, Object> file : filelist) {
                            final String item_rid = file.get("item_rid").toString();
                            if (item_rid.equals(fid)) {
                                dllink = file.get("url").toString();
                                clonedItemID = file.get("item_id").toString();
                                break;
                            }
                        }
                    }
                    if (StringUtils.isEmpty(clonedItemID)) {
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to find copied file in account");
                    } else if (StringUtils.isEmpty(dllink)) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    /**
                     * Delete file from account to save that valuable storage space. <br>
                     * The direct link we just obtained will still work just fine so we can download the file :)
                     */
                    final UrlQuery deleteItemQuery = getBaseQuery();
                    deleteItemQuery.appendEncoded("dirIds", "");
                    deleteItemQuery.appendEncoded("itemIds", clonedItemID);
                    this.callAPI(link, account, "/file/delete", deleteItemQuery);
                }
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(account));
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
            final Map<String, Object> data = this.checkErrorsAPI(null, account);
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
        // final Boolean auto_renew = (Boolean) userInfo.get("auto_renew");
        final long spaceUsedBytes = ((Number) userInfo.get("size_curr")).longValue();
        final long spaceMaxBytes = ((Number) userInfo.get("size_cap")).longValue();
        ai.setUsedSpace(spaceUsedBytes);
        account.setProperty(PROPERTY_ACCOUNT_STORAGE_MAX_BYTES, spaceMaxBytes);
        final long vip_end = ((Number) userInfo.get("vip_end")).longValue() * 1000;
        if (vip_end > System.currentTimeMillis()) {
            ai.setValidUntil(vip_end, br);
            /* As long as the account is not expired, it is considered to be a premium account. */
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
        }
        ai.setTrafficLeft(spaceMaxBytes - spaceUsedBytes);
        ai.setTrafficMax(spaceMaxBytes);
        ai.setTrafficRefill(true);
        /* Ignore traffic we set */
        ai.setSpecialTraffic(true);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    public Map<String, Object> callAPI(final DownloadLink link, final Account account, final String path, final UrlQuery query) throws IOException, PluginException {
        if (account != null) {
            final String token = account.getStringProperty(PROPERTY_ACCOUNT_TOKEN);
            query.appendEncoded("token", token);
        }
        br.getPage(this.getWebapiBase() + path + "?" + query.toString());
        return this.checkErrorsAPI(link, account);
    }

    private Map<String, Object> checkErrorsAPI(final DownloadLink link, final Account account) throws PluginException {
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String msg = "Invalid API response";
            final long waitMillis = 60 * 1000l;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, waitMillis);
            } else {
                throw new AccountUnavailableException(msg, 60 * 1000);
            }
        }
        /* {"msg":"LoginEmailErrAccount","status":50002} */
        final String msg = (String) entries.get("error");
        final Number statusO = (Number) entries.get("status");
        final Map<String, Object> data = (Map<String, Object>) entries.get("data");
        if (statusO != null && statusO.intValue() == 1) {
            return data;
        }
        if (msg == null || statusO == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final int status = statusO.intValue();
        final Number sizeDiff = data != null ? (Number) data.get("sizeDiff") : null;
        if (link != null && status == 50001 && sizeDiff != null) {
            errorInsufficientStorageSpaceAvailable(sizeDiff.longValue());
            /* This code should never be reached */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (status == 50001 || status == 50002) {
            throw new AccountInvalidException(msg);
        } else {
            if (link == null) {
                throw new AccountUnavailableException(msg, 5 * 60 * 1000);
            } else {
                throw new PluginException(LinkStatus.ERROR_FATAL, msg);
            }
        }
    }

    private void errorInsufficientStorageSpaceAvailable(final long missingSpaceBytes) throws AccountRequiredException {
        final SIZEUNIT maxSizeUnit = (SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue();
        throw new AccountRequiredException(SIZEUNIT.formatValue(maxSizeUnit, missingSpaceBytes) + " more storage required to download this file");
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
        if (account == null && !link.hasProperty(getDirectlinkproperty(null))) {
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