//jDownloader - Downloadmanager
//Copyright (C) 2013  JD-Team support@jdownloader.org
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 50303 $", interfaceVersion = 3, names = { "rapidb.it" }, urls = { "" })
public class RapidbIt extends PluginForHost {
    private final String                 API_BASE                    = "https://rapidb.it/api";
    private static MultiHosterManagement mhm                         = new MultiHosterManagement("rapidb.it");
    private final String                 PROPERTY_ACCOUNT_TOKEN      = "login_token";
    private final String                 PROPERTY_SERVERSIDE_FILE_ID = "file_id";

    /**
     * 2022-07-19: While their API docs state all possible errormessages, their API does not return any errormessage - only error-codes.
     * </br>
     * This is where this static mapping comes into play.
     */
    private Map<Integer, String> getErrorCodeMap() {
        final Map<Integer, String> ret = new HashMap<Integer, String>();
        ret.put(101, "Unknown PHP error");
        ret.put(102, "The selected controller does not exist");
        ret.put(103, "The selected controller exists but its source file was not found");
        ret.put(104, "Requested path/method does not exist");
        ret.put(105, "Your IP was banned due too high number of failed login attempts");
        ret.put(106, "You are logged off (access token is not valid or expired)");
        ret.put(107, "Owned permissions (scope) does not allow access to the controller/action");
        ret.put(108, "Input data contains invalid characters (only UTF-8)");
        return ret;
    }

    @SuppressWarnings("deprecation")
    public RapidbIt(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + this.getHost() + "/pricing");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String getAGBLink() {
        return "https://" + this.getHost() + "/page/tos";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
    }

    private String getPropertyKey(final String property) {
        return this.getHost() + "_" + property;
    }

    private String getMultihosterFileID(final DownloadLink link) {
        return link.getStringProperty(getPropertyKey(PROPERTY_SERVERSIDE_FILE_ID));
    }

    private void setMultihosterFileID(final DownloadLink link, final String file_id) {
        link.setProperty(getPropertyKey(PROPERTY_SERVERSIDE_FILE_ID), file_id);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        this.login(account, false);
        if (!attemptStoredDownloadurlDownload(link)) {
            String file_id = getMultihosterFileID(link);
            if (file_id == null) {
                logger.info("Creating internal file_id");
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("url", link.getDefaultPlugin().buildExternalDownloadURL(link, this));
                /*
                 * This timestamp is later used to notify the user that everything requested within a specified timespan has been
                 * downloaded.
                 */
                postdata.put("group_id", System.currentTimeMillis() / 1000);
                postdata.put("notif_db", false);
                postdata.put("notif_email", false);
                br.postPageRaw(API_BASE + "/services/downloadfile", JSonStorage.serializeToJson(postdata));
                this.checkErrors(account, link);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                handleErrorMap(account, link, entries);
                file_id = entries.get("file_id").toString();
                /* Save this ID to re-use on next try. */
                this.setMultihosterFileID(link, file_id);
            }
            logger.info("Trying to init download for internal file_id: " + file_id);
            final UrlQuery query = new UrlQuery();
            query.add("id", file_id);
            query.add("sort", "id");
            query.add("order", "asc");
            query.add("offset", "0");
            query.add("limit", "1");
            /*
             * This will return a list of files based on our criteria --> This list should only contain one result which is the file we
             * want.
             */
            br.getPage(API_BASE + "/files?" + query.toString());
            final Map<String, Object> dlresponse = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            handleErrorMap(account, link, dlresponse);
            final List<Map<String, Object>> files = (List<Map<String, Object>>) dlresponse.get("result");
            if (files.isEmpty()) {
                /* We're too fast or server too slow: Expected file is not yet on serverside queue list. */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Serverside download in progress (serverside file_id " + file_id + " is not yet on queue list)", 5 * 1000l);
            }
            final Map<String, Object> file = files.get(0);
            final String dllink = (String) file.get("download_url");
            if (StringUtils.isEmpty(dllink)) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Serverside download in progress " + file.get("download_percent") + "%", 10 * 1000l);
            }
            link.setProperty(this.getPropertyKey("directlink"), dllink);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                mhm.handleErrorGeneric(account, link, "unknown_dl_error", 10, 5 * 60 * 1000l);
            }
        }
        this.dl.startDownload();
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link) throws Exception {
        final String directurlproperty = this.getPropertyKey("directlink");
        final String url = link.getStringProperty(directurlproperty);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        boolean valid = false;
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, this.isResumeable(link, null), this.getMaxChunks(link, null));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                valid = true;
                return true;
            } else {
                link.removeProperty(directurlproperty);
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            logger.log(e);
            return false;
        } finally {
            if (!valid) {
                try {
                    dl.getConnection().disconnect();
                } catch (Throwable ignore) {
                }
                dl = null;
            }
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        final Map<String, Object> user = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        br.getPage(API_BASE + "/system/config");
        final Map<String, Object> apiconfig = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final AccountInfo ai = new AccountInfo();
        ai.setTrafficRefill(false);
        ai.setCreateTime(((Number) user.get("created")).longValue());
        final int level_id = ((Number) user.get("level_id")).intValue();
        final List<Map<String, Object>> levels = (List<Map<String, Object>>) apiconfig.get("levels");
        /* Information about users' current package. */
        final Map<String, Object> level = levels.get(level_id - 1);
        if (level_id == 0) {
            /* Free */
            account.setType(AccountType.FREE);
        } else {
            /* Bronze, Silver, Gold, Platinum */
            account.setType(AccountType.PREMIUM);
        }
        ai.setStatus(level.get("name").toString());
        account.setMaxSimultanDownloads(((Number) level.get("max_sim_downloads")).intValue());
        /* Traffic the user bought in this package */
        final long pointsBytes = ((Number) user.get("points")).longValue();
        /* Bought traffic + (daily_free_traffic_mb - daily_used_traffic_bytes) */
        ai.setTrafficLeft(pointsBytes + (((Number) level.get("points_free_mb")).longValue() * 1000 * 1000) - ((Number) user.get("points_free_used")).longValue());
        ai.setTrafficMax(pointsBytes);
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        final List<Map<String, Object>> filehostings = (List<Map<String, Object>>) apiconfig.get("filehostings");
        for (final Map<String, Object> filehosting : filehostings) {
            final String name = filehosting.get("name").toString();
            final MultiHostHost mhost = new MultiHostHost();
            mhost.setName(name);
            mhost.addDomains((List<String>) filehosting.get("domains"));
            /*
             * Important: Add name as domain since they got some hosts listed with domain starting with "www." which distracts our matching.
             */
            mhost.addDomain(name);
            if (((Number) filehosting.get("status")).intValue() != 1) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                mhost.setStatusText((String) filehosting.get("status_reason"));
            }
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    private void login(final Account account, final boolean validateLogins) throws IOException, PluginException, InterruptedException {
        synchronized (account) {
            if (account.hasProperty(PROPERTY_ACCOUNT_TOKEN)) {
                logger.info("Trying to login via token");
                br.getHeaders().put("Auth", account.getStringProperty(PROPERTY_ACCOUNT_TOKEN));
                if (!validateLogins) {
                    /* Do not verify logins */
                    return;
                } else {
                    logger.info("Validating login token...");
                    br.getPage(API_BASE + "/users/me");
                    try {
                        checkErrors(account, null);
                        logger.info("Token login successful");
                        return;
                    } catch (final PluginException e) {
                        logger.exception("Token login failed", e);
                        account.removeProperty(PROPERTY_ACCOUNT_TOKEN);
                    }
                }
            }
            logger.info("Performing full login");
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("email", account.getUser());
            postdata.put("password", account.getPass());
            postdata.put("googleauth", null);
            postdata.put("never_expires", true);
            br.postPageRaw(API_BASE + "/users/login", JSonStorage.serializeToJson(postdata));
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String token = (String) entries.get("access_token");
            if (StringUtils.isEmpty(token)) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            }
            br.getHeaders().put("Auth", token);
            account.setProperty(PROPERTY_ACCOUNT_TOKEN, token);
        }
    }

    private void checkErrors(final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        try {
            final Object jsonO = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
            if (jsonO == null || !(jsonO instanceof Map)) {
                return;
            }
            handleErrorMap(account, link, (Map<String, Object>) jsonO);
        } catch (final JSonMapperException jme) {
            if (link != null) {
                mhm.handleErrorGeneric(account, link, "Bad API answer", 50, 5 * 60 * 1000l);
            } else {
                throw Exceptions.addSuppressed(new AccountUnavailableException("Bad API answer", 1 * 60 * 1000l), jme);
            }
        }
    }

    private void handleErrorMap(final Account account, final DownloadLink link, final Map<String, Object> entries) throws PluginException, InterruptedException {
        final Object errorO = entries.get("error");
        if (errorO == null) {
            /* No error */
            return;
        }
        final int errorcode = ((Number) errorO).intValue();
        if (errorcode == 0) {
            /* No error */
            return;
        }
        final Map<Integer, String> errorCodeMapping = getErrorCodeMap();
        if (errorCodeMapping.containsKey(errorcode)) {
            /* Known errorcode */
            final String errorMsg = errorCodeMapping.get(errorcode);
            if (errorcode == 105) {
                /* IP is temporarily banned */
                throw new AccountUnavailableException(errorMsg, 5 * 60 * 1000l);
            } else if (errorcode == 106) {
                /* Invalid logins */
                throw new AccountInvalidException(errorMsg);
            } else if (link != null) {
                mhm.handleErrorGeneric(account, link, "Error " + errorcode, 50, 1 * 60 * 1000l);
            } else {
                throw new AccountUnavailableException(errorMsg, 3 * 60 * 1000l);
            }
        } else {
            /* Unknown errorcode */
            if (link != null) {
                mhm.handleErrorGeneric(account, link, "Error " + errorcode, 50, 1 * 60 * 1000l);
            } else {
                throw new AccountInvalidException("Error " + errorcode);
            }
        }
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        if (link != null) {
            /* This will allow our plugin to start a new serverside "job" on next try. */
            this.setMultihosterFileID(link, null);
        }
    }
}