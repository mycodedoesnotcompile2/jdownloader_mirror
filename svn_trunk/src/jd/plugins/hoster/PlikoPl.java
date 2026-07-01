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
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
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
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 52936 $", interfaceVersion = 3, names = { "pliko.pl" }, urls = { "" })
public class PlikoPl extends PluginForHost {
    private static final String          API_BASE                   = "https://pliko.pl/jd2";
    private static MultiHosterManagement mhm                        = new MultiHosterManagement("pliko.pl");
    private static final String          PROPERTY_APIKEY            = "plikopl_apikey";
    private static final String          PROPERTY_APIKEY_EXPIRES    = "plikopl_apikey_expires_timestamp";
    private static final String          PROPERTY_JOB_ID            = "plikopl_job_id";
    private static final String          PROPERTY_RESUMABLE         = "plikopl_resumable";
    private static final String          PROPERTY_MAXCHUNKS         = "plikopl_maxchunks";
    private static final long            APIKEY_RENEW_BEFORE_EXPIRE = 35 * 60 * 1000l;

    public PlikoPl(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/premium.html");
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/";
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        /* this should never be called */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        login(account);
        final String dllink = getDllink(account, link);
        final boolean resumable = link.getBooleanProperty(PROPERTY_RESUMABLE, true);
        int maxChunks = (int) link.getLongProperty(PROPERTY_MAXCHUNKS, 1);
        if (maxChunks > 1) {
            maxChunks = -maxChunks;
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, resumable, maxChunks);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            mhm.handleErrorGeneric(account, link, "Final downloadlink did not lead to file", 50);
        }
        this.dl.startDownload();
    }

    private String getDllink(final Account account, final DownloadLink link) throws Exception {
        final String apikey = account.getStringProperty(PROPERTY_APIKEY);
        String jobID = link.getStringProperty(PROPERTY_JOB_ID);
        if (StringUtils.isEmpty(jobID)) {
            br.postPage(API_BASE + "/download.php", "apikey=" + Encoding.urlEncode(apikey) + "&link=" + Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this)));
        } else {
            br.postPage(API_BASE + "/status.php", "apikey=" + Encoding.urlEncode(apikey) + "&job_id=" + Encoding.urlEncode(jobID));
        }
        Map<String, Object> resp = handleErrors(account, link);
        int loopCounter = 0;
        final int maxLoops = 200;
        while (!isDone(resp)) {
            if (this.isAbort()) {
                throw new InterruptedException();
            }
            jobID = resp.get("job_id").toString();
            link.setProperty(PROPERTY_JOB_ID, jobID);
            final Object progress = resp.get("progress_percent");
            final long waitSeconds = ((Number) resp.get("retry_in_seconds")).longValue();
            final String infotext = "Pliko.pl: " + resp.get("msg") + (progress != null ? " (" + progress + "%)" : "");
            if (loopCounter >= maxLoops) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server transfer retry count exhausted", 5 * 60 * 1000l);
            }
            sleep(waitSeconds * 1000l, link, infotext);
            loopCounter++;
            br.postPage(API_BASE + "/status.php", "apikey=" + Encoding.urlEncode(apikey) + "&job_id=" + Encoding.urlEncode(jobID));
            resp = handleErrors(account, link);
        }
        final Map<String, Object> data = (Map<String, Object>) resp.get("data");
        final String directURL = data.get("direct_url").toString();
        link.removeProperty(PROPERTY_JOB_ID);
        link.setProperty(PROPERTY_RESUMABLE, data.get("resumable"));
        link.setProperty(PROPERTY_MAXCHUNKS, ((Number) data.get("maxchunks")).longValue());
        return directURL;
    }

    private boolean isDone(final Map<String, Object> resp) {
        if (Boolean.TRUE.equals(resp.get("success"))) {
            return true;
        }
        final Map<String, Object> data = (Map<String, Object>) resp.get("data");
        return data != null && Boolean.TRUE.equals(data.get("ready"));
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        /*
         * TODO: This always performs a full login (= creates a new login session serverside) just to get fresh traffic/host data. Ask
         * Pliko.pl admin for a way to fetch this account data via the stored apikey instead so we do not need to re-login every time
         * fetchAccountInfo runs.
         */
        final Map<String, Object> accountData = this.login(account);
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        final List<Map<String, Object>> hostsList = (List<Map<String, Object>>) accountData.get("supportedhosts");
        for (final Map<String, Object> hostmap : hostsList) {
            final MultiHostHost mhost = new MultiHostHost();
            mhost.setName(hostmap.get("host").toString());
            mhost.setDomains((List<String>) hostmap.get("domains"));
            mhost.setMaxChunks(((Number) hostmap.get("maxChunks")).intValue());
            mhost.setMaxDownloads(((Number) hostmap.get("maxDownloads")).intValue());
            mhost.setResumable((Boolean) hostmap.get("resumable"));
            mhost.setTrafficLeft(((Number) hostmap.get("trafficleft")).longValue());
            if (Boolean.FALSE.equals(hostmap.get("currently_working"))) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        if (Boolean.TRUE.equals(accountData.get("premium"))) {
            account.setType(AccountType.PREMIUM);
        } else {
            account.setType(AccountType.FREE);
        }
        ai.setTrafficLeft(((Number) accountData.get("trafficleft")).longValue());
        ai.setTrafficMax(((Number) accountData.get("trafficmax")).longValue());
        ai.setValidUntil(System.currentTimeMillis() + ((Number) accountData.get("timeleft")).longValue());
        account.setConcurrentUsePossible(true);
        return ai;
    }

    /** Reuses the stored apikey as long as it is not about to expire, otherwise performs a full login. */
    private Map<String, Object> login(final Account account) throws IOException, PluginException, InterruptedException {
        synchronized (account) {
            final String apikey = account.getStringProperty(PROPERTY_APIKEY);
            final long expiresTimestamp = account.getLongProperty(PROPERTY_APIKEY_EXPIRES, 0);
            if (!StringUtils.isEmpty(apikey) && System.currentTimeMillis() < expiresTimestamp - APIKEY_RENEW_BEFORE_EXPIRE) {
                /* Trust stored apikey, it is not about to expire yet. */
                logger.info("Trust stored apikey, it is not about to expire yet | Remaining validity: " + (expiresTimestamp - System.currentTimeMillis()) / 1000 + " seconds");
                br.getPage(API_BASE + "/account.php?apikey=" + apikey);
                final Map<String, Object> resp = handleErrors(account, null);
                final Map<String, Object> data = (Map<String, Object>) resp.get("data");
                return data;
            }
            return performFullLogin(account);
        }
    }

    /** Performs a full login and returns the freshly fetched account data. */
    private Map<String, Object> performFullLogin(final Account account) throws IOException, PluginException, InterruptedException {
        synchronized (account) {
            br.postPage(API_BASE + "/account.php", "login=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
            final Map<String, Object> resp = handleErrors(account, null);
            final Map<String, Object> data = (Map<String, Object>) resp.get("data");
            account.setProperty(PROPERTY_APIKEY, data.get("apikey").toString());
            final long expiresInSeconds = ((Number) data.get("apikey_expires_in_seconds")).longValue();
            account.setProperty(PROPERTY_APIKEY_EXPIRES, System.currentTimeMillis() + expiresInSeconds * 1000l);
            return data;
        }
    }

    /** API docs: https://pliko.pl/jd2/docs.html */
    private Map<String, Object> handleErrors(final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        if (Boolean.TRUE.equals(entries.get("success"))) {
            /* No error */
            return entries;
        }
        final int statuscode = ((Number) entries.get("status")).intValue();
        final String msg = entries.get("msg").toString();
        switch (statuscode) {
        case 0:
            /* success == true case already handled above, status 0 without success is unexpected -> treat as ok */
            return entries;
        case 1:
            /* Invalid login or password */
            account.removeProperty(PROPERTY_APIKEY);
            throw new AccountInvalidException(msg);
        case 2:
            /* Account inactive */
            account.removeProperty(PROPERTY_APIKEY);
            throw new AccountInvalidException(msg);
        case 3:
            /* Premium/transfer inactive or expired */
            account.removeProperty(PROPERTY_APIKEY);
            throw new AccountInvalidException(msg);
        case 4:
            /* API token invalid or expired -> full re-login on next attempt */
            account.removeProperty(PROPERTY_APIKEY);
            throw new AccountUnavailableException(msg, 1 * 60 * 1000l);
        case 51:
            /* No traffic left */
            throw new AccountUnavailableException(msg, 60 * 60 * 1000l);
        case 52:
            /* Not enough traffic for this file -> permanently unfit for this host until traffic resets */
            mhm.putError(account, link, 60 * 60 * 1000l, msg);
        case 53:
            /* Host daily traffic limit reached -> host is broken until daily traffic resets */
            mhm.putError(account, link, 60 * 60 * 1000l, msg);
        case 101:
            /* Missing parameter (should never happen) */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, msg);
        case 102:
            /* Invalid URL */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, msg);
        case 103:
            /* Unsupported host -> dauerhaft broken */
            mhm.putError(account, link, 60 * 60 * 1000l, msg);
        case 104:
            /* Host disabled / missing active storage account -> dauerhaft broken */
            mhm.putError(account, link, 60 * 60 * 1000l, msg);
        case 105:
            /* File offline or not downloadable */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, msg);
        case 109:
            /* File too large for storage account traffic */
            mhm.handleErrorGeneric(account, link, msg, 50, 60 * 60 * 1000l);
        case 110:
            /* No active storage server for this host -> dauerhaft broken */
            mhm.putError(account, link, 60 * 60 * 1000l, msg);
        case 113:
            /* Download failed on Pliko storage */
            mhm.handleErrorGeneric(account, link, msg, 20, 5 * 60 * 1000l);
        case 114:
            /* Final storage file unavailable */
            mhm.handleErrorGeneric(account, link, msg, 20, 5 * 60 * 1000l);
        case 120:
        case 121:
            /* Downloading to / waiting for metadata from Pliko storage -> not an error, caller handles polling via this map */
            return entries;
        default:
            if (statuscode >= 900) {
                throw new AccountUnavailableException("Internal API error: " + msg, 5 * 60 * 1000l);
            }
            /* Unknown error, do not try again with this multihoster for now */
            if (link != null) {
                mhm.handleErrorGeneric(account, link, "Unknown API error code: " + statuscode + " | " + msg, 20);
            } else {
                throw new AccountUnavailableException("Unknown API error code: " + statuscode + " | " + msg, 5 * 60 * 1000l);
            }
        }
        return entries;
    }
}
