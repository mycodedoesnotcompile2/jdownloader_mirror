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

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.BearerAuthentication;
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
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 52567 $", interfaceVersion = 3, names = { "storage-portal.com" }, urls = { "" })
public class StoragePortalCom extends PluginForHost {
    /* API docs: https://www.storage-portal.com/docs/download-client-api */
    private static final String          API_BASE           = "https://www.storage-portal.com/api/external/v1";
    private static final String          PROPERTY_DIRECTURL = "storageportalcom_directlink";
    private static final String          PROPERTY_LINKID    = "storageportalcom_linkid";
    private static final String          PROPERTY_MAXCHUNKS = "storageportalcom_maxchunks";
    private static final String          PROPERTY_RESUMABLE = "storageportalcom_resumable";
    private static final String          PROPERTY_NOTRAFFIC = "storageportalcom_notraffic";
    private static MultiHosterManagement mhm                = new MultiHosterManagement("storage-portal.com");

    public StoragePortalCom(PluginWrapper wrapper) {
        super(wrapper);
        enablePremium("https://www." + getHost());
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.getHeaders().put("Content-Type", "application/json");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/terms";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return link.getBooleanProperty(PROPERTY_RESUMABLE, true);
    }

    private int getMaxChunks(final DownloadLink link, final Account account) {
        final int maxchunks = link.getIntegerProperty(PROPERTY_MAXCHUNKS, 1);
        if (maxchunks > 1) {
            return -maxchunks;
        }
        return maxchunks;
    }

    private void setAuthHeader(final Account account) {
        br.addAuthentication(new BearerAuthentication(this.getHost(), account.getPass(), null));
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws PluginException, IOException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        final String storedDirecturl = link.getStringProperty(PROPERTY_DIRECTURL);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
            link.setProperty(PROPERTY_NOTRAFFIC, true);
        } else {
            setAuthHeader(account);
            final String postBody = "{\"url\":\"" + PluginJSonUtils.escape(link.getDefaultPlugin().buildExternalDownloadURL(link, this)) + "\"}";
            br.postPageRaw(API_BASE + "/unrestrict", postBody);
            final Map<String, Object> data = handleAPIErrors(account, link);
            final String linkId = data.get("linkId").toString();
            link.setProperty(PROPERTY_LINKID, linkId);
            dllink = pollUntilReady(account, link, linkId, data);
            if (StringUtils.isEmpty(dllink)) {
                mhm.handleErrorGeneric(account, link, "Failed to find final downloadurl", 50, 5 * 60 * 1000l);
            }
            link.setProperty(PROPERTY_DIRECTURL, dllink);
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, isResumeable(link, account), getMaxChunks(link, account));
            if (!looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (dl.getConnection().getContentType().contains("json")) {
                    handleAPIErrors(account, link);
                }
                mhm.handleErrorGeneric(account, link, "Unknown download error", 50, 5 * 60 * 1000l);
            }
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(PROPERTY_DIRECTURL);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        dl.startDownload();
    }

    /**
     * Polls GET /link/{linkId} until download_ready is true or remote_status becomes "error". Returns the direct download URL once ready.
     * Returns immediately without sleeping if download_ready is already true in the initial data.
     */
    private String pollUntilReady(final Account account, final DownloadLink link, final String linkId, final Map<String, Object> initialData) throws Exception {
        Map<String, Object> data = initialData;
        int maxPolls = 60;
        while (maxPolls-- > 0) {
            if (Boolean.TRUE.equals(data.get("download_ready"))) {
                /* Set file properties from initial response — always present regardless of download_ready */
                link.setProperty(PROPERTY_MAXCHUNKS, data.get("maxchunks"));
                link.setProperty(PROPERTY_RESUMABLE, data.get("resumable"));
                if (Boolean.TRUE.equals(data.get("reusedExisting"))) {
                    link.setProperty(PROPERTY_NOTRAFFIC, true);
                } else {
                    link.removeProperty(PROPERTY_NOTRAFFIC);
                }
                return data.get("link").toString();
            }
            final String remoteStatus = (String) data.get("remote_status");
            if ("error".equalsIgnoreCase(remoteStatus)) {
                mhm.handleErrorGeneric(account, link, "Remote download failed on storage-portal servers (remote_status=error)", 50, 5 * 60 * 1000l);
                /* Unreachable */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final int pollAfterSeconds = Math.min(Math.max(((Number) data.get("poll_after_seconds")).intValue(), 1), 60);
            logger.info("storage-portal.com: Polling link " + linkId + " after " + pollAfterSeconds + "s (remote_status=" + remoteStatus + ")");
            this.sleep(pollAfterSeconds * 1000l, link);
            br.getPage(API_BASE + "/link/" + Encoding.urlEncode(linkId));
            data = handleAPIErrors(account, link);
        }
        mhm.handleErrorGeneric(account, link, "Polling timeout: file never became ready on storage-portal servers", 50, 5 * 60 * 1000l);
        /* Unreachable */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @SuppressWarnings("unchecked")
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> data = login(account);
        final AccountInfo ai = new AccountInfo();
        final Number login_check_interval_millis = (Number) data.get("login_check_interval");
        if (login_check_interval_millis != null) {
            account.setRefreshTimeout(login_check_interval_millis.longValue());
        }
        if ("premium".equalsIgnoreCase((String) data.get("accountType"))) {
            account.setType(AccountType.PREMIUM);
            ai.setTrafficLeft(((Number) data.get("trafficleft")).longValue());
            ai.setTrafficMax(((Number) data.get("trafficmax")).longValue());
        } else {
            account.setType(AccountType.FREE);
            account.setMaxSimultanDownloads(1);
            ai.setTrafficLeft(0);
        }
        br.getPage(API_BASE + "/hosts/grouped");
        final Map<String, Object> hostsData = handleAPIErrors(account, null);
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (final Map<String, Object> hostinfo : (List<Map<String, Object>>) hostsData.get("supportedhosts")) {
            final List<String> domains = (List<String>) hostinfo.get("domains");
            if (domains.isEmpty()) {
                /* This should never happen */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final MultiHostHost mhost = new MultiHostHost();
            mhost.addDomains(domains);
            if (!Boolean.TRUE.equals(hostinfo.get("currently_working"))) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            mhost.setMaxChunks(((Number) hostinfo.get("maxChunks")).intValue());
            mhost.setMaxDownloads(((Number) hostinfo.get("maxDownloads")).intValue());
            mhost.setResumable((Boolean) hostinfo.get("resumable"));
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        return ai;
    }

    private Map<String, Object> login(final Account account) throws Exception {
        synchronized (account) {
            setAuthHeader(account);
            br.getPage(API_BASE + "/account");
            return handleAPIErrors(account, null);
        }
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> handleAPIErrors(final Account account, final DownloadLink link) throws Exception {
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            final String msg = "Invalid API response";
            final long wait = 1 * 60 * 1000l;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, wait);
            } else {
                throw new AccountUnavailableException(msg, wait);
            }
        }
        if (Boolean.TRUE.equals(entries.get("success"))) {
            /* No error */
            return (Map<String, Object>) entries.get("data");
        }
        /* Error: parse numeric status code */
        final int statusCode = ((Number) entries.get("status")).intValue();
        final String msg = (String) entries.get("msg");
        final Object retryInO = entries.get("retry_in_seconds");
        final long retryWait = retryInO instanceof Number ? ((Number) retryInO).longValue() * 1000l : 5 * 60 * 1000l;
        switch (statusCode) {
        case 1:
            /* Missing or invalid API key */
            throw new AccountInvalidException(msg);
        case 2:
            /* Account not usable */
            throw new AccountUnavailableException(msg, retryWait);
        case 52:
            /* Insufficient traffic */
            throw new AccountUnavailableException(msg, retryWait);
        case 54:
            /* Per-host 24h limit exceeded */
            if (link != null) {
                mhm.putError(account, link, retryWait, msg);
            } else {
                throw new AccountUnavailableException(msg, retryWait);
            }
            break;
        case 55:
        case 56:
            /* Too many concurrent downloads */
            throw new AccountUnavailableException(msg, retryWait);
        case 101:
            /* Unsupported URL/host */
            mhm.handleErrorGeneric(account, link, msg, 50, retryWait);
            break;
        case 102:
            /* File banned */
            throw new PluginException(LinkStatus.ERROR_FATAL, msg);
        case 103:
            /* Private file */
            throw new PluginException(LinkStatus.ERROR_FATAL, msg);
        case 104:
            /* File offline */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        default:
            if (PluginEnvironment.ACCOUNT_CHECK.isCurrentPluginEnvironment()) {
                throw new AccountUnavailableException(msg, retryWait);
            } else {
                mhm.handleErrorGeneric(account, link, msg, 50, retryWait);
            }
        }
        /* Unreachable after mhm.handleErrorGeneric / mhm.putError, but compiler needs it */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public int getMaxSimultanDownload(final DownloadLink link, final Account account) {
        return Integer.MAX_VALUE;
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://www." + getHost() + "/profile";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        }
        /* API keys start with "obb_" per documentation */
        return StringUtils.startsWithCaseInsensitive(str, "obb_") && str.length() == 36;
    }

    @Override
    public void update(final DownloadLink link, final Account account, final long bytesTransfered) {
        if (link.hasProperty(PROPERTY_NOTRAFFIC)) {
            /* Do not deduct traffic for downloads when API signals that. */
            return;
        }
        /* Let upper handling do its job. */
        super.update(link, account, bytesTransfered);
    }
}