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
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Exceptions;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Request;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
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
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 50069 $", interfaceVersion = 3, names = { "uploadedpremiumlink.net" }, urls = { "" })
public class UploadedpremiumlinkNet extends PluginForHost {
    /** Docs: https://docs.uploadedpremiumlink.net/, alternative domain: uploadedpremiumlink.xyz */
    private final String                 API_BASE                                       = "https://api.uploadedpremiumlink.net/wp-json/api";
    private static MultiHosterManagement mhm                                            = new MultiHosterManagement("uploadedpremiumlink.net");
    private static final String          PROPERTY_UPLOADEDPREMIUMLINK_PASSWORD_REQUIRED = "uploadedpremiumlink_password_required";

    @SuppressWarnings("deprecation")
    public UploadedpremiumlinkNet(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + this.getHost() + "/register");
        /* https://docs.uploadedpremiumlink.net/#rate-limiting */
        // Browser.setBurstRequestIntervalLimitGlobal("api.uploadedpremiumlink.net", 60000, 600, 60000);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookiesExclusive(true);
        br.setFollowRedirects(true);
        br.getHeaders().put("User-Agent", "JDownloader " + this.getVersion());
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + this.getHost() + "/tos";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            return false;
        } else {
            mhm.runCheck(account, link);
            return super.canHandle(link, account);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        mhm.runCheck(account, link);
        String passCode = link.getDownloadPassword();
        if (link.hasProperty(PROPERTY_UPLOADEDPREMIUMLINK_PASSWORD_REQUIRED) && passCode == null) {
            passCode = getUserInput("Password?", link);
        }
        final UrlQuery query = new UrlQuery();
        query.add("link", Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this)));
        if (link.getDownloadPassword() != null) {
            query.add("password", Encoding.urlEncode(link.getDownloadPassword()));
        }
        final Map<String, Object> dlresponse = (Map<String, Object>) this.accessAPI(account, link, "/generate_link_by_api", query);
        final String dllink = (String) dlresponse.get("link");
        int chunks = ((Number) dlresponse.get("maxchunks")).intValue();
        if (chunks > 1) {
            /* That means "up to X chunks" */
            chunks = -chunks;
        }
        if (StringUtils.isEmpty(dllink)) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Failed to find final downloadurl", 10 * 1000l);
        }
        if (passCode != null && !StringUtils.equals(link.getDownloadPassword(), passCode)) {
            logger.info("User entered valid download password: " + passCode);
            link.setDownloadPassword(passCode);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), 0);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            mhm.handleErrorGeneric(account, link, "Unknown download error", 10, 5 * 60 * 1000l);
        }
        this.dl.startDownload();
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> user = login(account);
        final AccountInfo ai = new AccountInfo();
        /*
         * User only enters API Key so he can put anything into the username field -> Let's make sure that that value is correct by using
         * the one the API is returning.
         */
        account.setUser(user.get("username").toString());
        final Number expire_date_timestampO = (Number) user.get("expire_date_timestamp");
        long expire_date_timestamp = 0;
        if (expire_date_timestampO != null) {
            expire_date_timestamp = expire_date_timestampO.longValue() * 1000;
        }
        if (expire_date_timestamp > System.currentTimeMillis()) {
            /* Bronze, Silver, Gold, Platinum */
            account.setType(AccountType.PREMIUM);
            ai.setValidUntil(expire_date_timestamp);
        } else {
            /* Free */
            account.setType(AccountType.FREE);
            ai.setExpired(true);
        }
        final SIZEUNIT maxSizeUnit = (SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue();
        final int daily_links_generated = ((Number) user.get("daily_links_generated")).intValue();
        final int daily_links_limit = ((Number) user.get("daily_links_limit")).intValue();
        ai.setStatus(user.get("type").toString() + " | Total traffic left: " + user.get("traffic_left") + " | Daily links generated: " + daily_links_generated + "/" + daily_links_limit);
        ai.setTrafficMax(((Number) user.get("daily_traffic_limit")).longValue());
        ai.setTrafficLeft(ai.getTrafficMax() - ((Number) user.get("daily_traffic_used")).longValue());
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        final List<Map<String, Object>> hosters = (List<Map<String, Object>>) user.get("hosters");
        for (final Map<String, Object> hoster : hosters) {
            final List<String> domains = (List<String>) hoster.get("alternatives_domain");
            final String primary_domain = hoster.get("primary_domain").toString();
            final String customStatusText = (String) hoster.get("StatusText");
            final MultiHostHost mhost = new MultiHostHost(primary_domain);
            mhost.addDomains(domains);
            if (hoster.get("status").toString().equalsIgnoreCase("offline")) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            } else if (account.getType() == AccountType.FREE && !hoster.get("type").toString().equalsIgnoreCase("free")) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST_NOT_FOR_THIS_ACCOUNT_TYPE);
            }
            final long daily_quota_total = ((Number) hoster.get("daily_quota_total")).longValue();
            final long daily_quota_left = ((Number) hoster.get("daily_quota_left")).longValue();
            final long weekly_quota_total = ((Number) hoster.get("weekly_quota_total")).longValue();
            final long weekly_quota_left = ((Number) hoster.get("weekly_quota_left")).longValue();
            mhost.setTrafficMax(Math.min(daily_quota_total, weekly_quota_total));
            mhost.setTrafficLeft(Math.min(daily_quota_left, weekly_quota_left));
            mhost.setLinksMax(((Number) hoster.get("daily_links_limit")).intValue());
            mhost.setLinksLeft(mhost.getLinksMax() - ((Number) hoster.get("daily_links_used")).intValue());
            /* Double check for reached limit */
            final int weekly_percentage_used = ((Number) hoster.get("weekly_percentage_used")).intValue();
            if ((((Number) hoster.get("percentage_used")).intValue() >= 100 || weekly_percentage_used >= 100) && daily_quota_left > 0) {
                mhost.setTrafficLeft(0);
                mhost.setLinksLeft(0);
            }
            final String statustext;
            if (weekly_percentage_used == 0) {
                statustext = "Weekly traffic left: " + SIZEUNIT.formatValue(maxSizeUnit, weekly_quota_total);
            } else {
                statustext = "Weekly traffic left: " + SIZEUNIT.formatValue(maxSizeUnit, weekly_quota_left) + "/" + SIZEUNIT.formatValue(maxSizeUnit, weekly_quota_total);
            }
            if (customStatusText != null) {
                mhost.setStatusText(customStatusText);
            }
            mhost.setStatusText(statustext);
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        account.setConcurrentUsePossible(true);
        if (daily_links_generated >= daily_links_limit) {
            /* Account cannot be used for downloading. */
            throw new AccountUnavailableException("Reached daily max limits limit: " + daily_links_generated + "/" + daily_links_limit, 5 * 60 * 1000);
        }
        return ai;
    }

    private Map<String, Object> login(final Account account) throws IOException, PluginException, InterruptedException {
        synchronized (account) {
            logger.info("Performing full login");
            final Map<String, Object> entries = (Map<String, Object>) this.accessAPI(account, null, "/user", new UrlQuery());
            return entries;
        }
    }

    private Object accessAPI(final Account account, final DownloadLink link, final String path, final UrlQuery postdata) throws IOException, PluginException, InterruptedException {
        postdata.add("api_key", Encoding.urlEncode(account.getPass()));
        final PostRequest req = br.createPostRequest(this.API_BASE + path, postdata);
        return accessAPI(account, link, req);
    }

    private Object accessAPI(final Account account, final DownloadLink link, final Request req) throws IOException, PluginException, InterruptedException {
        br.getPage(req);
        return checkErrors(account, link);
    }

    private Object checkErrors(final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        try {
            final Object jsonO = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
            if (jsonO == null || !(jsonO instanceof Map)) {
                return jsonO;
            }
            final Map<String, Object> resp = (Map<String, Object>) jsonO;
            final Map<String, Object> data = (Map<String, Object>) resp.get("data");
            if (Boolean.TRUE.equals(resp.get("success"))) {
                /* Success */
                return data;
            }
            handleErrorMap(account, link, data);
            return jsonO;
        } catch (final JSonMapperException jme) {
            if (link != null) {
                mhm.handleErrorGeneric(account, link, "Bad API answer", 50, 5 * 60 * 1000l);
            } else {
                throw Exceptions.addSuppressed(new AccountUnavailableException("Bad API answer", 1 * 60 * 1000l), jme);
            }
        }
        return null;
    }

    private void handleErrorMap(final Account account, final DownloadLink link, final Map<String, Object> entries) throws PluginException, InterruptedException {
        final HashSet<String> accountErrorsPermanent = new HashSet<String>();
        accountErrorsPermanent.add("AUTH_MISSING_APIKEY");
        accountErrorsPermanent.add("AUTH_BAD_APIKEY");
        final HashSet<String> accountErrorsTemporary = new HashSet<String>();
        accountErrorsTemporary.add("AUTH_USER_BANNED");
        accountErrorsTemporary.add("AUTH_IP_BANNED");
        /* 2024-11-04: Is now HOSTER_DAILY_LIMIT_EXCEEDEDs */
        // accountErrorsTemporary.add("DAILY_LIMIT_EXCEEDED");
        accountErrorsTemporary.add("LINK_GENERATION_LIMIT_EXCEEDED");
        accountErrorsTemporary.add("PREMIUM_TRAFFIC_UNAVAILABLE");
        accountErrorsTemporary.add("RATE_LIMIT_EXCEEDED");
        accountErrorsTemporary.add("TRAFFIC_LIMIT_EXCEEDED");
        accountErrorsTemporary.add("USER_QUOTA_EXCEEDED");
        accountErrorsTemporary.add("WEEKLY_LIMIT_EXCEEDED");
        accountErrorsTemporary.add("FREE_TRAFFIC_LIMIT_EXCEEDED");
        accountErrorsTemporary.add("FREE_USER_DAILY_LIMIT_EXCEEDED");
        accountErrorsTemporary.add("PROXY_NOT_ALLOWED");
        final HashSet<String> downloadErrorsHostUnavailable = new HashSet<String>();
        downloadErrorsHostUnavailable.add("HOSTER_DAILY_LIMIT_EXCEEDEDs");
        downloadErrorsHostUnavailable.add("HOSTER_MAINTENANCE");
        downloadErrorsHostUnavailable.add("HOSTER_NOT_AVAILABLE_BY_API");
        downloadErrorsHostUnavailable.add("HOSTER_TEMPORARILY_UNAVAILABLE");
        downloadErrorsHostUnavailable.add("UNSUPPORTED_HOSTER");
        downloadErrorsHostUnavailable.add("SERVER_TRAFFIC_LIMIT_REACHED");
        final HashSet<String> downloadErrorsFileUnavailable = new HashSet<String>();
        downloadErrorsFileUnavailable.add("BAD_LINK");
        downloadErrorsFileUnavailable.add("FILESIZE_LIMIT");
        downloadErrorsFileUnavailable.add("FILESIZE_LIMIT_FREE");
        downloadErrorsFileUnavailable.add("FILESIZE_LIMIT_GUEST");
        downloadErrorsFileUnavailable.add("LINK_ERROR");
        downloadErrorsFileUnavailable.add("LINK_GENERATION_FAILED");
        downloadErrorsFileUnavailable.add("LINK_TEMPORARILY_UNAVAILABLE");
        downloadErrorsFileUnavailable.add("LINK_TYPE_UNSUPPORTED");
        downloadErrorsFileUnavailable.add("MUST_BE_PREMIUM");
        downloadErrorsFileUnavailable.add("RESOURCE_RETRIEVAL_FAILURE");
        final String message = entries.get("message").toString();
        String errorcode = (String) entries.get("category_error");
        if (accountErrorsPermanent.contains(errorcode)) {
            throw new AccountInvalidException(message);
        } else if (accountErrorsTemporary.contains(errorcode)) {
            throw new AccountUnavailableException(message, 5 * 60 * 1000);
        } else if (downloadErrorsHostUnavailable.contains(errorcode)) {
            mhm.putError(account, link, 5 * 60 * 1000l, message);
        } else if (downloadErrorsFileUnavailable.contains(errorcode)) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, message);
        } else if (errorcode.equalsIgnoreCase("FILE_NOT_FOUND")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
        } else if (errorcode.equalsIgnoreCase("LINK_PASS_PROTECTED")) {
            final String text;
            if (link.getDownloadPassword() == null) {
                text = "Password required";
            } else {
                text = "Password wrong";
            }
            link.setDownloadPassword(null);
            link.setProperty(PROPERTY_UPLOADEDPREMIUMLINK_PASSWORD_REQUIRED, true);
            throw new PluginException(LinkStatus.ERROR_RETRY, text);
        } else if (errorcode.equalsIgnoreCase("LINK_THIRD_PARTY_PR_SUB_REQUIRED")) {
            /* Extra subscription required to download that item. */
            throw new AccountRequiredException(message);
        } else {
            /* Unknown error code */
            if (link != null) {
                mhm.handleErrorGeneric(account, link, message, 50, 1 * 60 * 1000l);
            } else {
                throw new AccountUnavailableException(message, 5 * 60 * 1000);
            }
        }
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://www." + getHost() + "/myaccount";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("[a-zA-Z0-9]{24}")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}