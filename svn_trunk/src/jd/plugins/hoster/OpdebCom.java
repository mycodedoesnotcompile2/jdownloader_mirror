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

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 50125 $", interfaceVersion = 3, names = { "opdeb.com" }, urls = { "" })
public class OpdebCom extends PluginForHost {
    private static final String          API_BASE = "https://opdeb.com/apiv1";
    private static MultiHosterManagement mhm      = new MultiHosterManagement("opdeb.com");

    @SuppressWarnings("deprecation")
    public OpdebCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/purchase");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new AccountRequiredException();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.API_KEY_LOGIN };
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
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        final String directlinkproperty = this.getHost() + "directlink";
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        final String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            mhm.runCheck(account, link);
            this.login(account, false);
            final String ipValueFromAccount = account.getStringProperty("ip_linked");
            final String url = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("link", url);
            if (ipValueFromAccount != null) {
                /*
                 * 2024-10-31: This parameter is mandatory but completely useless. I've asked the admin to remove it since it does not
                 * contribute to "API security" at all.
                 */
                query.appendEncoded("ip", ipValueFromAccount);
            }
            final Map<String, Object> resp_generator = (Map<String, Object>) this.callAPI("/generator", link, account, query);
            dllink = JavaScriptEngineFactory.walkJson(resp_generator, "generator/{0}/link").toString();
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                mhm.handleErrorGeneric(account, link, "Final downloadurl did not lead to file", 50, 5 * 60 * 1000l);
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
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> user = login(account, true);
        account.setProperty("ip_linked", user.get("ip_linked"));
        final AccountInfo ai = new AccountInfo();
        /*
         * User only enters apikey as password and could enter anything into the username field --> Make sure it contains an unique value.
         */
        account.setUser(user.get("username").toString());
        final long plan_expire_timestamp = ((Number) user.get("plan_expire")).longValue() * 1000;
        if (plan_expire_timestamp > System.currentTimeMillis()) {
            account.setType(AccountType.PREMIUM);
            ai.setValidUntil(plan_expire_timestamp, br);
        } else {
            account.setType(AccountType.FREE);
            ai.setExpired(true);
        }
        final String plan_name = (String) user.get("plan_name");
        if (!StringUtils.isEmpty(plan_name)) {
            ai.setStatus(plan_name);
        }
        final Number account_create = (Number) user.get("account_create");
        if (account_create != null) {
            ai.setCreateTime(account_create.longValue() * 1000);
        }
        ai.setUnlimitedTraffic();
        final Map<String, Object> supportedhosts_root = (Map<String, Object>) this.callAPI("/hosters", null, account);
        final List<Map<String, Object>> arrayHoster = (List<Map<String, Object>>) supportedhosts_root.get("hosters");
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (final Map<String, Object> hostermap : arrayHoster) {
            final String domain = hostermap.get("name").toString();
            final MultiHostHost mhost = new MultiHostHost(domain);
            if (hostermap.get("status").toString().equalsIgnoreCase("down")) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                final String downsincedate = (String) hostermap.get("downsincedate");
                if (!StringUtils.isEmpty(downsincedate)) {
                    mhost.setStatusText("Broken/Down since " + downsincedate);
                }
            }
            final Number maxlinksO;
            if (account.getType() == AccountType.FREE) {
                maxlinksO = (Number) hostermap.get("free_max_links_limit_per_day");
            } else {
                maxlinksO = (Number) hostermap.get("premium_maxlinks_limit_per_day");
            }
            /* Set host-specific limits if any exist. */
            if (maxlinksO != null) {
                final long maxlinks = maxlinksO.longValue();
                mhost.setLinksLeft(maxlinks);
                mhost.setLinksMax(maxlinks);
                /* Cal also have value \u221e -> ∞ -> Unlimited symbol */
                final String maxTrafficStr = (String) hostermap.get("maxbandwidth");
                if (maxTrafficStr != null && maxTrafficStr.matches("^\\d+.+")) {
                    final long maxTraffic = SizeFormatter.getSize(maxTrafficStr);
                    mhost.setTrafficLeft(maxTraffic);
                    mhost.setTrafficMax(maxTraffic);
                }
            }
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        return ai;
    }

    private Map<String, Object> login(final Account account, final boolean validateToken) throws IOException, PluginException, InterruptedException {
        synchronized (account) {
            if (!validateToken) {
                logger.info("Trust token without login");
                return null;
            } else {
                final Map<String, Object> resp_login = (Map<String, Object>) callAPI("/account", null, account);
                return (Map<String, Object>) JavaScriptEngineFactory.walkJson(resp_login, "account/{0}");
            }
        }
    }

    private Object callAPI(final String path, final DownloadLink link, final Account account) throws PluginException, InterruptedException, IOException {
        return callAPI(path, link, account, null);
    }

    private Object callAPI(final String path, final DownloadLink link, final Account account, UrlQuery query) throws PluginException, InterruptedException, IOException {
        if (query == null) {
            query = new UrlQuery();
        }
        query.appendEncoded("key", account.getPass());
        br.getPage(API_BASE + path + "?" + query.toString());
        return handleErrorsAPI(br, link, account);
    }

    private Object handleErrorsAPI(final Browser br, final DownloadLink link, final Account account) throws PluginException, InterruptedException {
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String message = (String) entries.get("message");
        if (message == null) {
            /* No error */
            return entries;
        }
        if (link == null) {
            /* Account related error */
            throw new AccountInvalidException(message);
        }
        /* Decide if error is link/download related or account related. */
        final List<String> patternsLoginInvalid = new ArrayList<String>();
        patternsLoginInvalid.add("The Api Key .+ does not exists or is not authorized");
        patternsLoginInvalid.add("AUTH_KEY_BAD");
        patternsLoginInvalid.add("AUTH_ACCOUNT_BANNED");
        patternsLoginInvalid.add("AUTH_API_BANNED");
        patternsLoginInvalid.add("AUTH_BAD_APIKEY");
        patternsLoginInvalid.add("AUTH_NOT_PREMIUM");
        patternsLoginInvalid.add("VPN_DETECTION");
        for (final String pattern : patternsLoginInvalid) {
            if (message.matches(pattern)) {
                throw new AccountInvalidException(message);
            }
        }
        final HashSet<String> downloadErrorsHostUnavailable = new HashSet<String>();
        downloadErrorsHostUnavailable.add("GEN_LIMIT");
        for (final String pattern : downloadErrorsHostUnavailable) {
            if (message.matches(pattern)) {
                mhm.putError(account, link, 5 * 60 * 1000l, message);
            }
        }
        final HashSet<String> downloadErrorsFileUnavailable = new HashSet<String>();
        downloadErrorsFileUnavailable.add("GEN_SERVER_ERROR");
        downloadErrorsFileUnavailable.add("GEN_ERROR");
        for (final String pattern : downloadErrorsFileUnavailable) {
            if (message.matches(pattern)) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, message);
            }
        }
        mhm.handleErrorGeneric(account, this.getDownloadLink(), message, 5);
        return entries;
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/api";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.replace("-", "").matches("[A-Za-z0-9]{35,40}")) {
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