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

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
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

@HostPlugin(revision = "$Revision: 50039 $", interfaceVersion = 3, names = { "opdeb.com" }, urls = { "" })
public class OpdebCom extends PluginForHost {
    private static final String          API_BASE = "https://opdeb.com/apiv1";
    private static MultiHosterManagement mhm      = new MultiHosterManagement("opdeb.com");

    @SuppressWarnings("deprecation")
    public OpdebCom(PluginWrapper wrapper) {
        super(wrapper);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            this.enablePremium("https://" + getHost() + "/purchase");
        }
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
            return true;
        }
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        mhm.runCheck(account, link);
        this.login(account, false);
        final String url = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("link", url);
        // TODO: Implement this
        final Map<String, Object> entries = (Map<String, Object>) this.callAPI("/generator", link, account, query);
        String dllink = null;
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            mhm.handleErrorGeneric(account, link, "Final downloadurl did not lead to file", 10, 5 * 60 * 1000l);
        }
        link.setProperty(this.getHost() + "directlink", dl.getConnection().getURL().toString());
        this.dl.startDownload();
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> entries = login(account, true);
        final AccountInfo ai = new AccountInfo();
        /*
         * User only enters apikey as password and could enter anything into the username field --> Make sure it contains an unique value.
         */
        account.setUser(entries.get("username").toString());
        final long plan_expire_timestamp = ((Number) entries.get("plan_expire_timestamp")).longValue() * 1000;
        if (plan_expire_timestamp > System.currentTimeMillis()) {
            account.setType(AccountType.PREMIUM);
            ai.setValidUntil(plan_expire_timestamp);
        } else {
            account.setType(AccountType.FREE);
            ai.setExpired(true);
        }
        ai.setUnlimitedTraffic();
        final Map<String, Map<String, Object>> freehostmapping = new HashMap<String, Map<String, Object>>();
        final Map<String, Map<String, Object>> limithostermapping = new HashMap<String, Map<String, Object>>();
        if (account.getType() == AccountType.FREE || true) {
            final Map<String, Object> freehosters_root = (Map<String, Object>) this.callAPI("/freehosters", null, account);
            final List<Map<String, Object>> arrayHoster = (List<Map<String, Object>>) freehosters_root.get("freehosters");
            for (final Map<String, Object> hostermap : arrayHoster) {
                final String domain = hostermap.get("hoster").toString();
                freehostmapping.put(domain, hostermap);
            }
        }
        final Map<String, Object> limithosters_root = (Map<String, Object>) this.callAPI("/limithosters", null, account);
        final List<Map<String, Object>> limithosters_array = (List<Map<String, Object>>) limithosters_root.get("limithosters");
        for (final Map<String, Object> hostermap : limithosters_array) {
            final String domain = hostermap.get("hoster").toString();
            limithostermapping.put(domain, hostermap);
        }
        final Map<String, Object> supportedhosts_root = (Map<String, Object>) this.callAPI("/hosters", null, account);
        final List<Map<String, Object>> arrayHoster = (List<Map<String, Object>>) supportedhosts_root.get("hosters_list");
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (final Map<String, Object> hostermap : arrayHoster) {
            final String domain = hostermap.get("name").toString();
            final MultiHostHost mhost = new MultiHostHost(domain);
            if (hostermap.get("status").toString().equalsIgnoreCase("down")) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                final String downsincedate = (String) hostermap.get("downsincedate");
                if (!StringUtils.isEmpty(downsincedate)) {
                    mhost.setStatusText("Down since " + downsincedate);
                }
            }
            final Map<String, Object> limitmap;
            if (account.getType() == AccountType.FREE) {
                limitmap = freehostmapping.get(domain);
                if (limitmap == null) {
                    mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST_NOT_FOR_THIS_ACCOUNT_TYPE);
                }
            } else {
                limitmap = limithostermapping.get(domain);
            }
            if (limitmap != null) {
                final Number maxlinksO = (Number) limitmap.get("maxlinks");
                if (maxlinksO != null) {
                    final long maxlinks = maxlinksO.longValue();
                    mhost.setLinksLeft(maxlinks);
                    mhost.setLinksMax(maxlinks);
                }
                /* Cal also have value \u221e -> ∞ -> Unlimited symbol */
                final String maxTrafficStr = (String) limitmap.get("maxbandwidth");
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
            throw new AccountInvalidException(message);
        } else {
            mhm.handleErrorGeneric(account, this.getDownloadLink(), message, 50);
        }
        return entries;
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/TODO_ADD_API_KEY_HELP_URL";
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