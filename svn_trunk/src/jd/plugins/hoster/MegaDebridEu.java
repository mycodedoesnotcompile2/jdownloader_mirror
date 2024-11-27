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

@HostPlugin(revision = "$Revision: 50243 $", interfaceVersion = 3, names = { "mega-debrid.eu" }, urls = { "" })
public class MegaDebridEu extends PluginForHost {
    private final String                 mName = "www.mega-debrid.eu";
    private final String                 mProt = "https://";
    private static MultiHosterManagement mhm   = new MultiHosterManagement("mega-debrid.eu");

    public MegaDebridEu(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(mProt + mName + "/index.php");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setCustomCharset("UTF-8");
        br.getHeaders().put("User-Agent", "JDownloader-" + Math.max(super.getVersion(), 0));
        return br;
    }

    @Override
    public String getAGBLink() {
        return mProt + mName + "/index.php";
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ac = new AccountInfo();
        final Map<String, Object> user = login(account);
        final String secondsLeft = user.get("vip_end").toString();
        if (secondsLeft != null && secondsLeft.matches("\\d+")) {
            ac.setValidUntil(Long.parseLong(secondsLeft) * 1000l);
        } else {
            ac.setExpired(true);
        }
        // now it's time to get all supported hosts
        br.getPage("/api.php?action=getHostersList");
        Map<String, Object> results = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!"ok".equalsIgnoreCase((String) results.get("response_code"))) {
            throw new AccountInvalidException();
        }
        List<Map<String, Object>> hosterlist = (List<Map<String, Object>>) results.get("hosters");
        if (hosterlist == null) {
            /* workaround for missing hosters entry. It is available when the session cookies are NOT available. */
            final Browser brc = createNewBrowserInstance();
            brc.getPage("https://www." + getHost() + "/api.php?action=getHostersList");
            results = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            hosterlist = (List<Map<String, Object>>) results.get("hosters");
            if (hosterlist == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (final Map<String, Object> hostinfo : hosterlist) {
            final List<String> domains = (List<String>) hostinfo.get("domains");
            if (domains == null || domains.isEmpty()) {
                /* Skip invalid entries */
                continue;
            }
            final MultiHostHost mhost = new MultiHostHost();
            mhost.setName(hostinfo.get("name").toString());
            mhost.setDomains(domains);
            if (!"up".equals(hostinfo.get("status"))) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            supportedhosts.add(mhost);
        }
        ac.setMultiHostSupportV2(this, supportedhosts);
        account.setType(AccountType.PREMIUM);
        return ac;
    }

    private Map<String, Object> login(final Account account) throws Exception {
        synchronized (account) {
            br.getPage(mProt + mName + "/api.php?action=connectUser&login=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
            final Map<String, Object> entries = handleAPIErrors(br, account, null);
            final String token = (String) entries.get("token");
            if (StringUtils.isEmpty(token)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            account.setProperty("token", token);
            return entries;
        }
    }

    private Map<String, Object> handleAPIErrors(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String response_code = entries.get("response_code").toString();
        if (response_code.equalsIgnoreCase("ok")) {
            /* No error */
            return entries;
        }
        final String response_text = entries.get("response_text").toString();
        if (link == null) {
            /* Error during linkcheck */
            throw new AccountInvalidException(response_text);
        }
        /* Check for other account related errors */
        if (response_code.equals("UNALLOWED_IP")) {
            throw new AccountUnavailableException(response_text, 5 * 60 * 1000l);
        }
        /* Error is download related */
        mhm.handleErrorGeneric(account, link, response_text, 50, 5 * 60 * 1000l);
        /* Unreachable code */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        String url = link.getDefaultPlugin().buildExternalDownloadURL(link, this);
        // link corrections
        if (link.getHost().equals("filefactory.com")) {
            // http://www.filefactory.com/file/asd/n/ads.rar
            if (!url.endsWith("/")) {
                url += "/";
            }
            url += "/n/" + link.getName();
        }
        url = Encoding.urlEncode(url);
        String token = account.getStringProperty("token");
        if (token == null) {
            // this shouldn't happen!
            login(account);
            token = account.getStringProperty("token");
        }
        br.postPage(mProt + mName + "/api.php?action=getLink&token=" + token, "link=" + url);
        final Map<String, Object> entries = handleAPIErrors(br, account, link);
        final String dllink = entries.get("debridLink").toString();
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, true, 0);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            mhm.handleErrorGeneric(account, link, "unknown_dl_error", 50, 1 * 60 * 1000l);
        }
        dl.startDownload();
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
    }

    @Override
    public AvailableStatus requestFileInformation(DownloadLink link) throws Exception {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}