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
import java.util.Locale;
import java.util.Map;

import jd.PluginWrapper;
import jd.controlling.reconnect.ipcheck.BalancedWebIPCheck;
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

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 50812 $", interfaceVersion = 3, names = { "bestdebrid.com" }, urls = { "" })
public class BestdebridCom extends PluginForHost {
    private static final String          API_BASE            = "https://bestdebrid.com/api/v1";
    private static MultiHosterManagement mhm                 = new MultiHosterManagement("bestdebrid.com");
    private static final int             defaultMAXDOWNLOADS = -1;

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    @SuppressWarnings("deprecation")
    public BestdebridCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/plans");
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
        return "https://" + getHost() + "/";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(final DownloadLink downloadLink) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        /* handle premium should never be called */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        final String directlinkproperty = this.getHost() + "directlink";
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        String dllink = null;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            final String clientIPv4 = new BalancedWebIPCheck(br.getProxy()).getExternalIP().getIP();
            /* 2019-08-27: Test-code regarding Free Account download which is only possible via website. */
            final boolean use_website_for_free_account_downloads = false;
            if (account.getType() == AccountType.FREE && use_website_for_free_account_downloads) {
                this.br.postPage(API_BASE + "/generateLink", "&ip=" + clientIPv4 + "&link=" + Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this)) + "&pass=&boxlinklist=0");
                dllink = PluginJSonUtils.getJsonValue(br, "link");
                if (!StringUtils.isEmpty(dllink) && !dllink.startsWith("http") && !dllink.startsWith("/")) {
                    dllink = "https://" + this.getHost() + "/" + dllink;
                }
            } else {
                this.br.getPage(API_BASE + "/generateLink?auth=" + Encoding.urlEncode(account.getPass()) + "&ip=" + clientIPv4 + "&link=" + Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this)));
                final Map<String, Object> entries = handleAPIErrors(br, account, link);
                dllink = entries.get("link").toString();
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                if (br.getRequest().getHtmlCode().startsWith("{")) {
                    handleAPIErrors(this.br, account, link);
                }
                mhm.handleErrorGeneric(account, link, "Final downloadlink did not lead to file", 50, 5 * 60 * 1000l);
            }
        } catch (final Exception e) {
            if (e instanceof InterruptedException) {
                throw e;
            }
            if (storedDirecturl != null) {
                link.removeProperty(directlinkproperty);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        if (storedDirecturl == null) {
            link.setProperty(directlinkproperty, dllink);
        }
        dl.startDownload();
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        /*
         * 2019-07-30: This means that an account is owned by a reseller. Reseller accounts have no limits (no daily bandwidth/numberof
         * links limits even compared to premium).
         */
        final Map<String, Object> user = loginAPI(account);
        final Boolean bypass_api_limit = (Boolean) user.get("bypass_api_limit");
        final String expireDateStr = (String) user.get("expire");
        /* 2019-07-30: E.g. "premium":true --> Website may even show a more exact status e.g. "Debrid Plan : Silver" */
        // final String premium = PluginJSonUtils.getJson(br, "premium");
        /* 2019-07-30: credit value = for resellers --> Money on the account which can be used to 'buy more links'. */
        final String creditStr = user.get("credit").toString();
        int credit = 0;
        long validuntil = 0;
        if (expireDateStr != null) {
            validuntil = TimeFormatter.getMilliSeconds(expireDateStr, "yyyy-MM-dd HH:mm:ss", Locale.ENGLISH);
        }
        if (creditStr != null && creditStr.matches("\\d+")) {
            credit = Integer.parseInt(creditStr);
        }
        String statusAcc;
        if (validuntil > System.currentTimeMillis()) {
            account.setType(AccountType.PREMIUM);
            statusAcc = "Premium Account";
            if (bypass_api_limit == Boolean.TRUE) {
                statusAcc += " [Reseller]";
            }
            ai.setUnlimitedTraffic();
            account.setMaxSimultanDownloads(defaultMAXDOWNLOADS);
            ai.setValidUntil(validuntil, br);
        } else {
            account.setType(AccountType.FREE);
            statusAcc = "Free Account";
            if (bypass_api_limit == Boolean.TRUE) {
                statusAcc += " [Reseller]";
                ai.setUnlimitedTraffic();
            } else {
                /*
                 * 2019-07-12: Seems like they have hosts which free account users can download from but their hostlist does not state which
                 * ones these are. In the "Informations" tab there is a list of "Free hosters": https://bestdebrid.com/downloader (more
                 * precise: https://bestdebrid.com/info.php) For now, we will simply set the free account traffic to ZERO.
                 */
                /*
                 * 2019-08-37: Free-Account Downloads via API are not possible.
                 */
                ai.setTrafficLeft(0);
                account.setMaxSimultanDownloads(0);
                statusAcc += " [Downloads are only possible via browser]";
            }
        }
        statusAcc += " [" + credit + " credits]";
        ai.setPremiumPoints(credit);
        ai.setStatus(statusAcc);
        /* Get list of supported hosts */
        br.getPage(API_BASE + "/hosts?auth=" + Encoding.urlEncode(account.getPass()));
        final List<Map<String, Object>> hosters;
        final Object hostersO = restoreFromString(br.toString(), TypeRef.OBJECT);
        int counter = 0;
        if (hostersO instanceof Map) {
            /* 2019-07-15: They are using a map with numbers as String as key --> This is a workaround for that */
            final Map<String, Object> entries = (Map<String, Object>) hostersO;
            hosters = new ArrayList<Map<String, Object>>();
            Object tempO = null;
            do {
                if (counter == 0) {
                    /* 2019-07-15: Special case: Array might start with 1 so check here for Object on [0] */
                    tempO = entries.get("0");
                    if (tempO != null) {
                        hosters.add((Map<String, Object>) tempO);
                    }
                    counter++;
                }
                tempO = entries.get(Integer.toString(counter));
                if (tempO != null) {
                    hosters.add((Map<String, Object>) tempO);
                }
                counter++;
            } while (tempO != null && counter > 1);
        } else {
            /* 2019-07-15: In case they ever correct their Map to an Array, we will need the following line. */
            hosters = (List<Map<String, Object>>) restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
        }
        final ArrayList<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (final Map<String, Object> hostinfo : hosters) {
            final String domain = hostinfo.get("name").toString();
            final String status = (String) hostinfo.get("status");
            final String downsincedate = (String) hostinfo.get("downsincedate");
            final MultiHostHost mhost = new MultiHostHost(domain);
            if (!"up".equalsIgnoreCase(status)) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                if (!StringUtils.isEmpty(downsincedate)) {
                    mhost.setStatusText("Broken/Down since " + downsincedate);
                }
            }
            supportedhosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedhosts);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    private Map<String, Object> loginAPI(final Account account) throws IOException, PluginException, InterruptedException {
        synchronized (account) {
            account.setPass(account.getPass());
            br.getPage(API_BASE + "/user?auth=" + Encoding.urlEncode(account.getPass()));
            final Map<String, Object> user = handleAPIErrors(br, account, null);
            /*
             * Used logs in via apikey - via website, username & email are required. Set mail as username so: 1. User can identify different
             * accounts in JD better and 2. If someone steals the users' database he still cannot login via website!
             */
            final String email = (String) user.get("email");
            if (!StringUtils.isEmpty(email)) {
                account.setUser(email);
            }
            return user;
        }
    }

    private Map<String, Object> handleAPIErrors(final Browser br, final Account account, final DownloadLink link) throws PluginException, InterruptedException {
        final Map<String, Object> entries;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Invalid API response", 60 * 1000l);
            } else {
                throw new AccountUnavailableException("Invalid API response", 60 * 1000);
            }
        }
        final int error = ((Number) entries.get("error")).intValue();
        if (error == 0) {
            /* No error */
            return entries;
        }
        final String message = entries.get("message").toString();
        if (link == null) {
            throw new AccountInvalidException(message);
        } else {
            mhm.handleErrorGeneric(account, link, message, 50, 5 * 60 * 1000l);
        }
        /* Unreachable code */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("[a-zA-Z0-9_/\\+\\=\\-]+")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected String getAPILoginHelpURL() {
        return "https://" + getHost() + "/en/profile";
    }
}