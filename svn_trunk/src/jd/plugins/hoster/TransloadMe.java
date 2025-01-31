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

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.config.Property;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
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

@HostPlugin(revision = "$Revision: 50303 $", interfaceVersion = 3, names = { "transload.me" }, urls = { "" })
public class TransloadMe extends PluginForHost {
    private static final String          API_BASE                     = "https://api.transload.me/";
    private static final String          NORESUME                     = "transload_me_NORESUME";
    /* Connection limits */
    private static final boolean         ACCOUNT_PREMIUM_RESUME       = true;
    private static final int             ACCOUNT_PREMIUM_MAXCHUNKS    = 0;
    private static final int             ACCOUNT_PREMIUM_MAXDOWNLOADS = -1;
    private static MultiHosterManagement mhm                          = new MultiHosterManagement("transload.me");

    public TransloadMe(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("http://en." + getHost() + "/?p=register");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader " + getVersion());
        br.setCookie(getHost(), "lang", "en");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "http://en." + getHost() + "/?p=login&redir=helpdesk";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(DownloadLink downloadLink) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(DownloadLink link, Account account) throws Exception {
        /* handle premium should never be called */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        String dllink = checkDirectLink(link, this.getHost() + "directlink");
        if (dllink == null) {
            dllink = generateDownloadlinkAPI(account, link);
            if (dllink == null) {
                /* Should never happen */
                mhm.handleErrorGeneric(account, link, "dllinknull", 30, 2 * 60 * 1000l);
            }
        }
        handleDL(account, link, dllink);
    }

    @SuppressWarnings("deprecation")
    private void handleDL(final Account account, final DownloadLink link, final String dllink) throws Exception {
        /* we want to follow redirects in final stage */
        boolean resume = ACCOUNT_PREMIUM_RESUME;
        if (link.getBooleanProperty(NORESUME, false)) {
            resume = false;
            link.setProperty(NORESUME, Boolean.valueOf(false));
        }
        link.setProperty(this.getHost() + "directlink", dllink);
        try {
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, dllink, resume, ACCOUNT_PREMIUM_MAXCHUNKS);
            if (dl.getConnection().getResponseCode() == 416) {
                logger.info("Resume impossible, disabling it for the next try");
                link.setChunksProgress(null);
                link.setProperty(NORESUME, Boolean.valueOf(true));
                throw new PluginException(LinkStatus.ERROR_RETRY);
            }
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection();
                handleErrorsAPI(br, account, link);
                mhm.handleErrorGeneric(account, link, "unknowndlerror", 5, 2 * 60 * 1000l);
            }
            dl.startDownload();
        } catch (final Exception e) {
            link.setProperty(this.getHost() + "directlink", Property.NULL);
            throw e;
        }
    }

    private String checkDirectLink(final DownloadLink downloadLink, final String property) {
        final String dllink = downloadLink.getStringProperty(property);
        if (dllink != null) {
            URLConnectionAdapter con = null;
            try {
                final Browser br2 = br.cloneBrowser();
                con = br2.openHeadConnection(dllink);
                if (con.isOK() && (con.isContentDisposition() || (!con.getContentType().contains("html") && con.getLongContentLength() > 0))) {
                    return dllink;
                }
            } catch (final Exception e) {
                logger.log(e);
            } finally {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
            downloadLink.setProperty(property, Property.NULL);
        }
        return null;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        return fetchAccountInfoAPI(account);
    }

    private String generateDownloadlinkAPI(final Account account, DownloadLink link) throws Exception {
        getApi("require=downloadfile&link=" + Encoding.urlEncode(link.getDownloadURL()), account, link);
        final String dllink = PluginJSonUtils.getJsonValue(br, "link");
        return dllink;
    }

    private AccountInfo fetchAccountInfoAPI(final Account account) throws Exception {
        synchronized (account) {
            final AccountInfo ai = new AccountInfo();
            getApi("require=accountdetalis", account, null);
            final String result = PluginJSonUtils.getJsonValue(br, "result");
            if ("5".equals(result)) {
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            } else if (!"0".equals(result)) {
                final String error = PluginJSonUtils.getJson(br, "error");
                if (error != null) {
                    /* 2019-08-30: Hmm seems like their API is dead as it always returns error 6 */
                    throw new AccountInvalidException(error);
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            /* Balance left in USD */
            final String balance = PluginJSonUtils.getJsonValue(br, "balance");
            final String reg_date = PluginJSonUtils.getJsonValue(br, "reg_date");
            if (reg_date != null) {
                ai.setCreateTime(TimeFormatter.getMilliSeconds(reg_date, "yyyy-MM-dd", Locale.ENGLISH));
            }
            if (balance == null || Double.parseDouble(balance) <= 0) {
                account.setType(AccountType.FREE);
                throwZeroBalance(account);
            } else {
                account.setType(AccountType.PREMIUM);
                account.setMaxSimultanDownloads(ACCOUNT_PREMIUM_MAXDOWNLOADS);
                ai.setStatus("Premium account balance " + balance + " USD");
                /*
                 * Set unlimited traffic as each filehost costs a different amount of money per GB see:
                 * http://en.transload.me/index.php?p=statistic
                 */
                ai.setUnlimitedTraffic();
            }
            getApi("require=supporthost", account, null);
            final Map<String, Object> resp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final List<Map<String, Object>> list = (List<Map<String, Object>>) resp.get("host_list");
            final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
            for (final Map<String, Object> entry : list) {
                final MultiHostHost mhost = new MultiHostHost(entry.get("host").toString());
                final String status = String.valueOf(entry.get("status"));
                // 0 - Works
                // 1 - Unstable
                // 2 - does Not work
                // 3 - Support file exchanger in the recovery process
                if (status.equals("1")) {
                    mhost.setStatus(MultihosterHostStatus.WORKING_UNSTABLE);
                } else if (status.equals("2")) {
                    mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                } else if (status.equals("3")) {
                    mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
                }
                supportedhosts.add(mhost);
            }
            ai.setMultiHostSupportV2(this, supportedhosts);
            return ai;
        }
    }

    private void throwZeroBalance(final Account account) throws PluginException {
        synchronized (account) {
            AccountInfo ai = account.getAccountInfo();
            if (ai == null) {
                /* E.g. account gets added for the first time AND no balance left. */
                ai = new AccountInfo();
            }
            ai.setTrafficLeft(0);
            account.setAccountInfo(ai);
            throw new PluginException(LinkStatus.ERROR_PREMIUM, "Account balance 0.00 USD!", PluginException.VALUE_ID_PREMIUM_DISABLE);
        }
    }

    /**
     * http://transload.me/en/?p=api
     *
     * @param input
     * @param account
     * @param link
     * @throws Exception
     */
    private void getApi(final String input, final Account account, final DownloadLink link) throws Exception {
        String accesslink = API_BASE + "?" + input + "&username=" + URLEncode.encodeURIComponent(account.getUser()) + "&password=" + URLEncode.encodeURIComponent(account.getPass());
        // accesslink += "&client_id=jdownloader";
        br.getPage(accesslink);
        handleErrorsAPI(br, account, link);
    }

    private int updatestatuscodeAPI() {
        final String result = PluginJSonUtils.getJsonValue(br, "result");
        if (result != null && result.matches("\\d+")) {
            return Integer.parseInt(result);
        } else {
            return 0;
        }
    }

    /* Please do not remove this function - future usage!! */
    private void handleErrorsAPI(final Browser br, final Account account, final DownloadLink link) throws Exception {
        final int statuscode = updatestatuscodeAPI();
        String statusMessage = null;
        try {
            switch (statuscode) {
            case 0:
                // "error": "0" - the Request succeeds, if the code is 0, then you can process the result
                break;
            case 1:
                // "error": "1" - the File is not found or has been deleted.
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            case 2:
                // "error": "2" - file Sharing is not supported.
                // should be mh wide
                statusMessage = "Unsupported host";
                mhm.handleErrorGeneric(account, link, "unsupported_host", 5, 5 * 60 * 1000l);
            case 3:
                // "error": "3" - system error Occurred while processing, please try again later.
                statusMessage = "Temporary error occured";
                mhm.handleErrorGeneric(account, link, "temporary_error", 5, 2 * 60 * 1000l);
            case 4:
                // "error": "4" - On account of insufficient funds, replenish your balance.
                // update account scraping
                account.setAccountInfo(fetchAccountInfoAPI(account));
                mhm.handleErrorGeneric(account, link, "credits", 2, 10 * 60 * 1000l);
            case 5:
                // "error": "5" is Used or password is incorrect.
                throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
            case 6:
                // "error": "6" - Invalid method request.
                statusMessage = "Invalid API request - this should never happen!";
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            case 7:
                // "error": "7" - You asked for too many links, wait a few minutes, or refill your account. (This occurs when the user has
                // generated a lot of links, though lacking means for downloading, you need to wait a few minutes before clearing balance)
                // so no more links for the account?
                statusMessage = "You asked for too many links, wait a few minutes, or refill your account";
                throw new AccountUnavailableException(statusMessage, 5 * 60 * 1000l);
            case 8:
                // disabled api, switch to webmode
                throw new AccountUnavailableException("API is disabled", 60 * 60 * 1000l);
            default:
                mhm.handleErrorGeneric(account, link, "unknown_error_state", 50, 2 * 60 * 1000l);
            }
        } catch (final PluginException e) {
            logger.info(this.getHost() + ": Exception: statusCode: " + statuscode + " statusMessage: " + statusMessage);
            throw e;
        }
    }
}