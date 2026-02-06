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
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.BearerAuthentication;
import jd.http.Browser;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 52266 $", interfaceVersion = 3, names = { "offcloud.com" }, urls = { "" })
public class OffCloudCom extends PluginForHost {
    /* API docs: https://offcloud.com/api */
    private static final String API_ENDPOINT = "https://offcloud.com/api";
    // private static final String PROPERTY_DIRECTURL = "offcloudcom_directlink";

    @SuppressWarnings("deprecation")
    public OffCloudCom(PluginWrapper wrapper) {
        super(wrapper);
        enablePremium(this.getBaseUrl() + "/#pricing");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setFollowRedirects(true);
        return br;
    }

    private String getBaseUrl() {
        return "https://" + getHost();
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    @Override
    public String getAGBLink() {
        return this.getBaseUrl() + "/terms";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws PluginException, IOException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    /** This should never get called. */
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        throw new AccountRequiredException();
    }

    @Override
    /** This should never get called. */
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        // TODO: Add functionality
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final Map<String, Object> user = login(account, true);
        final AccountInfo ai = new AccountInfo();
        /*
         * Users enter API key only from now on --> Try to find user_id in API answer and set it so accounts in JD still have unique
         * username strings!
         */
        account.setUser(user.get("user_id").toString());
        if (!Boolean.TRUE.equals(user.get("is_premium"))) {
            /* Free accounts cannot be used for downloading */
            ai.setExpired(true);
            return ai;
        }
        if (!Boolean.TRUE.equals(user.get("can_download"))) {
            /* We don't know the reason for this but if ca_download is false, let's put error state on account. */
            throw new AccountInvalidException("Account cannot be used for downloading, contact Offcloud support!");
        }
        final String expiration_date = user.get("expiration_date").toString();
        ai.setValidUntil(TimeFormatter.getMilliSeconds(expiration_date, "yyyy-MM-dd", Locale.ENGLISH), br);
        account.setType(AccountType.PREMIUM);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    private Map<String, Object> login(final Account account, final boolean verifyLogins) throws Exception {
        synchronized (account) {
            br.addAuthentication(new BearerAuthentication(this.getHost(), account.getPass(), null));
            if (!verifyLogins) {
                return null;
            }
            br.getPage(API_ENDPOINT + "/account/info");
            /* No error here = account is valid. */
            return handleAPIErrors(account, null);
        }
    }

    private Map<String, Object> handleAPIErrors(final Account account, final DownloadLink link) throws Exception {
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        } catch (final JSonMapperException ignore) {
            /* This should never happen. */
            final String msg = "Invalid API response";
            final long wait = 1 * 60 * 1000;
            if (link != null) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, wait);
            } else {
                throw new AccountUnavailableException(msg, wait);
            }
        }
        final String error = (String) entries.get("error");
        if (error == null) {
            /* No error */
            return entries;
        }
        if (error.equalsIgnoreCase("NOAUTH")) {
            throw new AccountInvalidException("Invalid API key");
        }
        if (PluginEnvironment.ACCOUNT_CHECK.isCurrentPluginEnvironment()) {
            /* Account is currently being checked -> Any error that happened now is an account error */
            throw new AccountInvalidException(error);
        } else {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, error);
        }
    }

    @Override
    protected String getAPILoginHelpURL() {
        return this.getBaseUrl() + "/account";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("[a-zA-Z0-9]{32}")) {
            return true;
        } else {
            return false;
        }
    }
}