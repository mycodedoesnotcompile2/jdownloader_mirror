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

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Hash;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51437 $", interfaceVersion = 3, names = {}, urls = {})
public class CloudgenicNet extends PluginForHost {
    public CloudgenicNet(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/premium");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms-of-service";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "cloudgenic.net" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/file/([a-f0-9]{32})(/([^/]+))?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 1;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("class=\"fa fa-exclamation\"")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("title>\\s*Download([^<]+)</title>").getMatch(0);
        if (!StringUtils.isEmpty(filename)) {
            link.setName(Encoding.htmlDecode(filename.trim()));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        if (account != null) {
            login(account, false);
        }
        final String directlinkproperty = "directurl_" + (account != null ? account.getType().getLabel() : null);
        final String storedDirecturl = link.getStringProperty(directlinkproperty);
        String dllink;
        if (storedDirecturl != null) {
            logger.info("Re-using stored directurl: " + storedDirecturl);
            dllink = storedDirecturl;
        } else {
            requestFileInformation(link);
            final String statusText = br.getRegex("<b>Status:?\\s*</b>([^<]+)<").getMatch(0);
            if (statusText != null && StringUtils.containsIgnoreCase(statusText, "only for")) {
                throw new AccountRequiredException(statusText);
            }
            dllink = br.getRegex("window\\.location\\.href\\s*=\\s*\"(https?://[^\"]+)").getMatch(0);
            if (dllink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        try {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                if (dl.getConnection().getResponseCode() == 403) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
                } else if (dl.getConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
                }
                try {
                    br.followConnection(true);
                } catch (final IOException e) {
                    logger.log(e);
                }
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
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

    private void login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setFollowRedirects(true);
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            final String path_account_overview = "/user-account";
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
                if (!force) {
                    /* Don't validate cookies */
                    return;
                }
                br.getPage("https://" + this.getHost() + path_account_overview);
                if (this.isLoggedin(br)) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(null);
                }
            }
            logger.info("Performing full login");
            br.getPage("https://" + getHost() + "/login");
            final Form loginform = new Form();
            loginform.setMethod(MethodType.POST);
            loginform.setAction("/core/process.php");
            loginform.put("action", "login");
            final String lp_token = br.getRegex("var lp = \"([a-f0-9]{32})").getMatch(0);
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("email", account.getUser());
            postdata.put("password", Hash.getMD5(account.getPass()));
            /* Make cookies last long -> 30 days */
            postdata.put("defaultCheck1", "Agreed");
            postdata.put("lp", lp_token);
            loginform.put("data", Encoding.urlEncode(JSonStorage.serializeToJson(postdata)));
            final Browser brc = br.cloneBrowser();
            brc.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
            brc.getHeaders().put("Origin", "https://" + br.getHost());
            brc.getHeaders().put("x-requested-with", "XMLHttpRequest");
            brc.submitForm(loginform);
            if (brc.getRequest().getHtmlCode().startsWith("{")) {
                /* json response -> Parse it */
                final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                final String errormsg = (String) entries.get("error");
                if (errormsg != null) {
                    throw new AccountInvalidException(errormsg);
                }
            }
            /* Double-check login state */
            br.getPage(path_account_overview);
            if (!isLoggedin(br)) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    private boolean isLoggedin(final Browser br) {
        return br.containsHTML("onclick=\"signout");
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true);
        final AccountInfo ai = new AccountInfo();
        ai.setUnlimitedTraffic();
        final Date expireDate = extractAccountExpireDate(br);
        if (expireDate == null) {
            account.setType(AccountType.FREE);
            /* free accounts can still have captcha */
            account.setConcurrentUsePossible(false);
            ai.setExpired(true);
        } else {
            ai.setValidUntil(expireDate.getTime());
            account.setType(AccountType.PREMIUM);
            account.setConcurrentUsePossible(true);
        }
        final Date accountCreateDate = extractAccountCreateDate(br);
        if (accountCreateDate != null) {
            ai.setCreateTime(accountCreateDate.getTime());
        }
        final String planName = br.getRegex("<th[^>]*>\\s*<span[^>]*>\\s*<i[^>]*></i>\\s*Curent plan:\\s*</span>\\s*</th>\\s*<td>\\s*<span[^>]*>\\s*([^<]+)\\s*</span>").getMatch(0);
        if (planName != null) {
            /* Find auto renewal status */
            boolean autoRenewEnabled = false;
            String autoRenewalStatus = br.getRegex("<th[^>]*>\\s*<span[^>]*>\\s*<i[^>]*></i>\\s*Auto-Renewal:\\s*</span>\\s*</th>\\s*<td>\\s*<span[^>]*>([^<]+)</span>").getMatch(0);
            if (autoRenewalStatus != null) {
                autoRenewEnabled = !autoRenewalStatus.trim().equalsIgnoreCase("Canceled");
            }
            /* Set status text */
            ai.setStatus(Encoding.htmlDecode(planName).trim() + " | Auto-Renewal: " + autoRenewEnabled);
        }
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    /**
     * Extracts the account creation date from the HTML content
     *
     * @return Date object if found, null otherwise
     */
    public static Date extractAccountCreateDate(final Browser br) {
        String dateStr = br.getRegex("<th[^>]*>\\s*<span[^>]*>\\s*<i[^>]*></i>\\s*Signup date:?\\s*</span>\\s*</th>\\s*<td>\\s*<span[^>]*>([^<]+)</span>").getMatch(0);
        if (dateStr == null) {
            return null;
        }
        try {
            SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy", Locale.ENGLISH);
            return sdf.parse(dateStr);
        } catch (ParseException e) {
            return null;
        }
    }

    /**
     * Extracts the account expiration date by finding all dates with pattern MM/dd/yyyy and returning the highest one
     */
    public static Date extractAccountExpireDate(final Browser br) {
        // Get all dates in MM/dd/yyyy format
        String[] allDates = br.getRegex("(\\d{2}/\\d{2}/\\d{4})").getColumn(0);
        Date highestDate = null;
        SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy", Locale.ENGLISH);
        for (String dateStr : allDates) {
            try {
                Date date = sdf.parse(dateStr);
                if (highestDate == null || date.after(highestDate)) {
                    highestDate = date;
                }
            } catch (ParseException e) {
                // Ignore parsing errors
            }
        }
        return highestDate;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}