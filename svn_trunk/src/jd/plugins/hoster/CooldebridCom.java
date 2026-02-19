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
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.BearerAuthentication;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
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

@HostPlugin(revision = "$Revision: 52328 $", interfaceVersion = 3, names = { "cooldebrid.com" }, urls = { "" })
public class CooldebridCom extends PluginForHost {
    private static final String          WEBSITE_BASE = "https://cooldebrid.com";
    private static final String          API_BASE     = "https://cooldebrid.com/api/v1";
    private static MultiHosterManagement mhm          = new MultiHosterManagement("cooldebrid.com");
    private static final boolean         resume       = true;
    private static final int             maxchunks    = -10;
    /* 2026-02-18: Use API */
    private static final boolean         use_api      = true;

    @SuppressWarnings("deprecation")
    public CooldebridCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(WEBSITE_BASE + "/register.html");
    }

    @Override
    public String getAGBLink() {
        return WEBSITE_BASE + "/tos";
    }

    private Browser prepBR(final Browser br) {
        br.setCookiesExclusive(true);
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
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        if (use_api) {
            return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.API_KEY_LOGIN };
        } else {
            return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST };
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        if (use_api) {
            return this.fetchAccountInfoAPI(account);
        } else {
            return this.fetchAccountInfoWebsite(account);
        }
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        if (!attemptStoredDownloadurlDownload(link)) {
            final String dllink;
            if (use_api) {
                dllink = this.generateDirectlinkAPI(link, account);
            } else {
                dllink = this.generateDirectlinkWebsite(link, account);
            }
            if (StringUtils.isEmpty(dllink)) {
                /* This should never happen */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            link.setProperty(this.getHost() + "directlink", dllink);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, resume, maxchunks);
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                mhm.handleErrorGeneric(account, link, "Unknown download error", 50, 5 * 60 * 1000l);
            }
        }
        this.dl.startDownload();
    }

    public String generateDirectlinkWebsite(final DownloadLink link, final Account account) throws Exception {
        this.loginWebsite(account, false);
        final Form dlform = new Form();
        dlform.setMethod(MethodType.POST);
        dlform.setAction(WEBSITE_BASE + "/api/admin/generate.php");
        dlform.put("link", Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this)));
        dlform.put("res", "");
        final Browser brc = br.cloneBrowser();
        this.setAjaxHeadersWebsite(brc);
        brc.getHeaders().put("Referer", WEBSITE_BASE + "/generate.html");
        brc.submitForm(dlform);
        final Map<String, Object> root = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        /* TODO: Add/improve errorhandling */
        final String dllink = (String) root.get("dl_link");
        if (StringUtils.isEmpty(dllink)) {
            final String msg = (String) root.get("msg");
            if (msg != null) {
                /*
                 * E.g. {"status":"error","msg":"Could Not Be Generate Link Please Try Again Later ..."}
                 */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, msg, 1 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, null, 1 * 60 * 1000l);
            }
        }
        return dllink;
    }

    public String generateDirectlinkAPI(final DownloadLink link, final Account account) throws Exception {
        this.loginAPI(account, false);
        final Map<String, Object> postdata = new HashMap<String, Object>();
        postdata.put("link", link.getDefaultPlugin().buildExternalDownloadURL(link, this));
        br.postPageRaw(API_BASE + "/link/unlock", JSonStorage.serializeToJson(postdata));
        final Map<String, Object> resp = handleAPIErrors(account, link);
        String dllink = resp.get("download_url").toString();
        if (StringUtils.isEmpty(dllink)) {
            /* This field was used in older/first API version */
            dllink = resp.get("unlocked_url").toString();
        }
        return dllink;
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link) throws Exception {
        final String directurlproperty = this.getHost() + "directlink";
        final String url = link.getStringProperty(directurlproperty);
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        boolean valid = false;
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, resume, maxchunks);
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                valid = true;
                return true;
            } else {
                link.removeProperty(directurlproperty);
                brc.followConnection(true);
                throw new IOException();
            }
        } catch (final Throwable e) {
            logger.log(e);
            return false;
        } finally {
            if (!valid) {
                try {
                    dl.getConnection().disconnect();
                } catch (Throwable ignore) {
                }
                dl = null;
            }
        }
    }

    private void setAjaxHeadersWebsite(final Browser br) {
        br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
    }

    private void loginWebsite(final Account account, final boolean validateLogins) throws Exception {
        synchronized (account) {
            prepBR(this.br);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                logger.info("Trying to re-use cookies");
                br.setCookies(cookies);
                if (!validateLogins) {
                    /* Trust cookies without checking. */
                    return;
                }
                br.getPage(WEBSITE_BASE + "/generate.html");
                if (this.isLoggedinWebsite(this.br)) {
                    logger.info("Cookie login successful");
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return;
                } else {
                    logger.info("Cookie login failed");
                    br.clearCookies(br.getHost());
                }
            }
            logger.info("Performing full login");
            br.getPage(WEBSITE_BASE);
            final Form loginform = br.getFormbyProperty("id", "login_form");
            if (loginform == null) {
                logger.warning("Failed to find loginform");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            loginform.setAction("/api/login.php");
            loginform.setMethod(MethodType.POST);
            loginform.put("username", Encoding.urlEncode(account.getUser()));
            loginform.put("userpass", Encoding.urlEncode(account.getPass()));
            final String captcha = this.getCaptchaCode(WEBSITE_BASE + "/api/antibot/index.php", new DownloadLink(this, "Account", this.getHost(), "https://" + account.getHoster(), true));
            loginform.put("antibot", Encoding.urlEncode(captcha));
            final Browser brc = br.cloneBrowser();
            setAjaxHeadersWebsite(brc);
            brc.submitForm(loginform);
            /* We expect a json response */
            final Map<String, Object> root = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (root.get("status").toString().equalsIgnoreCase("error")) {
                /*
                 * Usually e.g. {"status":"error","msg":"Security Code Incorrect"} or
                 * {"status":"error","msg":"Username Or Password Is Incorrect"}
                 */
                final String msg = (String) root.get("msg");
                if (!StringUtils.isEmpty(msg)) {
                    if (msg.equalsIgnoreCase("Security Code Incorrect")) {
                        /* Invalid login captcha */
                        throw new AccountUnavailableException(msg, 1 * 60 * 1000l);
                    } else {
                        throw new AccountInvalidException(msg);
                    }
                } else {
                    throw new AccountInvalidException();
                }
            }
            /*
             * {"status":"ok","msg":"Login Successful.."} --> Returns cookie user_lang, userid and userpw (some hash, always the same per
             * user [dangerous])
             */
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    private AccountInfo fetchAccountInfoWebsite(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        loginWebsite(account, true);
        if (!StringUtils.endsWithCaseInsensitive(br.getURL(), "/generate.html")) {
            br.getPage("/generate.html");
        }
        final String accountType = br.getRegex("(Free User|Premium User)\\s*</span>").getMatch(0);
        if (accountType == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Regex usedTrafficRegex = br.getRegex("id=\"used_bw\">([^<]+)</span>\\s*/\\s*([^<]+)");
        if (!usedTrafficRegex.patternFind()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final long trafficMax = SizeFormatter.getSize(usedTrafficRegex.getMatch(1));
        ai.setTrafficMax(trafficMax);
        ai.setTrafficLeft(trafficMax - SizeFormatter.getSize(usedTrafficRegex.getMatch(0)));
        final Regex usedLinksRegex = br.getRegex("id=\"used_links\">(\\d+)</span>\\s*/\\s*(\\d+)\\s*links");
        if (!usedLinksRegex.patternFind()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final int linksPerDayUsed = Integer.parseInt(usedLinksRegex.getMatch(0));
        final int linksPerDayMax = Integer.parseInt(usedLinksRegex.getMatch(1));
        final int linksPerDayLeft = linksPerDayMax - linksPerDayUsed;
        if (linksPerDayLeft <= 0) {
            logger.info("Setting zero traffic left because max daily links limit has been reached");
            ai.setTrafficLeft(0);
        }
        ai.setStatus(accountType + " | " + "Daily links left: " + linksPerDayLeft + "/" + linksPerDayMax);
        if (accountType.equalsIgnoreCase("Premium User")) {
            account.setType(AccountType.PREMIUM);
            final String daysLeft = br.getRegex("([\\d+\\.]+)\\s*Days\\s*Left\\s*<").getMatch(0);
            final String hoursLeft = br.getRegex("([\\d+\\.]+)\\s*Hours\\s*Left\\s*<").getMatch(0);
            if (daysLeft != null) {
                ai.setValidUntil(System.currentTimeMillis() + (long) (Double.parseDouble(daysLeft) * TimeUnit.DAYS.toMillis(1)), this.br);
            } else if (hoursLeft != null) {
                ai.setValidUntil(System.currentTimeMillis() + (long) (Double.parseDouble(hoursLeft) * TimeUnit.HOURS.toMillis(1)), this.br);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        } else {
            /*
             * 2022-02-22: Website claims to also support some hosts for free users but when this plugin was developed, they did not have a
             * single free host on their list.
             */
            account.setType(AccountType.FREE);
            ai.setExpired(true);
        }
        /* An alternative table overview of supported hosts can be found here: /host-status.html */
        final List<MultiHostHost> supportedHosts = new ArrayList<MultiHostHost>();
        final String[] htmls = br.getRegex("<tr>(.*?)</tr>").getColumn(0);
        if (htmls == null || htmls.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find list of supported hosts");
        }
        final HashSet<String> skippedHTMLs = new HashSet<String>();
        for (final String html : htmls) {
            final String domain = new Regex(html, "favicons\\?domain=([^\"]+)").getMatch(0);
            if (domain == null) {
                /* Skip invalid items */
                skippedHTMLs.add(html);
                continue;
            }
            final String[] columns = new Regex(html, "<td(.*?)</td>").getColumn(0);
            if (columns.length != 3) {
                logger.warning("Skipping row because of column length mismatch --> @developer! Check plugin code! -> html: " + html);
                skippedHTMLs.add(html);
                continue;
            }
            /* Skip hosts that are marked as broken/offline by this multihost */
            final String hostStatusColumn = columns[2].toLowerCase(Locale.ROOT);
            final MultiHostHost mhost = new MultiHostHost(domain);
            if (account.getType() != AccountType.PREMIUM && StringUtils.containsIgnoreCase(html, "Only Premium")) {
                /* User owns free account but only premium users can download from this host. */
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST_NOT_FOR_THIS_ACCOUNT_TYPE);
            } else if (hostStatusColumn.contains("online")) {
                mhost.setStatus(MultihosterHostStatus.WORKING);
            } else if (hostStatusColumn.contains("unstable")) {
                mhost.setStatus(MultihosterHostStatus.WORKING_UNSTABLE);
            } else {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            /*
             * Skip hosts if individual limits have been reached. Some have "unlimited" links or bandwidth -> Limit-RegEx will fail for them
             * which automatically makes them pass rthis check.
             */
            final String hostLimitsHTML = columns[1];
            final Regex maxLinkLimitRegex = new Regex(hostLimitsHTML, "used_count=\"[^\"]+\">(\\d+)</span>\\s*/\\s*(\\d+)\\s*link\\s*</p>");
            if (maxLinkLimitRegex.patternFind()) {
                /* Host has individual "max links per day" limits */
                final int mhostLinksUsed = Integer.parseInt(maxLinkLimitRegex.getMatch(0));
                final int mhostLinksMax = Integer.parseInt(maxLinkLimitRegex.getMatch(1));
                final int mhostLinksLeft = mhostLinksMax - mhostLinksUsed;
                mhost.setLinksLeftAndMax(mhostLinksLeft, mhostLinksMax);
            }
            final Regex maxQuotaRegex = new Regex(hostLimitsHTML, "used_mb=\"[^\"]+\">(\\d+(?:\\.\\d{1,2})? [A-Za-z]{1,5})</span>\\s*/\\s*(\\d+(?:\\.\\d{1,2})? [A-Za-z]{1,5})\\s*<br>");
            if (maxQuotaRegex.patternFind()) {
                /* Host has individual traffic limits */
                final long mhostTrafficUsed = SizeFormatter.getSize(maxQuotaRegex.getMatch(0));
                final long mhostTrafficMax = SizeFormatter.getSize(maxQuotaRegex.getMatch(1));
                final long mhostTrafficLeft = mhostTrafficMax - mhostTrafficUsed;
                mhost.setTrafficLeftAndMax(mhostTrafficLeft, mhostTrafficMax);
            }
            supportedHosts.add(mhost);
        }
        if (supportedHosts.isEmpty()) {
            logger.info("Found " + skippedHTMLs.size() + " unparseable html snippets");
        }
        ai.setMultiHostSupportV2(this, supportedHosts);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    private boolean isLoggedinWebsite(final Browser br) {
        if (br.containsHTML("href=\"javascript:logout\\(\\)")) {
            return true;
        } else {
            return false;
        }
    }

    private AccountInfo fetchAccountInfoAPI(final Account account) throws Exception {
        final Map<String, Object> user = loginAPI(account, true);
        final AccountInfo ai = new AccountInfo();
        account.setUser(user.get("email").toString());
        ai.setValidUntil(((Number) user.get("premium_until")).longValue() * 1000, this.br);
        br.getPage(API_BASE + "/traffic");
        final Map<String, Object> traffic = handleAPIErrors(account, null);
        final Map<String, Object> traffic_daily = (Map<String, Object>) traffic.get("daily");
        ai.setTrafficMax(((Number) traffic_daily.get("bw_limit_mb")).longValue() * 1024 * 1024);
        ai.setTrafficLeft(((Number) traffic_daily.get("bw_remaining_mb")).longValue() * 1024 * 1024);
        final int linksPerDayMax = ((Number) traffic_daily.get("links_limit")).intValue();
        final int linksPerDayLeft = ((Number) traffic_daily.get("links_remaining")).intValue();
        if (linksPerDayLeft <= 0) {
            logger.info("Setting zero traffic left because max daily links limit has been reached");
            ai.setTrafficLeft(0);
        }
        final boolean isPremium = !ai.isExpired();
        ai.setStatus(user.get("plan") + " | " + "Daily links left: " + linksPerDayLeft + "/" + linksPerDayMax);
        /* Get list of supported hosts with their individual limits */
        br.getPage(API_BASE + "/user/hosts");
        final Map<String, Object> user_hosts = handleAPIErrors(account, null);
        final List<Map<String, Object>> host_infos = (List<Map<String, Object>>) user_hosts.get("hosts");
        final List<MultiHostHost> supportedHosts = new ArrayList<MultiHostHost>();
        for (final Map<String, Object> host_info : host_infos) {
            final String domain = host_info.get("host").toString();
            final MultiHostHost mhost = new MultiHostHost(domain);
            final long links_remaining_count = ((Number) host_info.get("remaining_count")).longValue();
            final long links_daily_limit_count = ((Number) host_info.get("daily_limit_count")).longValue();
            if (links_remaining_count != -1 && links_daily_limit_count != -1) {
                mhost.setLinksLeftAndMax(links_remaining_count, links_daily_limit_count);
            }
            final long daily_limit_mb = ((Number) host_info.get("daily_limit_mb")).longValue();
            final long remaining_mb = ((Number) host_info.get("remaining_mb")).longValue();
            if (remaining_mb != -1 && daily_limit_mb != -1) {
                mhost.setTrafficLeftAndMax(remaining_mb * 1024 * 1024, daily_limit_mb * 1024 * 1024);
            }
            final String status = host_info.get("status").toString();
            if ("degraded".equalsIgnoreCase(status)) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            } else if (!isPremium && Boolean.TRUE.equals(host_info.get("premium_only"))) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST_NOT_FOR_THIS_ACCOUNT_TYPE);
            }
            supportedHosts.add(mhost);
        }
        ai.setMultiHostSupportV2(this, supportedHosts);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    private Map<String, Object> loginAPI(final Account account, final boolean verifyLogins) throws Exception {
        synchronized (account) {
            br.addAuthentication(new BearerAuthentication(this.getHost(), account.getPass(), null));
            if (!verifyLogins) {
                return null;
            }
            br.getPage(API_BASE + "/user");
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
        final String status = (String) entries.get("status");
        if (status.equalsIgnoreCase("success")) {
            /* No error */
            final Map<String, Object> data = (Map<String, Object>) entries.get("data");
            return data;
        }
        final Map<String, Object> errormap = (Map<String, Object>) entries.get("error");
        final String code = (String) errormap.get("code");
        final String message = (String) errormap.get("message");
        /* Permanent account errors */
        if ("AUTH_MISSING_TOKEN".equals(code) || "AUTH_BAD_TOKEN".equals(code) || "AUTH_TOKEN_REVOKED".equals(code) || "AUTH_USER_NOT_FOUND".equals(code) || "AUTH_USER_BANNED".equals(code) || "AUTH_NOT_PREMIUM".equals(code)) {
            throw new AccountInvalidException(message);
        }
        /* Temporary account errors */
        if ("AUTH_IP_BLOCKED".equals(code) || "RATE_LIMIT_EXCEEDED".equals(code)) {
            throw new AccountUnavailableException(message, 10 * 60 * 1000);
        }
        /* Fallback for unknown errors during account check */
        if (PluginEnvironment.ACCOUNT_CHECK.isCurrentPluginEnvironment()) {
            throw new AccountInvalidException(message);
        }
        /* Download link available -> download error */
        /* Host-related errors */
        if ("HOST_NOT_SUPPORTED".equals(code) || "HOST_OFFLINE".equals(code) || "DAILY_LINK_LIMIT".equals(code) || "DAILY_BW_LIMIT".equals(code) || "HOST_DAILY_LIMIT".equals(code) || "HOST_COUNT_LIMIT".equals(code)) {
            mhm.putError(account, link, 5 * 60 * 1000l, message);
            return null;
        }
        /* All other errors */
        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, message);
    }

    @Override
    protected String getAPILoginHelpURL() {
        return WEBSITE_BASE + "/api.html";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        } else if (str.matches("[a-zA-Z0-9]{40}")) {
            return true;
        } else {
            return false;
        }
    }
}