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

import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperHostPluginRecaptchaV2;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.MultiHosterManagement;

@HostPlugin(revision = "$Revision: 51155 $", interfaceVersion = 3, names = {}, urls = {})
public class JubaGetCom extends PluginForHost {
    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final Account account) {
        return 0;
    }

    private String getDirecturlProperty() {
        return this.getHost() + "directlink";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "jubaget.com", "juba-get.com" });
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
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("");
        }
        return ret.toArray(new String[0]);
    }

    private static MultiHosterManagement mhm = new MultiHosterManagement("jubaget.com");

    public JubaGetCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/plans");
    }

    @Override
    public String rewriteHost(String host) {
        return this.rewriteHost(getPluginDomains(), host, this.getHost());
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setCookie(getHost(), "locale", "en");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.MULTIHOST, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws PluginException {
        return AvailableStatus.UNCHECKABLE;
    }

    @Override
    public void handleFree(DownloadLink downloadLink) throws Exception, PluginException {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handlePremium(DownloadLink link, Account account) throws Exception {
        /* handlePremium should never be called */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    @Override
    public void handleMultiHost(final DownloadLink link, final Account account) throws Exception {
        if (!attemptStoredDownloadurlDownload(link)) {
            login(account, true, "https://" + this.getHost() + "/generator");
            final String csrftoken = br.getRegex("<meta name=\"csrf-token\" content=\"([^\"]+)\"").getMatch(0);
            final UrlQuery query = new UrlQuery();
            query.add("url", Encoding.urlEncode(link.getDefaultPlugin().buildExternalDownloadURL(link, this)));
            br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
            if (csrftoken != null) {
                br.getHeaders().put("x-csrf-token", csrftoken);
            }
            br.postPage("/api/generate", query);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String dllink = (String) entries.get("download");
            if (StringUtils.isEmpty(dllink)) {
                /* E.g. {"error":true,"error_message":"Error generate"} */
                final String error_message = (String) entries.get("error_message");
                if (!StringUtils.isEmpty(error_message)) {
                    mhm.handleErrorGeneric(account, link, error_message, 50, 1 * 60 * 1000l);
                } else {
                    mhm.handleErrorGeneric(account, link, "Failed to generate downloadlink", 50, 1 * 60 * 1000l);
                }
            }
            link.setProperty(this.getDirecturlProperty(), dllink);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, account), this.getMaxChunks(account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                mhm.handleErrorGeneric(account, link, "Final downloadurl did not lead to downloadable content", 50, 1 * 60 * 1000l);
            }
        }
        this.dl.startDownload();
    }

    private boolean attemptStoredDownloadurlDownload(final DownloadLink link) throws Exception {
        final String url = link.getStringProperty(getDirecturlProperty());
        if (StringUtils.isEmpty(url)) {
            return false;
        }
        boolean valid = false;
        try {
            final Browser brc = br.cloneBrowser();
            dl = new jd.plugins.BrowserAdapter().openDownload(brc, link, url, isResumeable(link, null), this.getMaxChunks(null));
            if (this.looksLikeDownloadableContent(dl.getConnection())) {
                valid = true;
                return true;
            } else {
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
                this.dl = null;
            }
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        login(account, true, "https://" + this.getHost() + "/generator");
        final AccountInfo ai = new AccountInfo();
        final String trafficUsedToday = br.getRegex("<b>\\s*Total today:([^<]+)</b><br>").getMatch(0);
        final String trafficUsedEver = br.getRegex("<b>\\s*Total generated:([^<]+)<").getMatch(0);
        final String expireDate = br.getRegex("Expires in\\s*:\\s*([^<>\n\r\t]+)").getMatch(0);
        if (expireDate != null) {
            ai.setValidUntil(TimeFormatter.getMilliSeconds(expireDate, "MMMM dd, yyyy", Locale.ENGLISH), br);
            account.setType(AccountType.PREMIUM);
            ai.setUnlimitedTraffic();
        } else {
            /* No expire date found --> Assume it's a free account. */
            account.setType(AccountType.FREE);
            // ai.setExpired(true);
            /* Free accounts can be used to download from some specific hosts, see: https://juba-get.com/hosts */
        }
        if (trafficUsedToday != null && trafficUsedEver != null) {
            ai.setStatus(account.getType().getLabel() + " | Used today: " + trafficUsedToday.trim() + " | Total: " + trafficUsedEver.trim());
        }
        /* They do not have an API so we need to extract the host limit information from HTML. */
        /*
         * The website can contain the same information multiple times (same host as working and not working) so we try to narrow it down by
         * pre-filtering the html source.
         */
        String htmlsource = br.getRegex("div class=\"card-body\">(\\s*<img.*?)</div>\\s*</div>").getMatch(0);
        if (htmlsource == null) {
            /* Fallback */
            htmlsource = br.getRequest().getHtmlCode();
        }
        final String[] htmls = htmlsource.split("<img");
        final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
        for (final String html : htmls) {
            String hostWithoutTLD = new Regex(html, "data-original-title=\"([^\" \\(]+)\"").getMatch(0);
            if (hostWithoutTLD == null) {
                // data-original-title="forum.com (hoster.com)">
                hostWithoutTLD = new Regex(html, "data-original-title=\"[^\"]*\\(([^\"\\)]+)\\)").getMatch(0);
            }
            if (hostWithoutTLD != null && hostWithoutTLD.contains(",")) {
                /* Workaround e.g. for entry "Tezfiles" */
                hostWithoutTLD = new Regex(html, "alt=\"([^\"]+)\"").getMatch(0);
            }
            final String serverStatus = new Regex(html, "(servidor_offline|servidor_online)").getMatch(0);
            if (hostWithoutTLD == null || serverStatus == null) {
                /* Skip invalid items */
                // logger.info("Skipping invalid supported host html snippet: " + html);
                continue;
            }
            final MultiHostHost mhost;
            /* Some small corrections needed, because their website is horrible to parse. */
            if (hostWithoutTLD.equalsIgnoreCase("DropDownload")) {
                mhost = new MultiHostHost("drop.download");
            } else if (hostWithoutTLD.equalsIgnoreCase("FreeDLink")) {
                mhost = new MultiHostHost("freedl.ink");
            } else if (hostWithoutTLD.equalsIgnoreCase("Fast-Down")) {
                mhost = new MultiHostHost("down.fast-down.com");
            } else {
                mhost = new MultiHostHost(hostWithoutTLD);
            }
            if (serverStatus.equalsIgnoreCase("servidor_offline")) {
                mhost.setStatus(MultihosterHostStatus.DEACTIVATED_MULTIHOST);
            }
            final String hostValueForTrafficRegex;
            if (hostWithoutTLD.equalsIgnoreCase("DropDownload")) {
                hostValueForTrafficRegex = "DropAPK \\(DropDownload\\)";
            } else {
                hostValueForTrafficRegex = hostWithoutTLD;
            }
            final Regex dailyLimitRegex = br.getRegex("data-original-title=\"" + hostValueForTrafficRegex + "\">[^<]+<b>\\((\\d+[^/]+)/([^<]+)\\)</b><br>");
            if (dailyLimitRegex.patternFind()) {
                final long trafficUsed = SizeFormatter.getSize(dailyLimitRegex.getMatch(0));
                final long trafficMax = SizeFormatter.getSize(dailyLimitRegex.getMatch(1));
                mhost.setTrafficMax(trafficMax);
                mhost.setTrafficLeft(trafficMax - trafficUsed);
            } else {
                /* This is okay. Not all hosts got individual limits. */
                logger.info("Failed to find detailed limits for host: " + hostWithoutTLD);
            }
            supportedhosts.add(mhost);
        }
        if (supportedhosts.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find list of supported hosts");
        }
        /* Other source for supported hosts down below but it is not a good source because the entries are spread over multiple pages. */
        // br.getPage("/hosts");
        // final String[] htmls = br.getRegex("<tr>(.*?)</tr>").getColumn(0);
        // final ArrayList<String> supportedHosts = new ArrayList<String>();
        // for (final String html : htmls) {
        // final String host = new Regex(html, "class=\"servidor_online\"[^>]*alt=\"([^\"]+)\"").getMatch(0);
        // if (host == null) {
        // /* Bad HTML or broken plugin. */
        // continue;
        // }
        // supportedHosts.add(host);
        // }
        ai.setMultiHostSupportV2(this, supportedhosts);
        return ai;
    }

    private void login(final Account account, final boolean force, final String loginCheckURL) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                this.br.setCookies(getHost(), cookies);
                if (!force) {
                    return;
                }
                br.getPage(loginCheckURL);
                if (isLoggedIN(br)) {
                    logger.info("Cookie login successful");
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return;
                }
                logger.info("Cookie login failed");
                account.clearCookies("");
                br.clearCookies(br.getHost());
            }
            br.getPage("https://" + getHost() + "/locale/en");
            br.getPage("/login");
            final Form loginform = br.getFormbyActionRegex(".*/login.*");
            if (loginform == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            loginform.put("email", Encoding.urlEncode(account.getUser()));
            loginform.put("password", Encoding.urlEncode(account.getPass()));
            loginform.put("remember", "on"); // make sure that cookies last long
            loginform.put("g-recaptcha-response", Encoding.urlEncode(new CaptchaHelperHostPluginRecaptchaV2(this, br).getToken()));
            br.submitForm(loginform);
            final String errorMsg = br.getRegex("div class=\"alert alert-danger\">\\s*<ul>\\s*<li>([^<]+)</li>").getMatch(0);
            /*
             * 2022-08-10: Sometimes even after successful login website will redirect us to /generator and display error 500 this we'll try
             * this small workaround.
             */
            if (!isLoggedIN(br)) {
                br.getPage("/");
            }
            if (!isLoggedIN(br)) {
                if (errorMsg != null) {
                    throw new AccountInvalidException(Encoding.htmlDecode(errorMsg));
                } else {
                    throw new AccountInvalidException();
                }
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    private boolean isLoggedIN(final Browser br) {
        if (br.containsHTML("/logout")) {
            return true;
        } else {
            return false;
        }
    }
}