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

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountUnavailableException;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.jdownloader.plugins.components.XFileSharingProBasic;
import org.jdownloader.plugins.components.config.XFSConfigDdownloadCom;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

@HostPlugin(revision = "$Revision: 51361 $", interfaceVersion = 3, names = {}, urls = {})
public class DdownloadCom extends XFileSharingProBasic {
    public DdownloadCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info: 2019-05-22: premium untested, set FREE account limits <br />
     * captchatype-info: 2020-05-18: reCaptchaV2<br />
     * other:<br />
     */
    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public void setBrowser(Browser br) {
        super.setBrowser(br);
        if (br != null) {
            // re by admin
            br.setIPVersion(IPVERSION.IPV4_IPV6);
        }
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser ret = super.createNewBrowserInstance();
        // re by admin
        ret.setIPVersion(IPVERSION.IPV4_IPV6);
        ret.setHeader(HTTPConstants.HEADER_REQUEST_USER_AGENT, "JDownloader2");
        return ret;
    }

    @Override
    protected String[] supportsPreciseExpireDate() {
        return new String[] { "/?op=payments" };
    }

    @Override
    public String[] siteSupportedNames() {
        final String[] extraNames = { "ddl" };
        final String[] officiallySupportedNames = buildSupportedNames(getPluginDomains());
        String[] finalSupportedNames = new String[officiallySupportedNames.length + extraNames.length];
        System.arraycopy(officiallySupportedNames, 0, finalSupportedNames, 0, officiallySupportedNames.length);
        System.arraycopy(extraNames, 0, finalSupportedNames, officiallySupportedNames.length, extraNames.length);
        return finalSupportedNames;
    }

    public static String[] getAnnotationUrls() {
        return XFileSharingProBasic.buildAnnotationUrls(getPluginDomains());
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "ddownload.com", "ddl.to", "api.ddl.to", "esimpurcuesc.ddownload.com",
                /*
                 * download cdn, dedicated to ddownload?
                 */"ucdn.to" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("api.ddl.to");
        deadDomains.add("esimpurcuesc.ddownload.com");
        return deadDomains;
    }

    @Override
    public String rewriteHost(String host) {
        return this.rewriteHost(getPluginDomains(), host, new String[0]);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return true;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return true;
        }
    }

    @Override
    public String buildContainerDownloadURL(DownloadLink downloadLink, PluginForHost buildForThisPlugin) {
        final String ret = downloadLink.getContentUrl();
        if (ret != null) {
            return ret;
        } else {
            return buildExternalDownloadURL(downloadLink, buildForThisPlugin);
        }
    }

    @Override
    public String buildExternalDownloadURL(final DownloadLink link, final PluginForHost buildForThisPlugin) {
        final String fid = getFUIDFromURL(link);
        if (fid != null) {
            if (StringUtils.startsWithCaseInsensitive(link.getPluginPatternMatcher(), "https:")) {
                return "https://" + getHost() + "/" + fid;
            } else if (this.useHTTPS()) {
                return "https://" + getHost() + "/" + fid;
            } else {
                return "http://" + getHost() + "/" + fid;
            }
        } else {
            return super.buildExternalDownloadURL(link, buildForThisPlugin);
        }
    }

    @Override
    public int getMaxChunks(final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return 1;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return 1;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    public int getMaxDownloadSelect() {
        return PluginJsonConfig.get(this.getConfigInterface()).getMaxSimultaneousFreeDownloads();
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return getMaxDownloadSelect();
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return getMaxDownloadSelect();
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 10;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2020-01-16: Special: Disabled as it would return invalid results */
        return false;
    }

    @Override
    public void handleCaptcha(final DownloadLink link, Browser br, final Form captchaForm) throws Exception {
        /* 2019-08-14: Special: This might increase downloadspeed for free users */
        if (captchaForm != null && captchaForm.hasInputFieldByName("adblock_detected")) {
            captchaForm.put("adblock_detected", "0");
        }
        super.handleCaptcha(link, br, captchaForm);
    }

    @Override
    protected String regExTrafficLeft(final Browser br) {
        /* 2019-11-03: Special */
        final String src = this.getCorrectBR(br);
        final Regex trafficleft = new Regex(src, "<span>Traffic available</span>\\s*<div class=\"price\"><sup>([^<>]+)</sup>(-?\\d+)</div>");
        String availabletraffic = null;
        final String trafficleftUnit = trafficleft.getMatch(0);
        final String trafficleftTmp = trafficleft.getMatch(1);
        if (trafficleftUnit != null && trafficleftTmp != null) {
            availabletraffic = trafficleftTmp + trafficleftUnit;
        }
        if (availabletraffic == null) {
            /* Fallback to template handling */
            availabletraffic = super.regExTrafficLeft(br);
        }
        return availabletraffic;
    }

    @Override
    public void doFree(final DownloadLink link, final Account account) throws Exception, PluginException {
        if (checkShowFreeDialog(getHost())) {
            showFreeDialog(getHost());
        }
        super.doFree(link, account);
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        /* 2020-05-17 */
        super.scanInfo(html, fileInfo);
        String filename = new Regex(html, "<div class=\"name position-relative\">\\s*<h4>([^<>\"]+)</h4>").getMatch(0);
        if (StringUtils.isEmpty(filename)) {
            /* 2021-03-25 */
            filename = new Regex(html, ">File\\s*:\\s*<font[^>]*>([^<>\"]+)<").getMatch(0);
        }
        String filesize = new Regex(html, "class=\"file-size\">([^<>\"]+)<").getMatch(0);
        if (StringUtils.isEmpty(filesize)) {
            /* 2021-03-25 */
            filesize = new Regex(html, "\\[<font[^>]*>(\\d+[^<>\"]+)</font>\\]").getMatch(0);
        }
        if (!StringUtils.isEmpty(filename)) {
            fileInfo[0] = filename;
        }
        if (!StringUtils.isEmpty(filesize)) {
            fileInfo[1] = filesize;
        }
        return fileInfo;
    }

    @Override
    protected boolean allowAPIDownloadIfApikeyIsAvailable(final DownloadLink link, final Account account) {
        /* 2024-11-28: Public file downloads via API were turned off by the admins. */
        return false;
    }

    @Override
    protected AccountInfo fetchAccountInfoAPI(final Browser br, final Account account) throws Exception {
        final Browser brc = br.cloneBrowser();
        final AccountInfo ai = super.fetchAccountInfoAPI(brc, account);
        /* Original XFS API ('API Mod') does not return trafficleft but theirs is modified and more useful! */
        /* 2019-11-27: Not sure but this must be the traffic you can buy via 'extend traffic': /?op=payments */
        if (AccountType.PREMIUM != account.getType()) {
            /* Not a premium accout -> No reason to look for additional traffic information */
            /*
             * They will return "traffic_left":"0" for free accounts which is wrong. It is unlimited on their website. By setting it to
             * unlimited here it will be re-checked via website by our XFS template!
             */
            ai.setUnlimitedTraffic();
            return ai;
        }
        final String trafficleftStr = PluginJSonUtils.getJson(brc, "traffic_left");
        // final String trafficusedStr = PluginJSonUtils.getJson(brc, "traffic_used");
        /*
         * 2020-02-17: Their API has a bug where it randomly returns wrong values for some users and they did not fix it within 2 weeks:
         * https://board.jdownloader.org/showthread.php?t=82525&page=2
         */
        /* 2020-06-29: API returns wrong trafficleft values --> Don't trust it - obtain trafficleft value from website instead! */
        if (trafficleftStr != null && trafficleftStr.matches("\\d+")) {
            final boolean trustAPITrafficLeft = false;
            long traffic_left = SizeFormatter.getSize(trafficleftStr + "MB");
            final String premium_extra_trafficStr = PluginJSonUtils.getJson(brc, "premium_traffic_left");
            if (premium_extra_trafficStr != null && premium_extra_trafficStr.matches("\\d+")) {
                final long premium_extra_traffic = SizeFormatter.getSize(premium_extra_trafficStr + "MB");
                if (premium_extra_traffic > 0) {
                    traffic_left += premium_extra_traffic;
                    if (ai.getStatus() != null) {
                        ai.setStatus(ai.getStatus() + " | Extra traffic available: " + SIZEUNIT.formatValue((SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue(), premium_extra_traffic));
                    } else {
                        ai.setStatus("Premium account | Extra traffic available: " + SIZEUNIT.formatValue((SIZEUNIT) CFG_GUI.MAX_SIZE_UNIT.getValue(), premium_extra_traffic));
                    }
                }
            } else {
                logger.warning("Detected invalid premium_traffic_left value");
            }
            if (trustAPITrafficLeft) {
                logger.info("Trust API trafficleft value: " + traffic_left);
                ai.setTrafficLeft(traffic_left);
            } else {
                logger.info("Setting unlimited traffic instead of API trafficleft value '" + traffic_left + "' to prefer website value.");
                ai.setUnlimitedTraffic();
            }
        } else {
            logger.warning("Detected invalid traffic_left value");
        }
        return ai;
    }

    @Override
    public boolean isLoggedin(Browser br) {
        /* 2020-09-02: Allow "xfss" cookie without "login" cookie! */
        final String mainpage = getMainPage();
        logger.info("Doing login-cookiecheck for: " + mainpage);
        final boolean login_xfss_CookieOkay = br.getCookie(mainpage, "xfss", Cookies.NOTDELETEDPATTERN) != null;
        /* buttons or sites that are only available for logged in users */
        // remove script tags
        // remove comments, eg ddl.to just comment some buttons/links for expired cookies/non logged in
        final String htmlWithoutScriptTagsAndComments = br.toString().replaceAll("(?s)(<script.*?</script>)", "").replaceAll("(?s)(<!--.*?-->)", "");
        final String ahref = "<a[^<]*href\\s*=\\s*\"[^\"]*";
        final boolean logoutOkay = new Regex(htmlWithoutScriptTagsAndComments, ahref + "(&|\\?)op=logout").matches() || new Regex(htmlWithoutScriptTagsAndComments, ahref + "/(user_)?logout\"").matches();
        // unsafe, not every site does redirect
        final boolean loginURLFailed = br.getURL().contains("op=") && br.getURL().contains("op=login");
        /*
         * 2019-11-11: Set myAccountOkay to true if there is currently a redirect which means in this situation we rely on our cookie ONLY.
         * This may be the case if a user has direct downloads enabled. We access downloadurl --> Redirect happens --> We check for login
         */
        final boolean isRedirect = br.getRedirectLocation() != null;
        final boolean myAccountOkay = (new Regex(htmlWithoutScriptTagsAndComments, ahref + "(&|\\?)op=my_account").matches() || new Regex(htmlWithoutScriptTagsAndComments, ahref + "/my(-|_)account\"").matches() || isRedirect);
        logger.info("login_xfss_CookieOkay:" + login_xfss_CookieOkay);
        logger.info("logoutOkay:" + logoutOkay);
        logger.info("myAccountOkay:" + myAccountOkay);
        logger.info("loginURLFailed:" + loginURLFailed);
        final boolean ret = (login_xfss_CookieOkay) && ((logoutOkay || myAccountOkay) && !loginURLFailed);
        logger.info("loggedin:" + ret);
        return ret;
    }

    @Override
    protected AccountInfo fetchAccountInfoWebsite(final Account account) throws Exception {
        final AccountInfo ai = super.fetchAccountInfoWebsite(account);
        /*
         * 2020-05-05: Accounts created e.g. with premium balance of other accounts will be fine to login but if they do not yet contain an
         * e-mail address, they cannot be used for downloading and no matter which URL the user accesses (apart from API), the website will
         * redirect him to the account overview page with a message that tells him to add his e-mail address.
         */
        /*
         * 2020-05-06: Template also has handling for this but will not detect it until download-start which is why we will keep it in here
         * too.
         */
        this.getPage("/?op=my_reports");
        if (new Regex(getCorrectBR(br), "(?i)>\\s*?Please enter your e-mail").patternFind()) {
            final String accountErrorMsg;
            if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                accountErrorMsg = String.format("Ergänze deine E-Mail Adresse unter %s/?op=my_account um diesen Account verwenden zu können!", this.getHost());
            } else {
                accountErrorMsg = String.format("Go to %s/?op=my_account and enter your e-mail in order to be able to use this account!", this.getHost());
            }
            throw new AccountUnavailableException(accountErrorMsg, 10 * 60 * 1000l);
        }
        return ai;
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
        super.resetDownloadlink(link);
        if (link == null) {
            return;
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* 2019-11-11: Reset final downloadurls in dev mode. */
            link.removeProperty("freelink");
            link.removeProperty("freelink2");
            link.removeProperty("premlink");
        }
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        /* 2020-01-17: Special */
        if (br.containsHTML(">\\s*This file was banned by copyright")) {
            /* "<strong>Oops!</strong> This file was banned by copyright owner's report" */
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }

    @Override
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account, final boolean checkAll) throws NumberFormatException, PluginException {
        /* 2020-01-20: Special */
        if (new Regex(html, "(?i)>\\s*This server is in maintenance mode").patternFind()) {
            /* <strong>Oops!</strong> This server is in maintenance mode. Refresh this page in some minutes. */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "This server is in maintenance mode", 15 * 60 * 1000l);
        } else if (br.getHttpConnection().getResponseCode() == 500) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 500", 1 * 60 * 1000l);
        }
        /* Now execute template handling */
        super.checkErrors(br, html, link, account, checkAll);
    }

    @Override
    protected boolean supports_availablecheck_filename_abuse() {
        /* 2020-04-20: Not supported anymore */
        return false;
    }

    @Override
    protected boolean supports_availablecheck_alt() {
        /* 2020-04-20: Not supported anymore */
        return false;
    }

    @Override
    protected boolean supportsAPIMassLinkcheck() {
        return looksLikeValidAPIKey(this.getAPIKey());
    }

    @Override
    protected boolean supportsAPISingleLinkcheck() {
        return looksLikeValidAPIKey(this.getAPIKey());
    }

    // @Override
    // public String regexFilenameAbuse(final Browser br) {
    // String filename = br.getRegex("label>Filename</label>\\s*<input[^>]*value=\"([^<>\"]+)\"").getMatch(0);
    // if (StringUtils.isEmpty(filename)) {
    // /* Fallback to template */
    // filename = super.regexFilenameAbuse(br);
    // }
    // return filename;
    // }
    @Override
    public Class<? extends XFSConfigDdownloadCom> getConfigInterface() {
        return XFSConfigDdownloadCom.class;
    }

    @Override
    protected boolean supportsShortURLs() {
        return true;
    }
}