//    jDownloader - Downloadmanager
//    Copyright (C) 2013  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

import org.appwork.utils.Application;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.AccountFilter;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 51463 $", interfaceVersion = 2, names = {}, urls = {})
public class FileFactory extends PluginForHost {
    private String dllink = null;

    public FileFactory(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + this.getHost() + "/pricing");
    }

    @Override
    public String getAGBLink() {
        return "https://www." + this.getHost() + "/legal/terms";
    }

    private String getContentURL(final DownloadLink link) throws PluginException {
        return "https://www." + getHost() + "/file/" + this.getFUID(link);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (this.isPremiumAccount(account)) {
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return false;
        }
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if (this.isPremiumAccount(account)) {
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 1;
        }
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFUID(link);
        if (fid != null) {
            return getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "filefactory.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public FEATURE[] getFeatures() {
        return new FEATURE[] { LazyPlugin.FEATURE.FAVICON, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public Object getFavIcon(String host) throws IOException {
        return "https://assets-global.website-files.com/65991d8455ab821f56e541e6/659ca0dba9444b12d9112616_favicon32x32.jpg";
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    private static final Pattern PATTERN_FILE = Pattern.compile("/(?:file|stream)/([a-z0-9]+)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_FILE.pattern());
        }
        return ret.toArray(new String[0]);
    }

    public void checkErrorsWebsite(final DownloadLink link, final Browser br) throws PluginException {
        // TODO: Add more error handling
        final String error_code = br.getRegex("\"errorCode\":\"([^\"]+)\"").getMatch(0);
        if (error_code == null) {
            return;
        }
        if (error_code.equalsIgnoreCase("FILE_NOT_FOUND")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            logger.info("Unknown error happened: " + error_code);
            throw new PluginException(LinkStatus.ERROR_FATAL, error_code);
        }
    }

    @Override
    public boolean checkLinks(final DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return false;
        }
        final Browser br = this.createNewBrowserInstance();
        // logic to grab account cookie to do fast linkchecking vs one at a time.
        boolean loggedIn = false;
        final List<Account> filteredAccounts = AccountController.getInstance().listAccounts(new AccountFilter(this.getHost()).setEnabled(true).setValid(true));
        for (Account n : filteredAccounts) {
            try {
                loginWebsite(n, false, br);
                loggedIn = true;
                break;
            } catch (Exception e) {
                logger.log(e);
            }
        }
        if (loggedIn) {
            try {
                final StringBuilder sb = new StringBuilder();
                final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>();
                int index = 0;
                while (true) {
                    links.clear();
                    while (true) {
                        if (index == urls.length || links.size() == 100) {
                            break;
                        } else {
                            links.add(urls[index]);
                            index++;
                        }
                    }
                    sb.delete(0, sb.capacity());
                    sb.append("links=");
                    for (final DownloadLink link : links) {
                        sb.append(Encoding.urlEncode(this.getContentURL(link)));
                        sb.append("%0D%0A");
                    }
                    // lets remove last "%0D%0A"
                    sb.replace(sb.length() - 6, sb.length(), "");
                    sb.append("&Submit=Check+Links");
                    br.setFollowRedirects(false);
                    br.setCurrentURL("https://www." + this.getHost() + "/account/tools/link-checker.php");
                    br.postPage("https://www." + this.getHost() + "/account/tools/link-checker.php", sb.toString());
                    final String trElements[] = br.getRegex("<tr>\\s*(.*?)\\s*</tr>").getColumn(0);
                    for (final DownloadLink link : links) {
                        String name;
                        if (link.isNameSet()) {
                            name = link.getName();
                        } else {
                            /* Obtain filename from URL */
                            name = new Regex(link.getPluginPatternMatcher(), "(?i)https?://[^/]+/(.+)").getMatch(0);
                        }
                        try {
                            if (br.getRedirectLocation() != null && (br.getRedirectLocation().endsWith("/member/setpwd.php") || br.getRedirectLocation().endsWith("/member/setdob.php"))) {
                                // password needs changing or dob needs setting.
                                link.setAvailable(true);
                                continue;
                            }
                            final String fileID = getFUID(link);
                            /* Search html snippet belonging to the link we are working on to determine online status. */
                            String filehtml = null;
                            for (final String trElement : trElements) {
                                if (new Regex(trElement, ">\\s*(" + Pattern.quote(fileID) + ".*?</small>\\s*</span>)").getMatch(0) != null) {
                                    filehtml = trElement;
                                    break;
                                }
                            }
                            if (filehtml == null) {
                                link.setAvailable(false);
                            } else {
                                final String size = new Regex(filehtml, "Size:\\s*([\\d\\.]+\\s*(KB|MB|GB|TB))").getMatch(0);
                                if (size != null) {
                                    link.setDownloadSize(SizeFormatter.getSize(size));
                                }
                                final String filenameFromHTML = new Regex(filehtml, "Filename:\\s*(.*?)\\s*<br>").getMatch(0);
                                if (filenameFromHTML != null) {
                                    name = Encoding.htmlDecode(filenameFromHTML).trim();
                                }
                                if (filehtml.matches("(?s).*>\\s*(Gültig|Valid)\\s*</abbr>.*")) {
                                    link.setAvailable(true);
                                } else {
                                    link.setAvailable(false);
                                }
                            }
                        } finally {
                            if (name != null) {
                                link.setName(name.trim());
                            }
                        }
                    }
                    if (index == urls.length) {
                        break;
                    }
                }
            } catch (final Exception e) {
                logger.log(e);
                return false;
            }
        } else {
            // no account present or disabled account -> Fallback into requestFileInformation
            for (final DownloadLink link : urls) {
                try {
                    link.setAvailableStatus(requestFileInformationWebsite(null, link));
                } catch (final PluginException e) {
                    logger.log(e);
                    if (e.getLinkStatus() == LinkStatus.ERROR_FILE_NOT_FOUND) {
                        link.setAvailable(false);
                    } else {
                        return false;
                    }
                } catch (Throwable e) {
                    logger.log(e);
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        loginWebsite(account, true, br);
        if (br.getURL() == null || !br.getURL().endsWith("/account/")) {
            br.getPage("https://www." + this.getHost() + "/account/");
        }
        // <li class="tooltipster" title="Premium valid until: <strong>30th Jan, 2014</strong>">
        boolean isPremiumLifetime = false;
        boolean isPremium = false;
        final String accountTypeStr = br.getRegex("member_type\"?\\s*:\\s*\"([^\"]+)").getMatch(0);
        if (accountTypeStr != null) {
            if (accountTypeStr.equalsIgnoreCase("lifetime")) {
                isPremiumLifetime = true;
            } else if (accountTypeStr.equalsIgnoreCase("premium")) {
                // TODO: 2021-01-12: Not sure about this as I didn't have the html code for checking yet!
                isPremium = true;
            }
            /**
             * Other possible values: </br>
             * "expired" -> Free Account
             */
        }
        if (!isPremium && !isPremiumLifetime) {
            /* Fallback/Old handling */
            isPremiumLifetime = br.containsHTML("<strong>\\s*(Lebenszeit|Lifetime|Livstid|Levenslang|À vie|生涯|Vitalício|De por vida)\\s*</strong>") || br.containsHTML(">\\s*Lifetime Member\\s*<");
            isPremium = br.containsHTML("(>|\")\\s*Premium valid until\\s*(<|:)");
        }
        long expireTimestamp = 0;
        final String expireTimestampStr = br.getRegex("premium_ends\"?\\s*:\\s*\"?(\\d+)").getMatch(0);
        if (expireTimestampStr != null) {
            expireTimestamp = Long.parseLong(expireTimestampStr) * 1000;
        } else {
            /* Fallback/Old handling */
            final String expireDateStr = br.getRegex("(?i)Premium valid until\\s*:\\s*<strong>(.*?)</strong>").getMatch(0);
            if (expireDateStr != null) {
                expireTimestamp = TimeFormatter.getMilliSeconds(expireDateStr.replaceFirst("(st|nd|rd|th)", ""), "dd MMM, yyyy", Locale.ENGLISH);
            }
        }
        if (isPremium || isPremiumLifetime || expireTimestamp > System.currentTimeMillis()) {
            account.setType(AccountType.PREMIUM);
            if (isPremiumLifetime) {
                account.setType(AccountType.LIFETIME);
            } else {
                if (expireTimestamp > System.currentTimeMillis()) {
                    ai.setValidUntil(expireTimestamp);
                }
                final String space = br.getRegex("<strong>([0-9\\.]+ ?(KB|MB|GB|TB))\\s*</strong>\\s*Free Space").getMatch(0);
                if (space != null) {
                    ai.setUsedSpace(space);
                }
                final String traffic = br.getRegex("donoyet(.*?)xyz").getMatch(0);
                if (traffic != null) {
                    // OLD SHIT
                    String loaded = br.getRegex("(?i)You have used (.*?) out").getMatch(0);
                    String max = br.getRegex("limit of (.*?)\\. ").getMatch(0);
                    if (max != null && loaded != null) {
                        // you don't need to strip characters or reorder its structure. The source is fine!
                        ai.setTrafficMax(SizeFormatter.getSize(max));
                        ai.setTrafficLeft(ai.getTrafficMax() - SizeFormatter.getSize(loaded));
                    } else {
                        max = br.getRegex("(?i)You can now download up to (.*?) in").getMatch(0);
                        if (max != null) {
                            ai.setTrafficLeft(SizeFormatter.getSize(max));
                        } else {
                            ai.setUnlimitedTraffic();
                        }
                    }
                } else {
                    ai.setUnlimitedTraffic();
                }
            }
        } else {
            ai.setUnlimitedTraffic();
            account.setType(AccountType.FREE);
        }
        final String createTimestampStr = br.getRegex("created_at\"?\\s*:\\s*\"?(\\d+)").getMatch(0);
        if (createTimestampStr != null) {
            ai.setCreateTime(Long.parseLong(createTimestampStr) * 1000);
        }
        return ai;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public int getTimegapBetweenConnections() {
        return 200;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        if (checkShowFreeDialog(getHost())) {
            showFreeDialog(getHost());
        }
        handleFreeWebsite(link, null);
    }

    public void handleFreeWebsite(final DownloadLink link, final Account account) throws Exception {
        requestFileInformationWebsite(account, link);
        if (true) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        // link.setProperty(directlinkproperty, dl.getConnection().getURL().toExternalForm());
        dl.startDownload();
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        if (true) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Fallback to website-mode (old code, deprecated) */
        if (this.isPremiumAccount(account)) {
            loginWebsite(account, false, br);
            final String contenturl = this.getContentURL(link);
            // NOTE: no premium, pre download password handling yet...
            br.setFollowRedirects(false);
            br.getPage(contenturl);
            String finallink = br.getRedirectLocation();
            while (finallink != null && canHandle(finallink)) {
                // follow http->https redirect
                br.getPage(finallink);
                finallink = br.getRedirectLocation();
            }
            if (finallink == null) {
                // No directlink
                finallink = br.getRegex("\"(https?://[a-z0-9]+\\.filefactory\\.com/get/[^<>\"]+)\"").getMatch(0);
                if (finallink == null) {
                    this.checkErrorsWebsite(link, br);
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            br.setFollowRedirects(true);
            dl = new jd.plugins.BrowserAdapter().openDownload(br, link, finallink, this.isResumeable(link, account), this.getMaxChunks(link, account));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                checkErrorsWebsite(link, br);
                String redirecturl = br.getRegex("\"(https?://[a-z0-9]+\\.filefactory\\.com/get/[^<>\"]+)\"").getMatch(0);
                if (redirecturl == null) {
                    redirecturl = br.getRegex(Pattern.compile("10px 0;\">.*<a href=\"(.*?)\">Download with FileFactory Premium", Pattern.DOTALL)).getMatch(0);
                }
                if (redirecturl == null) {
                    redirecturl = br.getRegex("subPremium.*?ready.*?<a href=\"(.*?)\"").getMatch(0);
                    if (redirecturl == null) {
                        redirecturl = br.getRegex("downloadLink.*?href\\s*=\\s*\"(.*?)\"").getMatch(0);
                        if (redirecturl == null) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        }
                    }
                }
                logger.finer("Indirect download");
                dl = new jd.plugins.BrowserAdapter().openDownload(br, link, redirecturl, this.isResumeable(link, account), this.getMaxChunks(link, account));
                if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                    br.followConnection(true);
                    checkErrorsWebsite(link, br);
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            } else {
                logger.finer("DIRECT download");
            }
            dl.startDownload();
        } else {
            /* Free account handling */
            if (checkShowFreeDialog(getHost())) {
                showFreeDialog(getHost());
            }
            this.handleFreeWebsite(link, account);
        }
    }

    /*
     * This is for filefactory.com/trafficshare/ sharing links or I guess what we call public premium links.
     */
    public void handleTrafficShare(final DownloadLink link, final Account account) throws Exception {
        logger.info("Traffic sharing link - Free Premium Download");
        String finalLink = br.getRegex("<a href=\"(https?://\\w+\\.filefactory\\.com/get/t/[^\"]+)\"[^\r\n]*Download with FileFactory TrafficShare").getMatch(0);
        if (finalLink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (Application.getJavaVersion() < Application.JAVA17) {
            finalLink = finalLink.replaceFirst("(?i)https", "http");
        }
        dl = new jd.plugins.BrowserAdapter().openDownload(br, link, finalLink, true, 0);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            checkErrorsWebsite(link, br);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl.startDownload();
    }

    @Deprecated
    private void loginWebsite(final Account account, final boolean force, final Browser br) throws Exception {
        synchronized (account) {
            setBrowserExclusive();
            br.getHeaders().put("Accept-Encoding", "gzip");
            br.setFollowRedirects(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                br.setCookies(this.getHost(), cookies);
                if (!force) {
                    /* Do not verify cookies */
                    return;
                } else {
                    br.getPage("https://www." + this.getHost() + "/");
                    if (isLoggedinWebsite(br)) {
                        logger.info("Cookie login successful");
                        account.saveCookies(br.getCookies(br.getHost()), "");
                        return;
                    } else {
                        logger.info("Cookie login failed");
                        br.clearCookies(null);
                    }
                }
            }
            logger.info("Performing full login");
            br.getPage("https://www." + this.getHost() + "/member/signin.php");
            br.postPage("/member/signin.php", "loginEmail=" + Encoding.urlEncode(account.getUser()) + "&loginPassword=" + Encoding.urlEncode(account.getPass()) + "&Submit=Sign+In");
            if (!this.isLoggedinWebsite(br)) {
                this.checkErrorsWebsite(null, br);
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    /** Checks html code and or cookies to see if we're logged in. */
    private boolean isLoggedinWebsite(final Browser br) {
        if (br.getCookie(br.getHost(), "auth", Cookies.NOTDELETEDPATTERN) != null) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        correctDownloadLink(link);
        return requestFileInformationWebsite(null, link);
    }

    private AvailableStatus requestFileInformationWebsite(final Account account, final DownloadLink link) throws Exception {
        dllink = null;
        setBrowserExclusive();
        br.setFollowRedirects(true);
        for (int i = 0; i < 4; i++) {
            try {
                Thread.sleep(200);
            } catch (final Exception e) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            URLConnectionAdapter con = null;
            try {
                final String contenturl = this.getContentURL(link);
                con = br.openGetConnection(contenturl);
                if (this.looksLikeDownloadableContent(con)) {
                    link.setFinalFileName(Plugin.getFileNameFromConnection(con));
                    if (con.getCompleteContentLength() > 0) {
                        if (con.isContentDecoded()) {
                            link.setDownloadSize(con.getCompleteContentLength());
                        } else {
                            link.setVerifiedFileSize(con.getCompleteContentLength());
                        }
                    }
                    dllink = con.getURL().toExternalForm();
                    link.setAvailable(true);
                    return AvailableStatus.TRUE;
                } else {
                    br.followConnection();
                }
                break;
            } catch (final Exception e) {
                logger.log(e);
                if (i == 3) {
                    throw e;
                }
            } finally {
                try {
                    if (con != null) {
                        con.disconnect();
                    }
                } catch (final Throwable e) {
                }
            }
        }
        if (isPasswordProtectedFile(br)) {
            link.setPasswordProtected(true);
        } else {
            link.setPasswordProtected(false);
        }
        /* Correct json in html code to make br.getRegex calls down below work. */
        final String unescaped = PluginJSonUtils.unescape(br.getRequest().getHtmlCode());
        br.getRequest().setHtmlCode(unescaped);
        this.checkErrorsWebsite(link, br);
        String filename = br.getRegex("\"disp_filename\":\"([^\"]+)").getMatch(0);
        String filesizeBytes = br.getRegex("\"size\":\"\\$n(\\d+)").getMatch(0);
        if (filename != null) {
            link.setName(Encoding.htmlDecode(filename).trim());
        }
        if (filesizeBytes != null) {
            // TODO: Check if this is the exact filesize
            // link.setVerifiedFileSize(Long.parseLong(filesizeBytes));
            link.setDownloadSize(Long.parseLong(filesizeBytes));
        }
        return AvailableStatus.TRUE;
    }

    private boolean isPasswordProtectedFile(final Browser br) {
        if (br.containsHTML("\"requiresPassword\":false")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account account) {
        return false;
    }

    @Override
    public boolean hasAutoCaptcha() {
        return false;
    }

    private String getFUID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), PATTERN_FILE).getMatch(0);
    }

    private boolean isPremiumAccount(final Account account) {
        if (account == null) {
            return false;
        } else if (account.getType() == AccountType.PREMIUM || account.getType() == AccountType.LIFETIME) {
            return true;
        } else {
            return false;
        }
    }
}