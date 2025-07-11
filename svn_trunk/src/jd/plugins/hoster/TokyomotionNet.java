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

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.RandomUserAgent;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 51149 $", interfaceVersion = 3, names = {}, urls = {})
public class TokyomotionNet extends PluginForHost {
    public TokyomotionNet(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("http://www.tokyomotion.net/signup");
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    @Override
    public String getAGBLink() {
        return "http://www.tokyomotion.net/static/terms";
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "tokyomotion.net" });
        ret.add(new String[] { "osakamotion.net" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:video/\\d+(?:/[^/]+)?|embed/[a-f0-9]{20})");
        }
        return ret.toArray(new String[0]);
    }

    /* Connection stuff */
    private final boolean        RESUME                                = true;
    private final int            MAXCHUNKS                             = 0;
    private final int            MAXDOWNLOADS                          = 20;
    private String               dllink                                = null;
    private boolean              server_issues                         = false;
    /* 2017-11-21: Deactivated this as cookies can get invalid at any time. */
    private static final boolean TRUST_YOUNG_COOKIES_WITHOUT_ANY_CHECK = false;
    public static final long     trust_cookie_age                      = 300000l;
    private static final String  TYPE_NORMAL                           = "https?://[^/]+/video/(\\d+)(?:/([^/]+))?";
    private static final String  TYPE_NORMAL_WITH_TITLE                = "https?://[^/]+/video/(\\d+)/([^/]+)";
    private static final String  TYPE_EMBED                            = "https?://[^/]+/embed/([a-f0-9]{20})";

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return this.getHost() + "://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        if (link.getPluginPatternMatcher() == null) {
            return null;
        } else if (link.getPluginPatternMatcher().matches(TYPE_NORMAL)) {
            return new Regex(link.getPluginPatternMatcher(), TYPE_NORMAL).getMatch(0);
        } else {
            /* TYPE_EMBED */
            return new Regex(link.getPluginPatternMatcher(), TYPE_EMBED).getMatch(0);
        }
    }

    private String getWeakFileTitle(final DownloadLink link) {
        if (link.getPluginPatternMatcher() == null) {
            return null;
        } else if (link.getPluginPatternMatcher().matches(TYPE_NORMAL_WITH_TITLE)) {
            return new Regex(link.getPluginPatternMatcher(), TYPE_NORMAL_WITH_TITLE).getMatch(1).replace("-", " ");
        } else if (link.getPluginPatternMatcher().matches(TYPE_NORMAL)) {
            return new Regex(link.getPluginPatternMatcher(), TYPE_NORMAL).getMatch(0);
        } else {
            /* TYPE_EMBED */
            return new Regex(link.getPluginPatternMatcher(), TYPE_EMBED).getMatch(0);
        }
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser ret = super.createNewBrowserInstance();
        // blocked default UA from Request Class, firefoxRevision = "76.0"
        /*
         * <html><body><h1>403 Forbidden</h1> Request forbidden by administrative rules. </body></html>
         */
        ret.getHeaders().put("User-Agent", RandomUserAgent.generateFF());
        return ret;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        if (!link.isNameSet()) {
            link.setName(getWeakFileTitle(link) + ".mp4");
        }
        dllink = null;
        server_issues = false;
        this.setBrowserExclusive();
        final Account aa = AccountController.getInstance().getValidAccount(this);
        if (aa != null) {
            login(aa);
        }
        br.setFollowRedirects(true);
        br.getPage(link.getPluginPatternMatcher().replaceFirst("http://", "https://"));
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("(?i)>\\s*This video is not available on this platform")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (isPrivateContent(br)) {
            return AvailableStatus.TRUE;
        }
        String filename;
        if (br.getURL().matches(TYPE_EMBED)) {
            filename = br.getRegex("shareTitle\\s*:\\s*\"([^\"]+)\"").getMatch(0);
        } else {
            filename = br.getRegex("property=\"og:title\" content=\"([^<>\"]+)\"").getMatch(0);
        }
        if (filename != null) {
            filename += ".mp4";
            link.setFinalFileName(filename);
        }
        final String[] directurls = br.getRegex("<source src=\"(https?://[^<>\"]*?)\"[^>]*?type=(?:\"|\\')video/(?:mp4|flv)(?:\"|\\')").getColumn(0);
        if (directurls != null && directurls.length > 0) {
            for (final String directurl : directurls) {
                this.dllink = directurl;
                if (directurl.contains("/hd/")) {
                    /* Prefer HD */
                    break;
                }
            }
        }
        if (!StringUtils.isEmpty(dllink)) {
            dllink = Encoding.htmlDecode(dllink);
            URLConnectionAdapter con = null;
            try {
                final Browser brc = br.cloneBrowser();
                brc.setFollowRedirects(true);
                // X-Mod-H264-Streaming
                brc.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING, "identity;q=1, *;q=0");
                brc.getHeaders().put(OPEN_RANGE_REQUEST);
                con = brc.openHeadConnection(dllink);
                if (this.looksLikeDownloadableContent(con)) {
                    if (con.getCompleteContentLength() > 0) {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                    /*
                     * Special: First URL is only accessible once but it redirects to the final URL which we can access multiple times which
                     * is why we need to get that! 2017-11-14: Seems as if this was a serverside issue - it does not happen anymore!
                     */
                    this.dllink = con.getURL().toString();
                } else {
                    brc.followConnection(true);
                    server_issues = true;
                }
            } finally {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
        }
        return AvailableStatus.TRUE;
    }

    private boolean isPrivateContent(final Browser br) {
        return br.containsHTML("(?i)>\\s*This is a private video");
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        doFree(link);
    }

    private void doFree(final DownloadLink link) throws Exception, PluginException {
        if (server_issues) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error", 10 * 60 * 1000l);
        } else if (StringUtils.isEmpty(dllink)) {
            if (isPrivateContent(br)) {
                throw new AccountRequiredException("Private video");
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING, "identity;q=1, *;q=0");
        link.setProperty(DirectHTTP.PROPERTY_ServerComaptibleForByteRangeRequest, true);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, RESUME, MAXCHUNKS);
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return MAXDOWNLOADS;
    }

    private boolean isLoggedinHTML() {
        return br.containsHTML("/logout\"");
    }

    private void login(final Account account) throws Exception {
        synchronized (account) {
            try {
                br.setFollowRedirects(true);
                br.setCookiesExclusive(true);
                final Cookies cookies = account.loadCookies("");
                if (cookies != null) {
                    this.br.setCookies(this.getHost(), cookies);
                    if (System.currentTimeMillis() - account.getCookiesTimeStamp("") <= trust_cookie_age && TRUST_YOUNG_COOKIES_WITHOUT_ANY_CHECK) {
                        /* We trust these cookies --> Do not check them */
                        return;
                    }
                    br.getPage("https://www." + account.getHoster() + "/");
                    if (isLoggedinHTML()) {
                        /* Save cookies to save new timestamp */
                        account.saveCookies(this.br.getCookies(this.getHost()), "");
                        return;
                    }
                    /* Clear cookies */
                }
                br.postPage("https://www." + account.getHoster() + "/login", "submit_login=&username=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
                if (!isLoggedinHTML()) {
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nUngültiger Benutzername oder ungültiges Passwort!\r\nSchnellhilfe: \r\nDu bist dir sicher, dass dein eingegebener Benutzername und Passwort stimmen?\r\nFalls dein Passwort Sonderzeichen enthält, ändere es und versuche es erneut!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nInvalid username/password!\r\nQuick help:\r\nYou're sure that the username and password you entered are correct?\r\nIf your password contains special characters, change it (remove them) and try again!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    }
                }
                account.saveCookies(this.br.getCookies(this.getHost()), "");
            } catch (final PluginException e) {
                account.clearCookies("");
                throw e;
            }
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account);
        ai.setUnlimitedTraffic();
        account.setType(AccountType.FREE);
        account.setMaxSimultanDownloads(MAXDOWNLOADS);
        account.setConcurrentUsePossible(true);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link);
        /* No need to login as we're already logged in in availablecheck */
        doFree(link);
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return MAXDOWNLOADS;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}