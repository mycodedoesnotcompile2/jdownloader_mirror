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

import java.util.HashMap;
import java.util.Map;

import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.Property;
import jd.http.Browser;
import jd.http.Cookie;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.JDHash;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 50681 $", interfaceVersion = 3, names = { "slideshare.net" }, urls = { "" })
public class SlideShareNet extends PluginForHost {
    public SlideShareNet(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/business/premium/plans?cmp_src=main_nav");
    }

    private static final String APIKEY       = "ZXdvclNoQm0=";
    private static final String SHAREDSECRET = "UjZIRW9VVEQ=";

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/terms";
    }

    private static final String NOTDOWNLOADABLE = "class=\"sprite iconNoDownload j\\-tooltip\"";
    private String              dllink          = null;
    private boolean             isVideo         = false;
    private boolean             server_issues   = false;

    public static Browser prepBR(final Browser br) {
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(new int[] { 410, 500 });
        return br;
    }

    public static boolean isOffline(final Browser br) {
        if (br.getHttpConnection().getResponseCode() == 404) {
            return true;
        } else if (br.getHttpConnection().getResponseCode() == 410) {
            return true;
        } else if (br.containsHTML(">\\s*Sorry\\! We could not find what you were looking for|>Don\\'t worry, we will help you get to the right place|<title>404 error\\. Page Not Found\\.</title>") || br.containsHTML("(?i)>\\s*Uploaded Content Removed\\s*<")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, false);
    }

    // TODO: Implement API: http://www.slideshare.net/developers/documentation
    @SuppressWarnings("deprecation")
    public AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws Exception {
        dllink = null;
        isVideo = false;
        server_issues = false;
        this.setBrowserExclusive();
        prepBR(this.br);
        br.getPage(link.getPluginPatternMatcher());
        br.followConnection();
        if (isOffline(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String json = this.br.getRegex("slideshare_object,\\s*?(\\{.*?)\\);").getMatch(0);
        if (json == null && !this.br.containsHTML("id=\"slideview\\-container\"")) {
            /* 2016-11-23: No slideshow-class in html --> Probably offline content */
            if (isDownload) {
                throw new AccountRequiredException();
            } else {
                /* Item is online but can only be downloaded with account. */
                return AvailableStatus.TRUE;
            }
        }
        Map<String, Object> entries = JavaScriptEngineFactory.jsonToJavaMap(json);
        entries = (Map<String, Object>) entries.get("slideshow");
        final String url_filename = new Regex(link.getDownloadURL(), "https?://[^/]+/([a-z0-9\\-_]+/[a-z0-9\\-_]+)").getMatch(0).replace("/", " - ");
        String filename = (String) entries.get("title");
        final String type = (String) entries.get("type");
        final String ext;
        if (StringUtils.isEmpty(filename)) {
            /* Fallback */
            filename = url_filename;
        }
        filename = Encoding.htmlDecode(filename).trim();
        if ("video".equalsIgnoreCase(type)) {
            /* Video */
            // entries = (Map<String, Object>) entries.get("jsplayer");
            ext = ".mp4";
            isVideo = true;
            /* Easier to RegEx as it can be found in multiple places in the json. */
            final String embedCode = this.br.getRegex("embed_code/key/([A-Za-z0-9]+)").getMatch(0);
            if (embedCode != null && !embedCode.equals("")) {
                this.br.getPage("/slideshow/embed_code/key/" + embedCode);
                if (this.br.getHttpConnection().getResponseCode() == 500) {
                    /* HTTP/1.1 500 INKApi Error */
                    server_issues = true;
                } else {
                    /* E.g. https://vcdn.slidesharecdn.com/160818dataanalyticsandsocialmedia-160907135456-lva1-app6892-video-SD.mp4 */
                    /*
                     * 2016-11-23: videourl can also be built via information in json (in multiple places) but it (currently) makes more
                     * sense to grad it from the "embed_code" html code."
                     */
                    dllink = this.br.getRegex("(//[^<>\"]+\\.mp4)").getMatch(0);
                    if (dllink != null) {
                        /* Fix protocol/beginning of the url. */
                        dllink = "https:" + dllink;
                        /*
                         * Try HD first as most videos are available in HD ... and we can only try as there is no indicator on which
                         * resolution is available - browser (http version) always uses SD.
                         */
                        dllink = videourlSDtoHD(this.dllink);
                        URLConnectionAdapter con = null;
                        try {
                            con = br.openHeadConnection(dllink);
                            if (con.getResponseCode() == 403) {
                                /* Probably HD not available --> Try SD */
                                dllink = videourlHDtoSD(this.dllink);
                                con = br.openHeadConnection(dllink);
                            }
                            if (this.looksLikeDownloadableContent(con)) {
                                if (con.getCompleteContentLength() > 0) {
                                    link.setVerifiedFileSize(con.getCompleteContentLength());
                                }
                                link.setProperty("directlink", dllink);
                            } else {
                                server_issues = true;
                            }
                        } finally {
                            try {
                                con.disconnect();
                            } catch (final Throwable e) {
                            }
                        }
                    }
                }
            }
            link.setFinalFileName(filename + ext);
        } else {
            /* Document: pptx, ppt or pdf usually. */
            /* 2016-11-23: We'll simply assume that most content is .pdf. */
            final SlideshareNetConfigInterface cfg = PluginJsonConfig.get(jd.plugins.hoster.SlideShareNet.SlideshareNetConfigInterface.class);
            ext = ".pdf";
            if (cfg.getPreferServerFilenames()) {
                /* Final filename will be set on downloadstart. */
                link.setName(filename + ext);
            } else {
                /* Set final filename here because user wants it this way. */
                link.setFinalFileName(url_filename + ext);
            }
        }
        return AvailableStatus.TRUE;
    }

    private String videourlSDtoHD(final String inputVideoUrl) {
        return inputVideoUrl.replace("SD.mp4", "HD.mp4");
    }

    private String videourlHDtoSD(final String inputVideoUrl) {
        return inputVideoUrl.replace("HD.mp4", "SD.mp4");
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link, true);
        // TODO: Check if anything is downloadable without account (easy to check via API)
        handleErrors();
        if (!this.isVideo) {
            throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_ONLY);
        }
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, this.dllink, true, getMaxChunks());
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            logger.warning("The final dllink seems not to be a file!");
            handleServerErrors(null);
        }
        dl.startDownload();
    }

    private int getMaxChunks() {
        if (this.isVideo) {
            return 0;
        } else {
            return 1;
        }
    }

    private static final String MAINPAGE = "http://slideshare.net";

    @SuppressWarnings("unchecked")
    public void login(final Browser br, final Account account, final boolean force) throws Exception {
        synchronized (account) {
            try {
                // Load cookies
                br.setCookiesExclusive(true);
                final Object ret = account.getProperty("cookies", null);
                boolean acmatch = Encoding.urlEncode(account.getUser()).equals(account.getStringProperty("name", Encoding.urlEncode(account.getUser())));
                if (acmatch) {
                    acmatch = Encoding.urlEncode(account.getPass()).equals(account.getStringProperty("pass", Encoding.urlEncode(account.getPass())));
                }
                if (acmatch && ret != null && ret instanceof Map<?, ?> && !force) {
                    final Map<String, String> cookies = (Map<String, String>) ret;
                    if (account.isValid()) {
                        for (final Map.Entry<String, String> cookieEntry : cookies.entrySet()) {
                            final String key = cookieEntry.getKey();
                            final String value = cookieEntry.getValue();
                            br.setCookie(MAINPAGE, key, value);
                        }
                        return;
                    }
                }
                br.setFollowRedirects(false);
                br.getHeaders().put("Accept-Language", "en-us;q=0.7,en;q=0.3");
                br.getPage("https://www.slideshare.net/login");
                final String token = br.getRegex("name=\"authenticity_token\" type=\"hidden\" value=\"([^<>\"]*?)\"").getMatch(0);
                final String lang = System.getProperty("user.language");
                if (token == null) {
                    if ("de".equalsIgnoreCase(lang)) {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nPlugin defekt, bitte den JDownloader Support kontaktieren!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nPlugin broken, please contact the JDownloader Support!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    }
                }
                br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
                br.postPage("https://www.slideshare.net/login", "login_source=login.page&remember=1&source_from=&utf8=%E2%9C%93&authenticity_token=" + Encoding.urlEncode(token) + "&user_login=" + Encoding.urlEncode(account.getUser()) + "&user_password=" + Encoding.urlEncode(account.getPass()));
                if (!br.containsHTML("\"success\":true") || br.getCookie(MAINPAGE, "logged_in") == null) {
                    if ("de".equalsIgnoreCase(lang)) {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nUngültiger Benutzername oder ungültiges Passwort!\r\nSchnellhilfe: \r\nDu bist dir sicher, dass dein eingegebener Benutzername und Passwort stimmen?\r\nFalls dein Passwort Sonderzeichen enthält, ändere es und versuche es erneut!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "\r\nInvalid username/password!\r\nQuick help:\r\nYou're sure that the username and password you entered are correct?\r\nIf your password contains special characters, change it (remove them) and try again!", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    }
                }
                final String is_pro = br.getCookie(MAINPAGE, "is_pro");
                if (is_pro != null && !is_pro.equals("false")) {
                    /* Do not accept unsupported accounts! */
                    logger.info("Premium accounts are not (yet) supported, please contact us in our supportforum!");
                    final AccountInfo ai = new AccountInfo();
                    ai.setStatus("Premium accounts are not (yet) supported, please contact us in our supportforum!");
                    account.setAccountInfo(ai);
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, PluginException.VALUE_ID_PREMIUM_DISABLE);
                }
                // Save cookies
                final HashMap<String, String> cookies = new HashMap<String, String>();
                final Cookies add = br.getCookies(MAINPAGE);
                for (final Cookie c : add.getCookies()) {
                    cookies.put(c.getKey(), c.getValue());
                }
                account.setProperty("name", Encoding.urlEncode(account.getUser()));
                account.setProperty("pass", Encoding.urlEncode(account.getPass()));
                account.setProperty("cookies", cookies);
            } catch (final PluginException e) {
                account.setProperty("cookies", Property.NULL);
                throw e;
            }
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(this.br, account, true);
        ai.setUnlimitedTraffic();
        return ai;
    }

    @SuppressWarnings("deprecation")
    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        requestFileInformation(link, true);
        handleErrors();
        if (!this.isVideo) {
            login(this.br, account, false);
            br.setFollowRedirects(false);
            final boolean useAPI = false;
            if (useAPI) {
                // NOTE: This can also be used without username and password to check links but we always have to access the normal link
                // in
                // order to get this stupid slideshow_id
                br.getPage(link.getDownloadURL());
                br.followRedirect();
                final String slideshareID = br.getRegex("\"slideshow_id\":(\\d+)").getMatch(0);
                if (slideshareID == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String timestamp = System.currentTimeMillis() + "";
                // Examplelink: http://www.slideshare.net/webbmedia/webbmedia-group-2013-tech-trends
                final String getLink = "https://www.slideshare.net/api/2/get_slideshow?api_key=" + Encoding.Base64Decode(APIKEY) + "&ts=" + timestamp + "&hash=" + JDHash.getSHA1(Encoding.Base64Decode(SHAREDSECRET) + timestamp) + "&slideshow_id=" + slideshareID;
                br.getPage(getLink);
                dllink = getXML("DownloadUrl");
            } else {
                br.getPage(link.getDownloadURL());
                br.followRedirect();
                if (br.containsHTML(NOTDOWNLOADABLE)) {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "This document is not downloadable");
                }
                br.setFollowRedirects(true);
                br.getPage(link.getDownloadURL() + "/download");
                if (br.containsHTML("You have exhausted your daily limit")) {
                    /*
                     * E.g. html:
                     * "<span>You have exhausted your daily limit of 20 downloads. To download more, please try after 24 hours. Read <a href="
                     * #{help_page_url(:download_limit)}" target="_blank">FAQs</a>"
                     */
                    logger.info("Daily limit reached");
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, "Daily limit reached", PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
                }
                dllink = br.getRegex("class=\"altDownload\">[\t\n\r ]+<a class=\"btn\" href=\"(http[^<>\"]*?)\"").getMatch(0);
                if (dllink == null) {
                    dllink = br.getRegex("\"(https?://[a-z0-9]+\\.amazonaws\\.com/[^<>\"]*?)\"").getMatch(0);
                }
            }
            if (this.dllink != null) {
                this.dllink = Encoding.htmlOnlyDecode(dllink);
            }
        }
        if (dllink == null) {
            logger.warning("Final downloadlink (String is \"dllink\") regex didn't match!");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, this.dllink, true, getMaxChunks());
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            logger.warning("The final dllink seems not to be a file!");
            handleServerErrors(account);
        }
        dl.startDownload();
    }

    private void handleErrors() throws Exception {
        if (this.server_issues) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Unknown server error", 10 * 60 * 1000l);
        } else if (this.isVideo && this.dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    private void handleServerErrors(Account account) throws Exception {
        if (dl.getConnection().getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
        } else if (dl.getConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
        }
        br.followConnection();
        try {
            dl.getConnection().disconnect();
        } catch (final Throwable e) {
        }
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    private String getXML(final String parameter) {
        return br.getRegex("<" + parameter + "( type=\"[^<>\"/]*?\")?>([^<>]*?)</" + parameter + ">").getMatch(1);
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return SlideshareNetConfigInterface.class;
    }

    public static interface SlideshareNetConfigInterface extends PluginConfigInterface {
        @DefaultBooleanValue(true)
        @Order(10)
        boolean getPreferServerFilenames();

        void setPreferServerFilenames(boolean b);
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}