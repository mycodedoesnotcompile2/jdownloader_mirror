//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
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
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.components.config.EpornerComConfig;
import org.jdownloader.plugins.components.config.EpornerComConfig.PreferredStreamQuality;
import org.jdownloader.plugins.components.config.EpornerComConfig.PreferredVideoCodec;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
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
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51262 $", interfaceVersion = 3, names = {}, urls = {})
public class EPornerCom extends PluginForHost {
    public EPornerCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/");
    }

    public static final Pattern PATTERN_VIDEO           = Pattern.compile("/(?:hd\\-porn/|video-)(\\w+)(/([^/]+))?", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_VIDEO_EMBED     = Pattern.compile("/embed/([A-Za-z0-9]+)(/([\\w\\-]+)/?)?", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_VIDEO_URL       = Pattern.compile("/dload/(\\w+)/(\\d+)/(\\d+)-(\\d+)p(-av1)?\\.mp4", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_PHOTO           = Pattern.compile("/photo/([A-Za-z0-9]+)(/([\\w\\-]+)/)?", Pattern.CASE_INSENSITIVE);
    public static final String  PROPERTY_DIRECTURL      = "directurl";
    public static final String  PROPERTY_LAST_DIRECTURL = "directurl";

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(410);
        br.setCookie(this.getHost(), "ageverif_accepted", "T");
        br.setCookie(this.getHost(), "epcolor", "black");
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "eporner.com" });
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
            ret.add("https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_VIDEO.pattern() + "|" + PATTERN_VIDEO_EMBED.pattern() + "|" + PATTERN_VIDEO_URL.pattern() + "|" + PATTERN_PHOTO.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/terms/";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
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
        final Regex videoRegex = new Regex(link.getPluginPatternMatcher(), PATTERN_VIDEO);
        if (videoRegex.patternFind()) {
            return videoRegex.getMatch(0);
        }
        final Regex videoUrlRegex = new Regex(link.getPluginPatternMatcher(), PATTERN_VIDEO_URL);
        if (videoUrlRegex.patternFind()) {
            return videoUrlRegex.getMatch(0);
        }
        return new Regex(link.getPluginPatternMatcher(), PATTERN_PHOTO).getMatch(0);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, null, false);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        final String extDefault;
        final boolean isVideo;
        Regex videoRegex = null;
        String titleByURL = null;
        if ((videoRegex = new Regex(link.getPluginPatternMatcher(), PATTERN_VIDEO)).patternFind()) {
            extDefault = ".mp4";
            isVideo = true;
            titleByURL = videoRegex.getMatch(2);
        } else if ((videoRegex = new Regex(link.getPluginPatternMatcher(), PATTERN_VIDEO_URL)).patternFind()) {
            extDefault = ".mp4";
            isVideo = true;
            titleByURL = null;
        } else if ((videoRegex = new Regex(link.getPluginPatternMatcher(), PATTERN_VIDEO_EMBED)).patternFind()) {
            extDefault = ".mp4";
            isVideo = true;
            titleByURL = videoRegex.getMatch(2);
        } else {
            extDefault = ".jpg";
            isVideo = false;
            titleByURL = new Regex(link.getPluginPatternMatcher(), PATTERN_PHOTO).getMatch(2);
        }
        String fallbackFilename;
        if (titleByURL != null) {
            fallbackFilename = titleByURL.replace("-", " ").trim() + extDefault;
        } else {
            fallbackFilename = this.getFID(link) + extDefault;
        }
        if (!link.isNameSet()) {
            link.setName(fallbackFilename);
        }
        if (account != null) {
            this.login(account, false);
        }
        br.getPage(getPluginContentURL(link));
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (this.br.getHttpConnection().getResponseCode() == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("id=\"deletedfile\"")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.br.getURL().contains(this.getFID(link))) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        boolean isAgeVerificationBlocked = this.isAgeVerificationBlocked(br);
        if (isAgeVerificationBlocked && new Regex(br._getURL().getPath(), PATTERN_VIDEO).patternFind()) {
            logger.info("Trying to avoid age verification");
            /* 2025-07-28: Use video embed page to avoid the need of age verification. */
            br.getPage(br.getURL().replaceFirst("/video-", "/embed/"));
            isAgeVerificationBlocked = this.isAgeVerificationBlocked(br);
        }
        if (isAgeVerificationBlocked) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Age verification required! Change your IP or add a verified eporner account.");
        }
        long filesize = -1;
        String dllink = null;
        long filesizeBestH264 = 0;
        long filesizeSelectedH264 = 0;
        String dllinkBestH264 = null;
        String dllinkSelectedH264 = null;
        long filesizeBestAV1 = 0;
        long filesizeSelectedAV1 = -1;
        String dllinkBestAV1 = null;
        String dllinkSelectedAV1 = null;
        String title = br.getRegex("<title>([^<]+)</title>").getMatch(0);
        String pic[] = null;
        if (isVideo) {
            final String betterTitleByURL = new Regex(br.getURL(), PATTERN_VIDEO).getMatch(2);
            if (betterTitleByURL != null) {
                fallbackFilename = betterTitleByURL.replace("-", " ").trim() + extDefault;
            }
            final String vq = getPreferredStreamQuality(link);
            PreferredVideoCodec codec = getPreferredVideoCodec(link);
            /* Official downloadurls */
            final String[][] dloadinfo = br.getRegex("href=\"(/dload/[^<>\"]+)\"[^>]*>[^<]* MP4 \\((\\d+)p, ([^<>\"\\)]+)\\)</a>").getMatches();
            if (dloadinfo != null && dloadinfo.length != 0) {
                for (final String[] dlinfo : dloadinfo) {
                    final String directurl = dlinfo[0];
                    // final String heightStr = dlinfo[1];
                    final String filesizeStr = dlinfo[2];
                    final long tempsize = SizeFormatter.getSize(filesizeStr);
                    if (StringUtils.containsIgnoreCase(directurl, "av1")) {
                        if (dllinkBestAV1 == null || tempsize > filesizeBestAV1) {
                            filesizeBestAV1 = tempsize;
                            dllinkBestAV1 = directurl;
                        }
                        if (vq != null && directurl.contains(vq)) {
                            dllinkSelectedAV1 = directurl;
                            filesizeSelectedAV1 = tempsize;
                        }
                    } else {
                        if (dllinkBestH264 == null || tempsize > filesizeBestH264) {
                            filesizeBestH264 = tempsize;
                            dllinkBestH264 = directurl;
                        }
                        if (vq != null && directurl.contains(vq)) {
                            dllinkSelectedH264 = directurl;
                            filesizeSelectedH264 = tempsize;
                        }
                    }
                    /* Determine best candidate for fallback / "global best" */
                    // if (dllink == null || tempsize > filesize) {
                    // filesize = tempsize;
                    // dllink = directurl;
                    // }
                }
            }
            if (dllinkSelectedAV1 != null && codec == PreferredVideoCodec.AV1) {
                dllink = dllinkSelectedAV1;
                filesize = filesizeSelectedAV1;
            } else if (dllinkSelectedH264 != null && codec == PreferredVideoCodec.H264) {
                dllink = dllinkSelectedH264;
                filesize = filesizeSelectedH264;
            } else if (dllinkBestH264 != null && codec == PreferredVideoCodec.H264) {
                /* Best H264 */
                dllink = dllinkBestH264;
                filesize = filesizeBestH264;
            } else if (dllinkBestAV1 != null && codec == PreferredVideoCodec.AV1) {
                /* Fallback / best AV1 */
                dllink = dllinkBestAV1;
                filesize = filesizeBestAV1;
            }
            if (dllink == null) {
                /* Fallback */
                if (dllinkBestH264 != null) {
                    /* Fallback / best H264 */
                    dllink = dllinkBestH264;
                    filesize = filesizeBestH264;
                } else if (dllinkBestAV1 != null) {
                    /* Fallback / best AV1 */
                    dllink = dllinkBestAV1;
                    filesize = filesizeBestAV1;
                }
            }
            if (dllink == null && isDownload) {
                /* Fallback to stream download */
                final String correctedBR = br.getRequest().getHtmlCode().replace("\\", "");
                final String continueLink = new Regex(correctedBR, "(\"|\\')(/config\\d+/\\w+/[0-9a-f]+(/)?)(\"|\\')").getMatch(1);
                if (continueLink == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                br.getPage(Encoding.htmlOnlyDecode(continueLink) + (continueLink.endsWith("/") ? "1920" : "/1920"));
                dllink = br.getRegex("<hd\\.file>(https?://.*?)</hd\\.file>").getMatch(0);
                if (dllink == null) {
                    dllink = br.getRegex("<file>(https?://.*?)</file>").getMatch(0);
                    if (dllink == null) {
                        dllink = br.getRegex("file:[\r\n\r ]*?\"(https?://[^<>\"]*?)\"").getMatch(0);
                    }
                }
            }
        } else {
            /* Photo */
            pic = br.getRegex(">\\s*Pic\\s*(\\d+)\\s*of\\s*(\\d+)\\s*see").getRow(0);
            dllink = br.getRegex("src=\"(https?://[^\"]+)\" autoplay ").getMatch(0); // gifs -> mp4
            if (dllink == null) {
                dllink = br.getRegex("\"contentUrl\"\\s*:\\s*\"(.*?\\.(jpe?g|png))\"").getMatch(0);
                if (dllink == null) {
                    dllink = br.getRegex("class=\"mainphoto\" src=\"(https?://[^\"]+)").getMatch(0); // normal image -> jpg
                }
            }
            final String[] jsons = br.getRegex("<script type=\"application/ld\\+json\">([^<]+)</script>").getColumn(0);
            String betterPhotoTitle = null;
            if (jsons != null && jsons.length > 0) {
                for (final String json : jsons) {
                    final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
                    betterPhotoTitle = (String) entries.get("name");
                    if (betterPhotoTitle != null) {
                        break;
                    }
                }
            }
            if (betterPhotoTitle == null) {
                logger.warning("Failed to find betterPhotoTitle");
            } else {
                title = betterPhotoTitle;
            }
        }
        String ext = null;
        if (dllink != null) {
            ext = Plugin.getFileNameExtensionFromURL(dllink);
        }
        if (ext == null) {
            ext = extDefault;
        }
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            title = title.replaceFirst("(?i)\\s*Porn Pic - EPORNER\\s*", "");
            title = title.replaceFirst("\\s*\\- EPORNER Free HD Porn Tube\\s*", "");
            title = title.replaceFirst("\\s*- EPORNER\\s*", "");
            if (pic != null) {
                final int padLength = StringUtils.getPadLength(Integer.parseInt(pic[1]));
                title = StringUtils.formatByPadLength(padLength, Integer.parseInt(pic[0])) + "_" + title;
            }
            link.setFinalFileName(title + ext);
        } else {
            link.setFinalFileName(fallbackFilename);
        }
        if (dllink != null && !isBadDirecturl(dllink)) {
            final String completeURL = br.getURL(dllink).toExternalForm();
            link.setProperty(PROPERTY_DIRECTURL, completeURL);
            link.setProperty(PROPERTY_LAST_DIRECTURL, completeURL);
        }
        /*
         * 2020-05-26: Checking their downloadlink counts towards their daily downloadlimit so only check them if the filesize has not been
         * found already!
         */
        if (filesize > 0) {
            link.setDownloadSize(filesize);
        } else if (link.getView().getBytesTotal() <= 0 && dllink != null && !isBadDirecturl(dllink)) {
            /* Only get filesize from url if we were not able to find it in html --> Saves us time! */
            final Browser br2 = br.cloneBrowser();
            br2.setFollowRedirects(true);
            // In case the link redirects to the finallink
            URLConnectionAdapter con = null;
            try {
                con = br2.openHeadConnection(dllink);
                connectionErrorhandling(con, link, account);
                if (con.getCompleteContentLength() > 0) {
                    if (con.isContentDecoded()) {
                        link.setDownloadSize(con.getCompleteContentLength());
                    } else {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                }
            } finally {
                try {
                    con.disconnect();
                } catch (Throwable e) {
                }
            }
        }
        return AvailableStatus.TRUE;
    }

    private boolean isAgeVerificationBlocked(final Browser br) {
        if (br.containsHTML("<title>Eporner Age Verification</title>|ageVerif/blurred-background")) {
            /* 2025-07-28: French users need to verify. Checking links or downloading is not possible in this state. */
            return true;
        } else {
            return false;
        }
    }

    private PreferredVideoCodec getPreferredVideoCodec(DownloadLink link) {
        final Regex videoUrlRegex = new Regex(link.getProperty(PROPERTY_LAST_DIRECTURL, link.getPluginPatternMatcher()), PATTERN_VIDEO_URL);
        if (videoUrlRegex.patternFind()) {
            if ("-av1".equalsIgnoreCase(videoUrlRegex.getMatch(4))) {
                return PreferredVideoCodec.AV1;
            } else {
                return PreferredVideoCodec.H264;
            }
        }
        final EpornerComConfig cfg = PluginJsonConfig.get(this.getConfigInterface());
        PreferredVideoCodec ret = cfg.getPreferredVideoCodec();
        if (ret == PreferredVideoCodec.DEFAULT) {
            ret = PreferredVideoCodec.H264;
        }
        return ret;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        handleDownload(link, null);
    }

    @Override
    public String getPluginContentURL(DownloadLink link) {
        if (new Regex(link.getPluginPatternMatcher(), PATTERN_VIDEO_URL).patternFind()) {
            return "https://www.eporner.com/video-" + this.getFID(link) + "/";
        } else {
            return link.getPluginPatternMatcher();
        }
    }

    public void handleDownload(final DownloadLink link, final Account account) throws Exception {
        final String storedDirecturl = link.getStringProperty(PROPERTY_DIRECTURL);
        String directurl = null;
        if (storedDirecturl != null) {
            directurl = storedDirecturl;
        } else {
            requestFileInformation(link, account, true);
            directurl = link.getStringProperty(PROPERTY_DIRECTURL);
            if (directurl == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else if (isBadDirecturl(directurl)) {
                errorBrokenVideo();
            }
        }
        try {
            link.setProperty(PROPERTY_LAST_DIRECTURL, directurl);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, directurl, true, 0);
            connectionErrorhandling(dl.getConnection(), link, account);
        } catch (final Exception e) {
            if (storedDirecturl != null) {
                link.removeProperty(PROPERTY_DIRECTURL);
                throw new PluginException(LinkStatus.ERROR_RETRY, "Stored directurl expired", e);
            } else {
                throw e;
            }
        }
        dl.startDownload();
    }

    private void connectionErrorhandling(final URLConnectionAdapter con, final DownloadLink link, final Account account) throws PluginException, IOException {
        /* E.g. https://static.eporner.com/na.mp4 */
        if (isBadDirecturl(con.getURL().toExternalForm())) {
            errorBrokenVideo();
        }
        /* Double-check for broken/bad/dummy-video */
        final String etag = con.getRequest().getResponseHeader("etag");
        if (StringUtils.equalsIgnoreCase(etag, "\"52ee70df-9cdc1\"")) {
            errorBrokenVideo();
        }
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            /* 2020-05-26: Limit = 100 videos per day for unregistered users, no limit for registered users */
            if (br.containsHTML(">\\s*You have downloaded more than|>\\s*Please try again tomorrow or register for free to unlock unlimited")) {
                if (account != null) {
                    /* 2020-05-26: This should never happen in account mode */
                    throw new AccountUnavailableException("Daily download limit reached or session error", 10 * 60 * 1000l);
                } else {
                    throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "Daily download limit reached", 60 * 60 * 1000l);
                }
            } else {
                errorBrokenVideo();
            }
        }
    }

    private boolean isBadDirecturl(final String url) {
        if (url != null && url.matches("(?i)https?://[^/]+/na\\.(flv|mp4).*")) {
            return true;
        } else {
            return false;
        }
    }

    private void errorBrokenVideo() throws PluginException {
        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Broken video?", 5 * 60 * 1000l);
    }

    private String getPreferredStreamQuality(final DownloadLink link) {
        final Regex videoUrlRegex = new Regex(link.getProperty(PROPERTY_LAST_DIRECTURL, link.getPluginPatternMatcher()), PATTERN_VIDEO_URL);
        if (videoUrlRegex.patternFind()) {
            return videoUrlRegex.getMatch(1) + "p";
        }
        final EpornerComConfig cfg = PluginJsonConfig.get(this.getConfigInterface());
        final PreferredStreamQuality quality = cfg.getPreferredStreamQuality();
        switch (quality) {
        case Q2160P:
            return "2160p";
        case Q1440P:
            return "1440p";
        case Q1080P:
            return "1080p";
        case Q720P:
            return "720p";
        case Q480P:
            return "480p";
        case Q360P:
            return "360p";
        case Q240P:
            return "240p";
        case BEST:
        default:
            return null;
        }
    }

    private boolean login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                logger.info("Attempting cookie login");
                this.br.setCookies(this.getHost(), cookies);
                if (!force) {
                    /* Do not validate cookies. */
                    return false;
                }
                br.getPage("https://" + this.getHost() + "/");
                if (this.isLoggedin()) {
                    logger.info("Cookie login successful");
                    /* Refresh cookie timestamp */
                    account.saveCookies(br.getCookies(br.getHost()), "");
                    return true;
                } else {
                    logger.info("Cookie login failed");
                    account.clearCookies("");
                }
            }
            logger.info("Performing full login");
            br.getPage("https://www." + getHost());
            final Form loginform = new Form();
            loginform.setMethod(MethodType.POST);
            loginform.setAction("/xhr/login/");
            loginform.put("xhr", "1");
            loginform.put("act", "login");
            loginform.put("login", Encoding.urlEncode(account.getUser()));
            loginform.put("pass", Encoding.urlEncode(account.getPass()));
            loginform.put("googleToken", "");
            loginform.put("ref", "/");
            br.submitForm(loginform);
            /* 2020-05-26: E.g. login failed: {"status":0,"msg_head":"Login failed.","msg_body":"Bad login\/password"} */
            br.getPage("/");
            if (!isLoggedin()) {
                throw new AccountInvalidException();
            }
            account.saveCookies(br.getCookies(br.getHost()), "");
            return true;
        }
    }

    private boolean isLoggedin() {
        return br.containsHTML("/logout");
    }

    /** Free accounts do not have any downloadlimits. Anonymous users can download max. 100 files per day. */
    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        ai.setUnlimitedTraffic();
        account.setType(AccountType.FREE);
        account.setConcurrentUsePossible(false);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public Class<? extends EpornerComConfig> getConfigInterface() {
        return EpornerComConfig.class;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final jd.plugins.Account acc) {
        /* 2020-05-26: No captchas at all */
        return false;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
        link.removeProperty(PROPERTY_LAST_DIRECTURL);
        link.removeProperty(PROPERTY_DIRECTURL);
    }

    @Override
    public void resetPluginGlobals() {
    }
}