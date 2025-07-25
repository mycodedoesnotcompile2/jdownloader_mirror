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

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.jdownloader.plugins.components.XFileSharingProBasic;

@HostPlugin(revision = "$Revision: 51250 $", interfaceVersion = 3, names = {}, urls = {})
public class StreamwishCom extends XFileSharingProBasic {
    public StreamwishCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: 2024-04-04: null <br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "streamwish.com", "streamwish.to", "awish.pro", "embedwish.com", "wishembed.pro", "vidcloud.top", "gdplry.online", "jwplayerhls.com", "wishonly.site", "dwish.pro", "cloudwish.xyz", "playerwish.com", "rapidplayers.com", "streamhg.com", "hlsflex.com", "swiftplayers.com", "ultpreplayer.com", "recordplay.biz", "hgplaycdn.com", "hailindihg.com", "auvexiug.com", "davioad.com", "habetar.com", "hglink.to", "kravaxxa.com", "zuvioeb.com", "tryzendm.com", "yuguaab.com", "xenolyzb.com", "guxhag.com" });
        /** Tags for developers: streamwish, awish, sexbjcam.com */
        /**
         * Related links for developers: <br>
         * https://board.jdownloader.org/showthread.php?t=97597
         */
        /**
         * Additional infos for developers: <br>
         * jwplayer.key="ITWMv7t88JGzI0xPwW8I0+LveiXX9SWbfdmt0ArUSyc=" <br>
         * -> Use this key to match domains which you believe belong into this plugin. <br>
         * Date when this key was last updated in this comment: 2025-07-09
         */
        return ret;
    }

    /**
     * Keep this updated, it needs to be a domain that: <br>
     * - Works in browser - reCaptchaV3 works in browser (seems like website owner has not configured all domains he owns in his reCaptcha
     * config)
     */
    private static final String INTERNAL_DOWNLOAD_DOMAIN = "hlsflex.com";

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("streamwish.to"); // 2024-08-02
        deadDomains.add("embedwish.com"); // 2024-08-02
        deadDomains.add("awish.pro"); // 2024-08-02
        deadDomains.add("wishembed.pro"); // 2024-08-02
        deadDomains.add("vidcloud.top"); // 2024-08-02
        deadDomains.add("gdplry.online"); // 2024-08-02
        return deadDomains;
    }

    @Override
    public String rewriteHost(final String host) {
        return this.rewriteHost(getPluginDomains(), host);
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return StreamwishCom.buildAnnotationUrls(getPluginDomains());
    }

    public static final String getDefaultAnnotationPatternPartStreamwish() {
        return "/(?:d/[A-Za-z0-9]+|(?:embed-|e/|f/)?[a-z0-9]{12}(?:/[^/]+(?:\\.html)?)?)";
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(?::\\d+)?" + StreamwishCom.getDefaultAnnotationPatternPartStreamwish());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    protected String getPreferredHost(final DownloadLink link, URL url) {
        /*
         * 2024-08-02: Special e.g. streamwish.com can be used for login and website navigation but only INTERNAL_DOWNLOAD_DOMAIN can be
         * used for streaming/downloading.
         */
        if (link != null) {
            return INTERNAL_DOWNLOAD_DOMAIN;
        } else {
            return super.getPreferredHost(link, url);
        }
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return true;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return true;
        } else {
            /* Free(anonymous) and unknown account type */
            return true;
        }
    }

    @Override
    public int getMaxChunks(final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            /* Free Account */
            return 0;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            /* Premium account */
            return 0;
        } else {
            /* Free(anonymous) and unknown account type */
            return 0;
        }
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return -1;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return -1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return -1;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2023-09-06 */
        return false;
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        String betterFilename = br.getRegex("<h1 class=\"h5 mb-3\">([^<]+)</h1>").getMatch(0);
        if (betterFilename == null) {
            /* streamhg.com */
            betterFilename = new Regex(html, "<h3[^>]*>\\s*Download ([^<]+)</h3>").getMatch(0);
        }
        if (betterFilename != null) {
            fileInfo[0] = betterFilename;
        }
        return fileInfo;
    }

    @Override
    public String getFUIDFromURL(final DownloadLink link) {
        final Regex patternSpecial = new Regex(link.getPluginPatternMatcher(), "https://[^/]+/f/([a-z0-9]{12})");
        if (patternSpecial.patternFind()) {
            return patternSpecial.getMatch(0);
        } else {
            return super.getFUIDFromURL(link);
        }
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        /* There are problems with downloading in ipv4/6 mixed mode -> Use IPV4 only */
        br.setIPVersion(IPVERSION.IPV4_ONLY);
        return br;
    }

    @Override
    protected URL_TYPE getURLType(final String url) {
        if (url == null) {
            return null;
        }
        if (url.matches("(?i)^https?://[^/]+/f/([a-z0-9]{12}).*")) {
            return URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD;
        } else {
            return super.getURLType(url);
        }
    }

    @Override
    protected boolean supports_availablecheck_alt() {
        return false;
    }

    @Override
    protected boolean supports_availablecheck_filename_abuse() {
        return false;
    }

    @Override
    protected boolean isOffline(final DownloadLink link, final Browser br) {
        if (br.containsHTML("<div>\\s*This video has been locked watch or does not exist")) {
            return true;
        } else {
            return super.isOffline(link, br);
        }
    }

    @Override
    protected String getDllinkViaOfficialVideoDownloadNew(final Browser br, final DownloadLink link, final Account account, final boolean returnFilesize) throws Exception {
        if (returnFilesize) {
            logger.info("[FilesizeMode] Trying to find official video downloads");
        } else {
            logger.info("[DownloadMode] Trying to find official video downloads");
        }
        final String fuid = this.getFUIDFromURL(link);
        final String download_button_url = br.getRegex("(/f/" + fuid + ")").getMatch(0);
        if (download_button_url != null && !br.getURL().contains(download_button_url)) {
            logger.info("Accessing download page: " + download_button_url);
            this.getPage(br, download_button_url);
        }
        final String[] videourls = br.getRegex("(/f/[a-z0-9]{12}_[a-z]{1})").getColumn(0);
        final String[][] videoresolutionsAndFilesizes = br.getRegex(">\\s*(\\d+x\\d+), (\\d+(\\.\\d{1,2})?,? [A-Za-z]{1,5})").getMatches();
        if (videourls == null || videourls.length == 0) {
            logger.info("Failed to find any official video downloads");
            return null;
        }
        // Parse all video info once and store in list of maps
        final List<Map<String, Object>> parsedVideos = new ArrayList<Map<String, Object>>();
        for (int i = 0; i < videourls.length; i++) {
            final String videoURL = videourls[i];
            String resolution = null;
            String filesizeStr = null;
            if (videoresolutionsAndFilesizes != null && videoresolutionsAndFilesizes.length == videourls.length) {
                final String[] thisVideoResolutionAndFilesize = videoresolutionsAndFilesizes[i];
                resolution = thisVideoResolutionAndFilesize[0];
                filesizeStr = thisVideoResolutionAndFilesize[1];
            }
            final String videoQualityStr = new Regex(videoURL, "_([a-z]{1})$").getMatch(0);
            if (StringUtils.isEmpty(videoQualityStr)) {
                logger.warning("Found unidentifyable video quality");
                continue;
            }
            final Map<String, Object> videoData = new HashMap<String, Object>();
            videoData.put("videoURL", videoURL);
            videoData.put("resolution", resolution);
            videoData.put("filesizeStr", filesizeStr);
            videoData.put("videoQualityStr", videoQualityStr);
            parsedVideos.add(videoData);
        }
        if (parsedVideos.isEmpty()) {
            logger.info("No valid video qualities found");
            return null;
        }
        /*
         * Internal quality identifiers highest to lowest (inside 'download_video' String): o = original, h = high, n = normal, l=low
         */
        final Map<String, Integer> qualityMap = new HashMap<String, Integer>();
        qualityMap.put("l", 20); // low
        qualityMap.put("n", 40); // normal
        qualityMap.put("h", 60); // high
        qualityMap.put("o", 80); // original
        qualityMap.put("x", 100); // download
        // Filter out unknown qualities and add internal quality values
        List<Map<String, Object>> validVideos = new ArrayList<Map<String, Object>>();
        for (Map<String, Object> videoData : parsedVideos) {
            String videoQualityStr = (String) videoData.get("videoQualityStr");
            if (!qualityMap.containsKey(videoQualityStr)) {
                logger.info("Skipping unknown quality: " + videoQualityStr);
                continue;
            }
            videoData.put("internalQualityValue", qualityMap.get(videoQualityStr));
            validVideos.add(videoData);
        }
        if (validVideos.isEmpty()) {
            logger.warning("No valid video qualities found after filtering");
            return null;
        }
        final String userSelectedQualityValue = getPreferredDownloadQualityStr();
        if (userSelectedQualityValue == null) {
            logger.info("Trying to find highest quality for official video download");
        } else {
            logger.info(String.format("Trying to find user selected quality %s for official video download", userSelectedQualityValue));
        }
        Map<String, Object> selectedVideo = null;
        boolean foundUserSelectedQuality = false;
        // First try to find exact match for user selected quality
        if (userSelectedQualityValue != null) {
            for (Map<String, Object> videoData : validVideos) {
                String videoQualityStr = (String) videoData.get("videoQualityStr");
                if (videoQualityStr.equalsIgnoreCase(userSelectedQualityValue)) {
                    logger.info("Found user selected quality: " + userSelectedQualityValue);
                    foundUserSelectedQuality = true;
                    selectedVideo = videoData;
                    break;
                }
            }
            // If user selected quality not found, find next best
            if (!foundUserSelectedQuality) {
                logger.info("Failed to find user selected quality --> Finding next best quality");
                final int userSelectedQualityValueInt = qualityMap.get(userSelectedQualityValue.toLowerCase());
                int nextBestQualityValue = Integer.MAX_VALUE;
                for (Map<String, Object> videoData : validVideos) {
                    int internalQualityValue = (Integer) videoData.get("internalQualityValue");
                    // Find the smallest quality value that is still higher than user selected
                    if (internalQualityValue > userSelectedQualityValueInt && internalQualityValue < nextBestQualityValue) {
                        nextBestQualityValue = internalQualityValue;
                        selectedVideo = videoData;
                    }
                }
                if (selectedVideo != null) {
                    logger.info("Selected next best quality: " + selectedVideo.get("videoQualityStr") + " (higher than target " + userSelectedQualityValue + ")");
                }
            }
        }
        // If no user selection or next best found, find highest quality
        if (selectedVideo == null) {
            int maxInternalQualityValue = 0;
            for (Map<String, Object> videoData : validVideos) {
                int internalQualityValue = (Integer) videoData.get("internalQualityValue");
                if (internalQualityValue > maxInternalQualityValue) {
                    maxInternalQualityValue = internalQualityValue;
                    selectedVideo = videoData;
                }
            }
            if (userSelectedQualityValue != null && !foundUserSelectedQuality) {
                logger.info("No higher quality found, returning highest available: " + selectedVideo.get("videoQualityStr"));
            } else {
                logger.info("Returning BEST quality according to user preference");
            }
        }
        if (selectedVideo == null) {
            logger.warning("Video selection handling failed");
            return null;
        }
        // Extract final values
        final String filesizeStrChosen = (String) selectedVideo.get("filesizeStr");
        final String continueURL = (String) selectedVideo.get("videoURL");
        if (foundUserSelectedQuality) {
            logger.info("Returning user selected quality: " + userSelectedQualityValue);
        } else if (userSelectedQualityValue != null) {
            logger.info("Returning next best or highest quality as fallback");
        } else {
            logger.info("Returning BEST quality according to user preference");
        }
        if (returnFilesize) {
            /* E.g. in availablecheck */
            return filesizeStrChosen;
        }
        getPage(br, continueURL);
        checkErrors(br, continueURL, link, account, false);
        final Form download1 = br.getFormByInputFieldKeyValue("op", "download_orig");
        if (download1 != null) {
            handleCaptcha(link, br, download1);
            submitForm(br, download1);
            checkErrors(br, br.getRequest().getHtmlCode(), link, account, false);
        }
        String dllink = this.getDllink(link, account, br, br.getRequest().getHtmlCode());
        if (StringUtils.isEmpty(dllink)) {
            /*
             * 2019-05-30: Test - worked for: xvideosharing.com - not exactly required as getDllink will usually already return a result.
             */
            dllink = br.getRegex("<a href\\s*=\\s*\"(https?[^\"]+)\"[^>]*>\\s*Direct Download Link\\s*</a>").getMatch(0);
            if (dllink == null) {
                /* 2024-06-25 */
                dllink = br.getRegex("href=\"(https?://[^\"]+)\" class=\"dwnl").getMatch(0);
                if (dllink == null) {
                    /* 2025-04-17 */
                    dllink = br.getRegex("href=\"(https?://[^\"]+)\"[^>]*>\\s*Download\\s*<").getMatch(0);
                }
            }
        }
        if (StringUtils.isEmpty(dllink)) {
            logger.warning("Failed to find dllink via official video download");
            return null;
        }
        logger.info("Successfully found dllink via official video download");
        return dllink;
    }

    @Override
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account, final boolean checkAll) throws NumberFormatException, PluginException {
        if (br.containsHTML(">\\s*Video temporarily not available")) {
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Video temporarily not available");
        }
        super.checkErrors(br, html, link, account, checkAll);
        final String errorsMisc = br.getRegex("class=\"icon icon-info icon-size-16 me-3\"[^>]*></i>\\s*<div>([^<]+)</div>").getMatch(0);
        if (errorsMisc != null) {
            throw new PluginException(LinkStatus.ERROR_FATAL, Encoding.htmlDecode(errorsMisc).trim());
        }
        final String errorsMisc2 = br.getRegex("<div class=\"text-danger text-center[^\"]*\"[^>]*>([^<]+)</div>").getMatch(0);
        if (errorsMisc2 != null) {
            /* E.g. error "Downloads disabled 620" */
            throw new PluginException(LinkStatus.ERROR_FATAL, Encoding.htmlDecode(errorsMisc2).trim());
        }
    }

    @Override
    protected String getDllinkVideohost(final DownloadLink link, final Account account, final Browser br, final String src) {
        final String dllink = new Regex(src, "\"hls2\":\"(http[^\"]+)").getMatch(0);
        if (dllink != null) {
            return dllink;
        }
        return super.getDllinkVideohost(link, account, br, src);
    }

    @Override
    protected boolean isVideohoster_enforce_video_filename() {
        return true;
    }

    @Override
    public ArrayList<String> getCleanupHTMLRegexes() {
        /* 2025-04-17: Required because otherwise, "display:none" elements are removed from html code in a wrong way. */
        return null;
    }
}