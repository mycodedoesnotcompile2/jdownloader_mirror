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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasic;
import org.jdownloader.plugins.components.config.XFSConfigMediaCm;
import org.jdownloader.plugins.components.config.XFSConfigVideo;

@HostPlugin(revision = "$Revision: 51267 $", interfaceVersion = 3, names = {}, urls = {})
public class MediaCm extends XFileSharingProBasic {
    public MediaCm(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: 2023-04-03: reCaptchaV2 <br />
     * other: Similar to: streamhide.com <br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "media.cm", "cloudvideo.tv" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(?::\\d+)?" + "/(?:d/)?([a-z0-9]{12})");
        }
        return ret.toArray(new String[0]);
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
    protected String getDllinkViaOfficialVideoDownload(final Browser br, final DownloadLink link, final Account account, final boolean returnFilesize) throws Exception {
        if (returnFilesize) {
            logger.info("[FilesizeMode] Trying to find official video downloads");
        } else {
            logger.info("[DownloadMode] Trying to find official video downloads");
        }
        final String[][] videoInfo = br.getRegex("<a href=\"(https?://[^\"]+)\"[^>]*>\\s*<i data-feather=\"download\"></i> <strong>[^<]*</strong>[^<]*<strong class=\"tx-white tx-bold\">([^<]+)</strong>").getMatches();
        if (videoInfo == null || videoInfo.length == 0) {
            logger.info("Failed to find any official video downloads");
            return null;
        }
        // Parse all video info once and store in list of maps
        List<Map<String, Object>> parsedVideos = new ArrayList<Map<String, Object>>();
        for (int i = 0; i < videoInfo.length; i++) {
            final String videoURL = videoInfo[i][0];
            final String filesizeStr = videoInfo[i][1];
            final String videoQualityStr = new Regex(videoURL, "_([a-z]{1})$").getMatch(0);
            if (StringUtils.isEmpty(videoQualityStr)) {
                logger.warning("Found unidentifyable video quality");
                continue;
            }
            Map<String, Object> videoData = new HashMap<String, Object>();
            videoData.put("videoURL", videoURL);
            videoData.put("filesizeStr", filesizeStr);
            videoData.put("videoQualityStr", videoQualityStr);
            videoData.put("index", i);
            parsedVideos.add(videoData);
        }
        if (parsedVideos.isEmpty()) {
            logger.info("No valid video qualities found");
            return null;
        }
        /*
         * Internal quality identifiers highest to lowest (inside 'download_video' String): o = original, h = high, n = normal, l=low
         */
        final HashMap<String, Integer> qualityMap = new HashMap<String, Integer>();
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
        }
        // If user selected quality not found, find next best
        if (userSelectedQualityValue != null && !foundUserSelectedQuality) {
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
        this.getPage(br, continueURL);
        String dllink = null;
        final Form download1 = br.getFormByInputFieldKeyValue("op", "download_orig");
        if (download1 != null) {
            this.handleCaptcha(link, br, download1);
            this.submitForm(br, download1);
            this.checkErrors(br, br.getRequest().getHtmlCode(), link, account, false);
        }
        dllink = this.getDllink(link, account, br, br.toString());
        if (StringUtils.isEmpty(dllink)) {
            dllink = br.getRegex("This download link will be available for your IP-address next[^<]*</div>\\s*<a href=\"(https?://[^\"]+)").getMatch(0);
        }
        if (StringUtils.isEmpty(dllink)) {
            logger.warning("Failed to find dllink via official video download");
        } else {
            logger.info("Successfully found dllink via official video download");
        }
        return dllink;
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
    protected boolean isVideohoster_enforce_video_filename() {
        return true;
    }

    @Override
    protected String getContentURL(final DownloadLink link) {
        return this.getMainPage() + this.buildURLPath(link, this.getFUIDFromURL(link), URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD);
    }

    protected String getFUID(final String url, URL_TYPE type) {
        if (url != null) {
            return new Regex(url, this.getSupportedLinks()).getMatch(0);
        } else {
            return null;
        }
    }

    @Override
    protected boolean supports_availablecheck_filename_abuse() {
        return false;
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        final String betterFilename = new Regex(html, "(?i)<Title>\\s*Download ([^<]+)</Title>").getMatch(0);
        if (betterFilename != null) {
            fileInfo[0] = betterFilename;
        }
        return fileInfo;
    }

    @Override
    public Class<? extends XFSConfigVideo> getConfigInterface() {
        return XFSConfigMediaCm.class;
    }
}