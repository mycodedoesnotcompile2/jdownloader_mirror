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
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.PluginException;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.XFileSharingProBasic;

@HostPlugin(revision = "$Revision: 51267 $", interfaceVersion = 3, names = {}, urls = {})
public class MyqloudOrg extends XFileSharingProBasic {
    public MyqloudOrg(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    private boolean requiresAccountToDownload = false;

    @Override
    public void reset() {
        super.reset();
        requiresAccountToDownload = false;
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: 2020-03-11: reCaptchaV2<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "myqloud.org" });
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
        return XFileSharingProBasic.buildAnnotationUrls(getPluginDomains());
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
    public int getMaxChunks(final Account account) {
        if (account != null && account.getType() == AccountType.FREE) {
            /* Free Account */
            return -5;
        } else if (account != null && account.getType() == AccountType.PREMIUM) {
            /* Premium account */
            return -5;
        } else {
            /* Free(anonymous) and unknown account type */
            return -5;
        }
    }

    @Override
    public int getMaxSimultaneousFreeAnonymousDownloads() {
        return 1;
    }

    @Override
    public int getMaxSimultaneousFreeAccountDownloads() {
        return 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 1;
    }

    @Override
    protected boolean isVideohoster_enforce_video_filename() {
        /* 2020-03-11: Special */
        return true;
    }

    /**
     * This does currently not work via main template. Keep it this way once it is vlear whether this can be integrated in template or not!
     */
    @Override
    protected String getDllinkViaOfficialVideoDownload(final Browser brc, final DownloadLink link, final Account account, final boolean returnFilesize) throws Exception {
        logger.info("Trying to find official video downloads");
        String dllink = null;
        /* Info in table. E.g. xvideosharing.com, watchvideo.us */
        String[] videoQualityHTMLs = new Regex(getCorrectBR(brc), "<tr><td>[^\r\t\n]+download_video\\(.*?</td></tr>").getColumn(-1);
        if (videoQualityHTMLs.length == 0) {
            /* Match on line - safe attempt but this may not include filesize! */
            videoQualityHTMLs = new Regex(getCorrectBR(brc), "download_video\\([^\r\t\n]+").getColumn(-1);
        }
        if (videoQualityHTMLs.length == 0) {
            /* 2020-03-11: Special */
            videoQualityHTMLs = new Regex(getCorrectBR(brc), "<div class=\"badge-download high-quality.*?download icon-secondary").getColumn(-1);
        }
        if (videoQualityHTMLs.length == 0) {
            logger.info("Failed to find any official video downloads");
            return null;
        }
        // Parse all video quality HTML once and store in list of maps
        List<Map<String, Object>> parsedQualities = new ArrayList<Map<String, Object>>();
        for (int i = 0; i < videoQualityHTMLs.length; i++) {
            final String videoQualityHTML = videoQualityHTMLs[i];
            final String filesizeStrTmp = new Regex(videoQualityHTML, "(([0-9\\.]+)\\s*(KB|MB|GB|TB))").getMatch(0);
            final Regex videoinfo = new Regex(videoQualityHTML, "download_video\\('([a-z0-9]+)','([^<>\"\\']*)','([^<>\"\\']*)'");
            String videoQualityStrTmp = videoinfo.getMatch(1);
            if (videoQualityStrTmp == null) {
                /* 2020-03-11: Special */
                videoQualityStrTmp = new Regex(videoQualityHTML, "data-vmode=\"\\s*(.)\\s*\"").getMatch(0);
            }
            String videoHashTmp = videoinfo.getMatch(2);
            if (videoHashTmp == null) {
                /* 2020-03-11: Special */
                videoHashTmp = new Regex(videoQualityHTML, "data-hash=\"\\s*(.*?)\\s*\"").getMatch(0);
            }
            if (StringUtils.isEmpty(videoQualityStrTmp) || StringUtils.isEmpty(videoHashTmp)) {
                continue;
            }
            Map<String, Object> qualityData = new HashMap<String, Object>();
            qualityData.put("videoQualityStr", videoQualityStrTmp);
            qualityData.put("videoHash", videoHashTmp);
            qualityData.put("filesizeStr", filesizeStrTmp);
            qualityData.put("targetHTML", videoQualityHTML);
            qualityData.put("index", i);
            parsedQualities.add(qualityData);
        }
        if (parsedQualities.isEmpty()) {
            logger.info("No valid video qualities found");
            return null;
        }
        /** TODO: Add quality selection: Low, Medium, Original Example: deltabit.co */
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
        List<Map<String, Object>> validQualities = new ArrayList<Map<String, Object>>();
        for (Map<String, Object> qualityData : parsedQualities) {
            String videoQualityStr = (String) qualityData.get("videoQualityStr");
            if (!qualityMap.containsKey(videoQualityStr)) {
                logger.info("Skipping unknown quality: " + videoQualityStr);
                continue;
            }
            qualityData.put("internalQualityValue", qualityMap.get(videoQualityStr));
            validQualities.add(qualityData);
        }
        if (validQualities.isEmpty()) {
            logger.warning("No valid video qualities found after filtering");
            return null;
        }
        final String userSelectedQualityValue = getPreferredDownloadQualityStr();
        if (userSelectedQualityValue == null) {
            logger.info("Trying to find highest quality for official video download");
        } else {
            logger.info(String.format("Trying to find user selected quality %s for official video download", userSelectedQualityValue));
        }
        Map<String, Object> selectedQuality = null;
        boolean foundUserSelectedQuality = false;
        // First try to find exact match for user selected quality
        if (userSelectedQualityValue != null) {
            for (Map<String, Object> qualityData : validQualities) {
                String videoQualityStr = (String) qualityData.get("videoQualityStr");
                if (videoQualityStr.equalsIgnoreCase(userSelectedQualityValue)) {
                    logger.info("Found user selected quality: " + userSelectedQualityValue);
                    foundUserSelectedQuality = true;
                    selectedQuality = qualityData;
                    break;
                }
            }
        }
        // If user selected quality not found, find next best
        if (userSelectedQualityValue != null && !foundUserSelectedQuality) {
            logger.info("Failed to find user selected quality --> Finding next best quality");
            final int userSelectedQualityValueInt = qualityMap.get(userSelectedQualityValue.toLowerCase());
            int nextBestQualityValue = Integer.MAX_VALUE;
            for (Map<String, Object> qualityData : validQualities) {
                int internalQualityValue = (Integer) qualityData.get("internalQualityValue");
                // Find the smallest quality value that is still higher than user selected
                if (internalQualityValue > userSelectedQualityValueInt && internalQualityValue < nextBestQualityValue) {
                    nextBestQualityValue = internalQualityValue;
                    selectedQuality = qualityData;
                }
            }
            if (selectedQuality != null) {
                logger.info("Selected next best quality: " + selectedQuality.get("videoQualityStr") + " (higher than target " + userSelectedQualityValue + ")");
            }
        }
        // If no user selection or next best found, find highest quality
        if (selectedQuality == null) {
            int maxInternalQualityValue = 0;
            for (Map<String, Object> qualityData : validQualities) {
                int internalQualityValue = (Integer) qualityData.get("internalQualityValue");
                if (internalQualityValue > maxInternalQualityValue) {
                    maxInternalQualityValue = internalQualityValue;
                    selectedQuality = qualityData;
                }
            }
            if (userSelectedQualityValue != null && !foundUserSelectedQuality) {
                logger.info("No higher quality found, returning highest available: " + selectedQuality.get("videoQualityStr"));
            } else {
                logger.info("Trying to find highest quality for official video download");
            }
        }
        if (selectedQuality == null) {
            logger.info("Failed to find officially downloadable video quality although there are qualities available");
            return null;
        }
        // Extract final values
        String filesizeStr = (String) selectedQuality.get("filesizeStr");
        String videoQualityStr = (String) selectedQuality.get("videoQualityStr");
        String videoHash = (String) selectedQuality.get("videoHash");
        String targetHTML = (String) selectedQuality.get("targetHTML");
        logger.info("Selected videoquality: " + videoQualityStr);
        if (returnFilesize) {
            /* E.g. in availablecheck */
            return filesizeStr;
        }
        try {
            /* 2019-08-29: Waittime here is possible but a rare case e.g. deltabit.co */
            this.waitTime(link, System.currentTimeMillis());
            /*
             * TODO: Fix issue where first request leads to '<br><b class="err">Security error</b>' (reproduced over multiple filehosts e.g.
             * xvideosharing.com)
             */
            /* 2020-03-11: Special */
            getPage(brc, "/dl?op=download_orig_pre&id=" + this.getFUIDFromURL(link) + "&mode=" + videoQualityStr + "&hash=" + videoHash);
            if (isPremiumOnly(brc)) {
                /* 2020-11-12 */
                requiresAccountToDownload = true;
                throw new AccountRequiredException();
            }
            checkErrors(brc, getCorrectBR(brc), link, account, false);
            /* 2019-08-29: This Form may sometimes be given e.g. deltabit.co */
            Form download1 = brc.getFormByInputFieldKeyValue("op", "download1");
            if (download1 == null) {
                download1 = brc.getFormbyProperty("id", "generate-download-link");
            }
            if (download1 != null) {
                /* 2020-03-11: Special: reCaptchaV2 & waittime --> No captcha in premium mode */
                final long timebefore = System.currentTimeMillis();
                /* 2020-03-20: Try catch is small workaround attempt before the weekend. */
                if (containsRecaptchaV2Class(download1)) {
                    logger.info("download1 Form: Captcha required");
                    handleCaptcha(link, brc, download1);
                } else {
                    logger.info("download1 Form: No captcha required");
                }
                this.waitTime(link, timebefore);
                this.submitForm(brc, download1);
            }
            /*
             * 2019-10-04: TODO: Unsure whether we should use the general 'getDllink' method here as it contains a lot of RegExes (e.g. for
             * streaming URLs) which are completely useless here.
             */
            dllink = this.getDllink(link, account, brc, brc.toString());
            if (StringUtils.isEmpty(dllink)) {
                /* 2019-05-30: Test - worked for: xvideosharing.com */
                dllink = new Regex(brc.toString(), "<a href=\"(https?[^\"]+)\"[^>]*>Direct Download Link</a>").getMatch(0);
            }
            if (StringUtils.isEmpty(dllink)) {
                /* 2019-08-29: Test - worked for: deltabit.co */
                dllink = regexVideoStreamDownloadURL(brc.toString());
            }
            if (StringUtils.isEmpty(dllink)) {
                logger.info("Failed to find final downloadurl");
            }
        } catch (final Throwable e) {
            e.printStackTrace();
            logger.warning("Official video download failed: Exception occured");
            /*
             * Continue via upper handling - usually videohosts will have streaming URLs available so a failure of this is not fatal for us.
             */
        }
        if (StringUtils.isEmpty(dllink)) {
            logger.warning("Failed to find dllink via official video download");
        } else {
            logger.info("Successfully found dllink via official video download");
        }
        return dllink;
    }

    @Override
    protected void checkErrorsLastResort(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        if (requiresAccountToDownload) {
            throw new AccountRequiredException();
        } else {
            super.checkErrorsLastResort(br, link, account);
        }
    }

    @Override
    protected String getPremiumOnlyErrorMessage(final Browser br) {
        String msg = br.getRegex(">\\s*(Only Premium users can download files)").getMatch(0);
        if (msg != null) {
            return msg;
        } else {
            return super.getPremiumOnlyErrorMessage(br);
        }
    }

    @Override
    protected String regexWaittime(Browser br) {
        /**
         * TODO: 2019-05-15: Try to grab the whole line which contains "id"="countdown" and then grab the waittime from inside that as it
         * would probably make this more reliable.
         */
        /* Ticket Time */
        String waitStr = super.regexWaittime(br);
        if (waitStr == null) {
            waitStr = new Regex(br.getRequest().getHtmlCode(), "id=\"[a-z0-9]+\">(\\d+)</span> seconds").getMatch(0);
        }
        return waitStr;
    }

    private final String regexVideoStreamDownloadURL(final String src) {
        String dllink = new Regex(src, Pattern.compile("(https?://[^/]+[^\"]+[a-z0-9]{60}/v\\.mp4)", Pattern.CASE_INSENSITIVE)).getMatch(0);
        if (StringUtils.isEmpty(dllink)) {
            /* Wider attempt */
            dllink = new Regex(src, Pattern.compile("\"(https?://[^/]+/[a-z0-9]{60}/[^\"]+)\"", Pattern.CASE_INSENSITIVE)).getMatch(0);
        }
        return dllink;
    }

    @Override
    protected boolean supports_availablecheck_filesize_html() {
        /* 2020-08-10: Special: Prevents this plugin from picking up random trash as filesize */
        return false;
    }
}