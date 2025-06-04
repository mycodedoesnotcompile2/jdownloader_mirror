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
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.jdownloader.plugins.components.XFileSharingProBasic;

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

@HostPlugin(revision = "$Revision: 51110 $", interfaceVersion = 3, names = {}, urls = {})
public class VidhideCom extends XFileSharingProBasic {
    public VidhideCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(super.getPurchasePremiumURL());
    }

    /**
     * DEV NOTES XfileSharingProBasic Version SEE SUPER-CLASS<br />
     * mods: See overridden functions<br />
     * limit-info:<br />
     * captchatype-info: null 4dignum reCaptchaV2, hcaptcha<br />
     * other:<br />
     */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "vidhidepro.com", "vidhide.com", "vidhidehub.com", "moflix-stream.click", "javplaya.com", "filelions.to", "filelions.com", "filelions.online", "filelions.site", "alions.pro", "azipcdn.com", "vidhidepre.com", "dlions.pro", "playrecord.biz", "mycloudz.cc", "vidhideplus.com", "nikaplayer.com", "niikaplayerr.com", "vidhidefast.com", "seraphinapl.com" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("azipcdn.com");
        deadDomains.add("filelions.com"); // 2024-08-02
        deadDomains.add("filelions.site"); // 2024-08-02
        deadDomains.add("alions.pro"); // 2024-08-02
        deadDomains.add("filelions.site"); // 2024-11-25
        deadDomains.add("vidhide.com"); // 2025-03-14
        return deadDomains;
    }

    @Override
    public String rewriteHost(final String host) {
        return this.rewriteHost(getPluginDomains(), host);
    }

    @Override
    public Browser createNewBrowserInstance() {
        // final download URLs are signed with IP but download servers only support IPv4 -> signature containing IPv6 will fail on IPv4
        // Server
        final Browser ret = super.createNewBrowserInstance();
        ret.setIPVersion(IPVERSION.IPV4_ONLY);
        return ret;
    }

    /**
     * Not all domains can be used for downloadlinks. <br>
     * Example NOT working: vidhide.com <br>
     * Example working: vidhidehub.com <br>
     *
     * <b>IMPORTANT:</b> Keep this up2date!
     */
    private static final String MAIN_DOWNLOAD_DOMAIN = "vidhidehub.com";

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return VidhideCom.buildAnnotationUrls(getPluginDomains());
    }

    public static final String getDefaultAnnotationPatternPartVidhideCom() {
        return "/(?:d/[A-Za-z0-9]+|(?:embed-|embed/|e/|f/|file/|v/)?[a-z0-9]{12}(?:/[^/]+(?:\\.html)?)?)";
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(?::\\d+)?" + VidhideCom.getDefaultAnnotationPatternPartVidhideCom());
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

    private final Pattern PATTERN_SPECIAL   = Pattern.compile("/f/([a-z0-9]{12}).*", Pattern.CASE_INSENSITIVE);
    private final Pattern PATTERN_SPECIAL_2 = Pattern.compile("/v/([a-z0-9]{12}).*", Pattern.CASE_INSENSITIVE);
    private final Pattern PATTERN_SPECIAL_3 = Pattern.compile("/file/([a-z0-9]{12}).*", Pattern.CASE_INSENSITIVE);
    private final Pattern PATTERN_SPECIAL_4 = Pattern.compile("/embed/([a-z0-9]{12}).*", Pattern.CASE_INSENSITIVE);

    @Override
    protected URL_TYPE getURLType(final String url) {
        if (url == null) {
            return null;
        }
        if (new Regex(url, PATTERN_SPECIAL).patternFind()) {
            return URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD;
        } else if (new Regex(url, PATTERN_SPECIAL_2).patternFind()) {
            return URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD;
        } else if (new Regex(url, PATTERN_SPECIAL_3).patternFind()) {
            return URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD;
        } else if (new Regex(url, PATTERN_SPECIAL_4).patternFind()) {
            return URL_TYPE.OFFICIAL_VIDEO_DOWNLOAD;
        } else {
            return super.getURLType(url);
        }
    }

    @Override
    public String getFUIDFromURL(final DownloadLink link) {
        if (link == null) {
            return null;
        } else if (link.getPluginPatternMatcher() == null) {
            return null;
        }
        final Regex special1 = new Regex(link.getPluginPatternMatcher(), PATTERN_SPECIAL);
        final Regex special2;
        final Regex special3;
        final Regex special4;
        if (special1.patternFind()) {
            return special1.getMatch(0);
        } else if ((special2 = new Regex(link.getPluginPatternMatcher(), PATTERN_SPECIAL_2)).patternFind()) {
            return special2.getMatch(0);
        } else if ((special3 = new Regex(link.getPluginPatternMatcher(), PATTERN_SPECIAL_3)).patternFind()) {
            return special3.getMatch(0);
        } else if ((special4 = new Regex(link.getPluginPatternMatcher(), PATTERN_SPECIAL_4)).patternFind()) {
            return special4.getMatch(0);
        }
        return super.getFUIDFromURL(link);
    }

    @Override
    protected String getDllinkViaOfficialVideoDownloadNew(final Browser br, final DownloadLink link, final Account account, final boolean returnFilesize) throws Exception {
        if (returnFilesize) {
            logger.info("[FilesizeMode] Trying to find official video downloads");
        } else {
            logger.info("[DownloadMode] Trying to find official video downloads");
        }
        final String[] videourls = br.getRegex("(/download/[a-z0-9]{12}_[a-z]{1})").getColumn(0);
        final String[][] videoresolutionsAndFilesizes = br.getRegex(">\\s*(\\d+x\\d+),? (\\d+(\\.\\d{1,2})?,? [A-Za-z]{1,5})").getMatches();
        if (videourls == null || videourls.length == 0) {
            logger.info("Failed to find any official video downloads");
            return null;
        }
        final String[][] videoInfo = new String[videourls.length][];
        for (int i = 0; i < videourls.length; i++) {
            final String[] thisVideoInfos = new String[3];
            thisVideoInfos[0] = videourls[i];
            if (videoresolutionsAndFilesizes != null && videoresolutionsAndFilesizes.length == videourls.length) {
                final String[] thisVideoResolutionAndFilesize = videoresolutionsAndFilesizes[i];
                thisVideoInfos[1] = thisVideoResolutionAndFilesize[0];
                thisVideoInfos[2] = thisVideoResolutionAndFilesize[1];
            }
            videoInfo[i] = thisVideoInfos;
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
        long maxInternalQualityValue = 0;
        String filesizeStrBest = null;
        String filesizeStrSelected = null;
        String videoURLBest = null;
        String videoURLSelected = null;
        final String userSelectedQualityValue = getPreferredDownloadQualityStr();
        if (userSelectedQualityValue == null) {
            logger.info("Trying to find highest quality for official video download");
        } else {
            logger.info(String.format("Trying to find user selected quality %s for official video download", userSelectedQualityValue));
        }
        for (final String videoInfos[] : videoInfo) {
            final String videoURL = videoInfos[0];
            final String filesizeStr = videoInfos[2];
            final String videoQualityStrTmp = new Regex(videoURL, "_([a-z]{1})$").getMatch(0);
            if (StringUtils.isEmpty(videoQualityStrTmp)) {
                /*
                 * Possible plugin failure but let's skip bad items. Upper handling will fallback to stream download if everything fails!
                 */
                logger.warning("Found unidentifyable video quality");
                continue;
            } else if (!qualityMap.containsKey(videoQualityStrTmp)) {
                /*
                 * 2020-01-18: There shouldn't be any unknown values but we should consider allowing such in the future maybe as final
                 * fallback.
                 */
                logger.info("Skipping unknown quality: " + videoQualityStrTmp);
                continue;
            }
            /* Look for best quality */
            final int internalQualityValueTmp = qualityMap.get(videoQualityStrTmp);
            if (internalQualityValueTmp > maxInternalQualityValue || videoURLBest == null) {
                maxInternalQualityValue = internalQualityValueTmp;
                videoURLBest = videoURL;
                filesizeStrBest = filesizeStr;
            }
            if (userSelectedQualityValue != null && videoQualityStrTmp.equalsIgnoreCase(userSelectedQualityValue)) {
                logger.info("Found user selected quality: " + userSelectedQualityValue);
                videoURLSelected = videoURL;
                if (filesizeStr != null) {
                    /*
                     * Usually, filesize for official video downloads will be given but not in all cases. It may also happen that our upper
                     * RegEx fails e.g. for supervideo.tv.
                     */
                    filesizeStrSelected = filesizeStr;
                }
            }
        }
        if (videoURLBest == null && videoURLSelected == null) {
            logger.warning("Video selection handling failed");
            return null;
        }
        final String filesizeStrChosen;
        final String continueURL;
        if (filesizeStrSelected == null) {
            if (userSelectedQualityValue == null) {
                logger.info("Returning BEST quality according to user preference");
            } else {
                logger.info("Returning BEST quality as fallback");
            }
            filesizeStrChosen = filesizeStrBest;
            continueURL = videoURLBest;
        } else {
            logger.info("Returning user selected quality: " + userSelectedQualityValue);
            filesizeStrChosen = filesizeStrSelected;
            continueURL = videoURLSelected;
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
            logger.info("Waiting extra wait seconds: " + getDllinkViaOfficialVideoDownloadExtraWaittimeSeconds());
            this.sleep(getDllinkViaOfficialVideoDownloadExtraWaittimeSeconds() * 1000l, link);
            submitForm(br, download1);
            checkErrors(br, br.getRequest().getHtmlCode(), link, account, false);
        }
        final String dllink = this.getDllink(link, account, br, br.getRequest().getHtmlCode());
        if (StringUtils.isEmpty(dllink)) {
            logger.warning("Failed to find dllink via official video download");
            return null;
        }
        logger.info("Successfully found dllink via official video download");
        final String filesizeBytesStr = br.getRegex("Exact size\\s*</td>\\s*<td>(\\d+) bytes").getMatch(0);
        if (filesizeBytesStr != null) {
            logger.info("Found precise expected filesize");
            link.setVerifiedFileSize(Long.parseLong(filesizeBytesStr));
        }
        return dllink;
    }

    @Override
    public String[] scanInfo(final String html, final String[] fileInfo) {
        super.scanInfo(html, fileInfo);
        final String betterFilename = new Regex(html, "<h3[^>]*>\\s*Download([^<]*)</h3>").getMatch(0);
        if (betterFilename != null) {
            fileInfo[0] = betterFilename;
        }
        return fileInfo;
    }

    @Override
    protected void checkErrors(final Browser br, final String html, final DownloadLink link, final Account account, final boolean checkAll) throws NumberFormatException, PluginException {
        super.checkErrors(br, html, link, account, checkAll);
        if (br.containsHTML("Video embed restricted for this domain")) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Video embed restricted for this domain");
        }
        final String errorsMisc2 = br.getRegex("<div class=\"text-danger text-center[^\"]*\"[^>]*>([^<]+)</div>").getMatch(0);
        if (errorsMisc2 != null) {
            /* E.g. error "Downloads disabled 620" */
            throw new PluginException(LinkStatus.ERROR_FATAL, Encoding.htmlDecode(errorsMisc2).trim());
        }
    }

    @Override
    protected String getPreferredHost(final DownloadLink link, URL url) {
        return MAIN_DOWNLOAD_DOMAIN;
    }
}