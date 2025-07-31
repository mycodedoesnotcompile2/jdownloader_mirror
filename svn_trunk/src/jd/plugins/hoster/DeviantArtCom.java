//    jDownloader - Downloadmanager
//    Copyright (C) 2012  JD-Team support@jdownloader.org
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
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.downloader.text.TextDownloader;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.config.DeviantArtComConfig;
import org.jdownloader.plugins.components.config.DeviantArtComConfig.ImageDownloadMode;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLSearch;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;

@HostPlugin(revision = "$Revision: 51286 $", interfaceVersion = 3, names = {}, urls = {})
public class DeviantArtCom extends PluginForHost {
    private final String               TYPE_DOWNLOADALLOWED_HTML                   = "class=\"text\">\\s*HTML download\\s*</span>";
    private final String               TYPE_DOWNLOADFORBIDDEN_HTML                 = "<div class=\"grf\\-indent\"";
    private boolean                    downloadHTML                                = false;
    private String                     betterHTML                                  = null;
    private boolean                    accountRequiredWhenDownloadImpossible       = false;
    public static final Pattern        PATTERN_STASH                               = Pattern.compile("/stash/([a-z0-9]+)", Pattern.CASE_INSENSITIVE);
    public static final Pattern        PATTERN_ART                                 = Pattern.compile("/([\\w\\-]+/)?art/([\\w\\-]+)-(\\d+)", Pattern.CASE_INSENSITIVE);
    public static final Pattern        PATTERN_JOURNAL                             = Pattern.compile("/([\\w\\-]+/)?journal/([\\w\\-]+)-(\\d+)", Pattern.CASE_INSENSITIVE);
    public static final Pattern        PATTERN_STATUS                              = Pattern.compile("/([\\w\\-]+)/([\\w\\-]+/)?status(?:-update)?/(\\d+)", Pattern.CASE_INSENSITIVE);
    public static final String         PROPERTY_USERNAME                           = "username";
    public static final String         PROPERTY_TITLE                              = "title";
    public static final String         PROPERTY_TYPE                               = "type";
    private static final String        PROPERTY_OFFICIAL_DOWNLOADURL               = "official_downloadurl";
    public static final String         PROPERTY_IMAGE_DISPLAY_OR_PREVIEW_URL       = "image_display_or_preview_url";
    public static final String         PROPERTY_IMAGE_DISPLAY_OR_PREVIEW_URL_2     = "image_display_or_preview_url_2";
    public static final String         PROPERTY_VIDEO_DISPLAY_OR_PREVIEW_URL       = "video_display_or_preview_url";
    private static final String        PROPERTY_DEVIATION_HTML                     = "deviation_html";
    private static final String        PROPERTY_TIER_ACCESS                        = "tier_access";
    public static final String         PROPERTY_IMAGE_POSITION                     = "image_position";
    public static final String         PROPERTY_IMAGE_POSITION_MAX                 = "image_position_max";
    public static final String         PROPERTY_IMAGE_TOKEN                        = "image_token";
    private static final boolean       LOGIN_ALWAYS_REQUIRED_FOR_OFFICIAL_DOWNLOAD = true;
    /* Don't touch the following! */
    private static final AtomicInteger freeDownloadsRunning                        = new AtomicInteger(0);
    private static final AtomicInteger accountDownloadsRunning                     = new AtomicInteger(0);

    @Override
    public String getPluginContentURL(final DownloadLink link) {
        String ret = super.getPluginContentURL(link);
        if (ret == null) {
            return null;
        }
        final int imagePosition = getImagePosition(link);
        if (imagePosition > 1) {
            ret += "#image-" + imagePosition;
        }
        return ret;
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "deviantart.com" });
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
            String regex = "https?://[\\w\\.\\-]*?" + buildHostsPatternPart(domains) + "(";
            regex += PATTERN_STASH.pattern();
            regex += "|";
            regex += PATTERN_ART.pattern();
            regex += "|";
            regex += PATTERN_JOURNAL.pattern();
            regex += "|";
            regex += PATTERN_STATUS.pattern();
            regex += ")";
            ret.add(regex);
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY, LazyPlugin.FEATURE.COOKIE_LOGIN_ONLY };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setCookie(getHost(), "agegate_state", "1");
        br.setFollowRedirects(true);
        return br;
    }

    /**
     * @author raztoki, pspzockerscene, Jiaz
     */
    @SuppressWarnings("deprecation")
    public DeviantArtCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/join/");
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/about/policy/service/";
    }

    @Override
    public void init() {
        super.init();
        Browser.setRequestIntervalLimitGlobal(getHost(), 1500);
    }

    @Override
    public boolean isProxyRotationEnabledForLinkChecker() {
        return false;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            final int imageposition = getImagePosition(link);
            if (imageposition == -1) {
                return this.getHost() + "://" + fid;
            } else {
                return this.getHost() + "://" + fid + "/image_position/" + imageposition;
            }
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return false;
    }

    private String getFID(final DownloadLink link) {
        final String contenturl = link.getPluginPatternMatcher();
        String fid = new Regex(contenturl, PATTERN_STASH).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(contenturl, "(\\d+)$").getMatch(0);
        return fid;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        return requestFileInformation(link, account);
    }

    public static void parseDeviationJSON(final Plugin plugin, final DownloadLink link, final Map<String, Object> art) {
        // author can also be id(number) of author in users map
        setTitleProperty(link, art.get("title").toString());
        link.setProperty(PROPERTY_TYPE, art.get("type"));
        link.setProperty(PROPERTY_TIER_ACCESS, art.get("tierAccess"));
        link.setProperty(PROPERTY_DEVIATION_HTML, JavaScriptEngineFactory.walkJson(art, "textContent/html/markup"));
        final boolean isImage = isImage(link);
        final boolean isVideo = isVideo(link);
        final Map<String, Object> media = (Map<String, Object>) art.get("media");
        try {
            final String baseUri = (String) media.get("baseUri");
            final String prettyName = (String) media.get("prettyName");
            final List<Map<String, Object>> types = (List<Map<String, Object>>) media.get("types");
            Map<String, Object> bestType = null;
            final List<String> bestTypesList;
            if (isImage) {
                bestTypesList = Arrays.asList(new String[] { "fullview" });
            } else if (isVideo) {
                bestTypesList = Arrays.asList(new String[] { "video" });
            } else {
                bestTypesList = new ArrayList<String>(0);
            }
            typeStringLoop: for (final String typeString : bestTypesList) {
                for (final Map<String, Object> type : types) {
                    if (typeString.equals(type.get("t"))) {
                        if (isImage) {
                            bestType = type;
                            break typeStringLoop;
                        } else if (isVideo) {
                            if (bestType == null || ((Number) type.get("h")).intValue() > ((Number) bestType.get("h")).intValue()) {
                                bestType = type;
                            }
                        }
                    }
                }
            }
            String displayedImageURL = null;
            Number unlimitedImageSize = null;
            String displayedVideoURL = null;
            Number displayedVideoSize = null;
            if (bestType != null) {
                if (isImage) {
                    String c = (String) bestType.get("c");
                    if (c == null) {
                        if ("fullview".equals(bestType.get("t"))) {
                            // r=1? o=true??(maybe original)
                            c = "";// raw image without any processing?
                        } else {
                            final Number h = (Number) bestType.get("h");
                            final Number w = (Number) bestType.get("w");
                            if (h != null && w != null) {
                                c = "/v1/fit/w_" + w + ",h_" + h + "/";
                            }
                        }
                    }
                    if (c != null) {
                        c = c.replaceFirst(",q_\\d+(,strp)?", "");
                        final List<String> tokens = (List<String>) media.get("token");
                        final String token = tokens.get(0);
                        link.setProperty(PROPERTY_IMAGE_TOKEN, token);
                        displayedImageURL = baseUri + c.replaceFirst("<prettyName>", Matcher.quoteReplacement(prettyName));
                        displayedImageURL = displayedImageURL + "?token=" + token;
                    }
                } else if (isVideo) {
                    displayedVideoURL = (String) bestType.get("b");
                    displayedVideoSize = (Number) bestType.get("f");
                }
            }
            if (isImage && displayedImageURL != null) {
                link.setProperty(PROPERTY_IMAGE_DISPLAY_OR_PREVIEW_URL, displayedImageURL);
            } else if (isVideo && displayedVideoURL != null) {
                link.setProperty(PROPERTY_VIDEO_DISPLAY_OR_PREVIEW_URL, displayedVideoURL);
            }
            link.setProperty("displayedImageURL", displayedImageURL);
            link.setProperty("unlimitedImageSize", unlimitedImageSize);
            link.setProperty("displayedVideoURL", displayedVideoURL);
            link.setProperty("displayedVideoSize", displayedVideoSize);
        } catch (Exception e) {
            plugin.getLogger().log(e);
        }
        final Object authorO = art.get("author");
        final Map<String, Object> author = authorO instanceof Map ? (Map<String, Object>) authorO : null;
        if (author != null) {
            link.setProperty(PROPERTY_USERNAME, author.get("username"));
        }
    }

    private static String setTitleProperty(final DownloadLink link, String title) {
        if (title == null) {
            return null;
        }
        /* Do some corrections */
        title = title.replaceAll("(?i) on deviantart$", "");
        link.setProperty(PROPERTY_TITLE, title);
        return title;
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account) throws Exception {
        this.downloadHTML = false;
        this.betterHTML = null;
        this.accountRequiredWhenDownloadImpossible = false;
        final String contenturl = link.getPluginPatternMatcher();
        if (!link.isNameSet()) {
            /* Set weak filename */
            link.setName(new URL(contenturl).getPath() + getAssumedFileExtension(link));
        }
        this.setBrowserExclusive();
        if (account != null) {
            login(account, false);
        }
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("/error\\-title\\-oops\\.png\\)")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.canHandle(br.getURL()) && !br.getURL().contains(getFID(link))) {
            /* Redirect to unsupported URL. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        this.handleConnectionErrors(br, br.getHttpConnection(), false);
        if (new Regex(contenturl, PATTERN_STASH).patternFind()) {
            /* Stash download */
            /* 2025-07-30: Looks like all stash downloads require the user to be logged in. */
            this.accountRequiredWhenDownloadImpossible = true;
            String title = br.getRegex("<title>([^<]+)</title>").getMatch(0);
            if (title != null) {
                title = Encoding.htmlDecode(title).trim();
                title = title.replaceFirst(" on DeviantArt$", "");
                link.setName(title);
            }
            String directurl = br.getRegex("\"(https?://[^/]+/download[^\"]+)").getMatch(0);
            if (directurl != null) {
                directurl = Encoding.htmlOnlyDecode(directurl);
                link.setProperty(PROPERTY_OFFICIAL_DOWNLOADURL, directurl);
            } else {
                logger.info("Failed to find directurl");
            }
        } else {
            /* Art download */
            String json = br.getRegex("window\\.__INITIAL_STATE__ = JSON\\.parse\\(\"(.*?)\"\\);").getMatch(0);
            if (json != null) {
                json = PluginJSonUtils.unescape(json);
                final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
                parseFileInformation(br, link, account, entries);
            } else {
                logger.warning("Failed to find json with extended information");
                parseFileInformation(br, link, account, null);
            }
        }
        return AvailableStatus.TRUE;
    }

    public void parseFileInformation(final Browser br, final DownloadLink link, final Account account, final Map<String, Object> entries) throws Exception {
        final boolean isDownload = this.getPluginEnvironment() == PluginEnvironment.DOWNLOAD;
        final String fid = getFID(link);
        String displayedImageURL = null;
        Number unlimitedImageSize = null;
        Number displayedVideoSize = null;
        String officialDownloadurl = null;
        Boolean officialDownloadPossible = null;
        Number officialDownloadsizeBytes = null;
        List<String> blockReasons = new ArrayList<String>();
        Map<String, Object> premiumFolderData = null;
        jsonHandling: if (entries != null) {
            final Map<String, Object> entities = (Map<String, Object>) entries.get("@@entities");
            final Map<String, Object> user = (Map<String, Object>) entities.get("user");
            final Map<String, Object> deviation = (Map<String, Object>) entities.get("deviation");
            Map<String, Object> thisArt = (Map<String, Object>) deviation.get(fid);
            String alternativeDeviationID = null;
            if (thisArt == null) {
                /* E.g. https://www.deviantart.com/shinysmeargle/status-update/12312835 */
                final Iterator<Entry<String, Object>> iterator = deviation.entrySet().iterator();
                iteratorLoop: while (iterator.hasNext()) {
                    final Entry<String, Object> entry = iterator.next();
                    final String key = entry.getKey();
                    if (key.matches("^\\d+-" + fid + "$")) {
                        alternativeDeviationID = key;
                        thisArt = (Map<String, Object>) entry.getValue();
                        break iteratorLoop;
                    }
                }
            }
            officialDownloadPossible = (Boolean) thisArt.get("isDownloadable");
            blockReasons = (List<String>) thisArt.get("blockReasons");
            parseDeviationJSON(this, link, thisArt);
            displayedImageURL = link.getStringProperty("displayedImageURL");
            unlimitedImageSize = (Number) link.getProperty("unlimitedImageSize");
            displayedVideoSize = (Number) link.getProperty("displayedVideoSize");
            final Map<String, Object> thisUser = (Map<String, Object>) user.get(thisArt.get("author").toString());
            link.setProperty(PROPERTY_USERNAME, thisUser.get("username"));
            premiumFolderData = (Map<String, Object>) thisArt.get("premiumFolderData");
            final Map<String, Object> deviationExtended = (Map<String, Object>) entities.get("deviationExtended");
            if (deviationExtended == null) {
                logger.warning("Failed to find 'deviationExtended'");
                break jsonHandling;
            }
            Map<String, Object> deviationExtendedThisArt = (Map<String, Object>) deviationExtended.get(fid);
            if (deviationExtendedThisArt == null && alternativeDeviationID != null) {
                /*
                 * PATTERN_STATUS might be listed with key <someNumbers>-<fid> also typically deviationExtended will only contain one item.
                 */
                deviationExtendedThisArt = (Map<String, Object>) deviationExtended.get(alternativeDeviationID);
            }
            if (deviationExtendedThisArt == null) {
                logger.info("Failed to find 'deviationExtendedThisArt'");
                break jsonHandling;
            }
            final Map<String, Object> download = (Map<String, Object>) deviationExtendedThisArt.get("download");
            final Map<String, Object> originalFile = (Map<String, Object>) deviationExtendedThisArt.get("originalFile");
            if (download != null) {
                officialDownloadurl = download.get("url").toString();
                officialDownloadsizeBytes = (Number) download.get("filesize");
                officialDownloadPossible = Boolean.TRUE;
            } else if (originalFile != null) {
                officialDownloadsizeBytes = (Number) originalFile.get("filesize");
                // originalFileExt = originalFile.get("type").toString();
            }
            final Map<String, Object> descriptionText = (Map<String, Object>) deviationExtendedThisArt.get("descriptionText");
            if (descriptionText != null) {
                final String rawText = (String) descriptionText.get("excerpt");
                if (!StringUtils.isEmpty(rawText) && StringUtils.isEmpty(link.getComment())) {
                    link.setComment(rawText);
                }
            }
        }
        /* Fallbacks via website-html */
        if (!link.hasProperty(PROPERTY_TITLE)) {
            String titleFromHTML = HTMLSearch.searchMetaTag(br, "og:title");
            if (titleFromHTML != null) {
                titleFromHTML = Encoding.htmlDecode(titleFromHTML).trim();
                setTitleProperty(link, titleFromHTML);
            }
        }
        if (StringUtils.isEmpty(displayedImageURL) && isImage(link)) {
            displayedImageURL = HTMLSearch.searchMetaTag(br, "og:image");
            if (displayedImageURL != null) {
                displayedImageURL = Encoding.htmlOnlyDecode(displayedImageURL);
                link.setProperty(PROPERTY_IMAGE_DISPLAY_OR_PREVIEW_URL, displayedImageURL);
            }
        }
        final String officialDownloadFilesizeStr = br.getRegex(">\\s*Image size\\s*</div><div[^>]*>\\d+x\\d+px\\s*(\\d+[^>]+)</div>").getMatch(0);
        // final boolean accountNeededForOfficialDownload = br.containsHTML("Log in to download");
        if (StringUtils.isEmpty(officialDownloadurl) && officialDownloadPossible == null) {
            officialDownloadurl = br.getRegex("data-hook=\"download_button\"[^>]*href=\"(https?://[^\"]+)").getMatch(0);
            if (officialDownloadurl != null) {
                officialDownloadPossible = Boolean.TRUE;
                officialDownloadurl = Encoding.htmlOnlyDecode(officialDownloadurl);
            }
        }
        if (Boolean.TRUE.equals(officialDownloadPossible)) {
            link.setProperty(PROPERTY_OFFICIAL_DOWNLOADURL, officialDownloadurl);
        } else {
            link.removeProperty(PROPERTY_OFFICIAL_DOWNLOADURL);
        }
        final int imagePosition = getImagePosition(link);
        // final int imagePositionMax = getImagePositionMax(link);
        final boolean isImage = isImage(link);
        final boolean isVideo = isVideo(link);
        final boolean isLiterature = isLiterature(link);
        final boolean isStatus = isStatus(link);
        final DeviantArtComConfig cfg = PluginJsonConfig.get(DeviantArtComConfig.class);
        final ImageDownloadMode mode = cfg.getImageDownloadMode();
        String forcedExt = null;
        String dllink = null;
        try {
            dllink = getDirecturl(br, link, account);
        } catch (final PluginException e) {
            /**
             * This will happen if the item is not downloadable. </br>
             * We're ignoring this during linkcheck as by now we know the file is online.
             */
            if (isDownload) {
                throw e;
            }
            logger.log(e);
        }
        boolean allowGrabFilesizeFromHeader = false;
        /* Check if either user wants to download the html code or if we have a linktype which needs this. */
        if (new Regex(link.getPluginPatternMatcher(), PATTERN_JOURNAL).patternFind() || isLiterature || isStatus || isStatus(link)) {
            /* E.g. journal: https://www.deviantart.com/janny654/art/Nora-the-Goblin-s-Pony-chapter-1-824882173 */
            downloadHTML = true;
            forcedExt = ".html";
        } else if (isImage && mode == ImageDownloadMode.HTML) {
            /* HTML download */
            downloadHTML = true;
            forcedExt = ".html";
        } else if (br.containsHTML(TYPE_DOWNLOADALLOWED_HTML) || br.containsHTML(TYPE_DOWNLOADFORBIDDEN_HTML)) {
            downloadHTML = true;
            forcedExt = ".html";
        } else if (isVideo) {
            /* Video download */
            if (displayedVideoSize != null) {
                link.setVerifiedFileSize(displayedVideoSize.longValue());
            }
        } else if (isImage) {
            /* Image download */
            if (imagePosition > 1) {
                /* Image is part of multi image gallery -> File size is never available via website or API. */
                allowGrabFilesizeFromHeader = true;
            } else if (StringUtils.equalsIgnoreCase(dllink, officialDownloadurl)) {
                /* Official download -> Set official download filesize if possible */
                if (officialDownloadsizeBytes != null) {
                    link.setVerifiedFileSize(officialDownloadsizeBytes.longValue());
                } else if (officialDownloadFilesizeStr != null) {
                    link.setDownloadSize(SizeFormatter.getSize(officialDownloadFilesizeStr.replace(",", "")));
                }
            } else {
                /* Preview image download */
                if (unlimitedImageSize != null && dllink != null && !dllink.matches("(?i).*/v1/.+")) {
                    link.setVerifiedFileSize(unlimitedImageSize.longValue());
                } else if (officialDownloadsizeBytes != null) {
                    link.setDownloadSize(officialDownloadsizeBytes.longValue());
                } else if (officialDownloadFilesizeStr != null) {
                    link.setDownloadSize(SizeFormatter.getSize(officialDownloadFilesizeStr.replace(",", "")));
                } else {
                    allowGrabFilesizeFromHeader = true;
                }
            }
        } else {
            logger.warning("Got unknown media type");
        }
        this.setFilename(link, account, forcedExt);
        if (dllink == null || isBlurredImageLink(dllink)) {
            try {
                if ("locked".equalsIgnoreCase(link.getStringProperty(PROPERTY_TIER_ACCESS))) {
                    /* Paid content. All we could download would be a blurred image of the content. */
                    /* Example: https://www.deviantart.com/ohshinakai/art/Stretched-to-the-limit-Shanoli-996105058 */
                    throw new AccountRequiredException("Paid content");
                } else if (premiumFolderData != null && Boolean.FALSE.equals(premiumFolderData.get("hasAccess"))) {
                    final String premiumType = (String) premiumFolderData.get("type");
                    if (StringUtils.equalsIgnoreCase(premiumType, "watchers")) {
                        throw new AccountRequiredException("Item is only accessible for followers of this artist");
                    } else if (blockReasons != null && blockReasons.size() > 0) {
                        throw new AccountRequiredException("Paid content and blocked for reasons: " + getCommaSeparatedHumanReadableBlockedReasonString(blockReasons));
                    } else {
                        throw new AccountRequiredException("Paid content");
                    }
                } else if (blockReasons != null && !blockReasons.isEmpty()) {
                    /* Mature content (account required) or blocked for other reasons. */
                    /* Examples for block reasons we can always circumvent: mature_filter */
                    this.accountRequiredWhenDownloadImpossible = true;
                    if (dllink == null && officialDownloadurl == null) {
                        throw new AccountRequiredException("Item undownloadable for reasons: " + getCommaSeparatedHumanReadableBlockedReasonString(blockReasons));
                    }
                    /*
                     * Item is blocked due to mature content (= the only blocked reason here) but that limitation can be skipped as image
                     * does not seem to be blurred.
                     */
                    final List<String> remainingReasons = new ArrayList<String>(blockReasons);
                    remainingReasons.remove("mature_filter");
                    remainingReasons.remove("mature_loggedout");
                    if (remainingReasons.isEmpty() && !isBlurredImageLink(dllink)) {
                        return;
                    } else {
                        throw new AccountRequiredException("Item undownloadable for reasons: " + getCommaSeparatedHumanReadableBlockedReasonString(blockReasons));
                    }
                }
            } catch (final AccountRequiredException ar) {
                if (isDownload) {
                    throw ar;
                } else {
                    return;
                }
            }
        }
        /* Set file size if possible. */
        if (downloadHTML) {
            link.setVerifiedFileSize(-1);
            try {
                final String deviationHTML = link.getStringProperty(PROPERTY_DEVIATION_HTML);
                if (deviationHTML != null) {
                    this.betterHTML = deviationHTML;
                    link.setDownloadSize(deviationHTML.getBytes("UTF-8").length);
                } else {
                    link.setDownloadSize(br.getRequest().getHtmlCode().getBytes("UTF-8").length);
                }
            } catch (final UnsupportedEncodingException ignore) {
                ignore.printStackTrace();
            }
        } else if (allowGrabFilesizeFromHeader && !link.isSizeSet() && !cfg.isFastLinkcheckForSingleItems() && !isDownload && !StringUtils.isEmpty(dllink) && !isBlurredImageLink(dllink)) {
            /* No filesize value given -> Obtain from header */
            final Browser br2 = br.cloneBrowser();
            /* Workaround for old downloadcore bug that can lead to incomplete files */
            br2.getHeaders().put("Accept-Encoding", "identity");
            URLConnectionAdapter con = null;
            try {
                con = br2.openHeadConnection(dllink);
                handleConnectionErrors(br2, con, true);
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
    }

    private String getCommaSeparatedHumanReadableBlockedReasonString(final List<String> blockedReasons) {
        if (blockedReasons == null || blockedReasons.isEmpty()) {
            throw new IllegalArgumentException();
        }
        final StringBuilder sb = new StringBuilder();
        for (final String blockedReason : blockedReasons) {
            if (sb.length() > 0) {
                sb.append(",");
            }
            sb.append(getHumanReadableBlockedReason(blockedReason));
        }
        return sb.toString();
    }

    /**
     * Returns human readable error message to the provided blocked/error key. <br>
     * If no human readable string is known, the key will be returned instead.
     */
    private String getHumanReadableBlockedReason(final String blockedReasonKey) {
        if (blockedReasonKey == null) {
            throw new IllegalArgumentException();
        }
        if (blockedReasonKey.equalsIgnoreCase("mature_filter")) {
            return "Mature content: This deviation has been labeled as containing themes not suitable for all deviants.";
        } else if (blockedReasonKey.equalsIgnoreCase("mature_loggedout")) {
            return "You need to login";
        } else {
            return blockedReasonKey;
        }
    }

    public void setFilename(final DownloadLink link, final Account account, final String forcedExt) throws Exception {
        final String fid = this.getFID(link);
        String dllink = null;
        try {
            dllink = getDirecturl(br, link, account);
        } catch (final PluginException e) {
            /**
             * This will happen if the item is not downloadable. </br>
             * We're ignoring this here.
             */
            logger.log(e);
        }
        final int imagePosition = getImagePosition(link);
        final int imagePositionMax = getImagePositionMax(link);
        final String extByURL = dllink != null ? Plugin.getFileNameExtensionFromURL(dllink) : null;
        final String ext;
        if (forcedExt != null) {
            /* Forced ext has highest priority */
            ext = forcedExt;
        } else if (extByURL != null) {
            ext = extByURL;
        } else {
            ext = getAssumedFileExtension(link);
        }
        final String title = link.getStringProperty(PROPERTY_TITLE);
        String filename = null;
        final String username = link.getStringProperty(PROPERTY_USERNAME);
        if (title != null) {
            filename = title + " by " + username + "_ " + fid;
            if (imagePositionMax > 1) {
                /* Image is part of multi image gallery -> Make sure each item gets a different filename. */
                filename += "_" + imagePosition;
            }
        } else if (isStatus(link)) {
            /* A status typically doesn't have a title so we goota create our own. */
            filename = fid + " by " + username;
        }
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            if (ext != null) {
                filename = this.applyFilenameExtension(filename, ext);
            }
            link.setFinalFileName(filename);
        } else if (!StringUtils.isEmpty(dllink)) {
            /* Last resort fallback */
            final String filenameFromURL = Plugin.getFileNameFromURL(new URL(dllink));
            if (filenameFromURL != null) {
                link.setName(filenameFromURL);
            }
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        requestFileInformation(link, account);
        /* We never know what we get -> Remove verified filesize before download start is attempted. */
        link.setVerifiedFileSize(-1);
        if (this.downloadHTML) {
            /* Write text to file */
            final String text;
            if (this.betterHTML != null) {
                text = this.betterHTML;
            } else {
                text = br.getRequest().getHtmlCode();
            }
            dl = new TextDownloader(this, link, text);
            dl.startDownload();
        } else {
            /* Download file */
            final String dllink = getDirecturl(br, link, account);
            if (StringUtils.isEmpty(dllink)) {
                if (this.looksLikeAccountRequired(br)) {
                    throw new AccountRequiredException();
                } else if (this.accountRequiredWhenDownloadImpossible) {
                    throw new AccountRequiredException();
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            } else if (isBlurredImageLink(dllink)) {
                /**
                 * Last resort errorhandling for "probably premium-only items". </br>
                 * This should usually be catched before.
                 */
                throw new PluginException(LinkStatus.ERROR_FATAL, "Avoiding download of blurred image");
            }
            /* Workaround for old download core bug that can lead to incomplete files */
            br.getHeaders().put("Accept-Encoding", "identity");
            /* Remove hashInfo before download in case quality/mirror/user settings have changed. will be updated again on download */
            link.setHashInfo(null);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, isResumeable(link, account), 1);
            handleConnectionErrors(br, dl.getConnection(), true);
            /* Add a download slot */
            controlMaxFreeDownloads(account, link, +1);
            try {
                /* Start download */
                dl.startDownload();
            } finally {
                /* Remove download slot */
                controlMaxFreeDownloads(account, link, -1);
            }
        }
    }

    protected void controlMaxFreeDownloads(final Account account, final DownloadLink link, final int num) {
        if (account != null) {
            synchronized (accountDownloadsRunning) {
                final int before = accountDownloadsRunning.get();
                final int after = before + num;
                accountDownloadsRunning.set(after);
                logger.info("accountDownloadsRunning(" + link.getName() + ")|max:" + getMaxSimultanPremiumDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
            }
        } else {
            synchronized (freeDownloadsRunning) {
                final int before = freeDownloadsRunning.get();
                final int after = before + num;
                freeDownloadsRunning.set(after);
                logger.info("freeDownloadsRunning(" + link.getName() + ")|max:" + getMaxSimultanFreeDownloadNum() + "|before:" + before + "|after:" + after + "|num:" + num);
            }
        }
    }

    @Override
    protected boolean looksLikeDownloadableContent(final URLConnectionAdapter con) {
        if (super.looksLikeDownloadableContent(con)) {
            return true;
        } else if (downloadHTML && con.getContentType().contains("html")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        // final int max = 100;
        final int running = freeDownloadsRunning.get();
        // final int ret = Math.min(running + 1, max);
        // return ret;
        return running + 1;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        // final int max = 100;
        final int running = accountDownloadsRunning.get();
        // final int ret = Math.min(running + 1, max);
        // return ret;
        return running + 1;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        this.handleDownload(link, account);
    }

    private void handleConnectionErrors(final Browser br, final URLConnectionAdapter con, final boolean checkForDownloadableContent) throws PluginException, IOException {
        if (checkForDownloadableContent && !this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            if (con.getResponseCode() == 401) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 401", 10 * 60 * 1000l);
            } else if (con.getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 10 * 60 * 1000l);
            } else if (con.getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 1 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Media broken?");
            }
        } else if (con.getResponseCode() == 401) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 401", 10 * 60 * 1000l);
        } else if (con.getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 10 * 60 * 1000l);
        } else if (con.getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 1 * 60 * 1000l);
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        account.setType(AccountType.FREE);
        /**
         * Try to get unique username even if users use cookie login as they can theoretically enter whatever they want into username field.
         */
        final Cookies userCookies = account.loadUserCookies();
        String realUsername = getUsernameFromCookies(br);
        if (userCookies != null && !userCookies.isEmpty()) {
            if (!StringUtils.isEmpty(realUsername)) {
                account.setUser(realUsername);
            } else {
                logger.warning("Failed to find real username inside cookies");
            }
        }
        return ai;
    }

    public void login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            final Cookies userCookies = account.loadUserCookies();
            if (userCookies == null) {
                showCookieLoginInfo();
                throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_required());
            }
            br.setCookies(this.getHost(), userCookies);
            if (!force) {
                /* Do not validate cookies */
                return;
            }
            br.getPage("https://www." + this.getHost());
            if (!this.isLoggedIN(br)) {
                logger.info("User cookie login failed");
                if (account.hasEverBeenValid()) {
                    throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                } else {
                    throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                }
            }
            logger.info("User cookie login successful");
        }
    }

    private boolean isLoggedIN(final Browser br) {
        final String username = getUsernameFromCookies(br);
        if (!StringUtils.isEmpty(username) && br.containsHTML("data-hook=\"user_link\" data-username=\"" + Pattern.quote(username))) {
            return true;
        } else if (br.containsHTML("/users/logout")) {
            return true;
        } else {
            return false;
        }
    }

    private String getUsernameFromCookies(final Browser br) {
        String userinfoCookie = br.getCookie(br.getHost(), "userinfo", Cookies.NOTDELETEDPATTERN);
        if (userinfoCookie != null) {
            userinfoCookie = Encoding.htmlDecode(userinfoCookie);
            return PluginJSonUtils.getJson(userinfoCookie, "username");
        }
        return null;
    }

    public static boolean isStatus(final DownloadLink link) {
        if (StringUtils.equalsIgnoreCase(link.getStringProperty(PROPERTY_TYPE), "status") || new Regex(link.getPluginPatternMatcher(), PATTERN_STATUS).patternFind()) {
            return true;
        } else {
            return false;
        }
    }

    public static int getImagePosition(final DownloadLink link) {
        return link.getIntegerProperty(PROPERTY_IMAGE_POSITION, -1);
    }

    public static int getImagePositionMax(final DownloadLink link) {
        return link.getIntegerProperty(PROPERTY_IMAGE_POSITION_MAX, -1);
    }

    public static boolean isImage(DownloadLink link) {
        return StringUtils.equalsIgnoreCase(link.getStringProperty(PROPERTY_TYPE), "image");
    }

    public static boolean isVideo(DownloadLink link) {
        return StringUtils.equalsIgnoreCase(link.getStringProperty(PROPERTY_TYPE), "film");
    }

    public static boolean isLiterature(DownloadLink link) {
        return StringUtils.equalsIgnoreCase(link.getStringProperty(PROPERTY_TYPE), "literature");
    }

    private static boolean isBlurredImageLink(final String url) {
        if (StringUtils.containsIgnoreCase(url, ",blur_")) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Returns assumed file extension based on all information we currently have. Use this only for weak filenames e.g. before linkcheck is
     * done.
     */
    public static String getAssumedFileExtension(final DownloadLink link) {
        if (isVideo(link)) {
            return ".mp4";
        } else if (isImage(link)) {
            // TODO: add isGif support
            if (PluginJsonConfig.get(DeviantArtComConfig.class).getImageDownloadMode() == ImageDownloadMode.HTML) {
                return ".html";
            } else {
                return ".jpg";
            }
        } else if (isLiterature(link) || isStatus(link)) {
            /* TODO: Add proper handling to only download relevant text of this type of link and write it into .txt file. */
            return ".html";
            // return ".txt";
        } else {
            return ".html";
        }
    }

    private static boolean isAccountRequiredForOfficialDownload(final Browser br) {
        if (br.containsHTML("aria-label=\"Log in to download\"")) {
            return true;
        } else {
            return false;
        }
    }

    private boolean looksLikeAccountRequired(final Browser br) {
        if (looksLikeAccountRequiredUploaderDecision(br) || looksLikeAccountRequiredMatureContent(br)) {
            return true;
        } else {
            return false;
        }
    }

    private boolean looksLikeAccountRequiredUploaderDecision(final Browser br) {
        if (br.containsHTML("has limited the viewing of this artwork\\s*<")) {
            return true;
        } else {
            return false;
        }
    }

    private boolean looksLikeAccountRequiredMatureContent(final Browser br) {
        if (br.containsHTML(">\\s*This content is intended for mature audiences")) {
            return true;
        } else {
            return false;
        }
    }

    private static String getDirecturl(final Browser br, final DownloadLink link, final Account account) throws PluginException {
        String dllink = null;
        final String officialDownloadurl = link.getStringProperty(PROPERTY_OFFICIAL_DOWNLOADURL);
        final String multiImageGalleryPreviewUrl = getMultiImageGalleryPreviewUrl(link);
        final String imagePreviewUrl = link.getStringProperty(PROPERTY_IMAGE_DISPLAY_OR_PREVIEW_URL);
        if (isVideo(link)) {
            /* officialDownloadurl can be given while account is not given -> Will lead to error 404 then! */
            dllink = link.getStringProperty(PROPERTY_VIDEO_DISPLAY_OR_PREVIEW_URL);
        } else if (isImage(link)) {
            /* officialDownloadurl can be given while account is not given -> Will lead to error 404 then! */
            final int imagePosition = getImagePosition(link);
            if (imagePosition > 1) {
                /*
                 * Multi image gallery download -> 2nd image and all after that are never 'officially' downloadable -> Always return
                 * imagePreviewUrl.
                 */
                dllink = multiImageGalleryPreviewUrl;
            } else {
                if (PluginJsonConfig.get(DeviantArtComConfig.class).getImageDownloadMode() == ImageDownloadMode.OFFICIAL_DOWNLOAD_ONLY) {
                    /**
                     * User only wants to download only items with official download option available -> Check if that is possible for this
                     * item. <br>
                     * Important: Only check this for single items or for the first item of multi image items.
                     */
                    if (LOGIN_ALWAYS_REQUIRED_FOR_OFFICIAL_DOWNLOAD && account == null) {
                        /* Account is required to be able to use official download option. */
                        throw new AccountRequiredException("Account required for official download. Add account or change plugin settings to allow preview image download.");
                    } else if (isAccountRequiredForOfficialDownload(br)) {
                        /* Looks like official download is not available at all for this item */
                        throw new PluginException(LinkStatus.ERROR_FATAL, "Official download not available. Add account or change plugin settings to allow preview image download.");
                    } else if (officialDownloadurl == null) {
                        throw new PluginException(LinkStatus.ERROR_FATAL, "Official download not available. Change plugin settings to allow preview image download.");
                    }
                }
                if (officialDownloadurl != null && ((LOGIN_ALWAYS_REQUIRED_FOR_OFFICIAL_DOWNLOAD && account != null) || !LOGIN_ALWAYS_REQUIRED_FOR_OFFICIAL_DOWNLOAD)) {
                    dllink = officialDownloadurl;
                } else if (imagePreviewUrl != null) {
                    dllink = imagePreviewUrl;
                } else if (multiImageGalleryPreviewUrl != null) {
                    dllink = multiImageGalleryPreviewUrl;
                }
            }
        } else if (officialDownloadurl != null) {
            /* Non media item e.g. pdf file download */
            dllink = officialDownloadurl;
        }
        return dllink;
    }

    private static String getMultiImageGalleryPreviewUrl(final DownloadLink link) {
        String url = link.getStringProperty(PROPERTY_IMAGE_DISPLAY_OR_PREVIEW_URL_2);
        if (url == null) {
            return null;
        }
        final int imagePosition = getImagePosition(link);
        if (imagePosition > 1) {
            /**
             * Change resolution from thumbnail to fullsize and remove other unneeded stuff. This will also unblur blurred images (lol).
             * <br>
             * Removes everything until "?token=..." <br>
             * Important: This does not work for the first image, only for the images >= 2!
             */
            /*
             * TODO: 2025-04-25: Remove this if we are sure that the new handling works fine, see DeviantArtComCrawler -> Search for
             * "prettyName".
             */
            url = url.replaceAll("(/v1/fit/.*150\\.jpg)", "");
        }
        /**
         * 2025-04-22: Disabled this as it seems like this doesn't work anymore. <br>
         * In case those direct URLs expire, we probably need to use the crawler to refresh them. <br>
         * TODO: Remove this after 2025/09
         */
        final boolean refreshTokenInURL = false;
        if (refreshTokenInURL) {
            final String token = link.getStringProperty(PROPERTY_IMAGE_TOKEN);
            if (token != null) {
                /* Put fresh token into existing URL to ensure that the URL will be valid. */
                url = url.replaceAll("\\?token=.+", "?token=" + token);
            }
        }
        link.setComment(url);
        return url;
    }

    @Override
    public Class<? extends DeviantArtComConfig> getConfigInterface() {
        return DeviantArtComConfig.class;
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}