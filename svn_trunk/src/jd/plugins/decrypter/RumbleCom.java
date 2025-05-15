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
package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.controlling.linkcrawler.ArchiveInfo;
import jd.controlling.linkcrawler.CrawledLink;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.GenericM3u8;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.extensions.extraction.BooleanStatus;
import org.jdownloader.plugins.components.config.RumbleComConfig;
import org.jdownloader.plugins.components.config.RumbleComConfig.Quality;
import org.jdownloader.plugins.components.config.RumbleComConfig.QualitySelectionMode;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@DecrypterPlugin(revision = "$Revision: 51066 $", interfaceVersion = 3, names = {}, urls = {})
public class RumbleCom extends PluginForDecrypt {
    public RumbleCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "rumble.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:[^/]+\\.html|embedJS/[a-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    private static final String                         TYPE_EMBED        = "(?i)https?://[^/]+/embedJS/([a-z0-9]+)";
    private static final String                         TYPE_NORMAL       = "(?i)https?://[^/]+/([^/]+)\\.html";
    private final String                                PROPERTY_USERNAME = "username";
    private final String                                PROPERTY_TITLE    = "title";
    private final String                                PROPERTY_format   = "stream_format";
    private final String                                PROPERTY_WIDTH    = "width";
    private final String                                PROPERTY_HEIGHT   = "height";
    private final String                                PROPERTY_DATE     = "date";

    final Map<String, DownloadLink>                     bestMap           = new HashMap<String, DownloadLink>();
    final Map<String, DownloadLink>                     worstMap          = new HashMap<String, DownloadLink>();
    final Map<String, Map<Integer, List<DownloadLink>>> crawledQualities  = new HashMap<String, Map<Integer, List<DownloadLink>>>();

    @Override
    public CrawledLink convert(DownloadLink link) {
        final CrawledLink ret = super.convert(link);
        if ("tar".equals(link.getStringProperty(PROPERTY_format))) {
            final ArchiveInfo archiveInfo = new ArchiveInfo();
            archiveInfo.setAutoExtract(BooleanStatus.FALSE);
            ret.setArchiveInfo(archiveInfo);
        }
        return ret;
    }

    @Override
    public void clean() {
        super.clean();
        bestMap.clear();
        worstMap.clear();
        crawledQualities.clear();
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String videoID;
        if (param.getCryptedUrl().matches(TYPE_EMBED)) {
            videoID = new Regex(param.getCryptedUrl(), TYPE_EMBED).getMatch(0);
        } else {
            br.getPage(param.getCryptedUrl());
            br.followRedirect();
            videoID = br.getRegex("\"video\":\"([a-z0-9]+)\"").getMatch(0);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (videoID == null) {
                logger.info("Failed to find any downloadable content");
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        }
        final RumbleComConfig cfg = PluginJsonConfig.get(getConfigInterface());
        br.getPage("https://" + this.getHost() + "/embedJS/u3/?request=video&ver=2&v=" + videoID + "&ext=%7B%22ad_count%22%3Anull%7D&ad_wt=0");
        /* Double-check for offline content */
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> root = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        String dateFormatted = null;
        final String dateStr = (String) root.get("pubDate");
        if (!StringUtils.isEmpty(dateStr)) {
            dateFormatted = new Regex(dateStr, "^(\\d{4}-\\d{2}-\\d{2})").getMatch(0);
        }
        final String uploaderName = (String) JavaScriptEngineFactory.walkJson(root, "author/name");
        String title = Encoding.htmlDecode(root.get("title").toString()).trim();
        String baseTitle = Encoding.htmlDecode(title);
        if (StringUtils.isEmpty(baseTitle)) {
            /* Fallback */
            baseTitle = videoID;
        }
        if (!StringUtils.isEmpty(uploaderName)) {
            baseTitle = uploaderName + " - " + baseTitle;
        }
        if (dateFormatted != null) {
            baseTitle = dateFormatted + "_" + baseTitle;
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(baseTitle);
        final Map<String, Object> root_ua = (Map<String, Object>) root.get("ua");
        final Iterator<Entry<String, Object>> streamingTypeIterator = root_ua.entrySet().iterator();
        while (streamingTypeIterator.hasNext()) {
            final Entry<String, Object> entry = streamingTypeIterator.next();
            final String format = entry.getKey();
            if (!"mp4".equals(format) && !"tar".equals(format) && !"webm".equals(format)) {
                continue;
            }
            final List<Map<String, Object>> qualityInfoArray;
            if (entry.getValue() instanceof Map) {
                qualityInfoArray = new ArrayList<Map<String, Object>>();
                final Map<String, Object> entryMap = (Map<String, Object>) entry.getValue();
                if (entryMap.containsKey("meta")) {
                    qualityInfoArray.add(entryMap);
                } else {
                    final Map<String, Map<String, Object>> entryMapMap = (Map<String, Map<String, Object>>) entry.getValue();
                    qualityInfoArray.addAll(entryMapMap.values());
                }
            } else {
                qualityInfoArray = (List<Map<String, Object>>) entry.getValue();
            }
            for (final Map<String, Object> qualityInfo : qualityInfoArray) {
                addQuality(param, fp, format, uploaderName, title, baseTitle, dateFormatted, root, qualityInfo);
                if ("tar".equals(format)) {
                    addQuality(param, fp, "hls", uploaderName, title, baseTitle, dateFormatted, root, qualityInfo);
                }
            }
        }
        final List<DownloadLink> selectedQuality = new ArrayList<DownloadLink>();
        final List<String> sortedPreferredFormats = new ArrayList<String>(Arrays.asList("mp4", "hls", "webm", "tar"));
        final String preferredFormat;
        switch (cfg.getPreferredFormat()) {
        case TAR:
            preferredFormat = "tar";
            break;
        case MP4:
            preferredFormat = "mp4";
            break;
        case HLS:
            preferredFormat = "hls";
            break;
        case WEBM:
            preferredFormat = "webm";
            break;
        default:
            preferredFormat = null;
            break;
        }
        if (preferredFormat != null) {
            sortedPreferredFormats.remove(preferredFormat);
            sortedPreferredFormats.add(0, preferredFormat);
        }
        final int preferredHeight = getUserPreferredqualityHeight();
        for (final String checkpreferredFormat : sortedPreferredFormats) {
            if (crawledQualities.containsKey(checkpreferredFormat)) {
                for (Entry<Integer, List<DownloadLink>> qualities : crawledQualities.get(checkpreferredFormat).entrySet()) {
                    if (preferredHeight == qualities.getKey().intValue()) {
                        selectedQuality.addAll(qualities.getValue());
                    }
                    ret.addAll(qualities.getValue());
                }
                if (checkpreferredFormat.equals(preferredFormat)) {
                    break;
                }
            }
        }
        final QualitySelectionMode mode = cfg.getQualitySelectionMode();
        if (mode == QualitySelectionMode.WORST && (preferredFormat == null || worstMap.containsKey(preferredFormat))) {
            ret.clear();
            if (preferredFormat == null) {
                ret.addAll(bestMap.values());
            } else {
                ret.add(worstMap.get(preferredFormat));
            }
            return ret;
        }
        if (mode == QualitySelectionMode.BEST && (preferredFormat == null || bestMap.containsKey(preferredFormat))) {
            ret.clear();
            if (preferredFormat == null) {
                ret.addAll(bestMap.values());
            } else {
                ret.add(bestMap.get(preferredFormat));
            }
            return ret;
        }
        if (mode == QualitySelectionMode.SELECTED_ONLY && selectedQuality.size() > 0) {
            /* Same resolution can exist multiple times with different bitrates -> Prefer last -> Best */
            ret.clear();
            /* TODO: only return best */
            ret.addAll(selectedQuality);
            return ret;
        }
        /* Return all if wanted by user and also as fallback. */
        return ret;
    }

    private DownloadLink addQuality(CryptedLink param, FilePackage fp, String format, String uploaderName, final String title, String baseTitle, final String dateFormatted, Map<String, Object> root, Map<String, Object> qualityInfo) throws Exception {
        final Map<String, Object> qualityInfoMeta = (Map<String, Object>) qualityInfo.get("meta");
        String url = qualityInfo.get("url").toString();
        final Number filesizeN = (Number) qualityInfoMeta.get("size");
        final Number thisQualityWidth = (Number) qualityInfoMeta.get("w");
        final Number thisQualityHeight = (Number) qualityInfoMeta.get("h");
        final int height;
        final int width;
        if (thisQualityWidth != null && thisQualityHeight != null) {
            height = thisQualityHeight.intValue();
            width = thisQualityWidth.intValue();
        } else {
            final int generalWidth = ((Number) root.get("w")).intValue();
            final int generalHeight = ((Number) root.get("h")).intValue();
            /* Fallback: Rare case: For unplayable videos or videos with only one quality. */
            height = generalHeight;
            width = generalWidth;
        }
        final String fileFormat;
        if ("webm".equals(format)) {
            fileFormat = ".webm";
        } else if ("mp4".equals(format)) {
            fileFormat = ".mp4";
        } else if ("tar".equals(format)) {
            url = org.appwork.utils.net.URLHelper.getUrlWithoutParams(url);
            fileFormat = ".tar";
        } else if ("hls".equals(format)) {
            url = GenericM3u8.createURLForThisPlugin(url);
            fileFormat = "_hls.mp4";
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final DownloadLink dl = this.createDownloadlink(url);
        /* Set this so when user copies URL of any video quality he'll get the URL to the main video. */
        dl.setContentUrl(param.getCryptedUrl());
        dl.setForcedFileName(baseTitle + "_" + height + fileFormat);
        if ("hls".equals(format)) {
            dl.setProperty(GenericM3u8.PRESET_NAME_PROPERTY, dl.getFinalFileName());
        }
        if (filesizeN != null) {
            final long filesize = filesizeN.longValue();
            dl.setDownloadSize(filesize);
        }
        /* Do not set as available to allow generic HLS crawler to find all qualities */
        // dl.setAvailable(true);
        dl._setFilePackage(fp);
        if (uploaderName != null) {
            dl.setProperty(PROPERTY_USERNAME, uploaderName);
        }
        dl.setProperty(PROPERTY_TITLE, title);
        dl.setProperty(PROPERTY_WIDTH, width);
        dl.setProperty(PROPERTY_HEIGHT, height);
        dl.setProperty(PROPERTY_DATE, dateFormatted);
        dl.setProperty(PROPERTY_format, format);
        dl.setAvailable(true);
        final DownloadLink best = bestMap.get(format);
        if (best == null) {
            bestMap.put(format, dl);
        } else {
            if (filesizeN != null && filesizeN.longValue() > best.getKnownDownloadSize()) {
                bestMap.put(format, dl);
            } else if (filesizeN == null && height > best.getIntegerProperty(PROPERTY_HEIGHT, -1)) {
                bestMap.put(format, dl);
            }
        }
        final DownloadLink worst = worstMap.get(format);
        if (worst == null || height < worst.getIntegerProperty(PROPERTY_HEIGHT, -1)) {
            worstMap.put(format, dl);
        }
        Map<Integer, List<DownloadLink>> qualitiesMap = crawledQualities.get(format);
        if (qualitiesMap == null) {
            qualitiesMap = new HashMap<Integer, List<DownloadLink>>();
            crawledQualities.put(format, qualitiesMap);
        }
        List<DownloadLink> qualities = qualitiesMap.get(height);
        if (qualities == null) {
            qualities = new ArrayList<DownloadLink>();
            qualitiesMap.put(height, qualities);
        }
        qualities.add(dl);
        return dl;
    }

    private int getUserPreferredqualityHeight() throws PluginException {
        final Quality quality = PluginJsonConfig.get(getConfigInterface()).getPreferredQuality();
        switch (quality) {
        case Q240:
            return 240;
        case Q360:
            return 360;
        case Q480:
            return 480;
        case Q720:
            return 720;
        case Q1080:
            return 1080;
        case Q1440:
            return 1440;
        case Q2160:
            return 2160;
        default:
            /* Should never happen */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported:" + quality);
        }
    }

    @Override
    public Class<RumbleComConfig> getConfigInterface() {
        return RumbleComConfig.class;
    }
}
