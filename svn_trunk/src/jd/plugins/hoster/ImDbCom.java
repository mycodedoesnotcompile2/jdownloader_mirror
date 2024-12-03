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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.controlling.ffmpeg.json.StreamInfo;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 50275 $", interfaceVersion = 2, names = {}, urls = {})
public class ImDbCom extends PluginForHost {
    private String              dllink         = null;
    private boolean             mature_content = false;
    private static final String IDREGEX        = "(vi\\d+)$";
    public static final String  TYPE_VIDEO     = "(?i)/(video|videoplayer)/(?:([\\w\\-]+)/)?vi(\\d+)";
    public static final Pattern TYPE_PHOTO     = Pattern.compile("/[A-Za-z]+/[a-z]{2}(\\d+)/mediaviewer/rm(\\d+)", Pattern.CASE_INSENSITIVE);

    public ImDbCom(final PluginWrapper wrapper) {
        super(wrapper);
        setConfigElements();
        this.setStartIntervall(1000l);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setLoadLimit(br.getLoadLimit() * 3);
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "imdb.com" });
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
            String regex = "https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(";
            regex += TYPE_VIDEO + "|";
            regex += TYPE_PHOTO.pattern() + "";
            regex += ")";
            ret.add(regex);
        }
        return ret.toArray(new String[0]);
    }

    private String getContentURL(final DownloadLink link) {
        final Regex regex_video = new Regex(link.getPluginPatternMatcher(), TYPE_VIDEO);
        if (regex_video.patternFind()) {
            return "https://www." + getHost() + "/video/screenplay/vi" + regex_video.getMatch(2);
        } else {
            return link.getPluginPatternMatcher();
        }
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

    public static String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), "(?:rm|vi)(\\d+)").getMatch(0);
    }

    @Override
    public String getAGBLink() {
        return "http://www." + getHost() + "/help/show_article?conditions";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if (new Regex(link.getPluginPatternMatcher(), TYPE_VIDEO).patternFind()) {
            return 1;
        } else {
            return 0;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws Exception {
        this.dllink = null;
        this.mature_content = false;
        setBrowserExclusive();
        br.getPage(getContentURL(link));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String ending = null;
        String filename = null;
        final Regex regex_photo = new Regex(link.getPluginPatternMatcher(), TYPE_PHOTO);
        if (regex_photo.patternFind()) {
            /* 2020-11-03 */
            final String json = br.getRegex("__NEXT_DATA__\"\\s*type\\s*=\\s*\"application/json\"\\s*>\\s*(\\{.*?\\});?\\s*</script").getMatch(0);
            // final String id_main = new Regex(link.getDownloadURL(), "([a-z]{2}\\d+)/mediaviewer").getMatch(0);
            Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
            /* Now let's find the specific object ... */
            final String idright = regex_photo.getMatch(1);
            final Map<String, Object> targetMap = this.findPictureMap(entries, "rm" + idright);
            filename = (String) JavaScriptEngineFactory.walkJson(targetMap, "titles/{0}/titleText/text");
            dllink = (String) targetMap.get("url");
            if (StringUtils.isEmpty(filename)) {
                /* Fallback to fid */
                filename = this.getFID(link);
            }
            if (filename == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else if (dllink == null || !dllink.startsWith("http")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            filename = Encoding.htmlDecode(filename.trim());
            final String fid = new Regex(link.getDownloadURL(), "rm(\\d+)").getMatch(0);
            String artist = br.getRegex("itemprop=\\'url\\'>([^<>\"]*?)</a>").getMatch(0);
            if (artist != null) {
                filename = Encoding.htmlDecode(artist.trim()) + "_" + fid + "_" + filename;
            } else {
                filename = fid + "_" + filename;
            }
            ending = getFileNameExtensionFromString(dllink, ".jpg");
        } else {
            /* Video */
            /*
             * get the fileName from main download link page because fileName on the /player subpage may be wrong
             */
            // br.getPage(downloadURL + "/player");
            if (br.getHttpConnection().getResponseCode() == 404 || br.containsHTML("(<title>IMDb Video Player: </title>|This video is not available\\.)")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            filename = br.getRegex("<title>(.*?)</title>").getMatch(0);
            final String json = br.getRegex("__NEXT_DATA__\"\\s*type\\s*=\\s*\"application/json\"\\s*>\\s*(\\{.*?\\});?\\s*</script").getMatch(0);
            final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
            /* json inside json */
            final List<Map<String, Object>> videoObjects = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(entries, "props/pageProps/videoPlaybackData/video/playbackURLs");
            String dllink_http = null;
            String dllink_hls_master = null;
            for (final Map<String, Object> videoO : videoObjects) {
                String mimeType = (String) videoO.get("mimeType");
                if (mimeType == null) {
                    /* 2023-12-19 */
                    mimeType = (String) videoO.get("videoMimeType");
                }
                final String url = (String) videoO.get("url");
                // final String definition = (String) entries.get("definition"); // E.g. AUTO, 480p, SD
                if (StringUtils.isEmpty(mimeType) || StringUtils.isEmpty(url)) {
                    /* Skip invalid items */
                    continue;
                }
                if (mimeType.equalsIgnoreCase("video/mp4") || mimeType.equalsIgnoreCase("MP4")) {
                    if (dllink_http == null) {
                        // TODO: add quality selection support
                        dllink_http = url;
                    }
                } else if (mimeType.equalsIgnoreCase("application/x-mpegurl") || mimeType.equalsIgnoreCase("M3U8")) {
                    if (dllink_hls_master == null) {
                        dllink_hls_master = url;
                    }
                } else {
                    logger.info("Unsupported mimeType: " + mimeType);
                }
            }
            if (filename == null) {
                filename = this.getFID(link);
            } else {
                filename = this.getFID(link) + "_" + filename;
            }
            dllink = dllink_hls_master;
            if (StringUtils.isEmpty(dllink)) {
                // TODO: add quality selection support
                dllink = dllink_http;
            }
            filename = filename.trim();
            ending = ".mp4";
        }
        link.setFinalFileName(Encoding.htmlDecode(filename) + ending);
        if (dllink != null && dllink.contains(".m3u8")) {
            /* hls */
            /* Access HLS master */
            br.getPage(dllink);
            final List<HlsContainer> allQualities = HlsContainer.getHlsQualities(this.br);
            final HlsContainer hlsBest = HlsContainer.findBestVideoByBandwidth(allQualities);
            HlsContainer finalCandidate = null;
            final String configuredResolution = getConfiguredVideoResolution();
            final long configuredBandwidth = getConfiguredVideoBandwidth();
            if (configuredResolution.equalsIgnoreCase("BEST")) {
                finalCandidate = hlsBest;
            } else {
                for (final HlsContainer hlstemp : allQualities) {
                    if (hlstemp.getResolution().equalsIgnoreCase(configuredResolution) && hlstemp.getBandwidth() == configuredBandwidth) {
                        logger.info("Found User-Selection");
                        finalCandidate = hlstemp;
                    }
                }
                if (finalCandidate == null) {
                    logger.info("Failed to find configured quality --> Falling back to BEST");
                    finalCandidate = hlsBest;
                } else {
                    logger.info("Quality selection successful");
                }
            }
            dllink = finalCandidate.getDownloadurl();
            checkFFProbe(link, "Download a HLS Stream");
            final HLSDownloader downloader = new HLSDownloader(link, br, dllink);
            final StreamInfo streamInfo = downloader.getProbe();
            if (streamInfo == null) {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Broken stream?");
            }
            final long estimatedSize = downloader.getEstimatedSize();
            if (estimatedSize > 0) {
                link.setDownloadSize(estimatedSize);
            }
        } else if (this.dllink != null && !isDownload) {
            /* http */
            this.basicLinkCheck(br, br.createHeadRequest(dllink), link, filename, ending);
        }
        return AvailableStatus.TRUE;
    }

    /** Recursive function to find map containing specified key:value pair. */
    private Map<String, Object> findPictureMap(final Object jsono, final String sectionSlug) throws PluginException {
        if (jsono instanceof Map) {
            final Map<String, Object> mapTmp = (Map<String, Object>) jsono;
            final Iterator<Entry<String, Object>> iterator = mapTmp.entrySet().iterator();
            while (iterator.hasNext()) {
                final Entry<String, Object> entry = iterator.next();
                final String key = entry.getKey();
                final Object value = entry.getValue();
                if (key.equals("id") && value instanceof String && value.toString().equalsIgnoreCase(sectionSlug)) {
                    return mapTmp;
                } else if (value instanceof List || value instanceof Map) {
                    final Map<String, Object> result = findPictureMap(value, sectionSlug);
                    if (result != null) {
                        return result;
                    }
                }
            }
        } else if (jsono instanceof List) {
            final List<Object> ressourcelist = (List<Object>) jsono;
            for (final Object arrayo : ressourcelist) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final Map<String, Object> result = findPictureMap(arrayo, sectionSlug);
                    if (result != null) {
                        return result;
                    }
                }
            }
        }
        return null;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        if (this.mature_content) {
            /* Mature content --> Only viewable for registered users */
            throw new AccountRequiredException("Account needed to download mature content");
        } else if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (dllink.contains(".m3u8")) {
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, dllink);
        } else {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
            if (!this.looksLikeDownloadableContent(dl.getConnection())) {
                br.followConnection(true);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    private String getConfiguredVideoResolution() {
        final int selection = this.getPluginConfig().getIntegerProperty(SELECTED_VIDEO_FORMAT, 0);
        final String selectedFormat = FORMATS[selection];
        if (selectedFormat.contains("x")) {
            final String resolution = selectedFormat.split("@")[0];
            return resolution;
        } else {
            /* BEST selection */
            return selectedFormat;
        }
    }

    private long getConfiguredVideoBandwidth() {
        final int selection = this.getPluginConfig().getIntegerProperty(SELECTED_VIDEO_FORMAT, 0);
        final String selectedFormat = FORMATS[selection];
        if (selectedFormat.contains("x")) {
            final String bandwidth = selectedFormat.split("@")[1];
            return Long.parseLong(bandwidth);
        } else {
            /* BEST selection */
            return 0;
        }
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_COMBOBOX_INDEX, getPluginConfig(), SELECTED_VIDEO_FORMAT, FORMATS, "Preferred video quality:").setDefaultValue(0));
    }

    /* The list of qualities displayed to the user */
    private final String[] FORMATS               = new String[] { "BEST", "1920x1080@8735000", "1280x720@5632000", "1280x720@3480000", "704x396@2366000", "640x360@1638000", "640x360@1114000", "512x288@777000", "480x270@532000", "320x180@326000" };
    private final String   SELECTED_VIDEO_FORMAT = "SELECTED_VIDEO_FORMAT";

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
    }

    @Override
    public void resetPluginGlobals() {
    }
}