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

import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 52137 $", interfaceVersion = 3, names = {}, urls = {})
public class MySpassDe extends PluginForHost {
    public MySpassDe(PluginWrapper wrapper) {
        super(wrapper);
        setConfigElements();
    }

    private String                               dllink                  = null;
    private static final AtomicReference<String> token                   = new AtomicReference<String>(null);
    private static final AtomicReference<String> cdn                     = new AtomicReference<String>(null);
    private static final AtomicLong              timestampTokenRefreshed = new AtomicLong(0);
    private static final AtomicLong              timestampCDNRefreshed   = new AtomicLong(0);
    private static final Pattern                 PATH_OLD                = Pattern.compile("/player\\?video=(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                 PATH_NEW                = Pattern.compile("/player/.*?([a-z0-9-]+)/(\\d+)(\\?|$)", Pattern.CASE_INSENSITIVE); // 2025-04-10

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/myspass/kontakt/";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "myspass.de" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATH_OLD.pattern() + "|" + PATH_NEW.pattern() + ")");
        }
        return ret.toArray(new String[0]);
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
        String fid = new Regex(link.getPluginPatternMatcher(), PATH_NEW).getMatch(1);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), PATH_OLD).getMatch(0);
        return fid;
    }

    private String getTitleFromURL(final DownloadLink link) {
        String title = new Regex(link.getPluginPatternMatcher(), PATH_NEW).getMatch(1);
        if (title != null) {
            title = title.replace("-", " ").trim();
            return title;
        }
        return null;
    }

    @Override
    public String getPluginContentURL(DownloadLink link) {
        return getWorkingContentURL(link);
    }

    private String getWorkingContentURL(final DownloadLink link) {
        if (new Regex(link.getPluginPatternMatcher(), PATH_NEW).patternFind()) {
            /* URL we got already is a "new" one -> Return without changing it. */
            return link.getPluginPatternMatcher();
        } else {
            /* We got an old URL -> Make new one out of it */
            return generateContentURL(this.getFID(link));
        }
    }

    private static String generateContentURL(final String content_id) {
        return "https://www.myspass.de/player/" + new Random().nextInt(10000) + "/" + content_id;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException, InterruptedException {
        return requestFileInformation(link, false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws IOException, PluginException, InterruptedException {
        dllink = null;
        final String ext = ".mp4";
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            final String titleFromURL = this.getTitleFromURL(link);
            if (titleFromURL != null) {
                link.setName(titleFromURL + ext);
            } else {
                link.setName(fid + ext);
            }
        }
        synchronized (token) {
            if (token.get() == null || System.currentTimeMillis() - timestampTokenRefreshed.get() > 30 * 60 * 1000 || (isDownload && (cdn.get() == null || System.currentTimeMillis() - timestampCDNRefreshed.get() > 30 * 60 * 1000))) {
                logger.info("Obtaining fresh token and/or cdn value");
                String freshToken = null;
                String freshCDN = null;
                final Browser brc = br.cloneBrowser();
                brc.getPage(getWorkingContentURL(link));
                final String[] jsurls = brc.getRegex("\"([^\"]+[0-f0-9]+\\.js)").getColumn(0);
                if (jsurls == null || jsurls.length == 0) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final HashSet<String> jsurlsunique = new HashSet<String>(Arrays.asList(jsurls));
                int i = 0;
                for (final String jsurl : jsurlsunique) {
                    brc.getPage(jsurl);
                    if (freshCDN == null) {
                        freshCDN = brc.getRegex("uri:\\s*`(https?://[^/,]*?)\\$").getMatch(0);
                    }
                    if (freshToken == null) {
                        freshToken = brc.getRegex("Bearer ([a-f0-9]{256})").getMatch(0);
                    }
                    if (freshCDN != null && freshToken != null) {
                        break;
                    } else if (freshToken != null && !isDownload) {
                        /* During linkcheck, having only a fresh token is okay -> CDN uri is only needed for downloading */
                        break;
                    }
                    logger.info("Failed to find required data in js[" + i + "] -> " + jsurl);
                    i++;
                    if (this.isAbort()) {
                        throw new InterruptedException();
                    }
                }
                if (freshCDN != null) {
                    logger.info("Found new CDN uri: " + freshCDN);
                    cdn.set(freshCDN);
                    timestampCDNRefreshed.set(System.currentTimeMillis());
                }
                if (freshToken != null) {
                    logger.info("Found new token: " + freshToken);
                    token.set(freshToken);
                    timestampTokenRefreshed.set(System.currentTimeMillis());
                }
                if (freshToken == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
        final Browser brv = br.cloneBrowser();
        brv.getHeaders().put("Authorization", "Bearer " + token.get());
        brv.getPage("https://cms-myspass.vanilla-ott.com/api/videos/" + fid + "?populate[season][fields]=name&populate[format][fields]=name&");
        if (brv.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(brv.getRequest().getHtmlCode(), TypeRef.MAP);
        final Object errorO = entries.get("error");
        if (errorO != null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> attr = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "data/attributes");
        // final String broadcast_date = attr.get("broadcast_date").toString();
        String title = attr.get("title").toString();
        title = title.replaceFirst("\\(Folge \\d+\\)", "");
        final String unique_name = (String) attr.get("unique_name"); // can be null
        final String description = (String) attr.get("teaser_text");
        if (!StringUtils.isEmpty(description) && StringUtils.isEmpty(link.getComment())) {
            link.setComment(description);
        }
        final String video_url = (String) attr.get("video_url");
        if (video_url != null) {
            if (StringUtils.startsWithCaseInsensitive(video_url, "http")) {
                dllink = video_url;
            } else if (video_url.startsWith("/") && cdn.get() != null) {
                dllink = cdn.get() + video_url;
            }
        }
        final String format = (String) JavaScriptEngineFactory.walkJson(attr, "format/data/attributes/name");
        String filename = format;
        if (unique_name != null) {
            final String seasonStr = new Regex(unique_name, "(?i)Staffel (\\d+)").getMatch(0);
            final String episodeStr = new Regex(unique_name, "(?i)Folge (\\d+)").getMatch(0);
            final DecimalFormat df = new DecimalFormat("00");
            if (seasonStr != null && episodeStr != null) { // Sometimes episode = 9/Best Of, need regex to get only the integer
                filename += " - S" + df.format(Integer.parseInt(seasonStr)) + "E" + df.format(Integer.parseInt(episodeStr));
            }
            /* Avoid adding information twice */
            if (!format.contains(title)) {
                filename += " - " + title;
            }
        }
        filename = filename.trim();
        filename = Encoding.htmlDecode(filename);
        link.setFinalFileName(filename + ext);
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        if (dllink == null) {
            if (cdn.get() == null) {
                logger.warning("Failed to find final downloadurl because failed to find cdn url");
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (dllink.contains(".m3u8")) {
            br.getPage(dllink);
            final String url_hls;
            if (br.containsHTML("#EXT-X-PLAYLIST-TYPE:VOD")) {
                /* Single HLS stream */
                url_hls = br.getURL();
            } else {
                final HlsContainer hlsbest = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(this.br));
                url_hls = hlsbest.getStreamURL();
            }
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, url_hls);
            dl.startDownload();
        } else {
            /* 2017-02-04: Without the Range Header we'll be limited to ~100 KB/s */
            link.setProperty(DirectHTTP.PROPERTY_ServerComaptibleForByteRangeRequest, true);
            br.getHeaders().put(OPEN_RANGE_REQUEST);
            /* Workaround for old downloadcore bug that can lead to incomplete files */
            br.getHeaders().put("Accept-Encoding", "identity");
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, false, 1);
            handleConnectionErrors(br, dl.getConnection());
            dl.startDownload();
        }
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "FAST_LINKCHECK", "Enable fast linkcheck?\r\nFilesize will only be visible on downloadstart!").setDefaultValue(true));
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}
