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

import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.jdownloader.plugins.controller.LazyPlugin;

import com.formdev.flatlaf.util.StringUtils;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 50741 $", interfaceVersion = 2, names = { "tvigle.ru" }, urls = { "https?://cloud\\.tvigle\\.ru/video/\\d+|https?://www\\.tvigle\\.ru/video/[a-z0-9\\-]+/" })
public class TvigleRu extends PluginForHost {
    public TvigleRu(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    /* Connection stuff */
    private static final boolean free_resume    = true;
    private static final int     free_maxchunks = 0;
    private String               dllink         = null;
    private String               token          = null;
    private static final String  type_embedded  = "(?i)https?://cloud\\.tvigle\\.ru/video/\\d+";
    private static final String  type_normal    = "(?i)https?://www\\.tvigle\\.ru/video/[a-z0-9\\-]+/";

    @Override
    public String getAGBLink() {
        return "http://www.tvigle.ru/";
    }

    @SuppressWarnings({ "deprecation" })
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        dllink = null;
        String videoID = link.getStringProperty("videoID", null);
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        final String extDefault = ".mp4";
        if (link.getDownloadURL().matches(type_embedded)) {
            videoID = new Regex(link.getDownloadURL(), "(\\d+)$").getMatch(0);
        } else {
            br.getPage(link.getDownloadURL());
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            videoID = br.getRegex("var cloudId = \\'(\\d+)\\';").getMatch(0);
            if (videoID == null) {
                videoID = br.getRegex("class=\"video-preview current_playing\" id=\"(\\d+)\"").getMatch(0);
            }
            if (videoID == null) {
                videoID = br.getRegex("api/v1/video/(\\d+)").getMatch(0);
            }
            if (videoID == null) {
                /* 2020-11-30 */
                videoID = br.getRegex("cloud\\.tvigle\\.ru/video/(\\d+)").getMatch(0);
            }
            if (videoID == null) {
                /* 2024-07-02 */
                videoID = br.getRegex("\"first_video_id\":(\\d+)").getMatch(0);
            }
        }
        if (videoID == null) {
            if (!br.containsHTML(Pattern.quote(br._getURL().getPath()))) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        link.setName(videoID + extDefault);
        br.getPage("/api/bff/updateAppToken");
        final Map<String, Object> api_token_data = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        token = api_token_data.get("token").toString();
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Token " + token);
        brc.getPage("/api/video/" + videoID + "/");
        if (brc.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!brc.getHttpConnection().getContentType().contains("application/json")) {
            /* No json response */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        String title = (String) entries.get("name");
        if (!StringUtils.isEmpty(title)) {
            final String ext = getFileNameExtensionFromString(dllink, extDefault);
            String filename = this.applyFilenameExtension(title, ext);
            link.setFinalFileName(filename);
        }
        link.setProperty("videoID", videoID);
        link.setProperty("content_id", entries.get("content_id"));
        if (StringUtils.isEmpty(link.getComment())) {
            final String description = (String) entries.get("description");
            if (!StringUtils.isEmpty(description)) {
                link.setComment(description);
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        final String content_id = link.getStringProperty("content_id");
        if (content_id == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Browser brc = br.cloneBrowser();
        // brc.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Token " + token);
        brc.getPage("https://cloud.tvigle.ru/api/play/video/" + content_id + "/");
        final Map<String, Object> entries = JSonStorage.restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> playlist = (Map<String, Object>) entries.get("playlist");
        final Object itemsO = playlist.get("items");
        final Map<String, Object> video = (Map<String, Object>) ((List) itemsO).get(0);
        if (Boolean.TRUE.equals(video.get("isGeoBlocked"))) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "GEO blocked: " + video.get("errorMessage"));
        }
        final Object error_object = video.get("errorType");
        if (error_object != null) {
            final long error_code = ((Number) error_object).longValue();
            if (error_code == 1 || error_code == 7) { // "errorType": 7, "isGeoBlocked": true
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        long filesize = -1;
        final Map<String, Object> videos = (Map<String, Object>) video.get("videos");
        final Map<String, Object> videolinks_map = (Map<String, Object>) videos.get("mp4");
        final Map<String, Object> video_files_size = (Map<String, Object>) video.get("video_files_size");
        final Map<String, Object> video_files_size_map = (Map<String, Object>) video_files_size.get("mp4");
        final String[] qualities = { "1080p", "720p", "480p", "360p", "240p", "180p" };
        for (final String quality : qualities) {
            dllink = (String) videolinks_map.get(quality);
            if (dllink != null) {
                filesize = ((Number) video_files_size_map.get(quality)).longValue();
                break;
            }
        }
        link.setDownloadSize(filesize);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, free_resume, free_maxchunks);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
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
