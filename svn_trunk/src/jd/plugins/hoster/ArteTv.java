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
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.appwork.utils.StringUtils;
import org.jdownloader.downloader.hls.HLSContent;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.downloader.hls.M3U8Playlist;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.components.hls.HlsContainer.MEDIA;
import org.jdownloader.plugins.components.hls.HlsContainerStorable;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 53194 $", interfaceVersion = 3, names = { "arte.tv" }, urls = { "" })
public class ArteTv extends PluginForHost {
    @SuppressWarnings("deprecation")
    public ArteTv(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/sites/corporate/de/allgemeine-nutzungsbedingungen/";
    }

    @Override
    public String getPluginContentURL(final DownloadLink link) {
        final String directurl = getDirectURL(link);
        if (directurl != null) {
            return directurl;
        } else {
            return super.getPluginContentURL(link);
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, false);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws Exception {
        br.setFollowRedirects(true);
        final String directurl = this.getDirectURL(link);
        if (!StringUtils.isEmpty(directurl) && !isDownload && !this.isHLS(link)) {
            final URLConnectionAdapter con = br.openHeadConnection(directurl);
            try {
                handleConnectionErrors(br, con);
                if (con.getCompleteContentLength() > 0) {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
                findAndSetMd5Hash(link, con);
            } finally {
                con.disconnect();
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    protected void throwConnectionExceptions(Browser br, URLConnectionAdapter con) throws PluginException, IOException {
        switch (con.getResponseCode()) {
        case 403:
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", TimeUnit.HOURS.toMillis(1));
        case 404:
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        case 408:
            // HTTP/1.0 408 Request Time-out
            // Server: AkamaiGHost
            /**
             * <TITLE>Request Timeout</TITLE> </HEAD><BODY>
             * <H1>Request Timeout</H1> The server timed out while waiting for the browser's request.
             * <P>
             */
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Request Time-out", TimeUnit.MINUTES.toMillis(15));
        default:
            throw new PluginException(LinkStatus.ERROR_FATAL, "Video broken?(" + con.getResponseCode() + ")");
        }
    }

    private String getDirectURL(final DownloadLink link) {
        /* TODO: Remove this backward compatibility in 01-2023 */
        final String legacy_directURL = link.getStringProperty("directURL");
        if (legacy_directURL != null) {
            return legacy_directURL;
        } else {
            return link.getPluginPatternMatcher();
        }
    }

    private boolean isHLS(final DownloadLink link) {
        if (link.hasCompressedProperty(HlsContainerStorable.DOWNLOADLINK_PROPERTY)) {
            return true;
        }
        /* TODO: Remove this backward compatibility in 01-2023 */
        final String legacy_quality_intern = link.getStringProperty("quality_intern");
        if (StringUtils.contains(legacy_quality_intern, "hls_")) {
            return true;
        } else if (getDirectURL(link).contains(".m3u8")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        final HlsContainerStorable linkContainer = HlsContainerStorable.restoreFrom(link);
        if (linkContainer != null) {
            checkFFmpeg(link, "Download a HLS Stream");
            br.getPage(linkContainer.getM3u8URL());
            final List<HlsContainer> allContainer = HlsContainer.getHlsQualities(br);
            final HlsContainer found = HlsContainer.find(br, allContainer, linkContainer);
            if (found == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final List<M3U8Playlist> m3u8 = found.getM3U8(br.cloneBrowser());
            if (m3u8.size() != 1) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final HLSContent hlsContent = new HLSContent(m3u8.get(0));
            final MEDIA media = found.findMEDIA(linkContainer.getMedia().get(0));
            final List<M3U8Playlist> audioM3U8 = media.loadM3U8(br.cloneBrowser());
            if (audioM3U8 == null || audioM3U8.size() != 1) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            hlsContent.addAudioTrack(audioM3U8.get(0));
            final List<HLSContent> hlsContents = new ArrayList<HLSContent>();
            hlsContents.add(hlsContent);
            dl = new HLSDownloader(link, br, hlsContents);
            dl.startDownload();
            return;
        }
        final String directurl = this.getDirectURL(link);
        if (directurl == null) {
            /* This should never happen! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (this.isHLS(link)) {
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, directurl);
            dl.startDownload();
        } else {
            br.setFollowRedirects(true);
            // no need to hammer server when speed is not limited/throttled
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, directurl, true, -4);
            handleConnectionErrors(br, dl.getConnection());
            findAndSetMd5Hash(link, dl.getConnection());
            dl.startDownload();
        }
    }

    private void findAndSetMd5Hash(final DownloadLink link, final URLConnectionAdapter con) {
        final String etag = con.getRequest().getResponseHeader("etag");
        if (etag != null) {
            try {
                final String md5 = etag.replace("\"", "").split(":")[0];
                if (md5.matches("[A-Fa-f0-9]{32}")) {
                    link.setMD5Hash(md5);
                }
            } catch (final Throwable ignore) {
            }
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public String getDescription() {
        return "JDownloader's ARTE Plugin helps downloading videoclips from arte.tv. Arte provides different video qualities.";
    }

    @Override
    public boolean allowHandle(final DownloadLink link, final PluginForHost plugin) {
        /* No not allow multihost plugins to handle items from this plugin. */
        return link.getHost().equalsIgnoreCase(plugin.getHost());
    }
}