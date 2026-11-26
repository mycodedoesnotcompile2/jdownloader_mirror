// jDownloader - Downloadmanager
// Copyright (C) 2026 JD-Team support@jdownloader.org
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.downloader.hls.M3U8Playlist;
import org.jdownloader.plugins.components.hls.HlsContainer;

import jd.PluginWrapper;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 54197 $", interfaceVersion = 3, names = {}, urls = {})
public class PeestreamCom extends PluginForHost {
    public PeestreamCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    /* DEV NOTES */
    // Tags: Streaming host
    // other: Player is hosted on peeplayer.com. Web player builds its HLS playlists locally and gets the segment key via ECDH key
    // exchange (/kx). The Chromecast master playlist (/m/<msg_id>/master.m3u8) is a regular HLS playlist which
    // only needs the static token "STOK" from the embed page.
    private static final String EXT = ".mp4";

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "peestream.com", "peeplayer.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:v|e)/([A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final Account account) {
        return 1;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public String getPluginContentURL(final DownloadLink link) {
        return "https://" + getHost() + "/v/" + getFID(link);
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        return getFID(link) + EXT;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final String fid = getFID(link);
        if (fid == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        /* peestream.com/e/... redirects to the player domain (peeplayer.com) */
        br.getPage("https://" + getHost() + "/e/" + fid);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.containsHTML("var MSG_ID\\s*=")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("var INFO_TITLE\\s*=\\s*\"((?:[^\"\\\\]|\\\\.)*)\"").getMatch(0);
        if (title != null) {
            title = restoreFromString("\"" + title + "\"", TypeRef.STRING);
        } else {
            title = br.getRegex("<title>([^<]+)</title>").getMatch(0);
        }
        if (!StringUtils.isEmpty(title)) {
            title = Encoding.htmlDecode(title).trim();
            link.setFinalFileName(this.correctOrApplyFileNameExtension(title, EXT, null));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        final String msgID = br.getRegex("var MSG_ID\\s*=\\s*(\\d+)").getMatch(0);
        final String streamToken = br.getRegex("var STOK\\s*=\\s*\"([^\"]+)\"").getMatch(0);
        if (msgID == null || streamToken == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find stream token");
        }
        /* peeplayer.com/... */
        String streamBase = br.getRegex("var STRIM_BASE\\s*=\\s*\"(https?://[^\"]+)\"").getMatch(0);
        if (streamBase == null) {
            /* Fallback: Domain of player page */
            streamBase = br._getURL().getProtocol() + "://" + br.getHost(true);
        }
        streamBase = streamBase.replaceFirst("/+$", "");
        br.getHeaders().put("Referer", br.getURL());
        br.getPage(streamBase + "/m/" + msgID + "/master.m3u8?t=" + Encoding.urlEncode(streamToken));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.containsHTML("#EXTM3U")) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Invalid master playlist");
        }
        final HlsContainer best = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(br));
        if (best == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find any stream qualities");
        }
        checkFFmpeg(link, "Download a HLS Stream");
        /* Small workaround */
        dl = new HLSDownloader(link, br, best.getDownloadurl()) {
            @Override
            public boolean isEncrypted() {
                return false;
            }

            @Override
            protected boolean isSupported(final M3U8Playlist m3u8) {
                return true;
            }
        };
        dl.startDownload();
    }
}
