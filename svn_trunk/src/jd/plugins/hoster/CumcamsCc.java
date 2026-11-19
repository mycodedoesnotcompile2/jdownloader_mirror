//jDownloader - Downloadmanager
//Copyright (C) 2017  JD-Team support@jdownloader.org
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
import java.util.List;

import org.appwork.utils.StringUtils;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 54057 $", interfaceVersion = 3, names = {}, urls = {})
public class CumcamsCc extends PluginForHost {
    public CumcamsCc(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "cumcams.cc" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/video/(\\d+)(?:/play)?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms";
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
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    /* No @Override */
    public int getMaxChunks(final Account account) {
        return 0;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final String extDefault = ".mp4";
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            link.setName(fid + extDefault);
        }
        this.setBrowserExclusive();
        br.getPage("https://" + this.getHost() + "/video/" + fid + "/play");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String dmca = br.getRegex("data-dmca=\"([^\"]+)\"").getMatch(0);
        if (!StringUtils.isEmpty(dmca)) {
            /* Video has been taken down. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("<h1 class=\"videos__title\">([^<]+)</h1>").getMatch(0);
        if (StringUtils.isEmpty(title)) {
            title = br.getRegex("<title>([^<]+)</title>").getMatch(0);
        }
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            link.setFinalFileName(this.applyFilenameExtension(title, extDefault));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        final String fid = this.getFID(link);
        final String token = br.getRegex("id=\"play_button\"[^>]*data-token=\"([^\"]+)\"").getMatch(0);
        if (StringUtils.isEmpty(token)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        checkFFmpeg(link, "Download a HLS Stream");
        br.getHeaders().put("Accept", "*/*");
        br.getHeaders().put("Referer", "https://" + this.getHost() + "/video/" + fid + "/play");
        final String apiUrl = "https://" + this.getHost() + "/api/video/" + fid + "?token=" + Encoding.urlEncode(token);
        br.getPage(apiUrl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* The API returns a HTML snippet containing the player with the HLS URL. */
        String hlsUrl = br.getRegex("<source[^>]*src=\"(https?://[^\"]+\\.m3u8[^\"]*)\"").getMatch(0);
        if (StringUtils.isEmpty(hlsUrl)) {
            if (br.containsHTML("id=\"video-unavailable\"")) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Video temporarily unavailable", 5 * 60 * 1000l);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        hlsUrl = Encoding.htmlOnlyDecode(hlsUrl);
        br.getPage(hlsUrl);
        if (!br.containsHTML("#EXTM3U")) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getHeaders().put("Origin", "https://" + this.getHost());
        dl = new HLSDownloader(link, br, hlsUrl);
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}
