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
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.utils.StringUtils;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 52516 $", interfaceVersion = 3, names = {}, urls = {})
public class VidsonicNet extends PluginForHost {
    public VidsonicNet(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "vidsonic.net" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_NORMAL = Pattern.compile("/d/([a-z0-9]{12})");
    private static final Pattern PATTERN_EMBED  = Pattern.compile("/e/([a-z0-9]{12})");

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_NORMAL.pattern().substring(1) + "|" + PATTERN_EMBED.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        }
        return super.getLinkID(link);
    }

    private String getFID(final DownloadLink link) {
        String fid = new Regex(link.getPluginPatternMatcher(), PATTERN_NORMAL).getMatch(0);
        if (fid != null) {
            return fid;
        }
        fid = new Regex(link.getPluginPatternMatcher(), PATTERN_EMBED).getMatch(0);
        return fid;
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        return this.getFID(link) + ".mp4";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.getPage("https://" + getHost() + "/d/" + this.getFID(link));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("class=\"video-title\"[^>]*>([^<]+)<").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            link.setName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        /* Access embed page */
        br.getPage("/e/" + this.getFID(link));
        final String encoded_url = br.getRegex("const _0x1\\s*=\\s*'([^']+)").getMatch(0);
        if (encoded_url == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String hls_master = decodeVideoUrl(encoded_url);
        br.getPage(hls_master);
        final HlsContainer hlsBest = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(br));
        if (hlsBest == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        checkFFmpeg(link, "Download a HLS Stream");
        dl = new HLSDownloader(link, br, hlsBest.getStreamURL());
        dl.startDownload();
    }

    private static String decodeVideoUrl(String encoded) throws PluginException {
        final String hex = encoded.replace("|", "");
        final StringBuilder ascii = new StringBuilder();
        for (int i = 0; i < hex.length(); i += 2) {
            ascii.append((char) Integer.parseInt(hex.substring(i, i + 2), 16));
        }
        final String ret = ascii.reverse().toString();
        if (!StringUtils.startsWithCaseInsensitive(ret, "http")) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Invalid decoded videourl");
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}