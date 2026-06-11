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
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
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

@HostPlugin(revision = "$Revision: 52890 $", interfaceVersion = 3, names = {}, urls = {})
public class UpskirtTv extends PluginForHost {
    public UpskirtTv(PluginWrapper wrapper) {
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

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "upskirt.tv" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_NORMAL = Pattern.compile("/play/(\\d+)(/([a-z0-9\\-]+)/?)?");

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_NORMAL.pattern());
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
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        return 0;
    }

    @Override
    protected String getDefaultFileName(DownloadLink link) {
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), PATTERN_NORMAL);
        String urlTitle = urlinfo.getMatch(2);
        if (urlTitle != null) {
            /* Return <titleFromURL> + .mp4 */
            return Encoding.htmlDecode(urlTitle).replace("-", " ").trim() + ".mp4";
        }
        /* Return <file_id>.mp4 */
        return urlinfo.getMatch(0) + ".mp4";
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String urlTitle = new Regex(br.getURL(), PATTERN_NORMAL).getMatch(2);
        if (urlTitle != null) {
            urlTitle = Encoding.htmlDecode(urlTitle).replace("-", " ").trim();
            link.setFinalFileName(urlTitle + ".mp4");
        } else {
            logger.warning("Failed to find file title");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final Browser brc = br.cloneBrowser();
        brc.postPage("https://api.upskirt.tv/hls", "id=" + this.getFID(link));
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> mp4List = (List<Map<String, Object>>) entries.get("mp4");
        if (mp4List == null || mp4List.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String bestUrl = null;
        int bestQuality = -1;
        for (final Map<String, Object> entry : mp4List) {
            final String url = (String) entry.get("src");
            if (url == null) {
                continue;
            }
            final String title = (String) entry.get("title"); // e.g. 480p.mp4
            int quality = -1;
            /* Try title first, then URL */
            String qualityMatch = new Regex(title, "(\\d+)p").getMatch(0);
            if (qualityMatch == null) {
                qualityMatch = new Regex(url, "/(\\d+)p\\.mp4").getMatch(0);
            }
            if (qualityMatch != null) {
                quality = Integer.parseInt(qualityMatch);
            }
            if (quality == -1) {
                /* No quality found — use as fallback if nothing better found */
                if (bestUrl == null) {
                    bestUrl = url;
                }
                continue;
            }
            if (quality > bestQuality) {
                bestQuality = quality;
                bestUrl = url;
            }
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, bestUrl, this.isResumeable(link, null), this.getMaxChunks(link, null));
        this.handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
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