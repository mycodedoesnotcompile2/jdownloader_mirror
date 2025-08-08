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

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
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

@HostPlugin(revision = "$Revision: 51308 $", interfaceVersion = 3, names = {}, urls = {})
public class VideqCom extends PluginForHost {
    public VideqCom(PluginWrapper wrapper) {
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
        return "https://" + getHost() + "/terms-conditions";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "videq.com", "videq.dev", "videq.co", "videq.tel", "videq.mom", "videym.pro", "luluv.do", "videqstream.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:d|e)/([a-z0-9]{12})");
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
        /* 2025-07-09 */
        return 1;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String ext_default = ".mp4";
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(this.getFID(link) + ext_default);
        }
        this.setBrowserExclusive();
        final String contenturl = link.getPluginPatternMatcher().replace("/e/", "/d/");
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String filename = br.getRegex("<title>([^<]+)</title>").getMatch(0);
        String filesize = br.getRegex("d=\"M14 4l0 4l-6 0l0 -4\"[^>]*/></svg>([^<]+)</div>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            filename = this.correctOrApplyFileNameExtension(filename, ext_default, null);
            link.setFinalFileName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        if (filesize != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesize));
        } else {
            logger.warning("Failed to find filesize");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final String fuid = br.getRegex("var id = '([a-z0-9]{12})';").getMatch(0);
        if (fuid == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String iframe_url_part_1 = br.getRegex("'videq_iframe',\\s*'(https://[^']+)'").getMatch(0);
        final String iframe_url_part_2 = br.getRegex("'length',\\s*'([a-f0-9]+)'").getMatch(0);
        if (iframe_url_part_1 == null || iframe_url_part_2 == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* 2025-08-07: The "/911" request is not needed. */
        // final Browser brc = br.cloneBrowser();
        // brc.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
        // brc.getHeaders().put("Origin", "https://" + br.getHost());
        // brc.getHeaders().put("x-requested-with", "XMLHttpRequest");
        // brc.postPage("/911", "id=" + fuid);
        /* Important otherwise we'll get http response 403 */
        br.getHeaders().put("sec-fetch-dest", "iframe");
        br.getPage(iframe_url_part_1 + iframe_url_part_2);
        /* Typically https://embed.video-src.com/ or https://poop.video-src.com/ */
        final String baseURL = br.getRegex("var baseURL\\s*=\\s*\"(https://[^\"]+)\"").getMatch(0);
        /* Typically 'vplayer?id=' or 'dplayer?id=...' */
        final String playerPath = br.getRegex("var playerPath\\s*=\\s*'([^']+)';").getMatch(0);
        if (baseURL == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (playerPath == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String dllink = null;
        if (StringUtils.containsIgnoreCase(playerPath, "dplayer")) {
            /* 2025-08-07: dplayer -> Item embedded/hosted on doodstream.com -> Special handling */
            // throw new PluginException(LinkStatus.ERROR_FATAL, "Unsupported redirect to new system: " + newLink + " -> Manually
            // re-add this link to JDownloader to be able to download this file");
            logger.info("Handling doodstream embed link");
            br.getPage(baseURL + playerPath);
            final String newLink = br.getRegex("<iframe[^>]*src=\"(https://[^\"]+)\"").getMatch(0);
            if (newLink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final PluginForHost doodstreamPlugin = getNewPluginForHostInstance("dood.re").getLazyP().getPrototype(null);
            if (!doodstreamPlugin.canHandle(newLink)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "No idea how to handle this link: " + newLink);
            }
            link.setDefaultPlugin(doodstreamPlugin);
            link.setHost(doodstreamPlugin.getHost());
            link.setPluginPatternMatcher(newLink);
            link.setDomainInfo(null);
            // doodstreamPlugin.handleFree(link);
            throw new PluginException(LinkStatus.ERROR_RETRY, "Retry link that has been migrated from " + getHost() + " to doodstream");
        }
        if (!StringUtils.containsIgnoreCase(playerPath, "vplayer")) {
            logger.warning("Found possibly unsupported embed format: " + playerPath);
        }
        // final String fid = this.getFID(link);
        // br.getPage("https://embed.video-src.com/vplayer?id=" + fid);
        br.getPage(baseURL + playerPath);
        dllink = br.getRegex("\"l\",\\s*\"(https?:/[^\"]+)").getMatch(0);
        if (dllink == null) {
            /* New 2025-07-22 */
            final String key = br.getRegex("objectKey: \"([^\"]+)").getMatch(0);
            if (key != null) {
                dllink = "https://img.video-src.com/" + key;
            }
        }
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
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