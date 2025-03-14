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
import java.util.regex.Pattern;

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
import jd.plugins.components.SiteType.SiteTemplate;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 49243 $", interfaceVersion = 3, names = { "luxuretv.com", "homemoviestube.com" }, urls = { "https?://(?:www\\.|en\\.)?luxuretv\\.com/videos/[a-z0-9\\-]+\\-\\d+\\.html", "https?://(?:www\\.)?homemoviestube\\.com/videos/\\d+/[a-z0-9\\-]+\\.html" })
public class UnknownPornScript4 extends PluginForHost {
    public UnknownPornScript4(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    /* DEV NOTES */
    /* Porn_plugin */
    /* V0.1 */
    // other:
    /* Extension which will be used if no correct extension is found */
    private static final String type_1            = "^https?://(?:www\\.)?[^/]+/videos/([a-z0-9\\-]+)\\-(\\d+)\\.html$";
    /* E.g. homemoviestube.com */
    private static final String type_2            = "^https?://(?:www\\.)?[^/]+/videos/(\\d+)/([a-z0-9\\-]+)\\.html$";
    private static final String default_Extension = ".mp4";
    /* Connection stuff */
    private static final int    free_maxdownloads = -1;
    private String              dllink            = null;

    @Override
    public String getAGBLink() {
        return "https://www.homemoviestube.com/tos.php";
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
        if (link.getPluginPatternMatcher() == null) {
            return null;
        }
        if (link.getPluginPatternMatcher().matches(type_1)) {
            return new Regex(link.getPluginPatternMatcher(), type_1).getMatch(1);
        } else {
            return new Regex(link.getPluginPatternMatcher(), type_2).getMatch(0);
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return requestFileInformation(link, false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws IOException, PluginException {
        dllink = null;
        final String titleFromURL;
        if (link.getPluginPatternMatcher().matches(type_1)) {
            titleFromURL = new Regex(link.getPluginPatternMatcher(), type_1).getMatch(0).replace("-", " ");
        } else {
            /* E.g. homemoviestube.com */
            titleFromURL = new Regex(link.getPluginPatternMatcher(), type_2).getMatch(1).replace("-", " ");
        }
        if (!link.isNameSet()) {
            link.setName(titleFromURL + default_Extension);
        }
        final String host = link.getHost();
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404 || this.br.getURL().endsWith("/404.php")) {
            /* E.g. 404.php: http://www.bondagebox.com/ */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.br.getURL().contains(host + "/")) {
            /* E.g. http://www.watchgfporn.com/videos/she-fucked-just-about-all-of-us-that-night-9332.html */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = regexStandardTitleWithHost(host);
        if (title == null) {
            title = titleFromURL;
        }
        title = Encoding.htmlDecode(title).trim();
        String flashvars = this.br.getRegex("\"flashvars\"\\s*,\\s*\"([^<>\"]*?)\"").getMatch(0);
        if (flashvars == null) {
            /* E.g. homemoviestube.com */
            flashvars = this.br.getRegex("flashvars\\s*=\\s*\"([^<>\"]+)").getMatch(0);
        }
        if (flashvars != null) {
            dllink = new Regex(flashvars, "(https?://(?:www\\.)?[^/]+/playerConfig\\.php[^<>\"/\\&]+)").getMatch(0);
            if (dllink != null) {
                dllink = Encoding.htmlDecode(dllink);
                final Browser brc = new Browser();
                brc.getPage(dllink);
                dllink = brc.getRegex("flvMask:(.*?)(%7C|;)").getMatch(0);
                dllink = Encoding.htmlDecode(dllink);
                if (dllink == null) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
            }
        }
        if (dllink == null) {
            final String src = br.getRegex("<source src=\"([^<>\"]*?)\"").getMatch(0);
            if (src != null) {
                dllink = br.getURL(src).toExternalForm();
            }
        }
        if (title != null) {
            link.setName(this.applyFilenameExtension(title, default_Extension));
        }
        if (!StringUtils.isEmpty(dllink) && !isDownload) {
            basicLinkCheck(br.cloneBrowser(), br.createHeadRequest(dllink), link, title, default_Extension);
        }
        return AvailableStatus.TRUE;
    }

    private int getMaxChunks() {
        if ("homemoviestube.com".equals(getHost())) {
            return 1;
        } else {
            return 0;
        }
    }

    @Override
    public boolean isResumeable(DownloadLink link, final Account account) {
        if ("homemoviestube.com".equals(getHost())) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks());
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    private String regexStandardTitleWithHost(final String host) {
        final String[] hostparts = host.split("\\.");
        final String host_relevant_part = hostparts[0];
        String site_title = br.getRegex(Pattern.compile("<title>([^<>\"]*?) \\- " + host + "</title>", Pattern.CASE_INSENSITIVE | Pattern.DOTALL)).getMatch(0);
        if (site_title == null) {
            site_title = br.getRegex(Pattern.compile("<title>([^<>\"]*?) at " + host + "</title>", Pattern.CASE_INSENSITIVE | Pattern.DOTALL)).getMatch(0);
        }
        if (site_title == null) {
            site_title = br.getRegex(Pattern.compile("<title>([^<>\"]*?) at " + host_relevant_part + "</title>", Pattern.CASE_INSENSITIVE | Pattern.DOTALL)).getMatch(0);
        }
        return site_title;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return free_maxdownloads;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.UnknownPornScript4;
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
