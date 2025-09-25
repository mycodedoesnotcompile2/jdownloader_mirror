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
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51559 $", interfaceVersion = 3, names = {}, urls = {})
public class XiaoshenkeNet extends PluginForHost {
    public XiaoshenkeNet(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    /* Connection stuff */
    private static final boolean free_resume    = true;
    private static final int     free_maxchunks = 0;
    private String               dllink         = null;
    public static final String   PROPERTY_TITLE = "title";

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "xiaoshenke.net" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/video[a-z]?/([a-f0-9]+)(/([a-zA-Z0-9_/\\+\\=\\-%]+))?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String linkid = getFID(link);
        if (linkid != null) {
            return this.getHost() + "://" + linkid;
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public void clean() {
        dllink = null;
        super.clean();
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        /* No title given in html code --> Use title set by crawler if existent. */
        final String extDefault = ".mp4";
        if (link.hasProperty(PROPERTY_TITLE)) {
            link.setFinalFileName(link.getStringProperty(PROPERTY_TITLE) + extDefault);
        } else if (!link.isNameSet()) {
            link.setName(this.getFID(link) + extDefault);
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Oops\\! This video has been deleted")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String id = br.getRegex("var\\s*id\\s*=\\s*\"([^\"]*)\"").getMatch(0);
        if (id == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String qualityString = br.getRegex("var\\s*quality\\s*=\\s*parseInt\\(\"(\\d+)").getMatch(0);
        if (qualityString == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        id = new StringBuilder(id).reverse().toString();
        final int quality = Integer.parseInt(qualityString);
        if ((quality & 8) != 0) {
            dllink = "https://xiaoshenke.net/vid/" + id + "/1080";
        } else if ((quality & 4) != 0) {
            dllink = "https://xiaoshenke.net/vid/" + id + "/720";
        } else if ((quality & 2) != 0) {
            dllink = "https://xiaoshenke.net/vid/" + id + "/480";
        } else if ((quality & 1) != 0) {
            dllink = "https://xiaoshenke.net/vid/" + id + "/360";
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (!PluginEnvironment.getPluginEnvironment().equals(PluginEnvironment.DOWNLOAD) && !link.isSizeSet() && !StringUtils.isEmpty(dllink)) {
            final Browser brc = br.cloneBrowser();
            brc.setFollowRedirects(true);
            basicLinkCheck(brc, brc.createHeadRequest(dllink), link, null, null);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, free_resume, free_maxchunks);
        handleConnectionErrors(br, dl.getConnection());
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