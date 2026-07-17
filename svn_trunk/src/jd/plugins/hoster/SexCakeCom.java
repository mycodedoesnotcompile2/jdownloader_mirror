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

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 52998 $", interfaceVersion = 3, names = {}, urls = {})
public class SexCakeCom extends PluginForHost {
    public SexCakeCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    private String dllink = null;

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "bluevideos.net" });
        ret.add(new String[] { "filmexnxx.net", "bluevideos.net" });
        ret.add(new String[] { "pornoxnxx.video", "videospornograficos.net" });
        ret.add(new String[] { "rubias19.red" });
        /* 2026-07-01: porncomixone.net also redirects to this one RE forum 90642 */
        ret.add(new String[] { "redporn.video" });
        ret.add(new String[] { "voglioporno.gratis", "vogliosesso.org" });
        ret.add(new String[] { "xnxxvideos.gratis", "pretoporno.com" });
        ret.add(new String[] { "xxxbucetas.net" });
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
            ret.add("https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "/.+\\.html");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }
    // @Override
    // public String getLinkID(final DownloadLink link) {
    // final String linkid = getFID(link);
    // if (linkid != null) {
    // return this.getHost() + "://" + linkid;
    // } else {
    // return super.getLinkID(link);
    // }
    // }
    //
    // private String getFID(final DownloadLink link) {
    // return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(1);
    // }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return true;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws Exception {
        dllink = null;
        final String path = new URL(link.getPluginPatternMatcher()).getPath();
        final String title = Encoding.htmlDecode(path).substring(1).replace("-", " ").trim().replaceFirst("\\.html?$", "");
        link.setFinalFileName(title + ".mp4");
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        dllink = br.getRegex("source src=\"([^\"]+)\"[^>]*type=\"video/mp4\"").getMatch(0);
        if (!StringUtils.isEmpty(dllink) && !isDownload) {
            final Browser brc = br.cloneBrowser();
            brc.setCurrentURL(dllink);
            basicLinkCheck(brc, brc.createGetRequest(dllink), link, title, ".mp4");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(this.br, link, dllink, this.isResumeable(link, null), 0);
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    protected void handleConnectionErrors(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            if (br.containsHTML("deleted")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            super.handleConnectionErrors(br, con);
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}