//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
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
import org.jdownloader.plugins.controller.LazyPlugin;

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

@HostPlugin(revision = "$Revision: 53114 $", interfaceVersion = 3, names = {}, urls = {})
public class HotpicCc extends PluginForHost {
    public HotpicCc(final PluginWrapper wrapper) {
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
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_HOST };
    }

    private static final String default_extension = ".jpg";
    private String              dllink            = null;

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "hotpic.cc", "hotpic.me" });
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
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/i/([A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
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

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        dllink = null;
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("File Not Found\\s*</h1>")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (StringUtils.equalsIgnoreCase(br._getURL().getPath(), "/404/")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Find direct media URL */
        dllink = br.getRegex("id=\"download-button\"[^>]*data-media=\"(https?://[^\"]+)\"").getMatch(0);
        if (dllink == null) {
            dllink = br.getRegex("id=\"main-image\"[^>]*src=\"(https?://[^\"]+)\"").getMatch(0);
        }
        if (dllink == null) {
            dllink = br.getRegex("\"contentUrl\"\\s*:\\s*\"(https?://[^\"]+)\"").getMatch(0);
        }
        /* Find filename */
        String filename = br.getRegex("property=('|\")og:title\\1\\s*content=('|\")([^\"']+)\\2").getMatch(2);
        if (StringUtils.isEmpty(filename)) {
            filename = br.getRegex("<title>([^<]+)</title>").getMatch(0);
        }
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            final String ext = getFileNameExtensionFromString(dllink, default_extension);
            filename = this.applyFilenameExtension(filename, ext);
            link.setFinalFileName(filename);
        } else if (dllink != null) {
            /* Last resort: filename from direct URL */
            filename = getFileNameFromURL(new java.net.URL(dllink));
            link.setName(filename);
        }
        if (!link.isSizeSet() && !PluginEnvironment.DOWNLOAD.isCurrentPluginEnvironment()) {
            basicLinkCheck(br.cloneBrowser(), br.createGetRequest(dllink), link, filename, getFileNameExtensionFromString(dllink, default_extension));
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, true, 0);
        handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}
