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

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.utils.StringUtils;
import org.appwork.utils.net.URLHelper;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 51332 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { jd.plugins.decrypter.NhentaiNetCrawler.class })
public class NhentaiNet extends PluginForHost {
    public NhentaiNet(PluginWrapper wrapper) {
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

    /* DEV NOTES */
    // other:
    public static final String EXT_DEFAULT = ".jpg";

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(jd.plugins.decrypter.NhentaiNetCrawler.getPluginDomains());
    }

    @Override
    public String rewriteHost(String host) {
        return this.rewriteHost(jd.plugins.decrypter.NhentaiNetCrawler.getPluginDomains(), host);
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(jd.plugins.decrypter.NhentaiNetCrawler.getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : jd.plugins.decrypter.NhentaiNetCrawler.getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/g/(\\d+)/(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + this.getHost() + "/";
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

    private String getFID(final DownloadLink link) {
        if (link == null || link.getPluginPatternMatcher() == null) {
            return null;
        } else {
            final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
            return urlinfo.getMatch(0) + "_" + urlinfo.getMatch(1);
        }
    }

    private final String CACHED_URL = "CACHED_URL";

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        if (!link.isNameSet()) {
            link.setName(this.getFID(link) + EXT_DEFAULT);
        }
        this.setBrowserExclusive();
        String dllink = getDirecturl(link, br);
        if (dllink != null) {
            if (checkDownloadableRequest(link, br, br.createHeadRequest(dllink), 10, true) != null) {
                return AvailableStatus.TRUE;
            } else {
                link.removeProperty(CACHED_URL);
            }
        }
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        dllink = getDirecturl(link, br);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String urlExtension = getFileNameExtensionFromURL(dllink);
        final String fileExtension = getFileNameExtensionFromString(link.getName());
        if (urlExtension != null && !StringUtils.equalsIgnoreCase(urlExtension, fileExtension)) {
            final String fixExtension = link.getName().replaceFirst(fileExtension + "$", urlExtension);
            link.setFinalFileName(fixExtension);
        }
        if (checkDownloadableRequest(link, br, br.createHeadRequest(dllink), 10, true) != null) {
            link.setProperty(CACHED_URL, dllink);
            return AvailableStatus.TRUE;
        } else {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    private String getDirecturl(final DownloadLink link, final Browser br) throws IOException {
        String dllink = link.getStringProperty(CACHED_URL);
        if (dllink == null && br.getRequest() != null) {
            dllink = br.getRegex("(https?://[^/]+/galleries/\\d+/\\d+\\.(?:jpe?g|png|webp|gif))").getMatch(0);
            if (dllink == null) {
                /* 2022-08-11 */
                dllink = br.getRegex("href=\"/g/\\d+/\\d+/?\">\\s*<img src=\"(https?://[^\"]+)\"").getMatch(0);
                if (dllink == null) {
                    /* 2025-07-31 */
                    dllink = br.getRegex("href=\"/g/\\d+/\\d+/?\">\\s*<img src=\"([^\"]*/galleries/\\d+/\\d+\\.(?:jpe?g|png|webp|gif))\"").getMatch(0);
                    if (dllink == null) {
                        /* nhentai.xxx */
                        dllink = br.getRegex("data-src\\s*=\\s*\"([^\"]*/\\d+\\.(?:jpe?g|png|webp|gif))\"").getMatch(0);
                    }
                }
            }
        }
        if (dllink == null) {
            return null;
        }
        return URLHelper.parseLocation(new URL(link.getPluginPatternMatcher()), dllink);
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        final String dllink = getDirecturl(link, br);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, false, 1);
        if (!this.looksLikeDownloadableContent(dl.getConnection())) {
            link.removeProperty(CACHED_URL);
            br.followConnection(true);
            if (dl.getConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
            } else if (dl.getConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
            } else {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error");
            }
        }
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
        link.removeProperty(CACHED_URL);
    }
}