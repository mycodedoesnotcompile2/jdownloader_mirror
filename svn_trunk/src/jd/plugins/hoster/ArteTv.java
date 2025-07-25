//    jDownloader - Downloadmanager
//    Copyright (C) 2012  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.io.IOException;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.utils.StringUtils;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 51231 $", interfaceVersion = 3, names = { "arte.tv" }, urls = { "" })
public class ArteTv extends PluginForHost {
    @SuppressWarnings("deprecation")
    public ArteTv(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/sites/corporate/de/allgemeine-nutzungsbedingungen/";
    }

    @Override
    public String getPluginContentURL(final DownloadLink link) {
        final String directurl = getDirectURL(link);
        if (directurl != null) {
            return directurl;
        } else {
            return super.getPluginContentURL(link);
        }
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, false);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws Exception {
        br.setFollowRedirects(true);
        final String directurl = this.getDirectURL(link);
        if (!StringUtils.isEmpty(directurl) && !isDownload && !this.isHLS(link)) {
            URLConnectionAdapter con = null;
            try {
                con = br.openHeadConnection(directurl);
                handleConnectionErrors(br, con);
                if (con.getCompleteContentLength() > 0) {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
                findAndSetMd5Hash(link, con);
            } finally {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
        }
        return AvailableStatus.TRUE;
    }

    @Override
    protected void throwConnectionExceptions(Browser br, URLConnectionAdapter con) throws PluginException, IOException {
        if (con.getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
        } else if (con.getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Video broken?");
        }
    }

    private String getDirectURL(final DownloadLink link) {
        /* TODO: Remove this backward compatibility in 01-2023 */
        final String legacy_directURL = link.getStringProperty("directURL");
        if (legacy_directURL != null) {
            return legacy_directURL;
        } else {
            return link.getPluginPatternMatcher();
        }
    }

    private boolean isHLS(final DownloadLink link) {
        /* TODO: Remove this backward compatibility in 01-2023 */
        final String legacy_quality_intern = link.getStringProperty("quality_intern");
        if (StringUtils.contains(legacy_quality_intern, "hls_")) {
            return true;
        } else if (getDirectURL(link).contains(".m3u8")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        final String directurl = this.getDirectURL(link);
        if (directurl == null) {
            /* This should never happen! */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (this.isHLS(link)) {
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, directurl);
            dl.startDownload();
        } else {
            br.setFollowRedirects(true);
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, directurl, true, 0);
            handleConnectionErrors(br, dl.getConnection());
            findAndSetMd5Hash(link, dl.getConnection());
            dl.startDownload();
        }
    }

    private void findAndSetMd5Hash(final DownloadLink link, final URLConnectionAdapter con) {
        final String etag = con.getRequest().getResponseHeader("etag");
        if (etag != null) {
            try {
                final String md5 = etag.replace("\"", "").split(":")[0];
                if (md5.matches("[A-Fa-f0-9]{32}")) {
                    link.setMD5Hash(md5);
                }
            } catch (final Throwable ignore) {
            }
        }
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public String getDescription() {
        return "JDownloader's ARTE Plugin helps downloading videoclips from arte.tv. Arte provides different video qualities.";
    }

    @Override
    public boolean allowHandle(final DownloadLink link, final PluginForHost plugin) {
        /* No not allow multihost plugins to handle items from this plugin. */
        return link.getHost().equalsIgnoreCase(plugin.getHost());
    }
}