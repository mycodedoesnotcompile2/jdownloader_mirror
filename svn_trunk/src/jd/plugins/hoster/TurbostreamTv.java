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

import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 51050 $", interfaceVersion = 3, names = {}, urls = {})
public class TurbostreamTv extends PluginForHost {
    public TurbostreamTv(PluginWrapper wrapper) {
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
        ret.add(new String[] { "turbostream.tv" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/watch\\.php\\?id=(\\d+)");
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
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        final String extDefault = ".mp4";
        final String fallbackFilename = this.getFID(link) + extDefault;
        if (!link.isNameSet()) {
            /* Fallback */
            link.setFinalFileName(fallbackFilename);
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* 2025-05-07: Typically with plaintext response with text content "Video not found.". */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* 2025-05-07: Looks like file titles are only displayed for uploaders of the content. */
        String filename = br.getRegex("<h5 style[^>]*>([^<]+)</h5>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
            filename = this.correctOrApplyFileNameExtension(filename, extDefault, null);
            link.setFinalFileName(filename);
        } else {
            link.setFinalFileName(fallbackFilename);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final String fid = this.getFID(link);
        try {
            final String viewToken = br.getRegex("const viewToken = \"([a-f0-9]{64})\";").getMatch(0);
            if (viewToken == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find viewToken");
            }
            final Browser brc = br.cloneBrowser();
            brc.getPage("/record_view.php?id=" + fid + "&token=" + viewToken);
        } catch (final Throwable e) {
            logger.log(e);
            logger.warning("View count handling failed");
        }
        final String officialVideoDownload = br.getRegex("\"(download_page\\.php\\?video_id=" + fid + ")").getMatch(0);
        if (officialVideoDownload != null) {
            logger.info("Performing official video download");
            br.getPage(officialVideoDownload);
            final Form dlform = br.getFormbyKey("video_id");
            if (dlform == null) {
                if (br.getHttpConnection().getResponseCode() == 403) {
                    /* Along with plaintext response "Downloads are not allowed for this video." */
                    /*
                     * This should never happen because the download button existed before. It could theoretically happen if the uploader
                     * decided to disable official video downloads in exactly the moment where we are trying to perform an official video
                     * download.
                     */
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Downloads are not allowed for this video");
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dlform, this.isResumeable(link, null), this.getMaxChunks(link, null));
        } else {
            logger.info("Performing stream download");
            final String dllink = "/stream.php?id=" + fid + "&type=video";
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        }
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