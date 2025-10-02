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
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.Up2imgComAlbum;

@HostPlugin(revision = "$Revision: 51603 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { Up2imgComAlbum.class })
public class Up2imgCom extends PluginForHost {
    public Up2imgCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_HOST };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/terms.html";
    }

    private static List<String[]> getPluginDomains() {
        return Up2imgComAlbum.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/album/([a-zA-Z0-9]{6,})/([\\w-]+)/viewimage/([^/]+)/([^/]+)/([^/]+)/([^/]+)\\.html");
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
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        final String image_album_id_base64_encoded = urlinfo.getMatch(3);
        final String image_position_base64_encoded = urlinfo.getMatch(5);
        return image_album_id_base64_encoded + "_" + image_position_base64_encoded;
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        return false;
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        /* 2025-10-01: Limit max chunks to 1 since we're only downloading small image files from this website. */
        return 1;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        if (!link.isNameSet()) {
            /* Fallback */
            link.setName(this.getFID(link) + ".jpg");
        }
        this.setBrowserExclusive();
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        // TODO: Add better offline detection
        final String directurl = this.getDllink(br);
        if (directurl != null) {
            String filename = getFileNameFromURL(br.getURL(directurl));
            if (!StringUtils.isEmpty(filename)) {
                filename = Encoding.htmlDecode(filename).trim();
                final String[] parts = filename.split("\\.");
                if (parts.length > 1) {
                    /* Correct filename: First part is possibly the position with 5-digit-padding, base64 encoded e.g. "00001". */
                    final String part0 = parts[0];
                    final String filename_position = Encoding.Base64Decode(parts[0]);
                    if (filename_position.matches("\\d+")) {
                        filename = filename.replace(part0, filename_position);
                    }
                }
                link.setFinalFileName(filename);
            }
        } else {
            logger.warning("Cannot find filename because: Failed to find directurl");
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link);
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final String dllink = getDllink(br);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find final downloadurl");
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        this.handleConnectionErrors(br, dl.getConnection());
        if (dl.startDownload()) {
            // TODO: Find a better way to detect invalid/offline images and/or delete downloaded file in this case.
            if (link.getView().getBytesLoaded() == 106684) {
                /*
                 * E.g. dummy/offline image file:
                 * https://cdn3.up2img.com/album/MTg2NDA/43597922/showimage/Mg/666wMjkxOTA1MjAyMTEzMg/MDAwMDE.up2img.com.jpg
                 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        }
    }

    private String getDllink(final Browser br) {
        return br.getRegex("<img src=\"([^\"]+)\"[^>]*style=\"max-width:100%; height:auto;\"[^>]*>").getMatch(0);
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