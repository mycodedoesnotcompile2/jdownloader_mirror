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

import org.appwork.utils.Hash;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
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

@HostPlugin(revision = "$Revision: 51615 $", interfaceVersion = 3, names = {}, urls = {})
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
        // disable so our "File not found" image detection/peek does work
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
            /* 2025-10-02: Do not try to find filesize since their servers never return a content-length header. */
            final boolean findFilesize = false;
            if (findFilesize && !PluginEnvironment.DOWNLOAD.equals(this.getPluginEnvironment()) && !link.isSizeSet()) {
                /* Do not use HEAD request here since we need to look into the file content. */
                // this.basicLinkCheck(br, br.createHeadRequest(directurl), link, filename, null);
                this.basicLinkCheck(br, br.createGetRequest(directurl), link, filename, null);
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

    @Override
    protected boolean looksLikeDownloadableContent(final URLConnectionAdapter urlConnection) {
        if (!super.looksLikeDownloadableContent(urlConnection)) {
            return false;
        }
        try {
            /* try to detect special "File not found" image */
            final int knownImageSize = 106684;
            final byte[] probe = urlConnection.peek(knownImageSize + 1);// +1 more to only check hash on exactly 106684 content length
            if (probe.length != knownImageSize) {
                return true;
            }
            final String hash = Hash.getSHA256(probe);
            // TODO: Change to this value? 4a963b95bf081c3ea02923dceaeb3f8085e1a654fc54840aac61a57a60903fef
            if ("9d452fef9e7a588aeff42ea2f1e145a4f3fac5596898d99994b39db126fd7497".equals(hash)) {
                logger.info("Detected dummy \"File not found\" image");
                return false;
            }
            return true;
        } catch (IOException e) {
            logger.log(e);
        }
        return true;
    }

    @Override
    protected void handleConnectionErrors(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            throwConnectionExceptions(br, con);
            throwFinalConnectionException(br, con);
        }
        /* Looks like downloadable content -> Check for dummy image which we do not want to download. */
        try {
            /* try to detect special "File not found" image */
            final int knownImageSize = 106684;
            final byte[] probe = con.peek(knownImageSize + 1);// +1 more to only check hash on exactly 106684 content length
            if (probe.length != knownImageSize) {
                return;
            }
            final String hash = Hash.getSHA256(probe);
            if ("9d452fef9e7a588aeff42ea2f1e145a4f3fac5596898d99994b39db126fd7497".equals(hash)) {
                /* e.g. /album/MTg2NDA/hardcore-Jasmine-Rouge-George-Titus-Steel/viewimage/Mg/666wMjkxOTA1MjAyMTEzMg/MTQw/MDAwMDE.html */
                logger.info("Detected dummy \"File not found\" image");
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        } catch (IOException e) {
            logger.log(e);
        }
    }

    private void handleDownload(final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link);
        final String dllink = getDllink(br);
        if (StringUtils.isEmpty(dllink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find final downloadurl");
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, this.isResumeable(link, null), this.getMaxChunks(link, null));
        this.handleConnectionErrors(br, dl.getConnection());
        dl.startDownload();
    }

    private String getDllink(final Browser br) {
        /* @Dev: Commented out code down below returns direct-URL that leads to empty dummy image. */
        // if (true) {
        // return "https://cdn1.up2img.com/album/MTg2NDA/54842993/showimage/Mg/666wMjkxOTA1MjAyMTEzMg/MDAwMDE.up2img.com.jpg";
        // }
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