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
import java.util.ArrayList;
import java.util.List;

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

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 51189 $", interfaceVersion = 3, names = {}, urls = {})
public class PixhostTo extends PluginForHost {
    public PixhostTo(PluginWrapper wrapper) {
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
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_HOST, LazyPlugin.FEATURE.IMAGE_GALLERY };
    }

    /* DEV NOTES */
    // Tags: pichost
    // protocol: no https
    // other: related to: PixhstCom (pxhst.co)

    /* Extension which will be used if no correct extension is found */
    private static final String  default_extension = ".jpg";
    /* Connection stuff */
    private static final boolean free_resume       = false;
    private static final int     free_maxchunks    = 1;
    private String               dllink            = null;

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "pixhost.to" });
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
            ret.add("https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "/(?:show|thumbs|images)/((\\d+)/(\\d+)_([^/<>#]+))");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String imageid = getFID(link);
        if (imageid != null) {
            return "pixhostto://image/" + imageid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        final String id1 = urlinfo.getMatch(1);
        final String id2 = urlinfo.getMatch(2);
        return id1 + "_ " + id2;
    }

    private String getFullsizeImageContenturl(final DownloadLink link) {
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        return "https://" + getHost() + "/show/" + urlinfo.getMatch(1) + "/" + urlinfo.getMatch(2) + "_" + urlinfo.getMatch(3);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return requestFileInformation(link, false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws IOException, PluginException {
        dllink = null;
        final Regex urlinfo = new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks());
        // file name from URL should include fileID for
        // galleries all images may have same name but
        // different fileID
        final String filenameFromURL = urlinfo.getMatch(2) + "_" + urlinfo.getMatch(3);
        if (!link.isNameSet()) {
            link.setName(filenameFromURL);
        }
        this.setBrowserExclusive();
        br.getPage(getFullsizeImageContenturl(link));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Picture doesn\\'t exist")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final boolean isPartOfGallery = br.containsHTML("class=\"show-gallery\"");
        /* 2019-01-31: It is better to grab the filename via URL! */
        String filename = br.getRegex("title\\s*:\\s*'([^<>\"\\']+)'").getMatch(0);
        if (filename == null) {
            filename = br.getRegex("class=\"fa fa-picture-o\"[^>]*>\\s*</i>([^<]+)<").getMatch(0);
        }
        if (isPartOfGallery) {
            /*
             * Prefer filename from URL as it is unique. For items which are part of a gallery, the title given in html code may be the same
             * for all items.
             */
            filename = filenameFromURL;
        }
        /*
         * Picture might be part of a gallery and website will have an array of all of them --> Make sure that we grab the correct
         * downloadurl.
         */
        final String json_for_current_object = br.getRegex("\\{[^\\}]*?" + filenameFromURL + "[^\\}]*?\\}").getMatch(-1);
        if (json_for_current_object != null) {
            dllink = new Regex(json_for_current_object, "src\\s*?:\\s*?\\'(http[^\"\\']+)\\'").getMatch(0);
        }
        /* Fallback */
        if (dllink == null) {
            dllink = br.getRegex("(https?://[^/]+/images/[^<>\"\\']+)").getMatch(0);
        }
        String ext = null;
        if (filename != null) {
            filename = Encoding.htmlDecode(filename);
            filename = filename.trim();
            if (!StringUtils.isEmpty(dllink)) {
                ext = getFileNameExtensionFromString(dllink, default_extension);
            } else {
                ext = default_extension;
            }
            filename = applyFilenameExtension(filename, ext);
            link.setFinalFileName(filename);
        }
        if (!StringUtils.isEmpty(dllink)) {
            dllink = Encoding.htmlOnlyDecode(dllink);
            basicLinkCheck(br.cloneBrowser(), br.createHeadRequest(dllink), link, filename, ext);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
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
