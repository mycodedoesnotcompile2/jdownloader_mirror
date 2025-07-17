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
package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.ImageNetzDe;

@DecrypterPlugin(revision = "$Revision: 51193 $", interfaceVersion = 3, names = {}, urls = {})
public class ImagenetzDeCrawler extends PluginForDecrypt {
    public ImagenetzDeCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "imagenetz.de" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(filesgroup/[a-f0-9]{32}\\.html|download/[a-f0-9]{32}\\.html|[A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String folderID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        final String contenturl = param.getCryptedUrl();
        final boolean isFolderLink = StringUtils.contains(contenturl, "filesgroup");
        br.getPage(contenturl);
        checkOffline(br, folderID);
        final ImageNetzDe hosterplugin = (ImageNetzDe) this.getNewPluginForHostInstance(this.getHost());
        final String[] filelinks = br.getRegex("gDownloadLink' value='(https?://(?:www\\.)?imagenetz\\.de/[A-Za-z0-9]+)'").getColumn(0);
        if (isFolderLink && (filelinks == null || filelinks.length == 0)) {
            /* We know that this is a folder link and got no results -> Website must have changed and thus crawler is broken */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (filelinks != null && filelinks.length > 0) {
            final String[][] filenamesAndSizes = br.getRegex("href=\"#file\\d+\"[^>]*>([^<]+)<small>([^<]+)</small>").getMatches();
            if (filenamesAndSizes == null || filenamesAndSizes.length == 0) {
                logger.warning("Failed to find filename/filesize information");
            }
            String folder_title = br.getRegex("<h3 class='panel-title text-strong'[^>]*>([^<]+)</h3>").getMatch(0);
            int index = 0;
            for (final String singleLink : filelinks) {
                final DownloadLink file = new DownloadLink(hosterplugin, this.getHost(), singleLink);
                if (filenamesAndSizes != null && filenamesAndSizes.length == filelinks.length) {
                    final String[] this_file_info = filenamesAndSizes[index];
                    final String filename = this_file_info[0];
                    final String filesizeStr = this_file_info[1];
                    file.setName(Encoding.htmlDecode(filename).trim());
                    file.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                }
                file.setAvailable(true);
                ret.add(file);
                index++;
            }
            if (folder_title != null) {
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(Encoding.htmlDecode(folder_title).trim());
                fp.addLinks(ret);
            }
        } else {
            /* Looks like single file or invalid/offline link */
            final DownloadLink file = new DownloadLink(hosterplugin, this.getHost(), br.getURL());
            ImageNetzDe.parseFileInfo(br, file);
            file.setAvailable(true);
            ret.add(file);
        }
        return ret;
    }

    public static void checkOffline(final Browser br, final String contentID) throws PluginException {
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("Diese Datei existiert nicht mehr")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (contentID != null && !br.getURL().contains(contentID)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }
}
