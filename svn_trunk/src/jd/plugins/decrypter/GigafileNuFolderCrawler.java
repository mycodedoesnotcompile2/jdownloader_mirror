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
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;

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
import jd.plugins.hoster.GigafileNu;

@DecrypterPlugin(revision = "$Revision: 50512 $", interfaceVersion = 3, names = {}, urls = {})
public class GigafileNuFolderCrawler extends PluginForDecrypt {
    public GigafileNuFolderCrawler(PluginWrapper wrapper) {
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
        ret.add(new String[] { "gigafile.nu" });
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
            ret.add("https?://\\d+\\." + buildHostsPatternPart(domains) + "/(\\d+-[a-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String contentID = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String filesJson = br.getRegex("var files = (\\[.*?\\]);").getMatch(0);
        String fileIDForDownload = null;
        final GigafileNu hosterplugin = (GigafileNu) this.getNewPluginForHostInstance(this.getHost());
        final HashSet<String> fileIDs = new HashSet<String>();
        if (filesJson != null) {
            final String[] filenames = br.getRegex("alt=\"スキャン中\" style=\"height: 18px;\">\\s*</span>\\s*<span class=\"\">([^<]+)</span>").getColumn(0);
            final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) restoreFromString(filesJson, TypeRef.OBJECT);
            final boolean foundFilenames;
            if (filenames != null && filenames.length == ressourcelist.size()) {
                foundFilenames = true;
            } else {
                foundFilenames = false;
                logger.warning("Failed to find names of individual files");
            }
            for (int i = 0; i < ressourcelist.size(); i++) {
                final Map<String, Object> file = ressourcelist.get(i);
                final String fileID = file.get("file").toString();
                if (!fileIDs.add(fileID)) {
                    /* Skip duplicates */
                    continue;
                }
                final DownloadLink link = this.createDownloadlink(contenturl);
                link.setDefaultPlugin(hosterplugin);
                link.setHost(this.getHost());
                /* There is no specific single file URL -> set "folder" URL as contenturl. */
                link.setContentUrl(contenturl);
                if (foundFilenames) {
                    String filename = filenames[i];
                    filename = Encoding.htmlDecode(filename).trim();
                    link.setName(filename);
                    link.setProperty(GigafileNu.PROPERTY_FILE_NAME_FROM_CRAWLER, filename);
                }
                link.setDownloadSize(((Number) file.get("size")).longValue());
                link.setProperty(GigafileNu.PROPERTY_FILE_ID, fileID);
                link.setAvailable(true);
                ret.add(link);
            }
            if (ressourcelist.size() == 1) {
                /* Single file -> Download that, else .zip of all files. */
                final Map<String, Object> filemap = ressourcelist.get(0);
                fileIDForDownload = filemap.get("file").toString();
                logger.info("Downloading single file: " + fileIDForDownload);
            } else {
                logger.info("This is a folder containing " + ressourcelist.size() + " files --> Download .zip file containing all files");
            }
        }
        final String singleFileID = br.getRegex("var file = \"([^\"]+)").getMatch(0);
        if (singleFileID != null && fileIDs.add(singleFileID)) {
            final String fileSizeBytesStr = br.getRegex("var size = (\\d+);").getMatch(0);
            final DownloadLink link = this.createDownloadlink(contenturl);
            link.setDefaultPlugin(hosterplugin);
            link.setHost(this.getHost());
            /* There is no specific single file URL -> set "folder" URL as contenturl. */
            link.setContentUrl(contenturl);
            String filename = br.getRegex("onclick=\"download\\([^\\)]+\\);\">([^<>\"]+)</p>").getMatch(0);
            if (filename != null) {
                filename = Encoding.htmlDecode(filename).trim();
                link.setName(filename);
                link.setProperty(GigafileNu.PROPERTY_FILE_NAME_FROM_CRAWLER, filename);
            }
            if (fileSizeBytesStr != null) {
                link.setDownloadSize(Long.parseLong(fileSizeBytesStr));
            } else {
                logger.warning("Failed to find size of single file");
            }
            link.setProperty(GigafileNu.PROPERTY_FILE_ID, singleFileID);
            link.setAvailable(true);
            ret.add(link);
        }
        if (ret.isEmpty()) {
            if (!br.containsHTML("download\\('" + contentID) && !br.containsHTML("var file = \"" + contentID)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(contentID);
        fp.setPackageKey(this.getHost() + "/folder/" + contentID);
        fp.addLinks(ret);
        return ret;
    }
}
