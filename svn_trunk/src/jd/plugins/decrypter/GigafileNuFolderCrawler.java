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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.config.GigafileNuConfig;
import org.jdownloader.plugins.components.config.GigafileNuConfig.CrawlMode;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.GigafileNu;

@DecrypterPlugin(revision = "$Revision: 51775 $", interfaceVersion = 3, names = {}, urls = {})
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
        final String content_id_from_url = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        final String filesJson = br.getRegex("var files = (\\[.*?\\]);").getMatch(0);
        final GigafileNu hosterplugin = (GigafileNu) this.getNewPluginForHostInstance(this.getHost());
        long totalFilesize = 0;
        final Map<String, DownloadLink> filesFolders = new HashMap<String, DownloadLink>();
        if (filesJson != null) {
            final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) restoreFromString(filesJson, TypeRef.OBJECT);
            final String[] matomete_file_names = br.getRegex("class=\"matomete_file_name[^\"]*\"[^>]*>(?:\\s*<span[^>]*\"unchecked_filename_filter\"[^<]*</span>)?\\s*([^<]*?)\\s*</span>").getColumn(0);
            for (int i = 0; i < ressourcelist.size(); i++) {
                final Map<String, Object> file = ressourcelist.get(i);
                final String fileID = file.get("file").toString();
                final long filesize = ((Number) file.get("size")).longValue();
                final DownloadLink link = this.createDownloadlink(contenturl);
                link.setDefaultPlugin(hosterplugin);
                link.setHost(this.getHost());
                /* There is no specific single file URL -> set "folder" URL as contenturl. */
                link.setContentUrl(contenturl);
                String filename = matomete_file_names != null && matomete_file_names.length == ressourcelist.size() ? matomete_file_names[i] : null;
                if (!GigafileNu.isFilename(filename)) {
                    filename = br.getRegex("download\\('" + Pattern.quote(fileID) + "[^>]*>\\s*(.*?)\\s*<").getMatch(0);
                }
                if (!GigafileNu.isFilename(filename)) {
                    filename = null;
                }
                if (StringUtils.isNotEmpty(filename)) {
                    filename = Encoding.htmlDecode(filename).trim();
                    link.setName(filename);
                    link.setProperty(GigafileNu.PROPERTY_FILE_NAME_FROM_CRAWLER, filename);
                    link.setAvailableStatus(AvailableStatus.TRUE);
                } else {
                    /* Fallback: Set dummy filename to avoid all results having the same file name. */
                    link.setName(fileID);
                    /* Do not set online status here so that hoster plugin can find the real filenames. */
                    link.setAvailableStatus(AvailableStatus.UNCHECKED);
                }
                link.setVerifiedFileSize(filesize);
                link.setProperty(GigafileNu.PROPERTY_FILE_ID, fileID);
                totalFilesize += filesize;
                if (!filesFolders.containsKey(fileID)) {
                    filesFolders.put(fileID, link);
                }
            }
        }
        DownloadLink combinedZipOrFile = null;
        boolean singleFileIsCombinedZip = false;
        final String singleFileID = br.getRegex("var file = \"([^\"]+)").getMatch(0);
        if (singleFileID != null && !filesFolders.containsKey(singleFileID)) {
            combinedZipOrFile = this.createDownloadlink(contenturl);
            combinedZipOrFile.setDefaultPlugin(hosterplugin);
            combinedZipOrFile.setHost(this.getHost());
            /* There is no specific single file URL -> set "folder" URL as contenturl. */
            combinedZipOrFile.setContentUrl(contenturl);
            String filename = br.getRegex("matomete_zip_filename\"[^>]*>\\s*(.*?)\\s*<").getMatch(0);
            /*
             * If this is set to true, this means that no usable filename was found here and it will need to be fetched by the hoster plugin
             * during linkcheck.
             */
            if (!GigafileNu.isFilename(filename)) {
                filename = br.getRegex("download\\('" + Pattern.quote(singleFileID) + "[^>]*>\\s*(.*?)\\s*<").getMatch(0);
            }
            if (!GigafileNu.isFilename(filename)) {
                filename = null;
            }
            if (StringUtils.isNotEmpty(filename)) {
                filename = Encoding.htmlDecode(filename).trim();
                combinedZipOrFile.setName(filename);
                combinedZipOrFile.setProperty(GigafileNu.PROPERTY_FILE_NAME_FROM_CRAWLER, filename);
                combinedZipOrFile.setAvailableStatus(AvailableStatus.TRUE);
            } else {
                /* Fallback */
                /* Set .zip extension as we just assume that this is the .zip file containing all other files. */
                combinedZipOrFile.setName(singleFileID + ".zip");
                /* Do not set online status here so that hoster plugin can find the real filenames. */
                combinedZipOrFile.setAvailableStatus(AvailableStatus.UNCHECKED);
            }
            final String fileSizeBytesStr = br.getRegex("var size = (\\d+);").getMatch(0);
            if (fileSizeBytesStr != null) {
                combinedZipOrFile.setVerifiedFileSize(Long.parseLong(fileSizeBytesStr));
            } else if (totalFilesize > 0) {
                /*
                 * Use total size of all other files as fallback value -> Assume that this is the .zip file containing all other files.
                 */
                combinedZipOrFile.setDownloadSize(totalFilesize);
            } else {
                logger.warning("Failed to find size of single file in html code");
            }
            combinedZipOrFile.setProperty(GigafileNu.PROPERTY_FILE_ID, singleFileID);
            if (GigafileNu.isCombinedZipDownload(br, singleFileID)) {
                singleFileIsCombinedZip = true;
                combinedZipOrFile.setProperty(GigafileNu.REPORT_IS_COMPLETE_ZIP, true);
            }
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (filesFolders.size() == 1 && singleFileIsCombinedZip) {
            /*
             * "smart" optimization: if there is only one single file and the .zip containing that same file, return only that single file
             * regardless of the users' settings
             */
            logger.info("Single file and combined .zip");
            ret.addAll(filesFolders.values());
        } else {
            final CrawlMode crawlMode = PluginJsonConfig.get(getConfigInterface()).getCrawlMode().getMode();
            switch (crawlMode) {
            case ALL:
                ret.addAll(filesFolders.values());
                if (combinedZipOrFile != null) {
                    ret.add(combinedZipOrFile);
                }
                break;
            case FILES_FOLDERS:
                ret.addAll(filesFolders.values());
                if (ret.size() == 0 && combinedZipOrFile != null) {
                    // add zip because it's the only file
                    ret.add(combinedZipOrFile);
                }
                break;
            case ZIP:
                if (combinedZipOrFile != null) {
                    ret.add(combinedZipOrFile);
                }
                break;
            default:
                /* Developer mistake */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported CrawlMode:" + crawlMode);
            }
            if (ret.isEmpty()) {
                if (!br.containsHTML("download\\('" + content_id_from_url) && !GigafileNu.isCombinedZipDownload(br, content_id_from_url)) {
                    /* Assume that item is offline */
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else {
                    /* Unknown state */
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(content_id_from_url);
        fp.setPackageKey(this.getHost() + "/folder/" + content_id_from_url);
        fp.addLinks(ret);
        return ret;
    }

    @Override
    public Class<GigafileNuConfig> getConfigInterface() {
        return GigafileNuConfig.class;
    }
}
