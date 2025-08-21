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

import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.YetiShareCore;

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
import jd.plugins.components.SiteType.SiteTemplate;
import jd.plugins.hoster.FireloadCom;

@DecrypterPlugin(revision = "$Revision: 51346 $", interfaceVersion = 3, names = {}, urls = {})
public class FireloadComFolder extends PluginForDecrypt {
    public FireloadComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return FireloadCom.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/folder/([a-zA-Z0-9]{32,})/([^/]+)?");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final Regex folderInfo = new Regex(param.getCryptedUrl(), this.getSupportedLinks());
        final String folderID = folderInfo.getMatch(0);
        String folderNameFromAddedURL = folderInfo.getMatch(1);
        if (folderNameFromAddedURL != null) {
            folderNameFromAddedURL = Encoding.htmlDecode(folderNameFromAddedURL).trim();
        }
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.getURL().contains(folderID)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String folderNameFromBrowserURL = new Regex(br.getURL(), this.getSupportedLinks()).getMatch(1);
        final int maxItemsPerPage = 100;
        final String folderIDFromHTML = br.getRegex("nodeId'\\)\\.val\\('(\\d+)'\\)").getMatch(0);
        String currentFolderName = br.getRegex("<h2>\\s*Folder \\'([^\\']+)'\\s*</h2>").getMatch(0);
        if (currentFolderName == null) {
            currentFolderName = folderNameFromBrowserURL;
            if (currentFolderName == null) {
                /* Fallback */
                currentFolderName = folderNameFromAddedURL;
            }
        }
        if (currentFolderName == null) {
            /* Final fallback */
            currentFolderName = folderID;
        }
        currentFolderName = Encoding.htmlDecode(currentFolderName).trim();
        String filePath = this.getAdoptedCloudFolderStructure("");
        if (filePath.length() > 0) {
            filePath += "/";
        }
        filePath += currentFolderName;
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(filePath);
        /* Customized YetiShareFolder handling */
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("url_hash", folderID);
        query.appendEncoded("nodeId", folderIDFromHTML);
        query.appendEncoded("filterText", "");
        query.appendEncoded("filterOrderBy", "order_by_filename_asc");
        query.appendEncoded("perPage", Integer.toString(maxItemsPerPage));
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final HashSet<String> dupes = new HashSet<String>();
        int page = 0;
        pagination: do {
            query.addAndReplace("pageStart", Integer.toString(page));
            br.postPage("/ajax/_view_folder_v2_file_listing.ajax.php", query);
            int numberofNewItemsThisPage = 0;
            final String[] fileHTMLSnippets = br.getRegex("(<li.*?</li>)").getColumn(0);
            if (fileHTMLSnippets != null && fileHTMLSnippets.length > 0) {
                for (final String html : fileHTMLSnippets) {
                    final String url = new Regex(html, "dtfullurl\\s*=\\s*\"(https?[^\"]+)\"").getMatch(0);
                    final String filename = new Regex(html, "dtfilename\\s*=\\s*\"([^\"]+)\"").getMatch(0);
                    final String uploaddateStr = new Regex(html, "dtuploaddate\\s*=\\s*\"([^\"]+)\"").getMatch(0);
                    final String filesizeStr = new Regex(html, "dtsizeraw\\s*=\\s*\"(\\d+)\"").getMatch(0);
                    final String internalFileID = new Regex(html, "fileId\\s*=\\s*\"(\\d+)\"").getMatch(0);
                    if (StringUtils.isEmpty(url) || StringUtils.isEmpty(internalFileID)) {
                        /* Skip invalid items */
                        logger.info("Skipping invalid item: " + html);
                        continue;
                    }
                    if (!dupes.add(url)) {
                        continue;
                    }
                    numberofNewItemsThisPage++;
                    final DownloadLink dl = createDownloadlink(url);
                    if (!StringUtils.isEmpty(filename)) {
                        dl.setName(Encoding.htmlDecode(filename).trim());
                    }
                    if (!StringUtils.isEmpty(filesizeStr)) {
                        dl.setDownloadSize(Long.parseLong(filesizeStr));
                    }
                    dl.setProperty(org.jdownloader.plugins.components.YetiShareCore.PROPERTY_INTERNAL_FILE_ID, internalFileID);
                    if (uploaddateStr != null) {
                        /* 2020-11-26: For Packagizer/EventScripter - not used anywhere else. */
                        dl.setProperty(YetiShareCore.PROPERTY_UPLOAD_DATE_RAW, uploaddateStr);
                    }
                    /* We know for sure that this file is online! */
                    dl.setAvailable(true);
                    if (filePath.length() > 0) {
                        dl.setRelativeDownloadFolderPath(filePath);
                    }
                    dl._setFilePackage(fp);
                    ret.add(dl);
                    distribute(dl);
                }
            }
            /* Now crawl subfolders inside this folder */
            final String[] folderHashes = br.getRegex("(/folder/[a-zA-Z0-9]{32,})").getColumn(0);
            for (String folderHash : folderHashes) {
                if (folderHash.equalsIgnoreCase(folderID)) {
                    /* Don't re-add the folder we're just crawling! */
                    continue;
                }
                final String folderURL = br.getURL(folderHash).toExternalForm();
                final DownloadLink folder = this.createDownloadlink(folderURL);
                /*
                 * 2020-11-13: Not required. If a "root" folder is password-protected, all files within it are usually not password
                 * protected (WTF) and/or can require another password which can be different. Also subfolders inside folders will usually
                 * not require a password at all but users CAN set a (different) password on them.
                 */
                // folder.setDownloadPassword(passCode);
                ret.add(folder);
                distribute(folder);
            }
            logger.info("Crawled page " + page + "| Found items so far: " + ret.size());
            if (numberofNewItemsThisPage < maxItemsPerPage) {
                logger.info("Stopping because: Reached end?");
                break pagination;
            }
        } while (!this.isAbort());
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.MFScripts_YetiShare;
    }
}
