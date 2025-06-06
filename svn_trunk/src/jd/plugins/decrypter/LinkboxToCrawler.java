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
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.LinkboxTo;

@DecrypterPlugin(revision = "$Revision: 51117 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { LinkboxTo.class })
public class LinkboxToCrawler extends PluginForDecrypt {
    public LinkboxToCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return LinkboxTo.getPluginDomains();
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

    private static final Pattern TYPE_NORMAL = Pattern.compile("/a/(d|f|i|s)/([A-Za-z0-9]+)(\\?pid=(\\d+))?", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_SHORT  = Pattern.compile("/f/([A-Za-z0-9]+)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:(?:www|[a-z]{2})\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_NORMAL.pattern() + "|" + TYPE_SHORT.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        String contenturl = param.getCryptedUrl();
        if (new Regex(contenturl, TYPE_SHORT).patternFind()) {
            br.getPage(contenturl);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (!new Regex(br.getURL(), TYPE_NORMAL).patternFind()) {
                /* Assume that item is offline */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            logger.info("Redirect from shorturl: " + contenturl + " --> " + br.getURL());
            contenturl = br.getURL();
        }
        final Regex urlinfo = new Regex(contenturl, TYPE_NORMAL);
        final String folderType = urlinfo.getMatch(0);
        final String folderID = urlinfo.getMatch(1);
        final String subfolderID = urlinfo.getMatch(3);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final int maxItemsPerPage = 50;
        final UrlQuery query = LinkboxTo.getBaseQuery();
        query.appendEncoded("sortField", "name");
        query.appendEncoded("sortAsc", "1");
        query.appendEncoded("pageSize", Integer.toString(maxItemsPerPage));
        // query.add("token", "");
        query.appendEncoded("shareToken", folderID);
        query.appendEncoded("needTpInfo", "1");
        if (folderType.equals("d")) {
            /* 2024-02-19 */
            query.appendEncoded("scene", "comList");
        } else if (folderType.equals("f")) {
            /* Rare type of folder containing a single item. */
            query.appendEncoded("scene", "singleItem");
        } else {
            /* Type s */
            query.appendEncoded("scene", "singleGroup");
        }
        if (subfolderID != null) {
            query.appendEncoded("pid", subfolderID);
        } else {
            query.appendEncoded("pid", "0");
        }
        // query.add("name", "");
        final HashSet<String> dupes = new HashSet<String>();
        int page = 1;
        String path = this.getAdoptedCloudFolderStructure();
        FilePackage packageForFiles = null;
        do {
            query.addAndReplace("pageNo", Integer.toString(page));
            br.getPage("https://www." + getHost() + "/api/file/share_out_list/?" + query.toString());
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Map<String, Object> data = (Map<String, Object>) entries.get("data");
            if (data == null) {
                /* 2023-11-13: e.g. content abused: {"data":null,"msg":"data removed","status":700} */
                /* 2025-06-04: e.g. content expired: {"data":null,"msg":"expire","status":600} */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            /* Check if this folder leads to a single file */
            final String shareType = (String) data.get("shareType");
            if ("singleItem".equalsIgnoreCase(shareType)) {
                /* {"data":{"itemId":"redacted","shareType":"singleItem"},"status":1302} */
                final DownloadLink singleFile = this.createDownloadlink(createFileURL(data.get("itemId").toString()));
                ret.add(singleFile);
                return ret;
            }
            final String currentFolderName = (String) data.get("dirName");
            final List<Map<String, Object>> ressources = (List<Map<String, Object>>) data.get("list");
            if (ressources.isEmpty()) {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, path + "_" + folderID);
            }
            if (page == 1) {
                if (!StringUtils.isEmpty(currentFolderName)) {
                    if (!StringUtils.isEmpty(path)) {
                        path += "/" + currentFolderName;
                    } else {
                        path = currentFolderName;
                    }
                }
                packageForFiles = FilePackage.getInstance();
                if (path != null) {
                    packageForFiles.setName(path);
                }
            }
            int numberofNewItemsOnCurrentPage = 0;
            for (final Map<String, Object> ressource : ressources) {
                final DownloadLink link;
                if (ressource.get("type").equals("dir")) {
                    /* Subfolder */
                    final String thisSubfolderID = ressource.get("id").toString();
                    if (!dupes.add(thisSubfolderID)) {
                        continue;
                    }
                    numberofNewItemsOnCurrentPage++;
                    link = this.createDownloadlink(createFolderURL(folderType, folderID, thisSubfolderID));
                    final String thisFolderName = ressource.get("name").toString();
                    if (path != null) {
                        /* Add this folder to existing path */
                        link.setRelativeDownloadFolderPath(path + "/" + thisFolderName);
                    } else {
                        /* Use this folder as root */
                        link.setRelativeDownloadFolderPath(thisFolderName);
                    }
                } else {
                    /* File */
                    final String fileID = ressource.get("item_id").toString();
                    if (!dupes.add(fileID)) {
                        continue;
                    }
                    numberofNewItemsOnCurrentPage++;
                    link = this.createDownloadlink(createFileURL(fileID));
                    LinkboxTo.parseFileInfoAndSetFilename(this, link, null, ressource);
                    if (!StringUtils.isEmpty(path)) {
                        link.setRelativeDownloadFolderPath(path);
                    }
                    link._setFilePackage(packageForFiles);
                }
                ret.add(link);
                distribute(link);
            }
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            } else if (numberofNewItemsOnCurrentPage < maxItemsPerPage) {
                logger.info("Stopping because: Reached end?");
                break;
            } else {
                /* Continue to next page */
                page++;
            }
        } while (true);
        return ret;
    }

    public static String createFolderURL(final String folderType, final String folderID, final String subfolderID) {
        String url = "https://www.linkbox.to/a/" + folderType + "/" + folderID;
        if (subfolderID != null) {
            url += "?pid=" + subfolderID;
        }
        return url;
    }

    public static String createFileURL(final String fileID) {
        return "https://www.linkbox.to/f-detail/" + fileID;
    }
}
