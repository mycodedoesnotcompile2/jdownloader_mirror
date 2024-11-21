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
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 50191 $", interfaceVersion = 2, names = { "opendrive.com" }, urls = { "https?://(?:www\\.)?opendrive\\.com/folders\\?[A-Za-z0-9]+|https?://od\\.lk/(?:fl|s)/[A-Za-z0-9]+(?:\\?folderpath=[a-zA-Z0-9_/\\+\\=\\-%]+)?" })
public class OpenDriveComDecrypter extends PluginForDecrypt {
    public OpenDriveComDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String folderurl = param.getCryptedUrl().replaceFirst("(?i)http://", "https://");
        final String folderid = new Regex(folderurl, "([A-Za-z0-9\\-_]+)(\\?folderpath=.+)?$").getMatch(0);
        this.br.setFollowRedirects(true);
        br.getPage("https://od.lk/fl/" + folderid);
        if (br.getHttpConnection().getResponseCode() == 400 || br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String csrftoken = br.getRegex("data\\-csrftoken=\"([^<>\"]+)\"").getMatch(0);
        if (csrftoken == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        jd.plugins.hoster.OpenDriveCom.prepBRAjax(this.br);
        br.getHeaders().put("Origin", "https://od.lk");
        br.getHeaders().put("X-Ajax-CSRF-Token", csrftoken);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final FilePackage fp = FilePackage.getInstance();
        fp.setPackageKey(this.getHost() + "/folder/" + folderid);
        final HashSet<String> dupes = new HashSet<String>();
        int page = 1;
        int offset = 0;
        String path = null;
        final int maxItemsPerPage = 100;
        String last_request_time = "0";
        pagination: do {
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("action", "files.load-folder-content");
            query.appendEncoded("folder_id", folderid);
            query.appendEncoded("with_breadcrumbs", "1");
            query.appendEncoded("last_request_time", last_request_time);
            query.appendEncoded("public", "1");
            query.appendEncoded("offset", Integer.toString(offset));
            query.appendEncoded("order_by", "name");
            query.appendEncoded("order_type", "asc");
            query.appendEncoded("search_query", "");
            br.postPage("/ajax", query);
            final Map<String, Object> root = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Number errorcode = (Number) JavaScriptEngineFactory.walkJson(root, "error/code");
            if (br.getHttpConnection().getResponseCode() == 400 || br.getHttpConnection().getResponseCode() == 404 || errorcode != null) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final List<Map<String, Object>> subfolders = (List<Map<String, Object>>) root.get("Folders");
            final List<Map<String, Object>> files = (List<Map<String, Object>>) root.get("Files");
            if (subfolders.isEmpty() && files.isEmpty() && ret.isEmpty()) {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
            }
            if (page == 1) {
                /* Init some stuff */
                String currentFolderTitle = (String) root.get("Name");
                // final String name_of_previous_folder = (String) JavaScriptEngineFactory.walkJson(entries, "Breadcrumbs/{0}/Name");
                if (currentFolderTitle == null) {
                    currentFolderTitle = folderid;
                }
                path = new Regex(folderurl, "folderpath=(.+)").getMatch(0);
                if (path != null) {
                    path = Encoding.Base64Decode(path);
                    path += "/" + currentFolderTitle;
                } else {
                    /* Root */
                    path = currentFolderTitle;
                }
                fp.setName(path);
            }
            int numberofNewItemsThisPage = 0;
            for (final Map<String, Object> file : files) {
                /* Do not use this as linkid as id inside our url is the one we prefer (also unique). */
                // final String fileid = (String)entries.get("FileId");
                final String fileID = file.get("FileId").toString();
                if (!dupes.add(fileID)) {
                    continue;
                }
                numberofNewItemsThisPage++;
                final String url = file.get("Link").toString();
                final String directurl = file.get("DownloadLink").toString();
                final String filename = file.get("Name").toString();
                final Object filesize = file.get("Size");
                final DownloadLink dl = this.createDownloadlink(url);
                dl.setRelativeDownloadFolderPath(path);
                dl.setName(filename);
                if (filesize != null) {
                    if (filesize instanceof Number) {
                        dl.setVerifiedFileSize(((Number) filesize).longValue());
                    } else {
                        dl.setVerifiedFileSize(Long.parseLong(filesize.toString()));
                    }
                }
                if (!StringUtils.isEmpty(directurl)) {
                    dl.setProperty("directurl", directurl);
                }
                dl.setAvailable(true);
                dl._setFilePackage(fp);
                ret.add(dl);
                distribute(dl);
            }
            for (final Map<String, Object> folder : subfolders) {
                final String folderID = folder.get("FolderID").toString();
                if (!dupes.add(folderID)) {
                    continue;
                }
                numberofNewItemsThisPage++;
                String url = folder.get("Link").toString();
                url += "?folderpath=" + Encoding.Base64Encode(path);
                final DownloadLink dl = this.createDownloadlink(url);
                ret.add(dl);
                distribute(dl);
            }
            logger.info("Crawled page " + page + " | New items this page: " + numberofNewItemsThisPage + " | Total: " + ret.size());
            if (this.isAbort()) {
                logger.info("Aborted by user");
                throw new InterruptedException();
            } else if (numberofNewItemsThisPage < maxItemsPerPage) {
                logger.info("Stopping because: Reached end");
                break pagination;
            } else {
                /* Continue to next page */
                page++;
                offset += numberofNewItemsThisPage;
                last_request_time = root.get("DirUpdateTime").toString();
            }
        } while (!this.isAbort());
        return ret;
    }
}
