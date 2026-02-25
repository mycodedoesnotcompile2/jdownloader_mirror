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
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.StackstorageCom;

@DecrypterPlugin(revision = "$Revision: 52377 $", interfaceVersion = 3, names = { "stackstorage.com" }, urls = { "https?://([\\w\\-]+)\\.stackstorage\\.com/s/([A-Za-z0-9]+)(\\?dir=([^\\&]+)\\&node\\-id=(\\d+))?" })
public class StackstorageComCrawler extends PluginForDecrypt {
    public StackstorageComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(412, 503);
        final Regex urlinfo = new Regex(contenturl, this.getSupportedLinks());
        final String subdomain = urlinfo.getMatch(0);
        final String folderID = urlinfo.getMatch(1);
        String subdir = urlinfo.getMatch(3);
        if (subdir == null) {
            /* Start from root */
            subdir = "%2F";
        }
        final PluginForHost hosterplugin = this.getNewPluginForHostInstance(this.getHost());
        final String apiurl = "https://" + subdomain + ".stackstorage.com/api/v2/share/" + folderID;
        /* Obtain token / cookie */
        br.postPageRaw(apiurl, "{\"password\":\"\"}");
        if (br.getHttpConnection().getResponseCode() == 401) {
            logger.info("Password protected URLs are not yet supported");
            ret.add(this.createOfflinelink(contenturl, "PASSWORD_PROTECTED"));
            return ret;
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Item offline or account required to access it");
        } else if (br.getHttpConnection().getResponseCode() == 412) {
            throw new AccountRequiredException();
        } else if (br.getHttpConnection().getResponseCode() == 503) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Error 503 offline");
        }
        final String sharetoken = br.getRequest().getResponseHeader("X-Sharetoken");
        final String csrftoken = br.getRequest().getResponseHeader("X-Csrf-Token");
        if (StringUtils.isEmpty(sharetoken) || StringUtils.isEmpty(csrftoken)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.setCookie(br.getURL(), "ShareSession", sharetoken);
        br.getHeaders().put("X-Sharetoken", sharetoken);
        br.getPage(apiurl);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String main_item_id = entries.get("id").toString();
        if (Boolean.TRUE.equals(entries.get("dir"))) {
            /* Folder */
            final int max_items_per_page = 100;
            int offset = 0;
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("limit", Integer.toString(max_items_per_page));
            query.appendEncoded("orderBy", "default");
            query.appendEncoded("reverse", "false");
            query.appendEncoded("namePrefixExclude", "");
            query.appendEncoded("parentID", main_item_id);
            query.appendEncoded("search", "");
            query.appendEncoded("mediaType", "all");
            final String path = "/api/v2/share/" + folderID + "/nodes?";
            int page = 1;
            pagination: do {
                query.addAndReplace("offset", Integer.toString(offset));
                br.getPage(path + query.toString());
                Map<String, Object> entries2 = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final List<Map<String, Object>> nodes = (List<Map<String, Object>>) entries2.get("nodes");
                for (final Map<String, Object> node : nodes) {
                    final String name = node.get("name").toString();
                    String path_with_filename = (String) node.get("path");
                    final String node_id = node.get("id").toString();
                    final DownloadLink item;
                    if (Boolean.TRUE.equals(node.get("dir"))) {
                        /* Folder */
                        if (true) {
                            /* TODO: fix/implement subfolders */
                            continue;
                        }
                        item = createDownloadlink(String.format("https://%s.stackstorage.com/s/%s?dir=%s&node-id=%s", subdomain, folderID, URLEncode.encodeURIComponent(path_with_filename), node_id));
                    } else {
                        /* File */
                        item = createDownloadlink(String.format("https://stackstorage.com/fileid/%s", node_id));
                        item.setDefaultPlugin(hosterplugin);
                        item.setHost(this.getHost());
                        /* Path also contains filename but we need to separate that and remove filename from path */
                        final String path_without_filename;
                        final String[] pathParts = path_with_filename.split("/");
                        FilePackage fp = FilePackage.getInstance();
                        if (pathParts.length > 1) {
                            /* Remove filename from path */
                            path_without_filename = path_with_filename.replace("/" + name, "");
                        } else {
                            /* Root */
                            path_without_filename = folderID;
                        }
                        fp.setName(path_without_filename);
                        item._setFilePackage(fp);
                        item.setContainerUrl(contenturl);
                        /* There are no URLs for individual files! */
                        item.setContentUrl(contenturl);
                        item.setVerifiedFileSize(((Number) node.get("size")).longValue());
                        item.setFinalFileName(name);
                        item.setAvailable(true);
                        item.setRelativeDownloadFolderPath(path_without_filename);
                        item.setProperty(StackstorageCom.PROPERTY_SUBDOMAIN, subdomain);
                        item.setProperty(StackstorageCom.PROPERTY_FOLDER_ID, folderID);
                        item.setProperty(StackstorageCom.PROPERTY_SHARETOKEN, sharetoken);
                        item.setProperty(StackstorageCom.PROPERTY_CSRFTOKEN, csrftoken);
                        item.setProperty(StackstorageCom.PROPERTY_FILENAME, name);
                        item.setProperty(StackstorageCom.PROPERTY_FILE_ID, node_id);
                    }
                    ret.add(item);
                    distribute(item);
                }
                final int total_numberof_items = ((Number) entries2.get("total")).intValue();
                logger.info("Crawled page " + page + " | Found items so far: " + ret.size() + "/" + total_numberof_items);
                if (ret.size() >= total_numberof_items) {
                    logger.info("Stopping because: Found all items: " + total_numberof_items);
                    break pagination;
                }
                /* Continue to next page */
                page++;
                offset += nodes.size();
            } while (!this.isAbort());
        } else {
            /* Single file */
            final String filename = entries.get("name").toString();
            final DownloadLink file = createDownloadlink(String.format("https://stackstorage.com/fileid/%s", main_item_id));
            file.setDefaultPlugin(hosterplugin);
            file.setHost(this.getHost());
            file.setFinalFileName(filename);
            file.setVerifiedFileSize(((Number) entries.get("size")).longValue());
            file.setAvailable(true);
            file.setContainerUrl(contenturl);
            file.setProperty(StackstorageCom.PROPERTY_SUBDOMAIN, subdomain);
            file.setProperty(StackstorageCom.PROPERTY_FOLDER_ID, folderID);
            file.setProperty(StackstorageCom.PROPERTY_SHARETOKEN, sharetoken);
            file.setProperty(StackstorageCom.PROPERTY_CSRFTOKEN, csrftoken);
            file.setProperty(StackstorageCom.PROPERTY_FILENAME, filename);
            file.setProperty(StackstorageCom.PROPERTY_FILE_ID, main_item_id);
            ret.add(file);
        }
        return ret;
    }
}
