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

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
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
import jd.plugins.PluginForHost;
import jd.plugins.hoster.OneDriveLiveCom;

@DecrypterPlugin(revision = "$Revision: 51054 $", interfaceVersion = 3, names = { "onedrive.live.com" }, urls = { "https?://([a-zA-Z0-9\\-]+\\.)?(onedrive\\.live\\.com/.+|skydrive\\.live\\.com/.+|(sdrv|1drv)\\.ms/[A-Za-z0-9&!=#\\.,-_]+)" })
public class OneDriveLiveComCrawler extends PluginForDecrypt {
    public OneDriveLiveComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private String        cid         = null;
    private String        resource_id = null;
    private String        authkey     = null;
    private PluginForHost hostPlugin  = null;

    private void ensureInitHosterplugin() throws PluginException {
        if (this.hostPlugin == null) {
            this.hostPlugin = getNewPluginForHostInstance("onedrive.live.com");
        }
    }

    @SuppressWarnings({ "unchecked", "deprecation" })
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        String contenturl = param.getCryptedUrl().replaceFirst("^(?i)http://", "https://");
        setGlobalVars(contenturl);
        if (cid == null || resource_id == null) {
            /* Possibly a short-URL which redirects to another URL which should contain the parameters we are looking for. */
            br.getPage(contenturl);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String newURL = br.getURL();
            if (newURL.equalsIgnoreCase(contenturl)) {
                /* URL hasn't changed */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (!this.canHandle(newURL)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            setGlobalVars(newURL);
            contenturl = newURL;
        }
        if (cid == null || resource_id == null) {
            /* Invalid URL */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (authkey != null) {
            authkey = Encoding.htmlDecode(authkey);
        }
        cid = cid.toUpperCase(Locale.ENGLISH);
        contenturl = this.generateFolderLink(cid, resource_id, authkey);
        final UrlQuery rootquerypost = new UrlQuery();
        rootquerypost.add("%24select", "*%2CsharepointIds%2CwebDavUrl%2CcontainingDrivePolicyScenarioViewpoint");
        rootquerypost.add("24expand", "thumbnails");
        rootquerypost.add("ump", "1");
        if (authkey != null) {
            rootquerypost.add("authKey", Encoding.urlEncode(authkey));
        }
        br.getHeaders().put("Origin", "https://onedrive.live.com");
        br.getHeaders().put("Referer", "https://onedrive.live.com/");
        /* Browser is using POST request, we are using GET to keep things simple. */
        br.getPage("https://api.onedrive.com/v1.0/drives/" + cid.toLowerCase(Locale.ENGLISH) + "/items/" + resource_id + "?" + rootquerypost);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> resourceinfo = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> errormap = (Map<String, Object>) resourceinfo.get("error");
        if (errormap != null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, errormap.get("localizedMessage").toString());
        }
        final Map<String, Object> foldermap = (Map<String, Object>) resourceinfo.get("folder");
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (foldermap != null) {
            /* Crawl all items of a folder */
            final long size = ((Number) resourceinfo.get("size")).longValue();
            if (size == 0) {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
            }
            String path = resourceinfo.get("pathFromRoot").toString();
            path = Encoding.htmlDecode(path).trim();
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(path);
            fp.setPackageKey("onedrive://folder/" + cid);
            int page = 1;
            final HashSet<String> dupes = new HashSet<String>();
            final int maxItemsPerPage = 100;
            final UrlQuery querypagination = new UrlQuery();
            querypagination.add("%24top", Integer.toString(maxItemsPerPage));
            querypagination.add("%24expand", "");
            querypagination.add("select", "*%2Cocr%2CwebDavUrl%2CsharepointIds%2CisRestricted%2CcommentSettings%2CspecialFolder%2CcontainingDrivePolicyScenarioViewpoint");
            querypagination.add("ump", "1");
            if (authkey != null) {
                querypagination.add("authKey", Encoding.urlEncode(authkey));
            }
            String nextpageLink = "/v1.0/drives/" + cid.toLowerCase(Locale.ENGLISH) + "/items/" + resource_id + "/children?" + querypagination;
            do {
                br.getPage(nextpageLink);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final List<Map<String, Object>> items = (List<Map<String, Object>>) entries.get("value");
                int numberofNewItemsThisPage = 0;
                for (final Map<String, Object> item : items) {
                    final String itemID = item.get("id").toString();
                    if (!dupes.add(itemID)) {
                        /* Skip duplicates */
                        continue;
                    }
                    numberofNewItemsThisPage++;
                    final Map<String, Object> file = (Map<String, Object>) item.get("file");
                    if (file != null) {
                        /* Single file */
                        final DownloadLink dlfile = crawlProcessSingleFile(item);
                        dlfile._setFilePackage(fp);
                        /* Set additional important properties */
                        ret.add(dlfile);
                        distribute(dlfile);
                    } else {
                        /* Subfolder */
                        /* Do not use the URL from field "webUrl" because this will need one extra http request later. */
                        final String folderlink = this.generateFolderLink(cid, itemID, this.authkey);
                        // final Map<String, Object> folder = (Map<String, Object>) item.get("folder");
                        final DownloadLink dlfolder = this.createDownloadlink(folderlink);
                        ret.add(dlfolder);
                        distribute(dlfolder);
                    }
                }
                final int totalNumberofItems = ((Number) entries.get("@odata.count")).intValue();
                nextpageLink = (String) entries.get("@odata.nextLink");
                logger.info("Crawled page " + page + " | Found items on this page: " + items.size() + "/" + maxItemsPerPage + " | Found items so far: " + ret.size() + "/" + totalNumberofItems + " | nextpageLink = " + nextpageLink);
                if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    break;
                } else if (numberofNewItemsThisPage == 0) {
                    logger.info("Stopping because: Failed to find any new items on current page");
                    break;
                } else if (StringUtils.isEmpty(nextpageLink)) {
                    logger.info("Stopping because: Reached last page");
                    break;
                } else {
                    /* Continue to next page */
                    page++;
                }
            } while (!this.isAbort());
        } else {
            /* Crawl single file */
            ret.add(crawlProcessSingleFile(resourceinfo));
        }
        return ret;
    }

    private void setGlobalVars(final String url) throws MalformedURLException {
        final UrlQuery query = UrlQuery.parse(url);
        resource_id = query.getDecoded("resid");
        cid = query.getDecoded("cid");
        authkey = query.getDecoded("authkey");
        if (resource_id == null) {
            resource_id = getLastID(url);
        }
    }

    private DownloadLink crawlProcessSingleFile(final Map<String, Object> item) throws PluginException {
        ensureInitHosterplugin();
        final String url = item.get("webUrl").toString();
        final String filename = item.get("name").toString();
        final DownloadLink dlfile = this.createDownloadlink(url);
        dlfile.setFinalFileName(filename);
        dlfile.setVerifiedFileSize(((Number) item.get("size")).longValue());
        dlfile.setDefaultPlugin(this.hostPlugin);
        dlfile.setAvailable(true);
        {
            /* Set file-hash */
            final Map<String, Object> file = (Map<String, Object>) item.get("file");
            final Map<String, Object> hashes = (Map<String, Object>) file.get("hashes");
            final String sha256Hash = (String) hashes.get("sha256Hash");
            final String sha1Hash = (String) hashes.get("sha1Hash");
            if (!StringUtils.isEmpty(sha256Hash)) {
                dlfile.setSha256Hash(sha256Hash);
            } else if (!StringUtils.isEmpty(sha1Hash)) {
                dlfile.setSha1Hash(sha1Hash);
            }
        }
        String pathFromRoot = item.get("pathFromRoot").toString();
        pathFromRoot = Encoding.htmlDecode(pathFromRoot).trim();
        final String pathToFile = pathFromRoot.replaceFirst("/" + Pattern.quote(filename) + "$", "");
        dlfile.setRelativeDownloadFolderPath(pathToFile);
        /* Set additional important properties */
        dlfile.setProperty(OneDriveLiveCom.PROPERTY_FILE_ID, item.get("id"));
        dlfile.setProperty(OneDriveLiveCom.PROPERTY_CID, cid);
        dlfile.setProperty(OneDriveLiveCom.PROPERTY_FOLDER_ID, resource_id);
        dlfile.setProperty(OneDriveLiveCom.PROPERTY_AUTHKEY, authkey);
        dlfile.setProperty(OneDriveLiveCom.PROPERTY_VIEW_IN_BROWSER_URL, url);
        dlfile.setProperty(OneDriveLiveCom.PROPERTY_DIRECTURL, item.get("@content.downloadUrl"));
        {
            final Map<String, Object> parentReference = (Map<String, Object>) item.get("parentReference");
            if (parentReference != null) {
                dlfile.setProperty(OneDriveLiveCom.PROPERTY_PARENT_FOLDER_ID, parentReference.get("id"));
            }
        }
        return dlfile;
    }

    @Deprecated
    public static final int MAX_ENTRIES_PER_REQUEST_LEGACY = 200;

    private String generateFolderLink(final String cid, final String id, final String authkey) {
        String folderlink = "https://onedrive.live.com/?cid=" + cid + "&id=" + id;
        if (authkey != null) {
            /* Don't forget to add authKey if needed */
            folderlink += "&authkey=" + authkey;
        }
        return folderlink;
    }

    private String getLastID(final String url) {
        /* Get last ID */
        int pos = url.lastIndexOf("&id=") + 4;
        final String parameter_part = url.substring(pos, url.length());
        final String ret = new Regex(parameter_part, "([A-Z0-9]+(\\!|%21)\\d+)").getMatch(0);
        if (ret != null) {
            return ret.replace("%21", "!");
        } else {
            return ret;
        }
    }
}
