//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
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
import java.util.Set;

import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.MyPikpakCom;

/**
 * Crawler for shared files/folders of PikPak. Supports password protected shares and (nested) folders.
 */
@DecrypterPlugin(revision = "$Revision: 53964 $", interfaceVersion = 3, names = {}, urls = {})
public class MyPikpakComCrawler extends PluginForDecrypt {
    public MyPikpakComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return MyPikpakCom.getPluginDomains();
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
            /*
             * Optional sub path e.g. /s/<shareID>/<encodedFileID>_VNn is ignored. Internal file links of the hoster plugin (?fileid=...)
             * are excluded via negative lookahead.
             */
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/s/([A-Za-z0-9_\\-]+)(?![A-Za-z0-9_\\-]|\\?fileid=)(?:/[^\\s\\?#]*)?");
        }
        return ret.toArray(new String[0]);
    }

    /** Per-crawl state. */
    private static class CrawlContext {
        final String             shareID;
        final MyPikpakCom        hosterplugin;
        String                   passCode      = null;
        String                   passCodeToken = null;
        final List<DownloadLink> results       = new ArrayList<DownloadLink>();
        int                      numberofFiles = 0;
        /* IDs of all files and folders found so far. */
        final Set<String>        dupes         = new HashSet<String>();

        CrawlContext(final String shareID, final MyPikpakCom hosterplugin) {
            this.shareID = shareID;
            this.hosterplugin = hosterplugin;
        }
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        final String shareID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        final CrawlContext ctx = new CrawlContext(shareID, (MyPikpakCom) this.getNewPluginForHostInstance(this.getHost()));
        ctx.passCode = param.getDecrypterPassword();
        /* Root level of the share including the password check. */
        Map<String, Object> response = null;
        int attempt = 0;
        while (true) {
            final UrlQuery query = getListQuery(ctx.shareID, null);
            if (ctx.passCode != null) {
                query.appendEncoded("pass_code", ctx.passCode);
            }
            response = MyPikpakCom.apiGet(this, br, "/drive/v1/share", query);
            final String status = (String) response.get("share_status");
            if ("OK".equals(status)) {
                break;
            } else if ("PASS_CODE_EMPTY".equals(status) || "PASS_CODE_ERROR".equals(status)) {
                if (++attempt > 3) {
                    throw new DecrypterException(DecrypterException.PASSWORD);
                }
                ctx.passCode = getUserInput("Password?", param);
            } else {
                /* Share has been deleted/expired/blocked etc. */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, (String) response.get("share_status_text"));
            }
        }
        ctx.passCodeToken = (String) response.get("pass_code_token");
        String title = (String) response.get("title");
        if (StringUtils.isEmpty(title)) {
            title = ctx.shareID;
        }
        crawlItems(ctx, response, "/drive/v1/share", null, title, true);
        if (ctx.results.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        return new ArrayList<DownloadLink>(ctx.results);
    }

    private UrlQuery getListQuery(final String shareID, final String pageToken) {
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("limit", "100");
        query.appendEncoded("thumbnail_size", "SIZE_LARGE");
        query.appendEncoded("order", "6");
        query.appendEncoded("folders_first", "true");
        query.appendEncoded("share_id", shareID);
        if (pageToken != null) {
            query.appendEncoded("page_token", pageToken);
        }
        return query;
    }

    /**
     * Processes all items of the given (already loaded) response page, follows pagination and crawls subfolders recursively.
     *
     * @param parentID
     *            ID of the current folder or null for the share root.
     * @param path
     *            Relative path of the current folder or null for the share root. Share root items without a folder get no path.
     */
    private void crawlItems(final CrawlContext ctx, Map<String, Object> response, final String apiPath, final String parentID, final String packageName, final boolean isRoot) throws Exception {
        final String path = isRoot ? null : packageName;
        final List<DownloadLink> thisFolderFiles = new ArrayList<DownloadLink>();
        final List<Map<String, Object>> subfolders = new ArrayList<Map<String, Object>>();
        int page = 1;
        while (true) {
            int numberofNewItemsThisPage = 0;
            for (final Map<String, Object> item : (List<Map<String, Object>>) response.get("files")) {
                /* Every file/folder ID is only processed once: Protects against endless loops caused by repeated pages or cyclic folders. */
                if (!ctx.dupes.add((String) item.get("id"))) {
                    continue;
                }
                numberofNewItemsThisPage++;
                if ("drive#folder".equals(item.get("kind"))) {
                    subfolders.add(item);
                } else {
                    thisFolderFiles.add(createFileLink(ctx, item));
                }
            }
            final String nextPageToken = (String) response.get("next_page_token");
            logger.info("Crawled page " + page + " | Folder: " + packageName + " | New items on this page: " + numberofNewItemsThisPage + " | Files so far in this folder: " + thisFolderFiles.size() + " | Subfolders so far: " + subfolders.size() + " | Total files so far: " + ctx.numberofFiles + " | Next page token: " + nextPageToken);
            if (StringUtils.isEmpty(nextPageToken)) {
                logger.info("Stopping because: Reached end of folder: " + packageName);
                break;
            } else if (this.isAbort()) {
                throw new InterruptedException();
            } else if (numberofNewItemsThisPage == 0) {
                logger.info("Stopping because: Failed to find any new items on current page: " + page);
                break;
            }
            page++;
            response = loadFolderPage(ctx, apiPath, parentID, nextPageToken, isRoot);
        }
        if (!thisFolderFiles.isEmpty()) {
            final boolean isSingleLooseFile = isRoot && thisFolderFiles.size() == 1 && subfolders.isEmpty();
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(packageName);
            fp.setPackageKey("mypikpak://" + ctx.shareID + (parentID != null ? "/" + parentID : ""));
            for (final DownloadLink file : thisFolderFiles) {
                if (!isSingleLooseFile) {
                    if (path != null) {
                        file.setRelativeDownloadFolderPath(path);
                    }
                    file._setFilePackage(fp);
                }
                ctx.results.add(file);
                distribute(file);
            }
        }
        for (final Map<String, Object> folder : subfolders) {
            if (this.isAbort()) {
                throw new InterruptedException();
            }
            final String folderID = (String) folder.get("id");
            final String folderName = (String) folder.get("name");
            final Map<String, Object> folderResponse = loadFolderPage(ctx, "/drive/v1/share/detail", folderID, null, false);
            crawlItems(ctx, folderResponse, "/drive/v1/share/detail", folderID, path != null ? path + "/" + folderName : folderName, false);
        }
    }

    private Map<String, Object> loadFolderPage(final CrawlContext ctx, final String apiPath, final String parentID, final String pageToken, final boolean isRoot) throws Exception {
        final UrlQuery query = getListQuery(ctx.shareID, pageToken);
        if (isRoot) {
            /* Pagination of the share root: Same endpoint as the initial request. */
            if (ctx.passCode != null) {
                query.appendEncoded("pass_code", ctx.passCode);
            }
        } else {
            query.appendEncoded("parent_id", parentID);
            if (!StringUtils.isEmpty(ctx.passCodeToken)) {
                query.appendEncoded("pass_code_token", ctx.passCodeToken);
            }
        }
        final Map<String, Object> response = MyPikpakCom.apiGet(this, br, apiPath, query);
        final String status = (String) response.get("share_status");
        if (!"OK".equals(status)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unexpected share_status: " + status);
        }
        return response;
    }

    private DownloadLink createFileLink(final CrawlContext ctx, final Map<String, Object> item) {
        final String fileID = (String) item.get("id");
        final DownloadLink file = this.createDownloadlink("https://" + this.getHost() + "/s/" + ctx.shareID + "?fileid=" + fileID);
        file.setProperty(MyPikpakCom.PROPERTY_SHARE_ID, ctx.shareID);
        file.setProperty(MyPikpakCom.PROPERTY_FILE_ID, fileID);
        file.setFinalFileName((String) item.get("name"));
        file.setVerifiedFileSize(Long.parseLong((String) item.get("size")));
        file.setAvailable(true);
        if (ctx.passCode != null) {
            file.setDownloadPassword(ctx.passCode);
        }
        ctx.numberofFiles++;
        return file;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }
}
