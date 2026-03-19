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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.requests.PostRequest;
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
import jd.plugins.hoster.K2SApi;

import org.appwork.storage.TypeRef;
import org.appwork.utils.formatter.HexFormatter;
import org.jdownloader.plugins.components.config.Keep2shareConfig;
import org.jdownloader.plugins.components.config.Keep2shareConfig.FileLinkAddMode;

@DecrypterPlugin(revision = "$Revision: 52517 $", interfaceVersion = 2, names = {}, urls = {})
public class Keep2ShareCcDecrypter extends PluginForDecrypt {
    public Keep2ShareCcDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static final String[] domainsK2s                     = new String[] { "k2s.cc", "keep2share.cc", "k2share.cc", "keep2s.cc", "keep2.cc" };
    public static final String[] domainsFileboom                = new String[] { "fileboom.me", "fboom.me" };
    public static final String[] domainsTezfilesAndPublish2     = new String[] { "tezfiles.com", "publish2.me" };
    public static final String   SUPPORTED_LINKS_PATTERN_FILE   = "(?i)/(?:f|file|preview)/(?:info/)?([a-z0-9_\\-]{13,})(/([^/\\?]+))?.*";
    private static final String  SUPPORTED_LINKS_PATTERN_FOLDER = "(?i)/folder(?:/info)?/([a-z0-9]{13,}).*";

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(domainsK2s);
        ret.add(domainsFileboom);
        ret.add(domainsTezfilesAndPublish2);
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
            ret.add("https?://(?:[a-z0-9\\-]+\\.)?" + buildHostsPatternPart(domains) + "(" + SUPPORTED_LINKS_PATTERN_FILE + "|" + SUPPORTED_LINKS_PATTERN_FOLDER + ")");
        }
        return ret.toArray(new String[0]);
    }

    public static String fixContentID(final String content_id) {
        if (content_id.length() == 13 && !K2SApi.isSpecialFileID(content_id)) {
            /* Default IDs: Regex allows uppercase but we know that they are lowercase. */
            return content_id.toLowerCase(Locale.ENGLISH);
        } else {
            /* Do not touch ID */
            return content_id;
        }
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final boolean looksLikeSingleFileItem;
        String contentidFromURL = new Regex(param.getCryptedUrl(), SUPPORTED_LINKS_PATTERN_FOLDER).getMatch(0);
        if (contentidFromURL != null) {
            /* Looks like folder */
            looksLikeSingleFileItem = false;
        } else {
            /* Looks like single file */
            contentidFromURL = new Regex(param.getCryptedUrl(), SUPPORTED_LINKS_PATTERN_FILE).getMatch(0);
            looksLikeSingleFileItem = true;
        }
        if (contentidFromURL == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contentid = fixContentID(contentidFromURL);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final K2SApi plugin = (jd.plugins.hoster.K2SApi) getNewPluginForHostInstance(this.getHost());
        final Keep2shareConfig cfg = get(plugin.getConfigInterface());
        final FileLinkAddMode mode = cfg.getFileLinkAddMode();
        if (looksLikeSingleFileItem && (mode == FileLinkAddMode.HOSTER_PLUGIN_LINKCHECK || mode == FileLinkAddMode.DEFAULT)) {
            /* URL looks like single file URL -> Pass to hosterplugin so we can make use of mass-linkchecking feature. */
            ret.add(this.createDownloadlink(param.getCryptedUrl().replaceFirst(Pattern.quote(contentidFromURL), contentid)));
            return ret;
        }
        this.br = plugin.createNewBrowserInstance();
        // set cross browser support
        plugin.setBrowser(br);
        final String referer = K2SApi.getRefererFromURL(param.getCryptedUrl());
        Map<String, Object> response = null;
        List<Map<String, Object>> items = null;
        try {
            singleFileHandling: if (looksLikeSingleFileItem) {
                /**
                 * This handling is supposed to be for single files but can also be used for small folders. </br> Using the folder handling
                 * down below for single files will prohibit our special referrer handling from working since it seems to flag the current
                 * IP so the referrer will be ignored later and users who have configured a special referrer will not get better download
                 * speeds anymore. </br> More detailed explanation: https://board.jdownloader.org/showthread.php?t=94515
                 */
                logger.info("Link looks like single file link -> Jumping into single file handling");
                final Map<String, Object> postdataGetfilesinfo = new HashMap<String, Object>();
                postdataGetfilesinfo.put("ids", Arrays.asList(new String[] { contentid }));
                /**
                 * What this returns: </br> ID leads to a single file: Single file information </br> ID leads to a single SMALL(!) folder:
                 * All folder file items </br> ID leads to a big folder: Only folder meta-information -> Folder items need to be crawled
                 * using a separate request down below.
                 */
                response = plugin.postPageRaw(br, "https://" + this.getHost() + "/api/v2/getfilesinfo", postdataGetfilesinfo, null);
                if (!"success".equals(response.get("status"))) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                items = (List<Map<String, Object>>) response.get("files");
                for (final Map<String, Object> item : items) {
                    final String id = (String) item.get("id");
                    final String fileOrFolderName = (String) item.get("name");
                    if (fileOrFolderName == null) {
                        // not on this domain
                        continue;
                    }
                    final Boolean isFolder = (Boolean) item.get("is_folder");
                    if (isFolder == null) {
                        // not on this domain
                        continue;
                    } else if (Boolean.FALSE.equals(isFolder)) {
                        /* File */
                        final DownloadLink file = createDownloadlink(generateFileUrl(id, fileOrFolderName, referer));
                        plugin.parseFileInfo(file, item, contentidFromURL);
                        if (!file.isNameSet()) {
                            /* Fallback */
                            file.setName(id);
                        }
                        ret.add(file);
                    } else {
                        /* Item is a folder or offline (= invalid id) */
                        logger.info("Looks like this might be an invalid fileID/folderID or a bigger folder -> Jumping into folder crawler");
                        break singleFileHandling;
                    }
                }
                /* If we reach this line this means we found at least one file that is online. */
                return ret;
            }
            FilePackage fp = null;
            String thisFolderTitle = null;
            final Set<String> dupes = new HashSet<String>();
            final int maxItemsPerPage = 50;
            int offset = 0;
            int page = 1;
            boolean isSingleFile = false;
            String sourceFileID = null;
            String path = this.getAdoptedCloudFolderStructure();
            do {
                final Map<String, Object> postdataGetfilestatus = new HashMap<String, Object>();
                postdataGetfilestatus.put("id", contentid);
                postdataGetfilestatus.put("limit", maxItemsPerPage);
                postdataGetfilestatus.put("offset", offset);
                response = plugin.postPageRaw(br, "/getfilestatus", postdataGetfilestatus, null);
                items = (List<Map<String, Object>>) response.get("files");
                if ((items == null || items.size() == 0) && Boolean.TRUE.equals(response.get("is_available")) && Boolean.TRUE.equals(response.get("is_folder"))) {
                    items = listV1Files(contentid);
                }
                if (items == null && response.containsKey("is_available") && !response.containsKey("id")) {
                    /* Root map contains single loose file. */
                    isSingleFile = true;
                    sourceFileID = contentid;
                    items = new ArrayList<Map<String, Object>>();
                    items.add(response);
                } else {
                    /* Root map is folder containing files */
                    if (fp == null) {
                        fp = FilePackage.getInstance();
                        thisFolderTitle = response.get("name").toString();
                        if (path == null) {
                            path = thisFolderTitle;
                        } else {
                            path += "/" + thisFolderTitle;
                        }
                        fp.setName(path);
                    }
                }
                if (items.isEmpty()) {
                    if (ret.isEmpty()) {
                        if (thisFolderTitle != null) {
                            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, contentid + "_" + thisFolderTitle);
                        } else {
                            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, contentid);
                        }
                    } else {
                        logger.info("Stopping because: Failed to find any items on current page");
                        break;
                    }
                }
                int numberofNewItems = 0;
                for (final Map<String, Object> item : items) {
                    final String id;
                    if (isSingleFile) {
                        id = contentid;
                    } else {
                        id = item.get("id").toString();
                    }
                    if (!dupes.add(id)) {
                        continue;
                    }
                    numberofNewItems++;
                    final String filenameOrFoldername = (String) item.get("name");
                    final DownloadLink result;
                    if (Boolean.TRUE.equals(item.get("is_folder")) || "folder".equals(item.get("type"))) {
                        /* Folder */
                        result = createDownloadlink(generateFolderUrl(id, filenameOrFoldername, referer));
                    } else {
                        /* File */
                        result = createDownloadlink(generateFileUrl(id, filenameOrFoldername, referer));
                        plugin.parseFileInfo(result, item, sourceFileID);
                        if (!result.isNameSet()) {
                            /* Fallback */
                            result.setName(id);
                        }
                        if (fp != null) {
                            result._setFilePackage(fp);
                        }
                    }
                    if (path != null) {
                        result.setRelativeDownloadFolderPath(path);
                    }
                    ret.add(result);
                    distribute(result);
                }
                logger.info("Crawled page " + page + " | Offset: " + offset + " | New items this page: " + numberofNewItems + " | Found items so far: " + ret.size());
                if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    break;
                } else if (isSingleFile) {
                    logger.info("Stopping because: This item is a single file");
                    break;
                } else if (numberofNewItems == 0) {
                    logger.info("Stopping because: Failed to find any new items on current page: " + page);
                    break;
                } else if (numberofNewItems < maxItemsPerPage) {
                    logger.info("Stopping because: Current page contains less items than " + maxItemsPerPage);
                    break;
                } else {
                    offset += maxItemsPerPage;
                    page++;
                }
            } while (!this.isAbort());
        } catch (final PluginException e) {
            if (br.getHttpConnection().getResponseCode() == 400) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Invalid fileID");
            } else if (br.getHttpConnection().getResponseCode() == 403) {
                /* That's a vague assumption but it should be correct. */
                /*
                 * {"message":"You are not authorized for this action","status":"error","code":403,"errorCode":10}
                 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Private file which only the owner can download");
            } else {
                throw e;
            }
        }
        return ret;
    }

    public static AtomicBoolean LIST_V1_AVAILABLE = new AtomicBoolean(true);
    private Browser             v1BrowserInstance = null;

    private List<Map<String, Object>> listV1Files(String folderid) throws Exception {
        if (LIST_V1_AVAILABLE.get() == false) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        Browser br = v1BrowserInstance;
        token: if (br == null) {
            br = this.br.createNewBrowserInstance();
            final Map<String, Object> json = new HashMap<String, Object>();
            json.put("client_id", "k2s_web_app");
            json.put("client_secret", new String(HexFormatter.hexToByteArray("706A633870795A76377668736365786570464E7A6D753450"), "UTF-8"));
            json.put("grant_type", "client_credentials");
            final PostRequest token = br.createJSonPostRequest("https://api.k2s.cc/v1/auth/token", json);
            br.getPage(token);
            if (br.getRequest().getHttpConnection().getResponseCode() == 401) {
                LIST_V1_AVAILABLE.set(false);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            v1BrowserInstance = br;
        }
        final List<Map<String, Object>> ret = new ArrayList<Map<String, Object>>();
        final int limit = 50;
        int offset = 0;
        Number total = null;
        pagination: while (true) {
            br.getPage("https://api.k2s.cc/v1/files?limit=" + limit + "&offset=" + offset + "&sort=name&folderId=" + folderid + "&withFolders=true");
            final Map<String, Object> response = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            total = total != null ? total : (Number) response.get("total");
            final List<Map<String, Object>> items = (List<Map<String, Object>>) response.get("items");
            if (items == null || items.size() == 0) {
                break pagination;
            }
            for (Map<String, Object> item : items) {
                if (!item.containsKey("is_available")) {
                    final Boolean isDeleted = (Boolean) item.get("isDeleted");
                    item.put("is_available", !Boolean.TRUE.equals(isDeleted));
                }
                if ("folder".equals(item.get("type"))) {
                    item.put("is_folder", Boolean.TRUE);
                }
            }
            ret.addAll(items);
            if (isAbort()) {
                break pagination;
            } else if (total != null && ret.size() >= total.intValue()) {
                break pagination;
            } else {
                offset += items.size();
            }
        }
        return ret;
    }

    private String generateFolderUrl(final String folderid, final String foldername, final String referer) {
        String url = "https://" + this.getHost() + "/folder/" + folderid;
        if (foldername != null) {
            url += "/" + Encoding.urlEncode(foldername);
        }
        if (referer != null) {
            url += "?site=" + Encoding.urlEncode(referer);
        }
        return url;
    }

    private String generateFileUrl(final String fileid, final String filename, final String referer) {
        String url = "https://" + this.getHost() + "/file/" + fileid;
        if (filename != null) {
            url += "/" + Encoding.urlEncode(filename);
        }
        if (referer != null) {
            url += "?site=" + Encoding.urlEncode(referer);
        }
        return url;
    }
}
