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

import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.config.CivitaiComConfig;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.CivitaiCom;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 51677 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { CivitaiCom.class })
public class CivitaiComCrawler extends PluginForDecrypt {
    public CivitaiComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setLoadLimit(20 * 1024 * 1024);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return CivitaiCom.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(models/.+|posts/\\d+|user/.+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final Regex urlregex = new Regex(param.getCryptedUrl(), "(?i)/(posts|models|user)/([^/]+)");
        final UrlQuery query = UrlQuery.parse(contenturl);
        final String modelVersionId = query.get("modelVersionId");
        final String itemType = urlregex.getMatch(0);
        final String itemID = urlregex.getMatch(1);
        final CivitaiCom hosterplugin = (CivitaiCom) this.getNewPluginForHostInstance(this.getHost());
        final CivitaiComConfig cfg = PluginJsonConfig.get(CivitaiComConfig.class);
        /*
         * Using API: https://github.com/civitai/civitai/wiki/REST-API-Reference,
         * https://wiki.civitai.com/wiki/Civitai_API#GET_/api/v1/images
         */
        final String apiBase = "https://civitai.com/api/v1";
        final List<Map<String, Object>> modelVersions = new ArrayList<Map<String, Object>>();
        /**
         * 2024-07-18: About the "nsfw" parameter: According to their docs, without nsfw parameter, all items will be returned but that is
         * wrong --> Wrong API docs or bug in API. </br>
         * Only with the nsfw parameter set to "X", all items will be returned. <br>
         * 2024-07-18: Issue has been reported to civitai: https://github.com/civitai/civitai/issues/1277
         */
        final String special_api_params = "&nsfw=X";
        String modelName = null;
        if (itemType.equals("models")) {
            /* Crawl all versions of a model */
            /* https://github.com/civitai/civitai/wiki/REST-API-Reference#get-apiv1modelsmodelid */
            br.getPage(apiBase + "/models/" + itemID);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            modelName = entries.get("name").toString();
            final List<Map<String, Object>> _modelVersions_ = (List<Map<String, Object>>) entries.get("modelVersions");
            modelVersions.addAll(_modelVersions_);
            if (modelVersionId != null) {
                final Iterator<Map<String, Object>> it = modelVersions.iterator();
                while (it.hasNext()) {
                    final Map<String, Object> next = it.next();
                    if (!modelVersionId.equals(StringUtils.valueOfOrNull(next.get("id")))) {
                        it.remove();
                    }
                }
            }
        } else if (itemType.equals("posts")) {
            /**
             * Handles such links: https://civitai.com/posts/1234567 <br>
             * https://github.com/civitai/civitai/wiki/REST-API-Reference#get-apiv1images
             */
            br.getPage(apiBase + "/images?postId=" + itemID + special_api_params);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final List<Map<String, Object>> images = (List<Map<String, Object>>) entries.get("items");
            if (images == null || images.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(itemID);
            fp.setPackageKey("civitai://post/" + itemID);
            for (final Map<String, Object> image : images) {
                final String imageurl = image.get("url").toString();
                final DownloadLink link = this.createDownloadlink(br.getURL("/images/" + image.get("id")).toExternalForm());
                link.setProperty(CivitaiCom.PROPERTY_DATE, image.get("createdAt"));
                link.setProperty(CivitaiCom.PROPERTY_USERNAME, image.get("username"));
                link.setProperty(CivitaiCom.PROPERTY_DIRECTURL, imageurl);
                link.setProperty(CivitaiCom.PROPERTY_TYPE, image.get("type"));
                link._setFilePackage(fp);
                final String tempName;
                if (cfg.isUseIndexIDForImageFilename()) {
                    if ("video".equals(image.get("type"))) {
                        tempName = image.get("id").toString() + getFileNameExtensionFromURL(imageurl, ".mp4");
                    } else {
                        tempName = image.get("id").toString() + ".jpg";
                    }
                } else {
                    tempName = getFileNameFromURL(new URL(imageurl));
                }
                if (tempName != null) {
                    link.setName(tempName);
                }
                link.setAvailable(true);
                ret.add(link);
            }
        } else if (itemType.equals("user")) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(itemID);
            fp.setPackageKey("civitai://user/" + itemID);
            /* Handles such links: https://civitai.com/user/test */
            /* https://github.com/civitai/civitai/wiki/REST-API-Reference#get-apiv1images */
            /* 2024-07-17: use small limit/pagination size to avoid timeout issues */
            final int maxItemsPerPage = cfg.getProfileCrawlerMaxPaginationItems();
            final int paginationSleepMillis = cfg.getProfileCrawlerPaginationSleepMillis();
            String nextpage = apiBase + "/images?username=" + itemID + "&limit=" + maxItemsPerPage + special_api_params;
            if (modelVersionId != null) {
                nextpage += "&modelVersionId=" + modelVersionId;
            }
            int page = 1;
            final HashSet<String> dupes = new HashSet<String>();
            pagination: while (nextpage != null && !isAbort()) {
                URLConnectionAdapter con = br.openGetConnection(nextpage);
                final Map<String, Object> entries;
                try {
                    if (con.getResponseCode() == 404) {
                        br.followConnection();
                        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                    }
                    entries = JSonStorage.restoreFromInputStream(con.getInputStream(), TypeRef.MAP);
                } finally {
                    con.disconnect();
                }
                final List<Map<String, Object>> images = (List<Map<String, Object>>) entries.get("items");
                if (images == null || images.isEmpty()) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                int numberofNewItems = 0;
                for (final Map<String, Object> image : images) {
                    final String imageurl = image.get("url").toString();
                    final String imageid = image.get("id").toString();
                    if (!dupes.add(imageid)) {
                        continue;
                    }
                    numberofNewItems++;
                    final DownloadLink link = this.createDownloadlink(br.getURL("/images/" + imageid).toExternalForm());
                    link.setProperty(CivitaiCom.PROPERTY_DATE, image.get("createdAt"));
                    link.setProperty(CivitaiCom.PROPERTY_USERNAME, image.get("username"));
                    link.setProperty(CivitaiCom.PROPERTY_DIRECTURL, imageurl);
                    link.setProperty(CivitaiCom.PROPERTY_TYPE, image.get("type"));
                    link._setFilePackage(fp);
                    final String tempName;
                    if (cfg.isUseIndexIDForImageFilename()) {
                        if ("video".equals(image.get("type"))) {
                            tempName = imageid + getFileNameExtensionFromURL(imageurl, ".mp4");
                        } else {
                            tempName = imageid + ".jpg";
                        }
                    } else {
                        tempName = getFileNameFromURL(new URL(imageurl));
                    }
                    if (tempName != null) {
                        link.setName(tempName);
                    }
                    link.setAvailable(true);
                    ret.add(link);
                    distribute(link);
                }
                logger.info("Crawled page " + page + " | Found items so far: " + ret.size() + " | New items on this page: " + numberofNewItems + " | Max items per page: " + maxItemsPerPage + " | Pagination sleep millis: " + paginationSleepMillis);
                final Map<String, Object> metadata = (Map<String, Object>) entries.get("metadata");
                if (metadata == null) {
                    logger.info("Stopping because: Metadata map doesnt exist");
                    break pagination;
                }
                nextpage = (String) metadata.get("nextPage");
                if (nextpage == null) {
                    logger.info("Stopping because: Failed to find nextPage");
                    break pagination;
                }
                if (numberofNewItems == 0) {
                    /* Fail-safe */
                    logger.info("Stopping because: Current page did not contain any new items -> Reached end?");
                    break pagination;
                } else {
                    /* Continue to next page */
                    page++;
                    this.sleep(paginationSleepMillis, param);
                    continue pagination;
                }
            }
        } else {
            /* Unsupported link */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Process collected ModelVersions */
        for (final Map<String, Object> entries : modelVersions) {
            final ArrayList<DownloadLink> thisRet = new ArrayList<DownloadLink>();
            final String modelVersionName = entries.get("name").toString();
            final FilePackage fp = FilePackage.getInstance();
            if (modelName != null) {
                fp.setName(modelName + " - " + modelVersionName);
            } else {
                fp.setName(modelVersionName);
            }
            fp.setPackageKey("civitai://model/modelVersion/" + modelVersionId);
            final List<Map<String, Object>> files = (List<Map<String, Object>>) entries.get("files");
            for (final Map<String, Object> file : files) {
                final Map<String, Object> hashes = (Map<String, Object>) file.get("hashes");
                final String downloadurl = file.get("downloadUrl").toString();
                final DownloadLink link;
                if (hosterplugin.canHandle(downloadurl)) {
                    link = this.createDownloadlink(downloadurl);
                } else {
                    link = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(downloadurl));
                }
                String filename = file.get("name").toString();
                // images/videos may include ?token=YZX in name
                filename = filename.replaceFirst("\\?token=.+", "");
                final String mimeType = (String) file.get("mimeType");
                final String ext = getExtensionFromMimeType(mimeType);
                if (ext != null) {
                    link.setFinalFileName(this.applyFilenameExtension(filename, "." + ext));
                } else {
                    link.setName(filename);
                }
                link.setDownloadSize(((Number) file.get("sizeKB")).longValue() * 1024);
                link.setAvailable(true);
                final String sha256 = (String) hashes.get("SHA256");
                // final String crc32 = (String) hashes.get("CRC32");
                if (sha256 != null) {
                    link.setSha256Hash(sha256);
                }
                // if (crc32 != null) {
                // link.setHashInfo(HashInfo.newInstanceSafe(crc32, HashInfo.TYPE.CRC32C));
                // }
                thisRet.add(link);
            }
            final List<Map<String, Object>> images = (List<Map<String, Object>>) entries.get("images");
            for (final Map<String, Object> image : images) {
                final String directurl = image.get("url").toString();
                final String imageID = new Regex(directurl, "(\\d+)\\.[a-z]+$").getMatch(0);
                final DownloadLink link;
                if (imageID != null) {
                    /* Link will be handled via civitai.com hoster plugin. */
                    link = this.createDownloadlink(CivitaiCom.createImageURL(imageID));
                    link.setProperty(CivitaiCom.PROPERTY_DIRECTURL, directurl);
                } else {
                    /* Link will be handled via DirectHTTP hoster plugin. */
                    link = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(directurl));
                }
                link.setProperty(CivitaiCom.PROPERTY_DATE, image.get("createdAt"));
                link.setProperty(CivitaiCom.PROPERTY_USERNAME, image.get("username"));
                link.setProperty(CivitaiCom.PROPERTY_DIRECTURL, directurl);
                link.setProperty(CivitaiCom.PROPERTY_TYPE, image.get("type"));
                final String filenameFromURL = Plugin.getFileNameFromURL(new URL(directurl));
                if (filenameFromURL != null) {
                    link.setName(filenameFromURL);
                }
                link.setAvailable(true);
                thisRet.add(link);
            }
            for (final DownloadLink result : thisRet) {
                result._setFilePackage(fp);
                ret.add(result);
            }
        }
        // if(ret.isEmpty()) {
        // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        // }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }

    @Override
    public Class<? extends CivitaiComConfig> getConfigInterface() {
        return CivitaiComConfig.class;
    }
}
