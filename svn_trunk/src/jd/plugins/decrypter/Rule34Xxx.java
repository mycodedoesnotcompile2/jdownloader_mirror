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
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.ExtensionsFilterInterface;
import org.jdownloader.plugins.components.config.Rule34xxxConfig;
import org.jdownloader.plugins.components.config.Rule34xxxConfig.AccessMode;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.nutils.encoding.HTMLEntities;
import jd.parser.Regex;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.SiteType.SiteTemplate;

@DecrypterPlugin(revision = "$Revision: 51363 $", interfaceVersion = 3, names = { "rule34.xxx" }, urls = { "https?://(?:www\\.)?rule34\\.xxx/index\\.php\\?page=post\\&s=(view\\&id=\\d+|list\\&tags=.+)" })
public class Rule34Xxx extends PluginForDecrypt {
    private final String        prefixLinkID                          = getHost().replaceAll("[\\.\\-]+", "") + "://";
    private static final String ERROR_MESSAG_API_CREDENTIALS_REQUIRED = "API credentials required. Add them in plugin settings or change access mode to website and try again.";

    public Rule34Xxx(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public void init() {
        super.init();
        Browser.setRequestIntervalLimitGlobal(getHost(), 250);
    }

    @Override
    public Class<? extends Rule34xxxConfig> getConfigInterface() {
        return Rule34xxxConfig.class;
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final Rule34xxxConfig cfg = PluginJsonConfig.get(this.getConfigInterface());
        final AccessMode am = cfg.getCrawlerAccessMode();
        switch (am) {
        case WEBSITE:
            return this.crawlWebsite(param);
        case API:
            return this.crawlAPI(param);
        case AUTO:
        default:
            try {
                return this.crawlAPI(param);
            } catch (AccountRequiredException e) {
                return this.crawlWebsite(param);
            }
        }
    }

    private ArrayList<DownloadLink> crawlAPI(final CryptedLink param) throws Exception {
        final Rule34xxxConfig cfg = PluginJsonConfig.get(this.getConfigInterface());
        /**
         * 2025-08-21: API key is required for all API requests we are using. <br>
         * See: https://board.jdownloader.org/showthread.php?p=550333#post550333
         */
        final boolean authRequired = true;
        final String apiUser = cfg.getAPIUser();
        final String apiKey = cfg.getAPIKey();
        if (authRequired && (StringUtils.isEmpty(apiUser) || StringUtils.isEmpty(apiKey))) {
            throw new AccountRequiredException(ERROR_MESSAG_API_CREDENTIALS_REQUIRED);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final UrlQuery query = UrlQuery.parse(param.getCryptedUrl());
        final String s = query.get("s");
        final String tags = query.get("tags");
        /* API docs: https://api.rule34.xxx/ */
        final String api_base = "https://api.rule34.xxx/index.php";
        if (s.equalsIgnoreCase("view")) {
            /* Crawl single post which can contain multiple images */
            final String postID = query.get("id");
            if (postID == null) {
                /* Developer mistake */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final UrlQuery apiquery = new UrlQuery();
            apiquery.appendEncoded("page", "dapi");
            apiquery.appendEncoded("s", "post");
            apiquery.appendEncoded("q", "index");
            apiquery.appendEncoded("id", postID);
            apiquery.appendEncoded("json", "1");
            if (!StringUtils.isEmpty(apiUser) && !StringUtils.isEmpty(apiKey)) {
                apiquery.appendEncoded("api_key", apiKey);
                apiquery.appendEncoded("user_id", apiUser);
            }
            br.getPage(api_base + "?" + apiquery.toString());
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final List<Map<String, Object>> results = (List<Map<String, Object>>) checkErrorsAPI();
            if (results == null || results.size() == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (final Map<String, Object> result : results) {
                final DownloadLink image = processImageItem(result);
                ret.add(image);
            }
        } else {
            /* Crawl tags */
            if (tags == null) {
                /* Developer mistake */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else if (StringUtils.isEmpty(tags)) {
                /* User mistake */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(Encoding.htmlDecode(tags).trim());
            /* Do not e.g. remove underscores */
            fp.setCleanupPackageName(false);
            final HashSet<String> dupes = new HashSet<String>();
            int page = 0;
            final int maxItemsPerPage = 100;
            final UrlQuery apiquery = new UrlQuery();
            apiquery.appendEncoded("page", "dapi");
            apiquery.appendEncoded("s", "post");
            apiquery.appendEncoded("q", "index");
            apiquery.appendEncoded("tags", tags);
            apiquery.appendEncoded("json", "1");
            apiquery.appendEncoded("limit", Integer.toString(maxItemsPerPage));
            if (!StringUtils.isEmpty(apiUser) && !StringUtils.isEmpty(apiKey)) {
                apiquery.appendEncoded("api_key", apiKey);
                apiquery.appendEncoded("user_id", apiUser);
            }
            pagination: do {
                apiquery.addAndReplace("pid", Integer.toString(page));
                br.getPage(api_base + "?" + apiquery.toString());
                if (br.getHttpConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else if (br.getRequest().getHtmlCode().length() <= 10) {
                    /* No json response -> Either last page or tags revealed zero results. */
                    if (ret.size() > 0) {
                        logger.info("Stopping because: Got blank page -> Reached end?");
                        break pagination;
                    } else {
                        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                    }
                }
                final List<Map<String, Object>> results = (List<Map<String, Object>>) checkErrorsAPI();
                if (results == null || results.size() == 0) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                int numberofNewItems = 0;
                for (final Map<String, Object> result : results) {
                    final String id = result.get("id").toString();
                    if (!dupes.add(id)) {
                        continue;
                    }
                    final DownloadLink image = processImageItem(result);
                    ret.add(image);
                    image._setFilePackage(fp);
                    distribute(image);
                    numberofNewItems++;
                }
                logger.info("Crawled page " + page + " | Found items so far: " + ret.size());
                if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    break pagination;
                } else if (numberofNewItems == 0) {
                    logger.info("Stopping because: Failed to find any new items on current page");
                    break pagination;
                } else if (numberofNewItems < maxItemsPerPage) {
                    logger.info("Stopping because: Current page contains less than " + maxItemsPerPage + " items -> Reached end?");
                    break pagination;
                } else {
                    /* Continue to next page */
                    sleep(1000, param);
                    page++;
                    continue pagination;
                }
            } while (true);
        }
        return ret;
    }

    private Object checkErrorsAPI() throws PluginException {
        /**
         * Response when API key is required but not provided: <br>
         * "Missing authentication. Go to api.rule34.xxx for more information"
         */
        if (br.getRequest().getHtmlCode().startsWith("\"Missing authentication")) {
            if (StringUtils.containsIgnoreCase(br.getURL(), "api_key")) {
                throw new AccountRequiredException("API credentials invalid! Enter valid credentials or switch access mode to website in plugin settings.");
            } else {
                throw new AccountRequiredException(ERROR_MESSAG_API_CREDENTIALS_REQUIRED);
            }
        }
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (!br.getRequest().getHtmlCode().startsWith("[") && !br.getRequest().getHtmlCode().startsWith("{")) {
            /* Assume that content is offline */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Object entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.OBJECT);
        return entries;
    }

    private DownloadLink processImageItem(final Map<String, Object> result) {
        final String link = result.get("file_url").toString();
        final String id = result.get("id").toString();
        final DownloadLink image = createDownloadlink(link);
        image.setAvailable(true);
        image.setLinkID(prefixLinkID + id);
        final String originalFilename = result.get("image").toString();
        final String extension = getFileNameExtensionFromString(originalFilename, ".bmp");
        if (PluginJsonConfig.get(this.getConfigInterface()).isPreferServerFilenamesOverPluginDefaultFilenames()) {
            image.setFinalFileName(originalFilename);
        } else {
            image.setFinalFileName("rule34xxx-" + id + extension);
        }
        if (!originalFilename.matches("(?i).+\\.(mp4)$")) {
            // for videos the md5 doesn't match
            image.setMD5Hash(result.get("hash").toString());
        }
        return image;
    }

    /** 2024-07-16: Usage of this is not recommended anymore due to Cloudflare blocking the requests. */
    private ArrayList<DownloadLink> crawlWebsite(final CryptedLink param) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = Encoding.htmlDecode(param.getCryptedUrl());
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404 || br.containsHTML(">\\s*No Images Found\\s*<|>\\s*This post was deleted")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("<h1>\\s*Nobody here but us chickens")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (StringUtils.endsWithCaseInsensitive(br.getURL(), "/index.php?page=post&s=list&tags=all")) {
            // redirect to base list page of all content/tags.. we don't want to crawl the entire website
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final boolean preferServerFilenames = PluginJsonConfig.get(this.getConfigInterface()).isPreferServerFilenamesOverPluginDefaultFilenames();
        if (contenturl.contains("&s=view&")) {
            // from list to post page
            final String imageParts[] = br.getRegex("'domain'\\s*:\\s*'(.*?)'\\s*,.*?'dir'\\s*:\\s*(\\d+).*?'img'\\s*:\\s*'(.*?)'.*?'base_dir'\\s*:\\s*'(.*?)'").getRow(0);
            String image = null;
            if (imageParts != null) {
                image = imageParts[0] + "/" + imageParts[3] + "/" + imageParts[1] + "/" + imageParts[2];
            } else {
                image = br.getRegex("<img[^>]+\\s+src=('|\")([^>]+)\\1 id=('|\")image\\3").getMatch(1);
                // can be video (Webm)
                if (image == null) {
                    image = br.getRegex("<source\\s+[^>]*src=('|\"|)(.*?)\\1").getMatch(1);
                }
            }
            if (image != null) {
                final String link = HTMLEntities.unhtmlentities(image);
                String url = Request.getLocation(link, br.getRequest());
                // 2022-05-16: rewrite us location to wimg because us is missing some files(404 not found)
                url = url.replaceFirst("(?i)/(us|wimg)\\.rule34\\.xxx/", "/wimg.rule34.xxx/");
                final DownloadLink dl = createDownloadlink(url);
                dl.setAvailable(true);
                final String id = new Regex(contenturl, "id=(\\d+)").getMatch(0);
                // set by decrypter from list, but not set by view!
                try { // Pevent NPE: https://svn.jdownloader.org/issues/84419
                    if (!StringUtils.equals(this.getCurrentLink().getSourceLink().getLinkID(), prefixLinkID + id)) {
                        dl.setLinkID(prefixLinkID + id);
                    }
                } catch (Exception e) {
                    dl.setLinkID(id);
                }
                final String extension = getFileNameExtensionFromString(image);
                final ExtensionsFilterInterface fileType = CompiledFiletypeFilter.getExtensionsFilterInterface(extension.replaceFirst("^\\.", ""));
                if (fileType != null) {
                    dl.setMimeHint(fileType);
                } else if (".webm".equals(extension)) {
                    dl.setMimeHint(CompiledFiletypeFilter.VideoExtensions.WEBM);
                } else {
                    dl.setMimeHint(CompiledFiletypeFilter.ImageExtensions.BMP);
                }
                if (preferServerFilenames) {
                    final String filename = getFileNameFromURL(new URL(dl.getPluginPatternMatcher()));
                    dl.setFinalFileName(filename);
                } else {
                    dl.setFinalFileName("rule34xxx-" + id + extension);
                }
                dl.setContentUrl(contenturl);
                ret.add(dl);
            }
        } else {
            /* Crawl tags */
            final UrlQuery query = UrlQuery.parse(br.getURL());
            String title = query.get("tags");
            FilePackage fp = null;
            if (title != null) {
                fp = FilePackage.getInstance();
                fp.setCleanupPackageName(Boolean.FALSE);
                title = URLEncode.decodeURIComponent(title).trim();
                fp.setName(title);
            }
            final HashSet<String> dupes = new HashSet<String>();
            int maxIndex = getMaxIndexWebsite(br);
            int page = 0;
            int index = 0;
            final String relativeURLWithoutParams = br._getURL().getPath();
            do {
                // from list to post page
                final String[] links = br.getRegex("<a id=\"p\\d+\" href=('|\")(/?index\\.php\\?page=post&(:?amp;)?s=view&(:?amp;)?id=\\d+)\\1").getColumn(1);
                if (links == null || links.length == 0) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                int numberofNewItems = 0;
                for (String link : links) {
                    link = HTMLEntities.unhtmlentities(link);
                    if (dupes.add(link)) {
                        numberofNewItems++;
                        final DownloadLink dl = createDownloadlink(Request.getLocation(link, br.getRequest()));
                        if (fp != null) {
                            fp.add(dl);
                        }
                        // we should set temp filename also
                        final String id = new Regex(link, "id=(\\d+)").getMatch(0);
                        dl.setLinkID(prefixLinkID + id);
                        dl.setName(id);
                        /* Don't set availablestatus here as items need to go through this crawler once again. */
                        // dl.setAvailable(true);
                        distribute(dl);
                        ret.add(dl);
                    }
                }
                logger.info("Crawled page " + page + "  Index: " + index + "/" + maxIndex + " | Found items so far: " + ret.size());
                if (page == maxIndex) {
                    final int newMaxIndex = this.getMaxIndexWebsite(br);
                    if (newMaxIndex > maxIndex) {
                        logger.info("Found new maxIndex | Old: " + maxIndex + " | New: " + newMaxIndex);
                        maxIndex = newMaxIndex;
                    }
                }
                index += numberofNewItems;
                final boolean hasNextPage = br.containsHTML("pid=" + index);
                if (this.isAbort()) {
                    logger.info("Decryption aborted by user");
                    break;
                } else if (numberofNewItems == 0) {
                    logger.info("Stopping because: Failed to find any new items on current page");
                    break;
                } else if (!hasNextPage) {
                    logger.info("Stopping because: Reached last page: " + page + " | Index: " + maxIndex);
                    break;
                } else {
                    sleep(1000, param);
                    page++;
                    query.addAndReplace("pid", Integer.toString(index));
                    br.getPage(relativeURLWithoutParams + "?" + query.toString());
                    continue;
                }
            } while (true);
        }
        return ret;
    }

    private int getMaxIndexWebsite(final Browser br) {
        int maxPage = 0;
        final String[] pages = br.getRegex("pid=(\\d+)").getColumn(0);
        for (final String pageStr : pages) {
            final int pageInt = Integer.parseInt(pageStr);
            if (pageInt > maxPage) {
                maxPage = pageInt;
            }
        }
        return maxPage;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.Danbooru;
    }
}