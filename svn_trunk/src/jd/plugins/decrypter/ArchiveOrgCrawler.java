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
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.storage.SimpleMapper;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.archiveorg.ArchiveOrgConfig;
import org.jdownloader.plugins.components.archiveorg.ArchiveOrgConfig.ArchiveOrgType;
import org.jdownloader.plugins.components.archiveorg.ArchiveOrgConfig.BookCrawlMode;
import org.jdownloader.plugins.components.archiveorg.ArchiveOrgConfig.DeselectedTypesMode;
import org.jdownloader.plugins.components.archiveorg.ArchiveOrgConfig.NonDownloadableBookPagesMode;
import org.jdownloader.plugins.components.archiveorg.ArchiveOrgConfig.PlaylistCrawlMode;
import org.jdownloader.plugins.components.archiveorg.ArchiveOrgConfig.SingleFileAdoptFolderStructureMode;
import org.jdownloader.plugins.components.archiveorg.ArchiveOrgConfig.SingleFilePathNotFoundMode;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.download.HashInfo;
import jd.plugins.hoster.ArchiveOrg;

@DecrypterPlugin(revision = "$Revision: 51392 $", interfaceVersion = 2, names = { "archive.org", "subdomain.archive.org" }, urls = { "https?://(?:www\\.)?archive\\.org/((?:details|download|stream|embed)/.+|search\\?query=.+)", "https?://[^/]+\\.archive\\.org/view_archive\\.php\\?archive=[^\\&]+(?:\\&file=[^\\&]+)?" })
public class ArchiveOrgCrawler extends PluginForDecrypt {
    public ArchiveOrgCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        /* 2023-07-10: looks like some detail pages return 503 instead of 200 */
        br.setAllowedResponseCodes(503);
        return br;
    }

    private final Pattern PATTERN_DOWNLOAD = Pattern.compile("/download/([\\w\\-]+).*", Pattern.CASE_INSENSITIVE);
    private final Pattern PATTERN_SEARCH   = Pattern.compile("/search\\?query=.+", Pattern.CASE_INSENSITIVE);
    private ArchiveOrg    hostPlugin       = null;

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        /* Last revision with old website/v1 functions: 51380 */
        final String contenturl = param.getCryptedUrl();
        final String identifier;
        if (new Regex(contenturl, PATTERN_SEARCH).patternFind()) {
            return this.crawlSearchQueryURL(param);
        } else if ((identifier = getIdentifierFromURL(contenturl)) != null) {
            return this.crawlMetadataJsonV2(identifier, contenturl);
        } else if (isCompressedArchiveURL(contenturl)) {
            return this.crawlArchiveContentV2(contenturl, null, null);
        } else {
            /* Unsupported link -> Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    private void ensureInitHosterplugin() throws PluginException {
        if (this.hostPlugin == null) {
            this.hostPlugin = (ArchiveOrg) getNewPluginForHostInstance("archive.org");
        }
    }

    /**
     * Returns identifier from given URL. </br>
     * The definition of how an identifier is supposed to look is vague so in this case we're also including username strings a la
     * "@<username>" as possible return values.
     */
    public static String getIdentifierFromURL(final String url) {
        /* htmldecode because "@" of "@username" could be url-encoded. */
        String identifier = new Regex(Encoding.htmlDecode(url), "(?i)/(?:details|embed|download|metadata|stream)/(@?[A-Za-z0-9\\-_\\.]{2,})").getMatch(0);
        return identifier;
    }

    private ArrayList<DownloadLink> crawlCollection(final String sourceurl, final String collectionIdentifier) throws Exception {
        if (StringUtils.isEmpty(collectionIdentifier)) {
            throw new IllegalArgumentException();
        }
        final boolean useBetaAPI = true;
        if (useBetaAPI) {
            return crawlBetaSearchAPI(sourceurl, collectionIdentifier, "collection");
        } else {
            return crawlViaScrapeAPI("collection:" + collectionIdentifier, -1);
        }
    }

    private ArrayList<DownloadLink> crawlSearchQueryURL(final CryptedLink param) throws Exception {
        final boolean useBetaAPI = true;
        if (useBetaAPI) {
            return this.crawlBetaSearchAPI(param.getCryptedUrl(), null, null);
        } else {
            /* Old handling */
            return crawlOldSearch(param);
        }
    }

    @Deprecated
    private ArrayList<DownloadLink> crawlOldSearch(final CryptedLink param) throws Exception {
        final ArchiveOrgConfig cfg = PluginJsonConfig.get(ArchiveOrgConfig.class);
        final int maxResults = cfg.getSearchTermCrawlerMaxResultsLimit();
        if (maxResults == 0) {
            logger.info("User disabled search term crawler -> Returning empty array");
            return new ArrayList<DownloadLink>();
        }
        final UrlQuery query = UrlQuery.parse(param.getCryptedUrl());
        String searchQuery = query.get("query");
        if (searchQuery != null) {
            /* Gets encoded later -> Needs to be decoded here. */
            searchQuery = URLEncode.decodeURIComponent(searchQuery).trim();
        }
        if (StringUtils.isEmpty(searchQuery)) {
            /* User supplied invalid URL. */
            throw new DecrypterRetryException(RetryReason.FILE_NOT_FOUND, "INVALID_SEARCH_QUERY");
        }
        return crawlViaScrapeAPI(searchQuery, maxResults);
    }

    /**
     * Uses search APIv1 </br>
     * API: Docs: https://archive.org/help/aboutsearch.htm </br>
     * 2024-07-17: This API is limited in functionality which is why we are moving away from it and use
     * {@link #crawlBetaSearchAPI(String, String)}. </br>
     * Example of things which are NOT possible via this API: </br>
     * - Find all uploads of a user </br>
     * - New style search queries such as: query=test&and%5B%5D=lending%3A"is_readable"&and%5B%5D=year%3A%5B1765+TO+1780%5D
     */
    @Deprecated
    private ArrayList<DownloadLink> crawlViaScrapeAPI(final String searchTerm, final int maxResultsLimit) throws Exception {
        if (StringUtils.isEmpty(searchTerm)) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        } else if (maxResultsLimit == 0) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        logger.info("Searching for: " + searchTerm + " | maxResultsLimit = " + maxResultsLimit);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final int maxNumberofItemsPerPage = 10000;
        final int minNumberofItemsPerPage = 100;
        final UrlQuery query = new UrlQuery();
        query.add("fields", "identifier");
        query.add("q", URLEncode.encodeURIComponent(searchTerm));
        final int maxNumberofItemsPerPageForThisRun;
        if (maxResultsLimit == -1) {
            /* -1 means unlimited -> Use internal hardcoded limit. */
            maxNumberofItemsPerPageForThisRun = maxNumberofItemsPerPage;
        } else if (maxResultsLimit <= minNumberofItemsPerPage) {
            maxNumberofItemsPerPageForThisRun = minNumberofItemsPerPage;
        } else if (maxResultsLimit < maxNumberofItemsPerPage) {
            maxNumberofItemsPerPageForThisRun = maxResultsLimit;
        } else {
            maxNumberofItemsPerPageForThisRun = maxNumberofItemsPerPage;
        }
        query.add("count", Integer.toString(maxNumberofItemsPerPageForThisRun));
        String cursor = null;
        int page = 1;
        boolean displayedSearchNotification = false;
        do {
            br.getPage("https://" + this.getHost() + "/services/search/v1/scrape?" + query.toString());
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final int totalNumberofItems = ((Number) entries.get("total")).intValue();
            if (totalNumberofItems == 0) {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, searchTerm);
            }
            if (!displayedSearchNotification) {
                String message = "This item leads to " + totalNumberofItems + " results.";
                if (maxResultsLimit != -1 && maxResultsLimit < totalNumberofItems) {
                    message += "\r\nImportant: Due to your currently defined limits in the plugin settings, only " + maxResultsLimit + " of these items will be crawled.";
                }
                displayBubbleNotification("Search " + searchTerm + " | Crawling", message);
                displayedSearchNotification = true;
            }
            final List<Map<String, Object>> items = (List<Map<String, Object>>) entries.get("items");
            boolean stopDueToCrawlLimitReached = false;
            for (final Map<String, Object> item : items) {
                final DownloadLink link = this.createDownloadlink("https://archive.org/details/" + item.get("identifier").toString());
                ret.add(link);
                /* The following statement makes debugging easier. */
                if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    distribute(link);
                }
                if (ret.size() == maxResultsLimit) {
                    /* Do not step out of main loop yet so we can get the log output down below one last time. */
                    stopDueToCrawlLimitReached = true;
                    break;
                }
            }
            final String lastCursor = cursor;
            cursor = (String) entries.get("cursor");
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size() + "/" + totalNumberofItems + " | maxResultsLimit: " + maxResultsLimit + " | Cursor: " + lastCursor + " | Next cursor: " + cursor);
            if (stopDueToCrawlLimitReached) {
                logger.info("Stopping because: Reached max allowed results: " + maxResultsLimit);
                break;
            } else if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            } else if (StringUtils.isEmpty(cursor)) {
                logger.info("Stopping because: Reached last page: " + lastCursor);
                break;
            } else if (ret.size() >= totalNumberofItems) {
                /* Additional fail-safe */
                logger.info("Stopping because: Found all items: " + totalNumberofItems);
                break;
            } else if (items.size() < maxNumberofItemsPerPageForThisRun) {
                /* Additional fail-safe */
                logger.info("Stopping because: Current page contains less items than max allowed per page for this run: " + maxNumberofItemsPerPageForThisRun);
                break;
            } else {
                /* Continue to next page */
                query.add("cursor", Encoding.urlEncode(cursor));
                page++;
            }
        } while (true);
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, "NO_SEARCH_RESULTS_FOR_QUERY_" + searchTerm);
        }
        return ret;
    }

    /**
     * Crawls items using the archive.org "BETA Search API" which, funnily enough, they are also using in production on their website. </br>
     * Successor of {@link #crawlViaScrapeAPI(String, int)}
     */
    private ArrayList<DownloadLink> crawlBetaSearchAPI(final String sourceurl, String identifier, final String type) throws Exception {
        if (StringUtils.isEmpty(sourceurl)) {
            throw new IllegalArgumentException();
        }
        final ArchiveOrgConfig cfg = PluginJsonConfig.get(ArchiveOrgConfig.class);
        final int maxResults = cfg.getSearchTermCrawlerMaxResultsLimit();
        if (maxResults == 0) {
            logger.info("User disabled search term crawler -> Returning empty array");
            return new ArrayList<DownloadLink>();
        }
        final int defaultMaxNumberofItemsPerPage = 100;
        final int maxNumberofItemsPerPageForThisRun;
        if (maxResults == -1) {
            /* -1 means unlimited -> Use internal hardcoded limit. */
            maxNumberofItemsPerPageForThisRun = defaultMaxNumberofItemsPerPage;
        } else if (maxResults < defaultMaxNumberofItemsPerPage) {
            maxNumberofItemsPerPageForThisRun = maxResults;
        } else {
            maxNumberofItemsPerPageForThisRun = defaultMaxNumberofItemsPerPage;
        }
        if (identifier == null) {
            identifier = getIdentifierFromURL(sourceurl);
        }
        final UrlQuery sourceurlquery = UrlQuery.parse(sourceurl);
        String userSearchQuery = sourceurlquery.get("query");
        if (userSearchQuery == null) {
            userSearchQuery = "";
        }
        final String titleHumanReadable;
        final boolean isUserProfile = identifier != null && identifier.startsWith("@");
        final boolean isCollection = StringUtils.equalsIgnoreCase(type, "collection");
        if (isUserProfile) {
            titleHumanReadable = "Profile " + identifier;
        } else if (isCollection) {
            titleHumanReadable = "Collection " + identifier;
        } else if (!StringUtils.isEmpty(userSearchQuery)) {
            titleHumanReadable = "Search " + userSearchQuery;
        } else {
            titleHumanReadable = "Search URL " + new URL(sourceurl).getPath();
        }
        final String startPageStr = sourceurlquery.get("page");
        /* Allow user to define custom start-page in given URL. */
        final int startPage;
        if (startPageStr != null && startPageStr.matches("\\d+")) {
            logger.info("Starting from user defined page: " + startPageStr);
            startPage = Integer.parseInt(startPageStr);
        } else {
            logger.info("Starting from page 1");
            startPage = 1;
        }
        logger.info("Starting from page " + startPage);
        final UrlQuery query = new UrlQuery();
        query.add("user_query", userSearchQuery);
        if (isUserProfile) {
            query.add("page_type", "account_details");
            query.add("page_target", URLEncode.encodeURIComponent(identifier));
            query.add("page_elements", "%5B%22uploads%22%5D");
        } else if (isCollection) {
            query.add("page_type", "collection_details");
            query.add("page_target", URLEncode.encodeURIComponent(identifier));
        }
        query.add("hits_per_page", Integer.toString(maxNumberofItemsPerPageForThisRun));
        final Map<String, Map<String, String>> filter_map = parseFilterMap(sourceurl);
        if (!filter_map.isEmpty()) {
            final String json = new SimpleMapper().setPrettyPrintEnabled(false).objectToString(filter_map);
            query.add("filter_map", URLEncode.encodeURIComponent(json));
        }
        /* Not needed. If not given, server-side decides how results are sorted. */
        // query.add("sort", "publicdate%3Adesc");
        query.add("aggregations", "false");
        /* Not important */
        // query.add("uid", "NOT_NEEDED");
        query.add("client_url", URLEncode.encodeURIComponent(sourceurl));
        // query.add("client_url", sourceurl);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Browser brc = br.cloneBrowser();
        brc.setAllowedResponseCodes(400);
        int page = startPage;
        final HashSet<String> dupes = new HashSet<String>();
        int totalNumberofItems = -1;
        boolean displayedSearchNotification = false;
        pagination: do {
            query.addAndReplace("page", Integer.toString(page));
            /* This looks to be an internally used version of public crawl/search API v2 beta, see: https://archive.org/services/swagger/ */
            brc.getPage("https://" + getHost() + "/services/search/beta/page_production/?" + query.toString());
            if (brc.getHttpConnection().getResponseCode() == 400) {
                if (ret.size() > 0) {
                    logger.info("Stopping because: Surprisingly got http response 400 | Possibly missing items: " + (totalNumberofItems - ret.size()));
                    if (ret.size() < totalNumberofItems) {
                        displayBubbleNotification(titleHumanReadable + " | Search crawler stopped early", "Found items: " + ret.size() + "/" + totalNumberofItems);
                    }
                    break;
                } else {
                    /* This happened on the first page -> Assume that this profile is invalid/offline */
                    throw new DecrypterRetryException(RetryReason.FILE_NOT_FOUND, "INVALID_SEARCH_QUERY");
                }
            }
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            final Map<String, Object> hitsmap;
            if (isUserProfile) {
                hitsmap = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "response/body/page_elements/uploads/hits");
            } else {
                hitsmap = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "response/body/hits");
            }
            totalNumberofItems = ((Number) hitsmap.get("total")).intValue();
            if (!displayedSearchNotification) {
                String message = "This item leads to " + totalNumberofItems + " results.";
                if (maxResults != -1 && maxResults < totalNumberofItems) {
                    message += "\r\nImportant: Due to your currently defined limits in the plugin settings, only " + maxResults + " of these items will be crawled.";
                }
                if (startPage > 1) {
                    message += "\r\nImportant: According to the parameters in your added URL, this crawl process will not start from the first page but from page " + startPage + " so this will not find any items of previous pages.";
                }
                displayBubbleNotification(titleHumanReadable + " | Crawling", message);
                displayedSearchNotification = true;
            }
            final List<Map<String, Object>> hits = (List<Map<String, Object>>) hitsmap.get("hits");
            int numberofNewItemsThisPage = 0;
            boolean stopDueToCrawlLimitReached = false;
            for (final Map<String, Object> hit : hits) {
                final Map<String, Object> fields = (Map<String, Object>) hit.get("fields");
                final String this_identifier = fields.get("identifier").toString();
                if (!dupes.add(this_identifier)) {
                    continue;
                }
                numberofNewItemsThisPage++;
                final DownloadLink result = this.createDownloadlink("https://" + this.getHost() + "/download/" + this_identifier);
                ret.add(result);
                /* Distribute results live as pagination can run for a very very long time. */
                /* The following statement makes debugging easier. */
                if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    distribute(result);
                }
                if (dupes.size() == maxResults) {
                    stopDueToCrawlLimitReached = true;
                    break;
                }
            }
            logger.info("Crawled page " + page + " | Crawled new items this page: " + numberofNewItemsThisPage + " | Crawled items so far: " + ret.size() + "/" + totalNumberofItems + " | Max result limit: " + maxResults);
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break pagination;
            } else if (totalNumberofItems == 0) {
                if (isUserProfile) {
                    throw new DecrypterRetryException(RetryReason.EMPTY_PROFILE, "EMPTY_PROFILE_" + identifier);
                } else {
                    throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, "NO_SEARCH_RESULTS_FOR_QUERY_" + userSearchQuery);
                }
            } else if (stopDueToCrawlLimitReached) {
                logger.info("Stopping because: Reached user defined limit: " + maxResults);
                break pagination;
            } else if (hits.size() < maxNumberofItemsPerPageForThisRun) {
                /* Main stop condition */
                logger.info("Stopping because: Current page contains less items than " + maxNumberofItemsPerPageForThisRun + " --> Reached end");
                break pagination;
            } else if (ret.size() >= totalNumberofItems) {
                /* Fail-safe 1 */
                logger.info("Stopping because: Found all items: " + totalNumberofItems);
                break pagination;
            } else if (numberofNewItemsThisPage == 0) {
                /* Fail-safe 2 */
                logger.info("Stopping because: Failed to find any new items on current page");
                break pagination;
            } else {
                /* Continue to next page */
                page++;
            }
        } while (!this.isAbort());
        if (ret.isEmpty() && !filter_map.isEmpty()) {
            logger.info("Got zero results which might be the case because the user has supplied filters which are too restrictive: " + filter_map);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlArchiveContentV2(String contenturl, final String desiredFilePath, final Object fallbackResult) throws Exception {
        this.ensureInitHosterplugin();
        if (fallbackResult != null && !(fallbackResult instanceof DownloadLink) && !(fallbackResult instanceof List)) {
            throw new IllegalArgumentException();
        }
        String desiredFileName = null;
        if (desiredFilePath != null) {
            final String[] pathSegments = desiredFilePath.split("/");
            desiredFileName = pathSegments[pathSegments.length - 1];
            /*
             * Remove filename from URL so that we can find that file in the list of file which will give us more information such as the
             * last-modified date.
             */
            contenturl = contenturl.replaceFirst(Pattern.quote(URLEncode.encodeURIComponent(desiredFileName)) + "$", "");
        } else {
            final UrlQuery query = UrlQuery.parse(contenturl);
            final String filenameFromURLParameter = query.get("file");
            if (filenameFromURLParameter != null) {
                desiredFileName = Encoding.htmlDecode(filenameFromURLParameter);
                /* Change URL */
                query.remove("file");
                contenturl = URLHelper.getUrlWithoutParams(contenturl) + "?" + query.toString();
            }
        }
        ensureInitHosterplugin();
        /* Login whenever possible */
        final Account account = AccountController.getInstance().getValidAccount(hostPlugin.getHost());
        if (account != null) {
            hostPlugin.login(account, false);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        try {
            URLConnectionAdapter con = null;
            try {
                con = br.openGetConnection(contenturl);
                /* Check if we have a single file. */
                if (this.looksLikeDownloadableContent(con)) {
                    final DownloadLink link = new DownloadLink(hostPlugin, null, hostPlugin.getHost(), contenturl, true);
                    if (con.getCompleteContentLength() > 0) {
                        if (con.isContentDecoded()) {
                            link.setDownloadSize(con.getCompleteContentLength());
                        } else {
                            link.setVerifiedFileSize(con.getCompleteContentLength());
                        }
                    }
                    link.setFinalFileName(getFileNameFromConnection(con));
                    link.setAvailable(true);
                    ret.add(link);
                    return ret;
                } else {
                    br.followConnection();
                }
            } finally {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
            if (ArchiveOrg.isAccountRequired(br) || ArchiveOrg.isItemUnavailable(br)) {
                throw new AccountRequiredException();
            } else if (br.getHttpConnection().getResponseCode() == 403) {
                throw new AccountRequiredException();
            } else if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (!isCompressedArchiveURL(br.getURL())) {
                /* Redirect to some unsupported URL. */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final UrlQuery query = UrlQuery.parse(br.getURL());
            final String archiveName = new Regex(br.getURL(), ".*/([^/]+)$").getMatch(0);
            final String archivePath = query.get("archive");
            final FilePackage fp = FilePackage.getInstance();
            if (archivePath != null) {
                fp.setName(Encoding.htmlDecode(archivePath).trim());
            } else if (archiveName != null) {
                fp.setName(Encoding.htmlDecode(archiveName).trim());
            } else {
                /* Fallback */
                fp.setName(br._getURL().getPath());
            }
            final String[] htmls = br.getRegex("<tr><td>(.*?)</tr>").getColumn(0);
            boolean foundDesiredFile = false;
            for (final String html : htmls) {
                final String lastModifiedDateStr = new Regex(html, ">(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})<").getMatch(0);
                String url = new Regex(html, "(?i)(/download/[^\"\\']+)").getMatch(0);
                final String filesizeBytesStr = new Regex(html, "id=\"size\">(\\d+)").getMatch(0);
                if (StringUtils.isEmpty(url)) {
                    /* Skip invalid items */
                    continue;
                }
                url = "https://archive.org" + url;
                final DownloadLink dl = this.createDownloadlink(url);
                if (filesizeBytesStr != null) {
                    dl.setDownloadSize(Long.parseLong(filesizeBytesStr));
                }
                if (lastModifiedDateStr != null) {
                    final long lastModifiedTimestamp = TimeFormatter.getMilliSeconds(lastModifiedDateStr, "yyyy-MM-dd HH:mm", Locale.ENGLISH);
                    dl.setLastModifiedTimestamp(lastModifiedTimestamp);
                }
                dl.setAvailable(true);
                if (fp != null) {
                    dl._setFilePackage(fp);
                }
                ret.add(dl);
                if (desiredFileName != null && Encoding.htmlDecode(url).endsWith(desiredFileName)) {
                    logger.info("Found desired file: " + desiredFileName);
                    ret.clear();
                    /* Do not set FilePackage for single desired files. */
                    dl._setFilePackage(null);
                    ret.add(dl);
                    foundDesiredFile = true;
                    break;
                }
            }
            if (desiredFileName != null && !foundDesiredFile) {
                logger.info("Failed to find desired file with filename: " + desiredFileName);
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            return ret;
        } catch (final PluginException e) {
            /* Desired item looks to be offline -> Decide what to return. */
            final ArchiveOrgConfig cfg = PluginJsonConfig.get(ArchiveOrgConfig.class);
            final SingleFilePathNotFoundMode mode = cfg.getSingleFilePathNotFoundMode();
            if (e.getLinkStatus() == LinkStatus.ERROR_FILE_NOT_FOUND && mode == SingleFilePathNotFoundMode.ADD_NOTHING_AND_DISPLAY_ADDED_URL_AS_OFFLINE && fallbackResult != null) {
                if (fallbackResult instanceof DownloadLink) {
                    ret.add((DownloadLink) fallbackResult);
                } else {
                    return (ArrayList<DownloadLink>) fallbackResult;
                }
            } else {
                throw e;
            }
        }
        /* This should never happen! */
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    public ArrayList<DownloadLink> crawlBook(final Browser br, final String ajaxurl, final Account account) throws Exception {
        if (StringUtils.isEmpty(ajaxurl)) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        final String identifier = UrlQuery.parse(ajaxurl).get("id");
        if (StringUtils.isEmpty(identifier)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(ajaxurl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        ensureInitHosterplugin();
        final Map<String, Object> root = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> data = (Map<String, Object>) root.get("data");
        final Map<String, Object> metadata = (Map<String, Object>) data.get("metadata");
        final Object descriptionObject = metadata.get("description");
        final String description;
        if (descriptionObject instanceof String) {
            description = (String) descriptionObject;
        } else if (descriptionObject instanceof List) {
            description = StringUtils.join((List) descriptionObject, ";");
        } else {
            description = null;
        }
        final Map<String, Object> lendingInfo = (Map<String, Object>) data.get("lendingInfo");
        // final Map<String, Object> lendingStatus = (Map<String, Object>) lendingInfo.get("lendingStatus");
        final long daysLeftOnLoan = ((Number) lendingInfo.get("daysLeftOnLoan")).longValue();
        final long secondsLeftOnLoan = ((Number) lendingInfo.get("secondsLeftOnLoan")).longValue();
        long loanedSecondsLeft = 0;
        if (daysLeftOnLoan > 0) {
            loanedSecondsLeft += daysLeftOnLoan * 24 * 60 * 60;
        }
        if (secondsLeftOnLoan > 0) {
            loanedSecondsLeft += secondsLeftOnLoan;
        }
        final Map<String, Object> brOptions = (Map<String, Object>) data.get("brOptions");
        final Boolean isLendingRequired = (Boolean) lendingInfo.get("isLendingRequired");
        final Boolean isPrintDisabledOnly = (Boolean) lendingInfo.get("isPrintDisabledOnly");
        String contentURLFormat = generateBookContentURL(identifier);
        final String bookId = brOptions.get("bookId").toString();
        String title = ((String) brOptions.get("bookTitle")).trim();
        final String subPrefix = (String) brOptions.get("subPrefix");
        final boolean isMultiVolumeBook;
        if (subPrefix != null && !subPrefix.equals(bookId)) {
            /**
             * Books can have multiple volumes. In this case lending the main book will basically lend all volumes alltogether. </br>
             * Problem: Title is the same for all items --> Append this subPrefix to the title to fix that.
             */
            title += " - " + subPrefix;
            contentURLFormat += "/" + subPrefix;
            isMultiVolumeBook = true;
        } else {
            isMultiVolumeBook = false;
        }
        final String pageFormat;
        if (ajaxurl.matches("(?i).*/page/n\\d+.*")) {
            pageFormat = "/page/n%d";
        } else {
            pageFormat = "/page/%d";
        }
        /*
         * Defines how book pages will be arranged on the archive.org website. User can open single pages faster in browser if we get this
         * right.
         */
        final String bookDisplayMode = new Regex(ajaxurl, "(?i)/mode/([^/]+)").getMatch(0);
        final List<Object> imagesO = (List<Object>) brOptions.get("data");
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        int internalPageIndex = 0;
        for (final Object imageO : imagesO) {
            /**
             * Most of all objects will contain an array with 2 items --> Books always have two viewable pages. </br>
             * Exception = First page --> Cover
             */
            final List<Map<String, Object>> bookpages = (List<Map<String, Object>>) imageO;
            for (final Map<String, Object> bookpage : bookpages) {
                /* When this starts at 0 this means the book has a cover else this will start at 1 -> No cover. */
                final int archiveOrgPageIndex = ((Number) bookpage.get("leafNum")).intValue();
                final String url = bookpage.get("uri").toString();
                final DownloadLink link = new DownloadLink(hostPlugin, null, "archive.org", url, true);
                String contentURL = contentURLFormat;
                if (archiveOrgPageIndex > 1) {
                    contentURL += String.format(pageFormat, archiveOrgPageIndex + 1);
                }
                if (bookDisplayMode != null) {
                    contentURL += "/mode/" + bookDisplayMode;
                }
                link.setContentUrl(contentURL);
                link.setProperty(ArchiveOrg.PROPERTY_BOOK_ID, identifier);
                link.setProperty(ArchiveOrg.PROPERTY_BOOK_PAGE, archiveOrgPageIndex);
                link.setProperty(ArchiveOrg.PROPERTY_BOOK_PAGE_INTERNAL_INDEX, internalPageIndex);
                if (isMultiVolumeBook) {
                    link.setProperty(ArchiveOrg.PROPERTY_BOOK_SUB_PREFIX, subPrefix);
                }
                if (Boolean.TRUE.equals(isLendingRequired) || Boolean.TRUE.equals(isPrintDisabledOnly)) {
                    link.setProperty(ArchiveOrg.PROPERTY_IS_LENDING_REQUIRED, true);
                }
                if (loanedSecondsLeft > 0) {
                    link.setProperty(ArchiveOrg.PROPERTY_IS_BORROWED_UNTIL_TIMESTAMP, System.currentTimeMillis() + loanedSecondsLeft * 1000);
                }
                /**
                 * Mark pages that are not viewable in browser as offline. </br>
                 * If we have borrowed this book, this field will not exist at all.
                 */
                final Object viewable = bookpage.get("viewable");
                if (Boolean.FALSE.equals(viewable)) {
                    /* Only downloadable with account as book needs to be borrowed to view the pages. */
                    if (account == null && PluginJsonConfig.get(ArchiveOrgConfig.class).getNonDownloadableBookPagesMode() == NonDownloadableBookPagesMode.SET_AVAILABLE_STATUS_OFFLINE) {
                        /* User wants non-downloadable items to be displayed as offline. */
                        link.setAvailable(false);
                    } else {
                        /* Always mark all pages as online. Non-viewable pages can only be downloaded when an account is present. */
                        link.setAvailable(true);
                    }
                } else {
                    link.setAvailable(true);
                    if (account == null || loanedSecondsLeft == 0) {
                        /* This page can be viewed/downloaded for free and without account. */
                        link.setProperty(ArchiveOrg.PROPERTY_IS_FREE_DOWNLOADABLE_BOOK_PREVIEW_PAGE, true);
                    }
                }
                ret.add(link);
                internalPageIndex++;
            }
        }
        if (account != null) {
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
        /* Add additional properties and filenames. */
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        if (!StringUtils.isEmpty(description)) {
            fp.setComment(description);
        }
        fp.setPackageKey("internetarchive://book/" + identifier + "/" + subPrefix);
        /* Now we know how many pages the book has -> Set additional information and filename. */
        final int padLength = StringUtils.getPadLength(internalPageIndex);
        for (final DownloadLink result : ret) {
            result.setProperty(ArchiveOrg.PROPERTY_BOOK_PAGE_MAX, internalPageIndex);
            final int thispage = result.getIntegerProperty(ArchiveOrg.PROPERTY_BOOK_PAGE_INTERNAL_INDEX, 0);
            /* Set filename */
            if (result.isAvailable()) {
                result.setFinalFileName(StringUtils.formatByPadLength(padLength, thispage) + "_" + title + ".jpg");
            } else {
                result.setFinalFileName(StringUtils.formatByPadLength(padLength, thispage) + "_ADD_ACCOUNT_AND_RE_CRAWL_TO_DOWNLOAD_" + title + ".jpg");
                result.setComment("Archive.org Account required to be able to download this book page. Add an archive.org account to JDownloader, then delete and re-add the link to this book to be able to download all book pages.");
            }
            /* Assign FilePackage to item so all results of this run get placed into one package. */
            result._setFilePackage(fp);
        }
        return ret;
    }

    private String findBookReaderURLWebsite(final Browser br) {
        String url = br.getRegex("(?:\\'|\")([^\\'\"]+BookReaderJSIA\\.php\\?[^\\'\"]+)").getMatch(0);
        if (url == null) {
            return null;
        }
        url = PluginJSonUtils.unescape(url);
        return url;
    }

    /** Work in progress, see https://archive.org/metadata/<identifier> */
    private ArrayList<DownloadLink> crawlMetadataJsonV2(final String identifier, final String sourceurl) throws Exception {
        if (StringUtils.isEmpty(identifier)) {
            throw new IllegalArgumentException();
        }
        String sourceurlForThisHandling = sourceurl;
        if (identifier.startsWith("@")) {
            /* ideantifier looks to be a user profile. */
            return this.crawlProfile(identifier, sourceurl);
        }
        if (sourceurlForThisHandling != null) {
            /* Correct source-URL */
            /* Remove params so that URL-paths will be correct down below. */
            sourceurlForThisHandling = URLHelper.getUrlWithoutParams(sourceurlForThisHandling);
            /* Prevent handling down below from picking up specific parts of the URL as used desired file-path. */
            sourceurlForThisHandling = sourceurlForThisHandling.replaceAll("(?i)/start/\\d+/end/\\d+$", "");
        }
        /* The following request will return an empty map if the given identifier is invalid. */
        final Browser brc = br.cloneBrowser();
        /* The json answer can be really big. */
        brc.setLoadLimit(Integer.MAX_VALUE);
        brc.getPage("https://" + getHost() + "/metadata/" + Encoding.urlEncode(identifier));
        final Map<String, Object> root = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        // final Boolean is_dark = (Boolean) root.get("is_dark"); // This means that the content is offline(?)
        final List<Map<String, Object>> root_files = (List<Map<String, Object>>) root.get("files");
        if (root_files == null || root_files.isEmpty()) {
            /* Deleted item */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> root_metadata = (Map<String, Object>) root.get("metadata");
        /* There is different servers to choose from e.g. see also fields "d1", "d2" and "workable_servers". */
        final String mediatype = root_metadata.get("mediatype").toString();
        if (StringUtils.equalsIgnoreCase(mediatype, "collection")) {
            /* Crawl item with collection crawler */
            return this.crawlCollection(sourceurl, identifier);
        }
        final ArchiveOrgConfig cfg = PluginJsonConfig.get(ArchiveOrgConfig.class);
        PlaylistCrawlMode playlistCrawlMode = cfg.getPlaylistCrawlMode202404();
        if (playlistCrawlMode == PlaylistCrawlMode.DEFAULT) {
            playlistCrawlMode = PlaylistCrawlMode.AUTO;
        }
        final boolean isDownloadPage = sourceurl.matches("(?i)https?://[^/]+/download/.+");
        final String desiredSubpath;
        if (isDownloadPage) {
            desiredSubpath = new Regex(sourceurlForThisHandling, ".*/(" + Pattern.quote(identifier) + "/.+)").getMatch(0);
        } else {
            /* No download page/link -> No desired subpath */
            desiredSubpath = null;
        }
        boolean allowCrawlArchiveContents = false;
        String desiredSubpathDecoded = null;
        String desiredSubpathDecoded2 = null;
        if (desiredSubpath != null) {
            /*
             * In this case we only want to get all files in a specific subfolder or even only a single file.
             */
            desiredSubpathDecoded = Encoding.htmlDecode(desiredSubpath);
            /* Remove slash from end to allow for proper filename matching. */
            if (desiredSubpathDecoded.endsWith("/")) {
                desiredSubpathDecoded = desiredSubpathDecoded.substring(0, desiredSubpathDecoded.lastIndexOf("/"));
                /*
                 * URL ended with a slash which means that if the target file is an archive, user doesn't want to download the file itself
                 * but the contents inside that file.
                 */
                allowCrawlArchiveContents = true;
            }
            final String[] pathSegments = desiredSubpathDecoded.split("/");
            if (pathSegments.length >= 3) {
                desiredSubpathDecoded2 = pathSegments[pathSegments.length - 2];
            }
        }
        final String server = root.get("server").toString();
        final String dir = root.get("dir").toString();
        final Object descriptionObject = root_metadata.get("description");
        String description = null;
        if (descriptionObject instanceof String) {
            description = (String) descriptionObject;
        } else if (descriptionObject instanceof List) {
            description = StringUtils.join((List) descriptionObject, ";");
        }
        /* Crawl files */
        final Map<String, DownloadLink> itemMapping = new HashMap<String, DownloadLink>();
        final Map<String, String> originalMapping = new HashMap<String, String>();
        final ArrayList<String> audioPlaylistItems = new ArrayList<String>();
        final ArrayList<DownloadLink> desiredSubpathItems = new ArrayList<DownloadLink>();
        DownloadLink singleDesiredFile = null;
        DownloadLink singleDesiredFile2 = null;
        final ArrayList<DownloadLink> selectedItems = new ArrayList<DownloadLink>();
        final Map<String, FilePackage> packagemap = new HashMap<String, FilePackage>();
        final Set<ArchiveOrgType> selectedTypes = cfg.getFileCrawlerTypesToCrawl();
        final boolean skipDeselectedItems = cfg.getDeselectedTypesLinksMode() == DeselectedTypesMode.DO_NOT_ADD_SKIP;
        final boolean crawlOriginalFilesOnly = selectedTypes.size() == 1 && selectedTypes.contains(ArchiveOrgType.ORIGINAL);
        final List<String> skippedItemsFilepaths = new ArrayList<String>();
        String totalLengthSecondsOriginalStr = null;
        String totalLengthSecondsDerivativeStr = null;
        Object desiredFileArchiveFileCount = null;
        final HashSet<String> originalFilenamesDupeCollection = new HashSet<String>();
        /** Restricted access usually means that original files are not downloadable or only DRM protected / encrypted items exist. */
        final boolean isAccessRestricted = StringUtils.equalsIgnoreCase((String) root_metadata.get("access-restricted-item"), "true");
        /* If this gets set to true, we expect 60 second video streams to be available. */
        boolean identifierDotMp4Exists = false;
        logger.info("Crawling all (selected) files below path: " + desiredSubpathDecoded);
        if (selectedTypes.isEmpty()) {
            logger.info("User has deselected all types -> Adding all instead");
        }
        for (final Map<String, Object> filemap : root_files) {
            final String source = filemap.get("source").toString(); // "original", "derivative" or "metadata"
            final String format = (String) filemap.get("format");
            /* Boolean as string */
            final boolean isOldVersion = StringUtils.equalsIgnoreCase((String) filemap.get("old_version"), "true");
            final String name = filemap.get("name").toString();
            final Object originalO = filemap.get("original");
            /**
             * Determine type of this item. <br>
             * Important: Check for thumbnail first!! <br>
             * A thumbnail is also a devivative! <br>
             * Items with source "original" can at the same time contain "format" with value "Metadata" so check for original only after
             * checking for metadata!
             */
            ArchiveOrgType thistype = null;
            if (StringUtils.equalsIgnoreCase(format, "Thumbnail")) {
                thistype = ArchiveOrgType.THUMBNAIL;
            } else if (StringUtils.equalsIgnoreCase(format, "metadata")) {
                thistype = ArchiveOrgType.METADATA;
            } else if (source.equalsIgnoreCase("metadata")) {
                if (name.matches("(?i).+\\.torrent$")) {
                    thistype = ArchiveOrgType.METADATA_TORRENT;
                } else {
                    thistype = ArchiveOrgType.METADATA;
                }
            } else if (source.equalsIgnoreCase("original")) {
                if (StringUtils.endsWithCaseInsensitive(name, "__ia_thumb.jpg")) {
                    /* Special detection for unmarked thumbnails e.g. /download/MSNBCW_20211108_030000_Four_Seasons_Total_Documentary */
                    thistype = ArchiveOrgType.THUMBNAIL;
                } else {
                    thistype = ArchiveOrgType.ORIGINAL;
                }
            } else if (source.equalsIgnoreCase("derivative")) {
                if ("Columbia Peaks".equals(format)) {
                    // https://help.archive.org/help/audio-and-music-items-a-basic-guide/
                    // Columbia Peaks: .afpk are audio fingerprint files created by running the audfprint package by Prof Daniel Ellis from
                    // Columbia University.
                    thistype = ArchiveOrgType.DERIVATIVE_PEAKS;
                } else if ("Spectrogram".equals(format)) {
                    // https://help.archive.org/help/audio-and-music-items-a-basic-guide/
                    // Spectogram: a visual representation of the spectrum of frequencies of sound or other signals as they vary with time.
                    thistype = ArchiveOrgType.DERIVATIVE_SPECTROGRAM;
                } else if (name.matches("(?i).+\\.(png|jpg)$") && originalO instanceof String && ((String) originalO).matches("(?i).+\\.(mp3|ogg|flac|mp4|mkv|wav|opus)$") && !"Spectrogram".equals(format)) {
                    thistype = ArchiveOrgType.DERIVATIVE_COVER;
                } else {
                    thistype = ArchiveOrgType.DERIVATIVE;
                }
            } else {
                /* This should never happen */
                logger.warning("Found unknown source/type: " + source);
            }
            final boolean userWantsItem = thistype == null || selectedTypes.isEmpty() || selectedTypes.contains(thistype);
            /* Boolean as string */
            final boolean isAccountRequiredForDownload = StringUtils.equalsIgnoreCase((String) filemap.get("private"), "true");
            String pathWithFilename = name;
            if (Encoding.isHtmlEntityCoded(pathWithFilename)) {
                /* Will sometimes contain "&amp;" */
                pathWithFilename = Encoding.htmlOnlyDecode(pathWithFilename);
            }
            if (isOldVersion) {
                /* Skip old elements otherwise we run into the danger of adding the same item multiple times. */
                skippedItemsFilepaths.add(pathWithFilename);
                continue;
            }
            if (!identifierDotMp4Exists && pathWithFilename.equals(identifier + ".mp4")) {
                identifierDotMp4Exists = true;
            }
            /* Find path- and filename */
            /* Relative path to this file including identifier as root folder. */
            String pathToCurrentFolder;
            String filename = null;
            if (pathWithFilename.contains("/")) {
                /* Separate path and filename. */
                pathToCurrentFolder = "";
                final String[] pathSegments = pathWithFilename.split("/");
                int index = 0;
                for (final String pathSegment : pathSegments) {
                    final boolean isLastSegment = index == pathSegments.length - 1;
                    if (isLastSegment) {
                        filename = pathSegment;
                    } else {
                        if (pathToCurrentFolder == null) {
                            pathToCurrentFolder = pathSegment;
                        } else {
                            if (pathToCurrentFolder.length() > 0) {
                                pathToCurrentFolder += "/";
                            }
                            pathToCurrentFolder += pathSegment;
                        }
                    }
                    index++;
                }
                /* Add identifier slash root dir name to path. */
                pathToCurrentFolder = identifier + "/" + pathToCurrentFolder;
            } else {
                /* Current file is in root folder */
                pathToCurrentFolder = identifier;
                filename = pathWithFilename;
            }
            String url = "https://" + getHost() + "/download/" + identifier;
            if (pathWithFilename.startsWith("/")) {
                url += URLEncode.encodeURIComponent(pathWithFilename);
            } else {
                url += "/" + URLEncode.encodeURIComponent(pathWithFilename);
            }
            /* Directurl is not needed here because the "/download/..." URL will redirect to it later on download attempt. */
            // final String directurl = "https://" + server + dir + "/" + URLEncode.encodeURIComponent(pathWithFilename);
            final DownloadLink file = this.createDownloadlink(url);
            itemMapping.put(name, file);
            if (thistype == ArchiveOrgType.ORIGINAL) {
                originalMapping.put(name, null);
                if (totalLengthSecondsOriginalStr == null) {
                    totalLengthSecondsOriginalStr = (String) filemap.get("length");
                }
            } else if ("derivative".equals(source) && originalO instanceof String) {
                originalMapping.put(name, originalO.toString());
                if (totalLengthSecondsDerivativeStr == null && StringUtils.endsWithCaseInsensitive(name, ".mp4")) {
                    totalLengthSecondsDerivativeStr = (String) filemap.get("length");
                }
            }
            file.setProperty(ArchiveOrg.PROPERTY_FILENAME, filename);
            file.setProperty(ArchiveOrg.PROPERTY_ARTIST, filemap.get("artist")); // Optional field
            file.setProperty(ArchiveOrg.PROPERTY_TITLE, filemap.get("title")); // Optional field
            file.setProperty(ArchiveOrg.PROPERTY_ARTIST, filemap.get("artist")); // optional field
            file.setProperty(ArchiveOrg.PROPERTY_GENRE, filemap.get("genre")); // optional field
            final Object fileSizeO = filemap.get("size");
            if (fileSizeO != null) {
                if (fileSizeO instanceof Number) {
                    file.setVerifiedFileSize(((Number) fileSizeO).longValue());
                } else {
                    file.setVerifiedFileSize(Long.parseLong(fileSizeO.toString()));
                }
            }
            final String crc32 = (String) filemap.get("crc32");
            if (crc32 != null) {
                file.setHashInfo(HashInfo.parse(crc32));
            }
            final String md5 = (String) filemap.get("md5");
            if (md5 != null) {
                file.setMD5Hash(md5);
            }
            final String sha1 = (String) filemap.get("sha1");
            if (sha1 != null) {
                file.setSha1Hash(sha1);
            }
            file.setProperty(ArchiveOrg.PROPERTY_TIMESTAMP_FROM_API_LAST_MODIFIED, filemap.get("mtime"));
            if (isAccountRequiredForDownload) {
                file.setProperty(ArchiveOrg.PROPERTY_IS_ACCOUNT_REQUIRED, true);
            }
            if (StringUtils.containsIgnoreCase(format, "MP3")) {
                /* Item looks to be part of a playlist. */
                audioPlaylistItems.add(name);
            }
            file.setAvailable(true);
            file.setRelativeDownloadFolderPath(pathToCurrentFolder);
            /* Set filename */
            ArchiveOrg.setFinalFilename(file, filename);
            FilePackage fp = packagemap.get(pathToCurrentFolder);
            if (fp == null) {
                fp = FilePackage.getInstance();
                fp.setName(pathToCurrentFolder);
                fp.setComment(description);
                packagemap.put(pathToCurrentFolder, fp);
            }
            file._setFilePackage(fp);
            /* Disables unwanted items -> They will be greyed out in GUI */
            file.setEnabled(userWantsItem);
            /* Add items to list of selected results. */
            final String fullPath = identifier + "/" + pathWithFilename;
            if (desiredSubpathDecoded != null) {
                /* Add item to list of results which match our given subpath. */
                if (fullPath.endsWith(desiredSubpathDecoded)) {
                    /* Single file which the user wants. */
                    singleDesiredFile = file;
                    desiredFileArchiveFileCount = filemap.get("filecount");
                } else if (fullPath.startsWith(desiredSubpathDecoded)) {
                    /* File below sub-path which user wants -> Collect all files in that subpath. */
                    desiredSubpathItems.add(file);
                }
            }
            if (desiredSubpathDecoded2 != null && fullPath.endsWith(desiredSubpathDecoded2)) {
                desiredFileArchiveFileCount = filemap.get("filecount");
                singleDesiredFile2 = file;
            }
            if (originalO instanceof String) {
                originalFilenamesDupeCollection.add(originalO.toString());
            }
            if (!userWantsItem && skipDeselectedItems) {
                skippedItemsFilepaths.add(pathWithFilename);
                continue;
            } else {
                selectedItems.add(file);
            }
        }
        if (desiredSubpathDecoded != null) {
            if (singleDesiredFile != null) {
                if (allowCrawlArchiveContents && desiredFileArchiveFileCount != null && Integer.parseInt(desiredFileArchiveFileCount.toString()) > 1) {
                    /* Single archive file which user wants but user wants to have the content of that archive (rare case). */
                    logger.info("Looks like user does not want to download single found file but instead wants to download all " + desiredFileArchiveFileCount + " files inside archive file: " + desiredSubpathDecoded);
                    return this.crawlArchiveContentV2(sourceurlForThisHandling, null, selectedItems);
                } else {
                    /* Single file which user wants */
                    selectedItems.clear(); // Clear list of previously collected results
                    final SingleFileAdoptFolderStructureMode mode = cfg.getSingleFileAdoptFolderStructureMode();
                    if (mode == SingleFileAdoptFolderStructureMode.DISABLE) {
                        singleDesiredFile.setRelativeDownloadFolderPath(null);
                    }
                    /* Ensure that this item is enabled as it could have been disabled due to the users' settings. */
                    singleDesiredFile.setEnabled(true);
                    selectedItems.add(singleDesiredFile); // Add desired result only
                    return selectedItems;
                }
            } else if (desiredSubpathDecoded2 != null && singleDesiredFile2 != null && desiredFileArchiveFileCount != null) {
                /* User wants file which is part of an archive. */
                logger.info("Looks like user wants to download single file inside archive " + desiredSubpathDecoded2 + " which contains " + desiredFileArchiveFileCount + " files");
                return this.crawlArchiveContentV2(sourceurlForThisHandling, desiredSubpathDecoded, singleDesiredFile2);
            } else if (desiredSubpathItems.size() > 0) {
                /* User desired item(s) are available -> Return only them */
                return desiredSubpathItems;
            }
            logger.info("Failed to find single file/path: " + desiredSubpathDecoded);
            final SingleFilePathNotFoundMode mode = cfg.getSingleFilePathNotFoundMode();
            if (mode == SingleFilePathNotFoundMode.ADD_NOTHING_AND_DISPLAY_ADDED_URL_AS_OFFLINE) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                logger.info("Failed to find single file/path -> Adding all results instead");
            }
        }
        /* Log skipped results */
        if (skippedItemsFilepaths.size() > 0) {
            logger.info("Skipped file items: " + skippedItemsFilepaths.size());
            logger.info(skippedItemsFilepaths.toString());
            if (selectedItems.isEmpty()) {
                logger.info("All file items were skipped due to users' plugin settings");
            }
        }
        final FilePackage playlistpackage = FilePackage.getInstance();
        final String metadataTitle = (String) root_metadata.get("title");
        if (!StringUtils.isEmpty(metadataTitle)) {
            playlistpackage.setName(metadataTitle + " - playlist");
        } else {
            /* Fallback */
            playlistpackage.setName(identifier + " - playlist");
        }
        playlistpackage.setPackageKey("internetarchive://identifier/" + identifier + "/playlist");
        /* Build video Stream playlist if needed */
        final ArrayList<DownloadLink> videoPlaylistItems = new ArrayList<DownloadLink>();
        if (StringUtils.equalsIgnoreCase(mediatype, "texts")) {
            /* Book crawl handling */
            final BookCrawlMode mode = cfg.getBookCrawlMode();
            /* Decide whether or not we need to crawl the loose book pages. */
            if (isAccessRestricted || mode == BookCrawlMode.LOOSE_PAGES || mode == BookCrawlMode.ORIGINAL_AND_LOOSE_PAGES) {
                /* Crawl book */
                /* Books can be split into multiple "parts" -> Collect them here */
                final HashSet<String> subPrefixes = new HashSet<String>();
                final String subPrefixInSourceURL = new Regex(sourceurlForThisHandling, "(?i)/details/[^/]+/([^/#\\?\\&]+)").getMatch(0);
                for (final String str : originalFilenamesDupeCollection) {
                    final Regex chapterregex = new Regex(str, "(.+)_(djvu\\.xml|page_numbers\\.json)");
                    if (!chapterregex.patternFind()) {
                        continue;
                    }
                    final String subPrefix = chapterregex.getMatch(0);
                    subPrefixes.add(subPrefix);
                }
                if (subPrefixes.isEmpty()) {
                    /* This should never happen */
                    logger.warning("FATAL: Book handling failed: Failed to find any subPrefixes -> Returning selected files as fallback");
                    return selectedItems;
                }
                if (mode == BookCrawlMode.ORIGINAL_AND_LOOSE_PAGES) {
                    /*
                     * Make selected files already appear in linkgrabber now already because crawling loose book pages can take a LOT of
                     * time.
                     */
                    distribute(selectedItems);
                }
                ensureInitHosterplugin();
                final Account account = AccountController.getInstance().getValidAccount(hostPlugin.getHost());
                if (account != null) {
                    /* Login if possible as this can have an influence on the books' "lending-status". */
                    hostPlugin.login(account, false);
                }
                logger.info("Crawling book with " + subPrefixes.size() + " subPrefixes | subPrefixInSourceURL = " + subPrefixInSourceURL);
                if (subPrefixInSourceURL != null && subPrefixes.contains(subPrefixInSourceURL)) {
                    /* User wants to crawl specific subPrefix only. */
                    subPrefixes.clear();
                    subPrefixes.add(subPrefixInSourceURL);
                }
                int position = 1;
                final ArrayList<DownloadLink> bookResults = new ArrayList<DownloadLink>();
                for (final String subPrefix : subPrefixes) {
                    logger.info("Crawling book prefix " + position + "/" + subPrefixes.size());
                    final UrlQuery query = new UrlQuery();
                    query.add("id", identifier);
                    query.add("itemPath", dir);
                    query.add("server", server);
                    query.add("format", "jsonp");
                    query.add("subPrefix", subPrefix);
                    query.add("requestUri", "/details/" + identifier);
                    final String url = "https://" + server + "/BookReader/BookReaderJSIA.php?" + query.toString();
                    final ArrayList<DownloadLink> thisBookResults = this.crawlBook(br, url, account);
                    /* Make resulting items appear in linkgrabber now already. */
                    distribute(thisBookResults);
                    if (this.isAbort()) {
                        logger.info("Stopping because: Aborted by user");
                        break;
                    } else {
                        position++;
                    }
                }
                /**
                 * Return loose pages only <br>
                 * If user also wants file items, they've already been returned via "distribute" method.
                 */
                return bookResults;
            } else {
                // BookCrawlMode.PREFER_ORIGINAL
                /* Fall through and return selected files */
            }
        } else if (StringUtils.equalsIgnoreCase(mediatype, "movies") && identifierDotMp4Exists) {
            /* Video "playlist" handling */
            if (totalLengthSecondsOriginalStr == null || !totalLengthSecondsOriginalStr.matches("\\d+(\\.\\d+)?")) {
                /* This should never happen */
                logger.warning("Detected mediatype 'movies' item but failed to determine video playtime");
                /* Fallback: return selected files */
                return selectedItems;
            }
            final int secondsPerSegment = 60;
            double totalLengthSeconds = Double.parseDouble(totalLengthSecondsOriginalStr);
            if (totalLengthSeconds == 0 && (totalLengthSecondsDerivativeStr != null && totalLengthSecondsDerivativeStr.matches("\\d+(\\.\\d+)?"))) {
                /**
                 * Edge case: Looks like archive.org original data may be corrupt sometimes, providing "0.00" as length value. </br>
                 * Example: https://archive.org/details/KNTV_20200205_073400_The_Tonight_Show_Starring_Jimmy_Fallon
                 */
                logger.info("Failed to find video length via info from original item -> Fallback to length from derivative -> " + totalLengthSecondsDerivativeStr);
                totalLengthSeconds = Double.parseDouble(totalLengthSecondsDerivativeStr);
            }
            final int numberofVideoSegments;
            final double remainingSeconds = totalLengthSeconds % secondsPerSegment;
            if (remainingSeconds == 0) {
                numberofVideoSegments = (int) (totalLengthSeconds / secondsPerSegment);
            } else {
                /* Uneven total runtime -> Last segment will be shorter than the others. */
                numberofVideoSegments = (int) ((totalLengthSeconds / secondsPerSegment) + 1);
            }
            if (totalLengthSeconds == 0) {
                /* This should never happen */
                logger.warning("Detected mediatype 'movies' item but determined playtime of 0 seconds is unplausible");
                /* Return original files */
                return selectedItems;
            }
            int offsetSeconds = 0;
            /*
             * Video can't be officially downloaded but it can be streamed in segments of X seconds each -> Generate those stream-links
             */
            for (int position = 0; position < numberofVideoSegments; position++) {
                final String directurl = "https://" + getHost() + "/download/" + identifier + "/" + identifier + ".mp4?t=" + offsetSeconds + "/" + (offsetSeconds + secondsPerSegment) + "&ignore=x.mp4";
                final DownloadLink video = this.createDownloadlink(directurl);
                video.setProperty(ArchiveOrg.PROPERTY_FILETYPE, ArchiveOrg.FILETYPE_VIDEO);
                video.setProperty(ArchiveOrg.PROPERTY_PLAYLIST_POSITION, position);
                video.setProperty(ArchiveOrg.PROPERTY_PLAYLIST_SIZE, numberofVideoSegments);
                ArchiveOrg.setFinalFilename(video, identifier + ".mp4");
                video.setAvailable(true);
                video._setFilePackage(playlistpackage);
                videoPlaylistItems.add(video);
                /* Increment counters */
                offsetSeconds += secondsPerSegment;
            }
            if (isAccessRestricted && playlistCrawlMode == PlaylistCrawlMode.AUTO) {
                /*
                 * Original file is not downloadable at all -> Force-return playlist items to provide downloadable items for the user.
                 */
                return videoPlaylistItems;
            } else if (isDownloadPage && playlistCrawlMode == PlaylistCrawlMode.AUTO) {
                logger.info("Skipping video playlist because user added '/download/' page in auto mode");
            } else if (playlistCrawlMode == PlaylistCrawlMode.PLAYLIST_ONLY) {
                /* User prefers to only get stream downloads slash "video playlist". */
                return videoPlaylistItems;
            } else if (playlistCrawlMode == PlaylistCrawlMode.PLAYLIST_AND_FILES || playlistCrawlMode == PlaylistCrawlMode.AUTO) {
                /* Return playlist items and original files */
                selectedItems.addAll(videoPlaylistItems);
            } else {
                /* Do not return any playlist items but only original files. */
                logger.info("Skipping video playlist because user wants original/selected files only");
            }
        } else if (audioPlaylistItems.size() > 0) {
            /* Audio playlist handling */
            // final boolean isAudioPlaylist = mediatype.equalsIgnoreCase("audio");
            /**
             * The code down below finds the original files that match the titles you can see in the playlist on the archive.org website.
             * This ensures that we get the best of both worlds: <br>
             * - Original file <br>
             * - Original file size <br>
             * - Same order as on website (see ArchiveOrg.PROPERTY_PLAYLIST_POSITION)
             */
            final int playlistSize = audioPlaylistItems.size();
            final ArrayList<DownloadLink> audioPlaylistFinalResults = new ArrayList<DownloadLink>();
            final ListIterator<String> li = audioPlaylistItems.listIterator();
            while (li.hasNext()) {
                final int itemPosition = li.nextIndex() + 1;
                final String itemName = li.next();
                final String mappingName = originalMapping.get(itemName);
                final String originalName;
                final String derivativeName;
                if (mappingName == null) {
                    originalName = itemName;
                    derivativeName = null;
                } else {
                    originalName = mappingName;
                    if (crawlOriginalFilesOnly) {
                        derivativeName = null;
                    } else {
                        derivativeName = itemName;
                    }
                }
                for (final String entryName : new String[] { originalName, derivativeName }) {
                    if (entryName == null) {
                        continue;
                    }
                    final DownloadLink item = itemMapping.get(entryName);
                    final DownloadLink audioPlaylistLink = this.createDownloadlink(item.getPluginPatternMatcher());
                    audioPlaylistLink.setProperties(item.getProperties());
                    /* Remove relative folder path set on original item otherwise playlist results won't go into our desired package! */
                    audioPlaylistLink.setRelativeDownloadFolderPath(null);
                    /*
                     * Add playlist specific properties so hosterplugin "knows" that this item is part of a playlist and can set filenames
                     * accordingly.
                     */
                    audioPlaylistLink.setProperty(ArchiveOrg.PROPERTY_PLAYLIST_POSITION, itemPosition);
                    audioPlaylistLink.setProperty(ArchiveOrg.PROPERTY_PLAYLIST_SIZE, playlistSize);
                    audioPlaylistLink.setProperty(ArchiveOrg.PROPERTY_FILETYPE, ArchiveOrg.FILETYPE_AUDIO);
                    audioPlaylistLink.setAvailable(true);
                    /* Put item in dedicated playlist package. */
                    audioPlaylistLink._setFilePackage(playlistpackage);
                    ArchiveOrg.setFinalFilename(audioPlaylistLink, item.getName());
                    /* Collect results */
                    audioPlaylistFinalResults.add(audioPlaylistLink);
                }
            }
            if (playlistCrawlMode == PlaylistCrawlMode.PLAYLIST_ONLY) {
                /* Return playlist items only */
                return audioPlaylistFinalResults;
            } else if (playlistCrawlMode == PlaylistCrawlMode.AUTO && isDownloadPage) {
                logger.info("Skipping audio playlist because user added '/download/' page in auto mode -> Return items visible on the website -> Paylist is only display for '/details/' links");
            } else if (playlistCrawlMode == PlaylistCrawlMode.AUTO || playlistCrawlMode == PlaylistCrawlMode.PLAYLIST_AND_FILES) {
                /* Return playlist and selected files. */
                selectedItems.addAll(audioPlaylistFinalResults);
                return selectedItems;
            } else {
                /* Do not return any playlist items but only selected files. */
                logger.info("Skipping audio playlist vecause user wants selected files only");
            }
        }
        if (desiredSubpathDecoded != null && desiredSubpathItems.isEmpty()) {
            logger.info("User wanted specific path/file but that hasn't been found -> Returning all globally allowed items instead");
        }
        return selectedItems;
    }

    private Map<String, Map<String, String>> parseFilterMap(final String url) {
        final Map<String, Map<String, String>> filter_map = new HashMap<String, Map<String, String>>();
        /* Some keys need to be renamed. */
        final Map<String, String> replacements = new HashMap<String, String>();
        replacements.put("lending", "lending___status");
        final String[] andValueStrings = new Regex(url, "and%5B%5D=([^&]+)").getColumn(0);
        if (andValueStrings != null && andValueStrings.length > 0) {
            /* Filter parameters selected by the user. On the website you can find them on the left side as checkboxes. */
            for (String andValueString : andValueStrings) {
                andValueString = URLEncode.decodeURIComponent(andValueString);
                if (!andValueString.contains(":")) {
                    /* Skip invalid items */
                    continue;
                }
                final String keyValue[] = new Regex(andValueString, "(.*?)\\s*:\\s*\"(.*?)\"").getRow(0);
                if (keyValue != null) {
                    String key = replacements.get(keyValue[0]);
                    if (key == null) {
                        key = keyValue[0];
                    }
                    Map<String, String> valueMap = filter_map.get(key);
                    if (valueMap == null) {
                        valueMap = new HashMap<String, String>();
                        filter_map.put(key, valueMap);
                    }
                    valueMap.put(keyValue[1], "inc");
                    continue;
                }
                final String range[] = new Regex(andValueString, "(.*?)\\s*:\\s*\\[\\s*(\\d+)(?:\\+\\s*)?TO\\s*(?:\\+\\s*)?(\\d+)\\s*\\]").getRow(0);
                if (range != null) {
                    String key = replacements.get(range[0]);
                    if (key == null) {
                        key = range[0];
                    }
                    Map<String, String> valueMap = filter_map.get(key);
                    if (valueMap == null) {
                        valueMap = new HashMap<String, String>();
                        filter_map.put(key, valueMap);
                    }
                    valueMap.put(range[1], "gte");
                    valueMap.put(range[2], "lte");
                    continue;
                }
            }
        }
        final String[] notValueStrings = new Regex(url, "not%5B%5D=([^&]+)").getColumn(0);
        if (notValueStrings != null && notValueStrings.length > 0) {
            /* Filter parameters selected by the user. On the website you can find them on the left side as checkboxes. */
            for (String notValueString : notValueStrings) {
                notValueString = URLEncode.decodeURIComponent(notValueString);
                if (!notValueString.contains(":")) {
                    /* Skip invalid items */
                    continue;
                }
                final String keyValue[] = new Regex(notValueString, "(.*?)\\s*:\\s*\"(.*?)\"").getRow(0);
                if (keyValue != null) {
                    String key = replacements.get(keyValue[0]);
                    if (key == null) {
                        key = keyValue[0];
                    }
                    Map<String, String> valueMap = filter_map.get(key);
                    if (valueMap == null) {
                        valueMap = new HashMap<String, String>();
                        filter_map.put(key, valueMap);
                    }
                    valueMap.put(keyValue[1], "exc");
                }
            }
        }
        return filter_map;
    }

    /** Returns all uploads of a profile. */
    private ArrayList<DownloadLink> crawlProfile(String username, final String sourceurl) throws Exception {
        if (StringUtils.isEmpty(username)) {
            throw new IllegalArgumentException();
        }
        if (!username.startsWith("@")) {
            /* Curate given parameter. */
            username += "@";
        }
        return crawlBetaSearchAPI(sourceurl, username, null);
    }

    /** Crawls desired book. Given browser instance needs to access URL to book in beforehand! */
    @Deprecated
    public ArrayList<DownloadLink> crawlBookWebsite(final Browser br, final CryptedLink param, final Account account) throws Exception {
        /* Crawl all pages of a book */
        final String bookAjaxURL = findBookReaderURLWebsite(br);
        if (bookAjaxURL == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return crawlBook(br, bookAjaxURL, account);
    }

    /** Returns true if given URL leads to content inside an archive. */
    private static boolean isCompressedArchiveURL(final String url) throws MalformedURLException {
        return url.toLowerCase(Locale.ENGLISH).contains("view_archive.php");
    }

    @Override
    protected boolean looksLikeDownloadableContent(final URLConnectionAdapter urlConnection) {
        return hostPlugin.looksLikeDownloadableContent(urlConnection);
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return ArchiveOrgConfig.class;
    }

    public static String generateBookContentURL(final String bookID) {
        return "https://archive.org/details/" + bookID;
    }
}