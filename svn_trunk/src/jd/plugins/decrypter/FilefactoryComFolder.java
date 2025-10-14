//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.net.URLHelper;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
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
import jd.plugins.hoster.FileFactory;

@DecrypterPlugin(revision = "$Revision: 51663 $", interfaceVersion = 2, names = {}, urls = {})
@PluginDependencies(dependencies = { FileFactory.class })
public class FilefactoryComFolder extends PluginForDecrypt {
    public FilefactoryComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        // blocking default UA
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, Request.getSuggestedUserAgent("142.0"));
        br.setCookie(getHost(), "filefactory_relaunch", "seen");
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return jd.plugins.hoster.FileFactory.getPluginDomains();
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

    private static Pattern PATTERN_SHARE_LINK_WITH_MULTIPLE_FUIDS = Pattern.compile("/share/(fi:[a-z0-9,:]+)", Pattern.CASE_INSENSITIVE);
    private static Pattern PATTERN_FOLDER_NORMAL                  = Pattern.compile("/folder/([a-f0-9]{16})", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_SHARE_LINK_WITH_MULTIPLE_FUIDS.pattern() + "|" + PATTERN_FOLDER_NORMAL.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        String file_ids_text = new Regex(contenturl, PATTERN_SHARE_LINK_WITH_MULTIPLE_FUIDS).getMatch(0);
        if (file_ids_text != null) {
            /* Link which contains all fileIDs inside URL -> No http request required to process this. */
            file_ids_text = file_ids_text.replace("fi:", "");
            final String[] fileIDs = file_ids_text.split(",");
            final FilePackage fp = FilePackage.getInstance();
            for (final String fileid : fileIDs) {
                final String url = generateSingleFileLink(fileid, null);
                final DownloadLink link = this.createDownloadlink(url);
                link._setFilePackage(fp);
                ret.add(link);
            }
        } else {
            /* Folder link where single file links need to be crawled via html code */
            final String folder_id = new Regex(contenturl, PATTERN_FOLDER_NORMAL).getMatch(0);
            br.getPage(contenturl);
            /* Error handling */
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            old_website: {
                if (!br.getURL().contains(folder_id)) {
                    /* old website: e.g. redirect to error page such as: https://www.filefactory.com/error.php?code=300 */
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else if (br.containsHTML(">\\s*There are no files in this folder")) {
                    /* Empty folder */
                    throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
                }
                /* Folder title is not available in CSV file thus to get it, we need to look into the html code of the older URL. */
                String folderTitle = br.getRegex("<h1>Files in\\s*<span>([^<]+)</span>").getMatch(0);
                final FilePackage fp = FilePackage.getInstance();
                if (!StringUtils.isEmpty(folderTitle)) {
                    folderTitle = Encoding.htmlDecode(folderTitle).trim();
                    fp.setName(folderTitle);
                } else {
                    logger.warning("Failed to find folder title");
                    fp.setName(folder_id);
                }
                fp.setPackageKey(this.getHost() + "://folder/" + folder_id);
                parse_folder_csv_export: {
                    /* Try to extract folder information from CSV since this is the better source of data. */
                    final String folderExportLink = br.getRegex("(/folder/" + folder_id + "/\\?export=1)").getMatch(0);
                    if (folderExportLink == null) {
                        logger.warning("CSV crawler: Cannot parse folder CSV export because: Failed to find folder export link");
                        break parse_folder_csv_export;
                    }
                    try {
                        final Browser brc = br.cloneBrowser();
                        /* Allow unlimited size of CSV files. */
                        brc.setLoadLimit(Integer.MAX_VALUE);
                        brc.getPage(folderExportLink);
                        final String exportName = getFileNameFromConnection(brc.getHttpConnection());
                        if (!StringUtils.endsWithCaseInsensitive(exportName, ".csv") || brc.getHttpConnection().getResponseCode() != 200) {
                            logger.warning("CSV crawler: csv export failed");
                            break parse_folder_csv_export;
                        }
                        final String csvContent = brc.getRequest().getHtmlCode();
                        // Split by line breaks
                        final String[] lines = csvContent.split("\\r?\\n");
                        /* Validate CSV integrity by checking for the expected CSV headers. */
                        final List<String> header = Arrays.asList(parseCsvLine(lines[0]));
                        final int filenameIndex = header.indexOf("filename");
                        final int sizeIndex = header.indexOf("size");
                        final int urlIndex = header.indexOf("url");
                        if (filenameIndex == -1 || sizeIndex == -1 || urlIndex == -1) {
                            logger.warning("CSV crawler: cannot find all required columns in csv");
                            break parse_folder_csv_export;
                        }
                        for (int i = 1; i < lines.length; i++) { // Start at 1 to skip CSV header
                            final String line = lines[i];
                            final String[] data = parseCsvLine(line);
                            final String filename = data[filenameIndex];
                            final String sizeBytesStr = data[sizeIndex];
                            // final String createdDate = data[2];
                            final String url = data[urlIndex];
                            final DownloadLink link = this.createDownloadlink(url);
                            link.setFinalFileName(filename);
                            link.setVerifiedFileSize(Long.parseLong(sizeBytesStr));
                            link._setFilePackage(fp);
                            link.setAvailable(true);
                            ret.add(link);
                        }
                        logger.info("CSV crawler: Success!");
                        return ret;
                    } catch (Exception e) {
                        logger.log(e);
                        logger.info("CSV crawler: Failed!");
                        break parse_folder_csv_export;
                    }
                }
                /* Base URL needed for pagination. */
                String folder_url_without_params = URLHelper.getUrlWithoutParams(br.getURL());
                if (!folder_url_without_params.endsWith("/")) {
                    folder_url_without_params += "/";
                }
                int pageMax = 1;
                final HashSet<String> dupes = new HashSet<String>();
                pagination: for (int page = 1; page <= pageMax; page++) {
                    int numberofNewItemsThisPage = 0;
                    final String file_ids[] = br.getRegex("/file/([a-z0-9]+)").getColumn(0);
                    for (String file_id : file_ids) {
                        if (!dupes.add(file_id)) {
                            /* Skip duplicates */
                            continue;
                        }
                        String filename = null;
                        String filesizeStr = null;
                        final String html = new Regex(br.getRequest().getHtmlCode(), "<tr id=\"row_" + file_id + "\">.*?</td>\\s*</tr>").getMatch(-1);
                        if (html != null) {
                            filename = new Regex(html, "/file/" + file_id + "/([^\"/]+)").getMatch(0);
                            if (filename != null) {
                                filename = Encoding.htmlDecode(filename).trim();
                            }
                            /* 2025-10-09: File size unit starts from "KB", there is no "bytes". */
                            filesizeStr = new Regex(html, "(?i)Size:\\s*([\\d\\.]+\\s*(KB|MB|GB|TB))").getMatch(0);
                        } else {
                            logger.warning("Failed to find file information for file_id: " + file_id);
                        }
                        final String url = this.generateSingleFileLink(file_id, filename);
                        final DownloadLink link = createDownloadlink(url);
                        if (filename != null) {
                            link.setName(filename);
                        }
                        if (filesizeStr != null) {
                            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                        }
                        link._setFilePackage(fp);
                        /* We know that this item is online */
                        link.setAvailable(true);
                        ret.add(link);
                        distribute(link);
                        numberofNewItemsThisPage++;
                    }
                    /*
                     * Re-evaluate max page each loop since page size could change. Usually this happens only from first to 2nd page: 1st
                     * page =max 25 items (server side default), 2nd page until end: max 100 items per page.
                     */
                    final String pageMaxStr = br.getRegex("data\\-paginator\\-totalPages=\"(\\d+)\"").getMatch(0);
                    if (pageMaxStr != null) {
                        pageMax = Integer.parseInt(pageMaxStr);
                    }
                    logger.info("Crawled page " + page + "/" + pageMax + " | New items on this page: " + numberofNewItemsThisPage + " | Items found so far: " + ret.size());
                    if (page == pageMax) {
                        logger.info("Stopping because: Crawled last page");
                        break pagination;
                    } else if (numberofNewItemsThisPage == 0) {
                        /* Fail-safe */
                        logger.info("Stopping because: Failed to find any new items on current page");
                        break pagination;
                    }
                    /* Continue to next page */
                    br.getPage(folder_url_without_params + "?sort=filename&order=ASC&show=100&page=" + (page + 1));
                }
                if (ret.size() > 0) {
                    logger.info("Finished crawling via old_website handling");
                    return ret;
                }
            }
            if (br.containsHTML("errorData\\\\?\"")) {
                /*
                 * e.g. {\"errorData\":{\"title\":\"Invalid Folder Link\",\"message\":\"The requested folder cannot be displayed because the
                 * link is invalid.\" }
                 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            int numberOfFiles = -1;
            int maxItemsPerPage = -1;
            String title = null;
            FilePackage fp = null;
            int maxPage = 1;
            int page = 1;
            // TODO: Add support for password protected folders
            /*
             * 2025-09-10: Website json implies that password protected links exist but I was unable to create- or find such test-links.
             */
            boolean requiresPassword = false;
            final HashSet<String> dupes = new HashSet<String>();
            pagination: while (!this.isAbort()) {
                // final String[] filenames = br.getRegex("\"disp_filename\":\"([^\"]+)\"").getColumn(0);
                final String[] jsons = br.getRegex("<script>self\\.__next_f\\.push\\((.*?)\\)</script>").getColumn(0);
                Map<String, Object> entries = null;
                for (String json : jsons) {
                    json = new Regex(json, "\\[\\s*\\d+\\s*,\\s*\"(.*?)\"\\s*\\]$").getMatch(0);
                    if (json == null || !json.matches(".*\\{\\s*\\\\\"folder.*")) {
                        continue;
                    }
                    json = new Regex((String) JavaScriptEngineFactory.jsonToJavaObject("\"" + json + "\""), "\\d+\\s*:\\s*(.+)").getMatch(0);
                    final List<Object> object = (List<Object>) JavaScriptEngineFactory.jsonToJavaObject(json);
                    if (object == null) {
                        continue;
                    }
                    entries = (Map<String, Object>) object.get(3);
                    break;
                }
                if (entries == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find json source");
                }
                int numberofNewItemsThisPage = 0;
                final Map<String, Object> folder = (Map<String, Object>) entries.get("folder");
                final Map<String, Object> pagination = (Map<String, Object>) entries.get("pagination");
                if (page == 1) {
                    /* Init some vars */
                    numberOfFiles = ((Number) entries.get("totalCount")).intValue();
                    maxPage = ((Number) pagination.get("pageCount")).intValue();
                    maxItemsPerPage = ((Number) pagination.get("itemsPerPage")).intValue();
                    if (numberOfFiles == 0) {
                        throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
                    }
                    title = (String) folder.get("name");
                    fp = FilePackage.getInstance();
                    if (title != null) {
                        fp.setName(title);
                    } else {
                        /* Fallback */
                        fp.setName(folder_id);
                    }
                    fp.setPackageKey(this.getHost() + "://folder/" + folder_id);
                    requiresPassword = ((Boolean) entries.get("requiresPassword")).booleanValue();
                    if (requiresPassword) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Password protected folders are not yet supported!");
                    }
                }
                final List<Map<String, Object>> files = (List<Map<String, Object>>) entries.get("files");
                for (final Map<String, Object> file : files) {
                    final String file_id = file.get("viewhash").toString();
                    if (!dupes.add(file_id)) {
                        continue;
                    }
                    numberofNewItemsThisPage++;
                    final String filename = file.get("disp_filename").toString();
                    final String url = generateSingleFileLink(file_id, filename);
                    final DownloadLink link = this.createDownloadlink(url);
                    link.setFinalFileName(filename);
                    link.setVerifiedFileSize(((Number) file.get("size")).longValue());
                    link.setAvailable(true);
                    link._setFilePackage(fp);
                    ret.add(link);
                    distribute(link);
                }
                logger.info("Crawled page " + page + "/" + maxPage + " | New this page: " + numberofNewItemsThisPage + "/" + maxItemsPerPage + " | Found items so far: " + ret.size());
                if (page >= maxPage) {
                    logger.info("Stopping because: Reached last page");
                    break pagination;
                } else if (ret.size() >= numberOfFiles) {
                    logger.info("Stopping because: Found all items");
                    break pagination;
                } else if (numberofNewItemsThisPage == 0) {
                    /* Additional fail safe to prevent infinite loops */
                    logger.info("Stopping because: Failed to find new items on current page");
                    break pagination;
                } else {
                    /* Continue to next page */
                    page++;
                    br.getPage("?page=" + page);
                }
            }
        }
        return ret;
    }

    /**
     * Parses a CSV line correctly - handles quotes, commas, and escaped quotes within quoted values Java 1.6 compatible version
     *
     * CSV escape rules: - Fields with commas or quotes are wrapped in quotes: "value" - Quotes inside quoted fields are escaped by doubling
     * them: ""
     *
     * Example: "File with ""quotes"" in name.zip" represents: File with "quotes" in name.zip
     */
    private static String[] parseCsvLine(String line) {
        List<String> result = new ArrayList<String>();
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (c == '"') {
                // Check if this is an escaped quote ("")
                if (inQuotes && i + 1 < line.length() && line.charAt(i + 1) == '"') {
                    // Escaped quote: add one quote to output and skip the next one
                    current.append('"');
                    i++; // Skip next quote
                } else {
                    // Toggle quote mode (start or end of quoted field)
                    inQuotes = !inQuotes;
                }
            } else if (c == ',' && !inQuotes) {
                // Comma outside quotes = new field
                result.add(current.toString());
                current = new StringBuilder();
            } else {
                // Regular character
                current.append(c);
            }
        }
        // Add last field
        result.add(current.toString());
        return result.toArray(new String[result.size()]);
    }

    private String generateSingleFileLink(final String file_id, final String filename) {
        String url = "https://www." + this.getHost() + "/file/" + file_id;
        if (filename != null) {
            url += "/" + Encoding.urlEncode(filename);
        }
        return url;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}