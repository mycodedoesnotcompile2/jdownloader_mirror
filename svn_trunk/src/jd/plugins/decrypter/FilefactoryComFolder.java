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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
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

import org.appwork.net.protocol.http.HTTPConstants;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@DecrypterPlugin(revision = "$Revision: 51541 $", interfaceVersion = 2, names = {}, urls = {})
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
                final String url = generateSingleFileLink(fileid);
                final DownloadLink link = this.createDownloadlink(url);
                link._setFilePackage(fp);
                ret.add(link);
            }
        } else {
            final String folder_id = new Regex(contenturl, PATTERN_FOLDER_NORMAL).getMatch(0);
            br.getPage(contenturl);
            /* Error handling */
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML("errorData\\\\?\"")) {
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
                    final String url = generateSingleFileLink(file_id);
                    final DownloadLink link = this.createDownloadlink(url);
                    link.setFinalFileName(file.get("disp_filename").toString());
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

    private String generateSingleFileLink(final String file_id) {
        return "https://www." + this.getHost() + "/file/" + file_id;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}