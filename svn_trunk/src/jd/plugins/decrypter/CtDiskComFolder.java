//  jDownloader - Downloadmanager
//  Copyright (C) 2012  JD-Team support@jdownloader.org
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
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
import jd.plugins.hoster.CtDiskCom;

@DecrypterPlugin(revision = "$Revision: 51848 $", interfaceVersion = 2, names = {}, urls = {})
public class CtDiskComFolder extends PluginForDecrypt {
    public static final String   PROPERTY_PARENT_DIR = "parent_dir";
    private static final Pattern PATTERN_FOLDER      = Pattern.compile("/(dir|d)/(\\d+)-(\\d+)(-([a-f0-9]+))?.*", Pattern.CASE_INSENSITIVE);

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "ctfile.com", "ctdisk.com", "400gb.com", "pipipan.com", "t00y.com", "bego.cc", "72k.us", "tc5.us", "545c.com", "sn9.us", "089u.com", "u062.com", "474b.com", "590m.com", "n802.com" });
        return ret;
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String[]> pluginDomains = getPluginDomains();
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_FOLDER.pattern());
        }
        return ret.toArray(new String[0]);
    }

    public CtDiskComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static String getPath(final String url) {
        String path = new Regex(url, PATTERN_FOLDER).getMatch(0);
        return path;
    }

    public static String getUserID(final String url) {
        String userid = new Regex(url, PATTERN_FOLDER).getMatch(1);
        return userid;
    }

    public static String getFolderID(final String url) {
        String folder_id = new Regex(url, PATTERN_FOLDER).getMatch(2);
        return folder_id;
    }

    public static String getFolderHash(final String url) {
        String fileid = new Regex(url, PATTERN_FOLDER).getMatch(4);
        return fileid;
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String folder_base_url = URLHelper.getUrlWithoutParams(contenturl);
        final String folder_path = getPath(contenturl);
        final String folder_user_id = getUserID(contenturl);
        final String folder_id = getFolderID(contenturl);
        final String folder_hash = getFolderHash(contenturl);
        final UrlQuery addedlink_query = UrlQuery.parse(contenturl);
        String folder_id_from_url = addedlink_query.get("d");
        if (folder_id_from_url == null) {
            folder_id_from_url = "undefined";
        }
        String d_str = folder_user_id + "-" + folder_id;
        if (folder_hash != null) {
            d_str += "-" + folder_hash;
        }
        String fk = addedlink_query.get("fk");
        if (fk == null) {
            fk = "";
        }
        /* Root-ID of a folder */
        prepAjax(this.br);
        final UrlQuery query = new UrlQuery();
        query.add("path", folder_path);
        query.add("d", d_str);
        query.add("folder_id", folder_id_from_url);
        query.appendEncoded("fk", fk);
        query.add("token", "0");
        query.add("ref", "");
        query.appendEncoded("url", contenturl);
        br.getHeaders().put("Origin", "https://" + Browser.getHost(contenturl));
        br.getHeaders().put("Referer", contenturl);
        String passCode = param.getDecrypterPassword();
        Map<String, Object> entries = null;
        int passwordCounter = 0;
        int code = -1;
        password_loop: do {
            passwordCounter += 1;
            query.addAndReplace("passcode", passCode != null ? Encoding.urlEncode(passCode) : "");
            query.addAndReplace("r", "0." + System.currentTimeMillis());
            br.getPage(CtDiskCom.WEBAPI_BASE + "/getdir.php?" + query.toString());
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            code = ((Number) entries.get("code")).intValue();
            if (code != 401 && code != 423) {
                /* No password required or correct password has been entered */
                break password_loop;
            }
            if (passwordCounter > 3) {
                throw new DecrypterException(DecrypterException.PASSWORD);
            }
            /* Try again */
            logger.info("Wrong password or password required");
            passCode = getUserInput("Password?", param);
            continue password_loop;
        } while (true);
        final String message = (String) entries.get("message");
        if (code == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
        } else if (code == 504) {
            /* The share does not exist or has expired. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
        } else if (code != 200) {
            /* Assume that folder is offline */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, message);
        }
        final int maxItemsPerPageDefault = 100;
        final Map<String, Object> filemap = (Map<String, Object>) entries.get("file");
        final String folder_api_url = br.getURL(filemap.get("url").toString()).toExternalForm();
        final String folder_api_url_without_params = URLHelper.getUrlWithoutParams(folder_api_url);
        final UrlQuery folder_api_query = UrlQuery.parse(folder_api_url);
        if (!folder_api_query.containsKey("iDisplayLength") || !folder_api_query.containsKey("iDisplayStart")) {
            folder_api_query.add("iDisplayLength", Integer.toString(maxItemsPerPageDefault));
            folder_api_query.add("iDisplayStart", "0");
        }
        /**
         * 2025-11-19: By default, folder_api_url does not contain any limiting parameters thus we would get all items with one request.
         * <br>
         * Website is limiting results to max 10 items per page -> We use 100.
         */
        int maxItemsPerPage = -1;
        final String maxItemsPerPageStr = folder_api_query.get("iDisplayLength");
        if (maxItemsPerPageStr != null) {
            maxItemsPerPage = Integer.parseInt(maxItemsPerPageStr);
        }
        int page = 1;
        int index = 0;
        String subfolderpath = this.getAdoptedCloudFolderStructure();
        if (subfolderpath == null) {
            subfolderpath = (String) filemap.get("folder_name");
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(subfolderpath);
        final HashSet<String> dupes = new HashSet<String>();
        pagination: do {
            br.getPage(folder_api_url_without_params + "?" + folder_api_query.toString());
            final Map<String, Object> folderoverview = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if (((Number) folderoverview.get("iTotalRecords")).intValue() == 0) {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, d_str + "_" + subfolderpath);
            }
            int numberofNewItemsThisPage = 0;
            /* This is where the crappy part starts: json containing string-arrays with HTML code... */
            final List<List<Object>> items = (List<List<Object>>) folderoverview.get("aaData");
            for (final List<Object> item : items) {
                // final String info0 = item.get(0).toString();
                final String html0 = item.get(0).toString();
                final String html1 = item.get(1).toString();
                final Regex folderRegex1 = new Regex(html1, "onclick=\"load_subdir\\((\\d+)\\)\">([^<]+)</a>");
                String subfolderID = new Regex(html0, "value=\"d(\\d+)\"").getMatch(0);
                String subfolderHash = null;
                String subfolderTitle = null;
                if (folderRegex1.patternFind()) {
                    subfolderID = folderRegex1.getMatch(0);
                    subfolderTitle = folderRegex1.getMatch(1);
                } else {
                    final Regex folderRegex2 = new Regex(html1, "load_subdir\\((\\d+), '([a-f0-9]+)'\\)\">([^<]+)</a>");
                    subfolderID = folderRegex2.getMatch(0);
                    subfolderHash = folderRegex2.getMatch(1);
                    subfolderTitle = folderRegex2.getMatch(2);
                }
                if (subfolderID != null) {
                    /* Subfolder */
                    if (!dupes.add(subfolderID)) {
                        /* Skip dupes */
                        continue;
                    }
                    String url = folder_base_url + "?d=" + subfolderID;
                    if (subfolderHash != null) {
                        url += "&fk=" + subfolderHash;
                    }
                    final DownloadLink folder = this.createDownloadlink(url);
                    folder.setDownloadPassword(passCode);
                    if (subfolderTitle != null) {
                        subfolderTitle = Encoding.htmlDecode(subfolderTitle).trim();
                        folder.setRelativeDownloadFolderPath(subfolderpath + "/" + subfolderTitle);
                    } else {
                        logger.warning("Failed to find subfolder title for subfolder with folder_id " + subfolderID);
                        folder.setRelativeDownloadFolderPath(subfolderpath + "/" + subfolderID);
                    }
                    ret.add(folder);
                } else {
                    /* Single file */
                    final String filesize = item.get(2).toString();
                    final String file_id = new Regex(html0, "value=\"f(\\d+)\"").getMatch(0);
                    if (file_id == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    } else if (StringUtils.isEmpty(filesize)) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    if (!dupes.add(file_id)) {
                        /* Skip dupes */
                        continue;
                    }
                    /* Build url */
                    final String url = "https://" + br.getHost(true) + "/fs/" + folder_user_id + "-" + file_id;
                    final String filename = new Regex(html1, ">([^<]+)</a>").getMatch(0);
                    final DownloadLink link = this.createDownloadlink(url);
                    /* Set info only for fileURLs which then go into the hosterplugin! */
                    if (filename != null) {
                        link.setName(Encoding.htmlDecode(filename).trim());
                    } else {
                        logger.warning("Failed to find filename for item with file_id " + file_id);
                    }
                    link.setDownloadSize(SizeFormatter.getSize(filesize));
                    link.setAvailable(true);
                    link.setRelativeDownloadFolderPath(subfolderpath);
                    link.setProperty(PROPERTY_PARENT_DIR, param.getCryptedUrl());
                    link._setFilePackage(fp);
                    ret.add(link);
                }
                numberofNewItemsThisPage++;
            }
            logger.info("Crawled page " + page + " | Index: " + index + " | New items this page: " + numberofNewItemsThisPage + " | Found items: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                throw new InterruptedException();
            } else if (maxItemsPerPage == -1 || items.size() < maxItemsPerPage) {
                logger.info("Stopping because: Reached end");
                break pagination;
            } else if (numberofNewItemsThisPage == 0) {
                /* Fail-safe */
                logger.info("Stopping because: Found no new items on current page");
                break pagination;
            }
            /* Continue to next page */
            index += items.size();
            page++;
            folder_api_query.addAndReplace("iDisplayStart", Integer.toString(index));
        } while (true);
        return ret;
    }

    private Browser prepAjax(final Browser prepBr) {
        prepBr.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
        prepBr.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        prepBr.getHeaders().put("Accept-Charset", null);
        return prepBr;
    }

    @Override
    public boolean hasCaptcha(final CryptedLink link, final jd.plugins.Account acc) {
        return false;
    }
}