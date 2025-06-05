//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
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

import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.http.requests.GetRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.hoster.ChoMikujPl;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 51114 $", interfaceVersion = 2, names = { "chomikuj.pl" }, urls = { "https?://((?:www\\.)?chomikuj\\.pl//?[^<>\"]+|chomikujpagedecrypt\\.pl/result/.+)" })
public class ChoMikujPlFolder extends PluginForDecrypt {
    public ChoMikujPlFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setLoadLimit(4194304);
        return br;
    }

    private String                            FOLDERPASSWORD                  = null;
    private String                            FOLDERPASSWORD_SPECIAL          = null;
    protected static HashMap<String, Cookies> recentCookies                   = new HashMap<String, Cookies>();
    private final String                      PAGEDECRYPTLINK                 = "https?://chomikujpagedecrypt\\.pl/.+";
    private final String                      VIDEO_DIRECTURL                 = "https?://[^/]+//?video\\.ashx.+";
    private final String                      ENDINGS                         = "(?i)\\.(3gp|7zip|7z|abr|ac3|aiff|aifc|aif|ai|au|avi|bin|bat|bz2|cbr|cbz|ccf|chm|cso|cue|cvd|dta|deb|divx|djvu|dlc|dmg|doc|docx|dot|eps|epub|exe|ff|flv|flac|f4v|gsd|gif|gz|iwd|idx|iso|ipa|ipsw|java|jar|jpg|jpeg|load|m2ts|mws|mv|m4v|m4a|mkv|mp2|mp3|mp4|mobi|mov|movie|mpeg|mpe|mpg|mpq|msi|msu|msp|nfo|npk|oga|ogg|ogv|otrkey|par2|pkg|png|pdf|pptx|ppt|pps|ppz|pot|psd|qt|rmvb|rm|rar|ram|ra|rev|rnd|[r-z]\\d{2}|r\\d+|rpm|run|rsdf|reg|rtf|shnf|sh(?!tml)|ssa|smi|sub|srt|snd|sfv|swf|tar\\.gz|tar\\.bz2|tar\\.xz|tar|tgz|tiff|tif|ts|txt|url|viv|vivo|vob|webm|wav|wmv|wma|wpl|xla|xls|xpi|zeno|zip)";
    private final String                      UNSUPPORTED                     = "(?i)https?://(?:www\\.)?chomikuj\\.pl//?(action/[^<>\"]+|(Media|Kontakt|PolitykaPrywatnosci|Empty|Abuse|Sugestia|LostPassword|Zmiany|Regulamin|Platforma)\\.aspx|favicon\\.ico|konkurs_literacki/info)";
    private static Object                     LOCK                            = new Object();
    public static final String                CFG_FOLDERPASSWORD              = "password";
    public static final String                PROPERTY_FOLDERPASSWORD_SPECIAL = "password_special";
    public static final String                PROPERTY_DOWNLOADLINK_MAINLINK  = "mainlink";

    /**
     * 2021-11-16: Attempt to save requests and handle password protected folders faster so password input is e.g. only required the first
     * time the root of a folder is added.
     */
    protected void loadCookies(final Browser prepBr, final String host) {
        synchronized (recentCookies) {
            for (final Map.Entry<String, Cookies> cookieEntry : recentCookies.entrySet()) {
                final String key = cookieEntry.getKey();
                if (key != null && key.equals(host)) {
                    try {
                        prepBr.setCookies(key, cookieEntry.getValue(), false);
                    } catch (final Throwable e) {
                    }
                }
            }
        }
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 4;
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        loadHosterPlugin();
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (new Regex(param.getCryptedUrl(), Pattern.compile(VIDEO_DIRECTURL, Pattern.CASE_INSENSITIVE)).patternFind()) {
            /* 2019-07-16: Very rare case e.g. https://svn.jdownloader.org/issues/81525 */
            ret.add(this.createDownloadlink(DirectHTTP.createURLForThisPlugin(param.getCryptedUrl())));
            return ret;
        }
        int startPage = 1;
        boolean scanForMorePages = false;
        String parameter_without_page_number = null;
        String contenturl;
        if (param.getCryptedUrl().matches(PAGEDECRYPTLINK)) {
            final String base = new Regex(param.getCryptedUrl(), "\\.pl/result/([^\\?]+)").getMatch(0);
            contenturl = Encoding.Base64Decode(base);
            if (param.getCryptedUrl().contains("?check_for_more=true")) {
                scanForMorePages = true;
                startPage = Integer.parseInt(new Regex(contenturl, ",(\\d+)$").getMatch(0));
                parameter_without_page_number = contenturl.substring(0, contenturl.lastIndexOf(","));
            }
        } else {
            contenturl = param.getCryptedUrl().replace("chomikuj.pl//", "chomikuj.pl/");
            if (contenturl.contains(",")) {
                scanForMorePages = false;
                parameter_without_page_number = contenturl.substring(0, contenturl.lastIndexOf(","));
            } else {
                scanForMorePages = true;
                parameter_without_page_number = contenturl;
            }
        }
        if (contenturl.matches(UNSUPPORTED)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        boolean crawlFolders = plugin.getPluginConfig().getBooleanProperty(ChoMikujPl.CRAWL_SUBFOLDERS, ChoMikujPl.default_CRAWL_SUBFOLDERS);
        String linkending = null;
        if (contenturl.contains(",")) {
            linkending = contenturl.substring(contenturl.lastIndexOf(","));
        }
        if (linkending == null) {
            linkending = contenturl.substring(contenturl.lastIndexOf("/") + 1);
        }
        /* Correct added link */
        contenturl = contenturl.replace("www.", "").replace("http://", "https://");
        /********************** Load recent cookies ************************/
        this.loadCookies(br, this.getHost());
        /********************** Login if possible ************************/
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account != null) {
            this.plugin.login(account, false);
        }
        /********************** Multiple pages handling START ************************/
        getPage(contenturl);
        String requestVerificationToken = null;
        boolean performedSpecialWorkaround = false;
        String estimatedCurrentFolderPath = null;
        String username = null;
        specialWorkaround: if (br.getHttpConnection().getResponseCode() == 400 && DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /**
             * Workaround for a website bug where when trying to open a folder structuree via normal GET request fails although the folder
             * is online. <br>
             * Via browser it is possible to navigate to the final folder by starting from the users' root folder and then performing one
             * ajax request for each subfolder until the final file/folder is reached.
             */
            /* https://board.jdownloader.org/showthread.php?t=97447 */
            final String[] urlSegments = br._getURL().getPath().split("/");
            if (urlSegments.length < 2) {
                /* Workaround not possible */
                logger.info("Special workaround not possible: URL does not have enough segments");
                break specialWorkaround;
            }
            username = urlSegments[1];
            this.getPage("/" + username);
            /* TODO: Check and make this handling work for password protected folders */
            if (br.getHttpConnection().getResponseCode() != 200) {
                logger.info("Special workaround not possible: responsecode != 200");
                break specialWorkaround;
            }
            requestVerificationToken = findRequestVerificationToken(br);
            if (requestVerificationToken == null) {
                logger.info("Special workaround not possible: failed to find requestVerificationToken");
                break specialWorkaround;
            }
            if (this.isAbort()) {
                throw new InterruptedException();
            }
            final String targetFolderID = param.getDownloadLink() != null ? param.getDownloadLink().getStringProperty("folder_id") : null;
            if (targetFolderID != null) {
                /* The fast & easy way: We can navigate to target folder immediately */
                logger.info("Navigating right away to targetFolder with ID: " + targetFolderID);
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("chomikName", username);
                query.appendEncoded("folderId", targetFolderID);
                query.appendEncoded("fileListSortType", "Date");
                query.appendEncoded("fileListAscending", "False");
                query.appendEncoded("gallerySortType", "Name");
                query.appendEncoded("galleryAscending", "False");
                query.appendEncoded("isGallery", "False");
                query.appendEncoded("folderChanged", "true");
                query.appendEncoded("__RequestVerificationToken", requestVerificationToken);
                br.postPage("/action/Files/FilesList", query);
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
            } else {
                /* The hard way: Navigate to target folder through each subfolder */
                String currentVirtualPath = username;
                /* Important! Start from 2! [0] = empty string, [1] = username */
                final HashSet<String> processed_folder_ids = new HashSet<String>();
                for (int i = 2; i < urlSegments.length; i++) {
                    final String segment = urlSegments[i];
                    final boolean isLastSegment = i == urlSegments.length - 1;
                    currentVirtualPath += "/" + segment;
                    final String folderID = br.getRegex(Pattern.quote(currentVirtualPath) + "\" rel=\"(\\d+)").getMatch(0);
                    if (folderID == null) {
                        if (isLastSegment && new Regex(segment, ".*,\\d+.*").patternFind()) {
                            logger.info("Failed to find folder_id for last segment -> Assuming that we are looking for a single file");
                            /* We expect to have only files or only a single file -> Do not allow subfolder crawling down below. */
                            crawlFolders = false;
                            break;
                        }
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find folderID for path " + currentVirtualPath);
                    } else if (processed_folder_ids.contains(folderID)) {
                        /* Fail safe to prevent endless crawling activity */
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Found duplicated folderID");
                    }
                    logger.info("Ajax navigation to folderID: " + folderID + " | " + currentVirtualPath);
                    final UrlQuery query = new UrlQuery();
                    query.appendEncoded("chomikName", username);
                    query.appendEncoded("folderId", folderID);
                    query.appendEncoded("fileListSortType", "Date");
                    query.appendEncoded("fileListAscending", "False");
                    query.appendEncoded("gallerySortType", "Name");
                    query.appendEncoded("galleryAscending", "False");
                    query.appendEncoded("isGallery", "False");
                    query.appendEncoded("folderChanged", "true");
                    query.appendEncoded("__RequestVerificationToken", requestVerificationToken);
                    br.postPage("/action/Files/FilesList", query);
                    if (this.isAbort()) {
                        throw new InterruptedException();
                    }
                    processed_folder_ids.add(folderID);
                    estimatedCurrentFolderPath = currentVirtualPath;
                }
            }
            logger.info("Special workaround looks to be successful");
            performedSpecialWorkaround = true;
        }
        if (requestVerificationToken == null) {
            requestVerificationToken = findRequestVerificationToken(br);
        }
        passwordHandling(param);
        if (scanForMorePages) {
            /* Find all pages and re-add them to the crawler to handle one page after another! */
            if (br.containsHTML("fileListPage")) {
                int maxPage = 1;
                int pageTemp = 1;
                final String[] pages = br.getRegex("class=\"\" rel=\"(\\d+)\" ").getColumn(0);
                for (final String pageStr : pages) {
                    pageTemp = Integer.parseInt(pageStr);
                    if (pageTemp > maxPage) {
                        maxPage = pageTemp;
                    }
                }
                /* Add separate links for all found pages so call can be crawled independently from each other! */
                if (maxPage > startPage) {
                    logger.info("Found " + maxPage + " pages");
                    final FilePackage fp = FilePackage.getInstance();
                    for (int pageCounter = startPage + 1; pageCounter <= maxPage; pageCounter++) {
                        String crawlerURL = "https://chomikujpagedecrypt.pl/result/" + Encoding.Base64Encode(parameter_without_page_number + "," + pageCounter);
                        /*
                         * 2019-07-26: Folders with over 9 pages will not contain the final maxPage value inside html. Users will have to
                         * access page 9 to see more pages (or even to see the last page).
                         */
                        if (pageCounter == maxPage && br.containsHTML("rel=\"" + pageCounter + "\" title=\"" + pageCounter + " \\.\\.\\.\"")) {
                            crawlerURL += "?check_for_more=true";
                        }
                        final DownloadLink dl = createDownloadlink(crawlerURL);
                        fp.add(dl);
                        distribute(dl);
                        ret.add(dl);
                    }
                }
            } else {
                logger.info("Looks like we got a single page with files");
            }
        }
        /********************** Multiple pages handling END ************************/
        /* Checking if the single link is folder with EXTENSTION in the name */
        /* Check if we have a single file or a folder */
        final DownloadLink singleFile = this.crawlSingleFile(this.br);
        if (singleFile != null) {
            ret.add(singleFile);
            return ret;
        }
        logger.info("Failed to find single file --> Crawling folder");
        /*
         * If e.g. crawler found page 100 but that folder only has 50 pages, there will be a redirect to the max/last page --> We do not
         * want to crawl anything then!
         */
        if (!performedSpecialWorkaround && linkending != null && !br.getURL().contains(linkending)) {
            logger.info("Accessed page doesn't exist");
            final String errmsg = "LinkEnding mismatch: " + linkending;
            final DownloadLink offline = this.createOfflinelink(contenturl, errmsg, errmsg);
            ret.add(offline);
            return ret;
        }
        final String numberofFilesStr = br.getRegex("class=\"bold\">(\\d+)</span> plik\\&#243;w<br />").getMatch(0);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("Nie znaleziono \\- błąd 404")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.containsHTML("class=\"greenActionButton\"|name=\"FolderId\"")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Check if link can be crawled */
        final String errorMsg = getError();
        if (errorMsg != null) {
            logger.info(String.format(errorMsg, contenturl));
            // Offline
            final DownloadLink dummy = createDownloadlink(contenturl.replace("chomikuj.pl/", "chomikujdecrypted.pl/") + "," + System.currentTimeMillis() + new Random().nextInt(100000));
            dummy.setAvailable(false);
            dummy.setName(errorMsg + "_" + new Regex(contenturl, "chomikuj\\.pl/(.+)").getMatch(0));
            ret.add(dummy);
            return ret;
        }
        /* Obtain some requiredvalues */
        if (username == null) {
            username = br.getRegex("name=\"(?:chomikId|ChomikName)\" type=\"hidden\" value=\"(.+?)\"").getMatch(0);
            if (username == null) {
                username = br.getRegex("id=\"__(?:accno|accname)\" name=\"__(?:accno|accname)\" type=\"hidden\" value=\"(.+?)\"").getMatch(0);
                if (username == null) {
                    username = br.getRegex("name=\"friendId\" type=\"hidden\" value=\"(.+?)\"").getMatch(0);
                    if (username == null) {
                        username = br.getRegex("\\&amp;(?:chomikId|ChomikName)=(.+?)\"").getMatch(0);
                    }
                }
            }
        }
        String folderID = br.getRegex("type=\"hidden\" name=\"FolderId\" value=\"(\\d+)\"").getMatch(0);
        if (folderID == null) {
            folderID = br.getRegex("name=\"FolderId\" type=\"hidden\" value=\"(\\d+)\"").getMatch(0);
        }
        if (requestVerificationToken == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String url = br.getURL();
        String baseURL = br.getURL();
        if (baseURL.endsWith("/")) {
            baseURL = baseURL.substring(0, baseURL.lastIndexOf("/"));
        }
        String subfolderPath = br.getRegex("value=\"https?://[^/]+/([^\"]+)\"[^>]*id=\"FolderAddress\"").getMatch(0);
        if (subfolderPath == null) {
            logger.warning("Failed to find subfolderPath in HTML code");
            subfolderPath = estimatedCurrentFolderPath;
            if (subfolderPath == null) {
                subfolderPath = br._getURL().getPath();
            }
        }
        subfolderPath = Encoding.htmlDecode(subfolderPath);
        String[][] subfolders = null;
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(subfolderPath);
        fp.setAllowMerge(true);
        int page = 1;
        /** More than one page? Every page goes back into the crawler as a single url! */
        if (page > 1 && !param.getCryptedUrl().matches(PAGEDECRYPTLINK)) {
            // Moved up
        } else {
            /* Crawl all pages, start with 1 */
            page = 1;
            final String pageCountStr = new Regex(url, ",(\\d{1,3})$").getMatch(0);
            if (pageCountStr != null) {
                page = Integer.parseInt(pageCountStr);
            }
            logger.info("Crawling page " + page + " of link: " + url);
            final Browser brc = br.cloneBrowser();
            prepareBrowser(url, brc);
            /** Only request further pages is folder isn't password protected */
            if (FOLDERPASSWORD != null) {
                getPage(brc, url);
            } else {
                if (username == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                } else if (folderID == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("ChomikName", username);
                query.appendEncoded("folderId", folderID);
                query.appendEncoded("__RequestVerificationToken", requestVerificationToken);
                query.appendEncoded("pageNr", Integer.toString(page));
                brc.postPage("/action/Files/FilesList", query);
            }
            String[] htmls = brc.getRegex("<li class=\"fileItemContainer\"(.*?)href=\"javascript:;\"").getColumn(0);
            if (htmls == null || htmls.length == 0) {
                /* 2020-04-29 */
                htmls = brc.getRegex("class=\"fileinfo tab\"(.*?)class=\"filedescription\"").getColumn(0);
            }
            if (htmls == null || htmls.length == 0) {
                /* Old fallback */
                htmls = brc.getRegex("class=\"fileinfo tab\"(.*?)href=\"javascript:;\"").getColumn(0);
            }
            final String folderTable = brc.getRegex("<div id=\"foldersList\">\\s*<table>(.*?)</table>\\s*</div>").getMatch(0);
            if (folderTable != null) {
                subfolders = new Regex(folderTable, "<a href=\"(/[^\"]+)\" rel=\"(\\d+)\" title=\"([^<>\"]*?)\"").getMatches();
            }
            for (final String html : htmls) {
                final DownloadLink file = createDownloadlink(url.replace("chomikuj.pl/", "chomikujdecrypted.pl/") + "," + System.currentTimeMillis() + new Random().nextInt(100000));
                String ext = null;
                String url_filename = null;
                final String fid = new Regex(html, "rel=\"(\\d+)\"").getMatch(0);
                String content_url = new Regex(html, "<li><a href=\"(/[^<>\"]*?)\"").getMatch(0);
                if (content_url != null) {
                    content_url = "https://" + this.getHost() + content_url;
                    url_filename = new Regex(content_url, "/([^<>\"/]+)$").getMatch(0);
                } else {
                    /* Let's build the contentURL ourself though it will not contain any filename then. */
                    content_url = baseURL + "/dummy," + fid + ".dummy";
                }
                String filesize = new Regex(html, "<li><span>(\\d+(,\\d+)? [A-Za-z]{1,5})</span>").getMatch(0);
                if (filesize == null) {
                    filesize = new Regex(html, "<li>[\t\n\r ]*?(\\d+(,\\d{1,2})? [A-Za-z]{1,5})[\t\n\r ]*?</li>").getMatch(0);
                }
                final Regex finfo = new Regex(html, "<span class=\"bold\">(.*?)</span>(\\.[^<>\"/]*?)\\s*</a>");
                /* Title without ext --> Will be added later */
                String filename = new Regex(html, "downloadContext\" href=\"[^\"]+\" title=\"([^<>\"]+)\"").getMatch(0);
                if (filename == null) {
                    filename = finfo.getMatch(0);
                }
                /* This is usually only for filenames without ext */
                if (filename == null) {
                    filename = new Regex(html, "data\\-title=\"([^<>\"]*?)\"").getMatch(0);
                }
                ext = finfo.getMatch(1);
                if (fid == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                /* Use filename from content_url as fallback if necessary */
                if (filename == null && url_filename != null) {
                    filename = url_filename;
                }
                if (filename == null) {
                    /* Fallback */
                    file.setName(fid);
                } else if (filename != null) {
                    filename = filename.replaceAll("(\\*([a-f0-9]{2}))", "%$2");
                    if (filename.contains("%")) {
                        filename = URLDecoder.decode(filename, "UTF-8");
                    }
                    filename = correctFilename(Encoding.htmlDecode(filename).trim());
                    filename = filename.replace("<span class=\"e\"> </span>", "");
                    filename = filename.replace("," + fid, "");
                    if (ext == null && url_filename.contains(".") && url_filename.lastIndexOf(".") >= 0) {
                        /* Probably extension is already in filename --> Find & correct it */
                        final String tempExt = url_filename.substring(url_filename.lastIndexOf("."));
                        if (tempExt != null) {
                            ext = new Regex(tempExt, "(" + ENDINGS + ").*?$").getMatch(0);
                            if (ext == null) {
                                /*
                                 * Last try to find the correct extension - if we fail to find it here the host plugin should find it
                                 * anyways!
                                 */
                                ext = new Regex(tempExt, "(\\.[A-Za-z0-9]+)").getMatch(0);
                            }
                            /* We found the good extension? Okay then let's remove the previously found bad extension! */
                            if (ext != null) {
                                filename = filename.replace(tempExt, "");
                            }
                        }
                    }
                    if (ext != null) {
                        ext = Encoding.htmlDecode(ext.trim());
                        if (!filename.endsWith(ext)) {
                            filename += ext;
                        }
                    }
                    file.setName(filename);
                    file.setProperty(ChoMikujPl.PROPERTY_DOWNLOADLINK_CRAWLED_FILENAME, filename);
                }
                file.setProperty(ChoMikujPl.PROPERTY_DOWNLOADLINK_FILEID, fid);
                if (folderID != null) {
                    file.setProperty(ChoMikujPl.PROPERTY_DOWNLOADLINK_FOLDER_ID, folderID);
                }
                if (subfolderPath != null) {
                    file.setRelativeDownloadFolderPath(subfolderPath);
                }
                if (filesize != null) {
                    /* File size should always be given */
                    file.setDownloadSize(SizeFormatter.getSize(filesize));
                } else {
                    logger.warning("Failed to find filesize");
                }
                file.setAvailable(true);
                if (FOLDERPASSWORD != null) {
                    file.setDownloadPassword(FOLDERPASSWORD);
                }
                if (fp != null) {
                    file._setFilePackage(fp);
                }
                file.setContentUrl(content_url);
                distribute(file);
                ret.add(file);
            }
        }
        final List<DownloadLink> ret_subfolders = new ArrayList<DownloadLink>();
        if (subfolders != null && subfolders.length != 0) {
            String currentFolderPath = new Regex(url, "chomikuj\\.pl(/.+)").getMatch(0);
            // work around Firefox copy/paste URL magic that automatically converts brackets
            // ( and ) to %28 and %29. Chomikuj.pl in page source has links with unencoded
            // brackets, so we need to fix this or links will not match and won't be added.
            currentFolderPath = currentFolderPath.replaceAll("%28", "(").replaceAll("%29", ")");
            for (String[] subfolderinfo : subfolders) {
                final String folderLink = br.getURL(subfolderinfo[0]).toExternalForm();
                final String folder_id = subfolderinfo[1];
                if (!folderLink.contains(currentFolderPath)) {
                    /* Skip unrelated (folder) URLs */
                    continue;
                } else if (folderLink.equals(url)) {
                    /* Skip current folder */
                    continue;
                }
                final DownloadLink dl = createDownloadlink(folderLink);
                if (FOLDERPASSWORD != null) {
                    dl.setDownloadPassword(FOLDERPASSWORD);
                }
                if (FOLDERPASSWORD_SPECIAL != null) {
                    dl.setProperty(PROPERTY_FOLDERPASSWORD_SPECIAL, FOLDERPASSWORD_SPECIAL);
                }
                // fp.add(dl);
                /* Can be used to speed up folder crawling in next round */
                dl.setProperty("folder_id", folder_id);
                ret_subfolders.add(dl);
            }
        }
        final boolean hasSubfolders = ret_subfolders.size() > 0;
        if ("0".equals(numberofFilesStr) && ret_subfolders.isEmpty()) {
            final String folderName = getCurrentURLFolderPath(param.getCryptedUrl());
            final String message = "This folder doesn't contain any files or subfolders.";
            final DownloadLink dummy = createOfflinelink(param.getCryptedUrl(), "EMPTY_FOLDER_" + folderName, message);
            ret.add(dummy);
            return ret;
        }
        if (!crawlFolders && hasSubfolders && ret.isEmpty()) {
            logger.info("No files returned: Folder contains no files, and subfolder crawling is disabled. Ignored subfolders: " + subfolders.length);
        }
        if (crawlFolders) {
            ret.addAll(ret_subfolders);
            if (ret.isEmpty()) {
                /* No results while there should be results */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        /* Save cookies for next time */
        synchronized (recentCookies) {
            recentCookies.put(this.getHost(), br.getCookies(br.getHost()));
        }
        return ret;
    }

    /**
     * Returns DownloadLink if single downloadable file is available according to html code in given browser instance. </br>
     * This can be used to determine if the current page is a folder or a single file.
     */
    private DownloadLink crawlSingleFile(final Browser br) {
        String filename = br.getRegex("Download:\\s*<b>([^<]+)</b>").getMatch(0);
        final String filesize = br.getRegex("<p class=\"fileSize\">([^<>\"]*?)</p>").getMatch(0);
        final String fid = br.getRegex("id=\"fileDetails_(\\d+)\"").getMatch(0);
        final String singleFileFolderPath = getSingleFileFolderPath(br);
        if (filename == null || fid == null) {
            return null;
        }
        filename = correctFilename(Encoding.htmlDecode(filename));
        final DownloadLink file = createDownloadlink(br.getURL().replace("chomikuj.pl/", "chomikujdecrypted.pl/") + "," + System.currentTimeMillis() + new Random().nextInt(100000));
        file.setDefaultPlugin(this.plugin);
        file.setHost(this.getHost());
        file.setProperty(ChoMikujPl.PROPERTY_DOWNLOADLINK_FILEID, fid);
        file.setName(filename);
        file.setProperty(ChoMikujPl.PROPERTY_DOWNLOADLINK_CRAWLED_FILENAME, filename);
        if (filesize != null) {
            file.setDownloadSize(SizeFormatter.getSize(Encoding.htmlDecode(filesize.trim().replace(",", "."))));
        }
        file.setAvailable(true);
        file.setContentUrl(br.getURL());
        if (singleFileFolderPath != null) {
            file.setRelativeDownloadFolderPath(singleFileFolderPath);
        }
        return file;
    }

    private String getCurrentURLFolderPath(final String url) {
        final String path = new Regex(url, "https?://[^/]+/(.+)").getMatch(0);
        if (path == null) {
            return null;
        }
        return Encoding.htmlDecode(path);
    }

    private String getSingleFileFolderPath(final Browser br) {
        final String singleFileFolderPath = br.getRegex("id=\"fileDetails\"[^>]*>\\s*<h1[^>]*><a href=\"/([^\"]*?)/[^/]+\"").getMatch(0);
        if (singleFileFolderPath == null) {
            return null;
        }
        return Encoding.htmlDecode(singleFileFolderPath);
    }

    public static String findRequestVerificationToken(final Browser br) {
        return br.getRegex("<input name=\"__RequestVerificationToken\"\\s*type=\"hidden\"\\s*value=\"([^\"]+)\"").getMatch(0);
    }

    private String getError() {
        String error = null;
        if (br.containsHTML("label for=\"Password\">Hasło</label><input id=\"Password\"")) {
            error = "Password protected links can't be decrypted: %s";
        } else if (br.containsHTML("Konto czasowo zablokowane")) {
            error = "Can't crawl link, the account of the owner is banned: %s";
        } else if (br.containsHTML("Chomik o takiej nazwie nie istnieje<|Nie znaleziono - błąd 404")) {
            error = "This link is offline (received error 404): %s";
        } else if (br.containsHTML("Chomik Jareczczek zablokowany")) {
            error = "Link blocked";
        } else if (br.containsHTML("<title>Przyjazny dysk internetowy \\- Chomikuj\\.pl</title>")) {
            error = "Link leads to mainpage";
        }
        return error;
    }

    /**
     * Handles all kind of folder-passwords. </br>
     * Important: Folders can have special "user login folder password" protection AND simple "folder password" protections. </br>
     * Subfolders may require different passwords than root folders so even though we store working passwords and retry them, users will be
     * asked for passwords countless times when adding big folders!
     */
    public void passwordHandling(final Object param) throws Exception {
        synchronized (LOCK) {
            logger.info("Entered password handling");
            final String urlBeforeEnteringPassword = br.getURL();
            if (isSpecialUserPasswordProtected(br)) {
                logger.info("Content is password protected (special folder user password)");
                /**
                 * This is not a folder password but another type of password which needs to be entered before! Some folders have this
                 * protection AND a folder-password!
                 */
                boolean success = false;
                for (int i = 0; i <= 3; i++) {
                    final Form pass = getSpecialUserPasswordProtectedForm(br);
                    if (param instanceof DownloadLink) {
                        FOLDERPASSWORD_SPECIAL = ((DownloadLink) param).getStringProperty(PROPERTY_FOLDERPASSWORD_SPECIAL);
                    }
                    if (FOLDERPASSWORD_SPECIAL == null) {
                        /* Try last working password first */
                        FOLDERPASSWORD_SPECIAL = this.getPluginConfig().getStringProperty(PROPERTY_FOLDERPASSWORD_SPECIAL);
                    }
                    if (FOLDERPASSWORD_SPECIAL == null || i > 0) {
                        if (param instanceof CryptedLink) {
                            FOLDERPASSWORD_SPECIAL = getUserInput("Enter folder USER password", (CryptedLink) param);
                        } else {
                            FOLDERPASSWORD_SPECIAL = getUserInput("Enter folder USER password", new CryptedLink(((DownloadLink) param).getPluginPatternMatcher()));
                        }
                    }
                    pass.put("Password", FOLDERPASSWORD_SPECIAL);
                    pass.remove("Remember");
                    /* This is set to true in host plugin - we will try to save- and re-use cookies there! */
                    pass.put("Remember", "False");
                    submitForm(pass);
                    if (isSpecialUserPasswordProtected(br)) {
                        continue;
                    } else {
                        success = true;
                        break;
                    }
                }
                if (!success) {
                    logger.info("Special folder password handling failed");
                    throw new DecrypterException(DecrypterException.PASSWORD);
                }
                logger.info("Special folder password handling successful");
                if (param instanceof DownloadLink) {
                    ((DownloadLink) param).setProperty(PROPERTY_FOLDERPASSWORD_SPECIAL, FOLDERPASSWORD_SPECIAL);
                }
                this.getPluginConfig().setProperty(PROPERTY_FOLDERPASSWORD_SPECIAL, FOLDERPASSWORD_SPECIAL);
                if (!br.getURL().equals(urlBeforeEnteringPassword)) {
                    /* Sometimes redirect to root may happen --> Correct that */
                    logger.info("Correcting URL: " + br.getURL() + " --> " + urlBeforeEnteringPassword);
                    br.getPage(urlBeforeEnteringPassword);
                }
            }
            if (isFolderPasswordProtected(br)) {
                logger.info("Content is password protected (folder password)");
                // prevent more than one password from processing and displaying at
                // any point in time!
                prepareBrowser(param.toString(), br);
                final Form pass = br.getFormbyProperty("id", "LoginToFolder");
                if (pass == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                Map<String, Object> folderAnswer = null;
                boolean success = false;
                for (int i = 0; i <= 3; i++) {
                    if (param instanceof CryptedLink) {
                        FOLDERPASSWORD = ((CryptedLink) param).getDecrypterPassword();
                    } else {
                        FOLDERPASSWORD = ((DownloadLink) param).getDownloadPassword();
                    }
                    if (FOLDERPASSWORD == null) {
                        /* Try last working password first */
                        FOLDERPASSWORD = this.getPluginConfig().getStringProperty(CFG_FOLDERPASSWORD);
                    }
                    if (FOLDERPASSWORD == null || i > 0) {
                        if (param instanceof CryptedLink) {
                            FOLDERPASSWORD = getUserInput(null, (CryptedLink) param);
                        } else {
                            FOLDERPASSWORD = getUserInput(null, new CryptedLink(((DownloadLink) param).getPluginPatternMatcher()));
                        }
                    }
                    pass.put("Password", FOLDERPASSWORD);
                    pass.remove("Remember");
                    /* This is set to true in host plugin - we will try to save- and re-use cookies there! */
                    pass.put("Remember", "False");
                    submitForm(pass);
                    folderAnswer = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    /* Important! The other parts of this plugin cannot handle escaped results! */
                    br.getRequest().setHtmlCode(PluginJSonUtils.unescape(br.toString()));
                    if (Boolean.TRUE.equals(folderAnswer.get("IsSuccess"))) {
                        success = true;
                        break;
                    } else {
                        continue;
                    }
                }
                if (!success) {
                    logger.info("Folder password handling failed");
                    throw new DecrypterException(DecrypterException.PASSWORD);
                }
                logger.info("Folder password handling successful");
                if (param instanceof CryptedLink) {
                    ((CryptedLink) param).setDecrypterPassword(FOLDERPASSWORD);
                } else {
                    ((DownloadLink) param).setDownloadPassword(FOLDERPASSWORD);
                }
                this.getPluginConfig().setProperty(CFG_FOLDERPASSWORD, FOLDERPASSWORD);
                /** Small workaround so following code can work with the raw HTML code */
                /* TODO: Remove this as it will nullify "br.getHttpConnection()"! */
                final Request req = new GetRequest(urlBeforeEnteringPassword);
                req.setHtmlCode(folderAnswer.get("Data").toString());
                br.setRequest(req);
            }
        }
    }

    public static boolean isFolderPasswordProtected(final Browser br) {
        if (br.containsHTML("Ten folder jest (<b>)?zabezpieczony oddzielnym hasłem")) {
            return true;
        } else {
            return false;
        }
    }

    /** 2021-11-12: Not(!) folder-password but one layer before! */
    public static boolean isSpecialUserPasswordProtected(final Browser br) {
        if (getSpecialUserPasswordProtectedForm(br) != null) {
            return true;
        } else {
            return false;
        }
    }

    public static Form getSpecialUserPasswordProtectedForm(final Browser br) {
        return br.getFormbyActionRegex("(?i).*/action/UserAccess/LoginToProtectedWindow");
    }

    private static void prepareBrowser(final String referer, final Browser br) {
        // Not needed but has been implemented so lets use it
        br.getHeaders().put("Referer", referer);
        br.getHeaders().put("Accept", "*/*");
        br.getHeaders().put("Accept-Language", "de-de,de;q=0.8,en-us;q=0.5,en;q=0.3");
        br.getHeaders().put("Accept-Encoding", "gzip,deflate");
        br.getHeaders().put("Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.7");
        br.getHeaders().put("Cache-Control", "no-cache");
        br.getHeaders().put("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        br.getHeaders().put("Pragma", "no-cache");
    }

    private String correctFilename(final String filename) {
        return filename.replace("<span class=\"e\"> </span>", "");
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }

    private ChoMikujPl plugin = null;

    private void getPage(final String parameter) throws Exception {
        getPage(br, parameter);
    }

    private void getPage(final Browser br, final String parameter) throws Exception {
        loadHosterPlugin();
        plugin.setBrowser(br);
        br.getPage(parameter);
    }

    private void postPage(final Browser br, final String url, final String arg) throws Exception {
        loadHosterPlugin();
        plugin.setBrowser(br);
        br.postPage(url, arg);
    }

    private void submitForm(final Form form) throws Exception {
        submitForm(br, form);
    }

    private void submitForm(final Browser br, final Form form) throws Exception {
        loadHosterPlugin();
        plugin.setBrowser(br);
        br.submitForm(form);
    }

    public void loadHosterPlugin() throws PluginException {
        if (plugin == null) {
            plugin = (ChoMikujPl) getNewPluginForHostInstance(this.getHost());
        }
    }
}