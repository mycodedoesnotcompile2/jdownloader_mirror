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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.IDriveCom;

import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@DecrypterPlugin(revision = "$Revision: 52359 $", interfaceVersion = 3, names = {}, urls = {})
public class IdriveComFolder extends PluginForDecrypt {
    public IdriveComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "idrive.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_1 = Pattern.compile("/idrive/sh/sh/([a-z0-9]{2,})(/(.+))?");
    private static final Pattern PATTERN_2 = Pattern.compile("/idrive/sh/sh\\?k=([a-z0-9]{2,})");

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_1.pattern().substring(1) + "|" + PATTERN_2.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final PluginForHost defaultPlugin = getNewPluginForHostInstance(this.getHost()).getLazyP().getPrototype(null);
        Form pwform = getPasswordProtectedForm(this, br);
        String passCode = param.getDecrypterPassword();
        if (pwform != null) {
            logger.info("Item is password protected");
            boolean success = false;
            for (int i = 0; i <= 3; i++) {
                if (i > 0 || passCode == null) {
                    passCode = getUserInput("Password?", param);
                }
                pwform.put("password", Encoding.urlEncode(passCode));
                br.submitForm(pwform);
                pwform = getPasswordProtectedForm(this, br);
                if (pwform == null) {
                    success = true;
                    break;
                }
            }
            if (!success) {
                throw new DecrypterException(DecrypterException.PASSWORD);
            }
        }
        String relative_path_from_url = new Regex(URLHelper.getUrlWithoutParams(contenturl), PATTERN_1).getMatch(2);
        if (relative_path_from_url != null) {
            relative_path_from_url = URLEncode.decodeURIComponent(relative_path_from_url);
        }
        final String json_prefetch = br.getRegex("var\\s*share_Response\\s*=\\s*(\\{.*?\\});").getMatch(0);
        final Map<String, Object> prefetch = restoreFromString(json_prefetch, TypeRef.MAP);
        String serverAddress = prefetch.get("serverAddress").toString();
        List<Map<String, Object>> contents = (List<Map<String, Object>>) prefetch.get("contents");
        if (contents.size() == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String shareid = prefetch.get("shareid").toString();
        int numberofFiles = 0;
        int numberofFolders = 0;
        int numberofEmptyFolders = 0;
        try {
            final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
            final List<String> resourcePaths = new ArrayList<String>();
            {
                // query shareProperties
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("shareid", shareid);
                Browser brc = br.cloneBrowser();
                brc.postPage("/idrive/home/shareProperties", query);
                final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                serverAddress = entries.get("serverAddress").toString();
                contents = (List<Map<String, Object>>) entries.get("contents");
                if (contents.size() == 0) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final FilePackage fp = FilePackage.getInstance();
                for (final Map<String, Object> content : contents) {
                    final String resourcePath = content.get("resourcePath").toString();
                    final Object resourceType = content.get("resourceType");
                    if ("0".equals(resourceType)) {
                        /* Folder */
                        resourcePaths.add(resourcePath);
                        numberofFolders++;
                        continue;
                    }
                    /* File */
                    final String name = content.get("name").toString();
                    final DownloadLink link = new DownloadLink(defaultPlugin, defaultPlugin.getHost(), "");
                    link.setProperty(IDriveCom.PROPERTY_NAME, name);
                    link.setProperty(IDriveCom.PROPERTY_PATH, resourcePath);
                    link.setProperty(IDriveCom.PROPERTY_SHAREID, shareid);
                    link.setProperty(IDriveCom.PROPERTY_SERVER, serverAddress);
                    link.setFinalFileName(name);
                    final Number size = (Number) ReflectionUtils.cast(content.get("size"), Number.class);
                    if (size != null) {
                        link.setVerifiedFileSize(size.longValue());
                    }
                    final String path_without_filename = resourcePath.replaceFirst("/" + Pattern.quote(name) + "$", "");
                    link.setRelativeDownloadFolderPath(path_without_filename);
                    link.setDownloadPassword(passCode);
                    link.setAvailable(true);
                    fp.setName(path_without_filename);
                    link._setFilePackage(fp);
                    ret.add(link);
                    distribute(link);
                    numberofFiles++;
                }
            }
            if (!isAbort() && resourcePaths.size() > 0) {
                logger.info("Crawling subfolders");
                // EVSID token required for evs api
                final String evsTokenUrl = br.getRegex("var\\s*evsTokenUrl\\s*=\\s*(\"|')(.*?)(\"|')").getMatch(1);
                if (StringUtils.isNotEmpty(evsTokenUrl)) {
                    final Browser evsCookies = br.cloneBrowser();
                    evsCookies.openHeadConnection(evsTokenUrl).disconnect();
                } else if (br.getCookie(getHost(), "EVSID", Cookies.NOTDELETEDPATTERN) == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                if (relative_path_from_url != null) {
                    final String root_path = JavaScriptEngineFactory.walkJson(resourcePaths, "{0}/resourcePath").toString();
                    String full_path_to_desired_folder = root_path;
                    if (!full_path_to_desired_folder.endsWith("/")) {
                        full_path_to_desired_folder += "/";
                    }
                    if (relative_path_from_url.startsWith("/")) {
                        full_path_to_desired_folder += relative_path_from_url.substring(1);
                    } else {
                        full_path_to_desired_folder += relative_path_from_url;
                    }
                    /* API requires paths to end with slash. */
                    if (!full_path_to_desired_folder.endsWith("/")) {
                        full_path_to_desired_folder += "/";
                    }
                    logger.info("Crawling only items below user desired path: " + full_path_to_desired_folder);
                    resourcePaths.clear();
                    resourcePaths.add(full_path_to_desired_folder);
                }

                int numberof_http_requests = 0;
                final HashSet<String> dupes = new HashSet<String>();
                crawl_subfolders_recursive: do {
                    /* Crawl (desired) subfolders in recursive way */
                    final String resourcePath = resourcePaths.remove(0);
                    // query folders via evs/browseFolder
                    final UrlQuery query = new UrlQuery();
                    query.appendEncoded("p", resourcePath);
                    query.appendEncoded("json", "yes");
                    query.appendEncoded("device_id", "");
                    final Browser brc = br.cloneBrowser();
                    numberof_http_requests++;
                    brc.postPage("https://" + serverAddress + "/evs/browseFolder", query);
                    final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                    if (!"SUCCESS".equals(entries.get("message"))) {
                        logger.info("Stopping because: message!='success'");
                        break crawl_subfolders_recursive;
                    }
                    final String path = entries.get("path").toString();
                    contents = (List<Map<String, Object>>) entries.get("contents");
                    if (contents.isEmpty()) {
                        /* This subfolder is offline -> Provide feedback to user */
                        final DownloadLink emptyFolder = createLinkCrawlerRetry(getCurrentLink(), new DecrypterRetryException(RetryReason.EMPTY_FOLDER, resourcePath));
                        ret.add(emptyFolder);
                        distribute(emptyFolder);
                        numberofEmptyFolders++;
                        continue crawl_subfolders_recursive;
                    }
                    final FilePackage fp = FilePackage.getInstance();
                    fp.setName(resourcePath);
                    int numberofNewItemsThisRun = 0;
                    for (final Map<String, Object> content : contents) {
                        final String entryName = content.get("name").toString();
                        String entryPath = path;
                        if (!entryPath.endsWith("/")) {
                            entryPath += "/";
                        }
                        entryPath += entryName;
                        if (!dupes.add(entryPath)) {
                            /* Endless-loop protection -> This should never happen! */
                            logger.warning("Skipping duplicated path: " + entryPath);
                            continue;
                        }
                        if (Boolean.TRUE.equals(content.get("is_dir"))) {
                            resourcePaths.add(0, entryPath);
                            numberofFolders++;
                        } else {
                            /* File */
                            final DownloadLink link = new DownloadLink(defaultPlugin, defaultPlugin.getHost(), "");
                            link.setProperty(IDriveCom.PROPERTY_NAME, entryName);
                            link.setProperty(IDriveCom.PROPERTY_PATH, entryPath);
                            link.setProperty(IDriveCom.PROPERTY_SHAREID, shareid);
                            link.setProperty(IDriveCom.PROPERTY_SERVER, serverAddress);
                            link.setFinalFileName(entryName);
                            final Number size = (Number) ReflectionUtils.cast(content.get("size"), Number.class);
                            if (size != null) {
                                link.setVerifiedFileSize(size.longValue());
                            }
                            link.setRelativeDownloadFolderPath(path);
                            link.setDownloadPassword(passCode);
                            link.setAvailable(true);
                            link._setFilePackage(fp);
                            ret.add(link);
                            distribute(link);
                            numberofFiles++;
                        }
                        numberofNewItemsThisRun++;
                    }
                    logger.info("HTTP-Requests: " + numberof_http_requests + " | Crawled files: " + numberofFiles + " | Non-empty folders: " + numberofFolders + " | Empty folders: " + numberofEmptyFolders);
                    if (numberofNewItemsThisRun == 0) {
                        logger.warning("Found 0 new items this run -> Stopping to prevent endless loop");
                        break crawl_subfolders_recursive;
                    }
                } while (resourcePaths.size() > 0 && !this.isAbort());
            }
            return ret;
        } finally {
            logger.info("Crawled files: " + numberofFiles + " | Non-empty folders: " + numberofFolders + " | Empty folders: " + numberofEmptyFolders);
        }
    }

    public static Form getPasswordProtectedForm(Plugin plugin, final Browser br) {
        return br.getFormbyProperty("id", "share_pass_form");
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        return 1;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
