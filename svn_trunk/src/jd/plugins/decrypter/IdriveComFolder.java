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
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
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
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52335 $", interfaceVersion = 3, names = {}, urls = {})
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

    private static final Pattern PATTERN_1 = Pattern.compile("plugin_not_done_yet/idrive/sh/sh/([a-z0-9]{2,})(/(.+))?");
    private static final Pattern PATTERN_2 = Pattern.compile("plugin_not_done_yet/idrive/sh/sh\\?k=([a-z0-9]{2,})");

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_1.pattern().substring(1) + "|" + PATTERN_2.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final Regex regex1 = new Regex(URLHelper.getUrlWithoutParams(contenturl), PATTERN_1);
        String relative_path_from_url = null;
        String shareid_from_url;
        if (regex1.patternFind()) {
            relative_path_from_url = regex1.getMatch(2);
            shareid_from_url = regex1.getMatch(0);
        } else {
            shareid_from_url = new Regex(contenturl, PATTERN_2).getMatch(0);
        }
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (br.containsHTML("id=\"file_list_container_empty\"")) {
            final String title_for_empty_folder;
            if (relative_path_from_url != null) {
                title_for_empty_folder = shareid_from_url + "/" + relative_path_from_url;
            } else {
                title_for_empty_folder = shareid_from_url;
            }
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, title_for_empty_folder);
        }
        Form pwform = this.getPasswordProtectedForm(br);
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
                pwform = this.getPasswordProtectedForm(br);
                if (pwform == null) {
                    success = true;
                    break;
                }
            }
            if (!success) {
                throw new DecrypterException(DecrypterException.PASSWORD);
            }
        }
        // final String path_1 = br.getRegex("var path = \"([^\"]+)\";").getMatch(0);
        final String json_prefetch = br.getRegex("var share_Response = (\\{.*?\\});").getMatch(0);
        final Map<String, Object> prefetch = restoreFromString(json_prefetch, TypeRef.MAP);
        final String serverAddress = prefetch.get("serverAddress").toString();
        final String shareid = prefetch.get("shareid").toString();
        final Map<String, Object> start_content = (Map<String, Object>) JavaScriptEngineFactory.walkJson(prefetch, "contents/{0}");
        final String root_path = start_content.get("resourcePath").toString();
        final String resourceType = start_content.get("resourceType").toString();
        int numberofFiles = 0;
        int numberofFolders = 0;
        if (StringUtils.isEmpty(resourceType)) {
            /* Single file */
            final UrlQuery query = new UrlQuery();
            query.appendEncoded("shareid", shareid);
            br.postPage("/idrive/home/shareProperties", query);
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final List<Map<String, Object>> contents = (List<Map<String, Object>>) entries.get("contents");
            final FilePackage fp = FilePackage.getInstance();
            // fp.setName(start_path);
            for (final Map<String, Object> content : contents) {
                final String name = content.get("name").toString();
                final String file_path = content.get("resourcePath").toString();
                final String path_without_filename = file_path.replaceFirst("/" + Pattern.quote(name) + "$", "");
                final String url = "https://" + serverAddress + "/evs/downloadFile?version=0&p=" + URLEncode.encodeURIComponent(file_path);
                final DownloadLink link = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
                link.setFinalFileName(name);
                final Number size = (Number) ReflectionUtils.cast(content.get("size"), Long.class);
                if (size != null) {
                    link.setVerifiedFileSize(size.longValue());
                }
                link.setRelativeDownloadFolderPath(path_without_filename);
                link.setAvailable(true);
                fp.setName(path_without_filename);
                link._setFilePackage(fp);
                ret.add(link);
            }
            numberofFiles = 1;
        } else {
            /* Folder with multiple files / subfolders */
            String full_path_to_current_folder;
            if (relative_path_from_url != null) {
                full_path_to_current_folder = root_path;
                if (!full_path_to_current_folder.endsWith("/")) {
                    full_path_to_current_folder += "/";
                }
                full_path_to_current_folder += relative_path_from_url;
                /* API requires paths to end with slash. */
                if (!full_path_to_current_folder.endsWith("/")) {
                    full_path_to_current_folder += "/";
                }
            } else {
                full_path_to_current_folder = root_path;
            }
            final UrlQuery query = new UrlQuery();
            query.add("p", URLEncode.encodeURIComponent(full_path_to_current_folder));
            query.appendEncoded("json", "yes");
            query.appendEncoded("device_id", "");
            final Browser brc = br.cloneBrowser();
            brc.postPage("https://" + serverAddress + "/evs/browseFolder", query);
            final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            final String path = entries.get("path").toString();
            final List<Map<String, Object>> contents = (List<Map<String, Object>>) entries.get("contents");
            if (contents.isEmpty()) {
                throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, shareid_from_url + "/" + full_path_to_current_folder);
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(full_path_to_current_folder);
            for (final Map<String, Object> content : contents) {
                final String name = content.get("name").toString();
                if (Boolean.TRUE.equals(content.get("is_dir"))) {
                    /* Folder */
                    String this_folder_path = path;
                    if (!this_folder_path.endsWith("/")) {
                        this_folder_path += "/";
                    }
                    this_folder_path += name;
                    final String url = "https://www.idrive.com/idrive/sh/sh/" + shareid + this_folder_path;
                    final DownloadLink link = this.createDownloadlink(url);
                    ret.add(link);
                    numberofFolders++;
                } else {
                    /* File */
                    final String file_path = path + "/" + name;
                    final String url = "https://" + serverAddress + "/evs/downloadFile?version=0&p=" + URLEncode.encodeURIComponent(file_path);
                    final DownloadLink link = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
                    link.setFinalFileName(name);
                    final Number size = (Number) ReflectionUtils.cast(content.get("size"), Long.class);
                    if (size != null) {
                        link.setVerifiedFileSize(size.longValue());
                    }
                    link.setRelativeDownloadFolderPath(path);
                    link.setAvailable(true);
                    link._setFilePackage(fp);
                    ret.add(link);
                    numberofFiles++;
                }
            }
        }
        if (passCode != null) {
            for (final DownloadLink link : ret) {
                link.setDownloadPassword(passCode);
            }
        }
        logger.info("Crawled files: " + numberofFiles + " | Folders: " + numberofFolders);
        return ret;
    }

    private Form getPasswordProtectedForm(final Browser br) {
        return br.getFormbyProperty("id", "share_pass_form");
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
