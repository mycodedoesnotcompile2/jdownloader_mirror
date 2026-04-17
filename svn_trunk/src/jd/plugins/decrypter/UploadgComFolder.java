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

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.UploadgCom;

@DecrypterPlugin(revision = "$Revision: 52672 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { UploadgCom.class })
public class UploadgComFolder extends PluginForDecrypt {
    public UploadgComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        return UploadgCom.getPluginDomains();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return UploadgCom.getAnnotationUrls();
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String json = br.getRegex("window\\.bootstrapData = (\\{.*?\\});").getMatch(0);
        final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
        final Map<String, Object> linkinfo = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "loaders/shareableLinkPage/link");
        final String hash = linkinfo.get("hash").toString();
        final Map<String, Object> entry = (Map<String, Object>) linkinfo.get("entry");
        if (entry.get("deleted_at") != null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        Map<String, Object> folderChildren = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "loaders/shareableLinkPage/folderChildren");
        if (folderChildren == null || folderChildren.isEmpty()) {
            /* Single file */
            final DownloadLink main = this.createDownloadlink(contenturl);
            main.setFinalFileName(entry.get("name").toString());
            main.setVerifiedFileSize(((Number) entry.get("file_size")).longValue());
            ret.add(main);
            return ret;
        }
        final String xsrftoken = br.getCookie(br.getHost(), "XSRF-TOKEN");
        int page = 1;
        final Browser brc = br.cloneBrowser();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(entry.get("name").toString());
        fp.setPackageKey("uploadg://folder/" + entry.get("id"));
        pagination: do {
            final List<Map<String, Object>> filelist = (List<Map<String, Object>>) folderChildren.get("data");
            final HashSet<String> dupes = new HashSet<String>();
            /* 2026-04-16: Typically up to 50 items per page */
            int numberof_new_items_this_page = 0;
            for (final Map<String, Object> fileinfo : filelist) {
                /* 2026-04-15: They do not provide any ability to link to files that are part of a folder. */
                final DownloadLink link = this.createDownloadlink(contenturl);
                final String filename = fileinfo.get("name").toString();
                link.setFinalFileName(filename);
                link.setVerifiedFileSize(((Number) fileinfo.get("file_size")).longValue());
                /* Important properties */
                final String file_id = fileinfo.get("id").toString();
                if (!dupes.add(file_id)) {
                    continue;
                }
                link.setProperty(UploadgCom.PROPERTY_FILENAME, filename);
                link.setProperty(UploadgCom.PROPERTY_INTERNAL_FILE_ID, file_id);
                link.setProperty(UploadgCom.PROPERTY_INTERNAL_FILE_HASH, fileinfo.get("hash"));
                link.setAvailable(true);
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
                numberof_new_items_this_page++;
            }
            final int last_page = ((Number) folderChildren.get("last_page")).intValue();
            final int total_numberof_items = ((Number) folderChildren.get("total")).intValue();
            logger.info("Crawled page " + page + "/" + last_page + " | Crawled links so far: " + ret.size() + "/" + total_numberof_items);
            if (page >= last_page) {
                logger.info("Stopping because: Reached last page");
                break pagination;
            } else if (ret.size() >= total_numberof_items) {
                /* Fail-safe #1 */
                logger.info("Stopping because: Found all items");
                break pagination;
            } else if (numberof_new_items_this_page == 0) {
                /* Fail-safe #2 */
                logger.info("Stopping because: Failed to find any new items on current page");
                break pagination;
            } else if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                throw new InterruptedException();
            }
            /* Continue to next page */
            page++;
            if (StringUtils.isEmpty(xsrftoken)) {
                /* This token is required to perform API requests. */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            brc.getHeaders().put("x-xsrf-token", xsrftoken);
            brc.getPage("/api/v1/shareable-links/" + hash + "?loader=shareableLinkPage&page=" + page + "&order=updated_at:desc");
            final Map<String, Object> entries2 = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            folderChildren = (Map<String, Object>) entries2.get("folderChildren");
        } while (!this.isAbort());
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
