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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.LixstreamCom;

@DecrypterPlugin(revision = "$Revision: 51253 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { LixstreamCom.class })
public class LixstreamComFolder extends PluginForDecrypt {
    public LixstreamComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return LixstreamCom.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/d/([a-zA-Z0-9]{8,})");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        final String folder_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        final LixstreamCom hosterplugin = (LixstreamCom) this.getNewPluginForHostInstance(this.getHost());
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Referer", contenturl);
        final Request request = brc.createPostRequest(hosterplugin.getAPIBase() + "/s/home/resources/" + folder_id, "");
        request.getHeaders().put("Content-Type", "application/json");
        final DownloadLink dummy = this.createDownloadlink("");
        hosterplugin.getPage(dummy, brc, request);
        if (brc.getHttpConnection().getResponseCode() == 400) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server issues (error 400)", 15 * 60 * 1000l);
        } else if (brc.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> files = (List<Map<String, Object>>) entries.get("files");
        if (files.isEmpty()) {
            /* invalid folder id, example response: {"ip_country":"DE","files":[],"sid":"ZINoz666"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> folder_info = files.get(0);
        /* 2025-07-24: Value of size looks to always be 0 so we cannot use this to detect empty/offline folders. */
        // final Number folder_size = (Number) folder_info.get("size");
        // if (folder_size != null && folder_size.intValue() == 0) {
        // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        // }
        final String folder_suid = entries.get("suid").toString();
        final String folder_internal_id = folder_info.get("id").toString();
        String folder_title = folder_info.get("display_name").toString();
        final int page_size = 20;
        int page = 1;
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(folder_title);
        fp.setPackageKey(this.getHost() + ":/folder/" + folder_internal_id);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final HashSet<String> dupes = new HashSet<String>();
        long last_file_update_time = 0;
        String last_file_file_id = null;
        pagination: do {
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("uid", folder_suid);
            postdata.put("dir_id", folder_internal_id);
            postdata.put("page_size", page_size);
            postdata.put("offset", last_file_update_time);
            if (page > 1) {
                postdata.put("file_id", last_file_file_id);
            }
            // brc.postPageRaw("/v2/s/assets/d/list", JSonStorage.serializeToJson(postdata));
            final Request req = brc.createJSonPostRequest("/v2/s/assets/d/list", postdata);
            hosterplugin.getPage(dummy, brc, req);
            final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.OBJECT);
            int new_items_this_page = 0;
            for (final Map<String, Object> file : ressourcelist) {
                final String file_id = file.get("id").toString();
                if (!dupes.add(file_id)) {
                    continue;
                }
                new_items_this_page++;
                final String url = "https://www." + getHost() + "/s/" + folder_id + "?p=" + folder_suid + "&f=" + file_id;
                final DownloadLink link = createDownloadlink(url);
                hosterplugin.parseFileInfo(brc, null, link, file);
                link.setAvailable(true);
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
                /* Update values for pagination to next page */
                last_file_update_time = ((Number) file.get("update_time")).longValue();
                last_file_file_id = file_id;
            }
            logger.info("Crawled page " + page + " | Next offset: " + last_file_update_time + " | Items found so far: " + ret.size());
            if (ressourcelist.size() < page_size) {
                logger.info("Stopping because: Current page size is smaller than max page size of " + page_size);
                break pagination;
            } else if (new_items_this_page == 0) {
                /* Fail-safe */
                logger.info("Stopping because: Current page did not contain any new items");
                break pagination;
            }
            /* Continue to next page */
            page++;
        } while (!this.isAbort());
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* Try to avoid rate limit / error 400 responses */
        return 1;
    }
}
