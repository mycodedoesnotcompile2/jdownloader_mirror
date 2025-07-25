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
import java.util.List;
import java.util.Map;

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

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@DecrypterPlugin(revision = "$Revision: 51250 $", interfaceVersion = 3, names = {}, urls = {})
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
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            logger.info("This plugin is still under development -> Returning zero results in Stable version of JDownloader");
            return new ArrayList<DownloadLink>();
        }
        final String contenturl = param.getCryptedUrl();
        final String folderID = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Referer", contenturl);
        Request request = brc.createPostRequest("https://api.lixstreamingcaio.com/v2/s/home/resources/" + folderID, "");
        request.getHeaders().put("Content-Type", "application/json");
        brc.getPage(request);
        if (brc.getHttpConnection().getResponseCode() == 400) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server issues (error 400)", 15 * 60 * 1000l);
        } else if (brc.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> folder_info = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "files/{0}");
        /* 2025-07-24: Value of size looks to always be 0 so we cannot use this to detect empty/offline folders. */
        // final Number folder_size = (Number) folder_info.get("size");
        // if (folder_size != null && folder_size.intValue() == 0) {
        // throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        // }
        // TODO: Use getPage function of hoster plugin to perform requests
        final LixstreamCom hosterplugin = (LixstreamCom) this.getNewPluginForHostInstance(this.getHost());
        final String suid = entries.get("suid").toString();
        final String internal_folder_id = folder_info.get("id").toString();
        String folder_title = folder_info.get("display_name").toString();
        int offset = 0;
        final int page_size = 20;
        int page = 1;
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(folder_title);
        pagination: do {
            final Map<String, Object> postdata = new HashMap<String, Object>();
            postdata.put("uid", suid);
            postdata.put("dir_id", internal_folder_id);
            postdata.put("page_size", page_size);
            postdata.put("offset", offset);
            brc.postPageRaw("/v2/s/assets/d/list", JSonStorage.serializeToJson(postdata));
            final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.OBJECT);
            for (final Map<String, Object> file : ressourcelist) {
                final DownloadLink link = createDownloadlink("TODO");
                hosterplugin.parseFileInfo(brc, null, link, file);
                link.setAvailable(true);
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
            }
            offset += ressourcelist.size();
            logger.info("Crawled page " + page + " | Offset: " + offset + " | Items found so far: " + ret.size());
            if (ressourcelist.isEmpty()) {
                logger.info("Stopping because: Current page is empty");
                break pagination;
            }
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
}
