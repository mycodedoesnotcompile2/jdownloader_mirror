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
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.AllDebridCom;

import org.appwork.storage.TypeRef;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.scripting.JavaScriptEngineFactory;

@DecrypterPlugin(revision = "$Revision: 51328 $", interfaceVersion = 3, names = {}, urls = {})
public class AlldebridComFolder extends PluginForDecrypt {
    public AlldebridComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "alldebrid.com", "alldebrid.fr", "alldebrid.org", "alldebrid.it", "alldebrid.de", "alldebrid.es" });
        return ret;
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/getMagnet/(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    /** API docs: https://docs.alldebrid.com/#status */
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        /*
         * Important: Every account has its own files. A magnetID generated inside account A will not work in account B! Alldebrid support
         * knows about this issue and is thinking about adding a modifier into their magnetURL so that we know which account to use in case
         * the user owns multiple alldebrid.com accounts.
         */
        final String magnetID = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account == null) {
            throw new AccountRequiredException();
        }
        final AllDebridCom plg = (AllDebridCom) this.getNewPluginForHostInstance(this.getHost());
        plg.login(account, new AccountInfo(), false);
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("id", magnetID);
        /* When "jd" parameter is supplied, information which is unnecessary for JD will be excluded from json response. */
        query.appendEncoded("jd", "");
        br.postPage(AllDebridCom.api_base_v_41 + "/magnet/status", query);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> errormap = (Map<String, Object>) entries.get("error");
        if (errormap != null) {
            /* E.g. {"status":"error","error":{"code":"MAGNET_INVALID_ID","message":"This magnet ID does not exists or is invalid"}} */
            final String msg = errormap.get("message").toString();
            throw new DecrypterRetryException(RetryReason.FILE_NOT_FOUND, magnetID + "_" + msg, msg);
        }
        final Map<String, Object> magnet = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "data/magnets");
        final String torrentName = (String) magnet.get("filename");
        final List<Map<String, Object>> resourcelist = (List<Map<String, Object>>) magnet.get("files");
        if (resourcelist == null || resourcelist.isEmpty()) {
            /* Most likely we have an unfinished torrent download */
            final String status = (String) magnet.get("status");
            throw new DecrypterRetryException(RetryReason.FILE_NOT_FOUND, torrentName + "_" + status, status);
        }
        final String folderRoot = torrentName;
        final FilePackage fpRoot = FilePackage.getInstance();
        fpRoot.setName(folderRoot);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        crawlRecursive(ret, new HashMap<String, FilePackage>(), resourcelist, torrentName);
        return ret;
    }

    /**
     * Recursive function which crawls [nested] files from json structure, reconstructing the original folder structure if there are files
     * in subfolders.s
     */
    private List<DownloadLink> crawlRecursive(final List<DownloadLink> results, final Map<String, FilePackage> fps, Object o, final String path) {
        if (o instanceof List) {
            for (final Object resourceO : (List<Object>) o) {
                this.crawlRecursive(results, fps, resourceO, path);
            }
        } else {
            final Map<String, Object> resource = (Map<String, Object>) o;
            final String url = (String) resource.get("l");
            /* Name of file or subfolder */
            final String name = resource.get("n").toString();
            if (url != null) {
                /* Single file in folder root */
                final DownloadLink dl = this.createDownloadlink(url);
                dl.setName(name);
                final Number downloadSize = (Number) resource.get("s");
                if (downloadSize != null) {
                    dl.setDownloadSize(downloadSize.longValue());
                }
                /* File that goes into (nested) subfolder. */
                /* Remove filename from path. */
                final String filePath = path.replaceAll("/" + Pattern.quote(name) + "$", "");
                FilePackage nestedFilePackage = fps.get(filePath);
                if (nestedFilePackage == null) {
                    nestedFilePackage = FilePackage.getInstance();
                    nestedFilePackage.setName(filePath);
                    fps.put(filePath, nestedFilePackage);
                }
                dl.setRelativeDownloadFolderPath(filePath);
                dl._setFilePackage(nestedFilePackage);
                dl.setAvailable(true);
                results.add(dl);
            } else {
                final List<Map<String, Object>> nextSubFolderLevel = (List<Map<String, Object>>) resource.get("e");
                /* Go deeper */
                return this.crawlRecursive(results, fps, nextSubFolderLevel, path + "/" + name);
            }
        }
        return results;
    }
}
