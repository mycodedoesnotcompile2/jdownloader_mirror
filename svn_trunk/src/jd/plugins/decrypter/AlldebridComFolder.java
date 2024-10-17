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

import org.appwork.storage.TypeRef;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.AllDebridCom;

@DecrypterPlugin(revision = "$Revision: 49981 $", interfaceVersion = 3, names = {}, urls = {})
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
        final String addedURL = param.getCryptedUrl();
        /*
         * Important: Every account has its own files. A magnetID generated inside account A will not work in account B! Alldebrid support
         * knows about this issue and is thinking about adding a modifier into their magnetURL so that we know which account to use in case
         * the user owns multiple alldebrid.com accounts.
         */
        final String magnetID = new Regex(addedURL, this.getSupportedLinks()).getMatch(0);
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account == null) {
            throw new AccountRequiredException();
        }
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("agent", AllDebridCom.agent_raw);
        query.appendEncoded("apikey", AllDebridCom.getStoredApiKey(account));
        query.appendEncoded("id", magnetID);
        /* When "jd" parameter is supplied, information which is unnecessary for JD will be excluded from json response. */
        query.appendEncoded("jd", "");
        br.getPage(AllDebridCom.api_base_41 + "/magnet/status?" + query.toString());
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        if (entries.containsKey("error")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> magnet = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "data/magnets");
        final String torrentName = (String) magnet.get("filename");
        final String torrentNameEscaped = Pattern.quote(torrentName);
        final List<Map<String, Object>> resourcelist = (List<Map<String, Object>>) magnet.get("files");
        if (resourcelist == null || resourcelist.isEmpty()) {
            /* Probably unfinished torrent download */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String folderRoot = torrentName;
        final FilePackage fpRoot = FilePackage.getInstance();
        fpRoot.setName(folderRoot);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        crawlRecursive(ret, new HashMap<String, FilePackage>(), resourcelist, torrentNameEscaped, torrentName);
        return ret;
    }

    private List<DownloadLink> crawlRecursive(final List<DownloadLink> results, final Map<String, FilePackage> fps, Object o, final String torrentNameEscaped, final String path) {
        if (o == null) {
            return null;
        }
        if (o instanceof List) {
            for (final Object resourceO : (List<Object>) o) {
                this.crawlRecursive(results, fps, resourceO, torrentNameEscaped, path);
            }
            return results;
        } else {
            final Map<String, Object> resource = (Map<String, Object>) o;
            final String url = (String) resource.get("l");
            final String name = resource.get("n").toString();
            if (url != null) {
                /* Single file */
                final String folderRoot = new Regex(path, "^([^/]+)").getMatch(0);
                final DownloadLink dl = this.createDownloadlink(url);
                dl.setName(name);
                dl.setDownloadSize(((Number) resource.get("s")).longValue());
                final boolean isSpecialRar = name.matches("^" + torrentNameEscaped + "(\\.rar|\\.part\\d+\\.rar)$");
                if (isSpecialRar) {
                    FilePackage fpRoot = fps.get(folderRoot);
                    if (fpRoot == null) {
                        fpRoot = FilePackage.getInstance();
                        fpRoot.setName(folderRoot);
                        fps.put(folderRoot, fpRoot);
                    }
                    dl.setRelativeDownloadFolderPath(folderRoot);
                    dl._setFilePackage(fpRoot);
                } else {
                    /* Remove filename from path. */
                    final String filePath = path.replaceAll("/" + Pattern.quote(name) + "$", "");
                    /* File that goes into (nested) subfolder. */
                    dl.setRelativeDownloadFolderPath(filePath);
                    FilePackage nestedFilePackage = fps.get(filePath);
                    if (nestedFilePackage == null) {
                        nestedFilePackage = FilePackage.getInstance();
                        nestedFilePackage.setName(filePath);
                        fps.put(filePath, nestedFilePackage);
                    }
                    dl._setFilePackage(nestedFilePackage);
                }
                dl.setAvailable(true);
                results.add(dl);
            } else {
                final List<Map<String, Object>> nextSubFolderLevel = (List<Map<String, Object>>) resource.get("e");
                /* Go deeper */
                return this.crawlRecursive(results, fps, nextSubFolderLevel, torrentNameEscaped, path + "/" + name);
            }
        }
        return results;
    }
}
