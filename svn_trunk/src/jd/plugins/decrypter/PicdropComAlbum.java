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
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
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
import jd.plugins.hoster.PicdropCom;

@DecrypterPlugin(revision = "$Revision: 51807 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { PicdropCom.class })
public class PicdropComAlbum extends PluginForDecrypt {
    public PicdropComAlbum(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return PicdropCom.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([\\w-]+)/([a-zA-Z0-9]+)$");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final Regex urlinfo = new Regex(param.getCryptedUrl(), this.getSupportedLinks());
        final String username = urlinfo.getMatch(0);
        final String album_key = urlinfo.getMatch(1);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final PicdropCom hosterplugin = (PicdropCom) this.getNewPluginForHostInstance(getHost());
        this.br = hosterplugin.createNewBrowserInstance();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username + " - " + album_key);
        fp.setPackageKey(getHost() + "://user/" + username + "/album/" + album_key);
        int page = 1;
        String lastSortKey = null;
        final HashSet<String> dupes = new HashSet<String>();
        pagination: do {
            final UrlQuery query = new UrlQuery();
            /* 2025-11-07: Same limit value as website */
            query.add("limit", "500");
            if (lastSortKey != null) {
                query.add("lastSortKey", Encoding.urlEncode(lastSortKey));
            }
            br.getPage("https://www." + getHost() + "/api/content/" + username + ":" + album_key + "/files?limit=500");
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final List<Map<String, Object>> files = (List<Map<String, Object>>) entries.get("files");
            int numberofNewItems = 0;
            for (final Map<String, Object> file : files) {
                final String key = file.get("key").toString();
                if (!dupes.add(key)) {
                    /* Fail safe to prevent infinite loops: Skip duplicates */
                    continue;
                }
                numberofNewItems++;
                final String url = "https://www." + getHost() + "/" + username + "/" + album_key + "?file=" + file.get("key");
                final DownloadLink link = createDownloadlink(url);
                link.setFinalFileName(file.get("name").toString());
                link.setVerifiedFileSize(((Number) file.get("size")).longValue());
                link.setAvailable(true);
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
            }
            lastSortKey = (String) entries.get("lastSortKey");
            logger.info("Crawled page " + page + " | Found items this page: " + numberofNewItems + " | Found items so far: " + ret.size() + " | lastSortKey=" + lastSortKey);
            if (Boolean.FALSE.equals(entries.get("hasMore"))) {
                logger.info("Stopping because: Reached end");
                break pagination;
            } else if (numberofNewItems == 0) {
                /* Fail safe */
                logger.info("Stopping because: Failed to find any new items on current page");
                break pagination;
            } else if (StringUtils.isEmpty(lastSortKey)) {
                /* This should never happen */
                logger.info("Stopping because: lastSortKey is null while it shouldn't be null");
                break pagination;
            }
            /* Continue to next page */
            page++;
        } while (!this.isAbort());
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
