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

import org.appwork.storage.TypeRef;

import com.formdev.flatlaf.util.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 51287 $", interfaceVersion = 3, names = {}, urls = {})
public class ThreeCatCat extends PluginForDecrypt {
    public ThreeCatCat(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "3cat.cat" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/.+/(audio|video)/(\\d+)/");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final Regex urlinfo = new Regex(contenturl, this.getSupportedLinks());
        final String mediaType = urlinfo.getMatch(0);
        final String mediaID = urlinfo.getMatch(1);
        br.getPage("https://dinamics.ccma.cat/pvideo/media.jsp?media=" + mediaType + "&version=0s&idint=" + mediaID);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> info = (Map<String, Object>) entries.get("informacio");
        final Map<String, Object> images = (Map<String, Object>) entries.get("imatges");
        final Map<String, Object> media = (Map<String, Object>) entries.get("media");
        final String title = info.get("titol_complet").toString();
        final String description = (String) info.get("descripcio");
        /* Add thumbnail */
        final String thumbnail = images.get("url").toString();
        ret.add(this.createDownloadlink(DirectHTTP.createURLForThisPlugin(thumbnail)));
        /* Add video qualities */
        final List<Map<String, Object>> qualities = (List<Map<String, Object>>) media.get("url");
        for (final Map<String, Object> quality : qualities) {
            final String url = quality.get("file").toString();
            ret.add(this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url)));
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        if (!StringUtils.isEmpty(description)) {
            fp.setComment(description);
        }
        fp.setPackageKey(this.getHost() + ":/" + mediaType + "/" + mediaID);
        for (final DownloadLink result : ret) {
            result._setFilePackage(fp);
            result.setAvailable(true);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
