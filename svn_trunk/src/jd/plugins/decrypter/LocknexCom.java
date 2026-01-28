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

import org.appwork.storage.TypeRef;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52186 $", interfaceVersion = 3, names = {}, urls = {})
public class LocknexCom extends PluginForDecrypt {
    public LocknexCom(PluginWrapper wrapper) {
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
        ret.add(new String[] { "locknex.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/container/(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        /* They got an API but it can only be used with a personal API key, see: https://locknex.com/api/index */
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("class=\"fa-duotone fa-folder-open\"[^>]*>\\s*</i>([^<]+)<span").getMatch(0);
        String urls_json = br.getRegex("window\\.CNL_LINKS = (\\[[^\\]]+\\])").getMatch(0);
        if (urls_json == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final List<String> links = (List<String>) restoreFromString(urls_json, TypeRef.OBJECT);
        if (links == null || links.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        for (final String singleLink : links) {
            ret.add(createDownloadlink(singleLink));
        }
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(Encoding.htmlDecode(title).trim());
        }
        fp.addLinks(ret);
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
