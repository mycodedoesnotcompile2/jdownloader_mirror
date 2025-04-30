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
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DownloadRu;

@DecrypterPlugin(revision = "$Revision: 51028 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { DownloadRu.class })
public class DownloadRuFolder extends PluginForDecrypt {
    public DownloadRuFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return DownloadRu.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/folders/([A-Za-z0-9]{8})");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String folder_id = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        br.getHeaders().put("Referer", param.getCryptedUrl());
        br.getHeaders().put("Accept", "application/json, text/plain, */*");
        br.getPage("https://" + getHost() + "/folders/" + folder_id + ".json");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> items = (List<Map<String, Object>>) entries.get("contents");
        final Map<String, Object> folder = (Map<String, Object>) entries.get("object");
        final String folder_title = folder.get("name").toString();
        String path = this.getAdoptedCloudFolderStructure();
        if (path == null) {
            path = folder_title;
        } else {
            path += "/" + folder_title;
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(path);
        final DownloadRu hosterplugin = (DownloadRu) this.getNewPluginForHostInstance(this.getHost());
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        for (final Map<String, Object> item : items) {
            if (Boolean.TRUE.equals(item.get("is_dir"))) {
                final DownloadLink link = this.createDownloadlink("https://" + this.getHost() + "/folders/" + item.get("id"));
                link.setRelativeDownloadFolderPath(path);
                ret.add(link);
            } else {
                final DownloadLink link = this.createDownloadlink("https://" + this.getHost() + "/files/" + item.get("id"));
                hosterplugin.parseFileInfo(link, item);
                link.setRelativeDownloadFolderPath(path);
                link._setFilePackage(fp);
                ret.add(link);
            }
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
