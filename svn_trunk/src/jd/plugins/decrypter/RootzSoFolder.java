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
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.RootzSo;

@DecrypterPlugin(revision = "$Revision: 52079 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { RootzSo.class })
public class RootzSoFolder extends PluginForDecrypt {
    public RootzSoFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        return RootzSo.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/folder/([a-zA-Z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String folder_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage("https://www." + getHost() + "/api/folders/share/" + folder_id);
        if (br.getHttpConnection().getResponseCode() == 404) {
            /*
             * {"success":false,"error":"Folder not found or sharing is disabled"}
             */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> data = (Map<String, Object>) entries.get("data");
        final Map<String, Object> folderinfo = (Map<String, Object>) data.get("folder");
        final List<Map<String, Object>> files = (List<Map<String, Object>>) data.get("files");
        if (files == null || files.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(folderinfo.get("name").toString());
        fp.setPackageKey("rootz://folder/" + folder_id);
        for (final Map<String, Object> file : files) {
            final DownloadLink link = this.createDownloadlink("https://www." + getHost() + "/d/" + file.get("short_id"));
            link.setFinalFileName(file.get("name").toString());
            link.setVerifiedFileSize(((Number) file.get("size")).longValue());
            link.setAvailable(true);
            link._setFilePackage(fp);
            ret.add(link);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
