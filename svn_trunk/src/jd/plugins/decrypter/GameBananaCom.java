//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
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

import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52863 $", interfaceVersion = 3, names = { "gamebanana.com" }, urls = { "https?://gamebanana\\.com/(mods/(download/)?\\d+(#.*)?|linkfilter\\?url=https?%3A%2F%2F.+)" })
public class GameBananaCom extends PluginForDecrypt {
    public GameBananaCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final String API_BASE = "https://gamebanana.com/apiv12/";

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String url = param.getCryptedUrl();
        if (url.contains("/linkfilter?url=")) {
            final String redurl = new Regex(url, "/linkfilter\\?url=(http[^\\s#&]{5,})").getMatch(0);
            if (redurl == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            ret.add(createDownloadlink(URLDecoder.decode(redurl, "UTF-8")));
        } else {
            final String id = new Regex(url, "//gamebanana\\.com/mods/(?:download/)?(\\d+)").getMatch(0);
            if (id == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            br.getPage(API_BASE + "Mod/" + id + "/ProfilePage");
            if (br.getHttpConnection().getResponseCode() == 404) {
                /* E.g. {"_sErrorCode":"NO_SUCH_RECORD","_sErrorMessage":"This Mod doesn't exist"} */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> dlp = (Map<String, Object>) restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String name = dlp.get("_sName").toString();
            final List<Map<String, Object>> files = (List<Map<String, Object>>) dlp.get("_aFiles");
            final Map<String, FilePackage> fpmap = new HashMap<String, FilePackage>();
            for (final Map<String, Object> file : files) {
                String path = name + "_v" + file.get("_sVersion").toString();
                FilePackage fp = fpmap.get(path);
                if (fp == null) {
                    fp = FilePackage.getInstance();
                    fp.setName(path);
                    fpmap.put(path, fp);
                }
                final DownloadLink dl = createDownloadlink(DirectHTTP.createURLForThisPlugin(file.get("_sDownloadUrl").toString()));
                dl.setFinalFileName(file.get("_sFile").toString());
                dl.setDownloadSize(((Number) file.get("_nFilesize")).longValue());
                dl.setMD5Hash(file.get("_sMd5Checksum").toString());
                dl.setRelativeDownloadFolderPath(path);
                dl.setLastModifiedTimestamp(((Number) file.get("_tsDateAdded")).longValue() * 1000l);
                dl.setComment(file.get("_sDescription").toString());
                if (file.get("_sAnalysisResult").toString() == "ok") {
                    dl.setAvailable(true);
                }
                dl._setFilePackage(fp);
                ret.add(dl);
            }
            final List<Map<String, Object>> xfiles = (List<Map<String, Object>>) dlp.get("_aAlternateFileSources");
            for (final Map<String, Object> file : xfiles) {
                final DownloadLink dl = createDownloadlink(file.get("url").toString());
                dl.setComment(file.get("description").toString());
                ret.add(dl);
            }
        }
        return ret;
    }
}
