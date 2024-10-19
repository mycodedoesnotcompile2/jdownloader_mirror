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
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.storage.TypeRef;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 49989 $", interfaceVersion = 3, names = { "icloud.com" }, urls = { "https?://(?:www\\.)?icloud\\.com/sharedalbum/(?:[A-Za-z\\-]+/)?#[A-Za-z0-9]+" })
public class IcloudCom extends PluginForDecrypt {
    public IcloudCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    /* on = will try to grab final 'nice' filenames right away, off = will leave out this step! */
    private static final boolean extendedMode = true;

    /**
     * Crawls photos & videos from public iCloud URLs. It is possible to really speed this up and directly show final filenames via the
     * "/webassets" request, see host plugin.
     */
    @SuppressWarnings({ "unchecked", "deprecation" })
    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        LinkedHashMap<String, DownloadLink> hashmap_checksum = new LinkedHashMap<String, DownloadLink>();
        String jsonphotoGuids = "{\"photoGuids\":[";
        String jsonderivatives = "\"derivatives\":{";
        final String jsonAll;
        final String parameter = param.toString();
        final String folderid = new Regex(parameter, "([A-Za-z0-9]+)$").getMatch(0);
        /* Not necessarily needed! */
        br.getHeaders().put("Referer", "https://www.icloud.com/sharedalbum/");
        String host = "p121-sharedstreams.icloud.com";
        Map<String, Object> root = null;
        boolean triedSecondHost = false;
        secondHostLoop: do {
            br.postPageRaw("https://" + host + "/" + folderid + "/sharedstreams/webstream", "{\"streamCtag\":null}");
            root = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String newHost = (String) root.get("X-Apple-MMe-Host");
            if (newHost != null) {
                if (triedSecondHost) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                host = newHost;
                triedSecondHost = true;
                continue secondHostLoop;
            } else {
                break secondHostLoop;
            }
        } while (true);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        Map<String, Object> entries_tmp = null;
        final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) root.get("photos");
        if (ressourcelist.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        final String userFirstName = (String) root.get("userFirstName");
        final String userLastName = (String) root.get("userLastName");
        final String streamName = (String) root.get("streamName");
        if (userFirstName == null || userLastName == null || streamName == null) {
            return null;
        }
        final String contributorFullName = userFirstName + " " + userLastName;
        String fpName = contributorFullName + " - " + streamName;
        int counter = 0;
        for (final Map<String, Object> photo : ressourcelist) {
            final String photoGuid = (String) photo.get("photoGuid");
            final String batchGuid = (String) photo.get("batchGuid");
            final String mediaAssetType = (String) photo.get("mediaAssetType");
            final String ext_temp;
            final Map<String, Object> derivatives = (Map<String, Object>) photo.get("derivatives");
            if (photoGuid == null || batchGuid == null || derivatives == null) {
                continue;
            }
            String checksum = null;
            /* Get best quality picture/video */
            if ("video".equalsIgnoreCase(mediaAssetType)) {
                ext_temp = jd.plugins.hoster.IcloudCom.default_ExtensionVideo;
            } else {
                ext_temp = jd.plugins.hoster.IcloudCom.default_ExtensionImage;
            }
            /* Find highest quality photo/video */
            long filesizeMax = 0;
            long filesizeTemp;
            final Iterator<Entry<String, Object>> it = derivatives.entrySet().iterator();
            Entry<String, Object> entrytemp = null;
            while (it.hasNext()) {
                entrytemp = it.next();
                entries_tmp = (Map<String, Object>) entrytemp.getValue();
                filesizeTemp = JavaScriptEngineFactory.toLong(entries_tmp.get("fileSize"), 0);
                if (filesizeTemp > filesizeMax) {
                    filesizeMax = filesizeTemp;
                    checksum = (String) entries_tmp.get("checksum");
                }
            }
            if (checksum == null) {
                continue;
            }
            final DownloadLink dl = this.createDownloadlink("http://iclouddecrypted.com/" + photoGuid + "_" + checksum);
            dl.setContentUrl("https://www." + this.getHost() + "/sharedalbum/de-de/#" + folderid + ";" + photoGuid);
            if (filesizeMax > 0) {
                dl.setDownloadSize(filesizeMax);
            }
            dl.setName(photoGuid + "_" + checksum + ext_temp);
            dl.setAvailable(true);
            dl.setProperty("folderid", folderid);
            if (mediaAssetType != null) {
                dl.setProperty("type", mediaAssetType);
            }
            /* Build our POST-json for later. */
            if (counter > 0) {
                jsonphotoGuids += ",";
                jsonderivatives += ",";
            }
            jsonphotoGuids += "\"" + photoGuid + "\"";
            jsonderivatives += String.format("\"%s\":[\"%s\"]", photoGuid, checksum);
            if (!extendedMode) {
                /* Only add links to List here, if we're not in extended mode!! */
                ret.add(dl);
            }
            hashmap_checksum.put(checksum, dl);
            counter++;
        }
        jsonphotoGuids += "],";
        jsonderivatives += "}}";
        jsonAll = jsonphotoGuids + jsonderivatives;
        if (extendedMode) {
            /* Try to find final 'nice' filenames right away! */
            br.postPageRaw("/" + folderid + "/sharedstreams/webasseturls", jsonAll);
            final Map<String, Object> root_extended = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(br.getRequest().getHtmlCode());
            final Map<String, Object> items = (Map<String, Object>) root_extended.get("items");
            DownloadLink dl = null;
            final Iterator<Entry<String, DownloadLink>> it = hashmap_checksum.entrySet().iterator();
            Entry<String, DownloadLink> entrytemp = null;
            String checksum_tmp = null;
            String finallink_tmp = null;
            String final_filename_tmp = null;
            /* Iterate through our DownloadLinks and find the good filenames! */
            while (it.hasNext()) {
                entrytemp = it.next();
                checksum_tmp = entrytemp.getKey();
                dl = entrytemp.getValue();
                entries_tmp = (Map<String, Object>) items.get(checksum_tmp);
                /* Usually 'entries_tmp' should be != null, containing the filename information we want! */
                if (entries_tmp != null) {
                    finallink_tmp = jd.plugins.hoster.IcloudCom.getDirectlink(entries_tmp);
                    final_filename_tmp = jd.plugins.hoster.IcloudCom.getFilenameFromDirectlink(finallink_tmp);
                    if (finallink_tmp != null) {
                        dl.setProperty("directlink", finallink_tmp);
                    }
                    if (final_filename_tmp != null) {
                        dl.setFinalFileName(final_filename_tmp);
                    }
                }
                ret.add(dl);
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(Encoding.htmlDecode(fpName.trim()));
        fp.addLinks(ret);
        return ret;
    }
}
