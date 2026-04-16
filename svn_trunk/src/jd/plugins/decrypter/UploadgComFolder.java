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
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.UploadgCom;

@DecrypterPlugin(revision = "$Revision: 52664 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { UploadgCom.class })
public class UploadgComFolder extends PluginForDecrypt {
    public UploadgComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        return UploadgCom.getPluginDomains();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return UploadgCom.getAnnotationUrls();
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String json = br.getRegex("window\\.bootstrapData = (\\{.*?\\});").getMatch(0);
        final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
        final Map<String, Object> linkinfo = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "loaders/shareableLinkPage/link");
        final Map<String, Object> entry = (Map<String, Object>) linkinfo.get("entry");
        final Map<String, Object> folderChildren = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "loaders/shareableLinkPage/folderChildren");
        if (folderChildren == null) {
            /* Single file */
            final DownloadLink main = this.createDownloadlink(contenturl);
            main.setFinalFileName(entry.get("name").toString());
            main.setVerifiedFileSize(((Number) entry.get("file_size")).longValue());
            ret.add(main);
            return ret;
        }
        final List<Map<String, Object>> filelist = (List<Map<String, Object>>) folderChildren.get("data");
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(entry.get("name").toString());
        fp.setPackageKey("uploadg://folder/" + entry.get("id"));
        if (entry.get("deleted_at") != null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final HashSet<String> dupes = new HashSet<String>();
        for (final Map<String, Object> fileinfo : filelist) {
            /* 2026-04-15: They do not provide any ability to link to files that are part of a folder. */
            final DownloadLink link = this.createDownloadlink(contenturl);
            final String filename = fileinfo.get("name").toString();
            link.setFinalFileName(filename);
            link.setVerifiedFileSize(((Number) fileinfo.get("file_size")).longValue());
            /* Important properties */
            final String file_id = fileinfo.get("id").toString();
            link.setProperty(UploadgCom.PROPERTY_FILENAME, filename);
            link.setProperty(UploadgCom.PROPERTY_INTERNAL_FILE_ID, file_id);
            link.setProperty(UploadgCom.PROPERTY_INTERNAL_FILE_HASH, fileinfo.get("hash"));
            link.setAvailable(true);
            link._setFilePackage(fp);
            ret.add(link);
            distribute(link);
            dupes.add(file_id);
        }
        // TODO: implement pagination
        logger.info("Crawled page " + "TODO/" + folderChildren.get("last_page") + " | Crawled links so far: " + ret.size() + "/" + folderChildren.get("total"));
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
