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

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;

import org.appwork.storage.TypeRef;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.config.SubConfiguration;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.PCloudCom;

@DecrypterPlugin(revision = "$Revision: 50284 $", interfaceVersion = 2, names = {}, urls = {})
public class PCloudComFolder extends PluginForDecrypt {
    public PCloudComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        prepBR(br);
        return br;
    }

    public static void prepBR(final Browser br) {
        br.setFollowRedirects(true);
        br.getHeaders().put("Accept-Encoding", "gzip");
        br.getHeaders().put("Accept", "application/json, text/javascript, */*; q=0.01");
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        br.getHeaders().put("Accept-Language", "en-us;q=0.7,en;q=0.3");
        br.getHeaders().put("Accept-Charset", null);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "pcloud.com", "pcloud.link", "pc.cd" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "/(?:publink/show\\?code=|#page=publink\\&code=)([a-zA-Z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    private static final String DOWNLOAD_ZIP   = "DOWNLOAD_ZIP_2";
    long                        totalSize      = 0;
    private String              foldercode     = null;
    private Map<String, Object> emptyFolderMap = new HashMap<String, Object>();
    PluginForHost               plg            = null;

    @SuppressWarnings({ "deprecation", "unchecked" })
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        emptyFolderMap.clear();
        final String contenturl = param.getCryptedUrl();
        foldercode = new Regex(contenturl, "(?i)code=([A-Za-z0-9]+)").getMatch(0);
        if (foldercode == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String passCode = param.getDecrypterPassword();
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String fid = getFID(contenturl);
        int attempt = 0;
        Map<String, Object> entries = null;
        boolean passwordSuccess = false;
        int result = 0;
        do {
            attempt++;
            if (attempt > 1) {
                passCode = getUserInput("Password?", param);
            }
            final UrlQuery query = new UrlQuery();
            query.add("code", fid);
            if (passCode != null) {
                query.appendEncoded("linkpassword", passCode);
            }
            br.getPage("https://" + PCloudCom.getAPIDomain(new URL(contenturl).getHost()) + "/showpublink?" + query);
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            result = ((Number) entries.get("result")).intValue();
            if (result == PCloudCom.STATUS_CODE_DOWNLOAD_PASSWORD_REQUIRED || result == PCloudCom.STATUS_CODE_DOWNLOAD_PASSWORD_INVALID) {
                if (passCode != null) {
                    logger.info("User entered invalid password: " + passCode);
                } else {
                    logger.info("This item is password protected");
                }
                passwordSuccess = false;
                continue;
            } else {
                passwordSuccess = true;
                break;
            }
        } while (attempt <= 3);
        if (!passwordSuccess) {
            throw new DecrypterException(DecrypterException.PASSWORD);
        }
        final Map<String, Object> metadata = (Map<String, Object>) entries.get("metadata");
        if (metadata == null) {
            /* Looks like item is offline */
            final String errormsg = (String) entries.get("error");
            /* 7002 = deleted by the owner, 7003 = abused */
            if (result == 7002 || result == 7003) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                logger.info("Item is offline because: " + errormsg);
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        }
        final String folderNameMain = (String) metadata.get("name");
        /* This will skip empty folders */
        addFolder(ret, metadata, null, contenturl, null);
        if (ret.size() > 1 && SubConfiguration.getConfig(this.getHost()).getBooleanProperty(DOWNLOAD_ZIP, false)) {
            /* = all files (links) of the folder as .zip archive */
            final DownloadLink main = createDownloadlink("http://pclouddecrypted.com/" + System.currentTimeMillis() + new Random().nextInt(100000));
            main.setProperty("plain_code", foldercode);
            final String main_name = folderNameMain + ".zip";
            main.setFinalFileName(folderNameMain);
            main.setProperty("plain_name", main_name);
            main.setProperty("plain_size", Long.toString(totalSize));
            main.setProperty("complete_folder", true);
            main.setProperty("plain_code", foldercode);
            if (totalSize > 0) {
                main.setDownloadSize(totalSize);
            }
            main.setAvailable(true);
            ret.add(main);
        }
        /* Set additional properties */
        for (final DownloadLink thisresult : ret) {
            thisresult.setProperty("mainlink", contenturl);
            if (passCode != null) {
                thisresult.setDownloadPassword(passCode, true);
            }
        }
        final String desiredFolderID = new Regex(contenturl, "(?i)folder=(\\d+)").getMatch(0);
        if (desiredFolderID != null && emptyFolderMap.containsKey(desiredFolderID)) {
            logger.info("Desired folder is empty -> Ignore everything else we crawled and throw exception");
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, "EMPTY_FOLDER_" + emptyFolderMap.get(desiredFolderID));
        }
        if (emptyFolderMap.size() > 0) {
            logger.info("Number of skipped empty subfolders: " + emptyFolderMap.size());
            final Iterator<Entry<String, Object>> iterator = emptyFolderMap.entrySet().iterator();
            while (iterator.hasNext()) {
                final Entry<String, Object> entry = iterator.next();
                logger.info("Skipped empty subfolder: " + entry.getValue());
            }
        }
        /* This will only happen if the folder and all of its subfolders are empty. */
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, "EMPTY_FOLDER_" + folderNameMain);
        }
        return ret;
    }

    /**
     * Recursive function to crawl all folders/subfolders
     *
     * @throws PluginException
     */
    @SuppressWarnings("unchecked")
    private void addFolder(final ArrayList<DownloadLink> results, final Map<String, Object> entries, final String lastFpname, final String containerURL, String path) throws PluginException {
        List<Map<String, Object>> ressourcelist_temp = null;
        final boolean isFolder = ((Boolean) entries.get("isfolder"));
        if (isFolder) {
            /* Only update lastFoldername if we actually have a folder ... */
            final String thisFolderName = (String) entries.get("name");
            if (path == null) {
                path = thisFolderName;
            } else {
                path += "/" + thisFolderName;
            }
            ressourcelist_temp = (List<Map<String, Object>>) entries.get("contents");
            if (ressourcelist_temp.isEmpty()) {
                logger.info("Found empty folder: " + path);
                emptyFolderMap.put(entries.get("folderid").toString(), path);
            } else {
                for (final Map<String, Object> ressource : ressourcelist_temp) {
                    addFolder(results, ressource, thisFolderName, containerURL, path);
                }
            }
        } else {
            results.add(addSingleItem(entries, path));
        }
    }

    private void ensureInitHosterplugin() throws PluginException {
        if (this.plg == null) {
            plg = this.getNewPluginForHostInstance(this.getHost());
        }
    }

    private DownloadLink addSingleItem(final Map<String, Object> entries, final String path) throws PluginException {
        ensureInitHosterplugin();
        /* For single loose files, parentfolderid may not be given. */
        final Object parentfolderid = entries.get("parentfolderid");
        final String fileid = Long.toString(((Number) entries.get("fileid")).longValue());
        final PluginForHost plg = this.getNewPluginForHostInstance(this.getHost());
        final DownloadLink file = createDownloadlink(parentfolderid + "_" + fileid);
        file.setDefaultPlugin(plg);
        file.setHost(this.getHost());
        final long filesize = ((Number) entries.get("size")).longValue();
        String filename = entries.get("name").toString();
        totalSize += filesize;
        file.setProperty("plain_name", filename);
        file.setProperty("plain_size", filesize);
        file.setProperty("plain_fileid", fileid);
        if (parentfolderid != null) {
            file.setProperty("plain_parentfolderid", parentfolderid);
        }
        file.setProperty("plain_code", foldercode);
        file.setAvailable(true);
        if (path != null) {
            file.setRelativeDownloadFolderPath(path);
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(path);
            file._setFilePackage(fp);
        }
        filename = Encoding.htmlDecode(filename).trim();
        file.setVerifiedFileSize(filesize);
        file.setFinalFileName(filename);
        return file;
    }

    private String getFID(final String link) {
        return new Regex(link, this.getSupportedLinks()).getMatch(0);
    }
}
