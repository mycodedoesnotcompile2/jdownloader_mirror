//jDownloader - Downloadmanager
//Copyright (C) 2013  JD-Team support@jdownloader.org
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

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
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
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.MochaMy;

@DecrypterPlugin(revision = "$Revision: 53800 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { MochaMy.class })
public class MochaMyFolder extends PluginForDecrypt {
    public MochaMyFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        return MochaMy.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/share/([A-Za-z0-9_-]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public Browser createNewBrowserInstance() {
        return MochaMy.prepBrowserAPI(super.createNewBrowserInstance());
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String shareToken = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        final String apiBase = MochaMy.getAPIBase(getHost());
        br.getPage(apiBase + "/shares/" + shareToken);
        if (br.getHttpConnection().getResponseCode() == 404 || br.getHttpConnection().getResponseCode() == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> shareResponse = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> share = (Map<String, Object>) shareResponse.get("share");
        if (!Boolean.TRUE.equals(share.get("isActive"))) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (!Boolean.TRUE.equals(share.get("isFolder"))) {
            /* Single file share */
            final DownloadLink link = this.createDownloadlink("https://" + getHost() + "/dl/" + shareToken);
            MochaMy.setFileInfo(link, share.get("originalName").toString(), ((Number) share.get("fileSize")).longValue());
            link.setContentUrl(param.getCryptedUrl());
            link.setAvailable(true);
            link.setPasswordProtected(Boolean.TRUE.equals(share.get("passwordProtected")));
            ret.add(link);
            return ret;
        }
        String passCode = param.getDecrypterPassword();
        String accessToken = null;
        if (Boolean.TRUE.equals(share.get("passwordProtected"))) {
            int attempts = 0;
            do {
                if (passCode == null) {
                    passCode = getUserInput("Password?", param);
                }
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("password", passCode);
                br.postPageRaw(apiBase + "/shares/" + shareToken + "/verify-password", JSonStorage.serializeToJson(postdata));
                if (br.getHttpConnection().getResponseCode() == 200) {
                    final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    accessToken = entries.get("accessToken").toString();
                    logger.info("Password is correct: " + passCode);
                    break;
                }
                logger.info("Wrong password: " + passCode);
                passCode = null;
                attempts++;
            } while (attempts < 3);
            if (accessToken == null) {
                throw new DecrypterException(DecrypterException.PASSWORD);
            }
        } else {
            passCode = null;
        }
        String url = apiBase + "/shares/" + shareToken + "/folder";
        if (accessToken != null) {
            url += "?accessToken=" + Encoding.urlEncode(accessToken);
        }
        br.getPage(url);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> folder = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> entries = (List<Map<String, Object>>) folder.get("entries");
        final String folderName = share.get("folderName").toString();
        if (entries.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, "EMPTY_FOLDER_" + shareToken);
        }
        /* One FilePackage per (sub-)folder. Key = relative path. */
        final Map<String, FilePackage> packages = new HashMap<String, FilePackage>();
        for (final Map<String, Object> entry : entries) {
            final String fileID = entry.get("id").toString();
            final String fileName = entry.get("fileName").toString();
            final String path = entry.get("path").toString();
            final DownloadLink link = this.createDownloadlink("https://" + getHost() + "/dl/" + shareToken + "/" + fileID);
            MochaMy.setFileInfo(link, fileName, ((Number) entry.get("fileSize")).longValue());
            link.setContentUrl(param.getCryptedUrl());
            link.setAvailable(true);
            link.setPasswordProtected(passCode != null);
            if (passCode != null) {
                link.setDownloadPassword(passCode);
            }
            /* "path" contains the file name and the path of subfolders relative to the root folder. */
            String relativePath = folderName;
            final String subPath = new Regex(path, "^(.*)/[^/]+$").getMatch(0);
            if (!StringUtils.isEmpty(subPath)) {
                relativePath += "/" + subPath;
            }
            link.setRelativeDownloadFolderPath(relativePath);
            FilePackage fp = packages.get(relativePath);
            if (fp == null) {
                fp = FilePackage.getInstance();
                fp.setName(relativePath);
                fp.setPackageKey("mochamy://" + shareToken + "/" + relativePath);
                packages.put(relativePath, fp);
            }
            link._setFilePackage(fp);
            ret.add(link);
        }
        return ret;
    }
}
