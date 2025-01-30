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
import java.util.Locale;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.parser.UrlQuery;

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
import jd.plugins.hoster.AnonServices;

@DecrypterPlugin(revision = "$Revision: 50526 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { AnonServices.class })
public class AnonServicesFolder extends PluginForDecrypt {
    public AnonServicesFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return AnonServices.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/folder/([A-Fa-f0-9\\-]{32,}).*");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String folderID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0).toLowerCase(Locale.ENGLISH);
        if (!AnonServices.looksLikeValidContentID(folderID)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Invalid folderID");
        }
        final AnonServices hosterplugin = (AnonServices) this.getNewPluginForHostInstance(this.getHost());
        this.br = hosterplugin.getBrowser();
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        int counter = 0;
        boolean passwordSuccess = false;
        Map<String, Object> entries = null;
        String passCode = UrlQuery.parse(param.getCryptedUrl()).get("folder_pw");
        do {
            // if (passCode == null) {
            // passCode = param.getDecrypterPassword();
            // }
            if (counter > 0) {
                /* Ask user to enter folder password. */
                passCode = getUserInput("Password?", param);
            }
            String url = AnonServices.API_BASE + "/folders/" + folderID;
            if (passCode != null) {
                url += "?folder_pw=" + Encoding.urlEncode(passCode);
            }
            br.getPage(url);
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            if (br.getHttpConnection().getResponseCode() != 403) {
                passwordSuccess = true;
                break;
            }
            logger.info("Password required or wrong: " + passCode);
            counter++;
        } while (!this.isAbort() && counter <= 3);
        if (!passwordSuccess) {
            throw new DecrypterException(DecrypterException.PASSWORD);
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (Boolean.TRUE.equals(entries.get("banned"))) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Folder has been banned");
        }
        if (passCode != null) {
            logger.info("Valid folder password: " + passCode);
        }
        final String thisFolderName = entries.get("folder_name").toString();
        String path = this.getAdoptedCloudFolderStructure();
        if (path == null) {
            /* We are at root */
            path = thisFolderName;
        } else {
            path += "/" + thisFolderName;
        }
        final List<Map<String, Object>> files = (List<Map<String, Object>>) entries.get("files");
        final List<Map<String, Object>> subfolders = (List<Map<String, Object>>) entries.get("subfolders");
        if (files != null && files.size() > 0) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(path);
            for (final Map<String, Object> file : files) {
                final String file_uuid = file.get("file_uuid").toString();
                final String url = hosterplugin.getFileURL(file_uuid, passCode);
                final DownloadLink link = this.createDownloadlink(url);
                link.setFinalFileName(file.get("filename").toString());
                link.setVerifiedFileSize(((Number) file.get("size")).longValue());
                link._setFilePackage(fp);
                if (passCode != null) {
                    link.setDownloadPassword(passCode);
                }
                link.setRelativeDownloadFolderPath(path);
                if (Boolean.TRUE.equals(file.get("banned"))) {
                    link.setAvailable(false);
                } else if (Boolean.TRUE.equals(file.get("deleted"))) {
                    link.setAvailable(false);
                } else {
                    link.setAvailable(true);
                }
                ret.add(link);
            }
        }
        if (subfolders != null && subfolders.size() > 0) {
            for (final Map<String, Object> subfolder : subfolders) {
                final String folder_uuid = subfolder.get("folder_uuid").toString();
                final String url = hosterplugin.getFolderURL(folder_uuid, passCode);
                final DownloadLink link = this.createDownloadlink(url);
                if (passCode != null) {
                    link.setDownloadPassword(passCode);
                }
                link.setRelativeDownloadFolderPath(path);
                ret.add(link);
            }
        }
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}
