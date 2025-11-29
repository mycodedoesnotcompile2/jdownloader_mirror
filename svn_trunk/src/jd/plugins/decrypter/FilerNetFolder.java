//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
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
import jd.plugins.PluginForHost;
import jd.plugins.hoster.FilerNet;

@DecrypterPlugin(revision = "$Revision: 51898 $", interfaceVersion = 2, names = {}, urls = {})
@PluginDependencies(dependencies = { FilerNet.class })
public class FilerNetFolder extends PluginForDecrypt {
    public FilerNetFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.getHeaders().put("User-Agent", "JDownloader");
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return FilerNet.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/folder/([a-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String folderID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        if (folderID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final PluginForHost hostPlugin = getNewPluginForHostInstance(getHost());
        br.getPage(((FilerNet) hostPlugin).getAPI_BASE() + "/folder/" + folderID);
        if (br.getHttpConnection().getResponseCode() == 404) {
            /* {"message":"Folder not found"} */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Object codeO = entries.get("code");
        if (codeO != null) {
            int code = ((Integer) ReflectionUtils.cast(codeO, Integer.class)).intValue();
            if (code == 506) {
                /* Offline folder */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            /*
             * 2025-11-28: Looks like password protected folders do not exist anymore at least I was unable to set a password on self owned
             * folders.
             */
            if (code == 201) {
                /* Password protected folder */
                int attempt = 0;
                while (code == 201 && attempt < 3) {
                    attempt++;
                    final String passCode = getUserInput("Password?", param);
                    br.getPage("/api/folder/" + folderID + "?password=" + Encoding.urlEncode(passCode));
                    entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    code = ((Integer) ReflectionUtils.cast(entries.get("code"), Integer.class)).intValue();
                    if (code == 201) {
                        logger.info("Wrong password: " + passCode);
                    }
                }
                if (code == 201) {
                    throw new DecrypterException(DecrypterException.PASSWORD);
                }
            }
        }
        String title = (String) entries.get("name");
        if (StringUtils.isEmpty(title)) {
            /* Fallback */
            title = folderID;
        }
        final List<Map<String, Object>> files = (List<Map<String, Object>>) entries.get("files");
        if (files == null || files.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER, title);
        }
        for (final Map<String, Object> file : files) {
            // file.get("premiumOnly");
            final String file_id = file.get("hash").toString();
            final DownloadLink link = createDownloadlink("https://" + getHost() + "/get/" + file_id);
            link.setFinalFileName(file.get("name").toString());
            link.setVerifiedFileSize(((Long) ReflectionUtils.cast(file.get("size"), Long.class)).longValue());
            link.setAvailable(true);
            ret.add(link);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        fp.setPackageKey("filer.net://folder/" + folderID);
        fp.addLinks(ret);
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}