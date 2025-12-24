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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52046 $", interfaceVersion = 3, names = {}, urls = {})
public class DropmbComFolder extends PluginForDecrypt {
    public DropmbComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "dropmb.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/share/([a-zA-Z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    private String getApiBase() {
        return "https://" + getHost() + "/api";
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String folder_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        final Browser brc = br.cloneBrowser();
        String passCode = this.getTokenWithPassword(brc, folder_id, param);
        brc.getPage(getApiBase() + "/shares/" + folder_id);
        if (brc.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final String title = (String) entries.get("name");
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(title);
        }
        fp.setComment((String) entries.get("description"));
        fp.setPackageKey("dropmb://share/" + folder_id);
        final List<Map<String, Object>> files = (List<Map<String, Object>>) entries.get("files");
        if (files.isEmpty()) {
            /* 2025-12-23: Not sure if this is even possible. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        for (final Map<String, Object> file : files) {
            final String file_id = file.get("id").toString();
            final Number size = (Number) ReflectionUtils.cast(file.get("size"), Long.class);
            final String direct_download_url = getApiBase() + "/shares/" + folder_id + "/files/" + file_id;
            final DownloadLink link = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(direct_download_url));
            if (size != null) {
                link.setVerifiedFileSize(size.longValue());
            }
            link.setFinalFileName(file.get("name").toString());
            if (passCode != null) {
                link.setDownloadPassword(passCode);
            }
            link.setAvailable(true);
            link._setFilePackage(fp);
            ret.add(link);
        }
        return ret;
    }

    private String getTokenWithPassword(final Browser brc, final String folder_id, final CryptedLink param) throws Exception {
        String passCode = param.getDecrypterPassword();
        for (int i = 0; i < 3; i++) {
            if (i > 0) {
                passCode = getUserInput("Password?", param);
            }
            final Map<String, Object> postdata = new HashMap<String, Object>();
            if (passCode != null) {
                postdata.put("password", passCode);
            }
            brc.postPageRaw(getApiBase() + "/shares/" + folder_id + "/token", JSonStorage.serializeToJson(postdata));
            /* Token is also set as cookie so we do not need to store/evaluate it here. */
            // final Map<String, Object> tokenresp = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            // final String token = tokenresp.get("token").toString();
            final int responseCode = brc.getHttpConnection().getResponseCode();
            if (responseCode == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            if (responseCode != 403) {
                return passCode;
            }
            if (this.isAbort()) {
                break;
            }
        }
        throw new DecrypterException(DecrypterException.PASSWORD);
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
