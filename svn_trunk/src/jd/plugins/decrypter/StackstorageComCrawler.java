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
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.StackstorageCom;

@DecrypterPlugin(revision = "$Revision: 52341 $", interfaceVersion = 3, names = { "stackstorage.com" }, urls = { "https?://([\\w\\-]+)\\.stackstorage\\.com/s/([A-Za-z0-9]+)(\\?dir=([^\\&]+)\\&node\\-id=(\\d+))?" })
public class StackstorageComCrawler extends PluginForDecrypt {
    public StackstorageComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.setFollowRedirects(true);
        br.setAllowedResponseCodes(412, 503);
        final Regex urlinfo = new Regex(contenturl, this.getSupportedLinks());
        final String subdomain = urlinfo.getMatch(0);
        final String folderID = urlinfo.getMatch(1);
        String subdir = urlinfo.getMatch(3);
        if (subdir == null) {
            /* Start from root */
            subdir = "%2F";
        }
        final PluginForHost hosterplugin = this.getNewPluginForHostInstance(this.getHost());
        final String apiurl = "https://" + subdomain + ".stackstorage.com/api/v2/share/" + folderID;
        /* Obtain token / cookie */
        br.postPageRaw(apiurl, "{\"password\":\"\"}");
        if (br.getHttpConnection().getResponseCode() == 401) {
            logger.info("Password protected URLs are not yet supported");
            ret.add(this.createOfflinelink(contenturl, "PASSWORD_PROTECTED"));
            return ret;
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Item offline or account required to access it");
        } else if (br.getHttpConnection().getResponseCode() == 412) {
            throw new AccountRequiredException();
        } else if (br.getHttpConnection().getResponseCode() == 503) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Error 503 offline");
        }
        final String sharetoken = br.getRequest().getResponseHeader("X-Sharetoken");
        final String csrftoken = br.getRequest().getResponseHeader("X-Csrf-Token");
        if (StringUtils.isEmpty(sharetoken) || StringUtils.isEmpty(csrftoken)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.setCookie(br.getURL(), "ShareSession", sharetoken);
        br.getHeaders().put("X-Sharetoken", sharetoken);
        br.getPage(apiurl);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final String filename = entries.get("name").toString();
        final String file_id = entries.get("id").toString();
        final DownloadLink file = createDownloadlink(String.format("https://stackstorage.com/fileid/%s", file_id));
        file.setDefaultPlugin(hosterplugin);
        file.setHost(this.getHost());
        file.setFinalFileName(filename);
        file.setVerifiedFileSize(((Number) entries.get("size")).longValue());
        file.setAvailable(true);
        file.setContainerUrl(param.getCryptedUrl());
        file.setProperty(StackstorageCom.PROPERTY_SUBDOMAIN, subdomain);
        file.setProperty(StackstorageCom.PROPERTY_FOLDER_ID, folderID);
        file.setProperty(StackstorageCom.PROPERTY_SHARETOKEN, sharetoken);
        file.setProperty(StackstorageCom.PROPERTY_CSRFTOKEN, csrftoken);
        file.setProperty(StackstorageCom.PROPERTY_FILENAME, filename);
        file.setProperty(StackstorageCom.PROPERTY_FILE_ID, file_id);
        ret.add(file);
        return ret;
    }
}
