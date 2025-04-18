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

import org.appwork.utils.DebugMode;
import org.appwork.utils.parser.UrlQuery;

import com.formdev.flatlaf.util.StringUtils;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.TorboxApp;

@DecrypterPlugin(revision = "$Revision: 50982 $", interfaceVersion = 3, names = {}, urls = {})
public class TorboxAppCrawler extends PluginForDecrypt {
    public TorboxAppCrawler(PluginWrapper wrapper) {
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
        ret.add(new String[] { "torbox.app" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/download\\?id=\\d+&type=.+");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account == null) {
            /* Account required to access any torbox.app content */
            throw new AccountRequiredException();
        }
        final TorboxApp plugin = (TorboxApp) this.getNewPluginForHostInstance(this.getHost());
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* Unfinished plugin */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        // TODO: Add functionality
        final UrlQuery query = UrlQuery.parse(param.getCryptedUrl());
        final String dl_id = query.get("id");
        final String dl_type = query.get("type");
        final String dl_name = query.get("name");
        List<Map<String, Object>> files = null;
        Map<String, Object> data = null;
        if (dl_type.equalsIgnoreCase("torrents")) {
            final Request req = br.createGetRequest(TorboxApp.API_BASE + "/torrents/mylist?" + query.toString());
            data = (Map<String, Object>) plugin.callAPI(br, req, account, null);
        } else if (dl_type.equalsIgnoreCase("web_downloads")) {
        } else if (dl_type.equalsIgnoreCase("usenet_downloads")) {
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported type");
        }
        files = (List<Map<String, Object>>) data.get("files");
        for (final Map<String, Object> file : files) {
            final DownloadLink link = this.createDownloadlink("https://dummy.link.todo/" + file.get("name") + ".jdeatme");
            link.setFinalFileName(file.get("name").toString());
            link.setVerifiedFileSize(((Number) file.get("size")).longValue());
            link.setAvailable(true);
            if (Boolean.TRUE.equals(file.get("infected"))) {
                // TODO: Maybe store this information and avoid download of infected files
            }
            ret.add(link);
        }
        final String nameFromJson = (String) data.get("name");
        final FilePackage fp = FilePackage.getInstance();
        if (!StringUtils.isEmpty(dl_name)) {
            fp.setName(Encoding.htmlDecode(dl_name).trim());
        } else if (!StringUtils.isEmpty(nameFromJson)) {
            /* Fallback 1 */
            fp.setName(nameFromJson);
        } else {
            /* Fallback 2 */
            fp.setName(dl_id);
        }
        fp.addLinks(ret);
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
