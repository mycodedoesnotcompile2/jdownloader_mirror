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

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.components.config.HideCxConfig;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;

@DecrypterPlugin(revision = "$Revision: 51096 $", interfaceVersion = 3, names = {}, urls = {})
public class HideCx extends PluginForDecrypt {
    public HideCx(PluginWrapper wrapper) {
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
        ret.add(new String[] { "hide.cx" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/container/([a-f0-9-]{32,36})");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contentID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        if (!contentID.replace("-", "").matches("[a-f0-9]{32}")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Invalid format");
        }
        final HideCxConfig cfg = PluginJsonConfig.get(HideCxConfig.class);
        final String apikey = cfg.getAPIKey();
        if (apikey != null) {
            br.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + apikey);
        }
        // br.getHeaders().put("Content-Type", "application/json");
        // br.getHeaders().put("Origin", "https://hide.cx");
        // br.getHeaders().put("Referer", "https://hide.cx/");
        /* Important, else we will get http response 404 */
        br.getHeaders().put("Accept", "application/json, text/plain, */*");
        final String api_base = "https://api.hide.cx";
        /* API docs: https://hide.cx/settings?tab=api */
        Map<String, Object> entries = null;
        String passCode = param.getDecrypterPassword();
        boolean passwordSuccess = false;
        passwordLoop: for (int i = 0; i <= 3; i++) {
            if (i > 0 || passCode != null) {
                if (i > 0) {
                    passCode = getUserInput("Password?", param);
                }
                br.postPageRaw(api_base + "/containers/" + contentID + "/unlock", "{\"password\":\"" + PluginJSonUtils.escape(passCode) + "\"}");
                if (br.getHttpConnection().getResponseCode() == 403) {
                    /* {"error":"no permission password is required"} */
                    logger.info("Wrong password or password required");
                    continue;
                }
                entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                passwordSuccess = true;
                break passwordLoop;
            }
            br.getPage(api_base + "/containers/" + contentID);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            if (br.getHttpConnection().getResponseCode() == 403) {
                /* {"error":"no permission password is required"} */
                logger.info("Wrong password or password required");
                continue;
            }
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            passwordSuccess = true;
            break passwordLoop;
        }
        if (!passwordSuccess) {
            throw new DecrypterException(DecrypterException.PASSWORD);
        }
        if (passCode != null) {
            logger.info("User entered correct password: " + passCode);
        }
        final String title = (String) entries.get("name");
        final FilePackage fp = FilePackage.getInstance();
        if (!StringUtils.isEmpty(title)) {
            fp.setName(title);
        } else {
            /* Fallback */
            fp.setName(contentID);
        }
        fp.setPackageKey("hide.cx//container/" + contentID);
        final List<Map<String, Object>> downloads = (List<Map<String, Object>>) entries.get("links");
        int progr = 1;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        for (final Map<String, Object> download : downloads) {
            final String download_id = download.get("id").toString();
            logger.info("Crawling item " + progr + "/" + downloads.size() + " | ID: " + download_id);
            /* "Real" filename (not always provided) */
            final String filename = (String) download.get("name");
            /* "Weak" filename (sometimes this can be the URLs' content_id and not a filename at all) */
            final String filename_fallback = (String) download.get("file");
            final Number link_size = (Number) download.get("link_size");
            String url = (String) download.get("hoster_url");
            if (url == null) {
                /* Separate http request needed to fetch url */
                br.getPage("/containers/" + contentID + "/links/" + download_id);
                final Map<String, Object> linkresponse = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                url = linkresponse.get("url").toString();
            }
            final DownloadLink link = this.createDownloadlink(url);
            if (!StringUtils.isEmpty(filename)) {
                link.setName(filename);
            } else if (!StringUtils.isEmpty(filename_fallback)) {
                link.setName(filename_fallback);
            }
            if (link_size != null) {
                link.setDownloadSize(link_size.longValue());
            }
            if (download.get("link_status").toString().equalsIgnoreCase("online")) {
                link.setAvailable(true);
            }
            link._setFilePackage(fp);
            ret.add(link);
            distribute(link);
            progr++;
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return ret;
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return HideCxConfig.class;
    }
}
