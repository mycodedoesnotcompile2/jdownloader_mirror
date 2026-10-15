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
import java.util.UUID;
import java.util.regex.Pattern;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 53621 $", interfaceVersion = 3, names = {}, urls = {})
public class YumpuCom extends PluginForDecrypt {
    private static final String API_BASE = "https://api.yumpu.news/1.0";

    public YumpuCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "yumpu.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern TYPE_OLD = Pattern.compile("/([a-z]{2})/document/read/(\\d+)/([a-z0-9\\-]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_NEW = Pattern.compile("/([\\w\\-]+)/([a-z]{2})/issue/(\\d+)-([\\w\\-]+)(/read)?", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_OLD.pattern() + "|" + TYPE_NEW.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* The website sets a guest ("candyland") token cookie which is used as Bearer token for the API. */
        final String authtoken = br.getCookie(br.getHost(), "yn_caT", Cookies.NOTDELETEDPATTERN);
        if (StringUtils.isEmpty(authtoken)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String fid = new Regex(param.getCryptedUrl(), TYPE_NEW).getMatch(2);
        if (fid == null) {
            fid = new Regex(param.getCryptedUrl(), TYPE_OLD).getMatch(1);
        }
        final String url_name = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(1);
        br.getHeaders().put("Authorization", "Bearer " + authtoken);
        /* The guest token must be "connected" with a device fingerprint, otherwise the API answers with 403 "Not authenticated". */
        final Map<String, Object> connect = new HashMap<String, Object>();
        connect.put("fp_token", UUID.randomUUID().toString().replace("-", ""));
        connect.put("country", "DE");
        br.getHeaders().put("Content-Type", "application/json");
        br.postPageRaw(API_BASE + "/auth/access/connect", JSonStorage.serializeToJson(connect));
        br.getHeaders().remove("Content-Type");
        br.getPage(API_BASE + "/documents/" + fid + "/details");
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!Boolean.TRUE.equals(entries.get("success"))) {
            /* E.g. {"success":false,"message":"Not authenticated","code":"403U0600"} */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, String.valueOf(entries.get("message")));
        }
        final Map<String, Object> document = (Map<String, Object>) entries.get("data");
        final Map<String, Object> details = (Map<String, Object>) document.get("details");
        final String description = (String) document.get("description");
        String fpName = (String) document.get("title");
        if (StringUtils.isEmpty(fpName)) {
            /* Fallback */
            fpName = url_name;
        }
        /* Issue titles can be just a date -> prepend name of the magazine. */
        final Map<String, Object> group = (Map<String, Object>) document.get("group");
        final String groupName = group != null ? (String) group.get("name") : null;
        if (!StringUtils.isEmpty(groupName) && !fpName.contains(groupName)) {
            fpName = groupName + " - " + fpName;
        }
        /* Only the "large" variant is delivered with signed URLs. */
        final String base_path = (String) details.get("base_path");
        final String directory = (String) ((Map<String, Object>) details.get("directories")).get("large");
        final List<String> signedImages = (List<String>) ((Map<String, Object>) details.get("signed_images")).get("large");
        if (signedImages.size() == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final boolean setComment = !StringUtils.isEmpty(description) && !description.equalsIgnoreCase(fpName);
        final int padLength = String.valueOf(signedImages.size()).length();
        for (int i = 0; i < signedImages.size(); i++) {
            final String directurl = base_path + "/" + directory + "/" + signedImages.get(i);
            final String filename = StringUtils.fillPre(Integer.toString(i + 1), "0", padLength) + "_" + fpName + ".jpg";
            final DownloadLink dl = createDownloadlink(DirectHTTP.createURLForThisPlugin(directurl));
            dl.setFinalFileName(filename);
            dl.setAvailable(true);
            if (setComment) {
                dl.setComment(description);
            }
            ret.add(dl);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(Encoding.htmlDecode(fpName.trim()));
        fp.addLinks(ret);
        return ret;
    }
}
