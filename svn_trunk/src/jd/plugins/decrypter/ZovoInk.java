//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
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

import org.appwork.storage.TypeRef;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52867 $", interfaceVersion = 3, names = {}, urls = {})
public class ZovoInk extends PluginForDecrypt {
    public ZovoInk(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "go.zovo.ink", "zovo2.top" });
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
            ret.add("https?://" + buildHostsPatternPart(domains) + "/[a-zA-Z0-9]{5,}");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        br.setFollowRedirects(false);
        br.getPage(param.getCryptedUrl().replaceFirst("http://", "https://"));
        if (br.getRedirectLocation() != null && br.getRedirectLocation().contains("//recaptcha.cloud/")) {
            /* zovo2.top usually ends in this except if you have gone through the captcha process (cloudflare turnstile) on the same IP */
            throw new PluginException(LinkStatus.ERROR_CAPTCHA);
        }
        br.setFollowRedirects(true);
        Form form1 = br.getFormbyProperty("id", "form-continue");
        br.submitForm(form1);
        long wait = Integer.parseInt(br.getRegex("\"counter_value\":(\\d+),").getMatch(0));
        if (wait > 0) {
            sleep(wait * 1000l - br.getRequest().getReadTime(), param);
        }
        Form form2 = br.getFormbyAction("/links/go");
        br.setCookie(br.getURL(), "ab", "2");
        br.setHeader("X-Requested-With", "XMLHttpRequest");
        br.submitForm(form2);
        final Map<String, Object> go = (Map<String, Object>) restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        long code = -1;
        if (go.get("code") != null) {
            if (go.get("message") != null) {
                logger.info(go.get("message").toString());
            }
            code = ((Number) go.get("code")).longValue();
        }
        if (code == 400) {
            throw new PluginException(LinkStatus.ERROR_RETRY);
        }
        if (code == 403 || go.get("status") == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (go.get("status").toString() == "error") {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        ret.add(createDownloadlink(go.get("url").toString()));
        return ret;
    }
}
