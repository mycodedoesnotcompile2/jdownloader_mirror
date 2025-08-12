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

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperCrawlerPluginCloudflareTurnstile;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 51314 $", interfaceVersion = 2, names = {}, urls = {})
public class PrimewireTf extends PluginForDecrypt {
    public PrimewireTf(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "primewire.tf" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/links/gos/([a-zA-Z0-9-]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        br.setFollowRedirects(false);
        final String contenturl = param.getCryptedUrl().replaceFirst("(?i)http://", "https://");
        final String content_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(contenturl);
        String redirect = br.getRedirectLocation();
        if (redirect == null) {
            /* Token from: 2025-08-11 */
            logger.info("Captcha required");
            final String cfTurnstileResponse = new CaptchaHelperCrawlerPluginCloudflareTurnstile(this, br, "0x4AAAAAABm1oQ6L99ZEV9RZ").getToken();
            br.getPage("/links/go/" + content_id + "?token=" + Encoding.urlEncode(cfTurnstileResponse));
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            redirect = entries.get("link").toString();
            if (StringUtils.isEmpty(redirect)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        ret.add(this.createDownloadlink(redirect));
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}