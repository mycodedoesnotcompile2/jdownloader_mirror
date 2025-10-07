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
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperCrawlerPluginCloudflareTurnstile;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 51616 $", interfaceVersion = 3, names = {}, urls = {})
public class AbdlSt extends PluginForDecrypt {
    public AbdlSt(PluginWrapper wrapper) {
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
        ret.add(new String[] { "abdl.st" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/video/([\\w-]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        final String content_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /** 2025-10-06: See https://abdl.st/_next/static/chunks/app/video/%5Bslug%5D/page-903eb0127bcdb396.js */
        final String cfSiteKey = "0x4AAAAAABgccoPrNI6Z55n_";
        final String cfTurnstileResponse = new CaptchaHelperCrawlerPluginCloudflareTurnstile(this, br, cfSiteKey).getToken();
        br.getHeaders().put("Accept", "text/x-component");
        br.getHeaders().put("content-type", "text/plain;charset=UTF-8");
        br.getHeaders().put("Origin", "https://" + br.getHost());
        br.getHeaders().put("next-action", "4051b516cd96ea16b05c1d2b0290f7d0ccf584856c");
        final Form form = new Form();
        form.setAction(br.getURL());
        form.setMethod(MethodType.POST);
        form.put("1_turnstile-token", Encoding.urlEncode(cfTurnstileResponse));
        form.put("1_video-slug", Encoding.urlEncode(content_id));
        form.put("0", Encoding.urlEncode("[\"$K1\"]"));
        br.submitForm(form);
        /* Process Next.js output. */
        String[] lines = br.getRequest().getHtmlCode().split("\\r?\\n");
        Map<String, Object> entries = null;
        for (String line : lines) {
            line = line.replaceFirst("^\\d+:", "");
            final Map<String, Object> data = restoreFromString(line, TypeRef.MAP);
            if (data.containsKey("success")) {
                entries = data;
                break;
            }
        }
        final String finallink = entries.get("k2sLink").toString();
        if (finallink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        ret.add(createDownloadlink(finallink));
        return ret;
    }
}
