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

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.components.SiteType.SiteTemplate;

import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperCrawlerPluginCloudflareTurnstile;
import org.jdownloader.plugins.components.antiDDoSForDecrypt;

@DecrypterPlugin(revision = "$Revision: 53511 $", interfaceVersion = 3, names = { "tr.link", "ay.link" }, urls = { "https?://(?:www\\.)?tr\\.link/(?!dmca|skype|webroot)([A-Za-z0-9]+)", "https?://(?:www\\.)?(?:ay\\.link|aylink\\.co)/(?!dmca|skype|webroot)([A-Za-z0-9]+)" })
public class TrLink extends antiDDoSForDecrypt {
    public TrLink(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> decryptedLinks = new ArrayList<DownloadLink>();
        br.setFollowRedirects(false);
        br.getPage(param.getCryptedUrl());
        int loop = 0;
        while (br.getRedirectLocation() != null && loop <= 2) {
            if (!this.canHandle(br.getRedirectLocation())) {
                /* Direct redirect to external website */
                decryptedLinks.add(createDownloadlink(br.getRedirectLocation()));
                return decryptedLinks;
            }
            br.followRedirect();
            loop++;
        }
        if (br.getRedirectLocation() != null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Too many redirects");
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Form turnstileForm = br.getFormByRegex("id=\"turnstile-form\"");
        if (turnstileForm != null) {
            final CaptchaHelperCrawlerPluginCloudflareTurnstile turnStile = new CaptchaHelperCrawlerPluginCloudflareTurnstile(this, br);
            turnstileForm.put("cf-turnstile-response", turnStile.getToken());
            br.submitForm(turnstileForm);
        }
        final String alias = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        final String csrf = br.getRegex("app\\['csrf'\\] = '([^<>\"\\']+)';").getMatch(0);
        final String token = br.getRegex("app\\['token'\\] = '([^<>\"\\']+)';").getMatch(0);
        final String destContent = br.getRegex("<div class\\s*=\\s*\"destination-content\">\\s*(.*?)\\s*</div>").getMatch(0);
        if (csrf == null || token == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final UrlQuery query = new UrlQuery();
        query.add("alias", alias);
        query.add("csrf", Encoding.urlEncode(csrf));
        query.add("token", token);
        query.add("_", System.currentTimeMillis() + "");
        br.getHeaders().put("accept", "application/json, text/javascript, */*; q=0.01");
        br.getHeaders().put("x-requested-with", "XMLHttpRequest");
        /* 2020-06-30: Waittime is skippable */
        // this.sleep(5 * 1001l, param);
        getPage("/links/go2?" + query.toString());
        String finallink = PluginJSonUtils.getJson(br, "url");
        if (StringUtils.isEmpty(finallink)) {
            if (destContent != null) {
                final String links[] = HTMLParser.getHttpLinks(destContent, br.getURL());
                if (links != null) {
                    for (String link : links) {
                        decryptedLinks.add(createDownloadlink(link));
                    }
                }
                if (decryptedLinks.size() > 0) {
                    return decryptedLinks;
                }
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (Browser.getHost(finallink).equals(br.getHost())) {
            logger.info("Looking for internal redirect");
            br.setFollowRedirects(false);
            getPage(finallink);
            finallink = br.getRedirectLocation();
            if (finallink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        decryptedLinks.add(createDownloadlink(finallink));
        return decryptedLinks;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.MightyScript_AdLinkFly;
    }
}
