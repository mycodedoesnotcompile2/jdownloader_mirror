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

import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperCrawlerPluginRecaptchaV2;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 45697 $", interfaceVersion = 3, names = { "ally.sh" }, urls = { "https?://(?:www\\.)?(?:al\\.ly|ally\\.sh|dausel\\.co)/[A-Za-z0-9]+" })
public class AllySh extends PluginForDecrypt {
    public AllySh(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> decryptedLinks = new ArrayList<DownloadLink>();
        final String parameter = param.toString();
        br.setFollowRedirects(false);
        br.getPage(parameter);
        if (br.getHttpConnection().getResponseCode() == 404) {
            decryptedLinks.add(this.createOfflinelink(parameter));
            return decryptedLinks;
        }
        final String redirect = br.getRedirectLocation();
        if (redirect != null) {
            if (!this.canHandle(redirect)) {
                /* Direct-redirect */
                decryptedLinks.add(this.createDownloadlink(redirect));
                return decryptedLinks;
            } else {
                br.setFollowRedirects(true);
                br.followRedirect();
            }
        }
        br.setFollowRedirects(false);
        Form continueform = br.getFormbyProperty("id", "form-captcha");
        if (continueform == null) {
            continueform = br.getForm(0);
            if (continueform == null) {
                return null;
            }
        }
        if (continueform.containsHTML("user/login")) {
            return decryptedLinks;
        }
        final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br).getToken();
        continueform.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
        br.submitForm(continueform);
        String finallink = br.getRedirectLocation();
        if (finallink == null) {
            finallink = br.getRegex("attr\\(\"href\",\"(https?://.*?)\"").getMatch(0);
        }
        if (finallink != null) {
            final String url_within_url = new Regex(finallink, "(https?://.*?)https?").getMatch(0);
            if (url_within_url != null) {
                logger.info("Found url within url --> Using this as final url");
                decryptedLinks.add(createDownloadlink(url_within_url));
            }
            decryptedLinks.add(createDownloadlink(finallink));
            return decryptedLinks;
        } else {
            return null;
        }
    }
}
