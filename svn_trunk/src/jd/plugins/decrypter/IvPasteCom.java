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
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 51747 $", interfaceVersion = 3, names = { "ivpaste.com" }, urls = { "https?://(www\\.)?ivpaste\\.com/(v/|view\\.php\\?id=)[A-Za-z0-9]+" })
public class IvPasteCom extends PluginForDecrypt {
    public IvPasteCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final String RECAPTCHAFAILED = "(The reCAPTCHA wasn\\'t entered correctly\\.|Go back and try it again\\.)";

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        br = new Browser();
        final ArrayList<DownloadLink> decryptedLinks = new ArrayList<DownloadLink>();
        final String parameter = param.toString();
        br.getPage(parameter);
        String ID = new Regex(parameter, "ivpaste\\.com/(v/|view\\.php\\?id=)([A-Za-z0-9]+)").getMatch(1);
        if (ID == null) {
            return null;
        }
        br.getPage("https://ivpaste.com/v/" + ID);
        if (br.containsHTML("NO Existe\\!")) {
            logger.info("Link offline: " + parameter);
            return decryptedLinks;
        }
        br.getPage("https://ivpaste.com/p/" + ID);
        if (br.containsHTML("<b>Acceda desde: <a|Error: .*? NO EXISTE")) {
            logger.info("Link offline: " + parameter);
            return decryptedLinks;
        }
        // Avoid unsupported captchatype by reloading the page
        int auto = 0;
        int i = 0;
        while (true) {
            i++;
            final Form form = br.getFormbyActionRegex(".*?/p/" + ID);
            if (form == null) {
                break;
            }
            if (i >= 5) {
                logger.info(i + "/5:Unsupported captchatype: " + parameter);
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
            if (form.containsHTML("pluscaptcha\\.com/") || /* ads captcha */form.containsHTML("api\\.minteye\\.com/|api\\.adscaptcha\\.com/")) {
                logger.info(i + "/5:Unsupported captchatype: " + parameter);
                sleep(1000l, param);
                br.getPage("https://ivpaste.com/p/" + ID);
            } else if (form.containsHTML("class=(\"|')g-recaptcha\\1") && form.containsHTML("google\\.com/recaptcha")) {
                final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br).getToken();
                form.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                br.submitForm(form);
            } else if (br.containsHTML("g-recaptcha")) {
                final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br).getToken();
                form.put("g-recaptcha-response", Encoding.urlEncode(recaptchaV2Response));
                br.submitForm(form);
            } else {
                // this logic is bad, unsupported captcha will result in premature breaking and plugin defect.
                break;
            }
        }
        final String content = br.getRegex("<td nowrap align.*?pre>(.*?)</pre").getMatch(0);
        if (content == null) {
            logger.warning("Decrypter broken for link: " + parameter);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String[] links = new Regex(content, "<a href=\"(.*?)\"").getColumn(0);
        if (links == null || links.length == 0) {
            logger.info("Link offline (found no downloadable links): " + parameter);
            return decryptedLinks;
        }
        for (String dl : links) {
            String ID2 = new Regex(dl, "ivpaste\\.com/(v/|view\\.php\\?id=)([A-Za-z0-9]+)").getMatch(1);
            if (ID.equals(ID2)) {
                continue;
            }
            decryptedLinks.add(createDownloadlink(dl));
        }
        return decryptedLinks;
    }

    /* NO OVERRIDE!! */
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return true;
    }
}