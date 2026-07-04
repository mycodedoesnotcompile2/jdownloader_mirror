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

import org.appwork.utils.Regex;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52949 $", interfaceVersion = 2, names = { "peeplink.in", "alfalink.to" }, urls = { "https?://(?:www\\.)?peeplink\\.in/([a-f0-9]+)", "https?://(?:www\\.)?alfalink\\.(?:info|to)/([a-f0-9]+)" })
public class PrrpLinkIn extends PluginForDecrypt {
    public PrrpLinkIn(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        br.setFollowRedirects(true);
        ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String content_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 403) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.getURL().contains("/" + content_id)) {
            /* Redirect to main page */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Form pwform = this.findPasswordform(br);
        if (pwform != null) {
            final String passCode = getUserInput("Password?", param);
            pwform.put("pwd", Encoding.urlEncode(passCode));
            br.submitForm(pwform);
            if (this.findPasswordform(br) != null) {
                throw new DecrypterException(DecrypterException.PASSWORD);
            }
        }
        // if (br.containsHTML("class=\"QapTcha\"")) {
        // final Browser brc = this.br.cloneBrowser();
        // brc.postPage("/qaptcha/php/Qaptcha.jquery.php", "action=qaptcha");
        // br.postPage(this.br.getURL(), "iQapTcha=");
        // } else if (CaptchaHelperCrawlerPluginHCaptcha.containsHCaptcha(this.br)) {
        // final String hcaptchaResponse = new CaptchaHelperCrawlerPluginHCaptcha(this, br).getToken();
        // br.postPage(br.getURL(), "h-captcha-response=" + Encoding.urlEncode(hcaptchaResponse));
        // }
        String urlText = br.getRegex("<article.*?>(.*?)</article").getMatch(0);
        if (urlText == null) {
            logger.warning("Fallback to scanning complete HTML");
            urlText = this.br.getRequest().getHtmlCode();
        }
        final String[] finallinks = HTMLParser.getHttpLinks(urlText, "");
        for (final String aLink : finallinks) {
            if (!this.canHandle(aLink)) {
                ret.add(createDownloadlink(aLink));
            }
        }
        return ret;
    }

    private Form findPasswordform(final Browser br) {
        final Form[] forms = br.getForms();
        if (forms == null) {
            return null;
        }
        for (Form form : forms) {
            if (form.containsHTML("value=\"Enter Access Password\"")) {
                return form;
            }
        }
        return null;
    }
}
