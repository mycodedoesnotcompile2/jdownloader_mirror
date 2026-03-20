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

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickedPoint;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52533 $", interfaceVersion = 3, names = {}, urls = {})
public class LocknexCom extends PluginForDecrypt {
    public LocknexCom(PluginWrapper wrapper) {
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
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "locknex.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/container/(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        /* They got an API but it can only be used with a personal API key, see: https://locknex.com/api/index */
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Form captcha_or_password_form = br.getForm(0);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            // TODO: Test this with a link where we know the correct password
            if (captcha_or_password_form != null) {
                if (captcha_or_password_form.hasInputFieldByName("password")) {
                    String passCode = param.getDecrypterPassword();
                    if (passCode == null) {
                        /* Ask user */
                        passCode = getUserInput("Password?", param);
                    }
                    captcha_or_password_form.put("password", Encoding.urlEncode(passCode));
                }
                if (captcha_or_password_form.hasInputFieldByName("cc_x")) {
                    final String captcha_url = br.getRegex("(/captcha_circle\\.php\\?ts=\\d+)").getMatch(0);
                    if (captcha_url == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    final ClickedPoint cp = getCaptchaClickedPoint(getHost(), getCaptchaImage(captcha_url), param, "Click in the circle");
                    captcha_or_password_form.put("cc_x", Integer.toString(cp.getX()));
                    captcha_or_password_form.put("cc_y", Integer.toString(cp.getY()));
                }
                br.submitForm(captcha_or_password_form);
                if (br.containsHTML(">\\s*Passwort ist falsch")) {
                    throw new DecrypterException(DecrypterException.PASSWORD);
                }
                if (br.containsHTML(">\\s*Captcha-Klick war falsch")) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                }
            }
        }
        String title = br.getRegex("class=\"fa-duotone fa-folder-open\"[^>]*>\\s*</i>([^<]+)<span").getMatch(0);
        String urls_json = br.getRegex("window\\.CNL_LINKS = (\\[[^\\]]+\\])").getMatch(0);
        if (urls_json == null) {
            if (captcha_or_password_form != null && captcha_or_password_form.hasInputFieldByName("password")) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Password protected links aren't supported yet");
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final List<String> links = (List<String>) restoreFromString(urls_json, TypeRef.OBJECT);
        if (links == null || links.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        for (final String singleLink : links) {
            ret.add(createDownloadlink(singleLink));
        }
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(Encoding.htmlDecode(title).trim());
        }
        fp.addLinks(ret);
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
