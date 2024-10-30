//jDownloader - Downloadmanager
//Copyright (C) 2014  JD-Team support@jdownloader.org
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

import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.nutils.encoding.HTMLEntities;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.SiteType.SiteTemplate;

/**
 * This class supports, http://mfscripts.com/short_url_script.html template.
 *
 * @author raztoki
 */
@DecrypterPlugin(revision = "$Revision: 50040 $", interfaceVersion = 3, names = { "gourl.us" }, urls = { "https?://(?:www\\.)?gourl\\.us/([a-zA-Z0-9_\\-]+)$" })
public class MFS_ShortUrlScript extends PluginForDecrypt {
    public MFS_ShortUrlScript(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(false);
        return br;
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String parameter = param.getCryptedUrl().replaceFirst("^http://", "https://");
        // final String fuid = new Regex(parameter, this.getSupportedLinks()).getMatch(0);
        br.getPage(parameter);
        final String redirect = br.getRedirectLocation();
        if (redirect != null && !this.canHandle(redirect) && !redirect.contains(this.getHost() + "/")) {
            logger.info("Found direct redirect to final link: " + redirect);
            ret.add(createDownloadlink(redirect));
            return ret;
        }
        br.setFollowRedirects(true);
        if (redirect != null) {
            br.followRedirect(true);
        }
        final String errorFromURL = UrlQuery.parse(br.getURL()).get("e");
        if (errorFromURL != null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final int repeat = 3;
        Form pwform = getPasswordForm(br);
        if (pwform != null) {
            for (int i = 0; i <= repeat; i++) {
                final String pass = getUserInput("Password protected link", param);
                pwform.put("accessPass", Encoding.urlEncode(pass));
                br.submitForm(pwform);
                pwform = getPasswordForm(br);
                if (pwform == null) {
                    logger.info("User has entered correct password: " + pass);
                    break;
                }
                if (i + 1 >= repeat) {
                    logger.warning("Incorrect solve of password");
                    throw new DecrypterException(DecrypterException.PASSWORD);
                }
                /* Try again */
                continue;
            }
        }
        final String frame = br.getRegex("<frame [^>]*src=\"(interstitualAdTop\\.php\\?url=\\d+)\"").getMatch(0);
        if (frame == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(frame);
        String link = br.getRegex("<a [^>]*[^\\S]*href=\"(.*?)\" class=[^>]+>skip advert\\s*</a>").getMatch(0);
        if (link == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        ret.add(createDownloadlink(HTMLEntities.unhtmlentities(link)));
        return ret;
    }

    private Form getPasswordForm(final Browser br) {
        return br.getFormbyKey("accessPass");
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.MFScripts_Wurlie;
    }
}