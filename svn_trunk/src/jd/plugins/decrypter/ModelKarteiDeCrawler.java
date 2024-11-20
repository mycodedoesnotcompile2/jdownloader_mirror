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
import java.util.HashSet;
import java.util.List;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.ModelKarteiDe;

@DecrypterPlugin(revision = "$Revision: 50175 $", interfaceVersion = 3, names = {}, urls = {})
public class ModelKarteiDeCrawler extends PluginForDecrypt {
    public ModelKarteiDeCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return ModelKarteiDe.getPluginDomains();
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

    private static Pattern TYPE_GALLERY  = Pattern.compile("/(?:photos|fotos)/(?:gallery|galerie)/(\\d+)/?", Pattern.CASE_INSENSITIVE);
    private static Pattern TYPE_SEDCARDS = Pattern.compile("/sedcards/([\\w\\-]+)/(\\d+)/([\\w\\-]+)/?(tagged/?)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_GALLERY.pattern() + "|" + TYPE_SEDCARDS.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        final ModelKarteiDe hosterplugin = (ModelKarteiDe) this.getNewPluginForHostInstance(this.getHost());
        if (account != null) {
            hosterplugin.login(account, false);
        }
        final String contenturl = param.getCryptedUrl();
        hosterplugin.getPageEnsureEnglish(br, contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Sorry, the gallery does not contain any pictures")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Empty gallery");
        } else if (br.containsHTML(">\\s*This sedcard ist set to private")) {
            throw new AccountRequiredException();
        }
        String title = br.getRegex("<title>([^<]+)").getMatch(0);
        final FilePackage photosPackage = FilePackage.getInstance();
        if (title != null) {
            photosPackage.setName(Encoding.htmlDecode(title).trim());
        } else {
            /* Fallback */
            photosPackage.setName(br._getURL().getPath());
        }
        photosPackage.setPackageKey(br.getURL());
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
        final HashSet<String> dupes = new HashSet<String>();
        /* Add items to dupe list which should not be processed at all. */
        dupes.add(br.getURL());
        dupes.add(contenturl);
        final boolean isGallery = new Regex(contenturl, TYPE_GALLERY).patternFind();
        for (final String url : urls) {
            if (!dupes.add(url)) {
                /* Skip dupes */
                continue;
            }
            if (hosterplugin.canHandle(url)) {
                /* Single image/video which will be handled by hoster plugin. */
                final DownloadLink image = this.createDownloadlink(url);
                image.setDefaultPlugin(hosterplugin);
                image.setHost(this.getHost());
                /* Set weak filename */
                hosterplugin.setWeakFilename(image);
                image.setAvailable(true);
                image._setFilePackage(photosPackage);
                ret.add(image);
                continue;
            } else if (!isGallery && new Regex(url, TYPE_GALLERY).patternFind()) {
                /* Collect gallery links if link we are processing is not a gallery link. */
                /* Image gallery -> Goes back into crawler */
                ret.add(this.createDownloadlink(url));
            }
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }
}
