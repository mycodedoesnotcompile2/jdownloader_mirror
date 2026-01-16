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

import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.KsharedCom;

@DecrypterPlugin(revision = "$Revision: 52102 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { KsharedCom.class })
public class KsharedComFolder extends PluginForDecrypt {
    public KsharedComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return KsharedCom.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/folder/([A-Za-z0-9]+)(/([^/]+))?");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*The file you are trying to download is no longer|>\\s*This could be due to the following reasons")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Regex urlinfo = new Regex(param.getCryptedUrl(), this.getSupportedLinks());
        final String folderID = urlinfo.getMatch(0);
        final String titleFromURL = urlinfo.getMatch(2);
        String title = br.getRegex("<h2 class=\"name wordwrap s18 b font\"[^>]*>([^<]+)</h2>").getMatch(0);
        if (title == null && titleFromURL != null) {
            title = titleFromURL.replace("-", " ").trim();
        }
        final KsharedCom hosterplugin = (KsharedCom) this.getNewPluginForHostInstance(this.getHost());
        final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            fp.setName(title);
        } else {
            /* Fallback */
            fp.setName(folderID);
        }
        fp.setPackageKey(this.getHost() + "://folder/" + folderID);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        for (final String url : urls) {
            if (!hosterplugin.canHandle(url)) {
                /* Skip URLs we can't deal with. */
                continue;
            }
            final DownloadLink link = this.createDownloadlink(url);
            final String fileID = hosterplugin.getFID(link);
            final Regex extraInfo = br.getRegex("file-name-" + fileID + "\">([^<]+)</h2>\\s*<h2[^>]*>([^<]+)</h2>");
            if (extraInfo.patternFind()) {
                final String filename = extraInfo.getMatch(0);
                final String filesize = extraInfo.getMatch(1);
                link.setName(Encoding.htmlDecode(filename).trim());
                link.setDownloadSize(SizeFormatter.getSize(filesize));
            }
            link.setAvailable(true);
            link._setFilePackage(fp);
            ret.add(link);
        }
        if (ret.isEmpty()) {
            if (br.containsHTML("0 files")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Empty folder");
            } else if (!br.containsHTML("class=\"folder-view")) {
                /**
                 * 2026-01-15: offline folder with empty page -> No error message but also no file links <br>
                 * Exsample folder ID: RdjObNpQLe
                 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        return ret;
    }
}
