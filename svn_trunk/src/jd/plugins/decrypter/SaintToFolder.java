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

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.SaintTo;

@DecrypterPlugin(revision = "$Revision: 51302 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { SaintTo.class })
public class SaintToFolder extends PluginForDecrypt {
    public SaintToFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return SaintTo.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/a/([a-zA-Z0-9]{5,})");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String folderID = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Album is private, removed")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("<title>([^<]+) - Saint Video Hosting</title>").getMatch(0);
        if (title == null) {
            /* Fallback */
            logger.warning("Failed to find folder title");
            title = folderID;
        }
        title = Encoding.htmlDecode(title).trim();
        final String[] htmls = br.getRegex("<tr class=\"file-list__file\">(.*?)</tr>").getColumn(0);
        if (htmls == null || htmls.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        for (final String html : htmls) {
            final String url = new Regex(html, "file_dwn\\('(https?://[^']+)'\\)").getMatch(0);
            final String filename = new Regex(html, "class=\"filename\"[^>]*>([^<]+)</a>").getMatch(0);
            final String filesizeBytesStr = new Regex(html, "class=\"fs\" data=\"(\\d+)\"").getMatch(0);
            if (url == null || filename == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final DownloadLink file = createDownloadlink(url);
            file.setName(Encoding.htmlDecode(filename).trim());
            if (filesizeBytesStr != null) {
                file.setVerifiedFileSize(Long.parseLong(filesizeBytesStr));
            } else {
                logger.warning("Failed to find filesize for item: " + url);
            }
            file.setAvailable(true);
            ret.add(file);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        fp.setPackageKey(this.getHost() + "://folder/" + folderID);
        fp.addLinks(ret);
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
