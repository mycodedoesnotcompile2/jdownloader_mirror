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

import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.SendCm;

@DecrypterPlugin(revision = "$Revision: 51367 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { SendCm.class })
public class SendCmFolder extends PluginForDecrypt {
    public SendCmFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return SendCm.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(e|s)/(.+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl().replace("/e/", "/s/");
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Files not found")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        int page = 1;
        final Regex urlinfo = new Regex(param.getCryptedUrl(), this.getSupportedLinks());
        final String folderTitle = urlinfo.getMatch(1);
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(Encoding.htmlDecode(folderTitle).trim());
        final HashSet<String> dupes = new HashSet<String>();
        pagination: do {
            final String[] links = br.getRegex("(/[a-z0-9]{12})").getColumn(0);
            if (links == null || links.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String[] filenames = br.getRegex("class=\"tx-dark\"[^>]*>([^<]+)</a>").getColumn(0);
            final String[] filesizes = br.getRegex("class=\"label label-success\\s*\"[^>]*>([^<]+)</span>").getColumn(0);
            int index = 0;
            int newItemsThisPage = 0;
            for (String url : links) {
                if (!dupes.add(url)) {
                    continue;
                }
                newItemsThisPage++;
                url = br.getURL(url).toExternalForm();
                final DownloadLink link = createDownloadlink(url);
                if (filesizes != null && filesizes.length == links.length) {
                    final String filesizeStr = filesizes[index];
                    link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                }
                if (filenames != null && filenames.length == links.length) {
                    String filename = filenames[index];
                    filename = Encoding.htmlDecode(filename).trim();
                    link.setName(filename);
                }
                link.setAvailable(true);
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
                index++;
            }
            if (newItemsThisPage == 0) {
                logger.info("Stopping because: Failed to find any new items on current page");
                break pagination;
            }
            page++;
            final String nextPageUrl = br.getRegex("<a class\\s*=\\s*(\"|')page-link\\1[^>]*href\\s*=\\s*('|\")(/\\?[^\"']*op=user_public[^\"']*page=" + page + ")").getMatch(2);
            if (nextPageUrl == null) {
                logger.info("Stopping because: Reached end?");
                break pagination;
            }
            br.getPage(nextPageUrl);
        } while (!this.isAbort());
        return ret;
    }
}
