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

import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.WrzutaNet;

@DecrypterPlugin(revision = "$Revision: 51521 $", interfaceVersion = 3, names = {}, urls = {})
public class WrzutaNetFolder extends PluginForDecrypt {
    public WrzutaNetFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        return WrzutaNet.getPluginDomains();
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/folder/([A-Z0-9]{12})/?");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String this_folder_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Nie ma takiego foldera")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Brak plików do wyświetlenia")) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        final HashSet<String> dupes = new HashSet<String>();
        /* Add folder_id of current folder to prevent returning it again down below. */
        dupes.add(this_folder_id);
        /**
         * Website contains two folder tables: <br>
         * 1. Subfolders. <br>
         * 2. Files.
         */
        String title = br.getRegex("<h5 class=\"mb-0\">([^<]+)</h5>").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            title = title.replaceFirst("Folder – ", "");
            fp.setName(title);
        } else {
            logger.warning("Failed to find folder title");
            fp.setName(this_folder_id);
        }
        fp.setPackageKey(this.getHost() + "://folder/" + this_folder_id);
        /**
         * TODO: Check if the folders listed on the website on the left side are part of the current folders slash subfolders of it. <br>
         * If that is the case, we should crawl them too.
         */
        final String[] table_htmls = br.getRegex("<tbody class=\"table-border-bottom-0\">(.*?)</tbody>").getColumn(0);
        for (final String table_html : table_htmls) {
            final String[] htmls = new Regex(table_html, "<tr>\\s*<td>(.*?)</td>\\s*</tr>").getColumn(0);
            for (final String html : htmls) {
                final Regex fileregex = new Regex(html, "/file/([A-Za-z0-9]{20})(/([^/\"]+))?");
                if (!fileregex.patternFind()) {
                    /* Skip invalid items */
                    continue;
                }
                final String file_id = fileregex.getMatch(0);
                if (!dupes.add(file_id)) {
                    /* Skip duplicates */
                    continue;
                }
                String url = new Regex(html, "/file/([A-Za-z0-9]{20})(/([^/\"]+))?").getMatch(-1);
                url = br.getURL(url).toExternalForm();
                final String filename = Plugin.getFileNameFromURL(new URL(url));
                final String filesize = new Regex(html, "<td>([0-9\\.]+ [^<]+)</td>").getMatch(0);
                final DownloadLink file = this.createDownloadlink(url);
                if (filename != null) {
                    file.setName(Encoding.htmlDecode(filename).trim());
                }
                if (filesize != null) {
                    file.setDownloadSize(SizeFormatter.getSize(filesize));
                }
                file.setAvailable(true);
                file._setFilePackage(fp);
                ret.add(file);
            }
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
