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
import java.util.Locale;
import java.util.regex.Pattern;

import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;

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
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.BuzzheavierCom;

@DecrypterPlugin(revision = "$Revision: 52054 $", interfaceVersion = 3, names = {}, urls = {})
public class BuzzheavierComFolder extends PluginForDecrypt {
    public BuzzheavierComFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "buzzheavier.com" });
        ret.add(new String[] { "fuckingfast.net" });
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

    public static final Pattern PATTERN_FILE_FOLDER = Pattern.compile("/([a-z0-9]{12})", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_FILE_FOLDER.pattern());
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String folderID = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final BuzzheavierCom hosterplugin = (BuzzheavierCom) this.getNewPluginForHostInstance(this.getHost());
        final DownloadLink singlefile = this.createDownloadlink(contenturl);
        singlefile.setDefaultPlugin(hosterplugin);
        singlefile.setHost(this.getHost());
        hosterplugin.requestFileInformation(singlefile);
        singlefile.setAvailable(true);
        String title = br.getRegex("name=\"title\" content=\"([^\"]+)").getMatch(0);
        final HashSet<String> dupes = new HashSet<String>();
        dupes.add(br._getURL().getPath());
        final String[] htmls = br.getRegex("<table>(.*?Z)</td>\\s*</tr>").getColumn(0);
        if (htmls != null && htmls.length > 0) {
            for (final String html : htmls) {
                final Regex urlAndFilename = new Regex(html, "<a href=\"(/[a-z0-9]{12})\"[^>]*>([^<]+)</a>");
                if (!urlAndFilename.patternFind()) {
                    /* Skip invalid items */
                    continue;
                }
                String url = urlAndFilename.getMatch(0);
                if (!dupes.add(url)) {
                    continue;
                }
                url = br.getURL(url).toExternalForm();
                final String filename = urlAndFilename.getMatch(1);
                final String filesizeStr = new Regex(html, "<td class=\"text-center\"[^>]*>([^<]+)</td>").getMatch(0);
                final String dateLastModified = new Regex(html, "formatDateOrTime\\('(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z)'\\)").getMatch(0);
                final DownloadLink link = createDownloadlink(url);
                link.setDefaultPlugin(hosterplugin);
                link.setHost(this.getHost());
                link.setName(Encoding.htmlDecode(filename).trim());
                if (dateLastModified != null) {
                    final long timestampLastModified = TimeFormatter.getMilliSeconds(dateLastModified, "yyyy-MM-dd'T'HH:mm:ss'Z'", Locale.ENGLISH);
                    link.setLastModifiedTimestamp(timestampLastModified);
                }
                if (filesizeStr != null) {
                    link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                }
                link.setAvailable(true);
                ret.add(link);
            }
            lastResortFallback: if (ret.isEmpty()) {
                /* Last resort fallback */
                final String[] urls = br.getRegex("(/[a-z0-9]{12})\"").getColumn(0);
                if (urls == null || urls.length == 0) {
                    break lastResortFallback;
                }
                for (String url : urls) {
                    if (!dupes.add(url)) {
                        continue;
                    } else if (url.contains(folderID)) {
                        continue;
                    }
                    url = br.getURL(url).toExternalForm();
                    final DownloadLink link = createDownloadlink(url);
                    link.setDefaultPlugin(hosterplugin);
                    link.setHost(this.getHost());
                    ret.add(link);
                }
            }
            if (title != null) {
                title = Encoding.htmlDecode(title).trim();
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(title);
                fp.setPackageKey(this.getHost() + "/folder/" + folderID);
                fp.addLinks(ret);
            }
        }
        if (ret.isEmpty()) {
            /* Assume that we got a single file */
            ret.add(singlefile);
        }
        return ret;
    }
}
