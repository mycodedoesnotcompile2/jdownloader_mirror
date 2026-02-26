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

import org.appwork.utils.parser.UrlQuery;

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
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52391 $", interfaceVersion = 3, names = {}, urls = {})
public class TaiwebsComBr0wsersCom extends PluginForDecrypt {
    public TaiwebsComBr0wsersCom(PluginWrapper wrapper) {
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
        ret.add(new String[] { "br0wsers.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/blog/detail/([\\w-]+)-(\\d+)-(\\d+)\\.html");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Regex urlinfo = new Regex(param.getCryptedUrl(), this.getSupportedLinks());
        // final String urlSlug = urlinfo.getMatch(0);
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Origin", "https://br0wsers.com");
        brc.getHeaders().put("Referer", param.getCryptedUrl());
        brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("ils", urlinfo.getMatch(2));
        query.appendEncoded("tlv", urlinfo.getMatch(1));
        query.appendEncoded("actions", "bannersxyz");
        brc.postPage("https://br0wsers.com/data.php", query);
        if (brc.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String[] links = brc.getRegex("id=\"various1\" href=\"(https?://[^\"]+)\"").getColumn(0);
        if (links == null || links.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final HashSet<String> dupes = new HashSet<String>();
        FilePackage fp = null;
        final String filenameEncoded = brc.getRegex("&ref=([^&\\?\"]+)").getMatch(0);
        if (filenameEncoded != null) {
            fp = FilePackage.getInstance();
            fp.setName(Encoding.Base64Decode(filenameEncoded).trim());
            fp.setCleanupPackageName(false);
        }
        // fp.setName(urlSlug);
        final Browser brx = br.cloneBrowser();
        brx.setFollowRedirects(false);
        final ArrayList<String> pwlist = new ArrayList<String>();
        pwlist.add("taiwebs.com");
        int numberofOfflineItems = 0;
        int index = -1;
        for (final String link : links) {
            index++;
            if (!dupes.add(link)) {
                continue;
            }
            logger.info("Crawling item " + (index + 1) + "/" + links.length);
            final String gdriveFileIDEncoded = new Regex(link, "(?i)dl/goo/[^/]+/([a-zA-Z0-9_/\\+\\=\\-%]+)").getMatch(0);
            final String encodedURL;
            final DownloadLink result;
            if (gdriveFileIDEncoded != null) {
                final String gdriveFileID = Encoding.Base64Decode(Encoding.htmlDecode(gdriveFileIDEncoded.trim()));
                result = createDownloadlink(GoogleDriveCrawler.generateFileURL(gdriveFileID));
            } else if ((encodedURL = new Regex(link, "(?i)dlfast/[^/]+/(aHR0[a-zA-Z0-9_/\\+\\=\\-%]+)").getMatch(0)) != null) {
                /* Typically mediafire.com */
                final String decodedURL = Encoding.Base64Decode(Encoding.htmlDecode(encodedURL.trim()));
                result = createDownloadlink(decodedURL);
            } else {
                /* http request required to find final URL */
                brx.getPage(link);
                if (brx.getHttpConnection().getResponseCode() == 400 || brx.getHttpConnection().getResponseCode() == 404) {
                    numberofOfflineItems++;
                    continue;
                }
                final String redirect = brx.getRedirectLocation();
                if (redirect == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                result = createDownloadlink(DirectHTTP.createURLForThisPlugin(redirect));
            }
            result.setSourcePluginPasswordList(pwlist);
            if (fp != null) {
                result._setFilePackage(fp);
            }
            ret.add(result);
            distribute(result);
            if (this.isAbort()) {
                /* Aborted by user */
                throw new InterruptedException();
            }
        }
        if (ret.isEmpty() && numberofOfflineItems > 0) {
            /* Assume that all items are offline */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return ret;
    }
}
