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

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.ZzupCom;

@DecrypterPlugin(revision = "$Revision: 51069 $", interfaceVersion = 3, names = {}, urls = {})
public class ZzupComCrawler extends PluginForDecrypt {
    public ZzupComCrawler(PluginWrapper wrapper) {
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
        ret.add(new String[] { "zzup.com" });
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
            ret.add("https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "/content/([a-zA-Z0-9_/\\+\\=\\-%]+)/([\\w-]+)/([a-zA-Z0-9_/\\+\\=\\-%]+)/(page-\\d+|index)\\.html");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final Regex urlinfo = new Regex(param.getCryptedUrl(), this.getSupportedLinks());
        final String albumID = Encoding.htmlDecode(urlinfo.getMatch(0));
        final String albumSlug = urlinfo.getMatch(1);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        /* Remove page from URL so we always start on page 1. */
        final String contenturl = param.getCryptedUrl().replaceFirst("(?i)page-\\d+\\.html$", "index.html");
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        int page = 1;
        int pageMax = 1;
        /* Determine max page */
        final String pageMaxStr = br.getRegex("class=\"imgpagebar\"[^>]*><h2> 1 / (\\d+)").getMatch(0);
        if (pageMaxStr != null) {
            pageMax = Integer.parseInt(pageMaxStr);
        } else {
            final String[] pages = br.getRegex("page-(\\d+)\\.html").getColumn(0);
            if (pages != null && pages.length > 0) {
                for (final String pageStr : pages) {
                    final int pageInt = Integer.parseInt(pageStr);
                    if (pageInt > pageMax) {
                        pageMax = pageInt;
                    }
                }
            }
        }
        String title = br.getRegex("<center>\\s*<span[^>]*>([^<]+)</span><br>").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(Encoding.htmlDecode(title).trim());
        } else {
            /* Fallback */
            logger.warning("Failed to find album title");
            fp.setName(Encoding.htmlDecode(albumSlug).replace("_", " ").trim());
        }
        fp.setPackageKey(this.getHost() + "://album/" + albumID);
        final HashSet<String> dupes = new HashSet<String>();
        final ZzupCom hosterplugin = (ZzupCom) this.getNewPluginForHostInstance(this.getHost());
        pagination: do {
            int numberofNewItemsThisPage = 0;
            final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
            for (final String url : urls) {
                if (!hosterplugin.canHandle(url)) {
                    /* Skip unsupported links */
                    continue;
                } else if (!dupes.add(url)) {
                    /* Skip duplicates */
                    continue;
                }
                numberofNewItemsThisPage++;
                final DownloadLink image = createDownloadlink(url);
                image.setName(url.substring(url.lastIndexOf("/") + 1).replaceFirst("(?i)\\.html$", ".jpg"));
                image.setAvailable(true);
                image._setFilePackage(fp);
                ret.add(image);
                distribute(image);
            }
            logger.info("Crawled page " + page + " / " + pageMax + " | Items so far: " + ret.size());
            if (page >= pageMax) {
                logger.info("Stopping because: Reached end");
                break pagination;
            } else if (numberofNewItemsThisPage == 0) {
                logger.info("Stopping because: Failed to find any new items on current page");
                break pagination;
            }
            page++;
            br.getPage("page-" + page + ".html");
        } while (!this.isAbort());
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
