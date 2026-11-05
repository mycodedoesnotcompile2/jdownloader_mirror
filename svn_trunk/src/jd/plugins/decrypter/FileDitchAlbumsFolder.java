//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
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
import java.util.Set;
import java.util.regex.Pattern;

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
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

/**
 * Crawler for album/collection pages of fileditchalbums.st. The found file links point to other FileDitch domains and are handled by the
 * host plugin "FileDitchCom".
 */
@DecrypterPlugin(revision = "$Revision: 53900 $", interfaceVersion = 3, names = {}, urls = {})
public class FileDitchAlbumsFolder extends PluginForDecrypt {
    public FileDitchAlbumsFolder(PluginWrapper wrapper) {
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
        /* each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain */
        ret.add(new String[] { "fileditchalbums.st" });
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

    /** e.g. https://fileditchalbums.st/4p9uk5nbdh */
    private static final Pattern PATTERN_ALBUM = Pattern.compile("/([a-z0-9]{10})", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_ALBUM.pattern() + "(?:\\?[^#]*)?");
        }
        return ret.toArray(new String[0]);
    }

    /**
     * The website lets users choose how many items are displayed per page via the select box "fda-pp". The choice is stored in the cookie
     * "fda_pp" (path "/"), the page itself then reloads. If the album has more than one page, this sets the cookie to the highest available
     * value and reloads the first page, so fewer requests are needed. Must be called before any item of the first page has been collected.
     */
    private void selectMaxItemsPerPage(final String albumURL) throws Exception {
        if (!br.containsHTML("rel=\"next\"")) {
            /* Only one page -> nothing to optimize */
            return;
        }
        final String selectHTML = br.getRegex("<select id=\"fda-pp\"[^>]*>(.*?)</select>").getMatch(0);
        if (selectHTML == null) {
            logger.warning("Failed to find items-per-page selection");
            return;
        }
        int max = 0;
        for (final String value : new Regex(selectHTML, "<option value=\"(\\d+)\"").getColumn(0)) {
            max = Math.max(max, Integer.parseInt(value));
        }
        final String selectedStr = new Regex(selectHTML, "<option value=\"(\\d+)\" selected").getMatch(0);
        final int selected = selectedStr != null ? Integer.parseInt(selectedStr) : 0;
        if (max == 0 || max == selected) {
            logger.info("Items per page already at maximum: " + selected);
            return;
        }
        logger.info("Switching items per page from " + selected + " to " + max);
        br.setCookie(br.getHost(), "fda_pp", Integer.toString(max));
        br.getPage(albumURL);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.containsHTML("<option value=\"" + max + "\" selected")) {
            logger.warning("Failed to set items per page to " + max);
        }
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String albumID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        final String albumURL = "https://" + this.getHost() + "/" + albumID;
        final Set<String> dupes = new HashSet<String>();
        FilePackage fp = null;
        int page = 1;
        pagination: do {
            if (page == 1) {
                br.getPage(albumURL);
            } else {
                br.getPage(albumURL + "?p=" + page);
            }
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            if (page == 1) {
                selectMaxItemsPerPage(albumURL);
            }
            if (fp == null) {
                final String title = br.getRegex("<h1 class=\"title\"[^>]*>\\s*([^<]+)").getMatch(0);
                fp = FilePackage.getInstance();
                if (title != null) {
                    fp.setName(Encoding.htmlDecode(title).trim());
                } else {
                    /* Fallback */
                    logger.warning("Failed to find album title");
                    fp.setName(albumID);
                }
                /* Stable key: Do not derive this from the domain as the domain may change. */
                fp.setPackageKey("fileditchalbums://" + albumID);
            }
            final String[] cards = br.getRegex("<article class=\"card\"[^>]*>(.*?)</article>").getColumn(0);
            int numberofNewItems = 0;
            if (cards != null) {
                for (final String card : cards) {
                    final Regex nameInfo = new Regex(card, "<a class=\"name\"[^>]*href=\"([^\"]+)\"[^>]*>([^<]*)</a>");
                    final String url = nameInfo.getMatch(0);
                    if (url == null) {
                        /* Skip invalid items */
                        continue;
                    }
                    final String directurl = Encoding.htmlOnlyDecode(url);
                    if (!dupes.add(directurl)) {
                        continue;
                    }
                    numberofNewItems++;
                    final String filename = nameInfo.getMatch(1);
                    final String filesizeStr = new Regex(card, "<span class=\"size\">([^<]+)</span>").getMatch(0);
                    /* The URL is handled by the host plugin "FileDitchCom". */
                    final DownloadLink link = this.createDownloadlink(directurl);
                    if (filename != null && !filename.trim().isEmpty()) {
                        link.setName(Encoding.htmlDecode(filename).trim());
                    }
                    if (filesizeStr != null) {
                        link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
                    }
                    link.setAvailable(true);
                    fp.add(link);
                    ret.add(link);
                    distribute(link);
                }
            }
            logger.info("Crawled page " + page + " | New items: " + numberofNewItems + " | Total: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping pagination at page " + page + " because: Crawl process aborted by user");
                throw new InterruptedException();
            } else if (numberofNewItems == 0) {
                /*
                 * Fail-safe: Every item is checked against the dupe set. A page without any new item means either broken HTML parsing or
                 * the site returned an already crawled page -> Stop to avoid an endless loop.
                 */
                logger.info("Stopping pagination at page " + page + " because: No new items found on this page");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Pagination failed: No new items found on page " + page);
            } else if (!br.containsHTML("href=\"[^\"]*[\\?&]p=" + (page + 1) + "\"")) {
                /* Last page reached */
                logger.info("Stopping pagination at page " + page + " because: No link to next page found -> Reached end of album");
                break pagination;
            }
            page++;
        } while (true);
        return ret;
    }
}
