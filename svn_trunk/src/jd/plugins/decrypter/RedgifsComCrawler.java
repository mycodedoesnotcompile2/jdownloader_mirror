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
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Request;
import jd.http.requests.GetRequest;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.RedGifsCom;

@DecrypterPlugin(revision = "$Revision: 51331 $", interfaceVersion = 3, names = {}, urls = {})
public class RedgifsComCrawler extends PluginForDecrypt {
    public RedgifsComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY, LazyPlugin.FEATURE.XXX };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "redgifs.com" });
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

    private static final Pattern PATTERN_USERS                 = Pattern.compile("/users/([\\w\\-]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_GALLERY_OR_SINGLE_GIF = Pattern.compile("/(?!gifs/)(?:(?:watch|ifr)/)?([A-Za-z0-9]+)$", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_USERS.pattern() + "|" + PATTERN_GALLERY_OR_SINGLE_GIF.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final RedGifsCom plg = (RedGifsCom) this.getNewPluginForHostInstance(this.getHost());
        this.br = plg.createNewBrowserInstance();
        final String token = plg.getTemporaryToken(br, null);
        final String username = new Regex(param.getCryptedUrl(), PATTERN_USERS).getMatch(0);
        final String contentID = new Regex(param.getCryptedUrl(), PATTERN_GALLERY_OR_SINGLE_GIF).getMatch(0);
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_ORIGIN, "https://www." + this.getHost());
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_REFERER, "https://www." + this.getHost() + "/");
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_AUTHORIZATION, "Bearer " + token);
        int totalNumberofItems = -1;
        if (username != null) {
            /* Crawl all items of a user */
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(username);
            final int maxItemsPerPage = 40;
            int page = 1;
            final HashSet<String> dupes = new HashSet<String>();
            final List<Integer> pagesWithMissingItems = new ArrayList<Integer>();
            pagination: do {
                int numberofNewItemsThisPage = 0;
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("order", "new");
                query.appendEncoded("count", Integer.toString(maxItemsPerPage));
                if (page > 1) {
                    query.appendEncoded("page", Integer.toString(page));
                }
                final GetRequest req = br.createGetRequest("https://api.redgifs.com/v2/users/" + username + "/search?" + query.toString());
                plg.getPage(br, token, req);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                if (((Number) entries.get("total")).intValue() == 0) {
                    /* Profile contains zero items/content. */
                    throw new DecrypterRetryException(RetryReason.EMPTY_PROFILE);
                }
                final List<Map<String, Object>> gifs = (List<Map<String, Object>>) entries.get("gifs");
                for (final Map<String, Object> gif : gifs) {
                    final String mediaID = gif.get("id").toString();
                    if (!dupes.add(mediaID)) {
                        logger.info("Skipping dupe mediaID: " + mediaID);
                        continue;
                    }
                    numberofNewItemsThisPage++;
                    final DownloadLink link = this.createDownloadlink(generateSingleItemURL(mediaID, null));
                    RedGifsCom.parseFileInfo(link, gif);
                    link.setAvailable(true);
                    link._setFilePackage(fp);
                    ret.add(link);
                    distribute(link);
                }
                final int pageMax = ((Number) entries.get("pages")).intValue();
                totalNumberofItems = ((Number) entries.get("total")).intValue();
                logger.info("Crawled page " + page + "/" + pageMax + " | Found items: " + ret.size() + "/" + totalNumberofItems + " | New items this page: " + numberofNewItemsThisPage);
                final boolean isLastPage = page >= pageMax;
                if (gifs.size() < maxItemsPerPage && !isLastPage) {
                    /* Double fail-safe */
                    logger.info("Current page contains less than max items -> Most likely some items of this profile have been deleted!");
                    pagesWithMissingItems.add(page);
                    /* 2025-08-15: Do not stop, just log this case. */
                    // break pagination;
                }
                if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    throw new InterruptedException();
                } else if (isLastPage) {
                    logger.info("Stopping because: Reached last page: " + pageMax);
                    break pagination;
                } else if (numberofNewItemsThisPage == 0) {
                    /* Additional fail safe which should not be needed. */
                    logger.info("Stopping because: Failed to find any new items on current page: " + page);
                    break pagination;
                } else {
                    /* Continue to next page */
                    page++;
                    continue pagination;
                }
            } while (true);
            final int numberofMissingItems = totalNumberofItems - ret.size();
            if (numberofMissingItems > 0) {
                String message = String.format("Found only %d/%d items!\r\nThis may happen if items get deleted but the server side total number of items is not updated by redgifs.\r\nThis is not a JDownloader bug!", ret.size(), totalNumberofItems);
                if (!pagesWithMissingItems.isEmpty()) {
                    StringBuilder sb = new StringBuilder();
                    for (int i = 0; i < pagesWithMissingItems.size(); i++) {
                        if (i > 0) {
                            sb.append(", ");
                        }
                        sb.append(pagesWithMissingItems.get(i));
                    }
                    message += String.format("\r\nPage numbers with missing items: %s", sb.toString());
                }
                this.displayBubbleNotification(String.format("Profile: %s | Items missing!", username), message);
            }
        } else {
            /* Crawl single image or all images of a gallery */
            final Map<String, Object> response = plg.getView(br, token, contentID);
            final Map<String, Object> gif_main = (Map<String, Object>) response.get("gif");
            final String galleryID = (String) gif_main.get("gallery");
            if (StringUtils.isEmpty(galleryID)) {
                /* Single image */
                final DownloadLink link = this.createDownloadlink(generateSingleItemURL(contentID, null));
                RedGifsCom.parseFileInfo(link, gif_main);
                link.setAvailable(true);
                ret.add(link);
                return ret;
            }
            /* Gallery that can contain multiple images */
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(contentID);
            final Request req = br.createGetRequest("/v2/gallery/" + galleryID);
            plg.getPage(br, token, req);
            final Map<String, Object> gallery = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final List<Map<String, Object>> gifs = (List<Map<String, Object>>) gallery.get("gifs");
            for (final Map<String, Object> gif : gifs) {
                final String mediaID = gif.get("id").toString();
                final DownloadLink link = this.createDownloadlink(generateSingleItemURL(mediaID, null));
                RedGifsCom.parseFileInfo(link, gif);
                link.setAvailable(true);
                link._setFilePackage(fp);
                distribute(link);
                ret.add(link);
            }
        }
        return ret;
    }

    private String generateSingleItemURL(final String mediaID, final String username) {
        String url = "https://www." + getHost() + "/watch/" + mediaID;
        if (username != null) {
            url += "#rel=user%3A" + username + ";order=new";
        }
        return url;
    }
}
