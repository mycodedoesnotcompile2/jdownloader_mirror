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
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.URLHelper;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.AccountRequiredException;
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
import jd.plugins.PluginForHost;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 50554 $", interfaceVersion = 3, names = { "luscious.net" }, urls = { "https?://(?:(?:www|members)\\.)?luscious\\.net/albums/([a-z0-9\\-_]+)_(\\d+)/?" })
public class LusciousNetAlbum extends PluginForDecrypt {
    public LusciousNetAlbum(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String albumSlug = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        final String albumID = new Regex(contenturl, this.getSupportedLinks()).getMatch(1);
        /* 2024-10-23: That particular API method doesn't work anymore for albums? */
        final boolean useAPI = false;
        final String api_base = "https://apicdn.luscious.net/graphql";
        if (useAPI) {
            int page = 1;
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(albumSlug);
            int maxItemsPerPage = 50;
            br.getHeaders().put("Accept", "application/json");
            br.getHeaders().put("Content-Type", "text/plain");
            br.getHeaders().put("Origin", "https://www." + getHost());
            br.getHeaders().put("Referer", contenturl);
            pagination: do {
                br.getPage(api_base + "/nobatch/?operationName=AlbumListOwnPictures&query=+query+AlbumListOwnPictures%28%24input%3A+PictureListInput%21%29+%7B+picture+%7B+list%28input%3A+%24input%29+%7B+info+%7B+...FacetCollectionInfo+%7D+items+%7B+...PictureStandardWithoutAlbum+%7D+%7D+%7D+%7D+fragment+FacetCollectionInfo+on+FacetCollectionInfo+%7B+page+has_next_page+has_previous_page+total_items+total_pages+items_per_page+url_complete+%7D+fragment+PictureStandardWithoutAlbum+on+Picture+%7B+__typename+id+title+description+created+like_status+number_of_comments+number_of_favorites+moderation_status+width+height+resolution+aspect_ratio+url_to_original+url_to_video+is_animated+position+tags+%7B+category+text+url+%7D+permissions+url+thumbnails+%7B+width+height+size+url+%7D+%7D+&variables=%7B%22input%22%3A%7B%22filters%22%3A%5B%7B%22name%22%3A%22album_id%22%2C%22value%22%3A%22" + albumID
                        + "%22%7D%5D%2C%22display%22%3A%22position%22%2C%22page%22%3A" + page + "%7D%7D");
                Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                entries = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "data/picture/list");
                final Map<String, Object> listInfo = (Map<String, Object>) entries.get("info");
                final List<Map<String, Object>> ressourcelist = (List<Map<String, Object>>) entries.get("items");
                for (final Map<String, Object> picture : ressourcelist) {
                    final String directurl = (String) picture.get("url_to_original");
                    final String url = (String) picture.get("url");
                    final long position = ((Number) picture.get("position")).longValue();
                    if (StringUtils.isEmpty(directurl) || StringUtils.isEmpty(url)) {
                        /* Skip invalid items */
                        continue;
                    }
                    final DownloadLink dl = this.createDownloadlink(URLHelper.createURL(URLHelper.parseLocation(new URL("https://" + this.getHost()), url)).toString());
                    final String filename = Plugin.getFileNameFromURL(new URL(directurl));
                    if (filename != null) {
                        dl.setName(filename);
                    }
                    dl.setAvailable(true);
                    dl._setFilePackage(fp);
                    dl.setProperty("directlink", directurl);
                    /* Save Packagizer properties */
                    dl.setProperty("position", position);
                    ret.add(dl);
                    distribute(dl);
                }
                logger.info("Crawled page " + page + " / " + listInfo.get("total_pages") + " |  + items: " + ret.size() + " / " + listInfo.get("total_items"));
                if (ressourcelist.isEmpty() && page == 1) {
                    /* No items on first page --> Offline/invalid URL */
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                } else if (!((Boolean) listInfo.get("has_next_page")).booleanValue()) {
                    logger.info("Stopping because: reached last page");
                    break pagination;
                } else if (ressourcelist.size() < maxItemsPerPage) {
                    /* Fail-safe */
                    logger.info("Stopping because: Current page contains less items than: " + maxItemsPerPage);
                    break pagination;
                } else {
                    page += 1;
                }
            } while (!this.isAbort());
        } else {
            br.getPage(contenturl);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            boolean accountRequired = false;
            if (StringUtils.containsIgnoreCase(br.getURL(), "accounts/login")) {
                logger.info("Looks like account is required to view this album");
                accountRequired = true;
            }
            final String title = albumSlug.replace("-", " ").trim();
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(title);
            /*
             * 2024-10-28: Disabled single image crawling as it doesn't work: It looks to be impossible to reliably generate full size URLs
             * just by using their thumbnail URLs as a base.
             */
            String redirectURLPath = br.getRegex("\"(/download/r/\\d+/\\d+/?)\"").getMatch(0);
            if (redirectURLPath == null && accountRequired) {
                /* Workaround to get around the need to have an account :) */
                try {
                    final Browser brc = br.cloneBrowser();
                    brc.getPage(api_base
                            + "/nobatch/?operationName=AlbumListRelated&query=%2520query%2520AlbumListRelated%28%2524id%253A%2520ID%21%252C%2520%2524tag%253A%2520String%29%2520%257B%2520album%2520%257B%2520list_related%28id%253A%2520%2524id%29%2520%257B%2520more_like_this%2520%257B%2520...AlbumInSearchList%2520%257D%2520items_liked_like_this%2520%257B%2520...AlbumInSearchList%2520%257D%2520items_created_by_this_user%2520%257B%2520...AlbumInSearchList%2520%257D%2520items_by_best_tag%28tag%253A%2520%2524tag%29%2520%257B%2520...AlbumInSearchList%2520%257D%2520%257D%2520%257D%2520%257D%2520fragment%2520AlbumInSearchList%2520on%2520Album%2520%257B%2520__typename%2520id%2520title%2520description%2520created%2520modified%2520like_status%2520moderation_status%2520number_of_favorites%2520number_of_dislikes%2520number_of_pictures%2520number_of_animated_pictures%2520number_of_duplicates%2520slug%2520is_manga%2520url%2520download_url%2520labels%2520permissions%2520cover%2520%257B%2520width%2520height%2520size%2520url%2520%257D%2520created_by%2520%257B%2520id%2520url%2520name%2520display_name%2520user_title%2520avatar_url%2520%257D%2520language%2520%257B%2520id%2520title%2520url%2520%257D%2520tags%2520%257B%2520category%2520text%2520url%2520count%2520%257D%2520genres%2520%257B%2520id%2520title%2520slug%2520url%2520%257D%2520%257D%2520&variables=%7B%22id%22%3A%22"
                            + albumID + "%22%2C%22tag%22%3Anull%7D");
                    final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                    final Map<String, Object> firstUserItem = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "data/album/list_related/items_created_by_this_user/{0}");
                    final String download_url = firstUserItem.get("download_url").toString();
                    final String userID = new Regex(download_url, "(?i)/download/r/(\\d+)").getMatch(0);
                    if (userID == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find userID");
                    }
                    redirectURLPath = "/download/r/" + userID + "/" + albumID + "/";
                } catch (final Throwable e) {
                    logger.log(e);
                    logger.warning("Special handling failed");
                }
            }
            if (redirectURLPath != null) {
                final Browser brc = br.cloneBrowser();
                brc.setFollowRedirects(false);
                brc.getPage("https://www." + br.getHost() + redirectURLPath);
                /* Typically redirects to external file hoster 9cloud.us. */
                final String redirect = brc.getRedirectLocation();
                if (redirect == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final PluginForHost plg = this.getNewPluginForHostInstance("9cloud.us");
                final DownloadLink link = this.createDownloadlink(redirect);
                if (plg.canHandle(redirect)) {
                    link.setHost(plg.getHost());
                    link.setDefaultPlugin(plg);
                    /**
                     * For 9cloud.us we cannot find any filenames as long as any free download limit is reached which is often the case.
                     * </br>
                     * To counter that, we'll just set a filename here including the available status as we know that that file is online.
                     */
                    link.setName(title + ".zip");
                    link.setAvailable(true);
                }
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
            } else {
                logger.info("Failed to find zip download");
            }
            final boolean crawlSingleImages = true;
            if (crawlSingleImages) {
                // String[] thumbnails = br.getRegex("src=\"(https?://[^\"]+)\" class=\"\" loading=\"lazy\"").getColumn(0);
                // if (thumbnails == null || thumbnails.length == 0) {
                // thumbnails = br.getRegex("alt=\"\\d+\" src=\"(https?://[^\"]+)\"").getColumn(0);
                // }
                // final FilePackage fpThumbnails = FilePackage.getInstance();
                // fpThumbnails.setName(title + " - thumbnails");
                // for (final String urlThumbnail : thumbnails) {
                // /* Change thumbnail URL to a full size URL */
                // final String fullsizeUrl = urlThumbnail.replaceFirst("\\.315x0\\.jpg$", ".640x0.jpg");
                // /* This link may not always be available thus we will also add the original thumbnail link later. */
                // final DownloadLink directFullsize = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(urlThumbnail));
                // /* We know that this link is online. */
                // directFullsize.setAvailable(true);
                // directFullsize._setFilePackage(fp);
                // ret.add(directFullsize);
                // if (fullsizeUrl.equals(urlThumbnail)) {
                // /* Both links are the same -> No reason to add thumbnail separately. */
                // continue;
                // }
                // final DownloadLink directThumbnail = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(urlThumbnail));
                // /* We know that this link is online. */
                // directThumbnail.setAvailable(true);
                // directThumbnail._setFilePackage(fpThumbnails);
                // ret.add(directThumbnail);
                // }
                final int itemsPerPage = 50;
                int page = 1;
                final HashSet<String> dupes = new HashSet<String>();
                pagination: do {
                    br.getPage(api_base
                            + "/nobatch/?operationName=PictureListInsideAlbum&query=%2520query%2520PictureListInsideAlbum%28%2524input%253A%2520PictureListInput%21%29%2520%257B%2520picture%2520%257B%2520list%28input%253A%2520%2524input%29%2520%257B%2520info%2520%257B%2520...FacetCollectionInfo%2520%257D%2520items%2520%257B%2520__typename%2520id%2520title%2520description%2520created%2520like_status%2520number_of_comments%2520number_of_favorites%2520moderation_status%2520width%2520height%2520resolution%2520aspect_ratio%2520url_to_original%2520url_to_video%2520is_animated%2520position%2520permissions%2520url%2520tags%2520%257B%2520category%2520text%2520url%2520%257D%2520thumbnails%2520%257B%2520width%2520height%2520size%2520url%2520%257D%2520%257D%2520%257D%2520%257D%2520%257D%2520fragment%2520FacetCollectionInfo%2520on%2520FacetCollectionInfo%2520%257B%2520page%2520has_next_page%2520has_previous_page%2520total_items%2520total_pages%2520items_per_page%2520url_complete%2520%257D%2520&variables=%7B%22input%22%3A%7B%22filters%22%3A%5B%7B%22name%22%3A%22album_id%22%2C%22value%22%3A%22"
                            + albumID + "%22%7D%5D%2C%22display%22%3A%22rating_all_time%22%2C%22items_per_page%22%3A" + itemsPerPage + "%2C%22page%22%3A" + page + "%7D%7D");
                    final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                    final Map<String, Object> data = (Map<String, Object>) entries.get("data");
                    final Map<String, Object> picture = (Map<String, Object>) data.get("picture");
                    final Map<String, Object> list = (Map<String, Object>) picture.get("list");
                    final Map<String, Object> info = (Map<String, Object>) list.get("info");
                    final int total_numberof_items = ((Number) info.get("total_items")).intValue();
                    if (total_numberof_items == 0) {
                        throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
                    }
                    final int pageMax = ((Number) info.get("total_pages")).intValue();
                    final List<Map<String, Object>> items = (List<Map<String, Object>>) list.get("items");
                    int numberofNewItems = 0;
                    for (final Map<String, Object> item : items) {
                        if (!dupes.add(item.get("id").toString())) {
                            continue;
                        }
                        numberofNewItems++;
                        final String description = (String) item.get("description");
                        final String url_to_original = item.get("url_to_original").toString();
                        final DownloadLink image = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url_to_original));
                        if (!StringUtils.isEmpty(description)) {
                            image.setComment(description);
                        }
                        image.setAvailable(true);
                        image._setFilePackage(fp);
                        ret.add(image);
                        distribute(image);
                    }
                    logger.info("Cralwed page " + page + "/" + pageMax + " | Found items so far: " + dupes.size() + "/" + total_numberof_items);
                    if (Boolean.FALSE.equals(info.get("has_next_page"))) {
                        logger.info("Stopping because: Reached end");
                        break pagination;
                    } else if (numberofNewItems == 0) {
                        logger.info("Stopping because: Failed to find any new items on current page");
                        break pagination;
                    } else {
                        /* Continue to next page */
                        page++;
                        // continue;
                    }
                } while (!this.isAbort());
            }
            if (ret.isEmpty()) {
                if (accountRequired) {
                    throw new AccountRequiredException();
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
        return ret;
    }
}
