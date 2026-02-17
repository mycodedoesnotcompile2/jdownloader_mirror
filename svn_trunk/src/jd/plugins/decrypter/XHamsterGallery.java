//jDownloader - Downloadmanager
//Copyright (C) 2013  JD-Team support@jdownloader.org
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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;
import jd.plugins.hoster.XHamsterCom;

@DecrypterPlugin(revision = "$Revision: 52320 $", interfaceVersion = 3, names = {}, urls = {})
public class XHamsterGallery extends PluginForDecrypt {
    public XHamsterGallery(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY, LazyPlugin.FEATURE.XXX };
    }

    /** Make sure this is the same in classes XHamsterCom and XHamsterGallery! */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "xhamster.com", "xhamster.xxx", "xhamster.desi", "xhamster.one", "xhamster1.desi", "xhamster2.desi", "xhamster3.desi", "openxh.com", "openxh1.com", "openxh2.com", "megaxh.com", "xhvid.com", "xhbranch5.com", "xhamster.tv", "airportxh.life", "xhsay.life", "xhaccess.com", "xhopen.com" });
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

    public static final Pattern PATTERN_PHOTO_GALLERY                = Pattern.compile("/photos/gallery/([0-9A-Za-z_\\-/]+)-(\\d+)", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_PLAYLIST_MY_FAVORITES_VIDEOS = Pattern.compile("/my/favorites/videos(/([a-f0-9]{24})-([\\w\\-]+))?", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_PLAYLIST_NORMAL              = Pattern.compile("/playlists/([\\w-]+)-([a-f0-9]{24})", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_USER_VIDEOS                  = Pattern.compile("/users/(?:profiles/)?([^/]+)/videos", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_USER_SHORTS_MOMENTS          = Pattern.compile("/users/(?:profiles/)?([^/]+)/(shorts|moments)", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_USER_FAVORITE_VIDEOS         = Pattern.compile("/users/(?:profiles/)?([^/]+)/favorites/videos", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_USER_CREATOR_PHOTOS          = Pattern.compile("/(creators|users)/(?:profiles/)?([^/]+)/photos", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_CREATOR_SHORTS               = Pattern.compile("/(creators|users)/(?:profiles/)?([^/]+)/(shorts|moments)", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_CHANNELS                     = Pattern.compile("/channels/([^/]+)", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_PORNSTARS                    = Pattern.compile("/(?:[^/]+/)?pornstars/([^/]+)", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_CREATORS_VIDEOS              = Pattern.compile("/(?:[^/]+/)?creators/([^/]+)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            final StringBuilder sb = new StringBuilder();
            sb.append("https?://(?:[a-z0-9\\-]+\\.)?" + buildHostsPatternPart(domains));
            sb.append("/(");
            sb.append(PATTERN_PHOTO_GALLERY.pattern().substring(1)); // "/" entfernen
            sb.append("|" + PATTERN_PLAYLIST_MY_FAVORITES_VIDEOS.pattern().substring(1));
            sb.append("|" + PATTERN_PLAYLIST_NORMAL.pattern().substring(1));
            sb.append("|" + PATTERN_USER_VIDEOS.pattern().substring(1));
            sb.append("|" + PATTERN_USER_SHORTS_MOMENTS.pattern().substring(1));
            sb.append("|" + PATTERN_USER_FAVORITE_VIDEOS.pattern().substring(1));
            sb.append("|" + PATTERN_USER_CREATOR_PHOTOS.pattern().substring(1));
            sb.append("|" + PATTERN_CREATOR_SHORTS.pattern().substring(1));
            sb.append("|" + PATTERN_CHANNELS.pattern().substring(1));
            sb.append("|" + PATTERN_PORNSTARS.pattern().substring(1));
            sb.append("|" + PATTERN_CREATORS_VIDEOS.pattern().substring(1));
            sb.append(")");
            ret.add(sb.toString());
        }
        return ret.toArray(new String[0]);
    }

    public static String buildHostsPatternPart(String[] domains) {
        final StringBuilder pattern = new StringBuilder();
        pattern.append("(?:");
        for (int i = 0; i < domains.length; i++) {
            final String domain = domains[i];
            if (i > 0) {
                pattern.append("|");
            }
            if ("xhamster.com".equals(domain)) {
                /* Special: Allow e.g. xhamster4.com and so on. */
                pattern.append("xhamster\\d*\\.(?:com|xxx|desi|one)");
            } else {
                pattern.append(Pattern.quote(domain));
            }
        }
        pattern.append(")");
        return pattern.toString();
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        XHamsterCom.prepBr(this, br);
        // Login if possible
        final XHamsterCom hostPlugin = (XHamsterCom) this.getNewPluginForHostInstance(this.getHost());
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account != null) {
            hostPlugin.login(account, null, false);
        }
        final String url = param.getCryptedUrl();
        if (new Regex(url, PATTERN_USER_VIDEOS).patternFind()) {
            /* Crawl all videos of a user */
            return crawlUserProfile(param);
        } else if (new Regex(url, PATTERN_USER_FAVORITE_VIDEOS).patternFind()) {
            /* Crawl all videos of a user */
            return crawlUserProfileFavorites(param);
        } else if (new Regex(url, PATTERN_PORNSTARS).patternFind()) {
            /* Crawl all videos of a pornstar profile */
            return this.crawlUserProfilePornstar(param);
        } else if (new Regex(url, PATTERN_CREATORS_VIDEOS).patternFind()) {
            /* Crawl all videos of a creator profile */
            return this.crawlUserProfileCreator(param);
        } else if (new Regex(url, PATTERN_CHANNELS).patternFind()) {
            /* Crawl all videos of a channel */
            return crawlChannel(param);
        } else if (new Regex(url, PATTERN_PLAYLIST_MY_FAVORITES_VIDEOS).patternFind()) {
            /* Crawl users own favorites */
            return this.crawlUserFavorites(param, account);
        } else if (new Regex(url, PATTERN_PLAYLIST_NORMAL).patternFind()) {
            /* Crawl users own favorites */
            return this.crawlPlaylist(param, account);
        } else if (new Regex(url, PATTERN_USER_CREATOR_PHOTOS).patternFind()) {
            /* Crawl all photo galleries of a user/creator --> Goes back into crawler and crawler will crawl the single photos */
            return crawlAllGalleriesOfUserOrCreator(param);
        } else if (new Regex(url, PATTERN_CREATOR_SHORTS).patternFind()) {
            /* Crawl all shorts/moments of a user/creator --> Goes back into crawler and crawler will crawl the single photos */
            return crawlAllShortsOfUserOrCreator(param);
        } else {
            /* Single Photo gallery */
            return this.crawlPhotoGallery(param);
        }
    }

    private ArrayList<DownloadLink> crawlUserProfile(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException, InterruptedException {
        final String username = new Regex(param.getCryptedUrl(), PATTERN_USER_VIDEOS).getMatch(0);
        if (username == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contenturl = XHamsterCom.getCorrectedURL(param.getCryptedUrl());
        br.getPage(contenturl);
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username);
        final ArrayList<DownloadLink> ret = this.crawlPagination(param, fp);
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_PROFILE, "EMPTY_PROFILE_" + username);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlUserProfileFavorites(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException, InterruptedException {
        final String username = new Regex(param.getCryptedUrl(), PATTERN_USER_FAVORITE_VIDEOS).getMatch(0);
        if (username == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contenturl = XHamsterCom.getCorrectedURL(param.getCryptedUrl());
        br.getPage(contenturl);
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            /* Profile does not exist. */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username + " - Favorites");
        final ArrayList<DownloadLink> ret = this.crawlPagination(param, fp);
        if (ret.isEmpty()) {
            /* Profile has no favorites set. */
            throw new DecrypterRetryException(RetryReason.EMPTY_PROFILE, "EMPTY_PROFILE_FAVORITES_" + username);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlUserProfilePornstar(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException, InterruptedException {
        final String username = new Regex(param.getCryptedUrl(), PATTERN_PORNSTARS).getMatch(0);
        if (username == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contenturl = XHamsterCom.getCorrectedURL(param.getCryptedUrl());
        br.getPage(contenturl);
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username);
        final ArrayList<DownloadLink> ret = this.crawlPagination(param, fp);
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_PROFILE, "EMPTY_PROFILE_PORNSTAR_" + username);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlUserProfileCreator(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException, InterruptedException {
        final String username = new Regex(param.getCryptedUrl(), PATTERN_CREATORS_VIDEOS).getMatch(0);
        if (username == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contenturl = XHamsterCom.getCorrectedURL(param.getCryptedUrl());
        br.getPage(contenturl);
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username);
        final ArrayList<DownloadLink> ret = this.crawlPagination(param, fp);
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_PROFILE, "EMPTY_PROFILE_CREATOR_" + username);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlChannel(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException, InterruptedException {
        final String channelname = new Regex(param.getCryptedUrl(), PATTERN_CHANNELS).getMatch(0);
        if (channelname == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contenturl = XHamsterCom.getCorrectedURL(param.getCryptedUrl());
        br.getPage(contenturl);
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(channelname);
        final ArrayList<DownloadLink> ret = this.crawlPagination(param, fp);
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_PROFILE, "EMPTY_CHANNEL_" + channelname);
        }
        return ret;
    }

    /* Users can create custom favorites collections with custom names. This function can crawl them. */
    private ArrayList<DownloadLink> crawlUserFavorites(final CryptedLink param, final Account account) throws IOException, PluginException, DecrypterRetryException, InterruptedException {
        if (account == null) {
            throw new AccountRequiredException();
        }
        final String favoritesName = new Regex(param.getCryptedUrl(), PATTERN_PLAYLIST_MY_FAVORITES_VIDEOS).getMatch(2);
        if (favoritesName == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contenturl = XHamsterCom.getCorrectedURL(param.getCryptedUrl());
        br.getPage(contenturl);
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName("Favorites - " + favoritesName);
        final ArrayList<DownloadLink> ret = this.crawlPagination(param, fp);
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlPlaylist(final CryptedLink param, final Account account) throws IOException, PluginException, DecrypterRetryException, InterruptedException {
        final String playlistName = new Regex(param.getCryptedUrl(), PATTERN_PLAYLIST_NORMAL).getMatch(0);
        if (playlistName == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contenturl = XHamsterCom.getCorrectedURL(param.getCryptedUrl());
        br.getPage(contenturl);
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(playlistName);
        final ArrayList<DownloadLink> ret = this.crawlPagination(param, fp);
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        return ret;
    }

    /* Crawls all videos of all pages in given browsers' html. */
    private ArrayList<DownloadLink> crawlPagination(final CryptedLink param, final FilePackage fp) throws IOException, PluginException, InterruptedException, DecrypterRetryException {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final HashSet<String> dupes = new HashSet<String>();
        int page = 1;
        int maxPage = -1;
        int maxItems = -1;
        int maxItemsPerPage = -1;
        final String[] pageNums = br.getRegex("data-page=\"(\\d+)\"").getColumn(0);
        if (pageNums != null && pageNums.length > 0) {
            for (final String pageNumStr : pageNums) {
                final int pageNumTmp = Integer.parseInt(pageNumStr);
                if (pageNumTmp > maxPage) {
                    maxPage = pageNumTmp;
                }
            }
        }
        final XHamsterCom hostPlugin = (XHamsterCom) this.getNewPluginForHostInstance(this.getHost());
        final Pattern ignoreVideo = Pattern.compile("(?i).*/videos/[a-f0-9]{24}-watch-later.*");
        int numberofDeletedItemsTotal = 0;
        pagination: do {
            int numberofNewItemsThisPage = 0;
            int numberofDeletedItemsThisPage = 0;
            int nextPage = -1;
            crawlJson: {
                List<Map<String, Object>> videos = null;
                try {
                    final String json = br.getRegex("window\\.initials=(\\{.*?\\});</script>").getMatch(0);
                    if (json == null) {
                        logger.info("Failed to find json source");
                        break crawlJson;
                    }
                    final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
                    final Map<String, Object> singlePlaylistComponent = (Map<String, Object>) entries.get("singlePlaylistComponent");
                    final Map<String, Object> pagesCategoryComponent = (Map<String, Object>) entries.get("pagesCategoryComponent");
                    final Map<String, Object> sponsorChannel = (Map<String, Object>) JavaScriptEngineFactory.walkJson(pagesCategoryComponent, "channelLandingInfoProps/sponsorChannel");
                    final Map<String, Object> channelInfo = (Map<String, Object>) entries.get("channelInfo");
                    if (singlePlaylistComponent != null) {
                        /* Playlist */
                        final Map<String, Object> playlist = (Map<String, Object>) singlePlaylistComponent.get("playlist");
                        final Map<String, Object> playlist_meta = (Map<String, Object>) playlist.get("meta");
                        // final Map<String, Object> playlist_data = (Map<String, Object>) playlist.get("data");
                        maxItems = ((Number) playlist_meta.get("total")).intValue();
                        final Map<String, Object> pagination = (Map<String, Object>) playlist_meta.get("pagination");
                        maxPage = ((Number) pagination.get("maxPages")).intValue();
                        nextPage = ((Number) pagination.get("next")).intValue();
                        videos = (List<Map<String, Object>>) playlist.get("list");
                    } else if (pagesCategoryComponent != null) {
                        /* Channel */
                        final Map<String, Object> paginationProps = (Map<String, Object>) pagesCategoryComponent.get("paginationProps");
                        maxPage = ((Number) paginationProps.get("lastPageNumber")).intValue();
                        videos = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(pagesCategoryComponent, "trendingVideoListProps/videoThumbProps");
                        if (sponsorChannel != null) {
                            /* Channel */
                            maxItems = ((Number) sponsorChannel.get("videoCount")).intValue();
                        }
                        maxItemsPerPage = ((Number) entries.get("perPage")).intValue();
                    } else if (channelInfo != null) {
                        /* /creators/ link */
                        videos = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(entries, "trendingVideoSectionComponent/videoListProps/videoThumbProps");
                        maxItemsPerPage = ((Number) entries.get("perPage")).intValue();
                        final Map<String, Object> creatorInfo = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "infoComponent/pornstarTop");
                        maxItems = ((Number) creatorInfo.get("videoCount")).intValue();
                    }
                } catch (final Exception e) {
                    logger.info("json handling failed with exception");
                    break crawlJson;
                }
                if (videos == null || videos.isEmpty()) {
                    logger.info("Failed to find video items in json source");
                    break crawlJson;
                }
                for (final Map<String, Object> video : videos) {
                    final String video_id = video.get("id").toString();
                    final String title = video.get("title").toString();
                    final String icon = (String) video.get("icon");
                    final String url = (String) video.get("pageURL");
                    if ("deleted".equalsIgnoreCase(icon) || StringUtils.isEmpty(url)) {
                        /* Item is offline(deleted) or no longer have any pageURL/thumbURL/unageURL... assigned = offline/removed */
                        numberofDeletedItemsThisPage++;
                        logger.info("Found deleted video: id: " + video_id + " | title: " + title);
                        final String urlForOfflineItem;
                        if (!StringUtils.isEmpty(url)) {
                            urlForOfflineItem = url;
                        } else {
                            /* No URL given but we know how it would look */
                            urlForOfflineItem = "https://" + getHost() + "/videos/" + OrfAt.toSlug(title) + "-" + video_id;
                        }
                        if (!dupes.add(urlForOfflineItem)) {
                            /* This should never happen */
                            logger.warning("WTF found offline dupe: " + urlForOfflineItem);
                            continue;
                        }
                        final DownloadLink dummy = this.createDownloadlink(urlForOfflineItem);
                        /*
                         * A lot of deleted items have title "#deleted" so let's always include the video_id to make them easier to identify
                         * in the linkgrabber.
                         */
                        dummy.setFinalFileName(video_id + "_" + title + ".mp4");
                        dummy.setAvailable(false);
                        dummy._setFilePackage(fp);
                        ret.add(dummy);
                        distribute(dummy);
                        continue;
                    }
                    if (new Regex(url, ignoreVideo).patternFind()) {
                        continue;
                    } else if (!dupes.add(url)) {
                        /* Skip dupes by url */
                        continue;
                    } else if (!dupes.add(video_id)) {
                        /* Skip dupes by video_id */
                        continue;
                    }
                    final DownloadLink dl = this.createDownloadlink(url);
                    /* Set temp. name -> Will change once user starts downloading. */
                    dl.setName(video.get("title") + ".mp4");
                    dl.setAvailable(true);
                    dl._setFilePackage(fp);
                    ret.add(dl);
                    distribute(dl);
                    numberofNewItemsThisPage++;
                }
                logger.info("Json handling success: Items this page: " + videos.size());
            }
            /* Crawl links from html (legacy method) */
            final String[] urls = br.getRegex("(/videos/[^<>\"']+)").getColumn(0);
            if (urls != null && urls.length > 0) {
                for (String url : urls) {
                    url = br.getURL(url).toExternalForm();
                    if (new Regex(url, ignoreVideo).patternFind()) {
                        continue;
                    } else if (!hostPlugin.canHandle(url)) {
                        /* Skip items not supported by hosterplugin */
                        continue;
                    } else if (!dupes.add(url)) {
                        /* Skip dupes */
                        continue;
                    }
                    final DownloadLink dl = this.createDownloadlink(url);
                    /* Set temp. name -> Will change once user starts downloading. */
                    final String titleFromURL = url.replaceFirst("/videos/", "").replace("-", " ");
                    dl.setName(titleFromURL + ".mp4");
                    dl.setAvailable(true);
                    dl._setFilePackage(fp);
                    ret.add(dl);
                    distribute(dl);
                    numberofNewItemsThisPage++;
                }
            }
            numberofDeletedItemsTotal += numberofDeletedItemsThisPage;
            final int effectiveNumberofNewItemsThisPage = numberofNewItemsThisPage + numberofDeletedItemsThisPage;
            logger.info("Crawled page: " + page + "/" + maxPage + " | Found items so far: " + ret.size() + "/" + maxItems + " | Offline total: " + numberofDeletedItemsTotal + " | New this page: " + effectiveNumberofNewItemsThisPage + "/" + maxItemsPerPage + " | Offline this page: " + numberofDeletedItemsThisPage);
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                throw new InterruptedException();
            }
            if (page == maxPage) {
                logger.info("Stopping because: Reached last page: " + maxPage);
                break pagination;
            } else if (effectiveNumberofNewItemsThisPage == 0) {
                logger.info("Stopping because: Failed to find any new items on page: " + page);
                break pagination;
            }
            if (maxItemsPerPage != -1 && effectiveNumberofNewItemsThisPage < maxItemsPerPage) {
                /* This can happen and is not yet a reason to stop */
                logger.info("Current page contains less items than a full page | page: " + page + " | New items this page: " + effectiveNumberofNewItemsThisPage + "/" + maxItemsPerPage);
            }
            /* Try to continue to next page */
            page++;
            String nextpageurl = br.getRegex("class=\"xh-paginator-button[^\"]*\"[^>]*href=\"(https?://[^<>\"]+/" + page + ")\" data-page=\"" + page + "\">").getMatch(0);
            if (nextpageurl == null) {
                final String maybeNextPage = br.getURL().replaceFirst("/\\d*$", "") + "/" + page;
                if (br.containsHTML(Pattern.quote(maybeNextPage))) {
                    logger.info("Using guessed nextpageurl by html: " + maybeNextPage);
                    nextpageurl = maybeNextPage;
                } else if (maxPage != -1) {
                    /* If we know how many pages exist in total, we can just try to access the guessed next page URL. */
                    logger.info("Using guessed nextpageurl by maxPage: " + maybeNextPage);
                    nextpageurl = maybeNextPage;
                }
            }
            if (nextpageurl == null) {
                logger.info("Stopping because: Failed to find nextpage");
                break pagination;
            }
            /* Continue with next page */
            br.getPage(nextpageurl);
        } while (!this.isAbort());
        if (maxItems == 0) {
            /* Edge case: There are 0 elements to crawl */
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        } else if (maxItems != -1 && ret.size() < maxItems) {
            logger.info("Looks like some items are missing: " + (maxItems - ret.size()));
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlAllShortsOfUserOrCreator(final CryptedLink param) throws IOException, PluginException {
        final String username = new Regex(param.getCryptedUrl(), PATTERN_CREATOR_SHORTS).getMatch(1);
        final String type = new Regex(param.getCryptedUrl(), PATTERN_CREATOR_SHORTS).getMatch(0);
        if (username == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contenturl = XHamsterCom.getCorrectedURL(param.getCryptedUrl());
        br.getPage(contenturl);
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username);
        // fp.setPackageKey("xhamster://profile/" + username + "/shorts");
        int page = 1;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final HashSet<String> dupes = new HashSet<String>();
        pagination: do {
            logger.info("Crawling page: " + page);
            int newItemsThisPage = 0;
            final String[] urls = br.getRegex("(/moments/[a-z0-9-_]+)").getColumn(0);
            for (String url : new HashSet<String>(Arrays.asList(urls))) {
                if (!dupes.add(url)) {
                    continue;
                }
                newItemsThisPage++;
                url = br.getURL(url).toExternalForm();
                final DownloadLink link = this.createDownloadlink(url);
                link._setFilePackage(fp);
                ret.add(link);
                distribute(link);
            }
            logger.info("Crawled page: " + page + " | Found items so far: " + ret.size() + " | New this page: " + newItemsThisPage);
            page++;
            final String nextpageURL = br.getRegex("(/" + type + "/" + username + "/moments/" + page + ")").getMatch(0);
            if (nextpageURL == null) {
                logger.info("Stopping because: No nextpage available");
                break pagination;
            } else if (newItemsThisPage == 0) {
                logger.info("Stopping because: Failed to find any new items on current page");
                break pagination;
            }
            logger.info("Nextpage available: " + nextpageURL);
            br.getPage(nextpageURL);
        } while (!this.isAbort());
        return ret;
    }

    private ArrayList<DownloadLink> crawlAllGalleriesOfUserOrCreator(final CryptedLink param) throws IOException, PluginException {
        final String username = new Regex(param.getCryptedUrl(), PATTERN_USER_CREATOR_PHOTOS).getMatch(1);
        final String type = new Regex(param.getCryptedUrl(), PATTERN_USER_CREATOR_PHOTOS).getMatch(0);
        if (username == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contenturl = XHamsterCom.getCorrectedURL(param.getCryptedUrl());
        br.getPage(contenturl);
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username);
        int page = 1;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final HashSet<String> dupes = new HashSet<String>();
        pagination: do {
            logger.info("Crawling page: " + page);
            int newItemsThisPage = 0;
            final String[] urls = br.getRegex("(/photos/gallery/[a-z0-9\\-]+-\\d+)").getColumn(0);
            for (String url : new HashSet<String>(Arrays.asList(urls))) {
                if (!dupes.add(url)) {
                    continue;
                }
                newItemsThisPage++;
                url = br.getURL(url).toExternalForm();
                final DownloadLink link = this.createDownloadlink(url);
                ret.add(link);
                distribute(link);
            }
            logger.info("Crawled page: " + page + " | Found items so far: " + ret.size() + " | New this page: " + newItemsThisPage);
            page++;
            final String nextpageURL = br.getRegex("(/" + type + "/" + username + "/photos/" + page + ")").getMatch(0);
            if (nextpageURL == null) {
                logger.info("Stopping because: No nextpage available");
                break pagination;
            } else if (newItemsThisPage == 0) {
                logger.info("Stopping because: Failed to find any new items on current page");
                break pagination;
            }
            logger.info("Nextpage available: " + nextpageURL);
            br.getPage(nextpageURL);
        } while (!this.isAbort());
        return ret;
    }

    private ArrayList<DownloadLink> crawlPhotoGallery(final CryptedLink param) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = XHamsterCom.getCorrectedURL(param.getCryptedUrl());
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 410 || br.containsHTML("Sorry, no photos found|error\">\\s*Gallery not found\\s*<|>\\s*Page Not Found\\s*<")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*This gallery is visible for")) {
            throw new AccountRequiredException();
        }
        final String gallery_password_regex = ">\\s*This gallery (needs|requires) password\\s*<";
        if (br.containsHTML(gallery_password_regex)) {
            final boolean passwordHandlingBroken = true;
            if (passwordHandlingBroken) {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Password-protected handling broken svn.jdownloader.org/issues/88690");
            }
            boolean success = false;
            for (int i = 1; i <= 3; i++) {
                String passCode = getUserInput("Password?", param);
                br.postPage(br.getURL(), "password=" + Encoding.urlEncode(passCode));
                if (br.containsHTML(gallery_password_regex)) {
                    logger.info("User entered invalid password: " + passCode);
                    continue;
                } else {
                    success = true;
                    break;
                }
            }
            if (success) {
                throw new DecrypterException(DecrypterException.PASSWORD);
            }
        }
        if (new Regex(br.getURL(), "(?i)/gallery/[0-9]+/[0-9]+").patternFind()) {
            /* Single picture */
            final DownloadLink dl = createDownloadlink(DirectHTTP.createURLForThisPlugin(br.getRegex("class='slideImg'\\s+src='([^']+)").getMatch(0)));
            dl.setAvailable(true);
            ret.add(dl);
            return ret;
        }
        // final String total_numberof_picsStr = br.getRegex("<h1 class=\"gr\">[^<>]+<small>\\[(\\d+) [^<>\"]+\\]</small>").getMatch(0);
        final String total_numberof_picsStr = br.getRegex("\"photosCount\":(\\d+)").getMatch(0);
        logger.info("total_numberof_pics: " + total_numberof_picsStr);
        final int total_numberof_picsInt;
        if (total_numberof_picsStr != null) {
            total_numberof_picsInt = Integer.parseInt(total_numberof_picsStr);
        } else {
            total_numberof_picsInt = -1;
            logger.warning("Failed to find total number of images in this gallery");
        }
        final String galleryID = new Regex(contenturl, PATTERN_PHOTO_GALLERY).getMatch(1);
        String title = br.getRegex("<title>\\s*(.*?)\\s*\\-\\s*\\d+\\s*(Pics|Bilder)\\s*(?:\\-|\\|)\\s*xHamster(\\.com|\\.xxx|\\.desi|\\.one)?\\s*</title>").getMatch(0);
        if (title == null) {
            title = br.getRegex("<title>(.*?)</title>").getMatch(0);
        }
        /*
         * 2020-05-12: They often have different galleries with the exact same title --> Include galleryID so we do not get multiple
         * packages with the same title --> Then gets auto merged by default
         */
        if (title != null && !title.contains(galleryID)) {
            title = Encoding.htmlDecode(title).trim();
            title += "_" + galleryID;
        } else if (title == null) {
            /* Final fallback */
            title = galleryID;
        }
        /* Add name of uploader to the beginning of our packagename if possible */
        final String uploaderName = br.getRegex("/users/[^\"]+\"[^>]*class=\"link\">([^<>\"]+)<").getMatch(0);
        if (uploaderName != null && !title.contains(uploaderName)) {
            title = uploaderName + " - " + title;
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(Encoding.htmlDecode(title).trim());
        int page = 1;
        int maxItemsPerPage = -1;
        int image_position = 1;
        final HashSet<String> dupes = new HashSet<String>();
        pagination: while (!this.isAbort()) {
            int numberof_new_items_this_page = 0;
            String allLinks = br.getRegex("class='iListing'>(.*?)id='galleryInfoBox'>").getMatch(0);
            if (allLinks == null) {
                allLinks = br.getRegex("id='imgSized'(.*?)gid='\\d+").getMatch(0);
            }
            logger.info("Crawling page " + page);
            final String json_source = br.getRegex("\"photos\":(\\[\\{.*?\\}\\])").getMatch(0);
            // logger.info("json_source: " + json_source);
            if (json_source != null) {
                final List<Object> lines = (List) JavaScriptEngineFactory.jsonToJavaObject(json_source);
                for (final Object line : lines) {
                    // logger.info("line: " + line);
                    if (!(line instanceof Map)) {
                        continue;
                    }
                    final Map<String, Object> entries = (Map<String, Object>) line;
                    final String imageURL = (String) entries.get("imageURL");
                    if (imageURL == null) {
                        continue;
                    }
                    if (!dupes.add(imageURL)) {
                        /* Skip dupes */
                        continue;
                    }
                    numberof_new_items_this_page++;
                    final DownloadLink dl = createDownloadlink(DirectHTTP.createURLForThisPlugin(imageURL));
                    final String extension = getFileNameExtensionFromString(imageURL, ".jpg");
                    if (total_numberof_picsStr != null) {
                        dl.setFinalFileName(StringUtils.fillPre(Integer.toString(image_position), "0", total_numberof_picsStr.length()) + "_" + total_numberof_picsStr + extension);
                    } else {
                        dl.setFinalFileName(Integer.toString(image_position) + extension);
                    }
                    image_position++;
                    dl.setAvailable(true);
                    dl._setFilePackage(fp);
                    distribute(dl);
                    ret.add(dl);
                }
            }
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            }
            final String nextPage = br.getRegex("href=\"([^\"]+)\" rel=\"next\"[^>]*>").getMatch(0);
            if (StringUtils.isEmpty(nextPage)) {
                logger.info("Stopping because: Reached end");
                break pagination;
            } else if (total_numberof_picsInt != -1 && ret.size() >= total_numberof_picsInt) {
                logger.info("Stopping because: Found number of expected images");
                break pagination;
            } else if (numberof_new_items_this_page == 0) {
                /* Fail-safe */
                logger.info("Stopping because: Failed to find any new items on current page");
                break pagination;
            } else if (numberof_new_items_this_page < maxItemsPerPage) {
                /* Fail-safe */
                logger.info("Stopping because: Failed to find enough new items on current page");
                break pagination;
            }
            logger.info("Getting page " + nextPage);
            br.getPage(nextPage);
            if (br.getHttpConnection().getResponseCode() == 452 || br.containsHTML(">\\s*Page Not Found\\s*<")) {
                break;
            }
            page++;
        }
        if (total_numberof_picsInt != -1 && ret.size() < total_numberof_picsInt) {
            logger.warning("Seems like not all images have been found");
        }
        return ret;
    }

    @Override
    public boolean isProxyRotationEnabledForLinkCrawler() {
        return false;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}