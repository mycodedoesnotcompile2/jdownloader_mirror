//    jDownloader - Downloadmanager
//    Copyright (C) 2011  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.decrypter;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.plugins.components.config.DeviantArtComConfig;
import org.jdownloader.plugins.components.config.DeviantArtComConfig.ArtCrawlMode;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.hoster.DeviantArtCom;

@DecrypterPlugin(revision = "$Revision: 51286 $", interfaceVersion = 3, names = {}, urls = {})
public class DeviantArtComCrawler extends PluginForDecrypt {
    public DeviantArtComCrawler(PluginWrapper wrapper) {
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
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY };
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "deviantart.com" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            String regex = "https?://[\\w\\.\\-]*?" + buildHostsPatternPart(domains) + "(";
            regex += PATTERN_GALLERY.pattern();
            regex += "|";
            regex += PATTERN_GALLERY_2.pattern();
            regex += "|";
            regex += PATTERN_USER_FAVORITES.pattern();
            regex += "|";
            regex += PATTERN_USER_FAVORITES_GALLERY.pattern();
            regex += "|";
            regex += DeviantArtCom.PATTERN_ART.pattern();
            regex += "|";
            regex += PATTERN_USER.pattern();
            regex += ")";
            ret.add(regex);
        }
        return ret.toArray(new String[0]);
    }

    private static final Pattern PATTERN_USER                   = Pattern.compile("/(?!core-membership|search|developers|join|users)([\\w\\-]+)$", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_USER_FAVORITES         = Pattern.compile("/([\\w\\-]+)/favourites$", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_USER_FAVORITES_GALLERY = Pattern.compile("/([\\w\\-]+)/favourites/(\\d+)/([\\w\\-]+).*", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_GALLERY                = Pattern.compile("/([\\w\\-]+)/gallery/(\\d+)/([\\w\\-]+)$", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_GALLERY_2              = Pattern.compile("/([\\w\\-]+)/gallery/(all|scraps)$", Pattern.CASE_INSENSITIVE);
    DeviantArtCom                hosterplugin                   = null;

    @SuppressWarnings("deprecation")
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        final String path = new URL(contenturl).getPath();
        /* Login if possible. Sometimes not all items of a gallery are visible without being logged in. */
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        hosterplugin = (DeviantArtCom) this.getNewPluginForHostInstance(this.getHost());
        if (account != null) {
            hosterplugin.login(account, false);
        }
        final Regex favouritesGallery = new Regex(path, PATTERN_USER_FAVORITES_GALLERY);
        if (favouritesGallery.patternFind()) {
            return crawlFavouritesGallery(account, favouritesGallery.getMatch(0), favouritesGallery.getMatch(1), favouritesGallery.getMatch(2));
        } else if (new Regex(path, PATTERN_USER_FAVORITES).patternFind()) {
            return this.crawlProfileFavorites(account, param);
        } else if (new Regex(path, PATTERN_GALLERY).patternFind()) {
            return this.crawlProfileOrGallery(account, param);
        } else if (new Regex(path, PATTERN_GALLERY_2).patternFind()) {
            return this.crawlProfileOrGallery(account, param);
        } else if (new Regex(path, DeviantArtCom.PATTERN_ART).patternFind()) {
            return this.crawlArt(param, account);
        } else if (new Regex(path, PATTERN_USER).patternFind()) {
            return this.crawlProfileOrGallery(account, param);
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    /**
     * Crawls a collection of favourites of a user.
     *
     * @throws Exception
     */
    private ArrayList<DownloadLink> crawlFavouritesGallery(final Account account, final String username, final String galleryID, final String gallerySlug) throws Exception {
        if (username == null || galleryID == null) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        br.getPage("https://www." + this.getHost() + "/" + username + "/favourites/" + galleryID + "/" + gallerySlug);
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username + " - Favourites - " + gallerySlug.replace("-", " ").trim());
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("type", "collection");
        query.appendEncoded("folderid", "galleryID");
        query.appendEncoded("username", username);
        query.appendEncoded("folderid", galleryID);
        return crawlPagination(account, fp, "/_puppy/dashared/gallection/contents", query);
    }

    /**
     * Crawls all favourites of a user.
     *
     * @throws Exception
     */
    private ArrayList<DownloadLink> crawlProfileFavorites(final Account account, final CryptedLink param) throws Exception {
        final String username = new Regex(param.getCryptedUrl(), PATTERN_USER_FAVORITES).getMatch(0);
        if (username == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(param.getCryptedUrl());
        if (this.br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username + " - Favorites");
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("type", "collection");
        // query.add("folderid", "");
        query.appendEncoded("username", username);
        query.appendEncoded("all_folder", "true");
        return this.crawlPagination(account, fp, "/_puppy/dashared/gallection/contents", query);
    }

    private ArrayList<DownloadLink> crawlProfileOrGallery(final Account account, final CryptedLink param) throws Exception {
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String username = new Regex(br.getURL(), "https?://[^/]+/([^/\\?]+)").getMatch(0);
        final Regex gallery = new Regex(br.getURL(), PATTERN_GALLERY);
        String galleryID = null;
        String gallerySlug = null;
        if (gallery.patternFind()) {
            galleryID = gallery.getMatch(1);
            gallerySlug = gallery.getMatch(2);
        } else {
            gallerySlug = new Regex(br.getURL(), "/gallery/(all|scraps)").getMatch(0);
        }
        if (username == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (gallerySlug != null) {
            fp.setName(username + " - " + gallerySlug.replace("-", " ").trim());
        } else {
            fp.setName(username);
        }
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("type", "gallery");
        query.appendEncoded("username", username);
        if (galleryID != null) {
            query.appendEncoded("folderid", galleryID);
        } else {
            if ("scraps".equalsIgnoreCase(gallerySlug)) {
                query.appendEncoded("scraps_folder", "true");
            } else {
                query.appendEncoded("all_folder", "true");
            }
        }
        return crawlPagination(account, fp, "/_puppy/dashared/gallection/contents", query);
    }

    private ArrayList<DownloadLink> crawlPagination(final Account account, final FilePackage fp, final String action, final UrlQuery query) throws Exception {
        String csrftoken = br.getRegex("window\\.__CSRF_TOKEN__\\s*=\\s*'([^<>\"\\']+)';").getMatch(0);
        if (csrftoken == null) {
            csrftoken = br.getRegex("csrfToken\\s*:\\s*\"([^\"]+)").getMatch(0);
        }
        if (StringUtils.isEmpty(csrftoken)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final DeviantArtComConfig cfg = PluginJsonConfig.get(DeviantArtComConfig.class);
        final ArtCrawlMode mode = cfg.getArtCrawlMode();
        final boolean fastcrawl = mode == ArtCrawlMode.FAST_CRAWL_IGNORE_MULTI_IMAGE_GALLERIES;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        int page = 0;
        final int maxItemsPerPage = 24;
        int offset = 0;
        final HashSet<String> dupes = new HashSet<String>();
        query.appendEncoded("limit", Integer.toString(maxItemsPerPage));
        query.appendEncoded("csrf_token", csrftoken);
        pagination: do {
            query.addAndReplace("offset", Integer.toString(offset));
            page++;
            br.getPage(action + "?" + query.toString());
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Number nextOffset = (Number) entries.get("nextOffset");
            final List<Map<String, Object>> results = (List<Map<String, Object>>) entries.get("results");
            if (results.isEmpty()) {
                if (ret.isEmpty()) {
                    if (Boolean.TRUE.equals(entries.get("hasMore")) && account == null) {
                        throw new AccountRequiredException();
                    } else {
                        logger.info("This item doesn't contain any items");
                        throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
                    }
                } else {
                    logger.info("Stopping because: Current page doesn't contain any items");
                    break;
                }
            }
            int numberofNewItems = 0;
            for (final Map<String, Object> result : results) {
                Map<String, Object> deviation = (Map<String, Object>) result.get("deviation");
                if (deviation == null) {
                    /* 2023-09-23 */
                    deviation = result;
                }
                final String url = deviation.get("url").toString();
                if (!dupes.add(url)) {
                    /* Skip duplicates */
                    continue;
                }
                numberofNewItems++;
                final DownloadLink link = this.createDownloadlink(url);
                // TODO: Correctly implement this
                DeviantArtCom.parseDeviationJSON(this, link, deviation);
                hosterplugin.setFilename(link, account, null);
                /**
                 * This file extension may change later when file is downloaded. </br>
                 * 2022-11-11: Items of type "literature" (or simply != "image") will not get any file extension at all at this moment.
                 */
                if (fastcrawl) {
                    link.setAvailable(true);
                }
                if (fp != null) {
                    link._setFilePackage(fp);
                }
                ret.add(link);
                distribute(link);
            }
            logger.info("Crawled page " + page + " | Offset: " + offset + " | nextOffset: " + nextOffset + " | Found items so far: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break pagination;
            } else if (!Boolean.TRUE.equals(entries.get("hasMore"))) {
                logger.info("Stopping because: Reached end");
                break pagination;
            } else if (nextOffset == null) {
                logger.info("Stopping because: Reached end?");
                break pagination;
            } else if (numberofNewItems == 0) {
                /* Extra fail-safe */
                logger.info("Stopping because: Failed to find new items on page " + page);
                break pagination;
            }
            /* Continue to next page */
            offset = nextOffset.intValue();
            continue;
        } while (true);
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlArt(final CryptedLink param, final Account account) throws Exception {
        final DeviantArtComConfig cfg = PluginJsonConfig.get(DeviantArtComConfig.class);
        final String contenturl = param.getCryptedUrl();
        final DownloadLink mainlink = this.createDownloadlink(contenturl);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        ret.add(mainlink);
        if (cfg.getArtCrawlMode() == ArtCrawlMode.FAST_CRAWL_IGNORE_MULTI_IMAGE_GALLERIES) {
            /* Pass item to hosterplugin straight away. */
            return ret;
        }
        hosterplugin.requestFileInformation(mainlink, account);
        mainlink.setAvailable(true);
        if (!DeviantArtCom.isImage(mainlink)) {
            /* Item is not an image item -> No need to look for more image items. */
            return ret;
        }
        mainlink.setProperty(DeviantArtCom.PROPERTY_IMAGE_POSITION, 1);
        int position = 1;
        final String[] propertiesToCopy = new String[] { DeviantArtCom.PROPERTY_USERNAME, DeviantArtCom.PROPERTY_TITLE, DeviantArtCom.PROPERTY_TYPE };
        findAdditionalMedias: try {
            String json = br.getRegex("window\\.__INITIAL_STATE__ = JSON\\.parse\\(\"(.*?)\"\\);").getMatch(0);
            json = PluginJSonUtils.unescape(json);
            final Map<String, Object> entries = restoreFromString(json, TypeRef.MAP);
            final List<Map<String, Object>> additionalMedias = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(entries, "@@entities/deviationExtended/{0}/additionalMedia");
            if (additionalMedias == null) {
                logger.info("Failed to find any images");
                break findAdditionalMedias;
            }
            logger.info("Number of additionalMedia: " + additionalMedias.size());
            final boolean isImage = true;
            final boolean isVideo = false;
            /* Start at position 2 since these "extra" items do not contain the main image item. */
            position = 2;
            for (final Map<String, Object> additionalMedia : additionalMedias) {
                final Number filesize = (Number) additionalMedia.get("filesize");
                final Map<String, Object> media = (Map<String, Object>) additionalMedia.get("media");
                final DownloadLink image = this.createDownloadlink(contenturl);
                /* Inherit some properties from main link. */
                for (final String property : propertiesToCopy) {
                    image.setProperty(property, mainlink.getProperty(property));
                }
                image.setProperty(DeviantArtCom.PROPERTY_IMAGE_POSITION, position);
                final String baseUri = (String) media.get("baseUri");
                final String prettyName = (String) media.get("prettyName");
                final List<Map<String, Object>> types = (List<Map<String, Object>>) media.get("types");
                Map<String, Object> bestType = null;
                final List<String> bestTypesList;
                if (isImage) {
                    bestTypesList = Arrays.asList(new String[] { "fullview" });
                } else if (isVideo) {
                    bestTypesList = Arrays.asList(new String[] { "video" });
                } else {
                    bestTypesList = new ArrayList<String>(0);
                }
                typeStringLoop: for (final String typeString : bestTypesList) {
                    for (final Map<String, Object> type : types) {
                        if (typeString.equals(type.get("t"))) {
                            if (isImage) {
                                bestType = type;
                                break typeStringLoop;
                            } else if (isVideo) {
                                if (bestType == null || ((Number) type.get("h")).intValue() > ((Number) bestType.get("h")).intValue()) {
                                    bestType = type;
                                }
                            }
                        }
                    }
                }
                if (bestType == null) {
                    continue;
                }
                String displayedImageURL = null;
                Number unlimitedImageSize = null;
                String displayedVideoURL = null;
                Number displayedVideoSize = null;
                if (isImage) {
                    String c = (String) bestType.get("c");
                    if (c == null) {
                        if ("fullview".equals(bestType.get("t"))) {
                            // r=1? o=true??(maybe original)
                            c = "";// raw image without any processing?
                        } else {
                            final Number h = (Number) bestType.get("h");
                            final Number w = (Number) bestType.get("w");
                            if (h != null && w != null) {
                                c = "/v1/fit/w_" + w + ",h_" + h + "/";
                            }
                        }
                    }
                    if (c != null) {
                        c = c.replaceFirst(",q_\\d+(,strp)?", "");
                        final List<String> tokens = (List<String>) media.get("token");
                        final String token = tokens.get(0);
                        image.setProperty(DeviantArtCom.PROPERTY_IMAGE_TOKEN, token);
                        displayedImageURL = baseUri + c.replaceFirst("<prettyName>", Matcher.quoteReplacement(prettyName));
                        displayedImageURL = displayedImageURL + "?token=" + token;
                    }
                } else if (isVideo) {
                    displayedVideoURL = (String) bestType.get("b");
                    displayedVideoSize = (Number) bestType.get("f");
                }
                if (isImage && displayedImageURL == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                } else if (isVideo && displayedVideoURL == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                if (isImage) {
                    image.setProperty(DeviantArtCom.PROPERTY_IMAGE_DISPLAY_OR_PREVIEW_URL_2, displayedImageURL);
                } else {
                    // TODO
                    image.setProperty(DeviantArtCom.PROPERTY_VIDEO_DISPLAY_OR_PREVIEW_URL, displayedVideoURL);
                }
                if (filesize != null) {
                    image.setDownloadSize(filesize.longValue());
                }
                image.setProperty("displayedImageURL", displayedImageURL);
                image.setProperty("unlimitedImageSize", unlimitedImageSize);
                image.setProperty("displayedVideoURL", displayedVideoURL);
                image.setProperty("displayedVideoSize", displayedVideoSize);
                position++;
                ret.add(image);
            }
        } catch (final Throwable e) {
            logger.log(e);
            logger.warning("additionalMedias handling failed");
        }
        if (ret.size() == 1) {
            logger.info("Found single item");
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(mainlink.getStringProperty(DeviantArtCom.PROPERTY_TITLE));
        for (final DownloadLink result : ret) {
            result.setContentUrl(br.getURL());
            if (ret.size() > 1) {
                result._setFilePackage(fp);
            }
            result.setProperty(DeviantArtCom.PROPERTY_IMAGE_POSITION_MAX, ret.size());
            result.setAvailable(true);
            this.hosterplugin.setFilename(result, account, null);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}