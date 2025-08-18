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

@DecrypterPlugin(revision = "$Revision: 51338 $", interfaceVersion = 3, names = {}, urls = {})
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

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            final StringBuilder sb = new StringBuilder();
            sb.append("https?://(?:[a-z0-9\\-]+\\.)?" + buildHostsPatternPart(domains));
            sb.append("/(");
            sb.append("photos/gallery/[0-9A-Za-z_\\-/]+-\\d+");
            sb.append("|my/favorites/videos(?:/[a-f0-9]{24}-[\\w\\-]+)?");
            sb.append("|users/(?:profiles/)?[^/]+/videos");
            sb.append("|users/(?:profiles/)?[^/]+/shorts");
            sb.append("|users/(?:profiles/)?[^/]+/favorites/videos");
            sb.append("|users/(?:profiles/)?[^/]+/photos");
            sb.append("|creators/[^/]+/photos");
            sb.append("|creators/[^/]+/shorts");
            sb.append("|channels/[^/]+");
            sb.append("|(?:[^/]+/)?pornstars/[^/]+");
            sb.append("|(?:[^/]+/)?creators/[^/]+");
            sb.append(")");
            ret.add(sb.toString());
        }
        return ret.toArray(new String[0]);
    }

    private static final String TYPE_PHOTO_GALLERY                   = "(?i)https?://[^/]+/photos/gallery/[0-9A-Za-z_\\-/]+-(\\d+)";
    private static final String TYPE_FAVORITES_OF_CURRENT_USER       = "(?i)https?://[^/]+/my/favorites/videos(/[a-f0-9]{24}-([\\w\\-]+))?";
    private static final String TYPE_VIDEOS_OF_USER                  = "(?i)https?://[^/]+/users/(?:profiles/)?([^/]+)/videos";
    private static final String TYPE_VIDEOS_FAVORITES_OF_USER        = "(?i)https?://[^/]+/users/(?:profiles/)?([^/]+)/favorites/videos";
    private static final String TYPE_PHOTO_GALLERIES_OF_USER_CREATOR = "(?i)https?://[^/]+/(creators|users)/(?:profiles/)?([^/]+)/photos";
    private static final String TYPE_SHORTS_OF_USER_CREATOR          = "(?i)https?://[^/]+/(creators|users)/(?:profiles/)?([^/]+)/shorts";
    private static final String TYPE_VIDEOS_OF_CHANNEL               = "(?i)https?://[^/]+/channels/([^/]+)";
    private static final String TYPE_VIDEOS_OF_USER_PORNSTAR         = "(?i)https?://[^/]+/(?:[^/]+/)?pornstars/([^/]+)";
    private static final String TYPE_VIDEOS_OF_USER_CREATOR          = "(?i)https?://[^/]+/(?:[^/]+/)?creators/([^/]+)";

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
        if (param.getCryptedUrl().matches(TYPE_VIDEOS_OF_USER)) {
            /* Crawl all videos of a user */
            return crawlUserProfile(param);
        } else if (param.getCryptedUrl().matches(TYPE_VIDEOS_FAVORITES_OF_USER)) {
            /* Crawl all videos of a user */
            return crawlUserProfileFavorites(param);
        } else if (param.getCryptedUrl().matches(TYPE_VIDEOS_OF_USER_PORNSTAR)) {
            /* Crawl all videos of a pornstar profile */
            return this.crawlUserProfilePornstar(param);
        } else if (param.getCryptedUrl().matches(TYPE_VIDEOS_OF_USER_CREATOR)) {
            /* Crawl all videos of a creator profile */
            return this.crawlUserProfileCreator(param);
        } else if (param.getCryptedUrl().matches(TYPE_VIDEOS_OF_CHANNEL)) {
            /* Crawl all videos of a channel */
            return crawlChannel(param);
        } else if (param.getCryptedUrl().matches(TYPE_FAVORITES_OF_CURRENT_USER)) {
            /* Crawl users own favorites */
            return this.crawlUserFavorites(param, account);
        } else if (param.getCryptedUrl().matches(TYPE_PHOTO_GALLERIES_OF_USER_CREATOR)) {
            /* Crawl all photo galleries of a user/creator --> Goes back into crawler and crawler will crawl the single photos */
            return crawlAllGalleriesOfUserOrCreator(param);
        } else if (param.getCryptedUrl().matches(TYPE_SHORTS_OF_USER_CREATOR)) {
            /* Crawl all shorts/moments of a user/creator --> Goes back into crawler and crawler will crawl the single photos */
            return crawlAllShortsOfUserOrCreator(param);
        } else {
            /* Single Photo gallery */
            return this.crawlPhotoGallery(param);
        }
    }

    private ArrayList<DownloadLink> crawlUserProfile(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException {
        final String username = new Regex(param.getCryptedUrl(), TYPE_VIDEOS_OF_USER).getMatch(0);
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

    private ArrayList<DownloadLink> crawlUserProfileFavorites(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException {
        final String username = new Regex(param.getCryptedUrl(), TYPE_VIDEOS_FAVORITES_OF_USER).getMatch(0);
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

    private ArrayList<DownloadLink> crawlUserProfilePornstar(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException {
        final String username = new Regex(param.getCryptedUrl(), TYPE_VIDEOS_OF_USER_PORNSTAR).getMatch(0);
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

    private ArrayList<DownloadLink> crawlUserProfileCreator(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException {
        final String username = new Regex(param.getCryptedUrl(), TYPE_VIDEOS_OF_USER_CREATOR).getMatch(0);
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

    private ArrayList<DownloadLink> crawlChannel(final CryptedLink param) throws IOException, PluginException, DecrypterRetryException {
        final String channelname = new Regex(param.getCryptedUrl(), TYPE_VIDEOS_OF_CHANNEL).getMatch(0);
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
    private ArrayList<DownloadLink> crawlUserFavorites(final CryptedLink param, final Account account) throws IOException, PluginException, DecrypterRetryException {
        if (account == null) {
            throw new AccountRequiredException();
        }
        final String favoritesName = new Regex(param.getCryptedUrl(), TYPE_FAVORITES_OF_CURRENT_USER).getMatch(1);
        if (favoritesName == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String contenturl = XHamsterCom.getCorrectedURL(param.getCryptedUrl());
        br.getPage(contenturl);
        final FilePackage fp = FilePackage.getInstance();
        fp.setName("Favorites - " + favoritesName);
        final ArrayList<DownloadLink> ret = this.crawlPagination(param, fp);
        if (ret.isEmpty()) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        return ret;
    }

    /* Crawls all videos of all pages in given browsers' html. */
    private ArrayList<DownloadLink> crawlPagination(final CryptedLink param, final FilePackage fp) throws IOException, PluginException {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final HashSet<String> dupes = new HashSet<String>();
        int page = 1;
        int maxPage = -1;
        final String[] pageNums = br.getRegex("data-page=\"(\\d+)\"").getColumn(0);
        if (pageNums != null && pageNums.length > 0) {
            for (final String pageNumStr : pageNums) {
                final int pageNumTmp = Integer.parseInt(pageNumStr);
                if (pageNumTmp > maxPage) {
                    maxPage = pageNumTmp;
                }
            }
        }
        final Pattern ignoreVideo = Pattern.compile("(?i).*/videos/[a-f0-9]{24}-watch-later.*");
        do {
            final String[] urlParts = br.getRegex("(/videos/[^<>\"']+)").getColumn(0);
            if (urlParts == null || urlParts.length == 0) {
                logger.info("Stopping because: Failed to find any items on current page");
                break;
            }
            int numberofNewItems = 0;
            for (String urlPart : urlParts) {
                if (new Regex(urlPart, ignoreVideo).patternFind()) {
                    continue;
                } else if (!dupes.add(urlPart)) {
                    /* Skip dupes */
                    continue;
                }
                numberofNewItems++;
                final DownloadLink dl = this.createDownloadlink(br.getURL(urlPart).toString());
                /* Set temp. name -> Will change once user starts downloading. */
                final String titleFromURL = urlPart.replaceFirst("/videos/", "").replace("-", " ");
                dl.setName(titleFromURL + ".mp4");
                dl.setAvailable(true);
                dl._setFilePackage(fp);
                ret.add(dl);
                distribute(dl);
            }
            logger.info("Crawled page: " + page + "/" + maxPage + " | Found items on this page: " + numberofNewItems + " | Total: " + ret.size());
            page++;
            String nextpageurl = br.getRegex("class=\"xh-paginator-button[^\"]*\"[^>]*href=\"(https?://[^<>\"]+/" + page + ")\" data-page=\"" + page + "\">").getMatch(0);
            if (nextpageurl == null) {
                final String maybeNextPage = br.getURL().replaceFirst("/\\d*$", "") + "/" + page;
                if (br.containsHTML(Pattern.quote(maybeNextPage))) {
                    logger.info("Using slightly corrected nextpageurl: " + maybeNextPage);
                    nextpageurl = maybeNextPage;
                }
            }
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                break;
            } else if (nextpageurl == null) {
                logger.info("Stopping because: Failed to find nextpage");
                break;
            } else if (numberofNewItems == 0) {
                logger.info("Stopping because: Failed to find any new items on page: " + page);
                break;
            } else {
                /* Continue with next page */
                br.getPage(nextpageurl);
            }
        } while (!this.isAbort());
        return ret;
    }

    private ArrayList<DownloadLink> crawlAllShortsOfUserOrCreator(final CryptedLink param) throws IOException, PluginException {
        final String username = new Regex(param.getCryptedUrl(), TYPE_SHORTS_OF_USER_CREATOR).getMatch(1);
        final String type = new Regex(param.getCryptedUrl(), TYPE_SHORTS_OF_USER_CREATOR).getMatch(0);
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
        final String username = new Regex(param.getCryptedUrl(), TYPE_PHOTO_GALLERIES_OF_USER_CREATOR).getMatch(1);
        final String type = new Regex(param.getCryptedUrl(), TYPE_PHOTO_GALLERIES_OF_USER_CREATOR).getMatch(0);
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
        final String galleryID = new Regex(contenturl, TYPE_PHOTO_GALLERY).getMatch(0);
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
                /* Last resort fail-safe to prevent infinite loops */
                logger.info("Stopping because: Failed to find any new items on current page");
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