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
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 51316 $", interfaceVersion = 3, names = {}, urls = {})
public class SexComCrawler extends PornEmbedParser {
    public SexComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "sex.com" });
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

    private static final String PATTERN_RELATIVE_EXTERN_REDIRECT = "(?i)/link/out\\?id=\\d+";
    private static final String PATTERN_RELATIVE_USER            = "(?i)/user/([a-z0-9\\-]+)/([a-z0-9\\-]+)/";
    private static final String PATTERN_RELATIVE_PIN             = "(?i)/pin/\\d+(-[a-z0-9\\-]+)?/";
    private static final String PATTERN_RELATIVE_PIN_NEW         = "(?i)/[a-z]{2}/pics/\\d+";
    private static final String PATTERN_RELATIVE_PICTURE         = "(?i)/picture/\\d+/?";
    private static final String PATTERN_RELATIVE_GIFS            = "(?i)/(?:[a-z]{2}/)?gifs/\\d+/?";
    private static final String PATTERN_RELATIVE_SHORT           = "(?i)/(?:[a-z]{2}/)?shorts/(?:creator/)?(([\\w\\-]+)/video/([\\w\\-]+))";

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_RELATIVE_SHORT + "|" + PATTERN_RELATIVE_USER + "|" + PATTERN_RELATIVE_PIN + "|" + PATTERN_RELATIVE_PIN_NEW + "|" + PATTERN_RELATIVE_PICTURE + "|" + PATTERN_RELATIVE_GIFS + "|" + PATTERN_RELATIVE_EXTERN_REDIRECT + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String externID;
        String title;
        br.setAllowedResponseCodes(502);
        final String contenturl = param.getCryptedUrl().replaceFirst("^(?i)http://", "https://");
        String redirect = null;
        if (contenturl.matches(PATTERN_RELATIVE_EXTERN_REDIRECT)) {
            /* Single link which redirects to another website */
            br.setFollowRedirects(false);
            br.getPage(contenturl);
            redirect = this.br.getRedirectLocation();
            ret.add(this.createDownloadlink(redirect));
            return ret;
        }
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (isOffline(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String contenturl_path = new URL(contenturl).getPath();
        redirect = br.getRegex("onclick=\"window\\.location\\.href=\\'(/[^<>\"]*?)\\'").getMatch(0);
        if (redirect != null) {
            br.getPage(redirect);
        }
        final Pattern userpatternfull = Pattern.compile("https?://[^/]+" + PATTERN_RELATIVE_USER);
        final Pattern shortspatternfull = Pattern.compile("https?://[^/]+" + PATTERN_RELATIVE_SHORT);
        final Regex shortsRegex;
        if (new Regex(contenturl, userpatternfull).patternFind()) {
            /* Find all items of profile. Those can be spread across multiple pages -> Handle pagination */
            /* Example: http://www.sex.com/user/sanje/sexy-vika/ */
            if (!this.canHandle(br.getURL())) {
                /* E.g. redirect to mainpage */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Set<String> dupes = new HashSet<String>();
            final String userProfilePin = br.getRegex("\"user_profile_picture\"\\s*>\\s*<a\\s*href\\s*=\\s*\"(/pin/\\d+)").getMatch(0);
            dupes.add(userProfilePin);
            int page = 1;
            do {
                int numberofNewItems = 0;
                final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
                for (final String url : urls) {
                    if (this.canHandle(url) && !new Regex(url, userpatternfull).patternFind() && dupes.add(url)) {
                        final DownloadLink dl = createDownloadlink(br.getURL(url).toExternalForm());
                        ret.add(dl);
                        distribute(dl);
                        numberofNewItems++;
                    }
                }
                logger.info("Crawled page " + page + " | Found items so far: " + ret.size());
                final String next = br.getRegex("rel\\s*=\\s*\"next\"\\s*href\\s*=\\s*\"(https?://[^\"]*page=\\d+)").getMatch(0);
                if (this.isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    break;
                } else if (numberofNewItems == 0) {
                    logger.info("Stopping because: Failed to find any new items on current page");
                    break;
                } else if (next == null) {
                    logger.info("Stopping because: Failed to find nextpage");
                    break;
                } else {
                    /* Continue to next page */
                    br.getPage(next);
                    page++;
                }
            } while (!this.isAbort());
        } else if ((shortsRegex = new Regex(contenturl, shortspatternfull)).patternFind()) {
            if (!this.canHandle(br.getURL())) {
                /* E.g. redirect to mainpage */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final String shortspath = shortsRegex.getMatch(0);
            final String shortstitle = shortsRegex.getMatch(2).replace("-", " ").trim();
            br.getPage("https://shorts.sex.com/api/media/getMedia?relativeUrl=" + Encoding.urlEncode(shortspath));
            /* 2024-06-06: Alternative: https://iframe.sex.com/api/media/getMedia?relativeUrl= */
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Map<String, Object> media = (Map<String, Object>) entries.get("media");
            /* Check if account is required to access content. */
            if (Boolean.TRUE.equals(media.get("subscriptionRequired"))) {
                throw new AccountRequiredException();
            } else if (Boolean.TRUE.equals(media.get("mediaPurchaseRequired"))) {
                throw new AccountRequiredException();
            }
            final List<Map<String, Object>> sources = (List<Map<String, Object>>) media.get("sources");
            final ArrayList<DownloadLink> blurredVideos = new ArrayList<DownloadLink>();
            for (final Map<String, Object> source : sources) {
                final String url = source.get("fullPath").toString();
                final DownloadLink video = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
                /* Referer header is important for downloading! */
                video.setReferrerUrl(param.getCryptedUrl());
                video.setAvailable(true);
                if (StringUtils.containsIgnoreCase(url, "blurred")) {
                    video.setFinalFileName(shortstitle + "_blurred.mp4");
                    blurredVideos.add(video);
                } else {
                    video.setFinalFileName(shortstitle + ".mp4");
                    ret.add(video);
                }
            }
            if (ret.size() == 0 && blurredVideos.size() > 0) {
                /* Only add blurred videos if no other items were found */
                ret.addAll(blurredVideos);
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(shortspath);
            fp.setPackageKey("sex_com_shorts://" + shortspath);
            fp.addLinks(ret);
            return ret;
        } else if (new Regex(contenturl_path, PATTERN_RELATIVE_PIN).patternFind() || new Regex(contenturl_path, PATTERN_RELATIVE_PIN_NEW).patternFind() || new Regex(contenturl_path, PATTERN_RELATIVE_GIFS).patternFind()) {
            /* "PIN" item */
            if (!this.canHandle(br.getURL())) {
                /**
                 * E.g. redirect to mainpage, example: <br>
                 * /pin/64729322-fucking-her-with-that-big-cock/
                 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            title = br.getRegex("<title>\\s*([^<>\"]*?)\\s*(?:\\|?\\s*(Sex Videos and Pictures|Gif)\\s*\\|\\s*Sex\\.com)?\\s*</title>").getMatch(0);
            if (title == null || title.length() <= 2) {
                title = br.getRegex("addthis:title=\"([^<>\"]*?)\"").getMatch(0);
            }
            if (title == null || title.length() <= 2) {
                title = br.getRegex("property=\"og:title\" content=\"([^<>]*?)\\-  Pin #\\d+ \\| Sex\\.com\"").getMatch(0);
            }
            if (title == null || title.length() <= 2) {
                title = br.getRegex("<div class=\"pin\\-header navbar navbar\\-static\\-top\">[\t\n\r ]+<div class=\"navbar\\-inner\">[\t\n\r ]+<h1>([^<>]*?)</h1>").getMatch(0);
            }
            if (title == null || title.length() <= 2) {
                title = new Regex(param.getCryptedUrl(), "(\\d+)/?$").getMatch(0);
            }
            final String name = br.getRegex("(?:Picture|Video|Gif)\\s*-\\s*<span itemprop\\s*=\\s*\"name\"\\s*>\\s*(.*?)\\s*</span>").getMatch(0);
            if (name != null) {
                title = name;
            }
            title = Encoding.htmlDecode(title).trim();
            title = title.replace("#", "");
            title = title.replaceFirst("(?i) \\| Sex\\.com", "");
            externID = br.getRegex("<div class=\"from\">From <a rel=\"nofollow\" href=\"(https?://[^<>\"]*?)\"").getMatch(0);
            if (externID != null) {
                ret.add(createDownloadlink(externID));
                return ret;
            }
            externID = br.getRegex("<link rel=\"image_src\" href=\"(http[^<>\"]*?)\"").getMatch(0);
            if (externID == null) {
                /* 2025-07-04: For normal/.jpg images */
                externID = br.getRegex("=\"pin-carousel-image\" src=\"(https?://[^\"]+)").getMatch(0);
            }
            if (externID == null) {
                // For .gif images
                externID = br.getRegex("<link rel=\"preload\" as=\"image\" href=\"(http[^<>\"]*?\\.gif)\"").getMatch(0);
            }
            // For .gif images
            if (externID == null) {
                externID = br.getRegex("<div class=\"image_frame\"[^<>]*>\\s*(?:<[^<>]*>)?\\s*<img alt=[^<>]*?src=\"(https?://[^<>\"]*?)\"").getMatch(0);
            }
            if (externID == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            /* Fix encoding */
            externID = Encoding.htmlOnlyDecode(externID);
            final DownloadLink dl = createDownloadlink(DirectHTTP.createURLForThisPlugin(externID));
            final String ext = Plugin.getFileNameExtensionFromURL(externID, ".webp");
            // final String filePath = new URL(externID).getPath();
            dl.setContentUrl(contenturl);
            dl.setFinalFileName(this.applyFilenameExtension(title, ext));
            /* 2023-01-04: Add custom header to prefer .webp image (same way browser is doing it). */
            final ArrayList<String[]> customHeaders = new ArrayList<String[]>();
            customHeaders.add(new String[] { "Accept", "image/avif,image/webp,image/apng,image/svg+xml,image/*,*/*;q=0.8" });
            dl.setProperty(DirectHTTP.PROPERTY_HEADERS, customHeaders);
            dl.setAvailable(true);
            ret.add(dl);
            return ret;
        } else {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    @Override
    protected boolean isOffline(final Browser br) {
        final int responseCode = br.getHttpConnection().getResponseCode();
        if (responseCode == 404 || responseCode == 502) {
            return true;
        } else {
            return false;
        }
    }
}