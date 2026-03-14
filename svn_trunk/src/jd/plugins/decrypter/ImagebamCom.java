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

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52493 $", interfaceVersion = 3, names = {}, urls = {})
public class ImagebamCom extends PluginForDecrypt {
    public ImagebamCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "imagebam.com" });
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

    /** Path-based patterns; all start with "/" for substring(1) compatibility. */
    public static final Pattern PATTERN_THUMBNAIL     = Pattern.compile("/\\d+/[a-z0-9]+/[a-z0-9]+/([a-z0-9]+)\\.[a-z]{3,5}$", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_THUMBNAIL_NEW = Pattern.compile("/\\d+/[a-z0-9]+/[a-z0-9]+/([a-z0-9]+)_t\\.[a-z]{3,5}$", Pattern.CASE_INSENSITIVE);
    /* 2026-03-09: TODO: Check if links matching PATTERN_IMAGE still exist. */
    public static final Pattern PATTERN_IMAGE         = Pattern.compile("/image/([a-z0-9]+)", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_VIEW          = Pattern.compile("/view/([A-Za-z0-9]+)", Pattern.CASE_INSENSITIVE);
    public static final Pattern PATTERN_GALLERY       = Pattern.compile("/gallery/([a-z0-9]+)/?", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            final String hostsPattern = buildHostsPatternPart(domains);
            /* Main host: /image/, /gallery/, /view/ paths */
            String pattern = "https?://(?:www\\.)?" + hostsPattern + "/(";
            pattern += PATTERN_IMAGE.pattern().substring(1);
            pattern += "|" + PATTERN_GALLERY.pattern().substring(1);
            pattern += "|" + PATTERN_VIEW.pattern().substring(1);
            pattern += ")";
            /* Thumbnail CDN host: thumbsN.imagebam.com — domain included to avoid matching foreign hosts */
            final String thumbPrefix = "|https?://thumbs\\d+\\." + hostsPattern;
            pattern += thumbPrefix + PATTERN_THUMBNAIL.pattern();
            pattern += thumbPrefix + PATTERN_THUMBNAIL_NEW.pattern();
            ret.add(pattern);
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setFollowRedirects(true);
        final String url = param.getCryptedUrl();
        final String host = this.getHost();
        final String thumbPrefix = "https?://thumbs\\d+\\." + Pattern.quote(host);
        final String thumbnailFullPattern = thumbPrefix + PATTERN_THUMBNAIL.pattern();
        final String thumbnailNewFullPattern = thumbPrefix + PATTERN_THUMBNAIL_NEW.pattern();
        String id = new Regex(url, thumbnailFullPattern).getMatch(0);
        if (id != null) {
            /* Rewrite thumbnail to fullImage link */
            ret.add(this.createDownloadlink("https://www." + host + "/image/" + id));
            return ret;
        }
        id = new Regex(url, thumbnailNewFullPattern).getMatch(0);
        if (id != null) {
            /* Rewrite thumbnail to fullImage link */
            ret.add(this.createDownloadlink("https://www." + host + "/view/" + id));
            return ret;
        }
        if (new Regex(url, PATTERN_GALLERY).patternFind()) {
            return crawlGallery(param);
        } else if (new Regex(url, PATTERN_VIEW).patternFind()) {
            return crawlGalleryNew(param);
        } else {
            /* PATTERN_IMAGE */
            ret.add(crawlSingleImage(param));
            return ret;
        }
    }

    private ArrayList<DownloadLink> crawlGallery(final CryptedLink param) throws PluginException, IOException, InterruptedException {
        final String galleryID = new Regex(param.getCryptedUrl(), PATTERN_GALLERY).getMatch(0);
        if (galleryID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.getPage(param.getCryptedUrl());
        errorHandling(br, param);
        continueToYourImage(br, param);
        return crawlProcessGallery(param, this.br);
    }

    private Browser prepBR(final Browser br) {
        /* 2022-03-23: This will skip some "Continue to image" pages. */
        br.setCookie(this.getHost(), "nsfw_inter", "1");
        return br;
    }

    private void continueToYourImage(final Browser br, final CryptedLink param) throws InterruptedException, IOException {
        if (br.containsHTML(">\\s*Continue to your image")) {
            /* Reload page */
            final boolean skipWaittime = true;
            final String waitMillisStr = br.getRegex("show\\(\\);\\s*\\},\\s*(\\d+)\\);").getMatch(0);
            if (waitMillisStr != null && !skipWaittime) {
                this.sleep(Long.parseLong(waitMillisStr), param);
            }
            br.setCookie(br.getHost(), "sfw_inter", "1");
            br.getPage(br.getURL());
        }
    }

    /**
     * Handles new style "gallery" URLs which can either lead to a gallery or a single image.
     *
     * @throws InterruptedException
     * @throws NumberFormatException
     */
    private ArrayList<DownloadLink> crawlGalleryNew(final CryptedLink param) throws PluginException, IOException, NumberFormatException, InterruptedException {
        final String galleryID = new Regex(param.getCryptedUrl(), PATTERN_VIEW).getMatch(0);
        if (galleryID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        prepBR(br);
        br.getPage(param.getCryptedUrl());
        errorHandling(br, param);
        continueToYourImage(br, param);
        if (br.containsHTML("class=\"links gallery\"")) {
            return this.crawlProcessGallery(param, this.br);
        }
        /* Single image - very similar to "crawlSingleImage". */
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String finallink = br.getRegex("class=\"image-loader\"[^>]*>\\s*<img src=\"(https?://[^\"]+)\"").getMatch(0);
        if (finallink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String originalFilename = br.getRegex(galleryID + "\\?full=1\"[^<>]*title=\"([^\"]+)\"").getMatch(0);
        final DownloadLink img = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(finallink));
        img.setProperty(DirectHTTP.PROPERTY_CUSTOM_HOST, getHost());
        img.setContentUrl(param.getCryptedUrl());
        final String filenameURL = Plugin.getFileNameFromURL(new URL(finallink));
        if (originalFilename != null) {
            img.setFinalFileName(originalFilename);
            img.setProperty(DirectHTTP.FIXNAME, originalFilename);
        } else if (filenameURL != null) {
            img.setFinalFileName(filenameURL);
        }
        final String filesizeStr = br.getRegex("class=\"count text-uppercase\"[^>]*>[^<]*&bull;\\s*(\\d+[^<]+)</span>").getMatch(0);
        if (filesizeStr != null) {
            img.setDownloadSize(SizeFormatter.getSize(filesizeStr));
        }
        img.setAvailable(true);
        ret.add(img);
        return ret;
    }

    private ArrayList<DownloadLink> crawlProcessGallery(final CryptedLink param, final Browser br) throws IOException, InterruptedException {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final boolean isNewGallery;
        final String galleryID;
        if (new Regex(param.getCryptedUrl(), PATTERN_GALLERY).patternFind()) {
            isNewGallery = false;
            galleryID = new Regex(param.getCryptedUrl(), PATTERN_GALLERY).getMatch(0);
        } else {
            isNewGallery = true;
            galleryID = new Regex(param.getCryptedUrl(), PATTERN_VIEW).getMatch(0);
        }
        final String galleryTitle = br.getRegex("id=\"gallery-name\"[^>]*>([^<]+)<").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        if (galleryTitle != null) {
            fp.setName(galleryID + " - " + Encoding.htmlDecode(galleryTitle).trim());
        } else {
            fp.setName(galleryID);
        }
        int page = 1;
        do {
            logger.info("Crawling page: " + page);
            boolean foundNewItems = false;
            if (isNewGallery) {
                final String[] urls = br.getRegex(PATTERN_VIEW).getColumn(-1);
                for (String url : urls) {
                    final String imageID = new Regex(url, PATTERN_VIEW).getMatch(0);
                    /* Don't re-add previously added URL! */
                    if (imageID.equals(galleryID)) {
                        continue;
                    }
                    url = br.getURL(url).toExternalForm();
                    final DownloadLink dl = this.createDownloadlink(url);
                    dl._setFilePackage(fp);
                    ret.add(dl);
                    distribute(dl);
                    foundNewItems = true;
                }
            } else {
                final String[] urls = br.getRegex(PATTERN_IMAGE).getColumn(-1);
                for (String url : urls) {
                    final String imageID = new Regex(url, PATTERN_IMAGE).getMatch(0);
                    /* Don't re-add previously added URL! */
                    if (imageID.equals(galleryID)) {
                        continue;
                    }
                    url = br.getURL(url).toExternalForm();
                    final DownloadLink dl = this.createDownloadlink(url);
                    dl._setFilePackage(fp);
                    ret.add(dl);
                    distribute(dl);
                    foundNewItems = true;
                }
            }
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size());
            final String nextPage = br.getRegex("(/[^\"]+\\?page=" + (page + 1) + ")\"").getMatch(0);
            if (this.isAbort()) {
                throw new InterruptedException();
            } else if (!foundNewItems) {
                logger.info("Stopping because: Failed to find new items on current page");
                break;
            } else if (nextPage == null) {
                logger.info("Stopping because: No nextPage given");
                break;
            }
            /* Next page available --> Continue crawl process */
            page += 1;
            br.getPage(nextPage);
        } while (true);
        return ret;
    }

    private void errorHandling(Browser br, CryptedLink param) throws PluginException {
        /* Error handling */
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("Image not found|>\\s*Image violated our terms of service|>\\s*The requested image could not be located|>\\s*The image has been deleted")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("The gallery you are looking for")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    private DownloadLink crawlSingleImage(final CryptedLink param) throws Exception {
        final String imageID = new Regex(param.getCryptedUrl(), PATTERN_IMAGE).getMatch(0);
        if (imageID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        prepBR(br);
        br.getPage(param.getCryptedUrl());
        errorHandling(br, param);
        continueToYourImage(br, param);
        String finallink = br.getRegex("('|\")(https?://\\d+\\.imagebam\\.com/download/[^<>\\s]+)\\1").getMatch(1);
        if (finallink == null) {
            finallink = br.getRegex("('|\")(https?://images\\d+\\.imagebam\\.com/[^<>\\s]+\\.(jpe?g|png))\\1").getMatch(1);
        }
        if (finallink == null) {
            /* Broken plugin or broken link where image is not available on CDN anymore. */
            throw new DecrypterException("Decrypter broken for link: " + br.getURL());
        }
        finallink = Encoding.htmlDecode(finallink);
        final DownloadLink img = createDownloadlink(finallink);
        img.setProperty(DirectHTTP.PROPERTY_CUSTOM_HOST, getHost());
        img.setContentUrl(param.getCryptedUrl());
        String originalFilename = br.getRegex(imageID + "\\?full=1\"[^>]*title=\"([^\"]+)").getMatch(0);
        String urlFilename = extractFileNameFromURL(finallink);
        if (urlFilename != null) {
            if (originalFilename != null && getFileNameExtensionFromString(originalFilename) != null) {
                urlFilename = originalFilename;
            }
            urlFilename = Encoding.htmlDecode(urlFilename);
            /* If has extension don't set, if hasn't extension set default one. */
            urlFilename += getFileNameExtensionFromString(urlFilename) != null ? "" : ".jpg";
            img.setFinalFileName(urlFilename);
        }
        img.setAvailable(true);
        return img;
    }
}