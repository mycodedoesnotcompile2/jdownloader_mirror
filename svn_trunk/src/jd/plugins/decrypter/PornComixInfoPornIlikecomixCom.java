package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52918 $", interfaceVersion = 3, names = {}, urls = {})
/** Formerly known as: porncomix.one / porncomixone.net */
public class PornComixInfoPornIlikecomixCom extends PluginForDecrypt {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "ilikecomix.com", "porncomixone.net" });
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

    private static final Pattern PATTERN_1 = Pattern.compile("/allporn/comics/([\\w-]+)/([\\w-]+)/?", Pattern.CASE_INSENSITIVE);
    /* 2026-03-27: ilikecomix.com */
    private static final Pattern PATTERN_2 = Pattern.compile("/manga/c/([\\w-]+)((/([\\w-]+)/?|/))?", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_3 = Pattern.compile("/([\\w-]+)/([\\w-]+)/?", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_4 = Pattern.compile("/([\\w-]+)/([\\w-]+)/?(page/\\d+/?)?", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_1.pattern().substring(1) + "|" + PATTERN_2.pattern().substring(1) + "|" + PATTERN_3.pattern().substring(1) + "|" + PATTERN_4.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setFollowRedirects(true);
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String path = br._getURL().getPath();
        final Regex pattern1 = new Regex(path, PATTERN_1);
        final Regex pattern2 = new Regex(path, PATTERN_2);
        final Regex pattern3 = new Regex(path, PATTERN_3);
        final Regex pattern4 = new Regex(path, PATTERN_4);
        String urlBookSlug = null;
        final String urlBookChapterSlug;
        String[] images;
        String extra_image = null;
        String postTitle = null;
        if (pattern1.patternFind()) {
            urlBookChapterSlug = pattern1.getMatch(1);
            postTitle = br.getRegex("\"headline\": \"([^\"]+)").getMatch(0);
            images = br.getRegex("img id=\"image-\\d+\" src=\"([^\"]+)").getColumn(0);
        } else if (pattern2.patternFind()) {
            urlBookSlug = pattern2.getMatch(0);
            urlBookChapterSlug = pattern2.getMatch(3);
            postTitle = br.getRegex("property=\"og:title\" content=\"([^\"]+)").getMatch(0);
            images = br.getRegex("id=\"image-\\d+\" data-src=\"([^\"]+)").getColumn(0);
            /* 2026-05-04: First image is different */
            extra_image = br.getRegex("src=\"(https?://[^\"]+)\" id=\"image-0\"").getMatch(0);
        } else {
            urlBookChapterSlug = pattern3.getMatch(0);
            postTitle = br.getRegex("property=\"og:title\" content=\"([^\"]+)").getMatch(0);
            images = br.getRegex("/ImageObject\" data-pswp-src=\"(https[^\"]+)").getColumn(0);
            if (images == null || images.length == 0) {
                images = br.getRegex("data-pswp-src=\"(https[^\"]+)").getColumn(0);
            }
        }
        if (!StringUtils.isEmpty(postTitle)) {
            postTitle = Encoding.htmlDecode(postTitle).trim();
            /* Correct title */
            postTitle = postTitle.replaceFirst(" - Porn Comics \\| Ilike Comix$", "");
        } else {
            /* Fallback */
            postTitle = urlBookChapterSlug.replace("-", " ").trim();
        }
        if (images == null || images.length == 0) {
            /* Only allow one level of crawling here otherwise this one may result in endless crawling. */
            final DownloadLink previousGeneration = param.getDownloadLink();
            if (previousGeneration != null) {
                logger.info("Prohibit endless crawling -> Returning nothing");
                return ret;
            }
            if (pattern2.patternFind()) {
                /* Crawl all chapters of a book e.g. /manga/c/book-slug/<-- all chapters here */
                final HashSet<String> dupes = new HashSet<String>();
                final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
                for (final String url : urls) {
                    if (!dupes.add(url)) {
                        continue;
                    }
                    final Regex book_url_info = new Regex(url, PATTERN_2);
                    if (!book_url_info.patternFind()) {
                        continue;
                    }
                    final String thisUrlBookSlug = book_url_info.getMatch(0);
                    final String thisUrlBookChapterSlug = book_url_info.getMatch(3);
                    if (thisUrlBookChapterSlug == null) {
                        /* Not a single-chapter url */
                        continue;
                    }
                    if (!this.canHandle(url)) {
                        /* Skips invalid URLs such as: https://www.statcounter.com/counter/counter.js */
                        continue;
                    }
                    if (!thisUrlBookSlug.equalsIgnoreCase(urlBookSlug)) {
                        /* Chapter belongs to another book -> Skip it */
                        continue;
                    }
                    ret.add(this.createDownloadlink(url));
                }
                logger.info("Found book chapters: " + ret.size());
                if (ret.isEmpty()) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
            }
            if (pattern4.patternFind()) {
                /* Generic crawler e.g. for https://ilikecomix.com/author/bla/ */
                final HashSet<String> dupes = new HashSet<String>();
                final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
                for (final String url : urls) {
                    if (!dupes.add(url)) {
                        continue;
                    }
                    if (new Regex(url, PATTERN_3).patternFind()) {
                        ret.add(this.createDownloadlink(url));
                    }
                }
                if (ret.isEmpty()) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                return ret;
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(postTitle);
        final List<String> imgs = new ArrayList<String>();
        if (extra_image != null) {
            imgs.add(extra_image);
        }
        for (String imageurl : images) {
            imageurl = br.getURL(imageurl).toExternalForm();
            if (!imgs.contains(imageurl)) {
                imgs.add(imageurl);
            }
        }
        for (final String url : imgs) {
            final DownloadLink link = createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
            link.setAvailable(true);
            link.setContainerUrl(contenturl);
            link._setFilePackage(fp);
            ret.add(link);
        }
        return ret;
    }
}
