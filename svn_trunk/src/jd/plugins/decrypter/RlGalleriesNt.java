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

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperCrawlerPluginCloudflareTurnstile;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52469 $", interfaceVersion = 3, names = {}, urls = {})
public class RlGalleriesNt extends PluginForDecrypt {
    public RlGalleriesNt(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setReadTimeout(3 * 60 * 1000);
        br.getHeaders().put("Accept-Language", "en-gb, en;q=0.9");
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "urlgalleries.com", "urlgalleries.net" });
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

    public static final Pattern PATTERN_GALLERY         = Pattern.compile("/[\\w\\-]+.+");
    public static final Pattern PATTERN_SINGLE_REDIRECT = Pattern.compile("/dl?/([0-9]+)/?");
    public static Object        LOCK                    = new Object();

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + PATTERN_SINGLE_REDIRECT.pattern() + "|" + PATTERN_GALLERY.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl().replaceFirst("(?i)http://", "https://");
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (new Regex(contenturl, PATTERN_SINGLE_REDIRECT).patternFind()) {
            /* Crawl single redirect link */
            synchronized (LOCK) {
                br.setFollowRedirects(false);
                final String property_PHPSESSID = "PHPSESSID";
                final String phpsessid = this.getPluginConfig().getStringProperty(property_PHPSESSID);
                if (phpsessid != null) {
                    /* Re-use session cookie otherwise we might need to solve a captcha for each link. */
                    br.setCookie(this.getHost(), "PHPSESSID", phpsessid);
                }
                br.getPage(contenturl);
                if (br.getHttpConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                final Form form = br.getForm(0);
                String finallink = null;
                findFinallink: if (form != null) {
                    finallink = findFinallink(br);
                    if (finallink != null) {
                        logger.info("Found finallink without captcha");
                        break findFinallink;
                    }
                    final String previous_path = br._getURL().getPath();
                    final String cfTurnstileResponse = new CaptchaHelperCrawlerPluginCloudflareTurnstile(this, br).getToken();
                    form.put("cf-turnstile-response", Encoding.urlEncode(cfTurnstileResponse));
                    br.submitForm(form);
                    final String redirect = br.getRedirectLocation();
                    if (redirect == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                    /* Check for redirect on previous URL which will then reidrect to final URL. */
                    if (redirect.contains(previous_path)) {
                        logger.info("Handling double-redirect");
                        br.getPage(redirect);
                    }
                    finallink = findFinallink(br);
                    if (finallink == null) {
                        logger.info("Failed to fina finallink in html code -> Website might have changed and plugin is broken?");
                    }
                }
                if (finallink == null) {
                    finallink = br.getRedirectLocation();
                }
                if (finallink == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                ret.add(this.createDownloadlink(finallink));
                final String phpsessid_new = br.getCookie(br.getHost(), "PHPSESSID", Cookies.NOTDELETEDPATTERN);
                if (!StringUtils.isEmpty(phpsessid_new)) {
                    if (phpsessid == null || !phpsessid_new.equals(phpsessid)) {
                        logger.info("Remembering new phpsessid cookie: " + phpsessid_new);
                        this.getPluginConfig().setProperty(property_PHPSESSID, phpsessid_new);
                    }
                } else {
                    logger.warning("Failed to find PHPSESSID cookie");
                }
                return ret;
            }
        }
        String galleryID = new Regex(contenturl, "(?i)https?://[^/]+/[^/]+/(\\d+)").getMatch(0);
        if (galleryID == null) {
            /* For old links */
            galleryID = new Regex(contenturl, "(?i)(?:porn-gallery-|blog_gallery\\.php\\?id=)(\\d+)").getMatch(0);
        }
        if (galleryID != null) {
            /* Gallery */
            logger.info("Crawling gallery");
            /* 2023-11-09: API does not work anymore */
            final boolean useAPI = false;
            if (useAPI) {
                br.getPage("https://urlgalleries.net/api/v1.php?endpoint=get_gallery&gallery_id=" + galleryID + "&exclude_cat=undefined&_=" + System.currentTimeMillis());
                if (br.getHttpConnection().getResponseCode() == 404) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final Map<String, Object> data = (Map<String, Object>) entries.get("data");
                final String title = data.get("name").toString();
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(title);
                int page = 1;
                String nextpage = null;
                final HashSet<String> dupes = new HashSet<String>();
                do {
                    logger.info("Crawling page " + page + " of ??");
                    final List<Map<String, Object>> thumbs = (List<Map<String, Object>>) data.get("thumbs");
                    final ArrayList<DownloadLink> newitems = new ArrayList<DownloadLink>();
                    for (final Map<String, Object> thumb : thumbs) {
                        final String redirecturl = thumb.get("url").toString();
                        final String thumbnailurl = thumb.get("imgcode").toString();
                        if (dupes.add(redirecturl)) {
                            newitems.add(this.createDownloadlink(redirecturl));
                        }
                        if (dupes.add(thumbnailurl)) {
                            newitems.add(this.createDownloadlink(thumbnailurl));
                        }
                    }
                    for (final DownloadLink newitem : newitems) {
                        newitem._setFilePackage(fp);
                        ret.add(newitem);
                        distribute(newitem);
                    }
                    if (isAbort()) {
                        logger.info("Stopping because: Aborted by user");
                        break;
                    } else if (newitems.isEmpty()) {
                        /* Fail-safe */
                        logger.info("Stopping because: Failed to find any new item on this page");
                        break;
                    } else if (nextpage == null) {
                        logger.info("Stopping because: Reached last page?");
                        break;
                    } else {
                        br.getPage(nextpage);
                        page++;
                    }
                } while (true);
                return ret;
            }
            final String url = URLHelper.getUrlWithoutParams(contenturl);
            /* Display as many items as possible to avoid having to deal with pagination. */
            final UrlQuery query = UrlQuery.parse(contenturl);
            /* Display all images on one page */
            query.addAndReplace("a", "10000");
            /* Start from page 1 else we may get an empty page (website is buggy). */
            query.addAndReplace("p", "1");
            /**
             * Allow for any direct-URLs added by user <br>
             * Example: https://urlgalleries.com/img/88x31_RTA-5042-1996-1400-1577-RTA_b.gif
             */
            final URLConnectionAdapter con = br.openGetConnection(url + "?" + query.toString());
            if (this.looksLikeDownloadableContent(con)) {
                try {
                    con.disconnect();
                } catch (final Throwable ignore) {
                }
                final DownloadLink direct = getCrawler().createDirectHTTPDownloadLink(con.getRequest(), con);
                ret.add(direct.getDownloadLink());
                /* Early return */
                return ret;
            } else {
                br.followConnection();
            }
            if (isOffline(br)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            if (br.containsHTML("Continue to the image") || br.containsHTML("Continue to image")) {
                /* 2025-03-26: Single image */
                String finallink = br.getRegex("window\\.location\\.href = '(https?://[^']+)").getMatch(0);
                if (finallink == null) {
                    finallink = br.getRegex("var\\s*IMAGE_URL\\s*=\\s*\"(https?://[^\"]+)").getMatch(0);
                    if (finallink == null) {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
                ret.add(this.createDownloadlink(finallink));
                return ret;
            }
            String title = br.getRegex("border='0' /></a></div>(?:\\s*<h\\d+[^>]*>\\s*)?(.*?)(?:\\s*</h\\d+>\\s*)?</td></tr><tr>").getMatch(0);
            if (title == null) {
                title = br.getRegex("<title>([^<]*?)</title>").getMatch(0);
            }
            final FilePackage fp = FilePackage.getInstance();
            if (title != null) {
                title = Encoding.htmlDecode(title).trim();
                title = title.replaceAll("(?i) - URLGalleries$", "");
            }
            if (!StringUtils.isEmpty(title)) {
                fp.setName(title);
            } else {
                /* Fallback */
                fp.setName(br._getURL().getPath());
            }
            String zip_url = br.getRegex("class=\"k2sZipLink\"\\s*href=\"((https?|/)[^\"]+)").getMatch(0);
            if (zip_url != null) {
                zip_url = br.getURL(zip_url).toExternalForm();
                final DownloadLink zip = this.createDownloadlink(zip_url);
                zip._setFilePackage(fp);
                ret.add(zip);
                distribute(zip);
            }
            int page = 1;
            String nextpage = null;
            final HashSet<String> dupes = new HashSet<String>();
            final Pattern allowed_thumbnail_urls = Pattern.compile("(https?://[^/]*\\.(imagevenue\\.com|fappic\\.com|imagetwist\\.com)/[^<>\"\\']+)");
            do {
                logger.info("Crawling page " + page + " of ??");
                final ArrayList<DownloadLink> newitems = new ArrayList<DownloadLink>();
                String[] redirecturls = br.getRegex("rel='nofollow noopener' href='(/[^/\\']+)' target='_blank'").getColumn(0);
                if (redirecturls == null || redirecturls.length == 0) {
                    redirecturls = br.getRegex("href\\s*=\\s*\"(/[^\"]+)\"\\s*target\\s*=\\s*\"_blank\"\\s*rel\\s*=\\s*\"nofollow noopener").getColumn(0);
                }
                /*
                 * Check for special thumbnails that our host plugins will change to the original URLs without needing to crawl the
                 * individual urlgalleries.net URLs -> Speeds up things a lot!
                 */
                String[] thumbnailurls = br.getRegex("class='gallery' src='" + allowed_thumbnail_urls.pattern() + "'").getColumn(0);
                if (thumbnailurls == null || thumbnailurls.length == 0) {
                    thumbnailurls = br.getRegex("class=\"thumb\"\\s*src=\"" + allowed_thumbnail_urls.pattern() + "\"").getColumn(0);
                }
                if (thumbnailurls == null || thumbnailurls.length == 0) {
                    if (ret.size() > 0) {
                        return ret;
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
                for (final String thumbnailurl : thumbnailurls) {
                    if (dupes.add(thumbnailurl)) {
                        final DownloadLink link = this.createDownloadlink(thumbnailurl);
                        newitems.add(link);
                    }
                }
                if (redirecturls != null && thumbnailurls != null && redirecturls.length > thumbnailurls.length) {
                    for (String redirecturl : redirecturls) {
                        if (!dupes.add(redirecturl)) {
                            continue;
                        }
                        redirecturl = br.getURL(redirecturl).toExternalForm();
                        final DownloadLink link = this.createDownloadlink(redirecturl);
                        newitems.add(link);
                    }
                } else {
                    logger.info("Thumbnail crawling was successful");
                }
                for (final DownloadLink newitem : newitems) {
                    newitem._setFilePackage(fp);
                    ret.add(newitem);
                    distribute(newitem);
                }
                logger.info("Crawled page " + page + " | Found items so far: " + ret.size());
                nextpage = br.getRegex("(" + Pattern.quote(br._getURL().getPath()) + "\\?p=" + (page + 1) + ")").getMatch(0);
                if (isAbort()) {
                    logger.info("Stopping because: Aborted by user");
                    break;
                } else if (newitems.size() == 0) {
                    /* Fail-safe */
                    logger.info("Stopping because: Failed to find any new item on this page");
                    break;
                } else if (nextpage == null || !dupes.add(nextpage)) {
                    logger.info("Stopping because: Reached last page?");
                    break;
                } else if (this.isAbort()) {
                    throw new InterruptedException();
                }
                /* Continue to next page */
                br.getPage(nextpage);
                page++;
            } while (!this.isAbort());
            if (ret.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            return ret;
        } else {
            /* Single image */
            logger.info("Crawling single image");
            br.setFollowRedirects(false);
            br.getPage(contenturl.replaceFirst("(?i)http://", "https://"));
            int counter = 0;
            String redirect = null;
            do {
                counter++;
                redirect = br.getRedirectLocation();
                if (redirect == null || !redirect.contains(this.getHost())) {
                    break;
                }
                br.getPage(redirect);
            } while (counter <= 5);
            if (isOffline(br)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML("/not_found_adult\\.php")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            if (redirect != null) {
                ret.add(this.createDownloadlink(redirect));
                return ret;
            }
            String finallink = br.getRegex("linkDestUrl\\s*=\\s*\\'(https?[^<>\"\\']+)\\'").getMatch(0);
            if (finallink == null) {
                /* 2023-02-17 */
                finallink = br.getRegex("externalUrl\\s*=\\s*(?:\\'|\")(https?[^<>\"\\']+)").getMatch(0);
            }
            if (finallink == null) {
                /* Invalid link or plugin broken */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            ret.add(this.createDownloadlink(finallink));
            return ret;
        }
    }

    private String findFinallink(final Browser brc) {
        String finallink = brc.getRegex("var u = \"(https?://[^\"]+)").getMatch(0);
        if (finallink == null) {
            finallink = brc.getRegex("<a href=\"(https?://[^\"]+)\">\\s*If you are not redirected, click here").getMatch(0);
        }
        return finallink;
    }

    private boolean isOffline(final Browser br) {
        return br.getHttpConnection().getResponseCode() == 404 | br.containsHTML("(?i)<center>\\s*This blog has been closed down") | br.containsHTML("<title> - urlgalleries\\.net</title>|>ERROR - NO IMAGES AVAILABLE") || br.getURL().contains("/not_found_adult.php");
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        if (new Regex(link.getCryptedUrl(), PATTERN_SINGLE_REDIRECT).patternFind()) {
            /* 2026-03-02: Captcha required */
            return true;
        }
        return false;
    }
}