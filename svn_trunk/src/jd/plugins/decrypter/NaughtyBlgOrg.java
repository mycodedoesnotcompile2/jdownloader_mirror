//    jDownloader - Downloadmanager
//    Copyright (C) 2014  JD-Team support@jdownloader.org
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

import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.hcaptcha.CaptchaHelperCrawlerPluginHCaptcha;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperCrawlerPluginRecaptchaV2;
import org.jdownloader.plugins.components.antiDDoSForDecrypt;
import org.jdownloader.plugins.components.config.NaughtyBlgOrgConfig;
import org.jdownloader.plugins.components.config.NaughtyBlgOrgConfig.PreviewCrawlMode;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.Form;
import jd.parser.html.Form.MethodType;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@DecrypterPlugin(revision = "$Revision: 53362 $", interfaceVersion = 5, names = {}, urls = {})
public class NaughtyBlgOrg extends antiDDoSForDecrypt {
    private enum Category {
        UNDEF,
        SITERIP,
        CLIP,
        MOVIE
    }

    public NaughtyBlgOrg(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        /* Always add current domain to first position! */
        ret.add(new String[] { "naughtyblog.my", "naughtyblog.org", "naughtyblog.co", "naughtyblog.me", "nablog.org", "naughtyblog.st" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?!webmasters|contact)[a-z0-9\\-]+/?(?:#nocaptcha)?");
        }
        return ret.toArray(new String[0]);
    }

    private Category CATEGORY;

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        CATEGORY = Category.UNDEF;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        if (contenturl.matches("https://[^/]+/(category|linkex|feed|\\d{4}|tag|free\\-desktop\\-strippers|list\\-of\\-.+|contact\\-us|how\\-to\\-download\\-files|siterips)")) {
            logger.info("Invalid link: " + contenturl);
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        br.setFollowRedirects(true);
        getPage(contenturl);
        if (br.getRequest().getHttpConnection().getResponseCode() == 404 || br.containsHTML(">Page not found \\(404\\)<|>403 Forbidden<") || br.containsHTML("No htmlCode read")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">Deleted due DMCA report<")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        // String content = this.br.getRegex(Pattern.compile("<div id=\"main\\-content\" class=\"main\\-content\\-single\">(.*?)<h3
        // class=\"comments\"", 34)).getMatch(0);
        String contentReleaseName = br.getRegex("<h1 class=\"post\\-title entry\\-title\">(.*?)</h1>").getMatch(0);
        if (contentReleaseName == null) {
            // contentReleaseName = br.getRegex("<h1 class=\"post\\-title\">([^<>\"]*?)</h1>").getMatch(0);
            contentReleaseName = br.getRegex("<h1 class=\"post\\-title(.*?)</h1>").getMatch(0);
        }
        if (contentReleaseName == null) {
            logger.warning("Crawler broken or content offline");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        // replace en-dash with a real dash
        contentReleaseName = contentReleaseName.replace("&#8211;", "-");
        contentReleaseName = Encoding.htmlDecode(contentReleaseName).trim();
        String contentReleaseNamePrecise = br.getRegex("<p>[\\r\\n\\s]*<strong>(.*?)</strong>[\\r\\n\\s]*<br[/\\s]+>[\\r\\n\\s]*<em>Released:").getMatch(0);
        if (contentReleaseNamePrecise != null) {
            // remove possible link to tag-cloud
            contentReleaseNamePrecise = contentReleaseNamePrecise.replaceAll("<.*?>", "");
            // replace en-dash with a real dash
            contentReleaseNamePrecise = contentReleaseNamePrecise.replace("&#8211;", "-");
            contentReleaseNamePrecise = Encoding.htmlDecode(contentReleaseNamePrecise).trim();
            final int pos = contentReleaseName.lastIndexOf("-");
            if (pos != -1) {
                contentReleaseName = contentReleaseName.substring(0, pos).trim();
                contentReleaseName = contentReleaseName + " - " + contentReleaseNamePrecise;
            }
        }
        String contentReleaseNamePreciseSceneRelease = br.getRegex("<p>[\\r\\n\\s]*<strong>(.*?)</strong>[\\r\\n\\s]*<br[/\\s]+>[\\r\\n\\s]*<em>").getMatch(0);
        if (contentReleaseNamePreciseSceneRelease != null && contentReleaseNamePrecise == null) {
            // remove possible link to tag-cloud
            contentReleaseNamePreciseSceneRelease = contentReleaseNamePreciseSceneRelease.replaceAll("<.*?>", "");
            // contentReleaseNamePreciseSceneRelease = Encoding.htmlDecode(contentReleaseNamePreciseSceneRelease).trim();
            contentReleaseName = contentReleaseNamePreciseSceneRelease;
        }
        // check if DL is from the 'clips' section
        Regex categoryCheck = null;
        categoryCheck = br.getRegex("<div id=\"post-\\d+\" class=\".*category\\-clips.*\">");
        if (categoryCheck.matches()) {
            CATEGORY = Category.CLIP;
        }
        // check if DL is from the 'movies' section
        categoryCheck = br.getRegex("<div id=\"post-\\d+\" class=\".*category\\-movies.*\">");
        if (categoryCheck.matches()) {
            CATEGORY = Category.MOVIE;
        }
        // check if DL is from the 'siterips' section
        categoryCheck = br.getRegex("<div id=\"post-\\d+\" class=\".*category\\-siterips.*\">");
        if (categoryCheck.matches()) {
            CATEGORY = Category.SITERIP;
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(getFpName(contentReleaseName));
        fp.setPackageKey("naughtyblog://path/" + br._getURL().getPath());
        final Set<String> dupes = new HashSet<String>();
        /*
         * Step 1: Crawl all links which do not require a captcha and distribute them immediately so they show up in JDownloader right away.
         */
        String contentReleaseLinks = null;
        if (CATEGORY != Category.SITERIP) {
            contentReleaseLinks = br.getRegex(">Download:?</(.*?)</div>").getMatch(0);
            // Nothing found? Get all links from title till comment field
            if (contentReleaseLinks == null) {
                contentReleaseLinks = br.getRegex("<h(1|2) class=\"post\\-title\">(.*?)function validatecomment\\(form\\)\\{").getMatch(1);
            }
            if (contentReleaseLinks == null) {
                contentReleaseLinks = br.getRegex("<h(1|2) class=\"post\\-title\">(.*?)class=\"comments\">Comments are closed").getMatch(1);
            }
        } else {
            // Get all links from title till comment field
            contentReleaseLinks = br.getRegex("<h(?:1|2) class=\"post\\-title[^\"]*?\">(.*?)function validatecomment\\(form\\)\\{").getMatch(0);
            if (contentReleaseLinks == null) {
                contentReleaseLinks = br.getRegex("<h\\d+ class=\"post\\-title\">(.*?)class=\"comments\">").getMatch(0);
            }
        }
        if (contentReleaseLinks == null) {
            contentReleaseLinks = br.getRegex("<div\\s+id\\s*=[^>]*downloadhidden[^>]*>([^$]+)<div[^>]*id\\s*=[^>]*postinfo[^>]*class\\s*=[^>]*categories[^>]*>").getMatch(0);
        }
        if (contentReleaseLinks == null) {
            logger.warning("contentReleaseLinks == null");
            /* Final fallback --> Scan complete html */
            contentReleaseLinks = br.getRequest().getHtmlCode();
        }
        /*
         * Detect the "preview" links. Such links only lead to image galleries / previews rather than to the actual downloadable content.
         * They are usually listed below an "All Previews:" heading and/or point to a known image-preview host (e.g. pixhost galleries).
         */
        final Set<String> previewURLs = new HashSet<String>();
        final String previewsSection = br.getRegex(">\\s*All Previews\\s*:.*?(<div[^>]*\\bid\\s*=\\s*\"download\"[^>]*>.*?</div>)").getMatch(0);
        if (previewsSection != null) {
            final String[] previewSectionLinks = HTMLParser.getHttpLinks(previewsSection, null);
            if (previewSectionLinks != null) {
                for (final String url : previewSectionLinks) {
                    previewURLs.add(url);
                }
            }
        }
        // Known image-preview hosts are always treated as preview links, wherever they appear on the page.
        // final String[] imgs = br.getRegex("(https://([\\w\\.]+)?pixhost\\.to/show/[^\"]+)").getColumn(0);
        final String[] pixhostPreviews = br.getRegex("(https?://(?:[\\w\\.]+)?pixhost\\.(?:to|cc)/(?:show|gallery)/[^\"\\'<>]+)").getColumn(0);
        if (pixhostPreviews != null) {
            for (final String url : pixhostPreviews) {
                previewURLs.add(url);
            }
        }
        final boolean previewLinksAvailable = !previewURLs.isEmpty();
        /* Collect all non captcha protected links and keep only those which are not preview links. */
        final ArrayList<String> otherLinks = new ArrayList<String>();
        final String[] freeLinks = HTMLParser.getHttpLinks(contentReleaseLinks, null);
        if (freeLinks != null) {
            for (final String url : freeLinks) {
                if (!previewURLs.contains(url)) {
                    otherLinks.add(url);
                }
            }
        }
        final String[] otherLinksArray = otherLinks.toArray(new String[0]);
        final String[] previewLinksArray = previewURLs.toArray(new String[0]);
        /*
         * Decide - based on the configured PreviewCrawlMode - which links to add and whether the captcha protected spare links may be
         * crawled at all. In AUTO mode (= default = old behavior) preview links are only added if no other links have been found.
         */
        final PreviewCrawlMode previewCrawlMode = get(getConfigInterface()).getPreviewCrawlMode();
        final boolean allowSpareLinksCrawl;
        switch (previewCrawlMode) {
        case ALWAYS:
            addLinks(otherLinksArray, fp, dupes, ret);
            addLinks(previewLinksArray, fp, dupes, ret);
            allowSpareLinksCrawl = true;
            break;
        case NEVER:
            addLinks(otherLinksArray, fp, dupes, ret);
            allowSpareLinksCrawl = true;
            break;
        case PREVIEW_ONLY:
            /* Crawl only previews - but only if preview links have been found, otherwise fall back to the normal behavior. */
            if (previewLinksAvailable) {
                addLinks(previewLinksArray, fp, dupes, ret);
                allowSpareLinksCrawl = false;
            } else {
                addLinks(otherLinksArray, fp, dupes, ret);
                allowSpareLinksCrawl = true;
            }
            break;
        case ONLY_IF_NO_OTHER_LINKS_ARE_FOUND:
        case AUTO:
        default:
            addLinks(otherLinksArray, fp, dupes, ret);
            if (ret.isEmpty()) {
                addLinks(previewLinksArray, fp, dupes, ret);
            }
            allowSpareLinksCrawl = true;
            break;
        }
        /*
         * Step 2: Optionally crawl the "spare links" which are hidden behind a captcha. This is more expensive as it requires the user to
         * solve a captcha.
         */
        /*
         * Detect items where the freely accessible links only lead to image previews while the real download links are hidden behind the
         * captcha. Such items use an "All Previews:" heading instead of the usual "Download:" heading. In this case the captcha must always
         * be solved, otherwise the user would only end up with preview links.
         */
        final boolean onlyPreviewLinksWithoutCaptcha = br.containsHTML(">\\s*All Previews\\s*:");
        final boolean crawlSpareLinksBehindCaptcha;
        if (!allowSpareLinksCrawl) {
            logger.info("Not crawling captcha protected items because: PreviewCrawlMode is set to crawl previews only");
            crawlSpareLinksBehindCaptcha = false;
        } else if (ret.isEmpty()) {
            logger.info("Crawling captcha protected items because: Failed to find non protected items");
            crawlSpareLinksBehindCaptcha = true;
        } else if (onlyPreviewLinksWithoutCaptcha) {
            logger.info("Crawling captcha protected items because: Without a captcha only preview links are available");
            crawlSpareLinksBehindCaptcha = true;
        } else if (StringUtils.endsWithCaseInsensitive(contenturl, "#nocaptcha")) {
            logger.info("Avoiding captcha because: User added #nocaptcha to URL");
            crawlSpareLinksBehindCaptcha = false;
        } else {
            crawlSpareLinksBehindCaptcha = get(getConfigInterface()).isCrawlCaptchaProtectedSpareLinks();
        }
        if (crawlSpareLinksBehindCaptcha) {
            final String downloadhidden = crawlSpareLinks();
            addLinks(HTMLParser.getHttpLinks(downloadhidden, null), fp, dupes, ret);
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    /**
     * Adds all given URLs as {@link DownloadLink}s to the given package, skipping internal (self) links and duplicates, and distributes
     * them immediately.
     */
    private void addLinks(final String[] urls, final FilePackage fp, final Set<String> dupes, final ArrayList<DownloadLink> ret) {
        if (urls == null) {
            return;
        }
        for (final String url : urls) {
            if (new Regex(url, this.getSupportedLinks()).matches()) {
                /* Skip internal links pointing back to this website. */
                continue;
            }
            if (!dupes.add(url)) {
                /* Skip duplicate. */
                continue;
            }
            final DownloadLink link = createDownloadlink(url);
            link._setFilePackage(fp);
            ret.add(link);
            distribute(link);
        }
    }

    /**
     * Solves the captcha and submits the "passster" form to reveal the spare links which are hidden behind that captcha.
     *
     * @return the HTML fragment containing the revealed spare links, or null if this item does not have any captcha protected spare links.
     */
    private String crawlSpareLinks() throws Exception {
        final String nonce = br.getRegex("\"nonce\"\\s*:\\s*\"(.*?)\"").getMatch(0);
        final String post_id = br.getRegex("\"post_id\"\\s*:\\s*\"(.*?)\"").getMatch(0);
        final String captcha_key = br.getRegex("\"recaptcha_key\"\\s*:\\s*\"(.*?)\"").getMatch(0);
        final String data_protection = br.getRegex("data-protection\\s*=\\s*\"(.*?)\"").getMatch(0);
        final String data_area = br.getRegex("data-area\\s*=\\s*\"(.*?)\"").getMatch(0);
        final String data_psid = br.getRegex("data-psid\\s*=\\s*\"(.*?)\"").getMatch(0);
        if (!StringUtils.isAllNotEmpty(nonce, post_id, captcha_key, data_area, data_protection, data_psid)) {
            logger.info("This item does not have any spare links behind a captcha");
            return null;
        }
        final Form form = new Form();
        form.setAction("/wp-admin/admin-ajax.php");
        form.setMethod(MethodType.POST);
        form.put("action", "validate_input");
        form.put("nonce", nonce);
        form.put("post_id", post_id);
        form.put("protection", URLEncoder.encode(data_protection, "UTF-8"));
        form.put("area", URLEncoder.encode(data_area, "UTF-8"));
        form.put("captcha_id", data_psid);
        form.put("type", "recaptcha");
        if (CaptchaHelperCrawlerPluginRecaptchaV2.isValidSiteKey(captcha_key)) {
            final String response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br, captcha_key).getToken();
            form.put("token", response);
        } else if (CaptchaHelperCrawlerPluginHCaptcha.isValidSiteKey(captcha_key)) {
            final String response = new CaptchaHelperCrawlerPluginHCaptcha(this, br, captcha_key).getToken();
            form.put("token", response);
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported captchaKey:" + captcha_key);
        }
        final Browser brc = br.cloneBrowser();
        brc.submitForm(form);
        final Map<String, Object> response = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!Boolean.TRUE.equals(response.get("success"))) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return (String) response.get("content");
    }

    @Override
    public Class<NaughtyBlgOrgConfig> getConfigInterface() {
        return NaughtyBlgOrgConfig.class;
    }

    private String getFpName(String filePackageName) {
        switch (CATEGORY) {
        case CLIP:
            final int firstOccurrenceOfSeparator = filePackageName.indexOf(" - ");
            if (firstOccurrenceOfSeparator > -1) {
                final StringBuffer sb = new StringBuffer(filePackageName);
                sb.insert(firstOccurrenceOfSeparator, " - Clips");
                filePackageName = sb.toString();
            }
            break;
        case MOVIE:
            // filePackageName += " - Movie";
            break;
        case SITERIP:
            if (!filePackageName.toLowerCase().contains("siterip")) {
                filePackageName += " - SiteRip";
            }
            break;
        default:
            break;
        }
        return filePackageName;
    }
}