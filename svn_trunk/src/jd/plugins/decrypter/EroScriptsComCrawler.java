package jd.plugins.decrypter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

import org.appwork.storage.TypeRef;
import org.appwork.utils.Files;
import org.jdownloader.plugins.components.antiDDoSForDecrypt;
import org.jdownloader.plugins.controller.crawler.LazyCrawlerPlugin;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;

@DecrypterPlugin(revision = "$Revision: 52649 $", interfaceVersion = 3, names = { "discuss.eroscripts.com" }, urls = { "https?://discuss\\.eroscripts\\.com/t/([\\w\\-/]+)" })
public class EroScriptsComCrawler extends antiDDoSForDecrypt {

    private static final Set<String> VIDEO_EXTENSIONS = new HashSet<String>(Arrays.asList("mp4", "mkv", "wmv", "mov", "avi", "m4v", "webm"));
    private static final Set<String> IMAGE_EXTENSIONS = new HashSet<String>(Arrays.asList("jpg", "jpeg", "png", "gif", "webp", "bmp", "tiff", "avif"));

    /** DownloadLink property value: animated image (GIF/WebP with {@code class="animated"}). */
    private static final String      IMG_ANIMATED     = "animated";
    /** DownloadLink property value: static image (lightbox JPEG/PNG/…). */
    private static final String      IMG_STATIC       = "static";
    /** DownloadLink property key used to tag image links for later renaming. */
    private static final String      PROP_IMG         = "eros_img";
    /** Plugin config key: whether to download images and animated GIFs. */
    public static final String       DOWNLOAD_IMAGES  = "DOWNLOAD_IMAGES";

    public EroScriptsComCrawler(PluginWrapper wrapper) {
        super(wrapper);
        setConfigElements();
    }

    public void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), DOWNLOAD_IMAGES, "Download images and animated GIFs (first post only)").setDefaultValue(Boolean.TRUE));
    }

    @Override
    @SuppressWarnings("unchecked")
    public ArrayList<DownloadLink> decryptIt(final CryptedLink parameter, ProgressController progress) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(getHost());
        if (account != null) {
            jd.plugins.hoster.EroScriptsCom hoster = (jd.plugins.hoster.EroScriptsCom) this.getNewPluginForHostInstance("discuss.eroscripts.com");
            hoster.login(br, account, false);
        }

        final boolean downloadImages = getPluginConfig().getBooleanProperty(DOWNLOAD_IMAGES, true);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Set<String> seenUrls = new HashSet<String>();

        // Normalise to HTTPS
        String contentUrl = parameter.getCryptedUrl().replaceFirst("(?i)http://", "https://");

        // Extract topic ID for paginated post fetching
        String topicId = null;
        String[][] idMatch = new Regex(contentUrl, "/t/(?:[\\w\\-]+/)?(\\d+)").getMatches();
        if (idMatch != null && idMatch.length > 0) {
            topicId = idMatch[0][0];
        }

        // Build the Discourse JSON API URL for the topic
        String baseJsonUrl = contentUrl.replaceFirst("\\?.*$", "").replaceFirst("#.*$", "").replaceFirst("/+$", "").replaceFirst("\\.json$", "") + ".json";

        br.getPage(baseJsonUrl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }

        final Map<String, Object> root = restoreFromString(br.toString(), TypeRef.MAP);
        if (root == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }

        // Prefer plain "title"; fall back to HTML-encoded "fancy_title"
        String title = null;
        if (root.containsKey("title")) {
            title = (String) root.get("title");
        }
        if (title == null && root.containsKey("fancy_title")) {
            title = Encoding.htmlDecode((String) root.get("fancy_title"));
        }

        final Map<String, Object> postStream = (Map<String, Object>) root.get("post_stream");
        if (postStream == null) {
            return ret;
        }
        final List<Map<String, Object>> posts = (List<Map<String, Object>>) postStream.get("posts");
        if (posts == null) {
            return ret;
        }

        // Track already-loaded post IDs to avoid re-fetching them during pagination
        final Set<Object> loadedIds = new HashSet<Object>();
        for (Map<String, Object> post : posts) {
            loadedIds.add(post.get("id"));
        }

        for (Map<String, Object> post : posts) {
            extractLinksFromPost(post, ret, seenUrls, isPostNumber(post, 1), downloadImages);
        }

        // Pagination: fetch remaining posts in batches via posts.json
        if (topicId != null) {
            final List<Object> stream = (List<Object>) postStream.get("stream");
            if (stream != null) {
                final List<Object> missingIds = new ArrayList<Object>();
                for (Object id : stream) {
                    if (!loadedIds.contains(id)) {
                        missingIds.add(id);
                    }
                }
                final int BATCH = 20;
                final String postsJsonBase = "https://discuss.eroscripts.com/t/" + topicId + "/posts.json";
                for (int i = 0; i < missingIds.size(); i += BATCH) {
                    final List<Object> batch = missingIds.subList(i, Math.min(i + BATCH, missingIds.size()));
                    final StringBuilder sb = new StringBuilder(postsJsonBase).append("?");
                    for (Object id : batch) {
                        sb.append("post_ids[]=").append(((Number) id).longValue()).append("&");
                    }
                    br.getPage(sb.toString());
                    if (br.getHttpConnection().getResponseCode() != 200) {
                        break;
                    }
                    final Map<String, Object> batchRoot = restoreFromString(br.toString(), TypeRef.MAP);
                    if (batchRoot == null) {
                        continue;
                    }
                    final Map<String, Object> batchStream = (Map<String, Object>) batchRoot.get("post_stream");
                    if (batchStream == null) {
                        continue;
                    }
                    final List<Map<String, Object>> batchPosts = (List<Map<String, Object>>) batchStream.get("posts");
                    if (batchPosts == null) {
                        continue;
                    }
                    for (Map<String, Object> post : batchPosts) {
                        // Images are only taken from post #1 — paginated batches never contain post #1
                        extractLinksFromPost(post, ret, seenUrls, false, downloadImages);
                    }
                }
            }
        }

        if (ret.isEmpty()) {
            return ret;
        }

        // Rename collected image links to match the funscript base name (or topic title as fallback)
        renameImages(ret, title);

        if (title != null) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(title);
            fp.setAllowMerge(true);
            fp.setAllowInheritance(true);
            fp.addLinks(ret);
        }
        return ret;
    }

    /**
     * Renames image links using the pattern {@code [base]-N.ext}, where N reflects the order in which images appear in the cooked HTML (DOM
     * order).
     * <p>
     * The base name is derived from the first funscript link found. If no funscript is present, the topic title is used as fallback (with
     * filename-illegal characters removed). Does nothing if neither a funscript nor a title is available, or if no images were collected.
     *
     * @param links
     *            all collected download links for the topic
     * @param title
     *            topic title used as fallback base name; may be {@code null}
     */
    private void renameImages(final ArrayList<DownloadLink> links, final String title) {
        String base = findFunscriptBase(links);
        if (base == null && title != null) {
            base = title.replaceAll("[\\\\/:*?\"<>|]", "").trim();
            if (base.isEmpty()) {
                base = null;
            }
        }
        if (base == null) {
            return;
        }
        int idx = 1;
        for (final DownloadLink dl : links) {
            if (dl.getStringProperty(PROP_IMG) != null) {
                dl.setFinalFileName(base + "-" + idx + "." + extFromLink(dl));
                idx++;
            }
        }
    }

    /**
     * Returns the base name of the first funscript link found in {@code links}. The base name is the filename without the
     * {@code .funscript} extension and without any secondary dot-separated suffix (e.g. {@code "MyScript.v2.funscript"} →
     * {@code "MyScript"}).
     *
     * @param links
     *            list of download links to search
     * @return funscript base name, or {@code null} if no funscript link is present
     */
    private String findFunscriptBase(final List<DownloadLink> links) {
        for (final DownloadLink dl : links) {
            String name = dl.getFinalFileName();
            if (name == null) {
                name = dl.getName();
            }
            if (name == null || !name.endsWith(".funscript")) {
                continue;
            }
            final String withoutExt = name.substring(0, name.length() - ".funscript".length());
            final int dotIdx = withoutExt.indexOf('.');
            return dotIdx >= 0 ? withoutExt.substring(0, dotIdx) : withoutExt;
        }
        return null;
    }

    /**
     * Returns the file extension extracted from the link's URL, without the leading dot. Falls back to {@code "bin"} if no extension can be
     * determined.
     *
     * @param dl
     *            the download link
     * @return lowercase file extension, e.g. {@code "gif"}, or {@code "bin"}
     */
    private String extFromLink(final DownloadLink dl) {
        final String urlPath = dl.getPluginPatternMatcher().contains("?") ? dl.getPluginPatternMatcher().substring(0, dl.getPluginPatternMatcher().indexOf('?')) : dl.getPluginPatternMatcher();
                final String ext = Files.getExtension(urlPath, true);
                return ext != null ? ext : "bin";
    }

    /**
     * Returns {@code true} if the given Discourse post object has the specified {@code post_number}.
     *
     * @param post
     *            Discourse post map from the JSON API
     * @param number
     *            expected post number (1-based)
     */
    private boolean isPostNumber(final Map<String, Object> post, final int number) {
        final Object pn = post.get("post_number");
        return pn instanceof Number && ((Number) pn).intValue() == number;
    }

    /**
     * Extracts links from a single Discourse post object. Image extraction is only performed when both {@code isFirstPost} and
     * {@code downloadImages} are {@code true}.
     *
     * @param post
     *            Discourse post map from the JSON API
     * @param ret
     *            list to add discovered links to
     * @param seenUrls
     *            set of already-collected URLs for O(1) deduplication
     * @param isFirstPost
     *            {@code true} if this is post #1 (the OP)
     * @param downloadImages
     *            {@code true} if image downloading is enabled in config
     */
    private void extractLinksFromPost(final Map<String, Object> post, final ArrayList<DownloadLink> ret, final Set<String> seenUrls, final boolean isFirstPost, final boolean downloadImages) throws IOException {
        final String cooked = (String) post.get("cooked");
        if (cooked == null || cooked.isEmpty()) {
            return;
        }
        extractLinks(cooked, ret, seenUrls, isFirstPost && downloadImages);
    }

    /**
     * Single-pass HTML scanner that processes {@code href} links and {@code <img class="animated">} tags in DOM order. Compound attributes
     * such as {@code data-download-href} are skipped to avoid false matches on the {@code href="} substring.
     * <p>
     * At each iteration the position of the next {@code href="} marker is compared against the next {@code class="animated"} marker;
     * whichever appears earlier in the document is handled first. This preserves the original document order for image renaming.
     * <p>
     * Static images linked via {@code <a href>} are only collected when {@code extractImages} is {@code true}. Animated images are likewise
     * skipped entirely when {@code extractImages} is {@code false}.
     *
     * @param html
     *            the rendered (cooked) HTML of a post
     * @param ret
     *            list to add discovered links to
     * @param seenUrls
     *            set of already-collected URLs for O(1) deduplication
     * @param extractImages
     *            {@code true} to collect image links (post #1 with config enabled)
     */
    private void extractLinks(final String html, final ArrayList<DownloadLink> ret, final Set<String> seenUrls, final boolean extractImages) throws IOException {
        final String hrefMarker = "href=\"";
        final String animatedMarker = "class=\"animated\"";

        int pos = 0;
        while (pos < html.length()) {
            final int hrefPos = html.indexOf(hrefMarker, pos);
            final int animatedPos = extractImages ? html.indexOf(animatedMarker, pos) : -1;

            if (hrefPos < 0 && animatedPos < 0) {
                break;
            }

            // Process whichever marker appears earlier in the document
            final boolean doHref = hrefPos >= 0 && (animatedPos < 0 || hrefPos <= animatedPos);

            if (doHref) {
                // Skip compound attributes like data-download-href that contain href=" as substring
                if (hrefPos > 0) {
                    final char before = html.charAt(hrefPos - 1);
                    if (Character.isLetterOrDigit(before) || before == '-') {
                        pos = hrefPos + hrefMarker.length();
                        continue;
                    }
                }
                final int valueStart = hrefPos + hrefMarker.length();
                final int end = html.indexOf('"', valueStart);
                if (end < 0) {
                    break;
                }
                final String href = html.substring(valueStart, end);

                // Extract visible link text between the closing '>' of the <a> tag and </a>
                String linkText = null;
                final int tagClose = html.indexOf('>', end);
                if (tagClose >= 0) {
                    final int closeA = html.indexOf("</a>", tagClose + 1);
                    if (closeA > tagClose) {
                        final String rawText = html.substring(tagClose + 1, closeA).trim();
                        if (!rawText.contains("<")) {
                            linkText = rawText;
                        }
                    }
                }

                pos = end + 1;
                if (href.isEmpty() || href.startsWith("#")) {
                    continue;
                }
                if (!href.startsWith("http://") && !href.startsWith("https://") && !href.startsWith("/")) {
                    continue;
                }
                processUrl(href, linkText, ret, seenUrls, extractImages);
            } else {
                // Animated image: locate the <img tag that precedes class="animated"
                final int imgStart = html.lastIndexOf("<img", animatedPos);
                if (imgStart < 0) {
                    pos = animatedPos + 1;
                    continue;
                }
                final int tagEnd = html.indexOf('>', imgStart);
                if (tagEnd < 0) {
                    pos = animatedPos + 1;
                    continue;
                }
                final String imgTag = html.substring(imgStart, tagEnd + 1);

                final String srcMarker = "src=\"";
                final int srcStart = imgTag.indexOf(srcMarker);
                if (srcStart >= 0) {
                    final int srcValueStart = srcStart + srcMarker.length();
                    final int srcEnd = imgTag.indexOf('"', srcValueStart);
                    if (srcEnd > srcValueStart) {
                        final String src = imgTag.substring(srcValueStart, srcEnd);
                        if (src.startsWith("http://") || src.startsWith("https://")) {
                            if (!seenUrls.contains(src)) {
                                seenUrls.add(src);
                                final DownloadLink dl = this.createDownloadlink(src);
                                dl.setProperty(PROP_IMG, IMG_ANIMATED);
                                ret.add(dl);
                            }
                        }
                    }
                }
                pos = animatedPos + animatedMarker.length();
            }
        }
    }

    /**
     * Resolves and classifies a single URL extracted from a post, then appends it to {@code ret} if it matches a supported type:
     * <ul>
     * <li>Funscript uploads ({@code /uploads/…​.funscript}) — always included</li>
     * <li>Direct video files (by extension) — always included</li>
     * <li>Static image files (by extension) — included only when {@code extractImages} is {@code true}</li>
     * <li>Any URL handled by another JDownloader plugin — always included</li>
     * </ul>
     * Relative URLs are expanded to absolute. Duplicates are silently skipped via {@code seenUrls}.
     *
     * @param rawUrl
     *            the raw href value from the HTML (may be relative)
     * @param linkText
     *            visible anchor text, used as filename hint for funscripts; may be {@code null}
     * @param ret
     *            list to add the link to
     * @param seenUrls
     *            set of already-collected URLs for O(1) deduplication
     * @param extractImages
     *            {@code true} to include static image links
     */
    private void processUrl(final String rawUrl, final String linkText, final ArrayList<DownloadLink> ret, final Set<String> seenUrls, final boolean extractImages) throws IOException {
        final String url;
        try {
            url = br.getURL(rawUrl).toString();
        } catch (final Exception e) {
            getLogger().info("Skipping malformed URL: " + rawUrl + " (" + e.getMessage() + ")");
            return;
        }

        if (!seenUrls.add(url)) {
            return;
        }

        if (url.contains("/uploads/") && url.endsWith(".funscript")) {
            final DownloadLink dl = this.createDownloadlink(url);
            if (linkText != null && linkText.endsWith(".funscript") && !linkText.contains("<")) {
                dl.setFinalFileName(linkText);
            }
            ret.add(dl);
            return;
        }

        // Strip query string before extension detection to handle URLs like "file.jpeg?dl=1"
        final String urlPath = url.contains("?") ? url.substring(0, url.indexOf('?')) : url;
        final String ext = Files.getExtension(urlPath, true);

        if (ext != null && VIDEO_EXTENSIONS.contains(ext)) {
            ret.add(this.createDownloadlink(url));
            return;
        }

        if (ext != null && IMAGE_EXTENSIONS.contains(ext)) {
            if (!extractImages) {
                return;
            }
            final DownloadLink dl = this.createDownloadlink(url);
            dl.setProperty(PROP_IMG, IMG_STATIC);
            ret.add(dl);
            return;
        }

        if (isHandledByPlugin(url)) {
            ret.add(this.createDownloadlink(url));
        }
    }

    /**
     * Returns {@code true} if at least one non-generic JDownloader plugin (crawler or hoster) can handle the given URL. Generic catch-all
     * plugins ({@code "http links"}, {@code "https links"}) and this plugin itself are excluded from the check.
     *
     * @param url
     *            the URL to test
     */
    private boolean isHandledByPlugin(final String url) {
        for (final LazyCrawlerPlugin p : getCrawler().getSortedLazyCrawlerPlugins()) {
            if (p.getDisplayName().equalsIgnoreCase(getHost())) {
                continue;
            }
            if (p.canHandle(url)) {
                return true;
            }
        }
        for (final LazyHostPlugin p : getCrawler().getSortedLazyHostPlugins()) {
            final String name = p.getDisplayName();
            if (name.equalsIgnoreCase("http links") || name.equalsIgnoreCase("https links")) {
                continue;
            }
            if (p.canHandle(url)) {
                return true;
            }
        }
        return false;
    }
}