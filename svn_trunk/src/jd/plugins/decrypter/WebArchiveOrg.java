package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.http.requests.GetRequest;
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
import jd.plugins.PluginForHost;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52737 $", interfaceVersion = 3, names = {}, urls = {})
public class WebArchiveOrg extends PluginForDecrypt {
    private static final Pattern PATTERN_DIRECT = Pattern.compile("https?://web\\.archive\\.org/web/(\\d+)(if|im|oe)_/(https?.+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_OTHER  = Pattern.compile("https?://web\\.archive\\.org/web/(\\d+)/(https?.+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_FILE   = Pattern.compile("^https?://[^/]+/web/(\\d+)[^/]*/(.+)", Pattern.CASE_INSENSITIVE);

    public WebArchiveOrg(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "web.archive.org" });
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
            ret.add("https?://" + buildHostsPatternPart(domains) + "/web/[0-9]+((if|im|oe)_|\\*)?/.+");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final Regex regexFile = new Regex(contenturl, PATTERN_FILE);
        final String originalURL = new Regex(contenturl, PATTERN_OTHER).getMatch(1);
        final PluginForDecrypt yt = getNewPluginForDecryptInstance("youtube.com");
        final PluginForHost directhttp = getNewPluginForHostInstance("http links");
        final String youtubeVideoID = yt != null && yt.canHandle(originalURL) ? TbCmV2.getVideoIDFromUrl(contenturl) : null;
        if (youtubeVideoID != null) {
            /* Look for direct-URLs */
            final Browser brc = br.cloneBrowser();
            brc.setFollowRedirects(false);
            brc.getPage("https://web.archive.org/web/2oe_/http://wayback-fakeurl.archive.org/yt/" + Encoding.urlEncode(youtubeVideoID));
            /* Expect redirect to streaming-direct-downloadurl */
            final String directurl = brc.getRedirectLocation();
            if (directurl == null) {
                /* Simply assume that item is offline. */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            ret.add(createDownloadlink(DirectHTTP.createURLForThisPlugin(directurl)));
            return ret;
        }
        final Regex regexDirect = new Regex(contenturl, PATTERN_DIRECT);
        if (regexDirect.patternFind()) {
            /* direct-URL */
            ret.add(createDownloadlink(DirectHTTP.createURLForThisPlugin(contenturl)));
            return ret;
        }
        if (!regexFile.patternFind()) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Unsupported URL:" + contenturl);
        }
        /* Unsure if we got a direct-URL or if the url leads us to an html page with more downloadable items -> Check it */
        final String fileID = regexFile.getMatch(0);
        final String linkpart = regexFile.getMatch(1);
        final String fileurl = "https://web.archive.org/web/" + fileID + "if_" + "/" + linkpart;
        /* First check if maybe the user has added a directURL. */
        final GetRequest getRequest = br.createGetRequest(fileurl);
        final URLConnectionAdapter con = br.openRequestConnection(getRequest);
        final FilePackage fp = FilePackage.getInstance();
        try {
            if (this.looksLikeDownloadableContent(con)) {
                /* Direct downloadable url */
                final DownloadLink direct = getCrawler().createDirectHTTPDownloadLink(getRequest, con);
                ret.add(direct);
                return ret;
            }
            /* Not a direct downloadable url -> Check what we got */
            br.followConnection();
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (!br.getURL().contains(fileID)) {
                /* E.g. redirect to main page */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            fp.setName(br._getURL().getPath());
            fp.setPackageKey("waybackmachine://" + br._getURL().getPath());
            final String urls[] = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
            final Set<String> direct_url = new HashSet<String>();
            if (urls != null) {
                for (final String url : urls) {
                    if (!canHandle(url)) {
                        /* Ignore unsupported links */
                        continue;
                    }
                    if (PATTERN_DIRECT.matcher(url).matches() && direct_url.add(url)) {
                        direct_url.add(url);
                    } else {
                        final String externalWebsiteLink = new Regex(url, "https?://.*?(https*://.+)").getMatch(0);
                        if (externalWebsiteLink != null && directhttp.canHandle(externalWebsiteLink)) {
                            /* Generate direct-downloadable url */
                            final String directURL = "https://web.archive.org/web/" + fileID + "if_" + "/" + externalWebsiteLink;
                            direct_url.add(directURL);
                        }
                    }
                }
            }
            if (direct_url.size() > 0) {
                for (final String this_direct_url : direct_url) {
                    final DownloadLink file = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(this_direct_url));
                    /**
                     * Extract default filename here. <br>
                     * This is a small workaround since our default handling may fail for URLs like the following one: <br>
                     * https://web.archive.org/web/201657464848484if_/http://somesite.tld/123.html?file=files/Internet/2/3/4/filename.pdf
                     */
                    final String filename = new Regex(this_direct_url, "/([^/]+)$").getMatch(0);
                    if (filename != null) {
                        file.setName(filename);
                    }
                    ret.add(file);
                }
            } else {
                /* E.g. embedded PDF */
                // TODO: Review this handling
                final String directurl = br.getRegex("<iframe id=\"playback\"[^>]*src=\"(https?://[^\"]+)").getMatch(0);
                if (directurl == null) {
                    logger.info("URL is not supported or content is offline");
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                ret.add(this.createDownloadlink(DirectHTTP.createURLForThisPlugin(directurl)));
            }
        } finally {
            con.disconnect();
        }
        for (final DownloadLink result : ret) {
            result.setAvailable(true);
            result._setFilePackage(fp);
        }
        return ret;
    }

    @PluginHost(host = "web.archive.org", type = Type.CRAWLER)
    public static interface WebArchiveOrgConfig extends PluginConfigInterface {
        @AboutConfig
        @DefaultBooleanValue(false)
        @DescriptionForConfigEntry("Crawl embedded files from web.archive.org link?")
        @Order(10)
        boolean isCrawlEmbeddedFilesEnabled();

        void setCrawlEmbeddedFilesEnabled(boolean b);
    }

    @Override
    public Class<WebArchiveOrgConfig> getConfigInterface() {
        return WebArchiveOrgConfig.class;
    }
}
