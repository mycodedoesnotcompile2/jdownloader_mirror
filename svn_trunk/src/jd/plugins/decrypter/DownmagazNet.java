package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52549 $", interfaceVersion = 3, names = {}, urls = {})
public class DownmagazNet extends PluginForDecrypt {
    public DownmagazNet(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "downmagaz.net" });
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

    private static final Pattern PATTERN_MULTIPLE = Pattern.compile("/([a-z0-9\\-_]+)/(\\d+)-([a-z0-9-_]+)\\.html", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_REDIRECT = Pattern.compile("/out\\.php\\?f=[a-z0-9]+&down=(\\d+)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:[a-z0-9]+\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_MULTIPLE.pattern().substring(1) + "|" + PATTERN_REDIRECT.pattern().substring(1) + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (new Regex(param.getCryptedUrl(), PATTERN_MULTIPLE).patternFind()) {
            br.setFollowRedirects(true);
            br.getPage(param.getCryptedUrl());
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            String title = br.getRegex("id=\"news-title\"><a[^>]*>([^<]+)</a>").getMatch(0);
            if (title != null) {
                title = Encoding.htmlDecode(title).trim();
            }
            final String[] mirrorIDs = br.getRegex("href[^>]* data-field=\"([a-z0-9]+)\" data-down=\"\\d+\"").getColumn(0);
            if (mirrorIDs != null && mirrorIDs.length > 0) {
                /* Old handling */
                final String multiplicatorStr = br.getRegex("\\&down='\\+\\(down\\*(\\d+)\\)").getMatch(0);
                if (multiplicatorStr == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String contentIDStr = br.getRegex("data-down=\"(\\d+)\"").getMatch(0);
                if (contentIDStr == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final long downValue = Long.parseLong(contentIDStr) * Long.parseLong(multiplicatorStr);
                for (final String mirrorID : mirrorIDs) {
                    ret.add(this.createDownloadlink(br.getURL("/out.php?f=" + mirrorID + "&down=" + downValue).toExternalForm()));
                }
            }
            /* New 2023-09-05 */
            final String[] b64Strings = br.getRegex("url=([a-zA-Z0-9_/\\+\\=\\-%]+)").getColumn(0);
            if (b64Strings != null && b64Strings.length > 0) {
                for (final String b64String : b64Strings) {
                    ret.add(this.createDownloadlink(Encoding.Base64Decode(Encoding.htmlDecode(b64String))));
                }
            } else {
                /* 2023-12-19 */
                String htmlWithPlaintextLinks = br.getRegex(">\\s*Download Links:\\s*</div><br/>(.*?)<br/></div>").getMatch(0);
                if (htmlWithPlaintextLinks == null) {
                    htmlWithPlaintextLinks = br.getRequest().getHtmlCode();
                }
                final String[] urls = new Regex(htmlWithPlaintextLinks, "<a href=\"(https?://[^\"]+)\" target=\"?_blank").getColumn(0);
                if (urls != null && urls.length > 0) {
                    for (final String url : urls) {
                        ret.add(this.createDownloadlink(url));
                    }
                }
            }
            if (ret.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final FilePackage fp = FilePackage.getInstance();
            if (title != null) {
                fp.setName(title);
            } else {
                /* Fallback */
                fp.setName(br._getURL().getPath());
            }
            fp.addLinks(ret);
        } else {
            br.setFollowRedirects(false);
            br.getPage(param.getCryptedUrl());
            String redirect = br.getRedirectLocation();
            if (redirect == null) {
                redirect = br.getRegex("window\\.location\\.replace\\('(http[^<>\"\\']+)'\\)").getMatch(0);
            }
            if (redirect == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            ret.add(this.createDownloadlink(redirect));
        }
        return ret;
    }
}