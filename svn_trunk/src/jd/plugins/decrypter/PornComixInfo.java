package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52595 $", interfaceVersion = 3, names = {}, urls = {})
public class PornComixInfo extends PluginForDecrypt {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "bestporncomix.com", "porncomix.online", "porncomix.info" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/gallery/([a-z0-9\\-]+)/?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setFollowRedirects(true);
        String contenturl = param.getCryptedUrl();
        /* Replace dead domains with a working one */
        contenturl = contenturl.replace(Browser.getHost(contenturl) + "/", this.getHost() + "/");
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String title_from_url = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        String postTitle = br.getRegex("<title>([^<>\"]+) - \\| 18\\+ Porn Comics</title>").getMatch(0);
        if (StringUtils.isEmpty(postTitle)) {
            /* Fallback */
            postTitle = title_from_url.replace("-", " ").trim();
        }
        postTitle = Encoding.htmlDecode(postTitle).trim();
        String[] images = br.getRegex("/ImageObject\" data-pswp-src=\"(https[^\"]+)").getColumn(0);
        if (images == null || images.length == 0) {
            images = br.getRegex("data-pswp-src=\"(https[^\"]+)").getColumn(0);
        }
        if (images != null && images.length > 0) {
            for (final String imageurl : images) {
                final DownloadLink link = createDownloadlink(DirectHTTP.createURLForThisPlugin(imageurl));
                link.setAvailable(true);
                link.setContainerUrl(contenturl);
                ret.add(link);
            }
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(Encoding.htmlDecode(postTitle));
        fp.addLinks(ret);
        return ret;
    }
}
