package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.List;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52087 $", interfaceVersion = 2, names = {}, urls = {})
public class JoyReactor extends PluginForDecrypt {
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "reactor.cc", "joyreactor.cc", "joyreactor.com" });
        return ret;
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/post/(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    public JoyReactor(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String directurls[] = br.getRegex("((https?:)?//[^/\"]+/pics/post/full/.*?(jpe?g|png|gif))").getColumn(0);
        if (directurls == null || directurls.length == 0) {
            /* 2026-01-13: For .webm/.mp4 items */
            directurls = br.getRegex("data-src=\"(https://[^\"]+)\"").getColumn(0);
        }
        if (directurls == null || directurls.length == 0) {
            // Unsafe content - only for registered users (google translate)
            if (br.containsHTML("Небезопасный контент - только для зарегистрированных пользователей|joyreactor\\.cc/images/unsafe_ru\\.gif")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        String title = br.getRegex("<title>([^<]+)").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            fp.setName(title);
        } else {
            /* Fallback */
            fp.setName(br._getURL().getPath());
        }
        for (final String url : directurls) {
            final DownloadLink file = createDownloadlink(DirectHTTP.createURLForThisPlugin(br.getURL(url).toExternalForm()));
            file.setAvailable(true);
            /* Important otherwise some direct-urls may not work. */
            file.setReferrerUrl(br.getURL());
            file._setFilePackage(fp);
            ret.add(file);
        }
        return ret;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2026-01-13: Try to avoid rate limit (error 503) */
        return 1;
    }
}
