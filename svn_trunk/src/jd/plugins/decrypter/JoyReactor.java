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

@DecrypterPlugin(revision = "$Revision: 51078 $", interfaceVersion = 2, names = {}, urls = {})
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/post/\\d+");
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
        final String reactor[] = br.getRegex("((https?:)?//[^/\"]+/pics/post/full/.*?(jpe?g|png|gif))").getColumn(0);
        if (reactor == null || reactor.length == 0) {
            // Unsafe content - only for registered users (google translate)
            if (br.containsHTML("Небезопасный контент - только для зарегистрированных пользователей|joyreactor\\.cc/images/unsafe_ru.gif")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                logger.info("Looks like this post doesn't contain any images (most likely text only)");
                return ret;
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
        for (final String url : reactor) {
            final DownloadLink file = createDownloadlink(DirectHTTP.createURLForThisPlugin(br.getURL(url).toExternalForm()));
            file.setAvailable(true);
            file._setFilePackage(fp);
            ret.add(file);
        }
        return ret;
    }
}
