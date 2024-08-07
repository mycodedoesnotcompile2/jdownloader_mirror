package jd.plugins.decrypter;

import java.util.ArrayList;

import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 49103 $", interfaceVersion = 2, names = { "emulatorgames.net" }, urls = { "https?://(?:www\\.)?emulatorgames\\.net/(?:(?:roms|download)/).+" })
public class EmulatorgamesNet extends PluginForDecrypt {
    public EmulatorgamesNet(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Form dlform = br.getFormbyActionRegex(".*/download.*");
        if (dlform == null && contenturl.matches("(?i).*\\.net/roms/[^/]*/?$")) {
            // ignore index site
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String fpName = null;
        fpName = br.getRegex("([^<>]+)\\s+ROM\\s+-\\s+[^<]+\\s+-\\s+Emulator Games").getMatch(0);
        final String romID = br.getRegex("data-id=\"(\\d+)\"").getMatch(0);
        if (StringUtils.isEmpty(romID)) {
            getLogger().warning("Could not retrieve ROM ID required for download steps!");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.submitForm(dlform);
        // final PostRequest downloadPagePost = new PostRequest(br.getURL("/increment/"));
        // downloadPagePost.addVariable("get_type", "post");
        // downloadPagePost.addVariable("get_id", romID);
        // downloadPagePost.getHeaders().put("Referer", br.getURL());
        // downloadPagePost.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        // downloadPagePost.setContentType("application/x-www-form-urlencoded; charset=UTF-8");
        final PostRequest romTargetPost = new PostRequest(br.getURL("/prompt/"));
        romTargetPost.addVariable("get_type", "post");
        romTargetPost.addVariable("get_id", romID);
        romTargetPost.getHeaders().put("Referer", br.getURL(dlform.getAction()).toString());
        romTargetPost.getHeaders().put("Origin", "https://www." + br.getHost());
        romTargetPost.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        romTargetPost.setContentType("application/x-www-form-urlencoded; charset=UTF-8");
        br.getPage(romTargetPost);
        String directurl = br.getRegex("\\[\\s*\"([^\"]+)").getMatch(0);
        if (directurl == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        directurl = PluginJSonUtils.unescape(directurl);
        ret.add(createDownloadlink(DirectHTTP.createURLForThisPlugin(directurl)));
        if (StringUtils.isNotEmpty(fpName)) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(Encoding.htmlDecode(fpName).trim());
            fp.addLinks(ret);
        }
        return ret;
    }
}