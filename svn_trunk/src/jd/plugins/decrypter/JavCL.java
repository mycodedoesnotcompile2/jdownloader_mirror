package jd.plugins.decrypter;

import java.util.ArrayList;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;

@DecrypterPlugin(revision = "$Revision: 54120 $", interfaceVersion = 2, names = { "javcl.com" }, urls = { "https?://(?:www\\.)?javcl\\.com/([a-z0-9\\-]+)" })
public class JavCL extends PluginForDecrypt {
    public JavCL(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(CryptedLink parameter, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage(parameter.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            ret.add(this.createOfflinelink(parameter.getCryptedUrl()));
            return ret;
        }
        br.followRedirect();
        String filename = br.getRegex("<h1[^>]*>\\s*([^<]+?)\\s*</h1>").getMatch(0);
        if (filename != null) {
            filename = Encoding.htmlDecode(filename).trim();
        }
        final String data_id = br.getRegex("id=([\"']?)videoPlayer\\1[^>]*data-id=([\"']?)([0-9]+)\\2").getMatch(2);
        /* Each server/episode button contains an encoded link in its "value" attribute */
        final String data_links[] = br.getRegex("<button[^>]*value=([\"']?)([a-zA-Z0-9/+=]{16,})\\1[^>]*player-ep").getColumn(1);
        if (data_id == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (data_links == null || data_links.length == 0) {
            /* No downloadable content e.g. https://javcl.com/most-views/ */
            ret.add(this.createOfflinelink(parameter.getCryptedUrl()));
            return ret;
        }
        int index = 0;
        for (String data_link : data_links) {
            logger.info("data_id: " + data_id + ", data_links: " + data_link);
            final String url = decodejav(data_link, data_id);
            logger.info("Output of decodejav(data_link, data_id): " + url);
            final String name;
            if (data_links.length > 1) {
                name = filename + "_" + (char) ('a' + index);
            } else {
                name = filename;
            }
            final DownloadLink downloadLink = createDownloadlink(url);
            /* Used e.g. by the streamcl.click hoster plugin (StreamclClick) */
            downloadLink.setReferrerUrl(parameter.getCryptedUrl());
            if (!StringUtils.isEmpty(name)) {
                downloadLink.setProperty("javclName", name);
            }
            ret.add(downloadLink);
            index++;
        }
        return ret;
    }

    /* Static part of the XOR key, appended to the data-id of the video player. */
    private static final String DECODE_KEY_SUFFIX = "RUR3JyscVV0ABVJYBQYAAigAW1sqBBxhA213Ak5SBnQ=";

    private String decodejav(final String data_link, final String data_id) {
        final String key = new StringBuilder(Base64.encode(data_id + DECODE_KEY_SUFFIX)).reverse().toString();
        final byte[] link = Base64.decode(data_link);
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < link.length; i++) {
            sb.append((char) (link[i] ^ key.charAt(i % key.length())));
        }
        return new String(Base64.decode(sb.toString()));
    }
}
