package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 51084 $", interfaceVersion = 2, names = { "liveme.com" }, urls = { "https?://(?:www\\.)?liveme\\.com/(?:media/play/\\?videoid=\\d+|media/liveshort/dist/\\?videoid=\\d+&.*?|live\\.html\\?videoid=\\d+.*?|.*?/\\d+/index.html)" })
public class LiveMeCom extends PluginForDecrypt {
    public LiveMeCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* See: https://svn.jdownloader.org/issues/90516 */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final String videoid = getVideoID(contenturl);
        final String vali = vali(4) + "l" + vali(4) + "m" + vali(5);
        // br.postPage("https://live.ksmobile.net/live/queryinfo", "userid=1&videoid=" + videoid + "&area=&h5=1&vali=" + vali);
        /**
         * TODO: <br>
         * - Implement signed requests <br>
         * - Find all the parameters needed for the request down below
         */
        br.getHeaders().put("lm-s-sign", "TODO");
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("lm_s_id", "TODO");
        query.appendEncoded("lm_s_ts", "TODO");
        query.appendEncoded("lm_s_str", "TODO");
        query.appendEncoded("lm_s_ver", "1");
        query.appendEncoded("h5", "1");
        query.appendEncoded("_time", Long.toString(System.currentTimeMillis()));
        query.appendEncoded("thirdchannel", "6");
        query.appendEncoded("videoid", videoid);
        query.appendEncoded("area", "en");
        query.appendEncoded("vali", vali);
        query.appendEncoded("tuid", "TODO");
        query.appendEncoded("uid", "TODO");
        query.appendEncoded("userid", "TODO");
        query.appendEncoded("token", "TODO");
        query.appendEncoded("androidid", "TODO");
        br.postPage("https://live.liveme.com/live/queryinfosimple?alias=liveme&tongdun_black_box=TODO&os=web", query);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> response = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> data = (Map<String, Object>) response.get("data");
        final Map<String, Object> video_info = (Map<String, Object>) data.get("video_info");
        final Map<String, Object> user_info = (Map<String, Object>) data.get("user_info");
        final String title = (String) video_info.get("title");
        final String desc = (String) user_info.get("desc");
        final String url = (String) video_info.get("videosource");
        final Object videosize = video_info.get("videosize");
        final DownloadLink link;
        if (StringUtils.endsWithCaseInsensitive(url, "m3u8")) {
            link = createDownloadlink("m3u8" + url.substring(4));
        } else if (StringUtils.isNotEmpty(url)) {
            link = createDownloadlink(url);
        } else {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (videosize != null) {
            link.setDownloadSize(JavaScriptEngineFactory.toLong(videosize, -1));
        }
        if (StringUtils.isAllNotEmpty(title, desc)) {
            link.setFinalFileName(desc + "_" + title + ".mp4");
        } else if (StringUtils.isNotEmpty(title)) {
            link.setFinalFileName(title + ".mp4");
        } else if (StringUtils.isNotEmpty(desc)) {
            link.setFinalFileName(desc + ".mp4");
        }
        link.setContentUrl(param.getCryptedUrl());
        ret.add(link);
        return ret;
    }

    private String vali(int t) {
        // vali = t(4) + "l" + t(4) + "m" + t(5);
        // function t(t) {
        // t = t || 32;
        // for (var n = "ABCDEFGHJKMNPQRSTWXYZabcdefhijkmnprstwxyz2345678", e = n.length, A = "", r = 0; r < t; r++)
        // A += n.charAt(Math.floor(Math.random() * e));
        // return A
        // }
        final String n = "ABCDEFGHJKMNPQRSTWXYZabcdefhijkmnprstwxyz2345678";
        String A = "";
        for (int r = 0; r < t; r++) {
            A += n.charAt((int) Math.floor(Math.random() * n.length()));
        }
        return A;
    }

    private String getVideoID(final String url) {
        String result = new Regex(url, "(?i)[&?]videoid=(\\d+)").getMatch(0);
        if (result == null) {
            result = new Regex(url, "(?i)/(\\d+)/index\\.html").getMatch(0);
        }
        return result;
    }
}
