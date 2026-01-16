package jd.plugins.decrypter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52099 $", interfaceVersion = 2, names = { "genshin.hoyoverse.com" }, urls = { "https?://genshin\\.hoyoverse\\.com/(?:[a-z]{2}/)?manga/detail/(\\d+)" })
public class GenshinManga extends PluginForDecrypt {
    public GenshinManga(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        final String contentID = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage("https://sg-public-api-static.hoyoverse.com/content_v2_user/app/a1b1f9d3315447cc/getContent?iAppId=32&iInfoId=" + contentID + "&sLangKey=en-us&iAround=0");
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final int retcode = (((Number) entries.get("retcode"))).intValue();
        if (retcode != 0) {
            /*
             * {"data":null,"message":"info not found","retcode":-1002}
             */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> data = (Map<String, Object>) entries.get("data");
        final String chapterJson = data.get("sExt").toString();
        final Map<String, Object> image_info = restoreFromString(chapterJson, TypeRef.MAP);
        final String sChanId = JavaScriptEngineFactory.walkJson(data, "sChanId/{0}").toString();
        final String mangaTitle = data.get("sTitle").toString();
        if (mangaTitle == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String chapterTitle = null;
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final List<Map<String, Object>> pageEntries = (List<Map<String, Object>>) image_info.get(sChanId + "_0");
        for (final Map<String, Object> page : pageEntries) {
            final String url = page.get("url").toString();
            if (!StringUtils.startsWithCaseInsensitive(url, "http")) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final DownloadLink link = createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
            link.setFinalFileName(page.get("name").toString());
            link.setAvailable(true);
            ret.add(link);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (chapterTitle != null) {
            fp.setName(mangaTitle + "-" + chapterTitle);
        } else {
            fp.setName(mangaTitle);
        }
        fp.addLinks(ret);
        return ret;
    }
}
