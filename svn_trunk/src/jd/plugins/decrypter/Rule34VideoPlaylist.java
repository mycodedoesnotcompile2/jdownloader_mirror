package jd.plugins.decrypter;

import java.util.ArrayList;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52870 $", interfaceVersion = 3, names = { "rule34video.com" }, urls = { "https?://(\\w+\\.)?rule34video\\.com/playlists/(\\d+)/[^/]+/" })
public class Rule34VideoPlaylist extends PluginForDecrypt {
    public Rule34VideoPlaylist(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(CryptedLink parameter, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = parameter.getCryptedUrl();
        final String playlist_id = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        br.getPage(contenturl);
        if (br.getRequest().getHttpConnection().getResponseCode() == 404) {
            throw new DecrypterRetryException(RetryReason.FILE_NOT_FOUND);
        }
        final String title = br.getRegex("class\\s*=\\s*\"title_video\"[^>]*>\\s*(.*?)\\s*</h1>").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(Encoding.htmlDecode(title));
        }
        fp.setPackageKey(getHost() + "://playlist/" + playlist_id);
        int nextPage = 2;
        do {
            final String playListItems[] = br.getRegex("data-playlist-item\\s*=\\s*\"(.*?)\"").getColumn(0);
            if (playListItems == null || playListItems.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (String playListItem : playListItems) {
                final DownloadLink item = createDownloadlink(playListItem);
                fp.add(item);
                ret.add(item);
                distribute(item);
            }
            if (!br.containsHTML("from:0?" + nextPage)) {
                break;
            }
            br.getPage("?mode=async&function=get_block&block_id=playlist_view_playlist_view&sort_by=added2fav_date&from=" + nextPage + "&_=" + System.currentTimeMillis());
            if (br.getRequest().getHttpConnection().getResponseCode() == 404) {
                break;
            }
            nextPage++;
        } while (!isAbort());
        return ret;
    }
}
