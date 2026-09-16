package jd.plugins.decrypter;

import java.awt.Dialog.ModalityType;
import java.util.ArrayList;

import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 53426 $", interfaceVersion = 2, names = { "mycima.cc" }, urls = { "https://(?:[a-z]+\\.)?mycima\\.cc/[a-z]+\\.php\\?vid=[a-z0-9]+" })
public class MycimaCc extends PluginForDecrypt {
    public MycimaCc(PluginWrapper wrapper) {
        super(wrapper);
    }

    /** 2021-02-18: Formerly known as: javqd.tv */
    @Override
    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String link = param.getCryptedUrl();
        final String media_id = new Regex(link, "vid=([a-z0-9]+)$").getMatch(0);
        final String download_url = link.replaceFirst("/[a-z]+\\.php", "/downloads.php");
        br.getPage(download_url);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String[] seasons_container = new Regex(br.getRequest().getHtmlCode(), "<div id=\"Season\\d+\" class=\"tabcontent\"(.*?)</div>").getColumn(0);
        final ConfirmDialog confirm = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, "Crawl video " + media_id + " or the whole season", "This link contains a video and a season. What do you want do download?", null, "Episode", "Season") {
            @Override
            public ModalityType getModalityType() {
                return ModalityType.MODELESS;
            }

            @Override
            public boolean isRemoteAPIEnabled() {
                return true;
            }
        };
        CrawlMode choice = CrawlMode.EPISODE;
        if (param.getSource() == null && seasons_container.length > 0) {
            try {
                UIOManager.I().show(ConfirmDialogInterface.class, confirm).throwCloseExceptions();
            } catch (final DialogCanceledException e) {
                choice = CrawlMode.SEASON;
            } catch (final DialogClosedException e) {
                choice = CrawlMode.NONE;
            }
        }
        if (choice.equals(CrawlMode.EPISODE)) {
            final String[] container = new Regex(br.getRequest().getHtmlCode(), "<div id=\"pm-download\"(.*?)</div>").getColumn(0);
            if (container.length > 0) {
                final String[] links = new Regex(container[0], "href=\"([^\"]+)\"").getColumn(0);
                for (final String url : links) {
                    ret.add(this.createDownloadlink(url));
                }
            }
        }
        if (choice.equals(CrawlMode.SEASON)) {
            final String[] links = new Regex(seasons_container[0], "href=\"([^\"]+)\"").getColumn(0);
            for (final String url : links) {
                ret.add(this.createDownloadlink(url));
            }
        }
        return ret;
    }

    enum CrawlMode {
        NONE,
        EPISODE,
        SEASON
    }
}
