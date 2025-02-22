//jDownloader - Downloadmanager
//Copyright (C) 2009  JD-Team support@jdownloader.org
//
//This program is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.decrypter;

import java.util.ArrayList;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.DirectHTTP;
import jd.utils.JDUtilities;

@DecrypterPlugin(revision = "$Revision: 50681 $", interfaceVersion = 3, names = { "slideshare.net" }, urls = { "https?://(?:(?:\\w+)\\.)?slideshare\\.net/(slideshow/[a-z0-9\\-]+/\\d+|[a-z0-9\\-_]+/[a-z0-9\\-_]+)" })
public class SlideShareNetDecrypter extends PluginForDecrypt {
    public SlideShareNetDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        jd.plugins.hoster.SlideShareNet.prepBR(br);
        final String contenturl = param.getCryptedUrl().replaceAll("https?://(?:[a-z0-9]+\\.)?slideshare\\.net/", "https://www.slideshare.net/");
        final PluginForHost hostplugin = this.getNewPluginForHostInstance(this.getHost());
        final DownloadLink mainlink = createDownloadlink(contenturl);
        mainlink.setDefaultPlugin(hostplugin);
        mainlink.setHost(this.getHost());
        String title = null;
        final boolean isLoggedIN = getUserLogin(false);
        br.getPage(contenturl);
        br.followRedirect();
        if (jd.plugins.hoster.SlideShareNet.isOffline(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (br.containsHTML("class=\"profileHeader\"")) {
            // TODO: Check if this is still needed
            /* All documents/presentations/videos/info graphics of a user */
            int pagenum = 0;
            String next = null;
            do {
                logger.info("Crawling page: " + pagenum);
                if (this.isAbort()) {
                    logger.info("Decryption aborted by user");
                    return ret;
                }
                if (pagenum > 0) {
                    this.br.getPage(next);
                }
                final String[] entries = this.br.getRegex("<a class=(?:\"|\\')notranslate(?:\"|\\') title=(?:\"|\\')[^<>\"]+(?:\"|\\') href=(?:\"|\\')(/[^<>\"]*?)(?:\"|\\')").getColumn(0);
                if (entries == null || entries.length == 0) {
                    return null;
                }
                for (String url : entries) {
                    url = url = br.getURL(url).toExternalForm();
                    ret.add(this.createDownloadlink(url));
                }
                next = this.br.getRegex("href=\"(/[^<>\"]*?\\d+)\" rel=\"next\"").getMatch(0);
                pagenum++;
            } while (next != null);
        }
        /* Single url */
        title = br.getRegex("<title>([^<>\"]*?)</title>").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title.trim());
        }
        if (title == null) {
            /* Fallback */
            title = br._getURL().getPath();
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        final String[] htmls = br.getRegex("data-testid=\"vertical-slide-image\"([^<]+)").getColumn(0);
        if (htmls != null && htmls.length > 0) {
            for (final String html : htmls) {
                final String[] qualities = new Regex(html, "\\d+w, (https?://[^ ]+)").getColumn(0);
                /* Best quality version of each document page. */
                final String bestQualityImageURL = qualities[qualities.length - 1];
                final DownloadLink dl = createDownloadlink(DirectHTTP.createURLForThisPlugin(bestQualityImageURL));
                dl.setAvailable(true);
                dl._setFilePackage(fp);
                ret.add(dl);
            }
        }
        if (ret.isEmpty() || isLoggedIN) {
            mainlink.setName(title + ".pdf");
            mainlink.setAvailable(true);
            ret.add(mainlink);
        }
        return ret;
    }

    private boolean getUserLogin(final boolean force) throws Exception {
        final PluginForHost hostPlugin = JDUtilities.getPluginForHost(this.getHost());
        final Account aa = AccountController.getInstance().getValidAccount(hostPlugin);
        if (aa == null) {
            logger.warning("There is no account available, stopping...");
            return false;
        }
        try {
            ((jd.plugins.hoster.SlideShareNet) hostPlugin).login(this.br, aa, force);
        } catch (final PluginException e) {
            aa.setValid(false);
            return false;
        }
        // Account is valid, let's just add it
        AccountController.getInstance().addAccount(hostPlugin, aa);
        return true;
    }

    @Override
    public boolean isProxyRotationEnabledForLinkCrawler() {
        return false;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
