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
import java.util.HashSet;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.SiteType.SiteTemplate;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 48594 $", interfaceVersion = 3, names = { "rule34.paheal.net" }, urls = { "https?://(?:www\\.)?rule34\\.paheal\\.net/post/(list/[\\w\\-\\.%!]+|view)/\\d+" })
public class Rule34PahealNet extends PluginForDecrypt {
    public Rule34PahealNet(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404 || br.containsHTML(">\\s*No Images Found\\s*<")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(new Regex(contenturl, "/post/list/(.*?)/\\d+").getMatch(0));
        final HashSet<String> loop = new HashSet<String>();
        String next = null;
        final HashSet<String> dups = new HashSet<String>();
        do {
            if (next != null) {
                sleep(1000, param);
                br.getPage(next);
            }
            String[] links = br.getRegex("<br><a href=('|\")(https?://.*?)\\1>").getColumn(1);
            if (links == null || links.length == 0) {
                links = br.getRegex("('|\")(https?://[^/]+\\.paheal\\.net/_images/[a-z0-9]+/.*?)\\1").getColumn(1);
            }
            if (links == null || links.length == 0) {
                links = br.getRegex("('|\")(https?://rule34-[a-zA-Z0-9\\-]*?\\.paheal\\.net/_images/[a-z0-9]+/.*?)\\1").getColumn(1);
            }
            if (links == null || links.length == 0) {
                logger.warning("Decrypter broken for link: " + contenturl);
                return null;
            }
            for (final String singlelink : links) {
                if (dups.add(singlelink)) {
                    final DownloadLink dl = createDownloadlink(DirectHTTP.createURLForThisPlugin(singlelink));
                    dl.setAvailable(true);
                    fp.add(dl);
                    ret.add(dl);
                    distribute(dl);
                }
            }
            next = br.getRegex("\"(/post/[^<>\"]*?)\">Next</a>").getMatch(0);
        } while (!this.isAbort() && next != null && loop.add(next));
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.Danbooru;
    }
}