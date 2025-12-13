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

import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.SiteType.SiteTemplate;

@DecrypterPlugin(revision = "$Revision: 51975 $", interfaceVersion = 3, names = { "xbooru.com" }, urls = { "https?://(?:www\\.)?xbooru\\.com/index\\.php\\?page=post\\&s=list\\&tags=[A-Za-z0-9\\_]+" })
public class XbooruCom extends PluginForDecrypt {
    public XbooruCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String fpName = new Regex(contenturl, "tags=([A-Za-z0-9\\_]+)").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(Encoding.htmlDecode(fpName).trim());
        final String url_part = contenturl;
        int page_counter = 1;
        int offset = 0;
        final int max_entries_per_page = 42;
        int entries_per_page_current = 0;
        do {
            if (this.isAbort()) {
                logger.info("Decryption aborted by user");
                return ret;
            }
            if (page_counter > 1) {
                this.br.getPage(url_part + "&pid=" + offset);
            }
            logger.info("Decrypting: " + this.br.getURL());
            final String[][] links = br.getRegex("id=\"s(\\d+)\".*?alt=\"\\s*(.*?)\\s*\"").getMatches();
            if (links == null || links.length == 0) {
                logger.warning("Decrypter might be broken for link: " + contenturl);
                break;
            }
            entries_per_page_current = links.length;
            for (final String[] link : links) {
                final String linkID = link[0];
                final String url = "https://" + this.getHost() + "/index.php?page=post&s=view&id=" + linkID;
                final DownloadLink dl = createDownloadlink(url);
                dl.setLinkID(getHost() + "://" + linkID);
                dl.setAvailable(true);
                if (StringUtils.isNotEmpty(link[1])) {
                    dl.setName(linkID + "_" + link[1] + ".jpeg");
                } else {
                    dl.setName(linkID + ".jpeg");
                }
                dl._setFilePackage(fp);
                ret.add(dl);
                distribute(dl);
                offset++;
            }
            page_counter++;
        } while (entries_per_page_current >= max_entries_per_page);
        return ret;
    }

    @Override
    public SiteTemplate siteTemplateType() {
        return SiteTemplate.Danbooru;
    }
}
