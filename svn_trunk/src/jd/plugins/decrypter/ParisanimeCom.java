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

import org.jdownloader.plugins.components.antiDDoSForDecrypt;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@DecrypterPlugin(revision = "$Revision: 49699 $", interfaceVersion = 3, names = { "parisanime.com" }, urls = { "https?://(?:www\\.)?parisanime\\.com/video/[A-Za-z0-9]+" })
public class ParisanimeCom extends antiDDoSForDecrypt {
    public ParisanimeCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        /* 2020-05-11: First request can be skipped saving us some milliseconds ;) */
        final boolean skipFirstRequest = true;
        if (!skipFirstRequest) {
            getPage(contenturl);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
        }
        br.getHeaders().put("Referer", contenturl);
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404 || br.getRequest().getHtmlCode().trim().equalsIgnoreCase("no link found")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String finallink = this.br.getRegex("data-url=\\'((https?:)?//[^<>\"\\']+)\\'").getMatch(0);
        if (finallink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        finallink = br.getURL(finallink).toExternalForm();
        ret.add(createDownloadlink(finallink));
        return ret;
    }
}
