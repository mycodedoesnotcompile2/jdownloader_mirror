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
import jd.controlling.ProgressController;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52759 $", interfaceVersion = 2, names = { "firsturl.de" }, urls = { "https?://[\\w\\.]*?firsturl\\.de/(?!kontakt)[0-9a-zA-Z]{7}" })
public class FrstrlD extends PluginForDecrypt {
    public FrstrlD(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(CryptedLink parameter, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setFollowRedirects(true);
        final String contenturl = parameter.getCryptedUrl().replace("firsturl.net", "firsturl.de");
        br.getPage(contenturl);
        String redirect = br.getRegex("<p>The document has moved <a href=\"(.*?)\">here</a>.</p>").getMatch(0);
        if (redirect != null) {
            br.getPage(redirect);
        }
        if (br.getURL().matches(".+/not_exists\\.html")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String linkurl = br.getRegex("location.href='(.*?)';").getMatch(0);
        if (linkurl == null) {
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED);
        }
        ret.add(createDownloadlink(linkurl));
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}