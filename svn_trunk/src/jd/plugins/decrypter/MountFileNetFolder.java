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

import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.plugins.components.antiDDoSForDecrypt;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Request;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@DecrypterPlugin(revision = "$Revision: 45953 $", interfaceVersion = 3, names = { "mountfile.net" }, urls = { "https?://(?:www\\.)?mountfile\\.net/d/[A-Za-z0-9]+" })
public class MountFileNetFolder extends antiDDoSForDecrypt {
    public MountFileNetFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        ArrayList<DownloadLink> decryptedLinks = new ArrayList<DownloadLink>();
        final String parameter = param.toString();
        br.setFollowRedirects(true);
        getPage(parameter);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.canHandle(br.getURL())) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("(?i)>\\s*Folder was deleted\\s*<")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Folder is empty")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String fpName = br.getRegex("<h2 class=\"center\">Download files from folder ([^<>\"]*?)</h2>").getMatch(0);
        if (fpName == null) {
            fpName = br.getRegex("<title>Download files from folder ([^<>\"]*?) \\&mdash; Upload").getMatch(0);
        }
        final String[][] fileInfo = br.getRegex("\"(/[A-Za-z0-9]+)\" target=\"_blank\">([^<>\"]*?)</a></td>[\t\n\r ]+<td>([^<>\"]*?)</td>").getMatches();
        if (fileInfo == null || fileInfo.length == 0) {
            if (!br.containsHTML("class=\"tlist\"")) {
                /* Invalid folderURL e.g. mountfile.net/d/0 */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
        }
        for (final String linkInfo[] : fileInfo) {
            final DownloadLink dl = createDownloadlink(Request.getLocation(linkInfo[0], br.getRequest()));
            dl.setName(linkInfo[1]);
            dl.setDownloadSize(SizeFormatter.getSize(linkInfo[2]));
            dl.setAvailable(true);
            decryptedLinks.add(dl);
        }
        if (fpName != null) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(Encoding.htmlDecode(fpName.trim()));
            fp.addLinks(decryptedLinks);
        }
        return decryptedLinks;
    }

    /* NO OVERRIDE!! */
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}