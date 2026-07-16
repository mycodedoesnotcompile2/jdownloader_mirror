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
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52991 $", interfaceVersion = 3, names = { "newlunarrepublic.fr" }, urls = { "https?://(?:www\\.)?newlunarrepublic\\.fr/(episodes|films)/.+" })
public class NewlunarrepublicFr extends PluginForDecrypt {
    public NewlunarrepublicFr(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 500) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("<title>([^<]*?)</title>").getMatch(0);
        final String[] videourls = br.getRegex("\"([^\"]*\\.(?:webm|mkv))\"").getColumn(0);
        if (videourls == null || videourls.length == 0) {
            logger.warning("Decrypter broken for link: " + contenturl);
            return null;
        }
        for (String url : videourls) {
            /* For subtitles */
            url = br.getURL(url).toString();
            final DownloadLink direct = createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
            /* IMPORTANT: Their .webm urls won't work without correct Referer */
            direct.setReferrerUrl(this.br.getURL());
            /* It makes no sense to leave the direct urls in these case as they won't work in browser without the correct Referer. */
            direct.setContentUrl(contenturl);
            direct.setAvailable(true);
            ret.add(direct);
        }
        final String[] subtitleurls = br.getRegex("\"(/files/[^\"]*\\.(srt|vtt))\"").getColumn(0);
        if (subtitleurls == null || subtitleurls.length == 0) {
            logger.warning("Decrypter broken for link: " + contenturl);
            return null;
        }
        for (String url : subtitleurls) {
            /* For subtitles */
            url = br.getURL(url).toString();
            final DownloadLink direct = createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
            direct.setReferrerUrl(this.br.getURL());
            /* It makes no sense to leave the direct urls in these case as they won't work in browser without the correct Referer. */
            direct.setContentUrl(contenturl);
            direct.setAvailable(true);
            ret.add(direct);
        }
        if (title != null) {
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(Encoding.htmlDecode(title.trim()));
            fp.addLinks(ret);
        }
        return ret;
    }
}
