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
import java.util.regex.Pattern;

import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
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

@DecrypterPlugin(revision = "$Revision: 50489 $", interfaceVersion = 3, names = { "4share.vn" }, urls = { "https?://(?:www\\.)?(?:up\\.)?4share\\.vn/(?:d|dlist)/([a-f0-9]{16})" })
public class FourShareVnFolder extends PluginForDecrypt {
    public FourShareVnFolder(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl().replace("up.4share.vn/", "4share.vn/");
        final String folderID = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
        if (folderID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        br.setConnectTimeout(2 * 60 * 1000);
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Error: Not valid ID") && !br.containsHTML("up\\.4share\\.vn/f/")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("File suspended:") || br.containsHTML(">\\s*ErrorWeb: Not found folder")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML(">\\s*Empty folder")) {
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        } else if (!this.canHandle(br.getURL())) {
            /* E.g. redirect to mainpage */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final HashSet<String> dupes = new HashSet<String>();
        String currentFolderTitle = br.getRegex("<h1[^>]*>\\s*Folder: ([^<]+)</h1>").getMatch(0);
        if (currentFolderTitle == null) {
            currentFolderTitle = br.getRegex("<b>\\s*Thư mục:\\s*(.*?)\\s*</b>").getMatch(0);
            if (currentFolderTitle == null) {
                /* 2025-01-22 */
                currentFolderTitle = br.getRegex("<h2 class=\"\"[^>]*>([^<]+)</h2>").getMatch(0);
            }
        }
        if (currentFolderTitle == null) {
            /* Fallback */
            currentFolderTitle = folderID;
        }
        currentFolderTitle = Encoding.htmlDecode(currentFolderTitle).trim();
        String subfolderpath = this.getAdoptedCloudFolderStructure();
        if (subfolderpath == null) {
            subfolderpath = currentFolderTitle;
        } else {
            subfolderpath += "/" + currentFolderTitle;
        }
        FilePackage fp = FilePackage.getInstance();
        if (subfolderpath != null) {
            fp = FilePackage.getInstance();
            fp.setName(subfolderpath);
        } else {
            fp.setName(currentFolderTitle);
        }
        final String[] subfolderurls = br.getRegex("(/d/[a-f0-9]{16})").getColumn(0);
        if (subfolderurls != null && subfolderurls.length > 0) {
            for (String url : subfolderurls) {
                if (url.contains(folderID)) {
                    /* Skip folder which we are crawling at this moment */
                    continue;
                } else if (!dupes.add(url)) {
                    continue;
                }
                url = br.getURL(url).toExternalForm();
                final DownloadLink folder = createDownloadlink(url);
                folder.setRelativeDownloadFolderPath(subfolderpath);
                ret.add(folder);
            }
        }
        final String[] fileurls = br.getRegex("(/f/[a-f0-9]{16}(/[^<>\"]{1,})?)").getColumn(0);
        if (fileurls != null && fileurls.length > 0) {
            for (String url : fileurls) {
                if (!dupes.add(url)) {
                    continue;
                }
                final String urlQuoted = Pattern.quote(url);
                final String filename = br.getRegex(urlQuoted + "\">\\s*<[^>]*></i>([^<]+)</a>").getMatch(0);
                final String filesize = br.getRegex(">([^<]+)</td>\\s*<td><a href=\"" + urlQuoted).getMatch(0);
                url = br.getURL(url).toExternalForm();
                final DownloadLink file = createDownloadlink(url);
                if (filename != null) {
                    file.setName(Encoding.htmlDecode(filename).trim());
                }
                if (filesize != null) {
                    file.setDownloadSize(SizeFormatter.getSize(filesize));
                }
                file.setRelativeDownloadFolderPath(subfolderpath);
                file.setAvailable(true);
                file._setFilePackage(fp);
                ret.add(file);
            }
        }
        if (ret.isEmpty()) {
            /* Assume that we got an empty table of contents -> Empty folder */
            throw new DecrypterRetryException(RetryReason.EMPTY_FOLDER);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}