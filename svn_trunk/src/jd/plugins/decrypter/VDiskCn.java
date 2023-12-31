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

import java.net.SocketTimeoutException;
import java.util.ArrayList;

import org.appwork.utils.formatter.SizeFormatter;

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

@DecrypterPlugin(revision = "$Revision: 45355 $", interfaceVersion = 2, names = { "vdisk.cn" }, urls = { "http://(www\\.)?vdisk\\.cn/(?!down/)[a-z0-9]{4,}/$" })
public class VDiskCn extends PluginForDecrypt {
    public VDiskCn(PluginWrapper wrapper) {
        super(wrapper);
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> decryptedLinks = new ArrayList<DownloadLink>();
        final ArrayList<String> allPages = new ArrayList<String>();
        final String parameter = param.toString();
        br.setReadTimeout(3 * 60 * 1000);
        br.setConnectTimeout(3 * 60 * 1000);
        br.setFollowRedirects(true);
        br.getPage(parameter);
        if (br.containsHTML(">找不到您需要的页面\\!<|可能您访问的内容已经删除，或您无权访问本页面。<")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("class=\\'tab_sel\\'>所有文件\\(0\\)</div>")) {
            logger.info("Link offline (folder empty): " + parameter);
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String[] pages = br.getRegex("<a href=\\'\\?tag=ALLFILES\\&p=(\\d+)\\'").getColumn(0);
        if (pages == null || pages.length == 0) {
            allPages.add("1");
        } else {
            for (final String currentPage : pages) {
                if (!allPages.contains(currentPage)) {
                    allPages.add(currentPage);
                }
            }
        }
        int lastPage = Integer.parseInt(allPages.get(allPages.size() - 1));
        for (int i = 1; i <= lastPage; i++) {
            if (this.isAbort()) {
                logger.info("Decryption aborted by user: " + parameter);
                return decryptedLinks;
            }
            logger.info("Decrypting page " + i + " of " + lastPage);
            String currentPage = parameter;
            try {
                if (i != 1) {
                    currentPage = parameter + "?p=" + i;
                    br.getPage(currentPage);
                }
            } catch (final SocketTimeoutException e) {
                logger.info("Failed to decrypt current page (timeout) -> Skipping link: " + currentPage);
            }
            final String[][] links = br.getRegex("'(/[a-zA-Z0-9]+/.*?\\.html)'.*?blank'>(.*?)</a.*?(\\d+(?:\\.\\d+)?\\s*[KMGT]{0,1}B)").getMatches();
            if ((links == null || links.length == 0) && br.containsHTML("此处有个隐藏文件,只有上传者可见\\.\\.\\.")) {
                logger.info("Current page contains only inaccessable links, skipping that: " + currentPage);
            } else if (links == null || links.length == 0) {
                logger.warning("Decrypter broken for current link: " + currentPage);
                return null;
            }
            DownloadLink link;
            String filename;
            for (String singleLink[] : links) {
                link = createDownloadlink("http://vdisk.cn" + singleLink[0]);
                /* [NEW] --> Remove that from filenames */
                filename = singleLink[1].replace("<font color=\"#FF0000\">[新]</font>", "");
                link.setName(filename);
                link.setDownloadSize(SizeFormatter.getSize(singleLink[2].trim()));
                link.setAvailable(true);
                distribute(link);
                decryptedLinks.add(link);
            }
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(new Regex(parameter, "([a-z0-9]+)/?$").getMatch(0));
        fp.addLinks(decryptedLinks);
        return decryptedLinks;
    }

    /* NO OVERRIDE!! */
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}