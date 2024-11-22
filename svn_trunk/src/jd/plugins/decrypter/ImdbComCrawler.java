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
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.ImDbCom;

@DecrypterPlugin(revision = "$Revision: 50207 $", interfaceVersion = 2, names = { "imdb.com" }, urls = { "https?://(?:www\\.)?imdb\\.com/((?:name|title)/(?:nm|tt)\\d+((?:/mediaindex|videogallery)|/media/index/rg\\d+)?)" })
public class ImdbComCrawler extends PluginForDecrypt {
    public ImdbComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final String TYPE_ARTIST        = "(?i)https?://(www\\.)?imdb\\.com/media/index/rg\\d+";
    private static final String TYPE_TITLE         = "(?i)https?://(www\\.)?imdb\\.com/name|title/tt\\d+/mediaindex";
    private static final String TYPE_NAME          = "(?i)https?://(www\\.)?imdb\\.com/name/nm\\d+/mediaindex";
    private static final String TYPE_VIDEO_GALLERY = "(?i).+/videogallery";

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String contenturl = param.getCryptedUrl().replaceFirst("(?i)http://", "https://");
        if (contenturl.matches(".+(?:name|title)/(?:nm|tt)\\d+")) {
            contenturl = contenturl + "/videogallery";
        }
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getRequest().getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (this.br.containsHTML("id=\"no_content\"")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (this.br.containsHTML("class=\"ilm_notice\"")) {
            /*
             * E.g. <div class="ilm_notice"> <p>We're sorry. We don't have any videos that match your search.</p> </div>
             */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        int maxpage = 1;
        final String[] pages = br.getRegex("\\?page=(\\d+)\\&ref_=").getColumn(0);
        if (pages != null && pages.length != 0) {
            for (final String page : pages) {
                final int curpage = Integer.parseInt(page);
                if (curpage > maxpage) {
                    maxpage = curpage;
                }
            }
        }
        String fpName = br.getRegex("itemprop=\\'url\\'>([^<>\"]*?)</a>").getMatch(0);
        if (fpName == null) {
            fpName = br.getRegex("<title>\\s*(.*?)\\s*(-\\s*Videos\\s*-\\s*IMDb\\s*)?</title>").getMatch(0);
        }
        if (fpName == null) {
            fpName = "imdb.com - " + new Regex(contenturl, "([a-z]{2}\\d+)").getMatch(0);
        }
        fpName = Encoding.htmlDecode(fpName).trim();
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(fpName);
        final HashSet<String> dupes = new HashSet<String>();
        pagination: for (int i = 1; i <= maxpage; i++) {
            if (i > 1) {
                br.getPage(contenturl + "?page=" + i);
            }
            /* Small ugly hack */
            br.getRequest().setHtmlCode(br.getRequest().getHtmlCode().replace("pro.imdb.com", "www.imdb.com"));
            final int dupesSizeOld = dupes.size();
            if (contenturl.matches(TYPE_VIDEO_GALLERY)) {
                final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
                for (final String url : urls) {
                    if (!new Regex(url, ImDbCom.TYPE_VIDEO).patternFind()) {
                        continue;
                    } else if (!dupes.add(url)) {
                        continue;
                    }
                    final DownloadLink dl = createDownloadlink(url);
                    dl.setAvailable(true);
                    final String fid = ImDbCom.getFID(dl);
                    // TODO: maybe parse video element for better filename
                    dl.setName(fid + "_" + fpName + ".mp4");
                    fp.add(dl);
                    ret.add(dl);
                    distribute(dl);
                }
            } else {
                final String[][] links = br.getRegex("(/[^<>\"]+mediaviewer/rm\\d+)([^<>\"/]+)?\"([\t\n\r ]*?title=\"([^<>\"]*?)\")?").getMatches();
                if (links == null || links.length == 0) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                for (final String linkinfo[] : links) {
                    final String url = br.getURL(linkinfo[0]).toExternalForm();
                    if (!dupes.add(url)) {
                        continue;
                    }
                    final DownloadLink dl = createDownloadlink(url);
                    final String id = new Regex(url, "mediaviewer/[a-z]{2}(\\d+)").getMatch(0);
                    final String subtitle = linkinfo[3];
                    if (subtitle != null) {
                        dl.setName(fpName + "_" + id + "_" + Encoding.htmlDecode(subtitle.trim()) + ".jpg");
                    } else {
                        dl.setName(fpName + "_" + id + "_" + ".jpg");
                    }
                    dl.setAvailable(true);
                    dl._setFilePackage(fp);
                    distribute(dl);
                    ret.add(dl);
                }
            }
            int numberofNewItemsThisPage = dupes.size() - dupesSizeOld;
            logger.info("Crawled page " + i + "/" + maxpage + " | New items this page: " + numberofNewItemsThisPage + " | Total: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Decryption aborted by user: " + contenturl);
                return ret;
            } else if (numberofNewItemsThisPage == 0) {
                logger.info("Stopping because: Failed to find any new items on page" + i);
                break pagination;
            }
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }
}
