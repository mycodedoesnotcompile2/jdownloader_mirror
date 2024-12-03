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
import jd.http.Browser;
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

@DecrypterPlugin(revision = "$Revision: 50275 $", interfaceVersion = 2, names = { "imdb.com" }, urls = { "https?://(?:www\\.)?imdb\\.com/((?:name|title)/(?:nm|tt)\\d+((?:/mediaindex|videogallery)|/media/index/rg\\d+)?)" })
public class ImdbComCrawler extends PluginForDecrypt {
    public ImdbComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
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
        } else if (br.containsHTML("video-total\">\\s*0–0")) {
            /* Video link with zero video results */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        int maxpage = 1;
        final String[] pages = br.getRegex("\\?page=(\\d+)\\&ref_=").getColumn(0);
        if (pages != null && pages.length > 0) {
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
        final String extImage = ".jpg";
        int page = 1;
        pagination: do {
            /* Small ugly hack */
            br.getRequest().setHtmlCode(br.getRequest().getHtmlCode().replace("pro.imdb.com", "www.imdb.com"));
            final int dupesSizeOld = dupes.size();
            final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
            if (contenturl.matches(TYPE_VIDEO_GALLERY)) {
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
                for (final String url : urls) {
                    final Regex urlinfo = new Regex(url, ImDbCom.TYPE_PHOTO);
                    if (!urlinfo.patternFind()) {
                        continue;
                    } else if (!dupes.add(url)) {
                        continue;
                    }
                    final DownloadLink dl = createDownloadlink(url);
                    final String id = urlinfo.getMatch(1);
                    final String subtitle = null;
                    if (subtitle != null) {
                        dl.setName(fpName + "_" + id + "_" + Encoding.htmlDecode(subtitle.trim()) + extImage);
                    } else {
                        dl.setName(fpName + "_" + id + "_" + extImage);
                    }
                    dl.setAvailable(true);
                    dl._setFilePackage(fp);
                    distribute(dl);
                    ret.add(dl);
                }
            }
            final int numberofNewItemsThisPage = dupes.size() - dupesSizeOld;
            logger.info("Crawled page " + page + "/" + maxpage + " | New items this page: " + numberofNewItemsThisPage + " | Total: " + ret.size());
            if (this.isAbort()) {
                logger.info("Stopping because: Decryption aborted by user: " + contenturl);
                return ret;
            } else if (page >= maxpage) {
                logger.info("Stopping because: Reached end");
                break pagination;
            } else if (numberofNewItemsThisPage == 0) {
                logger.info("Stopping because: Failed to find any new items on page" + page);
                break pagination;
            } else {
                /* Continue to next page */
                page++;
                br.getPage(contenturl + "?page=" + page);
            }
        } while (!this.isAbort());
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }
}
