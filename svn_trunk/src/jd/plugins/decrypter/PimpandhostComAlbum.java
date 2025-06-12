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
import java.util.List;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.PimpandhostCom;

@DecrypterPlugin(revision = "$Revision: 51139 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { PimpandhostCom.class })
public class PimpandhostComAlbum extends PluginForDecrypt {
    public PimpandhostComAlbum(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        return PimpandhostCom.getPluginDomains();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/album/(\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final int max_items_per_page = 100; // Website: 16
        int page = 1;
        final HashSet<String> dupes = new HashSet<String>();
        FilePackage fp = null;
        pagination: do {
            br.getPage(contenturl + "?page=" + page + "&per-page=" + max_items_per_page);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            } else if (br.containsHTML(">\\s*Album not found")) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            int numberofNewItemsThisPage = 0;
            final String album_id = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
            final String[] filenames = br.getRegex("data-filename=\"([^\"]+)").getColumn(0);
            /* Filesizes in bytes */
            final String[] filesizes = br.getRegex("data-filename=[^>]*data-size=\"(\\d+)").getColumn(0);
            final String[] links = br.getRegex("(/image/\\d+)\"").getColumn(0);
            if (links == null || links.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final List<String> urlsWithoutDupes = new ArrayList<String>();
            for (final String url : links) {
                if (!dupes.add(url)) {
                    continue;
                }
                urlsWithoutDupes.add(url);
                dupes.add(url);
                numberofNewItemsThisPage++;
            }
            if (numberofNewItemsThisPage == 0) {
                logger.info("Stopping because: Failed to find any new items on current page");
                break;
            }
            if (fp == null) {
                String album_title = br.getRegex("album-name\"[^>]*>\\s*<span>([^<]+)</span>").getMatch(0);
                fp = FilePackage.getInstance();
                if (album_title != null) {
                    fp.setName(Encoding.htmlDecode(album_title).trim());
                } else {
                    fp.setName(album_id);
                }
                fp.setPackageKey(getHost() + "://album/" + album_id);
            }
            int index = 0;
            for (String url : urlsWithoutDupes) {
                url = br.getURL(url).toExternalForm();
                final DownloadLink image = createDownloadlink(url);
                if (filenames != null && filenames.length == urlsWithoutDupes.size()) {
                    String filename = filenames[index];
                    filename = Encoding.htmlDecode(filename).trim();
                    image.setName(filename);
                } else {
                    logger.warning("Failed to find filename");
                    /* Fallback */
                    image.setName(url + ".jpg");
                }
                if (filesizes != null && filesizes.length == urlsWithoutDupes.size()) {
                    final String filesizeBytes = filesizes[index];
                    image.setVerifiedFileSize(Long.parseLong(filesizeBytes));
                } else {
                    logger.warning("Failed to find filesize");
                }
                image.setAvailable(true);
                image._setFilePackage(fp);
                ret.add(image);
                distribute(image);
                index++;
            }
            logger.info("Crawled page " + page + " | New items this page: " + numberofNewItemsThisPage + " | Total so far: " + ret.size());
            if (numberofNewItemsThisPage < max_items_per_page) {
                logger.info("Stopping because: Looks like we've reached the end");
                break pagination;
            }
            page++;
        } while (!this.isAbort());
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
