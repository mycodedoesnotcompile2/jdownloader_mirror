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
import java.util.regex.Pattern;

import org.appwork.utils.formatter.SizeFormatter;

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
import jd.plugins.hoster.ImagepondNet;

@DecrypterPlugin(revision = "$Revision: 52663 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { ImagepondNet.class })
public class ImagepondNetGallery extends PluginForDecrypt {
    public ImagepondNetGallery(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        return ImagepondNet.getPluginDomains();
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

    private static final Pattern PATTERN_SUPPORTED = Pattern.compile("/(?:album|a)/([\\w\\-\\.]+)");

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_SUPPORTED.pattern());
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        String album_id = new Regex(contenturl, PATTERN_SUPPORTED).getMatch(0);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String album_id_from_browser_url = new Regex(br.getURL(), PATTERN_SUPPORTED).getMatch(0);
        if (album_id_from_browser_url != null) {
            /* Prefer this id, especially useful when redirect from legacy "/album/..." links to new "/a/..." links happens. */
            album_id = album_id_from_browser_url;
        }
        final Regex galleryinfo = br.getRegex("<span>(\\d+) items\\s*</span>\\s*<span>(\\d+[^<]+)</span>");
        int numberofItems = -1;
        String galleryFilesizeStr = null;
        long estimated_filesize_per_image = -1;
        if (galleryinfo.patternFind()) {
            numberofItems = Integer.parseInt(galleryinfo.getMatch(0));
            galleryFilesizeStr = galleryinfo.getMatch(1);
            estimated_filesize_per_image = SizeFormatter.getSize(galleryFilesizeStr) / numberofItems;
        }
        String title = br.getRegex("<h1 class=\"text-2xl font-bold mb-2\"[^>]*>([^<]+)</h1>").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
        }
        String description = br.getRegex("<p class=\"text-gray-400 mb-3\"[^>]*>([^<]+)</p>").getMatch(0);
        if (description != null) {
            description = Encoding.htmlDecode(description).trim();
        }
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(title);
        } else {
            logger.warning("Failed to find gallery title");
            fp.setName(album_id);
        }
        /* Description is optional */
        if (description != null) {
            fp.setComment(description);
        }
        fp.setPackageKey("imagepond://album/" + album_id);
        final HashSet<String> dupes = new HashSet<String>();
        int page = 1;
        pagination: do {
            int newItemsThisPage = 0;
            final List<String> file_ids_deduped = new ArrayList<String>();
            final String[] filenames = br.getRegex("<p class=\"mt-1.5 text-xs text-gray-400 truncate\"[^>]*>([^<]+)</p>").getColumn(0);
            final String[] file_ids = br.getRegex("/i/([\\w\\-\\.]+)").getColumn(0);
            if (file_ids == null || file_ids.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (final String file_id : file_ids) {
                if (!dupes.add(file_id)) {
                    continue;
                }
                file_ids_deduped.add(file_id);
                newItemsThisPage++;
            }
            int i = 0;
            for (final String file_id : file_ids_deduped) {
                final String url = "https://www.imagepond.net/i/" + file_id;
                final DownloadLink image = this.createDownloadlink(url);
                if (filenames != null && filenames.length == file_ids_deduped.size()) {
                    String filename = filenames[i];
                    filename = Encoding.htmlDecode(filename).trim();
                    image.setName(filename);
                } else {
                    if (i == 0) {
                        /* Log this only once (per page) */
                        logger.warning("Failed to find filenames");
                    }
                }
                if (estimated_filesize_per_image != -1) {
                    image.setDownloadSize(estimated_filesize_per_image);
                }
                image.setAvailable(true);
                image._setFilePackage(fp);
                ret.add(image);
                distribute(image);
                i++;
            }
            logger.info("Crawled page " + page + " | Items so far: " + ret.size() + "/" + numberofItems);
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                throw new InterruptedException();
            } else if (ret.size() == numberofItems) {
                logger.info("Stopping because: Found all items");
                break pagination;
            } else if (newItemsThisPage == 0) {
                /* Fail safe */
                logger.info("Stopping because: Reached last page?");
                break pagination;
            }
            /* Continue to next page */
            page++;
            final String nextPageURL = br.getRegex("/a/" + Pattern.quote(album_id) + "\\?page=" + page).getMatch(-1);
            if (nextPageURL == null) {
                logger.info("Stopping because: Failed to find next page -> Reached end?");
                break pagination;
            }
            br.getPage(nextPageURL);
        } while (!this.isAbort());
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
