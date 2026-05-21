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
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52820 $", interfaceVersion = 2, names = {}, urls = {})
public class ImageVenueComGallery extends PluginForDecrypt {
    public ImageVenueComGallery(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "imagevenue.com" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_GALLERY = Pattern.compile("/(GA[A-Za-z0-9]+)", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + PATTERN_GALLERY.pattern());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY };
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String contenturl = param.getCryptedUrl().replaceFirst("(?i)http://", "https://");
        final String gallery_id = new Regex(contenturl, PATTERN_GALLERY).getMatch(0);
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 403 || br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("<title>\\s*(?:ImageVenue.com\\s*-)?\\s*(.*?)\\s*</title>").getMatch(0);
        if (title == null) {
            title = new Regex(contenturl, "gal=gallery_(.+)").getMatch(0);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            fp.setName(title);
        } else {
            /* Fallback */
            fp.setName(gallery_id);
        }
        fp.setPackageKey("imagevenue://gallery/" + gallery_id);
        final HashSet<String> dupes = new HashSet<String>();
        int page = 1;
        pagination: do {
            /* Current gallery format: parse duids, filenames and filesizes in one pass */
            final String[] duids = br.getRegex("data-duid=\"([A-Za-z0-9]+)\"").getColumn(0);
            if (duids == null || duids.length == 0) {
                logger.info("Stopping because: No links found on page " + page);
                break pagination;
            }
            final String[] filenames = br.getRegex("class=\"filename[^\"]*\"[^>]*title=\"([^\"]+)\"").getColumn(0);
            final String[] filesizes = br.getRegex("<span class=\"size\">([^<]+)</span>").getColumn(0);
            final String[] extensions = br.getRegex("<span class=\"extension\">([^<]+)</span>").getColumn(0);
            final boolean hasFilenames = filenames != null && filenames.length == duids.length;
            final boolean hasFilesizes = filesizes != null && filesizes.length == duids.length;
            final boolean hasExtensions = extensions != null && extensions.length == duids.length;
            final int sizeBefore = ret.size();
            for (int i = 0; i < duids.length; i++) {
                if (!dupes.add(duids[i])) {
                    /* Skip duplicates */
                    continue;
                }
                final DownloadLink link = createDownloadlink("https://www.imagevenue.com/" + duids[i]);
                link.setAvailable(true);
                if (hasFilenames) {
                    link.setName(filenames[i]);
                } else {
                    /* Fallback: use image id + extension */
                    final String ext = hasExtensions ? "." + extensions[i] : ".jpg";
                    link.setName(duids[i] + ext);
                }
                if (hasFilesizes) {
                    link.setDownloadSize(SizeFormatter.getSize(filesizes[i]));
                }
                if (fp != null) {
                    link._setFilePackage(fp);
                }
                ret.add(link);
                distribute(link);
            }
            logger.info("Crawled page " + page + " | Items so far: " + ret.size());
            if (ret.size() == sizeBefore) {
                logger.info("Stopping because: No new items found on page " + page);
                break pagination;
            }
            page++;
            String nextPageUrl = br.getRegex("<a[^>]*class=\"page-link\"[^>]*href=\"([^\"]+\\?page=" + page + ")\"").getMatch(0);
            if (nextPageUrl == null) {
                logger.info("Stopping because: Failed to find next page URL");
                break pagination;
            }
            nextPageUrl = Encoding.htmlDecode(nextPageUrl);
            br.getPage(nextPageUrl);
        } while (!this.isAbort());
        if (ret.size() == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, jd.plugins.Account acc) {
        return false;
    }
}