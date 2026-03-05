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

import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.utils.Regex;
import org.appwork.utils.formatter.SizeFormatter;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 52432 $", interfaceVersion = 3, names = {}, urls = {})
public class TurboimagehostCom extends PluginForDecrypt {
    public TurboimagehostCom(PluginWrapper wrapper) {
        super(wrapper);
    }

    // Single image page, e.g. https://www.turboimagehost.com/p/12345/imagename.html
    public static final Pattern PATTERN_IMAGE     = Pattern.compile("/p/(\\d+)/(\\d+[^/]+\\.html)", Pattern.CASE_INSENSITIVE);
    // Thumbnail/direct CDN URL, e.g. https://abc123.turboimg.net/t1/12345_imagename.jpg
    public static final Pattern PATTERN_THUMBNAIL = Pattern.compile("//([a-z0-9\\-]+\\.turboimg\\.net)/t1/(\\d+)_([^/]+(\\.jpe?g|png|gif))", Pattern.CASE_INSENSITIVE);
    // Album page, e.g. https://www.turboimagehost.com/album/67890/albumname/
    public static final Pattern PATTERN_ALBUM     = Pattern.compile("/album/(\\d+)/([^/]+)/?", Pattern.CASE_INSENSITIVE);

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "turboimagehost.com", "turboimagehost.net", "turboimg.net" });
        return ret;
    }

    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("turboimagehost.net"); // 2026-03-04
        return deadDomains;
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
            String pattern = "https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(";
            pattern += PATTERN_IMAGE.pattern().substring(1);
            pattern += "|" + PATTERN_ALBUM.pattern().substring(1);
            pattern += ")";
            // turboimg.net CDN URLs are matched separately as they use a different host
            ret.add(pattern + "|https?://" + PATTERN_THUMBNAIL.pattern().substring(2));
            // ret.add("https?://" + PATTERN_THUMBNAIL.pattern().substring(2)); // strip leading //
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        final String url = param.getCryptedUrl();
        if (new Regex(url, PATTERN_ALBUM).patternFind()) {
            return crawlAlbum(param);
        } else {
            return crawlImage(param);
        }
    }

    private ArrayList<DownloadLink> crawlAlbum(CryptedLink param) throws Exception {
        br.setFollowRedirects(true);
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        checkOffline(br);
        final String gallery_id = new Regex(contenturl, PATTERN_ALBUM).getMatch(0);
        String galleryTitle = br.getRegex("class\\s*=\\s*\"galleryTitle\">\\s*<h1>\\s*(.*?)\\s*</h1>").getMatch(0);
        final FilePackage fp = FilePackage.getInstance();
        if (galleryTitle == null) {
            logger.warning("Failed to find gallery title -> Using gallery_id as fallback");
            galleryTitle = gallery_id;
        }
        fp.setName(galleryTitle);
        fp.setPackageKey("turboimagehost://gallery/" + gallery_id);
        final Set<String> pages = new HashSet<String>();
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        int page = 1;
        pagination: while (!isAbort()) {
            final String urls[] = br.getRegex("(https?://(?:www\\.)?turboimagehost\\.com/p/\\d+/[^/]+\\.html)").getColumn(0);
            for (String url : urls) {
                final DownloadLink image = createDownloadlink(url);
                image._setFilePackage(fp);
                ret.add(image);
                distribute(image);
            }
            logger.info("Crawled page " + page + " | Found items so far: " + ret.size());
            final String nextPage = br.getRegex("label\\s*=\\s*\"Next\"\\s*href\\s*=\\s*\"(\\?p=\\d+)\"").getMatch(0);
            if (nextPage == null) {
                logger.info("Stopping because: Failed to find nextPage");
                break pagination;
            } else if (!pages.add(nextPage)) {
                logger.info("Stopping because: Detected nextPage has already been crawled -> Preventing infinite loop");
                break pagination;
            }
            /* Continue to next page */
            br.getPage(nextPage);
            page++;
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlImage(final CryptedLink param) throws Exception {
        final Regex thumbnail = new Regex(param.getCryptedUrl(), PATTERN_THUMBNAIL);
        final String contenturl;
        if (thumbnail.patternFind()) {
            /* Change thumbnail --> Normal URL --> Then we can crawl the fullsize URL. */
            contenturl = "https://www." + this.getHost() + "/p/" + thumbnail.getMatch(1) + "/" + thumbnail.getMatch(2) + ".html";
        } else {
            contenturl = param.getCryptedUrl();
        }
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        checkOffline(br);
        final String finallink = this.br.getRegex("\"(https?://s\\d+d\\d+\\.[^/]+/sp/[a-z0-9]+/.*?)\"").getMatch(0);
        if (finallink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final DownloadLink link = createDownloadlink(DirectHTTP.createURLForThisPlugin(finallink));
        String filename = getFileNameFromURL(new URL(finallink));
        if (filename != null) {
            filename = filename.replaceFirst("(?i)\\.html?$", "");
            link.setName(filename);
        } else {
            logger.warning("Failed to find filename");
        }
        final String filesizeStr = br.getRegex("File size: (\\d+[^<]+)</p>").getMatch(0);
        if (filesizeStr != null) {
            link.setDownloadSize(SizeFormatter.getSize(filesizeStr));
        } else {
            logger.warning("Failed to find filesize");
        }
        link.setAvailable(true);
        link.setContentUrl(contenturl);
        ret.add(link);
        return ret;
    }

    private void checkOffline(final Browser br) throws PluginException {
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getURL().matches("^/?$")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }
}
