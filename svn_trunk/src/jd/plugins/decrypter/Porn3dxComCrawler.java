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
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.MediadeliveryNet;

@DecrypterPlugin(revision = "$Revision: 53020 $", interfaceVersion = 3, names = {}, urls = {})
public class Porn3dxComCrawler extends PluginForDecrypt {
    public Porn3dxComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "porn3dx.com" });
        return ret;
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

    private static final Pattern PATTERN_POST = Pattern.compile("/post/(\\d+)(/([a-z0-9\\-_]+))?", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:[a-z0-9]+\\.)?" + buildHostsPatternPart(domains) + PATTERN_POST.pattern());
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String postID = new Regex(contenturl, PATTERN_POST).getMatch(0);
        final String titleSlug = br.getRegex("/post/" + postID + "/([a-z0-9\\-_]+)").getMatch(0);
        String authorSlug = br.getRegex("href=\"https?://[^/]+/tag/([^\"]+)\" class=\"tag-artist\"[^>]*>").getMatch(0);
        if (authorSlug == null) {
            /* 2022-02-08 */
            authorSlug = br.getRegex("class=\"avatar-link\" href=\"https?://[^/]+/([^/\"]+)\"").getMatch(0);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (authorSlug != null && titleSlug != null) {
            fp.setName(authorSlug + " - " + titleSlug);
        } else if (titleSlug != null) {
            fp.setName(titleSlug);
        } else {
            /* Fallback */
            fp.setName(postID);
        }
        final String[] newEmbedURLs = br.getRegex("(https?://iframe\\.mediadelivery\\.net/embed/[^\"]+)\"").getColumn(0);
        if (newEmbedURLs != null && newEmbedURLs.length > 0) {
            /* 2022-02-08 */
            int index = 0;
            for (final String newEmbedURL : newEmbedURLs) {
                final DownloadLink link = this.createDownloadlink(newEmbedURL);
                link.setReferrerUrl(br.getURL());
                link.setProperty(MediadeliveryNet.PROPERTY_PORN3DX_POST_ID, postID);
                if (authorSlug != null) {
                    link.setProperty(MediadeliveryNet.PROPERTY_AUTHOR, authorSlug);
                }
                if (titleSlug != null) {
                    link.setProperty(MediadeliveryNet.PROPERTY_TITLE, titleSlug);
                }
                link.setProperty(MediadeliveryNet.PROPERTY_POSITION, (index + 1));
                MediadeliveryNet.setFilename(link);
                link.setAvailable(true);
                link._setFilePackage(fp);
                ret.add(link);
                index++;
            }
            return ret;
        }
        final String embedURL = br.getRegex("(https?://[^/]+/videos/embed/[a-f0-9\\-]+)").getMatch(0);
        if (embedURL != null) {
            /* Self-embedded video hosted most likely on tube.porn3dx.com (peertube instance). */
            ret.add(createDownloadlink(embedURL));
        } else {
            final String filenameBase;
            if (authorSlug != null && titleSlug != null) {
                filenameBase = authorSlug + "_" + postID + "_" + titleSlug;
            } else if (titleSlug != null) {
                filenameBase = postID + "_" + titleSlug;
            } else {
                filenameBase = postID;
            }
            final String imagesJson = br.getRegex("\"image\":\\s*(\\[[^\\]]+\\])").getMatch(0);
            final String[] imageURLs = br.getRegex("data-full-url=\"(https?://[^/]+/post/\\d+/[a-f0-9]+\\.jpg)\"").getColumn(0);
            /* 2026-06-17: extract contentUrl values from hasPart ImageObjects in ld+json */
            final String ldJson = br.getRegex("<script type=\"application/ld\\+json\">\\s*(\\{[\\s\\S]*?\\})\\s*</script>").getMatch(0);
            final String[] contentURLs = ldJson != null ? new Regex(ldJson, "\"contentUrl\":\"(https?://[^\"]+)\"").getColumn(0) : new String[0];
            int imagecounter = 1;
            if (imageURLs != null && imageURLs.length > 0) {
                /* Old */
                final int padLength = StringUtils.getPadLength(imageURLs.length);
                for (final String imageURL : imageURLs) {
                    final DownloadLink image = this.createDownloadlink(imageURL);
                    image.setFinalFileName(filenameBase + "_" + StringUtils.formatByPadLength(padLength, imagecounter) + ".jpg");
                    image.setAvailable(true);
                    image._setFilePackage(fp);
                    ret.add(image);
                    imagecounter++;
                }
            } else if (imagesJson != null) {
                /* New 2023-10-30 */
                final List<String> imageurls = restoreFromString(imagesJson, TypeRef.STRING_LIST);
                final int padLength = StringUtils.getPadLength(imageurls.size());
                for (final String imageurl : imageurls) {
                    final DownloadLink image = this.createDownloadlink(imageurl);
                    image.setFinalFileName(filenameBase + "_" + StringUtils.formatByPadLength(padLength, imagecounter) + ".jpg");
                    image.setAvailable(true);
                    image._setFilePackage(fp);
                    ret.add(image);
                    imagecounter++;
                }
            } else if (contentURLs.length > 0) {
                /* New 2026-06-17: hasPart ImageObject array in ld+json */
                final int padLength = StringUtils.getPadLength(contentURLs.length);
                for (final String contentURL : contentURLs) {
                    final String ext = Plugin.getFileNameExtensionFromString(contentURL, ".jpg");
                    final DownloadLink image = this.createDownloadlink(contentURL);
                    image.setFinalFileName(filenameBase + "_" + StringUtils.formatByPadLength(padLength, imagecounter) + ext);
                    image.setAvailable(true);
                    image._setFilePackage(fp);
                    ret.add(image);
                    imagecounter++;
                }
            }
            final String imageURLLarge = br.getRegex("(https?://media\\.[^/]+/post/\\d+/large\\.[a-z]+)").getMatch(0);
            if (imagecounter == 1 && imageURLLarge != null) {
                /* No images from image gallery found -> Add single image if possible */
                final DownloadLink image = this.createDownloadlink(imageURLLarge);
                image.setFinalFileName(filenameBase + Plugin.getFileNameExtensionFromString(imageURLLarge));
                image.setAvailable(true);
                image._setFilePackage(fp);
                ret.add(image);
            }
        }
        if (ret.isEmpty()) {
            if (br.containsHTML("<title>Porn3dx</title>")) {
                /* Empty page without error message e.g. /post/44652/widowmaker-playboy-cover */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }
}
