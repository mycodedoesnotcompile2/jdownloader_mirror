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
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;

@DecrypterPlugin(revision = "$Revision: 51291 $", interfaceVersion = 3, names = {}, urls = {})
public class ThreeCatCat extends PluginForDecrypt {
    public ThreeCatCat(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "3cat.cat" });
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

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/.+/(audio|video)/(\\d+)/");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        final Regex urlinfo = new Regex(contenturl, this.getSupportedLinks());
        final String mediaType = urlinfo.getMatch(0);
        final String mediaID = urlinfo.getMatch(1);
        final boolean isAudio = mediaType.equalsIgnoreCase("audio");
        /**
         * Alternative API Requests: <br>
         * https://api-media.3cat.cat/pvideo/media.jsp?media=video&versio=vast&idint=<videoID>&profile=pc_3cat&format=dm <br>
         * and: <br>
         * https://api.ccma.cat/pvideo/media.jsp?media=video&version=0s&idint=<videoID>
         */
        br.getPage("https://dinamics.ccma.cat/pvideo/media.jsp?media=" + mediaType + "&version=0s&idint=" + mediaID);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /**
         * Important! Some items may not have video streams available anymore but thumbnail and/or subtitle(s) are still available. <br>
         * Example thumbnail only: /3cat/el-prego/video/5702805/ <br>
         * Example thumbnail + subtitles: /3cat/casa-en-flames/video/6346812/
         */
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> info = (Map<String, Object>) entries.get("informacio");
        final Map<String, Object> images = (Map<String, Object>) entries.get("imatges");
        final List<Map<String, Object>> subtitles = (List<Map<String, Object>>) entries.get("subtitols");
        final Map<String, Object> media = (Map<String, Object>) entries.get("media");
        String title = (String) info.get("titol_complet");
        if (StringUtils.isEmpty(title)) {
            title = info.get("titol").toString();
        }
        final String description = (String) info.get("descripcio");
        /* Add video qualities */
        String lastMediaTitle = null;
        List<Map<String, Object>> video_qualities = null;
        if (media != null) {
            if (isAudio) {
                /* Audio, typically mp3 file */
                final String audio_url = media.get("url").toString();
                final DownloadLink audio = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(audio_url));
                audio.setFinalFileName(title + "." + media.get("format").toString().toLowerCase());
                ret.add(audio);
            } else {
                /* Video */
                video_qualities = (List<Map<String, Object>>) media.get("url");
                for (final Map<String, Object> quality : video_qualities) {
                    final String url = quality.get("file").toString();
                    final String label = quality.get("label").toString();
                    /* Skip unsupported streaming formats */
                    if (label.equalsIgnoreCase("DASH")) {
                        continue;
                    }
                    final DownloadLink video = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
                    final String title_video = title + "_" + label;
                    final String filename = title_video + ".mp4";
                    video.setFinalFileName(filename);
                    ret.add(video);
                    lastMediaTitle = title_video;
                }
                if (ret.isEmpty()) {
                    logger.warning("This item contained only unsupported streaming formats");
                }
            }
        }
        /* Add subtitles */
        if (subtitles != null && !subtitles.isEmpty()) {
            for (final Map<String, Object> subtitleinfo : subtitles) {
                final String url = subtitleinfo.get("url").toString();
                final String ext = subtitleinfo.get("format").toString();
                final DownloadLink subtitle = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
                if (video_qualities != null && video_qualities.size() == 1) {
                    /* If we have only a single quality, make subtitle filename same as video filename. */
                    subtitle.setFinalFileName(lastMediaTitle + "." + subtitleinfo.get("iso") + "." + ext);
                } else {
                    subtitle.setFinalFileName(title + "." + ext);
                }
                ret.add(subtitle);
            }
        }
        /* Add thumbnail */
        final String thumbnail_url = images.get("url").toString();
        final DownloadLink thumbnail = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(thumbnail_url));
        /* If we have only a single quality, make thumbnail filename same as video filename. */
        final String thumbnail_url_ext = Plugin.getFileNameExtensionFromURL(thumbnail_url, ".jpg");
        if (video_qualities != null && video_qualities.size() == 1) {
            thumbnail.setFinalFileName(lastMediaTitle + thumbnail_url_ext);
        } else {
            thumbnail.setFinalFileName(title + thumbnail_url_ext);
        }
        ret.add(thumbnail);
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        if (!StringUtils.isEmpty(description)) {
            fp.setComment(description);
        }
        fp.setPackageKey(this.getHost() + ":/" + mediaType + "/" + mediaID);
        for (final DownloadLink result : ret) {
            result._setFilePackage(fp);
            result.setAvailable(true);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }
}
