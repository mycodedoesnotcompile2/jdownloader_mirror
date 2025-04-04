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
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.components.PluginJSonUtils;

@DecrypterPlugin(revision = "$Revision: 50919 $", interfaceVersion = 2, names = {}, urls = {})
public class FrancetelevisionsFrCrawler extends PluginForDecrypt {
    /**
     * Returns the annotations names array
     *
     * @return
     */
    public static String[] getAnnotationNames() {
        return new String[] { "france.tv", "franceo.fr", "francetvinfo.fr", "francetv.fr", "francetvsport.fr", "ludo.fr", "zouzous.fr" };
    }

    /**
     * returns the annotation pattern array
     *
     * @return
     */
    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String domain : getAnnotationNames()) {
            ret.add("https?://(?:www\\.)?" + Pattern.quote(domain) + "/.+\\.html");
        }
        return ret.toArray(new String[0]);
    }

    public FrancetelevisionsFrCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public ArrayList<DownloadLink> decryptIt(CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String[] embedurls = br.getRegex("<iframe[^<]*data-src=\"([^\"]+)").getColumn(0);
        if (embedurls != null && embedurls.length > 0) {
            /**
             * E.g. dailymotion.com embed: <br>
             * https://www.francetvinfo.fr/politique/notre-dame-des-landes/video-sur-france-inter-cecile-duflot-denonce-le-regard-meprisant-de-patrick-cohen_1520091.html
             */
            for (String url : embedurls) {
                url = br.getURL(url).toExternalForm();
                ret.add(this.createDownloadlink(url));
            }
            return ret;
        }
        final String unescaped = PluginJSonUtils.unescape(br.getRequest().getHtmlCode());
        final String[] videoIDs = new Regex(unescaped, "\"(?:video_factory_id|playerReplayId)\":\"([a-f0-9-]+)\"").getColumn(0);
        if (videoIDs == null || videoIDs.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final HashSet<String> videoIDs_unique = new HashSet<String>(Arrays.asList(videoIDs));
        // for(final String videoID:videoIDs_unique) {
        //
        // }
        if (videoIDs_unique.size() > 1) {
            logger.info("Found " + videoIDs_unique.size() + " video items in total but crawling only the first one because this crawler is broken either way due to DRM protected content");
        }
        // final String video_id = videoIDs_unique.iterator().next();
        final String video_id = videoIDs[0];
        final Browser brc = br.cloneBrowser();
        brc.getHeaders().put("Origin", "https://www." + getHost());
        brc.getHeaders().put("Referer", contenturl);
        // final String[] browsers = new String[] { "chrome", "safari" };
        // final String[] device_types = new String[] { "desktop", "mobile" };
        /* Request and error handling: Thx to: https://github.com/yt-dlp/yt-dlp/blob/master/yt_dlp/extractor/francetv.py#L103 */
        /**
         * Parameter country_code: <br>
         * Anything except "FR" -> Instant GEO-block <br>
         * We can just supply "FR" here but it we were to download the video streams, we would still be GEO-blocked outside france according
         * to yt-dlp code.
         */
        brc.getPage("https://k7.ftven.fr/videos/" + video_id + "?country_code=FR&domain=www.france.tv&device_type=mobile&browser=safari&video_product_id=7051124&capabilities=drm2");
        if (brc.getHttpConnection().getResponseCode() == 422) {
            throw new DecrypterRetryException(RetryReason.GEO);
        }
        final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final Number codeO = (Number) entries.get("code");
        if (codeO != null) {
            final int code = codeO.intValue();
            if (code == 2009) {
                throw new DecrypterRetryException(RetryReason.GEO);
            } else if (code == 2015 || code == 2017) {
                /* DRM, TODO: Change to RetryReason.UNSUPPORTED_DRM */
                // throw new DecrypterRetryException(RetryReason.UNSUPPORTED_DRM);
                throw new DecrypterRetryException(RetryReason.UNSUPPORTED_LIVESTREAM);
            }
        }
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* 2025-04-03: All HLS streams are AES-128 encrypted */
            /* DRM, TODO: Change to RetryReason.UNSUPPORTED_DRM */
            // throw new DecrypterRetryException(RetryReason.UNSUPPORTED_DRM);
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> video = (Map<String, Object>) entries.get("video");
        final Map<String, Object> meta = (Map<String, Object>) entries.get("meta");
        final String meta_title = meta.get("title").toString();
        final String meta_additional_title = (String) meta.get("additional_title");
        final String meta_broadcasted_at = meta.get("broadcasted_at").toString();
        final String meta_cover_url = meta.get("image_url").toString();
        final String meta_description = (String) meta.get("description");
        if (Boolean.TRUE.equals(video.get("is_live"))) {
            throw new DecrypterRetryException(RetryReason.UNSUPPORTED_LIVESTREAM);
        }
        final String format = video.get("format").toString();
        if (!StringUtils.equalsIgnoreCase(format, "hls")) {
            /* This should never happen */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String hlsmaster = video.get("url").toString();
        /* The request below will return json with the same URL but added authorization parameters. */
        brc.getPage("https://hdfauth.ftven.fr/esi/TA?format=json&url=" + Encoding.urlEncode(hlsmaster));
        br.getPage(hlsmaster);
        final Map<String, Object> authresp = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        hlsmaster = authresp.get("url").toString();
        System.out.print("hlsmaster: " + hlsmaster);
        return ret;
    }
}
