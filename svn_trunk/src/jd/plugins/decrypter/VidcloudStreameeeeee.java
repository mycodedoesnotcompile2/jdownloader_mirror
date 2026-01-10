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
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;
import jd.plugins.hoster.GenericM3u8;

@DecrypterPlugin(revision = "$Revision: 52075 $", interfaceVersion = 3, names = {}, urls = {})
public class VidcloudStreameeeeee extends PluginForDecrypt {
    public VidcloudStreameeeeee(PluginWrapper wrapper) {
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
        /* Formerly vidcloud.co, vidcloud.ru, vcstream.to */
        // tags: rapid-cloud.co
        ret.add(new String[] { "streameeeeee.site" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/embed-1/v3/e-1/([a-zA-Z0-9]+)\\?z=");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String referer = null;
        String title = null;
        final DownloadLink src = param.getDownloadLink();
        if (src != null) {
            /* Get information set by previous crawler. */
            referer = src.getReferrerUrl();
            title = src.getStringProperty(GenericM3u8.PRESET_NAME_PROPERTY);
        }
        if (referer != null) {
            /* Important: Most or all of their content is referer-protected */
            br.getHeaders().put("Referer", referer);
        } else {
            logger.warning("No referer given -> Crawl will most likely fail!");
        }
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("/file-not-found\\.jpg")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String key = br.getRegex("(\"|')([a-zA-Z0-9]{48})(\"|')").getMatch(1);
        if (key == null) {
            key = br.getRegex(":\\s*([a-zA-Z0-9]{48})\\s*-").getMatch(0);
            if (key == null) {
                final String keyV1[] = br.getRegex("\\w\\s*:\\s*\"([a-zA-Z0-9]{16})").getColumn(0);
                if (keyV1 != null) {
                    key = StringUtils.join(keyV1, "");
                }
            }
        }
        if (key == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String content_id = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        br.getPage("/embed-1/v3/e-1/getSources?id=" + content_id + "&_k=" + key);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final List<Map<String, Object>> sources = (List<Map<String, Object>>) entries.get("sources");
        final List<Map<String, Object>> subtitles = (List<Map<String, Object>>) entries.get("tracks");
        final boolean is_encrypted = ((Boolean) entries.get("encrypted")).booleanValue();
        if (is_encrypted) {
            logger.warning("Content is encrypted -> Ignoring video content and returning subtitles only, if subtitles exist");
            /* Continue and possible crawl subtitle */
            if (subtitles.isEmpty()) {
                /* Video is encrypted and no subtitles there for us to crawl. */
                throw new DecrypterRetryException(RetryReason.UNSUPPORTED_DRM);
            }
        } else {
            if (sources == null || sources.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (final Map<String, Object> source : sources) {
                final String url = source.get("file").toString();
                final DownloadLink video = this.createDownloadlink(url);
                if (title != null) {
                    video.setProperty(GenericM3u8.PRESET_NAME_PROPERTY, title);
                }
                /* Without correct referer, mirror "UpCloud" which has internal server_id "29"will fail with Cloudflare error. */
                video.setReferrerUrl("https://streameeeeee.site/");
                ret.add(video);
            }
        }
        for (final Map<String, Object> subtitle : subtitles) {
            final String kind = subtitle.get("kind").toString();
            if (!"captions".equalsIgnoreCase(kind)) {
                logger.info("Skipping unsupported subtitle type: " + kind);
                continue;
            }
            final String url = subtitle.get("file").toString();
            final DownloadLink sub = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
            /* Skip linkcheck */
            sub.setAvailable(true);
            ret.add(sub);
        }
        final FilePackage fp = FilePackage.getInstance();
        if (title != null) {
            fp.setName(title);
        }
        if (referer != null) {
            /* Allow for auto merge with other mirrors */
            fp.setPackageKey(referer);
        } else {
            fp.setPackageKey(this.getHost() + "://" + content_id);
        }
        fp.addLinks(ret);
        return ret;
    }
}
