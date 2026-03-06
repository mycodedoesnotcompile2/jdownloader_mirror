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

import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;
import jd.plugins.hoster.GenericM3u8;

@DecrypterPlugin(revision = "$Revision: 52444 $", interfaceVersion = 3, names = {}, urls = {})
public class FkbaeTo extends PluginForDecrypt {
    public FkbaeTo(PluginWrapper wrapper) {
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
        ret.add(new String[] { "fkbae.to" });
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

    private static final Pattern PATTERN_NORMAL = Pattern.compile("/(\\d+)/");
    /* snstrhls = HLS, XX = Progressive */
    private static final Pattern PATTERN_EMBED  = Pattern.compile("/(?:snstr|snstrhls)\\.php\\?fileid=([a-zA-Z0-9]+)");

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            String pattern = "https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(";
            pattern += PATTERN_NORMAL.pattern().substring(1);
            pattern += "|" + PATTERN_EMBED.pattern().substring(1);
            pattern += ")";
            ret.add(pattern);
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        String title = null;
        final String self_embed_url;
        if (new Regex(contenturl, PATTERN_EMBED).patternFind()) {
            /* We already got the embed url we need though in this case we cannot get a meaningful title. */
            self_embed_url = contenturl;
        } else {
            /* We need to find the embed url */
            br.getPage(contenturl);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            self_embed_url = new Regex(br.toString(), PATTERN_EMBED).getMatch(-1);
            if (self_embed_url == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            title = br.getRegex("class=\"fl-heading-text\"[^>]*>([^<]+)<").getMatch(0);
            if (title != null) {
                title = Encoding.htmlDecode(title).trim();
            } else {
                logger.warning("Failed to find title");
            }
        }
        br.getPage(self_embed_url);
        final String stream_url = br.getRegex("file:\\s*\"(https?://[^\"]+)").getMatch(0);
        if (stream_url == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (StringUtils.containsIgnoreCase(stream_url, ".m3u8")) {
            /* HLS video stream */
            final DownloadLink video = createDownloadlink(stream_url);
            if (title != null) {
                video.setProperty(GenericM3u8.PRESET_NAME_PROPERTY, title);
            }
            ret.add(video);
        } else {
            /* Progressive video stream */
            final DownloadLink video = createDownloadlink(DirectHTTP.createURLForThisPlugin(stream_url));
            if (title != null && StringUtils.containsIgnoreCase(stream_url, ".mp4")) {
                video.setFinalFileName(title + ".mp4");
            }
            ret.add(video);
        }
        return ret;
    }
}
