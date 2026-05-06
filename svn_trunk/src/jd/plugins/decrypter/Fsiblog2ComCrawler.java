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
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.utils.Regex;

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

@DecrypterPlugin(revision = "$Revision: 52772 $", interfaceVersion = 3, names = {}, urls = {})
public class Fsiblog2ComCrawler extends PluginForDecrypt {
    public Fsiblog2ComCrawler(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    /** Old domain: freesexyindians.com */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "fsiblogx.com", "fsiblog.club", "fsiblog2.com", "fsiblog3.club" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([a-z0-9\\-]+)/([a-z0-9\\-]+)/?");
        }
        return ret.toArray(new String[0]);
    }

    static final Pattern PATTERN_IGNORE = Pattern.compile("/(?:page|category|tag|wp-admin|wp-content|wp-includes)(?:/|$)", Pattern.CASE_INSENSITIVE);

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl().replaceFirst("(?i)^http://", "https://");
        final String path = new URL(contenturl).getPath();
        if (new Regex(path, PATTERN_IGNORE).patternFind()) {
            logger.info("Ignoring invalid/unsupported url");
            return new ArrayList<DownloadLink>();
        }
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String titleFromURL = new Regex(contenturl, this.getSupportedLinks()).getMatch(1).replace("-", " ").trim();
        final String videoURL = br.getRegex("\"(https?://[^\"]+\\.mp4)").getMatch(0);
        if (videoURL != null) {
            final DownloadLink video = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(videoURL));
            video.setFinalFileName(titleFromURL + ".mp4");
            ret.add(video);
        } else {
            /* Maybe photo content */
            final String[] photos = br.getRegex("class=\"e-gallery-item elementor-gallery-item elementor-animated-content\" href=\"(https?://[^\"]+)\"").getColumn(0);
            if (photos != null && photos.length > 0) {
                for (final String url : photos) {
                    final DownloadLink link = createDownloadlink(DirectHTTP.createURLForThisPlugin(url));
                    ret.add(link);
                }
            }
        }
        if (ret.isEmpty()) {
            if (br.containsHTML("class=\"elementor-post__text\"")) {
                logger.info("Looks like text-content only -> Nothing for us to download");
                return ret;
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        for (final DownloadLink result : ret) {
            result.setAvailable(true);
            /* Direct link cannot be used without correct referer so let's set a contentURL which the user can actually open in browser. */
            result.setContentUrl(contenturl);
            /*
             * Direct-URL cannot be used without correct referer -> Set that here so that DirectHTTP plugin is able to download the file
             * later on.
             */
            result.setReferrerUrl(br.getURL());
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(titleFromURL);
        fp.addLinks(ret);
        return ret;
    }
}
