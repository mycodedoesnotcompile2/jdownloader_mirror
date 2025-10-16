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

import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Browser.REFERRER_POLICY;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;
import jd.plugins.hoster.GenericM3u8;

@DecrypterPlugin(revision = "$Revision: 51676 $", interfaceVersion = 3, names = {}, urls = {})
public class VidsrcCrawler extends PluginForDecrypt {
    public VidsrcCrawler(PluginWrapper wrapper) {
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
        ret.add(new String[] { "vidsrc.net", "vidsrcme.ru", "vidsrcme.su", "vsrc.su", "vidsrc-embed.ru", "vidsrc-embed.su", "vsdash.net" });
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

    /** Possible types of links: See examples in API docs: https://vidsrc-embed.ru/api/ */
    private static final Pattern TYPE_MOVIE   = Pattern.compile("/embed/movie/((tt)?\\d+).*", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_MOVIE_2 = Pattern.compile("/embed/movie\\?(imdb|tmdb)=.+", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_TV      = Pattern.compile("/embed/tv/((tt)?\\d+)(/(\\d+)-(\\d+))?.*", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_TV_2    = Pattern.compile("/embed/tv\\?(imdb|tmdb)=.+", Pattern.CASE_INSENSITIVE);
    // private static final Pattern TYPE_MOVIE = Pattern.compile("/embed/movie/(tt\\d+)", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_MOVIE.pattern() + "|" + TYPE_MOVIE_2.pattern() + "|" + TYPE_TV.pattern() + "|" + TYPE_TV_2.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final UrlQuery query = UrlQuery.parse(br.getURL());
        String episode = query.get("episode");
        if (episode == null) {
            /* Try to obtain episode-number from url-path */
            episode = new Regex(contenturl, TYPE_TV).getMatch(4);
        }
        if (episode != null || new Regex(contenturl, TYPE_MOVIE).patternFind() || new Regex(contenturl, TYPE_MOVIE_2).patternFind()) {
            /* We expect a single video stream -> Crawl it */
            final String next_url = br.getRegex("id=\"player_iframe\" src=\"([^\"]+)").getMatch(0);
            if (next_url == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            br.getPage(next_url);
            final String next_url_2 = br.getRegex("src:\\s*'(/prorcp/[^']+)").getMatch(0);
            if (next_url_2 == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            br.getPage(next_url_2);
            String title = br.getRegex("removeExtension\\(atob\\('([^']+)").getMatch(0);
            if (title != null) {
                title = Encoding.Base64Decode(title).trim();
            }
            /* TODO: Add subtitle crawler? */
            final String hls_master = br.getRegex("file\\s*:\\s*'(https?://[^']+)").getMatch(0);
            if (hls_master != null) {
                final DownloadLink video = this.createDownloadlink(hls_master);
                video.setReferrerUrl(REFERRER_POLICY.getOrigin(br._getURL()));
                if (title != null) {
                    video.setProperty(GenericM3u8.PRESET_NAME_PROPERTY, title);
                }
                ret.add(video);
            } else {
                logger.warning("Failed to find video stream link");
            }
            String url_poster = br.getRegex("poster:\"([^\"]+)").getMatch(0);
            if (url_poster != null) {
                url_poster = br.getURL(url_poster).toExternalForm();
                final DownloadLink image = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url_poster));
                image.setAvailable(true);
                ret.add(image);
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(title);
            fp.addLinks(ret);
        } else {
            /* Collect links to other episodes which will be crawled separately */
            final String[] urls_episodes = br.getRegex("/embed/tv\\?imdb=tt\\d+&season=\\d+&episode=\\d+").getColumn(-1);
            for (String url_episode : urls_episodes) {
                url_episode = br.getURL(url_episode).toExternalForm();
                final DownloadLink link = this.createDownloadlink(url_episode);
                ret.add(link);
            }
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2025-10-15: Try to avoid Cloudflare blocks */
        return 1;
    }
}
