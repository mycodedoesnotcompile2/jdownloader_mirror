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

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 50080 $", interfaceVersion = 3, names = {}, urls = {})
public class RedditComCrawlerRedditMedia extends PluginForDecrypt {
    public RedditComCrawlerRedditMedia(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "redditmedia.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/mediaembed/([a-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.setFollowRedirects(true);
        final String mediaID = new Regex(param.getCryptedUrl(), this.getSupportedLinks()).getMatch(0);
        /* Important: Needs "www."! */
        br.getPage("https://www." + this.getHost() + "/mediaembed/" + mediaID);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!br.getURL().contains(mediaID)) {
            /* E.g. redirect to mainpage */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Sometimes protocol in URL is missing e.g. https://www.redditmedia.com/mediaembed/7q164i */
        String finallink = br.getRegex("<iframe[^>]*src=\"((https?:)?//[^\"]+)").getMatch(0);
        if (finallink == null) {
            /* 2024-11-06: Reddit selfhosted video content e.g. https://www.redditmedia.com/mediaembed/xmz9o3 */
            finallink = br.getRegex("data-seek-preview-url=\"([^\"]+)").getMatch(0);
            if (finallink == null) {
                /* 2024-11-06 e.g. https://www.redditmedia.com/mediaembed/xmz9o3 */
                finallink = br.getRegex("data-hls-url=\"([^\"]+)").getMatch(0);
            }
        }
        if (finallink != null) {
            /* Single result */
            /* Change potential relative URL to absolute URL. */
            finallink = br.getURL(finallink).toExternalForm();
            ret.add(createDownloadlink(finallink));
        } else {
            /*
             * Fallback for misc externally hosted content -> Add all http URLs e.g. twitter items e.g.
             * https://www.redditmedia.com/mediaembed/1dzak7a
             */
            final String[] urls = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
            if (urls == null || urls.length == 0) {
                /* No links at all -> Item must be offline */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            for (final String url : urls) {
                ret.add(createDownloadlink(url));
            }
        }
        return ret;
    }
}
