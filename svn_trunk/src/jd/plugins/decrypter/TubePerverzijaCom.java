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

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.XtremestreamCo;

@DecrypterPlugin(revision = "$Revision: 52593 $", interfaceVersion = 3, names = {}, urls = {})
public class TubePerverzijaCom extends PluginForDecrypt {
    public TubePerverzijaCom(PluginWrapper wrapper) {
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
        ret.add(new String[] { "tube.perverzija.com" });
        ret.add(new String[] { "pervtube.net" });
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
            ret.add("https?://" + buildHostsPatternPart(domains) + "/([\\w\\-]+)/?");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = param.getCryptedUrl();
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("property\\s*=\\s*\"og:title\" content\\s*=\\s*\"([^\"]+)").getMatch(0);
        if (title != null) {
            /* Perform some corrections */
            title = Encoding.htmlDecode(title).trim();
            title = title.replaceFirst("^Watch FREE\\s*", "");
            title = title.replaceFirst("\\s*(\\|?\\s*)" + Pattern.quote(br.getHost()) + "$", "");
        }
        final PluginForHost plg = this.getNewPluginForHostInstance("xtremestream.co");
        String[] links = HTMLParser.getHttpLinks(br.getRequest().getHtmlCode(), br.getURL());
        for (final String singleLink : links) {
            if (plg.canHandle(singleLink)) {
                final DownloadLink video = createDownloadlink(singleLink);
                /* Required in order to access embedded content later. */
                video.setReferrerUrl(br.getURL());
                if (title != null) {
                    video.setProperty(XtremestreamCo.PROPERTY_TITLE, title);
                } else {
                    final String urlSlug = new Regex(contenturl, this.getSupportedLinks()).getMatch(0);
                    final String titleFallback = urlSlug.replace("-", " ").trim();
                    video.setProperty(XtremestreamCo.PROPERTY_TITLE, titleFallback);
                }
                ret.add(video);
            }
        }
        if (!ret.isEmpty()) {
            return ret;
        }
        /* Search for iframe embedded items e.g. from playhydrax.com */
        links = br.getRegex("<iframe[^>]*src=\"(https?://[^\"]+)").getColumn(0);
        if (links != null && links.length > 0) {
            for (final String url : links) {
                ret.add(createDownloadlink(url));
            }
        }
        if (!ret.isEmpty()) {
            return ret;
        }
        /**
         * Examples for invalid links: <br>
         * https://tube.perverzija.com/advanced-search/ <br>
         * https://tube.perverzija.com/contact/ <br>
         * https://tube.perverzija.com/wp-admin/
         */
        logger.info("Failed to find any supported links -> Link unsupported or plugin problem");
        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
    }
}
