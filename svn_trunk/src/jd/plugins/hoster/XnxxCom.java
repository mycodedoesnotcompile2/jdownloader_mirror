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
package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.jdownloader.plugins.components.config.XvideosComXnxxComConfig;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 50841 $", interfaceVersion = 2, names = {}, urls = {})
public class XnxxCom extends XvideosCore {
    public XnxxCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/");
    }

    @Override
    public String getAGBLink() {
        return "https://info.xnxx.com/legal/tos";
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "xnxx.com", "xnxx.cc" });
        return ret;
    }

    protected String[] getAllDomains() {
        return getPluginDomains().get(0);
    };

    @Override
    protected String getFallbackPremiumDomain() {
        return this.getHost();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    private static final Pattern PATTERN_VIDEO = Pattern.compile("/video-([a-z0-9\\-]+)/([^/]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_EMBED = Pattern.compile("/embedframe/\\d+", Pattern.CASE_INSENSITIVE);

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            String pattern = "https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "(";
            pattern += PATTERN_VIDEO.pattern();
            pattern += "|";
            pattern += PATTERN_EMBED.pattern();
            pattern += "|";
            pattern += TYPE_CLICK.pattern();
            pattern += ")";
            ret.add(pattern);
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public Class<XvideosComXnxxComConfig> getConfigInterface() {
        return XvideosComXnxxComConfig.class;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    protected String buildNormalContentURL(final DownloadLink link) {
        if (new Regex(link.getPluginPatternMatcher(), TYPE_CLICK).patternFind()) {
            /* We cannot change this type of link to any other type. */
            return link.getPluginPatternMatcher();
        }
        final String urlHost = Browser.getHost(link.getPluginPatternMatcher(), false);
        final String videoID = this.getVideoidFromURL(link.getPluginPatternMatcher());
        if (videoID != null) {
            /* 2021-07-23: This needs to end with a slash otherwise the URL will be invalid! */
            String newURL = "https://www." + urlHost + "/video-" + videoID;
            final String urlTitle = getURLTitle(link);
            if (urlTitle != null) {
                newURL += "/" + urlTitle;
            } else {
                /* URL needs to contain a title otherwise we'll get error 404! */
                newURL += "/dummytext";
            }
            return newURL;
        }
        return null;
    }

    @Override
    protected boolean looksLikeSupportedLink(final String url) {
        if (new Regex(url, PATTERN_VIDEO).patternFind()) {
            return true;
        }
        if (new Regex(url, PATTERN_EMBED).patternFind()) {
            return true;
        }
        return this.canHandle(url);
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}