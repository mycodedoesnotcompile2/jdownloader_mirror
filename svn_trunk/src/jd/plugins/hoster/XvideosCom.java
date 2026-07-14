//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.jdownloader.plugins.components.config.XvideosComConfig;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.PluginDependencies;

@HostPlugin(revision = "$Revision: 52974 $", interfaceVersion = 2, names = {}, urls = {})
@PluginDependencies(dependencies = { jd.plugins.decrypter.XvideosComProfile.class })
public class XvideosCom extends XvideosCore {
    public XvideosCom(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://xvideos.red/");
    }

    private static final Pattern PATTERN_NORMAL_DOT = Pattern.compile("video\\.[a-z0-9]+/.*", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_NORMAL     = Pattern.compile("video\\d+/.*", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_SPECIAL    = Pattern.compile("[a-z0-9\\-]+/(upload|pornstar|model)/[a-z0-9\\-_]+/\\d+/[^/#\\?]+", Pattern.CASE_INSENSITIVE);

    @Override
    public String getAGBLink() {
        return "https://info." + getHost() + "/legal/tos/";
    }

    private static List<String[]> getPluginDomains() {
        return jd.plugins.decrypter.XvideosComProfile.getPluginDomains();
    }

    protected String[] getAllDomains() {
        return getPluginDomains().get(0);
    };

    @Override
    public String[] getDeadDomains() {
        return new String[] { "xvideos2.com", "xvideos3.com", "haysex.biz" };
    }

    @Override
    public String[] getAvoidDomains() {
        /*
         * 2023-12-04: Added haysex.biz because that website looks quite diffent than main xvideos layout so using the main domain fixes
         * linkcheck and download handling in this case.
         */
        return new String[] { "haysex.biz" };
    };

    @Override
    protected String getFallbackPremiumDomain() {
        return "xvideos.red";
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        final String[] domains = buildSupportedNames(getPluginDomains());
        String[] supportedNames = new String[domains.length + 1];
        for (int i = 0; i < domains.length; i++) {
            supportedNames[i] = domains[i];
        }
        /* Add additional names here */
        supportedNames[supportedNames.length - 1] = "xvideos";
        return supportedNames;
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            final String pattern = "https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "/(" + PATTERN_NORMAL_DOT.pattern() + "|" + PATTERN_NORMAL.pattern() + "|" + TYPE_EMBED.pattern().substring(1) + "|" + PATTERN_SPECIAL.pattern()
            /* 2024-04-02 */
                    + "|" + TYPE_CLICK.pattern().substring(1) + ")";
            ret.add(pattern);
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public Class<XvideosComConfig> getConfigInterface() {
        return XvideosComConfig.class;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    protected String buildNormalContentURL(final DownloadLink link) {
        final String urlHost = Browser.getHost(link.getPluginPatternMatcher(), false);
        final String videoID = this.getEncodedVideoID(link);
        if (videoID == null) {
            return null;
        }
        String newURL;
        if (new Regex(link.getPluginPatternMatcher(), TYPE_NORMAL_DOT).patternFind() || new Regex(link.getPluginPatternMatcher(), TYPE_CLICK).patternFind() || new Regex(link.getPluginPatternMatcher(), TYPE_EMBED).patternFind()) {
            newURL = "https://www." + urlHost + "/video." + videoID;
        } else {
            newURL = "https://www." + urlHost + "/video" + videoID;
        }
        final String urlTitle = getURLTitle(link);
        if (urlTitle != null) {
            newURL += "/" + urlTitle;
        } else {
            /* URL needs to contain a title otherwise we'll get error 404! */
            newURL += "/dummytext";
        }
        return newURL;
    }
}