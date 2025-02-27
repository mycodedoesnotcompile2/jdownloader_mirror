//jDownloader - Downloadmanager
//Copyright (C) 2020  JD-Team support@jdownloader.org
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

import jd.PluginWrapper;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 50708 $", interfaceVersion = 3, names = {}, urls = {})
public class SunPornoCom extends KernelVideoSharingComV2 {
    public SunPornoCom(final PluginWrapper wrapper) {
        super(wrapper);
    }

    /** Add all KVS hosts to this list that fit the main template without the need of ANY changes to this class. */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "sunporno.com" });
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(v/\\d+/[a-z0-9\\-]+/?|embed/\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    private static final Pattern PATTERN_VIDEO = Pattern.compile("/v/(\\d+)/([a-z0-9\\-]+)/?", Pattern.CASE_INSENSITIVE);

    @Override
    protected String getURLTitle(final String url) {
        if (url == null) {
            return null;
        }
        return new Regex(url, PATTERN_VIDEO).getMatch(1);
    }

    @Override
    protected String getFUID(final DownloadLink link) {
        if (link.getPluginPatternMatcher() == null) {
            return null;
        }
        final String url = link.getPluginPatternMatcher();
        String fid = new Regex(url, PATTERN_VIDEO).getMatch(1);
        if (fid == null) {
            fid = new Regex(link.getPluginPatternMatcher(), pattern_embedded).getMatch(0);
        }
        return fid;
    }

    @Override
    String generateContentURL(final String host, final String fuid, final String urlSlug) {
        if (host == null || fuid == null || urlSlug == null) {
            return null;
        }
        return this.getProtocol() + "www." + host + "/v/" + fuid + "/" + urlSlug + "/";
    }
}