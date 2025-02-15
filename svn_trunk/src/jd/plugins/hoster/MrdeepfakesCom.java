//jDownloader - Downloadmanager
//Copyright (C) 2013  JD-Team support@jdownloader.org
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

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 50530 $", interfaceVersion = 3, names = {}, urls = {})
public class MrdeepfakesCom extends KernelVideoSharingComV2 {
    public MrdeepfakesCom(final PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "mrdeepfakes.com" });
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
        return buildAnnotationUrlsDefaultVideosPatternWithoutFileID(getPluginDomains());
    }

    public static String[] buildAnnotationUrlsDefaultVideosPatternWithoutFileID(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(video/\\d+/[a-z0-9\\-]+/?|embed/\\d+/?)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    String generateContentURL(final String host, final String fuid, final String urlSlug) {
        if (host == null || fuid == null || urlSlug == null) {
            return null;
        }
        return this.getProtocol() + host + "/video/" + fuid + "/" + urlSlug;
    }

    @Override
    protected boolean isPrivateVideoWebsite(final Browser br) {
        if (br.containsHTML("class=\"message\">\\s*This video is a Premium video uploaded by")) {
            /* 2022-09-05 */
            return true;
        } else {
            return super.isPrivateVideoWebsite(br);
        }
    }

    @Override
    protected boolean useEmbedWorkaround() {
        return true;
    }

    @Override
    protected boolean isOfflineWebsite(final Browser br) {
        if (br.containsHTML(">\\s*Video deleted")) {
            return true;
        } else {
            return super.isOfflineWebsite(br);
        }
    }
}