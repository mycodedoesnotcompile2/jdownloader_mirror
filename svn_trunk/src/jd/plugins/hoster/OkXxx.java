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

import org.appwork.utils.Regex;

@HostPlugin(revision = "$Revision: 51169 $", interfaceVersion = 3, names = {}, urls = {})
public class OkXxx extends KernelVideoSharingComV2 {
    public OkXxx(final PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "ok.xxx" });
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
            ret.add("https?://(?:\\w+\\.)?" + buildHostsPatternPart(domains) + "/(?:video/\\d+/|embed/\\d+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    protected String getFUIDFromURL(String url) {
        final String ret = new Regex(url, "/(?:video|embed)/(\\d+)").getMatch(0);
        if (ret != null) {
            return ret;
        }
        return super.getFUIDFromURL(url);
    }

    @Override
    String generateContentURL(final String host, final String fuid, final String urlSlug) {
        if (host == null || fuid == null) {
            return null;
        }
        return this.getProtocol() + host + "/video/" + fuid + "/";
    }

    @Override
    protected String regexNormalTitleWebsite(final Browser br) {
        String title = br.getRegex("(?i)<title>Video\\s*🌶️([^<]+) - OK\\.XXX</title>").getMatch(0);
        if (title == null) {
            title = br.getRegex("(?i)<div class=\"desc\">(?!description)([^<]+)</div>").getMatch(0);
        }
        if (title != null) {
            return title;
        } else {
            /* Fallback to upper handling */
            return super.regexNormalTitleWebsite(br);
        }
    }

    @Override
    protected boolean useEmbedWorkaround() {
        return true;
    }

    @Override
    protected boolean enableFastLinkcheck() {
        /* 2024-04-25: They're using HLS streams and at this moment we are not calculating the filesize in upper handling. */
        return true;
    }
}