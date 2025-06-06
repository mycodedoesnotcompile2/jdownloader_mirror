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

import org.jdownloader.plugins.components.config.KVSConfig;
import org.jdownloader.plugins.components.config.KVSConfigPorntrexCom;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 51052 $", interfaceVersion = 3, names = {}, urls = {})
public class PorntrexCom extends KernelVideoSharingComV2 {
    public PorntrexCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/signup/");
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "porntrex.com" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(video/\\d+/[a-z0-9\\-]+|embed/\\d+/?)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    String generateContentURL(final String host, final String fuid, final String urlSlug) {
        if (host == null || fuid == null || urlSlug == null) {
            return null;
        }
        return this.getProtocol() + "www." + host + "/video/" + fuid + "/" + urlSlug;
    }

    @Override
    protected boolean preferTitleHTML() {
        return true;
    }

    @Override
    protected String regexNormalTitleWebsite(final Browser br) {
        final String title = br.getRegex("video_title:\\s*'([^']+)'").getMatch(0);
        if (title != null) {
            return title;
        } else {
            return super.regexNormalTitleWebsite(br);
        }
    }

    @Override
    public Class<? extends KVSConfig> getConfigInterface() {
        return KVSConfigPorntrexCom.class;
    }
}