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
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 51377 $", interfaceVersion = 3, names = {}, urls = {})
public class WatchMdhTo extends KernelVideoSharingComV2 {
    public WatchMdhTo(final PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "watchdirty.org", "watchdirty.is", "watchdirty.to", "watchmdh.to" });
        return ret;
    }

    @Override
    public String rewriteHost(final String host) {
        return this.rewriteHost(getPluginDomains(), host);
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return KernelVideoSharingComV2.buildAnnotationUrlsDefaultVideosPatternWithoutFileID(getPluginDomains());
    }

    @Override
    protected boolean hasFUIDInsideURL(final String url) {
        return false;
    }

    @Override
    protected String generateContentURL(final String host, final String fuid, final String urlTitle) {
        if (host == null || urlTitle == null) {
            return null;
        }
        return this.getProtocol() + this.getHost() + "/video/" + urlTitle + "/";
    }

    @Override
    protected boolean isRequiresWWW() {
        return false;
    }
}