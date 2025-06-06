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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51093 $", interfaceVersion = 3, names = {}, urls = {})
public class KernelVideoSharingComV2TabootubeXxx extends KernelVideoSharingComV2 {
    public KernelVideoSharingComV2TabootubeXxx(final PluginWrapper wrapper) {
        super(wrapper);
    }

    /** Add all KVS hosts to this list that fit the main template without the need of ANY changes to this class. */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "tabootube.xxx" });
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
        return KernelVideoSharingComV2.buildAnnotationUrlsDefaultNoVideosNoFUID(getPluginDomains());
    }

    @Override
    protected boolean hasFUIDInsideURL(final String url) {
        return false;
    }

    @Override
    protected String generateContentURL(final String host, final String fuid, final String urlSlug) {
        return generateContentURLDefaultNoVideosNoFUID(host, urlSlug);
    }

    @Override
    protected String getDllink(final DownloadLink link, final Browser br) throws PluginException, IOException {
        final String[] downloadlinks = br.getRegex("class=\"download-link\" data-href=\"(https?://[^\"]+)").getColumn(0);
        if (downloadlinks == null || downloadlinks.length == 0) {
            return null;
        }
        /* Last = Best */
        return downloadlinks[downloadlinks.length - 1];
    }

    @Override
    protected boolean isOfflineWebsite(final Browser br) {
        if (!StringUtils.containsIgnoreCase(br.getURL(), "/embed/") && !br.containsHTML("/embed/\\d+")) {
            /* Invalid link e.g. /categories/ */
            return true;
        } else {
            return super.isOfflineWebsite(br);
        }
    }
}