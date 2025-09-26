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

import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 51575 $", interfaceVersion = 3, names = {}, urls = {})
public class KernelVideoSharingComV2NudeyogaNet extends KernelVideoSharingComV2 {
    public KernelVideoSharingComV2NudeyogaNet(final PluginWrapper wrapper) {
        super(wrapper);
    }

    /** Add all KVS hosts to this list that fit the main template without the need of ANY changes to this class. */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "nudeyoga.net" });
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
    protected boolean isOfflineWebsite(final Browser br) {
        if (StringUtils.containsIgnoreCase(br.getURL(), "/embed/")) {
            /* Embed link -> Use upper code for offline detection, the special code down below will fail for embed items. */
            return super.isOfflineWebsite(br);
        } else if (!br.containsHTML("/embed/\\d+") && !br.containsHTML("video_id:") && !br.containsHTML("params\\['video_id'\\]")) {
            /* e.g. https://nudeyoga.net/contact/ */
            return true;
        } else {
            return super.isOfflineWebsite(br);
        }
    }

    @Override
    protected String getPrivateVideoWebsiteMessage(final Browser br) {
        if (br.containsHTML("class=\"player\"[^>]*>\\s*This video is a unique video uploaded by our users and friends")) {
            /**
             * Examples: <br>
             * /naked-couple/ <br>
             * /ballet-teacher-humiliate-young-ballerina/
             */
            return "This video is a private video. Only active members can watch private videos.";
        } else {
            return super.getPrivateVideoWebsiteMessage(br);
        }
    }
}