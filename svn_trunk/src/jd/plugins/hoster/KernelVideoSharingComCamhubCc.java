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

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 50749 $", interfaceVersion = 3, names = {}, urls = {})
public class KernelVideoSharingComCamhubCc extends KernelVideoSharingComV2 {
    public KernelVideoSharingComCamhubCc(final PluginWrapper wrapper) {
        super(wrapper);
    }

    /** Add all KVS hosts to this list that fit the main template without the need of ANY changes to this class. */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "camhub.cc", "camhub.world", "zzpornozz.xyz" });
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
        return KernelVideoSharingComV2.buildAnnotationUrlsDefaultVideosPattern(getPluginDomains());
    }

    @Override
    protected String generateContentURL(final String host, final String fuid, final String urlTitle) {
        return generateContentURLDefaultVideosPattern(host, fuid, urlTitle);
    }

    @Override
    protected boolean isOfflineWebsite(final Browser br) {
        final int responsecode = br.getHttpConnection().getResponseCode();
        if (responsecode == 403) {
            return true;
        } else {
            return super.isOfflineWebsite(br);
        }
    }

    @Override
    protected String getDllink(final DownloadLink link, final Browser br) throws PluginException, IOException {
        try {
            String dllink = super.getDllink(link, br);
            if (dllink != null) {
                /**
                 * Website does not "double-embed" video but direct-URL is contained in html code on first page. <br>
                 * Example: /videos/975585/videotitle/
                 */
                return dllink;
            }
        } catch (final Exception ignore) {
        }
        /**
         * Special handling e.g. for camhub.cc -> camhub.cc self-embed -> camhub.world -> Final video link here <br>
         * Example: /de/videos/738936/videotitle/
         */
        if (!isEmbedURL(br.getURL())) {
            final String fid = this.getFUIDFromURL(br.getURL());
            String continueLink = br.getRegex("(/([a-z0-9]{2}/)?embed/" + fid + ")").getMatch(0);
            if (continueLink != null) {
                br.getPage(continueLink);
            }
        }
        /* Look for "camhub.world" embed item. */
        String continueLink2 = br.getRegex("<iframe[^>]*src=\"(https?://[^\"]+)\"").getMatch(0);
        if (continueLink2 != null) {
            br.getPage(continueLink2);
        }
        return super.getDllink(link, br);
    }
}