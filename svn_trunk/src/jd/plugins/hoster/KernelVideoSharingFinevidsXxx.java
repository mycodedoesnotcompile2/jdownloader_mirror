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
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51093 $", interfaceVersion = 3, names = {}, urls = {})
public class KernelVideoSharingFinevidsXxx extends KernelVideoSharingComV2 {
    public KernelVideoSharingFinevidsXxx(final PluginWrapper wrapper) {
        super(wrapper);
    }

    /** Add all KVS hosts to this list that fit the main template without the need of ANY changes to this class. */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "finevids.xxx" });
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
    protected String getDllink(final DownloadLink link, final Browser br) throws PluginException, IOException {
        /**
         * 2025-05-27: .flv videos still exist e.g. /videos/40199/alina-jerry-amazing-hose-movie/ <br>
         * Example broken video: /videos/6064/watch-hawt-veronika-get-in-nature-s-garb/
         */
        String dllink = br.getRegex("src:\\s*'(http[^\\']+\\.(flv|mp4))'").getMatch(0);
        if (dllink == null) {
            dllink = br.getRegex("<source src=\"(https?://[^\"]+)\" type=\"video/mp4").getMatch(0);
        }
        if (dllink != null) {
            return dllink;
        } else {
            return super.getDllink(link, br);
        }
    }

    @Override
    protected int getMaxChunks(final Account account) {
        /* 2025-01-09 */
        return 1;
    }
}