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
import org.jdownloader.plugins.components.config.KVSConfigWhoreshubCom;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 50551 $", interfaceVersion = 3, names = {}, urls = {})
public class WhoreshubCom extends KernelVideoSharingComV2 {
    public WhoreshubCom(final PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://www." + getHost() + "/");
    }

    /** Add all KVS hosts to this list that fit the main template without the need of ANY changes to this class. */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "whoreshub.com" });
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
    protected int getMaxChunks(final Account account) {
        /* 2022-08-10: Max total connections per IP: 10 */
        return -2;
    }

    @Override
    protected boolean isLoggedIN(final Browser br) {
        if (!super.isLoggedIN(br)) {
            return false;
        }
        /* Looks like we are logged in -> Perform deeper check */
        try {
            // normal main website does not indicate login being expired but accessing profile or videos result in "please login"
            final Browser ibr = br.cloneBrowser();
            ibr.getPage("/my/");
            if (ibr.getURL().endsWith("/my/")) {
                return true;
            }
        } catch (Exception e) {
            logger.log(e);
        }
        return false;
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return 5;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return 5;
    }

    @Override
    public Class<? extends KVSConfig> getConfigInterface() {
        return KVSConfigWhoreshubCom.class;
    }
}