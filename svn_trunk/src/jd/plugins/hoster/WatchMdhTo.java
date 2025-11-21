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
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.StringUtils;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.AccountRequiredException;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51859 $", interfaceVersion = 3, names = {}, urls = {})
public class WatchMdhTo extends KernelVideoSharingComV2 {
    public WatchMdhTo(final PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "fyxxr.to", "watchdirty.org", "watchdirty.is", "watchdirty.to", "watchmdh.to" });
        ret.add(new String[] { "mdhstream.cc", "fyxstream.cc" });
        ret.add(new String[] { "mdhporn.co", "fyxporn.co" });
        return ret;
    }

    @Override
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        deadDomains.add("watchdirty.to");
        deadDomains.add("watchmdh.to");
        return deadDomains;
    }

    private boolean isSupportedDomain(final URL url) throws IOException {
        final String host = url.getHost();
        for (final String[] pluginDomains : getPluginDomains()) {
            for (final String pluginDomain : pluginDomains) {
                if (StringUtils.containsIgnoreCase(host, pluginDomain)) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    protected String getDllink(final DownloadLink link, final Browser br) throws PluginException, IOException {
        /* Special handling */
        final String embed = br.getRegex("(https?://[^/]*/embed/\\d+)").getMatch(0);
        if (embed == null) {
            /* No embed link found -> No special handling needed */
            return super.getDllink(link, br);
        }
        if (StringUtils.equals(br._getURL().getPath(), new URL(embed).getPath())) {
            /* We are already on the URL we just regexed -> No special handling needed */
            return super.getDllink(link, br);
        }
        if (!isSupportedDomain(new URL(embed))) {
            /* URL inside embed link is not supported -> No special handling allowed */
            logger.info("Found embed link with unsupported domain -> " + embed);
            return super.getDllink(link, br);
        }
        br.setFollowRedirects(true);
        br.getPage(embed);
        final String msg = this.getPrivateVideoWebsiteMessage(br);
        if (msg != null) {
            throw new AccountRequiredException(msg);
        }
        if (isOfflineWebsite(br)) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return getDllink(link, br);
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
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : getPluginDomains()) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/((videos?/)?[^/\\?#]+/?|embed/\\d+/?)");
        }
        return ret.toArray(new String[0]);
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