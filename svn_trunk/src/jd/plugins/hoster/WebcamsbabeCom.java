//jDownloader - Downloadmanager
//Copyright (C) 2020  JD-Team support@jdownloader.org
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
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@HostPlugin(revision = "$Revision: 51795 $", interfaceVersion = 3, names = {}, urls = {})
public class WebcamsbabeCom extends KernelVideoSharingComV2 {
    public WebcamsbabeCom(final PluginWrapper wrapper) {
        super(wrapper);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "sexcams-24.com", "webcamsbabe.com" });
        /* Russian version of webcamsbabe.com. Same URL-pattern but different content/file-servers/contentIDs! */
        ret.add(new String[] { "privaterecords.life", "private-records.com", "webcamvau.com", "privat-zapisi.biz" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(videos/\\d+-[a-z0-9\\-]+\\.html|embed/\\d+/?)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String rewriteHost(String host) {
        /* 2022-07-29: privat-zapisi.biz is now private-records.com */
        return this.rewriteHost(getPluginDomains(), host);
    }

    protected String getURLTitle(final String url) {
        if (url == null) {
            return null;
        } else {
            return new Regex(url, "(?i)/videos/\\d+-(.+)\\.html$").getMatch(0);
        }
    }

    @Override
    String generateContentURL(final String host, final String fuid, final String urlSlug) {
        if (host == null || fuid == null || urlSlug == null) {
            return null;
        }
        return this.getProtocol() + host + "/videos/" + fuid + "-" + urlSlug + ".html";
    }

    @Override
    protected String getDllink(final DownloadLink link, final Browser br) throws PluginException, IOException {
        /* Special handling */
        final String embed = br.getRegex("<iframe[^>]*(https?://(?:www\\.)?sexcams-24\\.com/embed/\\d+)").getMatch(0);
        if (embed != null && !StringUtils.equals(br._getURL().getPath(), new URL(embed).getPath())) {
            /* 2025-11-05: e.g. privaterecords.life/embed... -> sexcams-24.com/... */
            br.getPage(embed);
            if (isOfflineWebsite(br)) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            return super.getDllink(link, br);
        } else {
            return super.getDllink(link, br);
        }
    }
}