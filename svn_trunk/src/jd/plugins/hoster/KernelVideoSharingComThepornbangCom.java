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
import java.util.Map;

import org.jdownloader.plugins.components.config.KVSConfig;
import org.jdownloader.plugins.components.config.KVSConfigPorngreyNet;
import org.jdownloader.plugins.components.config.KVSConfigThepornbangCom;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;

@HostPlugin(revision = "$Revision: 52431 $", interfaceVersion = 3, names = {}, urls = {})
public class KernelVideoSharingComThepornbangCom extends KernelVideoSharingComV2 {
    public KernelVideoSharingComThepornbangCom(final PluginWrapper wrapper) {
        super(wrapper);
    }

    /** Add all KVS hosts to this list that fit the main template without the need of ANY changes to this class. */
    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "thepornbang.com" });
        ret.add(new String[] { "porngrey.net" });
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
        return KernelVideoSharingComThepornbangCom.buildAnnotationThepornbang(getPluginDomains());
    }

    public static String[] buildAnnotationThepornbang(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/video/([^/\\?#]+)/?");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    protected String generateContentURL(final String host, final String fuid, final String urlTitle) {
        return generateContentURLDefaultVideoNoFUID(host, fuid);
    }

    @Override
    protected boolean hasFUIDInsideURL(final String url) {
        return false;
    }

    @Override
    protected String handleQualitySelection(Browser br, DownloadLink link, Map<Integer, String> qualityMap) {
        if (qualityMap == null || qualityMap.isEmpty()) {
            return null;
        }
        final Map<Integer, String> qualityMapNew = handleOkRuGenerateMp4(br);
        if (qualityMapNew != null) {
            return super.handleQualitySelection(br, link, qualityMapNew);
        }
        return super.handleQualitySelection(br, link, qualityMap);
    }

    @Override
    protected boolean preferTitleHTML() {
        return true;
    }

    @Override
    protected boolean isOfflineWebsite(final Browser br) {
        if (!this.canHandle(br.getURL()) && !br.containsHTML("video_id:") && !br.containsHTML("var flashvars")) {
            /* Redirect to unsupported link happened e.g. https://www.thepornbang.com/home15/ */
            return true;
        }
        return super.isOfflineWebsite(br);
    }

    @Override
    protected AvailableStatus requestFileInformationWebsite(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        final AvailableStatus status = super.requestFileInformationWebsite(link, account, isDownload);
        final String htmlTitleTag = br.getRegex("<title>([^<]+)").getMatch(0);
        if (htmlTitleTag != null) {
            link.setProperty("title", Encoding.htmlDecode(htmlTitleTag).trim());
        }
        return status;
    }

    @Override
    public Class<? extends KVSConfig> getConfigInterface() {
        if (this.getHost().equals("thepornbang.com")) {
            return KVSConfigThepornbangCom.class;
        } else {
            return KVSConfigPorngreyNet.class;
        }
    }

    @Override
    protected boolean enableFastLinkcheck() {
        /* 2024-09-25: To counter problems reported here: https://board.jdownloader.org/showthread.php?t=96207 */
        return true;
    }
}