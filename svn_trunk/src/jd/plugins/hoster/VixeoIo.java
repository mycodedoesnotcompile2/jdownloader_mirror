//jDownloader - Downloadmanager
//Copyright (C) 2026  JD-Team support@jdownloader.org
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
import java.util.Map;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 54198 $", interfaceVersion = 3, names = {}, urls = {})
public class VixeoIo extends PluginForHost {
    public VixeoIo(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.VIDEO_STREAMING };
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost();
    }

    private static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "vixeo.io" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?:e|v)/([A-Za-z0-9]{12})");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String fid = getFID(link);
        if (fid != null) {
            return this.getHost() + "://" + fid;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(0);
    }

    @Override
    protected String getDefaultFileName(final DownloadLink link) {
        return this.getFID(link) + ".mp4";
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            return true;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            return true;
        } else {
            return true;
        }
    }

    public int getMaxChunks(final Account account) {
        final AccountType type = account != null ? account.getType() : null;
        if (AccountType.FREE.equals(type)) {
            return 0;
        } else if (AccountType.PREMIUM.equals(type) || AccountType.LIFETIME.equals(type)) {
            return 0;
        } else {
            return 0;
        }
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        return false;
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        this.setBrowserExclusive();
        br.getPage("https://" + getHost() + "/e/" + this.getFID(link));
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> config = parsePlayerConfig(br);
        final String title = (String) config.get("title");
        if (!StringUtils.isEmpty(title)) {
            link.setFinalFileName(this.correctOrApplyFileNameExtension(title.trim(), ".mp4", null));
        }
        final Number size = (Number) config.get("size");
        if (size != null && size.longValue() > 0) {
            link.setDownloadSize(size.longValue());
        }
        return AvailableStatus.TRUE;
    }

    /** Player config is a base64 encoded json inside attribute "data-config" of the player root element. */
    private Map<String, Object> parsePlayerConfig(final Browser br) throws PluginException {
        final String b64 = br.getRegex("id=\"streamsonic-player-root\"[^>]*data-config=\"([A-Za-z0-9+/=]+)\"").getMatch(0);
        if (b64 == null) {
            if (!br.containsHTML("streamsonic-player-root")) {
                /* No player -> Most likely offline */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Player config not found");
        }
        final String json = Base64.decodeToString(b64);
        if (json == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to decode player config");
        }
        return restoreFromString(json, TypeRef.MAP);
    }

    /**
     * Field "source" is a hex encoded string, split into chunks by "|". Decoded string is the reversed stream URL. <br>
     * See function J5 in embed player javascript.
     */
    private static String decodeSource(final String source) {
        if (source == null) {
            return null;
        }
        final String hex = source.replace("|", "");
        if (hex.length() == 0 || hex.length() % 2 != 0 || !hex.matches("(?i)[0-9a-f]+")) {
            return null;
        }
        final StringBuilder sb = new StringBuilder(hex.length() / 2);
        for (int i = 0; i < hex.length(); i += 2) {
            sb.append((char) Integer.parseInt(hex.substring(i, i + 2), 16));
        }
        return sb.reverse().toString();
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(link, null);
    }

    private void handleDownload(final DownloadLink link, final Account account) throws Exception, PluginException {
        requestFileInformation(link);
        final Map<String, Object> config = parsePlayerConfig(br);
        final String streamURL = decodeSource((String) config.get("source"));
        if (StringUtils.isEmpty(streamURL) || !StringUtils.startsWithCaseInsensitive(streamURL, "http")) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to decode stream URL");
        }
        if (Boolean.TRUE.equals(config.get("isMp4"))) {
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, streamURL, this.isResumeable(link, account), this.getMaxChunks(account));
            handleConnectionErrors(br, dl.getConnection());
            dl.startDownload();
        } else {
            checkFFmpeg(link, "Download a HLS Stream");
            final Browser brc = br.cloneBrowser();
            brc.getPage(streamURL);
            if (brc.getHttpConnection().getResponseCode() == 403) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 5 * 60 * 1000l);
            } else if (brc.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 5 * 60 * 1000l);
            }
            /* Source can be either a master playlist or directly a media playlist (as seen 2026-11). */
            final HlsContainer hlsbest = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(brc));
            final String hlsURL = hlsbest != null ? hlsbest.getDownloadurl() : streamURL;
            dl = new HLSDownloader(link, br, hlsURL);
            dl.startDownload();
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }
}
