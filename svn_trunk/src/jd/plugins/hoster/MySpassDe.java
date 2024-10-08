//jDownloader - Downloadmanager
//Copyright (C) 2009  JD-Team support@jdownloader.org
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
import java.text.DecimalFormat;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 49927 $", interfaceVersion = 3, names = { "myspass.de" }, urls = { "https?://(?:www\\.)?myspassdecrypted\\.de/.+\\d+/?$|https://(?:www\\.)?myspass\\.de/player\\?video=\\d+" })
public class MySpassDe extends PluginForHost {
    public MySpassDe(PluginWrapper wrapper) {
        super(wrapper);
        setConfigElements();
    }

    private String dllink = null;

    @Override
    public String getAGBLink() {
        return "https://www." + getHost() + "/myspass/kontakt/";
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
        return new Regex(link.getPluginPatternMatcher(), "(\\d+)/?$").getMatch(0);
    }

    private static final AtomicReference<String> token                   = new AtomicReference<String>(null);
    private static final AtomicReference<String> cdn                     = new AtomicReference<String>(null);
    private static final AtomicLong              timestampTokenRefreshed = new AtomicLong(0);
    private static final AtomicLong              timestampCDNRefreshed   = new AtomicLong(0);

    /*
     * Example final url (18.05.2015):
     * http://x3583brainc11021.s.o.l.lb.core-cdn.net/secdl/78de6150fffffffffff1f136aff77d61/55593149/11021brainpool/ondemand
     * /3583brainpool/163840/myspass2009/14/660/10680/18471/18471_61.mp4
     */
    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws IOException, PluginException {
        return requestFileInformation(link, false);
    }

    private AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws IOException, PluginException {
        dllink = null;
        final String ext = ".mp4";
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        final String fid = this.getFID(link);
        if (!link.isNameSet()) {
            link.setName(fid + ext);
        }
        synchronized (token) {
            if (token.get() == null || System.currentTimeMillis() - timestampTokenRefreshed.get() > 30 * 60 * 1000) {
                logger.info("Obtaining fresh token");
                final Browser brc = br.cloneBrowser();
                brc.getPage("https://www." + getHost() + "/_next/static/chunks/pages/_app-42298c98727543c2.js");
                final String freshToken = brc.getRegex("Bearer ([a-f0-9]{256})").getMatch(0);
                if (freshToken == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                token.set(freshToken);
                timestampTokenRefreshed.set(System.currentTimeMillis());
                String freshCDN = brc.getRegex("uri:`(https?://[^/]+)\\$").getMatch(0);
                if (freshCDN == null) {
                    logger.warning("Failed to find CDN -> Using static fallback");
                    freshCDN = "https://cms-myspass.vanilla-ott.com/api/videos/"; // 2024-10-07
                }
                cdn.set(freshCDN);
            }
        }
        synchronized (cdn) {
            if (cdn.get() == null || System.currentTimeMillis() - timestampCDNRefreshed.get() > 30 * 60 * 1000) {
                logger.info("Obtaining fresh cdn value");
                final Browser brc = br.cloneBrowser();
                brc.getPage("https://www." + getHost() + "/_next/static/chunks/329-696a1b2fb2c4bf5e.js");
                String freshCDN = brc.getRegex("uri:`(https?://[^/]+)\\$").getMatch(0);
                if (freshCDN == null) {
                    logger.warning("Failed to find CDN -> Using static fallback");
                    freshCDN = "https://cms-myspass.vanilla-ott.com/api/videos/"; // 2024-10-07
                }
                cdn.set(freshCDN);
                timestampCDNRefreshed.set(System.currentTimeMillis());
            }
        }
        final Browser brv = br.cloneBrowser();
        brv.getHeaders().put("Authorization", "Bearer " + token.get());
        brv.getPage("https://cms-myspass.vanilla-ott.com/api/videos/" + fid + "?populate[season][fields]=name&populate[format][fields]=name&");
        if (brv.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(brv.getRequest().getHtmlCode(), TypeRef.MAP);
        final Object errorO = entries.get("error");
        if (errorO != null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> attr = (Map<String, Object>) JavaScriptEngineFactory.walkJson(entries, "data/attributes");
        // final String broadcast_date = attr.get("broadcast_date").toString();
        String title = attr.get("title").toString();
        title = title.replaceFirst("\\(Folge \\d+\\)", "");
        final String unique_name = attr.get("unique_name").toString();
        final String description = (String) attr.get("teaser_text");
        this.dllink = (String) attr.get("video_url");
        if (this.dllink.startsWith("/")) {
            /* Add host, can be found here: https://www.myspass.de/_next/static/chunks/329-696a1b2fb2c4bf5e.js */
            this.dllink = cdn.get() + this.dllink;
        }
        if (!StringUtils.isEmpty(description) && StringUtils.isEmpty(link.getComment())) {
            link.setComment(description);
        }
        final String format = (String) JavaScriptEngineFactory.walkJson(attr, "format/data/attributes/name");
        final String seasonStr = new Regex(unique_name, "(?i)Staffel (\\d+)").getMatch(0);
        final String episodeStr = new Regex(unique_name, "(?i)Folge (\\d+)").getMatch(0);
        final DecimalFormat df = new DecimalFormat("00");
        String filename = format + " - ";
        if (seasonStr != null && episodeStr != null) { // Sometimes episode = 9/Best Of, need regex to get only the integer
            filename += "S" + df.format(Integer.parseInt(seasonStr)) + "E" + df.format(Integer.parseInt(episodeStr)) + " - ";
        }
        filename += title;
        filename = filename.trim();
        filename = Encoding.htmlDecode(filename);
        link.setFinalFileName(filename + ext);
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        if (dllink == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (dllink.contains(".m3u8")) {
            br.getPage(dllink);
            final HlsContainer hlsbest = HlsContainer.findBestVideoByBandwidth(HlsContainer.getHlsQualities(this.br));
            final String url_hls = hlsbest.getStreamURL();
            checkFFmpeg(link, "Download a HLS Stream");
            dl = new HLSDownloader(link, br, url_hls);
            dl.startDownload();
        } else {
            /* 2017-02-04: Without the Range Header we'll be limited to ~100 KB/s */
            link.setProperty(DirectHTTP.PROPERTY_ServerComaptibleForByteRangeRequest, true);
            br.getHeaders().put(OPEN_RANGE_REQUEST);
            /* Workaround for old downloadcore bug that can lead to incomplete files */
            br.getHeaders().put("Accept-Encoding", "identity");
            dl = jd.plugins.BrowserAdapter.openDownload(br, link, dllink, false, 1);
            handleConnectionErrors(br, dl.getConnection());
            dl.startDownload();
        }
    }

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "FAST_LINKCHECK", "Enable fast linkcheck?\r\nFilesize will only be visible on downloadstart!").setDefaultValue(true));
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}
