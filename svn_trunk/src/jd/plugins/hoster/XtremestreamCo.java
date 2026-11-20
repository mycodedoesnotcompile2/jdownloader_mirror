// jDownloader - Downloadmanager
// Copyright (C) 2017 JD-Team support@jdownloader.org
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;

import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.downloader.hls.M3U8Playlist;
import org.jdownloader.plugins.components.config.XtremestreamCoConfig;
import org.jdownloader.plugins.components.config.XtremestreamCoConfig.DownloadMode;
import org.jdownloader.plugins.components.config.XtremestreamCoConfig.Quality;
import org.jdownloader.plugins.components.hls.HlsContainer;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

@HostPlugin(revision = "$Revision: 54085 $", interfaceVersion = 3, names = {}, urls = {})
public class XtremestreamCo extends PluginForHost {
    public XtremestreamCo(PluginWrapper wrapper) {
        super(wrapper);
    }

    public static final String PROPERTY_TITLE = "title";

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.XXX };
    }
    /* DEV NOTES */
    // Tags: Porn plugin
    // other:

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForHost, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "xtremestream.co", "xtremestream.xyz" });
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
            ret.add("https?://(\\w+)\\." + buildHostsPatternPart(domains) + "/player/index\\.php\\?data=([a-f0-9]{32})");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public String getAGBLink() {
        return "https://" + this.getHost();
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        final String file_id = getFID(link);
        if (file_id != null) {
            return this.getHost() + "://" + file_id;
        } else {
            return super.getLinkID(link);
        }
    }

    private String getFID(final DownloadLink link) {
        return new Regex(link.getPluginPatternMatcher(), this.getSupportedLinks()).getMatch(1);
    }

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final String ext = ".mp4";
        final String pre_set_title = link.getStringProperty(PROPERTY_TITLE);
        if (!link.isNameSet()) {
            if (pre_set_title != null) {
                link.setName(pre_set_title + ext);
            } else {
                link.setName(this.getFID(link) + ext);
            }
        }
        this.setBrowserExclusive();
        br.setFollowRedirects(true);
        String referer = link.getReferrerUrl();
        if (referer == null || StringUtils.containsIgnoreCase(referer, "tube.perverzija.com")) {
            referer = "https://tube.perverzija.com/";
        }
        br.getHeaders().put("Referer", referer);
        br.getPage(link.getPluginPatternMatcher());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getRequest().getHtmlCode().length() <= 100) {
            /* Empty page */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("var video_title = .([^\"]*?)`;").getMatch(0);
        if (title == null) {
            title = br.getRegex("infoTitle:\\s*\"((?:[^\"\\\\]|\\\\.)*)\"").getMatch(0);
            if (title != null) {
                title = title.replace("\\'", "'");
            }
        }
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
        } else {
            /* Use pre-set title if available */
            title = pre_set_title;
        }
        if (title != null) {
            title = this.correctOrApplyFileNameExtension(title, ext, null);
            link.setFinalFileName(title);
        }
        return AvailableStatus.TRUE;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link);
        /* 2022-02-28: They're using an abnormal kind of m3u8 lists which is why a plugin is required in the first place. */
        final String referer = link.getReferrerUrl();
        final Browser br2 = br.cloneBrowser();
        final String fid = getFID(link);
        final String data_folderid;
        String data_xtremestream;
        String dltoken;
        if (referer != null) {
            br2.getPage(referer);
            data_folderid = br2.getRegex("data-folderid=\"([^\"]+)").getMatch(0);
            data_xtremestream = br2.getRegex("data-xtremestream=\"([^\"]+)").getMatch(0);
            dltoken = br2.getRegex("data-token=\"([^\"]+)").getMatch(0);
        } else {
            /* Fallback with hardcoded referer (fake blog that displays advertisements) */
            br2.getHeaders().put("Referer", "https://glasbanjaluke.net/");
            br2.getPage("https://pervm1.xtremestream.co/p/" + fid);
            data_folderid = br2.getRegex("folder:\\s*\"([^\"]+)").getMatch(0);
            data_xtremestream = br2.getRegex("xtremestream:\\s*\"([^\"]+)").getMatch(0);
            dltoken = br2.getRegex("token=([^\"\\&]+)").getMatch(0);
        }
        final XtremestreamCoConfig cfg = PluginJsonConfig.get(XtremestreamCoConfig.class);
        officialVideoDownload: if (data_folderid != null && data_xtremestream != null && dltoken != null) {
            /* Attempt official/progressive download */
            final DownloadMode mode = cfg.getDownloadMode();
            if (mode == DownloadMode.STREAM_DOWNLOAD) {
                /* User prefers stream download */
                logger.info("User prefers stream download");
                break officialVideoDownload;
            }
            boolean allowAutoFallbackToStreamDownload = mode == DownloadMode.AUTO;
            try {
                logger.info("Attempting official download");
                dltoken = Encoding.htmlDecode(dltoken);
                final Map<String, Object> postdata = new HashMap<String, Object>();
                postdata.put("folder", data_folderid);
                postdata.put("xtremestream", data_xtremestream);
                final UrlQuery query = new UrlQuery();
                query.appendEncoded("folder", data_folderid);
                query.appendEncoded("xtremestream", data_xtremestream);
                query.appendEncoded("token", dltoken);
                final Browser brc = br.cloneBrowser();
                final PostRequest req = brc.createJSonPostRequest("https://download.xtremestream.xyz/generateLinkForPlayer?" + query.toString(), postdata);
                req.getHeaders().put("Content-Type", "application/json");
                req.getHeaders().put("Accept", "*/*");
                brc.getPage(req);
                final Map<String, Object> entries = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
                String dllink = entries.get("link").toString();
                /* Typically https://pervm1.xtremestream.co/ */
                brc.getHeaders().put("Referer", "https://" + data_xtremestream + "." + getHost() + "/");
                dl = jd.plugins.BrowserAdapter.openDownload(brc, link, dllink, true, 0);
                this.handleConnectionErrors(brc, dl.getConnection());
                logger.info("Official download success!");
                /* Do not allow fallback at this stage! */
                allowAutoFallbackToStreamDownload = false;
                dl.startDownload();
                return;
            } catch (final InterruptedException ie) {
                throw ie;
            } catch (final Exception e) {
                if (!allowAutoFallbackToStreamDownload) {
                    throw e;
                }
                logger.log(e);
                logger.info("Official download failed -> Fallback to stream download");
            }
        }
        logger.info("Attempting stream download");
        /*
         * 2026-11-20: The old playlist (xs1.php) now only returns segment URLs which respond with 404 because the web player loads the
         * (encrypted) segments via a script. The server offers a regular HLS playlist with AES-128 encryption for Chromecast clients
         * ("castmaster") which only needs a token from the player page and a signature ("castsign").
         */
        final String playerOrigin = new Regex(link.getPluginPatternMatcher(), "^(https?://[^/]+)").getMatch(0);
        final String playerToken = br.getRegex("XSFTK\\.boot\\(\"[^\"]+\"\\s*,\\s*\"([^\"]+)\"").getMatch(0);
        if (playerOrigin == null || fid == null || playerToken == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find player token");
        }
        final String apiBase = playerOrigin + "/player/proto_api.php?data=" + fid + "&vt=" + Encoding.urlEncode(playerToken);
        br.getHeaders().put("Referer", playerOrigin + "/");
        String downloadurl = null;
        List<M3U8Playlist> legacyPlaylist = null;
        try {
            br.getHeaders().put("X-XS-P", "1");
            br.getPage(apiBase + "&op=castsign");
            br.getHeaders().remove("X-XS-P");
            final String castSignature = br.getRequest().getHtmlCode().trim();
            if (StringUtils.isEmpty(castSignature)) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to get cast signature");
            }
            br.getPage(apiBase + "&ct=" + Encoding.urlEncode(castSignature) + "&op=castmaster");
            downloadurl = selectHlsQualityUrl(HlsContainer.getHlsQualities(this.br));
        } catch (final InterruptedException ie) {
            throw ie;
        } catch (final Exception e) {
            logger.log(e);
            logger.info("Cast playlist failed -> Fallback to legacy playlist with fixed (encrypted) segment URLs");
            br.getHeaders().remove("X-XS-P");
            legacyPlaylist = loadLegacyEncryptedPlaylist(playerOrigin, fid, apiBase);
        }
        checkFFmpeg(link, "Download a HLS Stream");
        /*
         * Small workaround for stable.
         */
        final String hlsUrl = legacyPlaylist != null ? br.getURL() : downloadurl;
        dl = new HLSDownloader(link, br, hlsUrl, legacyPlaylist) {
            @Override
            public boolean isEncrypted() {
                return false;
            }

            @Override
            protected boolean isSupported(final M3U8Playlist m3u8) {
                return true;
            }
        };
        dl.startDownload();
    }

    /** Returns the download url of the quality preferred by the user or of the best quality if the preferred one is not available. */
    private String selectHlsQualityUrl(final List<HlsContainer> qualities) throws PluginException {
        if (qualities == null || qualities.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to find any stream qualities");
        }
        final int preferredQualityHeight = this.getUserPreferredQualityHeight();
        for (final HlsContainer quality : qualities) {
            if (quality.getHeight() == preferredQualityHeight) {
                return quality.getDownloadurl();
            }
        }
        return HlsContainer.findBestVideoByBandwidth(qualities).getDownloadurl();
    }

    /**
     * Legacy way: The playlist from xs1.php contains plain segment URLs (/cdn/down/ID/1080/10800.html) which return 404. The real segments
     * are AES-128 encrypted and located in "/cdn/down/ID/1080_enc/10800.html". The key is available via proto_api.php (op=key) and needs
     * the header X-XS-P. The IV is the segment index (default HLS behavior). Therefore we fix the playlist content and let HLSDownloader
     * handle the decryption.
     */
    private List<M3U8Playlist> loadLegacyEncryptedPlaylist(final String playerOrigin, final String fid, final String apiBase) throws Exception {
        br.getPage(playerOrigin + "/player/xs1.php?data=" + fid);
        br.getPage(selectHlsQualityUrl(HlsContainer.getHlsQualities(this.br)));
        String playlist = br.getRequest().getHtmlCode();
        if (!playlist.contains("#EXTM3U")) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Invalid legacy playlist");
        }
        playlist = playlist.replaceAll("(/cdn/down/[^/\\s]+/)(\\d+p?)/(\\2\\d+\\.html)", "$1$2_enc/$3");
        final String keyLine = "#EXT-X-KEY:METHOD=AES-128,URI=\"" + apiBase + "&op=key\"\n";
        playlist = playlist.replaceFirst("(#EXTM3U\\r?\\n)", "$1" + Matcher.quoteReplacement(keyLine));
        /* The key request is sent with the headers of this browser instance. */
        br.getHeaders().put("X-XS-P", "1");
        br.getRequest().setHtmlCode(playlist);
        return M3U8Playlist.parseM3U8(br);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return XtremestreamCoConfig.class;
    }

    private int getUserPreferredQualityHeight() {
        final Quality quality = PluginJsonConfig.get(XtremestreamCoConfig.class).getPreferredStreamQuality();
        switch (quality) {
        case Q360:
            return 360;
        case Q480:
            return 480;
        case Q720:
            return 720;
        case Q1080:
            return 1080;
        case Q2160:
            return 2160;
        default:
            /* E.g. BEST */
            return 2160;
        }
    }
}