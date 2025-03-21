//    jDownloader - Downloadmanager
//    Copyright (C) 2014  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.Cookies;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.utils.StringUtils;
import org.jdownloader.controlling.ffmpeg.json.Stream;
import org.jdownloader.controlling.ffmpeg.json.StreamInfo;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.downloader.hls.M3U8Playlist;
import org.jdownloader.plugins.components.config.GenericM3u8DecrypterConfig;
import org.jdownloader.plugins.components.hls.HlsContainer.StreamCodec;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

@HostPlugin(revision = "$Revision: 49336 $", interfaceVersion = 3, names = { "M3u8" }, urls = { "m3u8s?://.+" })
public class GenericM3u8 extends PluginForHost {
    public static final String PRESET_NAME_PROPERTY               = "preSetName";
    public static final String DEPRECATED_NAME_PROPERTY           = "deprecatedName";
    public static final String PROPERTY_HEIGHT                    = "height";
    public static final String PROPERTY_WIDTH                     = "width";
    public static final String PROPERTY_BANDWIDTH                 = "hlsBandwidth";
    public static final String PROPERTY_BANDWIDTH_AVERAGE         = "hlsBandwidthAverage";
    public static final String PROPERTY_FRAME_RATE                = "framerate";
    public static final String PROPERTY_M3U8_CODECS               = "m3u8_codecs";
    public static final String PROPERTY_FFMPEG_CODECS             = "ffmpeg_codecs";
    public static final String PROPERTY_M3U8_NAME                 = "m3u8_name";
    public static final String PROPERTY_DURATION_ESTIMATED_MILLIS = "duration_estimated_millis";
    public static final String PROPERTY_CUSTOM_HOST               = "PROPERTY_CUSTOM_HOST";

    public GenericM3u8(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getHost(final DownloadLink link, final Account account, boolean includeSubdomain) {
        if (link != null) {
            final String customHost = link.getStringProperty(PROPERTY_CUSTOM_HOST, null);
            if (StringUtils.isNotEmpty(customHost)) {
                return customHost;
            } else {
                return Browser.getHost(link.getPluginPatternMatcher(), includeSubdomain);
            }
        } else {
            return super.getHost(link, account, includeSubdomain);
        }
    }

    @Override
    public boolean isSpeedLimited(final DownloadLink link, final Account account) {
        return false;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.GENERIC };
    }

    @Override
    public String getAGBLink() {
        return "";
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    @Override
    public void correctDownloadLink(final DownloadLink link) throws Exception {
        final String newurl = getContentURL(link);
        if (!link.getPluginPatternMatcher().equals(newurl)) {
            link.setPluginPatternMatcher(newurl);
        }
    }

    private String getContentURL(final DownloadLink link) {
        if (link.getPluginPatternMatcher().startsWith("m3u8")) {
            return "http" + link.getPluginPatternMatcher().substring(4);
        } else {
            return link.getPluginPatternMatcher();
        }
    }

    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        return requestFileInformation(link, false);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final boolean isDownload) throws Exception {
        checkFFProbe(link, "Check a HLS Stream");
        this.setBrowserExclusive();
        final String cookiesString = link.getStringProperty("cookies");
        final String downloadurl = getContentURL(link);
        if (cookiesString != null) {
            final String host = Browser.getHost(downloadurl);
            br.setCookies(host, Cookies.parseCookies(cookiesString, host, null));
        }
        final String referer = getReferer(link);
        if (referer != null) {
            br.getPage(referer);
            br.followRedirect();
        }
        HLSDownloader downloader = null;
        try {
            downloader = new HLSDownloader(link, br, downloadurl);
            final StreamInfo streamInfo = downloader.getProbe();
            if (downloader.isEncrypted()) {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Encrypted HLS(" + downloader.getEncryptionMethod() + ") is not supported!");
            } else if (streamInfo == null) {
                /* Invalid/broken stream */
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final int hlsBandwidth = link.getIntegerProperty(PROPERTY_BANDWIDTH, 0);
            if (hlsBandwidth > 0) {
                for (M3U8Playlist playList : downloader.getPlayLists()) {
                    playList.setAverageBandwidth(hlsBandwidth);
                }
            }
            final long estimatedSize = downloader.getEstimatedSize();
            if (estimatedSize > 0) {
                link.setDownloadSize(estimatedSize);
            }
            StringBuilder ffmpegCodecs = new StringBuilder();
            for (final Stream s : streamInfo.getStreams()) {
                if (ffmpegCodecs.length() > 0) {
                    ffmpegCodecs.append(",");
                }
                ffmpegCodecs.append(s.getCodec_name()).append("(").append(s.getCodec_tag_string()).append(")");
                if ("video".equalsIgnoreCase(s.getCodec_type())) {
                    link.setProperty(PROPERTY_HEIGHT, s.getHeight());
                    link.setProperty(PROPERTY_WIDTH, s.getWidth());
                } else if ("audio".equalsIgnoreCase(s.getCodec_type())) {
                }
            }
            final long estimatedDurationMillis = M3U8Playlist.getEstimatedDuration(downloader.getPlayLists());
            if (estimatedDurationMillis > 0) {
                link.setProperty(PROPERTY_DURATION_ESTIMATED_MILLIS, estimatedDurationMillis);
            }
            if (ffmpegCodecs.length() > 0) {
                link.setProperty(PROPERTY_FFMPEG_CODECS, ffmpegCodecs.toString());
            }
            setFilename(this, link, true);
            if (isDownload) {
                this.dl = downloader;
            } else {
                this.dl = null;
            }
        } finally {
            if (downloader != null && this.dl == null) {
                downloader.close();
            }
        }
        return AvailableStatus.TRUE;
    }

    /** Wrapper function for backward compatibility. */
    private String getReferer(final DownloadLink link) {
        return link.getStringProperty("Referer", link.getReferrerUrl());
    }

    public static void setFilename(Plugin plugin, final DownloadLink link, final boolean setFinalFilename) throws MalformedURLException {
        if (link.getFinalFileName() != null) {
            /**
             * No not modify filename once final name has been set. </br> This e.g. allows other plugins/crawlers to set desired filenames
             * telling this plugin not to use the default filenames down below.
             */
            return;
        }
        final int videoHeight = link.getIntegerProperty(PROPERTY_HEIGHT, 0);
        final int bandwidth = link.getIntegerProperty(PROPERTY_BANDWIDTH, 0);
        /* 2024-02-16: Do not touch this "DEPRECATED_NAME_PROPERTY" handling for now! */
        String name = link.getStringProperty(PRESET_NAME_PROPERTY, link.getStringProperty(DEPRECATED_NAME_PROPERTY));
        if (name == null) {
            name = link.isNameSet() ? link.getName() : getFileNameFromURL(new URL(link.getPluginPatternMatcher().replaceFirst("(?i)^m3u8s?", "https://")));
            /* .m3u8 is not a valid file extension and we don't want to have this in our filename */
            name = name.replaceFirst("(?i)\\.m3u8$", "");
            /* store name as property to avoid name duplication issue */
            link.setProperty(DEPRECATED_NAME_PROPERTY, name);
        }
        String assumedFileExtension = null;
        final String codecsString = link.getStringProperty(PROPERTY_M3U8_CODECS, link.getStringProperty(PROPERTY_FFMPEG_CODECS, null));
        String audioq = null;
        String videoq = null;
        boolean hasVideoCodec = false;
        boolean hasAudioCodec = false;
        if (codecsString != null) {
            final List<StreamCodec> streamCodecs = StreamCodec.parse(codecsString);
            if (streamCodecs != null) {
                for (StreamCodec streamCodec : streamCodecs) {
                    switch (streamCodec.getCodec().getType()) {
                    case VIDEO:
                        hasVideoCodec = true;
                        /* Possibly mixed audio/video --> Prefer video container file extension */
                        assumedFileExtension = streamCodec.getCodec().getDefaultExtension();
                        break;
                    case AUDIO:
                        hasAudioCodec = true;
                        if (audioq == null) {
                            audioq = streamCodec.getCodec().getCodecName();
                        }
                        if (assumedFileExtension == null) {
                            assumedFileExtension = streamCodec.getCodec().getDefaultExtension();
                        }
                        break;
                    case UNKNOWN:
                        break;
                    }
                }
            }
        }
        if (videoHeight > 0) {
            if (!hasVideoCodec) {
                plugin.getLogger().warning("no videoCodec detected but has videoHeight:" + videoHeight);
            }
            videoq = videoHeight + "p";
        }
        if (assumedFileExtension == null) {
            /* Fallback */
            if (videoHeight > 0) {
                assumedFileExtension = "mp4";
            } else {
                assumedFileExtension = "m4a";
            }
        }
        if (videoq != null && audioq != null) {
            name += " (" + videoq + "_" + audioq + ")";
        } else if (videoq != null) {
            name += " (" + videoq + ")";
        } else if (audioq != null) {
            name += " (" + audioq + ")";
        }
        if (bandwidth > 0 && ((videoq == null && audioq == null) || PluginJsonConfig.get(GenericM3u8DecrypterConfig.class).isAddBandwidthValueToFilenames())) {
            name += "_bw_" + bandwidth;
        }
        name = plugin.applyFilenameExtension(name, "." + assumedFileExtension);
        if (setFinalFilename) {
            link.setFinalFileName(name);
        } else {
            link.setName(name);
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        requestFileInformation(link, true);
        if (this.dl == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            checkFFmpeg(link, "Download a HLS Stream");
            dl.startDownload();
        }
    }

    /** Converts given URL into an URL which this plugin can handle. */
    public static String createURLForThisPlugin(final String url) {
        return url == null ? null : url.replaceFirst("^(?i)http(s?://)", "m3u8$1");
    }

    @Override
    public boolean hasCaptcha(final DownloadLink link, final Account acc) {
        /* This is a generic plugin. Captchas are never required for direct HLS downloads. */
        return false;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }

    @Override
    public void resetPluginGlobals() {
    }

    @Override
    public Boolean siteTesterDisabled() {
        return Boolean.TRUE;
    }
}