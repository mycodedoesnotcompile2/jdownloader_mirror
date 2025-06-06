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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.storage.JSonStorage;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogSource;
import org.jdownloader.controlling.ffmpeg.FFmpegMetaData;
import org.jdownloader.controlling.ffmpeg.json.Stream;
import org.jdownloader.controlling.ffmpeg.json.StreamInfo;
import org.jdownloader.downloader.hls.HLSDownloader;
import org.jdownloader.downloader.hls.M3U8Playlist;
import org.jdownloader.downloader.hls.M3U8Playlist.M3U8Segment;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.config.SubConfiguration;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.http.requests.GetRequest;
import jd.http.requests.PostRequest;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.components.PluginJSonUtils;
import jd.plugins.download.raf.FileBytesMap;
import jd.utils.locale.JDL;

@HostPlugin(revision = "$Revision: 50515 $", interfaceVersion = 2, names = { "twitch.tv" }, urls = { "http://twitchdecrypted\\.tv/\\d+" })
public class TwitchTv extends PluginForHost {
    public TwitchTv(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://secure.twitch.tv/products/turbo_year/ticket");
        setConfigElements();
    }

    @Override
    public String getAGBLink() {
        return "http://www.twitch.tv/user/legal?page=terms_of_service";
    }

    @Override
    public String getLinkID(DownloadLink link) {
        return super.getLinkID(link);
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    @Override
    public boolean isSpeedLimited(DownloadLink link, Account account) {
        return false;
    }

    private final String        FASTLINKCHECK             = "FASTLINKCHECK";
    private final String        NOCHUNKS                  = "NOCHUNKS";
    private final static String CUSTOM_DATE_2             = "CUSTOM_DATE_2";
    private final static String CUSTOM_FILENAME_3         = "CUSTOM_FILENAME_3";
    private final static String CUSTOM_FILENAME_4         = "CUSTOM_FILENAME_4";
    private final static String PARTNUMBERFORMAT          = "PARTNUMBERFORMAT";
    private static final int    ACCOUNT_FREE_MAXDOWNLOADS = 20;
    private String              dllink                    = null;

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink downloadLink) throws Exception {
        br = new Browser();
        dllink = downloadLink.getStringProperty("plain_directlink", downloadLink.getStringProperty("m3u", null));
        if (dllink == null && isChatDownload(downloadLink)) {
            // incase the user updates the formatting in configs between decrypter and download.
            downloadLink.setName(getFormattedFilename(downloadLink));
            return AvailableStatus.TRUE;
        }
        if (downloadLink.getBooleanProperty("offline", false) || dllink == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (dllink.endsWith("m3u8")) {
            // whilst requestFileInformation isn't threaded, I'm calling it directly from decrypter as a setting method. We now want to
            // prevent more than one thread running, incase of issues from hoster
            synchronized (ctrlLock) {
                checkFFProbe(downloadLink, "File Checking a HLS Stream");
                if (downloadLink.getBooleanProperty("encrypted")) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Encrypted HLS is not supported");
                }
                if (Thread.currentThread() instanceof SingleDownloadController) {
                    // we can skip it here
                    return AvailableStatus.TRUE;
                }
                br.getHeaders().put("Accept", "*/*");
                br.getHeaders().put("X-Requested-With", "ShockwaveFlash/22.0.0.192");
                br.getHeaders().put("Referer", downloadLink.getContentUrl());
                final HLSDownloader downloader = new HLSDownloader(downloadLink, br, downloadLink.getStringProperty("m3u", null));
                StreamInfo streamInfo = applyMissingVideoStreamWorkaround(downloader, downloadLink);
                if (streamInfo == null) {
                    streamInfo = downloader.getProbe();
                }
                if (downloadLink.getBooleanProperty("encrypted")) {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Encrypted HLS is not supported");
                }
                if (streamInfo == null) {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
                estimateSize(downloader, downloadLink);
                String extension = ".m4a";
                for (Stream s : streamInfo.getStreams()) {
                    if ("video".equalsIgnoreCase(s.getCodec_type())) {
                        extension = ".mp4";
                        if (s.getHeight() > 0) {
                            downloadLink.setProperty("videoQuality", s.getHeight());
                        }
                        if (s.getCodec_name() != null) {
                            downloadLink.setProperty("videoCodec", s.getCodec_name());
                        }
                        if (s.getR_frame_rate() != null && !downloadLink.hasProperty("fps")) {
                            final String fps = new Regex(s.getR_frame_rate(), "(\\d+)/1").getMatch(0);
                            if (fps != null) {
                                downloadLink.setProperty("fps", Integer.parseInt(fps));
                            }
                        }
                    } else if ("audio".equalsIgnoreCase(s.getCodec_type())) {
                        if (s.getBit_rate() != null) {
                            downloadLink.setProperty("audioBitrate", Integer.parseInt(s.getBit_rate()) / 1024);
                        }
                        if (s.getCodec_name() != null) {
                            downloadLink.setProperty("audioCodec", s.getCodec_name());
                        }
                    }
                }
                downloadLink.setProperty("extension", extension);
                downloadLink.setName(getFormattedFilename(downloadLink));
                return AvailableStatus.TRUE;
            }
        } else {
            this.setBrowserExclusive();
            URLConnectionAdapter con = null;
            final Browser br2 = br.cloneBrowser();
            // In case the link redirects to the finallink
            br2.setFollowRedirects(true);
            try {
                con = br2.openGetConnection(dllink);
                if (!con.getContentType().contains("html")) {
                    downloadLink.setDownloadSize(con.getLongContentLength());
                } else {
                    throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
                }
            } finally {
                try {
                    con.disconnect();
                } catch (Throwable e) {
                }
            }
            final String formattedFilename = getFormattedFilename(downloadLink);
            downloadLink.setFinalFileName(formattedFilename);
            return AvailableStatus.TRUE;
        }
    }

    private boolean isChatDownload(DownloadLink downloadLink) {
        return downloadLink.getBooleanProperty(jd.plugins.hoster.TwitchTv.grabChatHistory, jd.plugins.hoster.TwitchTv.defaultGrabChatHistory);
    }

    private static String getVuid(final DownloadLink downloadLink) {
        final String result = new Regex(downloadLink.getLinkID(), "twitch:(\\d+):").getMatch(0);
        return result;
    }

    @Override
    public void handleFree(final DownloadLink downloadLink) throws Exception {
        requestFileInformation(downloadLink);
        if (dllink != null && dllink.endsWith("m3u8")) {
            doHLS(downloadLink);
        } else if (isChatDownload(downloadLink)) {
            doChatDownload(downloadLink);
        } else {
            doFree(downloadLink);
        }
    }

    private void doChatDownload(DownloadLink downloadLink) throws Exception {
        // I don't see the point in linkchecking these!
        dllink = "https://rechat.twitch.tv/rechat-messages?start=0&video_id=v" + getVuid(downloadLink);
        // remove all of log, and start from scratch. for instance twitter/admin deletes comment, the order then changes and you have messed
        // up history.
        br.setAllowedResponseCodes(400);
        br.getHeaders().put("Referer", downloadLink.getContainerUrl());
        br.getHeaders().put("Accept", "application/vnd.api+json");
        dl = jd.plugins.BrowserAdapter.openDownload(br, downloadLink, dllink, false, 1);
        if (dl.getConnection().getContentType().contains("html") || dl.getConnection().getContentType().contains("application/json")) {
            br.followConnection();
            final String tt = br.getRegex("is not between (\\d+)").getMatch(0);
            dllink = dllink.replace("start=0", "start=" + tt);
            if (dllink == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            } else {
                dl = jd.plugins.BrowserAdapter.openDownload(br, downloadLink, dllink, false, 1);
                if (dl.getConnection().getContentType().contains("html") || dl.getConnection().getContentType().contains("application/json")) {
                    br.followConnection();
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
            }
        }
        dl.startDownload();
    }

    private HLSDownloader getHLSDownloader(final DownloadLink downloadLink, Browser br, final String m3u8Url) throws Exception {
        final FFmpegMetaData ffMpegMetaData;
        final SubConfiguration config = getPluginConfig();
        if (config.getBooleanProperty("meta", true)) {
            ffMpegMetaData = new FFmpegMetaData();
            ffMpegMetaData.setTitle(downloadLink.getStringProperty("plainfilename", null));
            ffMpegMetaData.setArtist(downloadLink.getStringProperty("channel", null));
            final String originaldate = downloadLink.getStringProperty("originaldate", null);
            if (originaldate != null) {
                final String year = new Regex(originaldate, "^(\\d{4})").getMatch(0);
                if (year != null) {
                    final GregorianCalendar calendar = new GregorianCalendar();
                    calendar.set(Calendar.YEAR, Integer.parseInt(year));
                    ffMpegMetaData.setYear(calendar);
                }
            }
            ffMpegMetaData.setComment(downloadLink.getContentUrl());
        } else {
            ffMpegMetaData = null;
        }
        return new HLSDownloader(downloadLink, br, m3u8Url) {
            final boolean isTwitchOptimized = Boolean.TRUE.equals(config.getBooleanProperty("expspeed", false));

            @Override
            protected boolean isMapMetaDataEnabled() {
                return ffMpegMetaData != null && !ffMpegMetaData.isEmpty();
            }

            @Override
            protected FFmpegMetaData getFFmpegMetaData() {
                return ffMpegMetaData;
            }

            @Override
            protected boolean onSegmentConnectException(URLConnectionAdapter connection, IOException e, final FileBytesMap fileBytesMap, int retry, Map<String, Object> retryMap, LogSource logger) throws Exception {
                if (isTwitchOptimized && connection != null && connection.getResponseCode() == 400) {
                    config.setProperty("expspeed", false);
                }
                return super.onSegmentConnectException(connection, e, fileBytesMap, retry, retryMap, logger);
            }

            @Override
            protected List<M3U8Playlist> getM3U8Playlists() throws Exception {
                final List<M3U8Playlist> ret = super.getM3U8Playlists();
                for (final M3U8Playlist playList : ret) {
                    for (int index = playList.size() - 1; index >= 0; index--) {
                        final M3U8Segment segment = playList.getSegment(index);
                        if (segment != null && StringUtils.endsWithCaseInsensitive(segment.getUrl(), "end_offset=-1")) {
                            playList.removeSegment(index);
                        }
                    }
                }
                return ret;
            }

            /*
             * TODO: move logic to getM3U8Playlists
             */
            protected String optimizeM3U8Playlist(String m3u8Playlist) {
                if (m3u8Playlist != null && isTwitchOptimized) {
                    final StringBuilder sb = new StringBuilder();
                    long lastSegmentDuration = 0;
                    String lastSegment = null;
                    long lastSegmentStart = -1;
                    long lastSegmentEnd = -1;
                    long lastMergedSegmentDuration = 0;
                    final long maxSegmentSize = 10000000l;// server-side limit
                    for (final String line : Regex.getLines(m3u8Playlist)) {
                        if (line.matches("^https?://.+") || !line.trim().startsWith("#")) {
                            final String segment = new Regex(line, "^(.*?)(\\?|$)").getMatch(0);
                            final String segmentStart = new Regex(line, "\\?.*?start_offset=(-?\\d+)").getMatch(0);
                            final String segmentEnd = new Regex(line, "\\?.*?end_offset=(-?\\d+)").getMatch(0);
                            if ("-1".equals(segmentEnd)) {
                                continue;
                            }
                            if (lastSegment != null && !lastSegment.equals(segment) || segmentStart == null || segmentEnd == null || lastSegmentEnd != Long.parseLong(segmentStart) - 1 || Long.parseLong(segmentEnd) - lastSegmentStart > maxSegmentSize) {
                                if (lastSegment != null) {
                                    if (sb.length() > 0) {
                                        sb.append("\n");
                                    }
                                    sb.append("#EXTINF:");
                                    sb.append(M3U8Segment.toExtInfDuration(lastMergedSegmentDuration));
                                    lastMergedSegmentDuration = 0;
                                    sb.append(",\n");
                                    sb.append(lastSegment + "?start_offset=" + lastSegmentStart + "&end_offset=" + lastSegmentEnd);
                                    lastSegment = null;
                                }
                            }
                            if (segment != null && segmentStart != null && segmentEnd != null) {
                                if (lastSegment == null) {
                                    lastSegment = segment;
                                    lastSegmentStart = Long.parseLong(segmentStart);
                                    lastSegmentEnd = Long.parseLong(segmentEnd);
                                    lastMergedSegmentDuration = lastSegmentDuration;
                                } else {
                                    lastSegmentEnd = Long.parseLong(segmentEnd);
                                    lastMergedSegmentDuration += lastSegmentDuration;
                                }
                            } else {
                                if (sb.length() > 0) {
                                    sb.append("\n");
                                }
                                sb.append("#EXTINF:");
                                sb.append(M3U8Segment.toExtInfDuration(lastSegmentDuration));
                                lastSegmentDuration = 0;
                                sb.append(",\n");
                                sb.append(line);
                            }
                        } else {
                            if (line.startsWith("#EXT-X-ENDLIST")) {
                                if (lastSegment != null) {
                                    if (sb.length() > 0) {
                                        sb.append("\n");
                                    }
                                    sb.append("#EXTINF:");
                                    sb.append(M3U8Segment.toExtInfDuration(lastMergedSegmentDuration));
                                    lastMergedSegmentDuration = 0;
                                    sb.append(",\n");
                                    sb.append(lastSegment + "?start_offset=" + lastSegmentStart + "&end_offset=" + lastSegmentEnd);
                                    lastSegment = null;
                                }
                                if (sb.length() > 0) {
                                    sb.append("\n");
                                }
                                sb.append(line);
                            } else if (line.startsWith("#EXTINF:")) {
                                final String duration = new Regex(line, "#EXTINF:(\\d+(\\.\\d+)?)").getMatch(0);
                                if (duration != null) {
                                    if (duration.contains(".")) {
                                        lastSegmentDuration = Long.parseLong(duration.replace(".", ""));
                                    } else {
                                        lastSegmentDuration = Long.parseLong(duration) * 1000;
                                    }
                                }
                            } else {
                                if (sb.length() > 0) {
                                    sb.append("\n");
                                }
                                sb.append(line);
                            }
                        }
                    }
                    if (lastSegment != null) {
                        if (sb.length() > 0) {
                            sb.append("\n");
                        }
                        sb.append("#EXTINF:");
                        sb.append(M3U8Segment.toExtInfDuration(lastMergedSegmentDuration));
                        lastMergedSegmentDuration = 0;
                        sb.append(",\n");
                        sb.append(lastSegment + "?start_offset=" + lastSegmentStart + "&end_offset=" + lastSegmentEnd);
                        lastSegment = null;
                    }
                    return sb.toString();
                }
                return m3u8Playlist;
            };
        };
    }

    private void estimateSize(HLSDownloader dl, DownloadLink downloadLink) {
        if (!downloadLink.hasProperty(twitchEstimatedSize)) {
            downloadLink.setProperty(twitchEstimatedSize, Boolean.TRUE);
            final int hlsBandwidth = downloadLink.getIntegerProperty("hlsBandwidth", -1);
            if (hlsBandwidth > 0) {
                for (M3U8Playlist playList : dl.getPlayLists()) {
                    playList.setAverageBandwidth(hlsBandwidth);
                }
            }
            final long estimatedSize = dl.getEstimatedSize();
            if (downloadLink.getKnownDownloadSize() == -1) {
                downloadLink.setDownloadSize(estimatedSize);
            } else {
                downloadLink.setDownloadSize(Math.max(downloadLink.getKnownDownloadSize(), estimatedSize));
            }
        }
    }

    private StreamInfo applyMissingVideoStreamWorkaround(HLSDownloader dl, DownloadLink downloadLink) throws Exception {
        final Object apply = downloadLink.getProperty(applyMissingVideoStreamWorkaround);
        if (apply == null) {
            StreamInfo streamInfo = dl.getProbe();
            if (streamInfo != null) {
                if (streamInfo._getVideoStreams().size() == 0) {
                    if (dl.getPlayLists().size() == 1) {
                        logger.info("Apply workaround for first chunk missing video stream!");
                        final M3U8Segment removed = dl.getPlayLists().get(0).removeSegment(0);
                        streamInfo = dl.getProbe();
                        if (streamInfo != null) {
                            if (streamInfo._getVideoStreams().size() == 0) {
                                dl.getPlayLists().get(0).addSegment(0, removed);
                                logger.info("Workaround failed");
                            } else {
                                downloadLink.setProperty(applyMissingVideoStreamWorkaround, Boolean.TRUE);
                                logger.info("Workaround successfull");
                            }
                        }
                    }
                } else {
                    downloadLink.setProperty(applyMissingVideoStreamWorkaround, Boolean.FALSE);
                }
            }
            return streamInfo;
        } else if (Boolean.TRUE.equals(apply)) {
            logger.info("Apply workaround for first chunk missing video stream!");
            dl.getPlayLists().get(0).removeSegment(0);
        }
        return null;
    }

    private final void doHLS(final DownloadLink downloadLink) throws Exception {
        checkFFmpeg(downloadLink, "Download a HLS Stream");
        if (downloadLink.getBooleanProperty("encrypted")) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Encrypted HLS is not supported");
        }
        final HLSDownloader dl = getHLSDownloader(downloadLink, br, dllink);
        this.dl = dl;
        estimateSize(dl, downloadLink);
        applyMissingVideoStreamWorkaround(dl, downloadLink);
        dl.startDownload();
    }

    private void doFree(final DownloadLink downloadLink) throws Exception {
        int maxChunks = 0;
        if (downloadLink.getBooleanProperty(NOCHUNKS, false)) {
            maxChunks = 1;
        }
        dl = jd.plugins.BrowserAdapter.openDownload(br, downloadLink, dllink, true, maxChunks);
        if (dl.getConnection().getContentType().contains("html")) {
            br.followConnection();
            if (br.containsHTML(">416 Requested Range Not Satisfiable<")) {
                /* unknown error, we disable multiple chunks */
                if (downloadLink.getBooleanProperty(NOCHUNKS, false) == false) {
                    downloadLink.setProperty(NOCHUNKS, Boolean.valueOf(true));
                    throw new PluginException(LinkStatus.ERROR_RETRY);
                }
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error", 5 * 60 * 1000l);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        if (!this.dl.startDownload()) {
            try {
                if (dl.externalDownloadStop()) {
                    return;
                }
            } catch (final Throwable e) {
            }
            /* unknown error, we disable multiple chunks */
            if (downloadLink.getBooleanProperty(NOCHUNKS, false) == false) {
                downloadLink.setProperty(NOCHUNKS, Boolean.valueOf(true));
                throw new PluginException(LinkStatus.ERROR_RETRY);
            }
        }
    }

    @SuppressWarnings("deprecation")
    public static String getFormattedFilename(final DownloadLink downloadLink) throws ParseException {
        String videoName = downloadLink.getStringProperty("plainfilename", null);
        final SubConfiguration cfg = SubConfiguration.getConfig("twitch.tv");
        String formattedFilename = downloadLink.getStringProperty("m3u", null) != null ? cfg.getStringProperty(CUSTOM_FILENAME_4, defaultCustomFilenameHls) : cfg.getStringProperty(CUSTOM_FILENAME_3, defaultCustomFilenameWeb);
        if (formattedFilename == null || formattedFilename.equals("")) {
            formattedFilename = downloadLink.getStringProperty("m3u", null) != null ? defaultCustomFilenameHls : defaultCustomFilenameWeb;
        }
        if (!formattedFilename.contains("*videoname") || !formattedFilename.contains("*ext*")) {
            formattedFilename = downloadLink.getStringProperty("m3u", null) != null ? defaultCustomFilenameHls : defaultCustomFilenameWeb;
        }
        String partnumberformat = cfg.getStringProperty(PARTNUMBERFORMAT);
        if (partnumberformat == null || partnumberformat.equals("")) {
            partnumberformat = "00";
        }
        final DecimalFormat df = new DecimalFormat(partnumberformat);
        final String date = downloadLink.getStringProperty("originaldate", null);
        final String channelName = downloadLink.getStringProperty("channel", "");
        final int partNumber = downloadLink.getIntegerProperty("partnumber", -1);
        final String quality = downloadLink.getStringProperty("quality", "");
        final int videoQuality = downloadLink.getIntegerProperty("videoQuality", -1);
        final String videoCodec = downloadLink.getStringProperty("videoCodec", "");
        final int audioBitrate = downloadLink.getIntegerProperty("audioBitrate", -1);
        final int fps = downloadLink.getIntegerProperty("fps", -1);
        final String audioCodec = downloadLink.getStringProperty("audioCodec", "");
        final String extension = downloadLink.getStringProperty("extension", ".flv");
        String formattedDate = null;
        if (date != null) {
            final String userDefinedDateFormat = cfg.getStringProperty(CUSTOM_DATE_2, "dd.MM.yyyy_HH-mm-ss");
            final String[] dateStuff = date.split("T");
            final String input = dateStuff[0] + ":" + dateStuff[1].replace("Z", "GMT");
            SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd:HH:mm:ssZ");
            Date dateStr = formatter.parse(input);
            formattedDate = formatter.format(dateStr);
            Date theDate = formatter.parse(formattedDate);
            formatter = new SimpleDateFormat(userDefinedDateFormat);
            formattedDate = formatter.format(theDate);
        }
        // parse for user characters outside of wild card, only used as separators.
        final String p = new Regex(formattedFilename, "\\*?(([^\\*]*)?\\*partnumber\\*([^\\*]*)?)").getMatch(0);
        if (partNumber == -1) {
            if (p != null) {
                formattedFilename = formattedFilename.replace(p, "");
            } else {
                formattedFilename = formattedFilename.replace("*partnumber*", "");
            }
        } else {
            formattedFilename = formattedFilename.replace("*partnumber*", df.format(partNumber));
        }
        formattedFilename = formattedFilename.replace("*quality*", quality);
        formattedFilename = formattedFilename.replace("*channelname*", channelName);
        final String videoQualityString;
        if (videoQuality != -1) {
            if (StringUtils.containsIgnoreCase(downloadLink.getStringProperty("m3u"), "/chunked")) {
                videoQualityString = "chunked_" + videoQuality + "p";
            } else {
                videoQualityString = videoQuality + "p";
            }
        } else {
            videoQualityString = "";
        }
        formattedFilename = formattedFilename.replace("*videoQuality*", videoQualityString);
        formattedFilename = formattedFilename.replace("*videoCodec*", videoCodec);
        formattedFilename = formattedFilename.replace("*fps*", fps == -1 ? "" : String.valueOf(fps));
        formattedFilename = formattedFilename.replace("*audioBitrate*", audioBitrate == -1 ? "" : audioBitrate + "kbits");
        formattedFilename = formattedFilename.replace("*audioCodec*", audioCodec);
        if (formattedDate != null) {
            formattedFilename = formattedFilename.replace("*date*", formattedDate);
        } else {
            formattedFilename = formattedFilename.replace("*date*", "");
        }
        formattedFilename = formattedFilename.replace("*ext*", extension);
        // Insert filename at the end to prevent errors with tags
        formattedFilename = formattedFilename.replace("*videoname*", videoName);
        final String vuid = getVuid(downloadLink);
        if (vuid != null) {
            formattedFilename = formattedFilename.replace("*vuid*", vuid);
        }
        return formattedFilename;
    }

    private static Object                  ctrlLock = new Object();
    private static AtomicReference<String> CLIENTID = new AtomicReference<String>();

    public static String getClientID(final Browser currentBrowser, Plugin plugin) throws Exception {
        synchronized (CLIENTID) {
            String clientID = CLIENTID.get();
            if (!StringUtils.isEmpty(clientID)) {
                return clientID;
            } else {
                if (currentBrowser != null) {
                    clientID = currentBrowser.getRegex("\"Client-ID\"\\s*:\\s*\"([^\"]+)\"").getMatch(0);
                }
                if (StringUtils.isEmpty(clientID)) {
                    final Browser br;
                    if (currentBrowser != null) {
                        br = currentBrowser.cloneBrowser();
                    } else {
                        br = new Browser();
                    }
                    br.setFollowRedirects(true);
                    br.getPage("https://www.twitch.tv/");
                    clientID = br.getRegex("\"Client-ID\"\\s*:\\s*\"([^\"]+)\"").getMatch(0);
                }
                if (StringUtils.isEmpty(clientID)) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                } else {
                    CLIENTID.set(clientID);
                    return clientID;
                }
            }
        }
    }

    public void login(Browser br, final Account account, final boolean force) throws Exception {
        synchronized (account) {
            try {
                if (true) {
                    // 23.11.2020
                    // https://api.twitch.tv/kraken/user no longer exists
                    throw new PluginException(LinkStatus.ERROR_PREMIUM, "Account support is currently broken!", PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE);
                }
                br.addAllowedResponseCodes(400);
                // Load cookies
                br.setCookiesExclusive(true);
                Cookies cookies = account.loadCookies("");
                String access_token = account.getStringProperty("access_token", null);
                if (cookies != null && access_token != null) {
                    br.setCookies(getHost(), cookies);
                    br.getPage("https://www.twitch.tv/");
                    final GetRequest request = br.createGetRequest("https://api.twitch.tv/kraken/user");
                    request.getHeaders().put("Authorization", "OAuth " + access_token);
                    br.getPage(request);
                    if (request.getHttpConnection().getResponseCode() == 200) {
                        account.saveCookies(br.getCookies(getHost()), "");
                        return;
                    }
                }
                br.setFollowRedirects(true);
                br.getPage("https://www.twitch.tv/");
                final Map<String, String> map = new HashMap<String, String>();
                map.put("username", account.getUser());
                map.put("password", account.getPass());
                map.put("client_id", getClientID(br, this));
                final PostRequest request = br.createJSonPostRequest("https://passport.twitch.tv/login", JSonStorage.toString(map));
                br.getPage(request);
                if (request.getHttpConnection().getResponseCode() == 400) {
                    final String error_code = PluginJSonUtils.getJsonValue(request.getHtmlCode(), "error_code");
                    if ("1000".equals(error_code)) {
                        // captcha, not yet supported
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    } else if ("1014".equals(error_code)) {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "user does not exist", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    } else if ("3001".equals(error_code)) {
                        throw new PluginException(LinkStatus.ERROR_PREMIUM, "user credentials incorrect", PluginException.VALUE_ID_PREMIUM_DISABLE);
                    } else {
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    }
                }
                final String redirect = PluginJSonUtils.getJsonValue(request.getHtmlCode(), "redirect_path");
                access_token = PluginJSonUtils.getJsonValue(request.getHtmlCode(), "access_token");
                if (access_token == null || redirect == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                } else {
                    account.setProperty("access_token", access_token);
                    br.getPage(redirect);
                    br.setCookie(getHost(), "api_token", access_token);
                    account.saveCookies(br.getCookies(getHost()), "");
                }
            } catch (final PluginException e) {
                if (e.getLinkStatus() == LinkStatus.ERROR_PREMIUM) {
                    account.clearCookies("");
                }
                throw e;
            }
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(br, account, true);
        ai.setUnlimitedTraffic();
        account.setType(AccountType.FREE);
        account.setMaxSimultanDownloads(ACCOUNT_FREE_MAXDOWNLOADS);
        ai.setStatus("Free Account");
        account.setValid(true);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink downloadLink, final Account account) throws Exception {
        requestFileInformation(downloadLink);
        if (dllink != null && dllink.endsWith("m3u8")) {
            doHLS(downloadLink);
        } else if (isChatDownload(downloadLink)) {
            doChatDownload(downloadLink);
        } else {
            doFree(downloadLink);
        }
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return ACCOUNT_FREE_MAXDOWNLOADS;
    }

    @Override
    public String getDescription() {
        return "JDownloader's twitch.tv plugin helps downloading videoclips. JDownloader provides settings for the filenames.";
    }

    private final static String defaultCustomFilenameWeb = "*partnumber**videoname*_*quality*_*fps**ext*";
    private final static String defaultCustomFilenameHls = "*partnumber* - *videoname* -*videoQuality*_*videoCodec*-*audioBitrate*_*audioCodec*_*fps**ext*";
    public final static String  grabChatHistory          = "grabChatHistory";
    public final static boolean defaultGrabChatHistory   = false;

    private void setConfigElements() {
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), FASTLINKCHECK, JDL.L("plugins.hoster.twitchtv.fastlinkcheck", "Activate fast linkcheck (filesize won't be shown in linkgrabber)?")).setDefaultValue(false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, "Customize the filename properties"));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), CUSTOM_DATE_2, JDL.L("plugins.hoster.twitchtv.customdate", "Define how the date should look:")).setDefaultValue("dd.MM.yyyy_hh-mm-ss"));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), PARTNUMBERFORMAT, JDL.L("plugins.hoster.twitchtv.custompartnumber", "Define how the partnumbers should look:")).setDefaultValue("00"));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), CUSTOM_FILENAME_3, JDL.L("plugins.hoster.twitchtv.customfilename1", "Define how standard filenames should look:")).setDefaultValue(defaultCustomFilenameWeb));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_TEXTFIELD, getPluginConfig(), CUSTOM_FILENAME_4, JDL.L("plugins.hoster.twitchtv.customfilename2", "Define how vod /v/ filenames should look:")).setDefaultValue(defaultCustomFilenameHls));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        final StringBuilder sb = new StringBuilder();
        sb.append("Explanation of the available tags: (shared)\r\n");
        sb.append("*channelname* = name of the channel/uploader\r\n");
        sb.append("*date* = date when the video was posted - appears in the user-defined format above\r\n");
        sb.append("*videoname* = name of the video without extension\r\n");
        sb.append("*partnumber* = number of the part of the video - if there is only 1 part, it's 1\r\n");
        sb.append("*ext* = the extension of the file, in this case usually '.flv'\r\n");
        sb.append("\r\nThis tag is only used for standard downloads\r\n");
        sb.append("*quality* = the quality of the file, e.g. '720p'. (used for older formats, not present new /v/ videos)\r\n");
        sb.append("\r\nThese following tags are only used for HLS /v/ urls\r\n");
        sb.append("*videoQuality* = the frame size/quality, e.g. '720p'\r\n");
        sb.append("*videoCodec* = video codec used, e.g. 'h264'\r\n");
        sb.append("*audioBitrate* = audio bitrate, e.g. '128kbits'\r\n");
        sb.append("*audioCodec* = audio encoding type, e.g. 'aac'\r\n");
        sb.append("*fps* = frames per second, e.g. '30'\r\n");
        sb.append("*vuid* = video unquie identifier");
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, sb.toString()));
        // best shite for hls
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_SEPARATOR));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_LABEL, "Quality selection, this is for HLS /v/ links only"));
        final ConfigEntry cfgbest = new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "useBest", JDL.L("plugins.hoster.twitchtv.usebest", "Only grab best video within selection below?, Else will return available videos within your selected above")).setDefaultValue(true);
        getConfig().addEntry(cfgbest);
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "q1080p", JDL.L("plugins.hoster.twitchtv.check1080p", "Grab 1080?")).setDefaultValue(true));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "q720p", JDL.L("plugins.hoster.twitchtv.check720p", "Grab 720p?")).setDefaultValue(true));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "q480p", JDL.L("plugins.hoster.twitchtv.check480p", "Grab 480p?")).setDefaultValue(true));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "q360p", JDL.L("plugins.hoster.twitchtv.check360p", "Grab 360p?")).setDefaultValue(true));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "q240p", JDL.L("plugins.hoster.twitchtv.check240p", "Grab 240p?")).setDefaultValue(true));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_COMBOBOX_INDEX, getPluginConfig(), SELECTED_PREFERRED_FPS, AVAILABLE_FPS_SETTING, "Preferred FPS:").setDefaultValue(0));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "avoidChunked", JDL.L("plugins.hoster.twitchtv.avoidChunked", "Avoid source quality (chunked)?")).setDefaultValue(true));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "expspeed", JDL.L("plugins.hoster.twitchtv.expspeed", "Increase download speed (experimental)? ")).setDefaultValue(false));
        getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), "meta", JDL.L("plugins.hoster.twitchtv.meta", "Set meta data? ")).setDefaultValue(true));
        // getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, getPluginConfig(), grabChatHistory,
        // JDL.L("plugins.hoster.twitchtv.grabChatHistory", "Download given videos chat
        // history.")).setDefaultValue(defaultGrabChatHistory));
    }

    private final String[]      AVAILABLE_FPS_SETTING  = new String[] { "30 & 60 FPS", "30 FPS", "60 FPS" };
    private static final String SELECTED_PREFERRED_FPS = "SELECTED_PREFERRED_FPS";

    @Override
    public void reset() {
    }

    private final String twitchEstimatedSize               = "twitchEstimatedSize";
    private final String applyMissingVideoStreamWorkaround = "applyMissingVideoStreamWorkaround";

    @Override
    public void resetDownloadlink(DownloadLink link) {
        if (link == null) {
            return;
        }
        link.removeProperty(twitchEstimatedSize);
        link.removeProperty(applyMissingVideoStreamWorkaround);
    }

    @Override
    public boolean allowHandle(final DownloadLink link, final PluginForHost plugin) {
        /* No not allow multihost plugins to handle items from this plugin. */
        return link.getHost().equalsIgnoreCase(plugin.getHost());
    }

    @Override
    public void resetPluginGlobals() {
    }
}