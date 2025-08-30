package org.jdownloader.downloader.hls;

import java.awt.Color;
import java.awt.Image;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PushbackInputStream;
import java.net.MalformedURLException;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import javax.imageio.ImageIO;

import jd.controlling.downloadcontroller.DiskSpaceReservation;
import jd.controlling.downloadcontroller.ExceptionRunnable;
import jd.controlling.downloadcontroller.FileIsLockedException;
import jd.controlling.downloadcontroller.ManagedThrottledConnectionHandler;
import jd.http.Browser;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.nutils.Formatter;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.download.DownloadInterface;
import jd.plugins.download.DownloadLinkDownloadable;
import jd.plugins.download.Downloadable;
import jd.plugins.download.raf.FileBytesMap;

import org.appwork.exceptions.WTFException;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.scheduler.DelayedRunnable;
import org.appwork.storage.SimpleMapper;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.net.CountingInputStream;
import org.appwork.utils.net.EmptyInputStream;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.LimitedInputStream;
import org.appwork.utils.net.NoClosingInputStream;
import org.appwork.utils.net.SkippingLimitedOutputStream;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpserver.HttpServer;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.net.throttledconnection.MeteredThrottledInputStream;
import org.appwork.utils.net.throttledconnection.ThrottledConnection;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.speedmeter.AverageSpeedMeter;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.controlling.ffmpeg.AbstractFFmpegBinary;
import org.jdownloader.controlling.ffmpeg.FFMpegException;
import org.jdownloader.controlling.ffmpeg.FFMpegProgress;
import org.jdownloader.controlling.ffmpeg.FFmpeg;
import org.jdownloader.controlling.ffmpeg.FFmpegMetaData;
import org.jdownloader.controlling.ffmpeg.FFmpegSetup;
import org.jdownloader.controlling.ffmpeg.FFprobe;
import org.jdownloader.controlling.ffmpeg.json.Stream;
import org.jdownloader.controlling.ffmpeg.json.StreamInfo;
import org.jdownloader.downloader.hls.M3U8Playlist.M3U8Segment;
import org.jdownloader.downloader.hls.M3U8Playlist.M3U8Segment.X_KEY_METHOD;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.downloads.columns.ETAColumn;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.DownloadPluginProgress;
import org.jdownloader.plugins.SkipReason;
import org.jdownloader.plugins.SkipReasonException;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.translate._JDT;

//http://tools.ietf.org/html/draft-pantos-http-live-streaming-13
public class HLSDownloader extends DownloadInterface {
    private class PartFile {
        private final File          file;
        private final int           index;
        private final AtomicBoolean downloadFlag = new AtomicBoolean(false);
        private final AtomicBoolean concatFlag   = new AtomicBoolean(false);

        private PartFile(int index, File file) {
            this.index = index;
            this.file = file;
        }
    }

    public static enum CONCATSOURCE {
        HTTP,
        FILE
    }

    private final AtomicLong                                 bytesWritten         = new AtomicLong(0);
    private DownloadLinkDownloadable                         downloadable;
    private DownloadLink                                     link;
    private long                                             startTimeStamp       = -1;
    private final LogInterface                               logger               = initLogger();
    private URLConnectionAdapter                             currentConnection;
    private ManagedThrottledConnectionHandler                connectionHandler;
    private File                                             outputCompleteFile;
    private PluginException                                  caughtPluginException;
    private String                                           m3uUrl;
    private String                                           persistentParameters;
    private HttpServer                                       server;
    private Browser                                          sourceBrowser;
    private long                                             processID;
    private List<M3U8Playlist>                               m3u8Playlists;
    private final List<PartFile>                             outputPartFiles      = new ArrayList<PartFile>();
    private volatile Map<String, File>                       fileMap              = new HashMap<String, File>();
    private final AtomicInteger                              currentPlayListIndex = new AtomicInteger(0);
    private final HashMap<String, SecretKeySpec>             aes128Keys           = new HashMap<String, SecretKeySpec>();
    private final WeakHashMap<M3U8Segment, M3U8SEGMENTSTATE> m3u8SegmentsStates   = new WeakHashMap<M3U8Playlist.M3U8Segment, M3U8SEGMENTSTATE>();
    private volatile Thread                                  ffmpegThread;
    private final Map<Integer, Map<String, Object>>          retryMap             = new HashMap<Integer, Map<String, Object>>();

    public CONCATSOURCE getConcatSource() {
        return CONCATSOURCE.HTTP;
    }

    public int getCurrentPlayListIndex() {
        return currentPlayListIndex.get();
    }

    public M3U8Playlist getCurrentPlayList() {
        return getPlayLists().get(getCurrentPlayListIndex());
    }

    public List<M3U8Playlist> getPlayLists() {
        return m3u8Playlists;
    }

    protected void init(final DownloadLink link, Browser br, String m3uUrl, final String persistantParameters, final List<M3U8Playlist> list) throws Exception {
        setPersistentParameters(persistantParameters);
        this.m3uUrl = Request.getLocation(m3uUrl, br.getRequest());
        this.sourceBrowser = br.cloneBrowser();
        this.link = link;
        connectionHandler = new ManagedThrottledConnectionHandler() {
            @Override
            public void addThrottledConnection(ThrottledConnection con) {
                final long transfered = con.transfered();
                if (transfered > 0) {
                    traffic.addAndGet(-transfered);
                    if (connections.addIfAbsent(con)) {
                        con.setHandler(this);
                    } else {
                        traffic.addAndGet(transfered);
                    }
                } else {
                    super.addThrottledConnection(con);
                }
            }
        };
        downloadable = new DownloadLinkDownloadable(link) {
            @Override
            public boolean isResumable() {
                return link.getBooleanProperty("RESUME", true);
            }

            @Override
            public void setResumeable(boolean value) {
                link.setProperty("RESUME", value);
                super.setResumeable(value);
            }
        };
        if (list != null) {
            this.m3u8Playlists = list;
        } else {
            this.m3u8Playlists = getM3U8Playlists();
        }
        if (m3u8Playlists == null || m3u8Playlists.size() == 0) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    @Deprecated
    public HLSDownloader(final DownloadLink link, Browser br, String m3uUrl, final String persistentParameters) throws Exception {
        init(link, br, m3uUrl, persistentParameters, null);
    }

    public HLSDownloader(final DownloadLink link, final Browser br, final String m3uUrl) throws Exception {
        init(link, br, m3uUrl, null, null);
    }

    public HLSDownloader(final DownloadLink link, final Browser br, final String m3uUrl, final List<M3U8Playlist> list) throws Exception {
        init(link, br, m3uUrl, null, list);
    }

    @Deprecated
    // bad as it might be too late to set this after constructor as getM3U8Playlists is already called
    protected void setPersistentParameters(final String persistentParameters) {
        if (StringUtils.isEmpty(persistentParameters)) {
            this.persistentParameters = null;
        } else {
            final String parameter = persistentParameters.trim();
            if (parameter.startsWith("?") || parameter.startsWith("&")) {
                this.persistentParameters = parameter.substring(1);
            } else {
                this.persistentParameters = parameter;
            }
        }
    }

    protected String buildDownloadUrl(final String url) {
        final String persistentParameters = this.persistentParameters;
        if (persistentParameters == null) {
            return url;
        } else {
            try {
                return URLHelper.parseLocation(new URL(url), "&" + persistentParameters);
            } catch (MalformedURLException e) {
                if (url.contains("?")) {
                    return url + "&" + persistentParameters;
                } else {
                    return url + "?" + persistentParameters;
                }
            }
        }
    }

    public long getEstimatedSize() {
        return M3U8Playlist.getEstimatedSize(getPlayLists());
    }

    public boolean isEncrypted() {
        return !isEncryptionSupported(getEncryptionMethod());
    }

    public X_KEY_METHOD getEncryptionMethod() {
        if (m3u8Playlists != null) {
            for (final M3U8Playlist playlist : m3u8Playlists) {
                final X_KEY_METHOD encryptionMethod = playlist.getEncryptionMethod();
                if (!X_KEY_METHOD.NONE.equals(encryptionMethod)) {
                    return encryptionMethod;
                }
            }
        }
        return X_KEY_METHOD.NONE;
    }

    private boolean isEncryptionSupported(X_KEY_METHOD method) {
        switch (method) {
        case AES_128:
            return DebugMode.TRUE_IN_IDE_ELSE_FALSE;
        case SAMPLE_AES:
        default:
            return false;
        case NONE:
            return true;
        }
    }

    protected boolean isSupported(final M3U8Playlist m3u8) {
        if (m3u8 != null && !isEncryptionSupported(m3u8.getEncryptionMethod())) {
            return false;
        } else if (isEncrypted()) {
            return false;
        } else {
            return true;
        }
    }

    protected LogInterface initLogger() {
        final Plugin plg = Plugin.getCurrentActivePlugin();
        LogInterface log = null;
        if (plg != null) {
            log = plg.getLogger();
        }
        if (log == null) {
            log = LogController.CL();
        }
        return log;
    }

    protected void terminate() {
        if (terminated.getAndSet(true) == false) {
            final Thread thread = ffmpegThread;
            if (thread != null) {
                logger.log(new Exception("Interrupt ffmpegThread:" + thread));
                thread.interrupt();
            }
            if (!externalDownloadStop()) {
                logger.log(new Exception("A critical Downloaderror occured. Terminate..."));
            }
        }
    }

    public StreamInfo getProbe() throws Exception {
        return getProbe(0);
    }

    public StreamInfo getProbe(int index) throws Exception {
        try {
            if (index > getM3U8Playlists().size() - 1) {
                throw new IllegalArgumentException("Index " + index + " > m3u8 playlist size " + getM3U8Playlists().size());
            }
            currentPlayListIndex.set(index);
            final FFprobe ffprobe = new FFprobe() {
                @Override
                public LogInterface getLogger() {
                    return HLSDownloader.this.logger;
                }

                @Override
                public String runCommand(FFMpegProgress progress, List<String> commandLine) throws IOException, InterruptedException, FFMpegException {
                    ffmpegThread = Thread.currentThread();
                    try {
                        return super.runCommand(progress, commandLine);
                    } finally {
                        ffmpegThread = null;
                    }
                }
            };
            if (!ffprobe.isAvailable()) {
                logger.info("FFProbe is not available");
                return null;
            } else if (!ffprobe.isCompatible()) {
                logger.info("FFProbe is incompatible");
                return null;
            } else {
                this.processID = new UniqueAlltimeID().getID();
                initPipe(ffprobe);
                return ffprobe.getStreamInfo("http://" + server.getServerAddress() + "/m3u8.m3u8?id=" + processID);
            }
        } finally {
            if (stopHttpServer()) {
                waitForProcessingRequests();
            }
        }
    }

    protected void waitForProcessingRequests() throws InterruptedException {
        while (true) {
            synchronized (requestsInProcess) {
                if (requestsInProcess.get() == 0) {
                    break;
                } else {
                    requestsInProcess.wait(50);
                }
            }
        }
    }

    protected String guessFFmpegFormat(final StreamInfo streamInfo) {
        if (streamInfo != null && streamInfo.getStreams() != null) {
            for (final Stream s : streamInfo.getStreams()) {
                if ("video".equalsIgnoreCase(s.getCodec_type())) {
                    return "mp4";
                }
            }
        }
        return null;
    }

    protected String getFFmpegFormat(final AbstractFFmpegBinary ffmpeg) throws Exception {
        String name = link.getForcedFileName();
        if (StringUtils.isEmpty(name)) {
            name = link.getFinalFileName();
            if (StringUtils.isEmpty(name)) {
                name = link.getRawName();
            }
            if (StringUtils.isEmpty(name)) {
                final String url = link.getContentUrlOrPatternMatcher();
                name = Plugin.extractFileNameFromURL(url);
            }
        }
        String format = ffmpeg.getDefaultFormatByFileName(name);
        if (format == null) {
            final StreamInfo streamInfo = getProbe();
            format = guessFFmpegFormat(streamInfo);
            if (format == null) {
                final String extension = Files.getExtension(name);
                if (extension == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String extensionID = extension.toLowerCase(Locale.ENGLISH);
                final FFmpegSetup config = JsonConfig.create(FFmpegSetup.class);
                synchronized (HLSDownloader.class) {
                    HashMap<String, String> map = config.getExtensionToFormatMap();
                    if (map == null) {
                        map = new HashMap<String, String>();
                    } else {
                        map = new HashMap<String, String>(map);
                    }
                    try {
                        format = map.get(extensionID);
                        if (format == null) {
                            final ArrayList<String> queryDefaultFormat = new ArrayList<String>();
                            queryDefaultFormat.add(ffmpeg.getFullPath());
                            final File dummy = Application.getTempResource("ffmpeg_dummy-" + org.appwork.utils.UniqueAlltimeID.next() + "." + extension);
                            try {
                                queryDefaultFormat.add(dummy.getAbsolutePath());
                                queryDefaultFormat.add("-y");
                                ffmpeg.runCommand(null, queryDefaultFormat);
                            } finally {
                                if (!dummy.delete() && dummy.exists()) {
                                    dummy.deleteOnExit();
                                }
                            }
                        }
                    } catch (FFMpegException e) {
                        final String res = e.getStdErr();
                        format = new Regex(res, "Output \\#0\\, ([^\\,]+)").getMatch(0);
                        if (format == null) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, null, -1, e);
                        }
                        map.put(extensionID, format);
                        config.setExtensionToFormatMap(map);
                    }
                }
            }
        }
        return format;
    }

    private void runConcat() throws IOException, SkipReasonException, PluginException {
        try {
            final FFmpeg ffmpeg = new FFmpeg() {
                @Override
                public LogInterface getLogger() {
                    return HLSDownloader.this.logger;
                }

                @Override
                public String runCommand(FFMpegProgress progress, List<String> commandLine) throws IOException, InterruptedException, FFMpegException {
                    ffmpegThread = Thread.currentThread();
                    try {
                        return super.runCommand(progress, commandLine);
                    } finally {
                        ffmpegThread = null;
                    }
                }

                @Override
                protected void parseLine(boolean isStdout, String line) {
                }
            };
            processID = new UniqueAlltimeID().getID();
            initPipe(ffmpeg);
            final AtomicReference<File> outputFile = new AtomicReference<File>();
            final FFMpegProgress progress = new FFMpegProgress() {
                final long total;
                {
                    long total = 0;
                    for (final PartFile partFile : outputPartFiles) {
                        total += partFile.file.length();
                    }
                    this.total = total;
                }

                @Override
                public void setTotal(long total) {
                }

                public void updateValues(long current, long total) {
                };

                @Override
                public long getCurrent() {
                    final File file = outputFile.get();
                    if (file != null) {
                        return file.length();
                    } else {
                        return 0;
                    }
                };

                @Override
                public long getTotal() {
                    return total;
                }

                @Override
                public String getMessage(Object requestor) {
                    if (requestor instanceof ETAColumn) {
                        final long eta = getETA();
                        if (eta > 0) {
                            return Formatter.formatSeconds(eta);
                        }
                        return null;
                    }
                    return getDetaultMessage();
                }

                @Override
                public long getETA() {
                    final long runtime = System.currentTimeMillis() - startedTimestamp;
                    if (runtime > 0) {
                        final double speed = getCurrent() / (double) runtime;
                        if (speed > 0) {
                            return (long) ((getTotal() - getCurrent()) / speed);
                        } else {
                            return -1;
                        }
                    }
                    return -1;
                }

                @Override
                protected String getDetaultMessage() {
                    return _GUI.T.FFMpegProgress_getMessage_concat();
                }
            };
            progress.setProgressSource(this);
            boolean deleteOutput = true;
            final String concatFormat;
            if (CrossSystem.isWindows() && m3u8Playlists.size() > 1) {
                concatFormat = getFFmpegFormat(ffmpeg);
            } else {
                concatFormat = null;
            }
            if (CONCATSOURCE.HTTP.equals(getConcatSource())) {
                final Map<String, File> fileMap = new HashMap<String, File>();
                for (final PartFile partFile : outputPartFiles) {
                    fileMap.put(UniqueAlltimeID.create(), partFile.file);
                }
                this.fileMap = fileMap;
            }
            try {
                downloadable.addPluginProgress(progress);
                try {
                    outputFile.set(outputCompleteFile);
                    ffmpeg.runCommand(null, buildConcatCommandLine(concatFormat, ffmpeg, outputCompleteFile.getAbsolutePath()));
                    deleteOutput = false;
                } catch (FFMpegException e) {
                    // some systems have problems with special chars to find the in or out file.
                    if (FFMpegException.ERROR.PATH_LENGTH.equals(e.getError())) {
                        final File tmpOut = new File(outputCompleteFile.getParent(), "ffmpeg_out" + UniqueAlltimeID.create());
                        logger.info("Try workaround:" + e.getError() + "|Tmp:" + tmpOut + "|Dest:" + outputCompleteFile);
                        boolean deleteTmp = true;
                        try {
                            outputFile.set(tmpOut);
                            ffmpeg.runCommand(null, buildConcatCommandLine(concatFormat, ffmpeg, tmpOut.getAbsolutePath()));
                            ffmpeg.moveFile(tmpOut, outputCompleteFile);
                            deleteTmp = false;
                            deleteOutput = false;
                        } finally {
                            if (deleteTmp && !tmpOut.delete() && tmpOut.exists()) {
                                tmpOut.deleteOnExit();
                            }
                        }
                    } else {
                        throw e;
                    }
                }
            } finally {
                downloadable.removePluginProgress(progress);
                if (deleteOutput) {
                    outputCompleteFile.delete();
                } else {
                    for (final PartFile partFile : outputPartFiles) {
                        partFile.concatFlag.set(true);
                        partFile.file.delete();
                    }
                }
            }
        } catch (final FFMpegException e) {
            if (FFMpegException.ERROR.PATH_LENGTH.equals(e.getError())) {
                throw new SkipReasonException(SkipReason.INVALID_DESTINATION, e);
            } else if (FFMpegException.ERROR.DISK_FULL.equals(e.getError())) {
                throw new SkipReasonException(SkipReason.DISK_FULL, e);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, e.getMessage(), -1, e);
            }
        } catch (Exception e) {
            logger.log(e);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, e.getMessage(), -1, e);
        } finally {
            // link.removePluginProgress(set);
            stopHttpServer();
        }
    }

    private void runDownload() throws Exception {
        link.setDownloadSize(-1);
        try {
            final AtomicLong lastBitrate = new AtomicLong(-1);
            final AtomicLong lastBytesWritten = new AtomicLong(0);
            final AtomicLong lastTime = new AtomicLong(0);
            final AtomicLong completeTime = new AtomicLong(0);
            final long estimatedDuration = M3U8Playlist.getEstimatedDuration(m3u8Playlists) / 1000;
            final FFmpeg ffmpeg = new FFmpeg() {
                @Override
                protected int exitProcess(Process process, String stdout, String stderr) throws IllegalThreadStateException {
                    try {
                        return process.exitValue();
                    } catch (IllegalThreadStateException e) {
                        if (stderr != null && stderr.matches("(?s).*video:\\d+\\w*\\s*audio:\\d+\\w*\\s*subtitle:\\d+\\w*.+")) {
                            return 0;
                        } else {
                            throw e;
                        }
                    }
                }

                @Override
                public String runCommand(FFMpegProgress progress, List<String> commandLine) throws IOException, InterruptedException, FFMpegException {
                    ffmpegThread = Thread.currentThread();
                    try {
                        return super.runCommand(progress, commandLine);
                    } finally {
                        ffmpegThread = null;
                    }
                }

                @Override
                public LogInterface getLogger() {
                    return HLSDownloader.this.logger;
                }

                @Override
                protected long getLastUpdateTimestampTimeout() {
                    return 2 * super.getLastUpdateTimestampTimeout();
                }

                @Override
                protected void parseLine(boolean isStdout, String line) {
                    try {
                        final String trimmedLine = line.trim();
                        if (trimmedLine.startsWith("Duration:")) {
                            // if (!line.contains("Duration: N/A")) {
                            // final String durationString = new Regex(line, "Duration\\: (.*?).?\\d*?\\, start").getMatch(0);
                            // if (durationString != null) {
                            // final long duration = formatStringToMilliseconds(durationString);
                            // }
                            // }
                        } else if (trimmedLine.startsWith("Stream #")) {
                            final String bitrateString = new Regex(line, "(\\d+) kb\\/s").getMatch(0);
                            if (bitrateString != null) {
                                if (lastBitrate.get() == -1) {
                                    lastBitrate.set(Integer.parseInt(bitrateString));
                                } else {
                                    lastBitrate.addAndGet(Integer.parseInt(bitrateString));
                                }
                                final long bitrate = lastBitrate.get();
                                if (estimatedDuration > 0 && bitrate > 0) {
                                    final long estimatedSize = ((estimatedDuration) * bitrate * 1024) / 8;
                                    downloadable.setDownloadTotalBytes(Math.max(M3U8Playlist.getEstimatedSize(m3u8Playlists), estimatedSize));
                                }
                            }
                        } else if (trimmedLine.startsWith("Output #0")) {
                            final long bitrate = lastBitrate.get();
                            if (estimatedDuration > 0 && bitrate > 0) {
                                final long estimatedSize = ((estimatedDuration) * bitrate * 1024) / 8;
                                downloadable.setDownloadTotalBytes(Math.max(M3U8Playlist.getEstimatedSize(m3u8Playlists), estimatedSize));
                            }
                        } else if (trimmedLine.startsWith("frame=") || trimmedLine.startsWith("size=")) {
                            final String sizeString = new Regex(line, "size=\\s*(\\S+)\\s+").getMatch(0);
                            final long size = SizeFormatter.getSize(sizeString);
                            final long currentBytesWritten = lastBytesWritten.get() + size;
                            bytesWritten.set(currentBytesWritten);
                            downloadable.setDownloadBytesLoaded(currentBytesWritten);
                            final String timeString = new Regex(line, "time=\\s*(\\S+)\\s+").getMatch(0);
                            long time = (formatStringToMilliseconds(timeString) / 1000);
                            lastTime.set(time);
                            time += completeTime.get();
                            final long estimatedSize;
                            if (time > 0 && estimatedDuration > 0) {
                                final long rate = currentBytesWritten / time;
                                estimatedSize = (estimatedDuration) * rate;
                            } else {
                                estimatedSize = currentBytesWritten;
                            }
                            downloadable.setDownloadTotalBytes(Math.max(M3U8Playlist.getEstimatedSize(m3u8Playlists), estimatedSize));
                        }
                    } catch (Throwable e) {
                        logger.log(e);
                    }
                };
            };
            currentPlayListIndex.set(0);
            final String downloadFormat;
            if (CrossSystem.isWindows() && m3u8Playlists.size() > 1) {
                downloadFormat = "mpegts";
            } else {
                downloadFormat = getFFmpegFormat(ffmpeg);
            }
            processID = new UniqueAlltimeID().getID();
            initPipe(ffmpeg);
            for (int index = 0; index < m3u8Playlists.size(); index++) {
                final PartFile partFile = outputPartFiles.get(index);
                final File destination = partFile.file;
                try {
                    completeTime.addAndGet(lastTime.get());
                    lastBitrate.set(-1);
                    lastBytesWritten.set(bytesWritten.get());
                    currentPlayListIndex.set(index);
                    while (true) {
                        try {
                            ffmpeg.runCommand(null, buildDownloadCommandLine(downloadFormat, ffmpeg, destination.getAbsolutePath()));
                            break;
                        } catch (FFMpegException e) {
                            if (FFMpegException.ERROR.UNKNOWN.equals(e.getError()) && StringUtils.containsIgnoreCase(e.getStdErr(), "Codec 'mp3'") && Boolean.TRUE.equals(putRetryMapValue("aac_adtstoasc", Boolean.FALSE, false))) {
                                // [aac_adtstoasc @ 0x6b34f00] Codec 'mp3' (86017) is not supported by the bitstream filter 'aac_adtstoasc'.
                                // Supported codecs are: aac (86018)
                                logger.log(e);
                                continue;
                            }
                            throw e;
                        }
                    }
                    partFile.downloadFlag.set(true);
                } catch (FFMpegException e) {
                    // some systems have problems with special chars to find the in or out file.
                    if (FFMpegException.ERROR.PATH_LENGTH.equals(e.getError())) {
                        final File tmpOut = new File(destination.getParent(), "ffmpeg_out" + UniqueAlltimeID.create());
                        logger.info("Try workaround:" + e.getError() + "|Tmp:" + tmpOut + "|Dest:" + destination);
                        boolean deleteTmp = true;
                        try {
                            ffmpeg.runCommand(null, buildDownloadCommandLine(downloadFormat, ffmpeg, tmpOut.getAbsolutePath()));
                            ffmpeg.moveFile(destination, tmpOut);
                            partFile.downloadFlag.set(true);
                            deleteTmp = false;
                        } finally {
                            if (deleteTmp && !tmpOut.delete() && tmpOut.exists()) {
                                tmpOut.deleteOnExit();
                            }
                        }
                    } else {
                        throw e;
                    }
                }
            }
        } catch (final FFMpegException e) {
            if (FFMpegException.ERROR.PATH_LENGTH.equals(e.getError())) {
                throw new SkipReasonException(SkipReason.INVALID_DESTINATION, e);
            } else if (FFMpegException.ERROR.DISK_FULL.equals(e.getError())) {
                throw new SkipReasonException(SkipReason.DISK_FULL, e);
            } else {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, e.getMessage(), -1, e);
            }
        } catch (Exception e) {
            logger.log(e);
            throw e;
        } finally {
            currentPlayListIndex.set(0);
            try {
                if (connectionHandler != null) {
                    for (ThrottledConnection con : connectionHandler.getConnections()) {
                        connectionHandler.removeThrottledConnection(con);
                    }
                }
            } finally {
                // link.removePluginProgress(set);
                stopHttpServer();
            }
        }
    }

    protected boolean stopHttpServer() {
        final HttpServer server = this.server;
        this.server = null;
        if (server != null) {
            server.stop();
            return true;
        } else {
            return false;
        }
    }

    protected boolean isMapMetaDataEnabled() {
        return false;
    }

    protected Object getRetryMapValue(final String key) {
        final int playListIndex = getCurrentPlayListIndex();
        synchronized (HLSDownloader.this.retryMap) {
            final Map<String, Object> currentRetryMap = HLSDownloader.this.retryMap.get(playListIndex);
            return currentRetryMap == null ? null : currentRetryMap.get(key);
        }
    }

    protected Object putRetryMapValue(final String key, final Object value, final boolean removeFlag) {
        final int playListIndex = getCurrentPlayListIndex();
        synchronized (HLSDownloader.this.retryMap) {
            Map<String, Object> currentRetryMap = HLSDownloader.this.retryMap.get(playListIndex);
            if (currentRetryMap == null && !removeFlag) {
                currentRetryMap = new HashMap<String, Object>();
                HLSDownloader.this.retryMap.put(playListIndex, currentRetryMap);
            }
            if (removeFlag) {
                return currentRetryMap == null ? null : currentRetryMap.remove(key);
            } else {
                return currentRetryMap.put(key, value);
            }
        }
    }

    protected boolean requiresAdtstoAsc(final String format, final FFmpeg ffmpeg) {
        boolean ret = ffmpeg.requiresAdtstoAsc(format);
        if (ret && Boolean.FALSE.equals(getRetryMapValue("aac_adtstoasc"))) {
            ret = false;
        }
        return ret;
    }

    protected ArrayList<String> buildConcatCommandLine(final String format, FFmpeg ffmpeg, String out) {
        final ArrayList<String> l = new ArrayList<String>();
        l.add(ffmpeg.getFullPath());
        final CONCATSOURCE concatSource = getConcatSource();
        if (CrossSystem.isWindows() && CONCATSOURCE.FILE.equals(concatSource)) {
            // workaround to support long path lengths
            // https://trac.ffmpeg.org/wiki/Concatenate
            // TODO: test if this still works, for example check for unsafe filename
            l.add("-i");
            final StringBuilder sb = new StringBuilder();
            sb.append("concat:");
            boolean seperator = false;
            for (final PartFile outputPartFile : outputPartFiles) {
                if (seperator) {
                    sb.append("|");
                } else {
                    seperator = true;
                }
                sb.append("\\\\?\\" + outputPartFile.file.getAbsolutePath());
            }
            l.add(sb.toString());
        } else {
            l.add("-f");
            l.add("concat");
            if (true) {
                // required, see https://trac.ffmpeg.org/wiki/Concatenate
                l.add("-safe");
                l.add("0");
            }
            if (true) {
                // https://blog.yo1.dog/fix-for-ffmpeg-protocol-not-on-whitelist-error-for-urls/
                l.add("-protocol_whitelist");
                switch (concatSource) {
                case FILE:
                    l.add("file,http,tcp");
                    break;
                case HTTP:
                    l.add("http,tcp");
                    break;
                }
            }
            l.add("-i");
            l.add("http://" + server.getServerAddress() + "/concat?id=" + processID);
        }
        if (isMapMetaDataEnabled()) {
            final FFmpegMetaData ffMpegMetaData = getFFmpegMetaData();
            if (ffMpegMetaData != null && !ffMpegMetaData.isEmpty()) {
                l.add("-i");
                l.add("http://" + server.getServerAddress() + "/meta?id=" + processID);
                l.add("-map_metadata");
                l.add("1");
            }
        }
        l.add("-c");
        l.add("copy");
        applyBitStreamFilter(l, format, ffmpeg);
        // TODO: try to add support for http output and request handler writing bytes to file
        if (CrossSystem.isWindows() && out.length() > 259) {
            // https://msdn.microsoft.com/en-us/library/aa365247.aspx
            l.add("\\\\?\\" + out);
        } else {
            l.add(out);
        }
        l.add("-y");
        return l;
    }

    protected ArrayList<String> buildDownloadCommandLine(String format, FFmpeg ffmpeg, String out) {
        final ArrayList<String> l = new ArrayList<String>();
        l.add(ffmpeg.getFullPath());
        l.add("-analyzeduration");// required for low bandwidth streams!
        l.add("15000000");// 15 secs
        l.add("-i");
        l.add("http://" + server.getServerAddress() + "/m3u8.m3u8?id=" + processID);
        if (isMapMetaDataEnabled()) {
            final FFmpegMetaData ffMpegMetaData = getFFmpegMetaData();
            if (ffMpegMetaData != null && !ffMpegMetaData.isEmpty()) {
                l.add("-i");
                l.add("http://" + server.getServerAddress() + "/meta?id=" + processID);
                l.add("-map_metadata");
                l.add("1");
            }
        }
        if (false) {
            // https://svn.jdownloader.org/issues/85472#note-8
            l.add("-copyts");
        }
        applyBitStreamFilter(l, format, ffmpeg);
        l.add("-c:v");
        l.add("copy");
        l.add("-c:a");
        l.add("copy");
        l.add("-f");
        l.add(format);
        // TODO: try to add support for http output and request handler writing bytes to file
        if (CrossSystem.isWindows() && out.length() > 259) {
            // https://msdn.microsoft.com/en-us/library/aa365247.aspx
            l.add("\\\\?\\" + out);
        } else {
            l.add(out);
        }
        l.add("-y");
        // l.add("-progress");
        // l.add("http://127.0.0.1:" + server.getPort() + "/progress?id=" + processID);
        return l;
    }

    protected void applyBitStreamFilter(List<String> cmdLine, String format, FFmpeg ffmpeg) {
        if (format != null && "mpegts".equals(format)) {
            cmdLine.add("-bsf:v");
            cmdLine.add("h264_mp4toannexb");
        }
        if (format != null && requiresAdtstoAsc(format, ffmpeg)) {
            cmdLine.add("-bsf:a");
            cmdLine.add("aac_adtstoasc");
            putRetryMapValue("aac_adtstoasc", Boolean.TRUE, false);
        }
    }

    protected FFmpegMetaData getFFmpegMetaData() {
        return null;
    }

    protected Browser getRequestBrowser() {
        final Browser ret = sourceBrowser.cloneBrowser();
        ret.setConnectTimeout(30 * 1000);
        ret.setReadTimeout(30 * 1000);
        ret.setFollowRedirects(true);
        return ret;
    }

    protected List<M3U8Playlist> getM3U8Playlists() throws Exception {
        List<M3U8Playlist> ret = this.m3u8Playlists;
        if (ret != null) {
            return ret;
        } else {
            final Browser br = this.getRequestBrowser();
            // work around for longggggg m3u pages
            final int was = br.getLoadLimit();
            // lets set the connection limit to our required request
            br.setLoadLimit(Integer.MAX_VALUE);
            try {
                ret = M3U8Playlist.loadM3U8(buildDownloadUrl(m3uUrl), br);
                this.currentConnection = br.getHttpConnection();
                return ret;
            } finally {
                br.setLoadLimit(was);
            }
        }
    }

    private final AtomicInteger requestsInProcess = new AtomicInteger(0);

    protected void handleFileRequest(final File file, GetRequest request, HttpResponse response) throws FileNotFoundException, IOException {
        final FileInputStream fis = new FileInputStream(file);
        try {
            final long fileSize = file.length();
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_DISPOSITION, "attachment;filename*=UTF-8''" + URLEncoder.encode(file.getName(), "UTF-8")));
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCEPT_RANGES, "bytes"));
            final String requestRange = request.getRequestHeaders().getValue(HTTPConstants.HEADER_REQUEST_RANGE);
            long from = 0;
            long to = fileSize - 1;
            boolean gotValidRange = false;
            if (requestRange != null) {
                final long[] range = HTTPConnectionUtils.parseRequestRange(requestRange);
                if (range[0] != -1 && range[1] == -1) {
                    final long rangeStart = range[0];
                    if (rangeStart < fileSize - 1) {
                        from = rangeStart;
                        to = fileSize - 1;
                        gotValidRange = true;
                    }
                } else if (range[0] != -1 && range[1] == -1) {
                    final long rangeStart = range[0];
                    final long rangeEnd = range[1];
                    if (rangeStart < fileSize - 1 && rangeEnd > rangeStart && rangeEnd <= fileSize - 1) {
                        from = rangeStart;
                        to = rangeEnd;
                        gotValidRange = true;
                    }
                }
            }
            final InputStream is;
            if (gotValidRange) {
                final long length = 1 + to - from;
                response.setResponseCode(ResponseCode.SUCCESS_PARTIAL_CONTENT);
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, Long.toString(length)));
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_RANGE, "bytes " + from + "-" + to + "/" + fileSize));
                fis.getChannel().position(from);
                is = new LimitedInputStream(fis, length);
            } else {
                response.setResponseCode(ResponseCode.SUCCESS_OK);
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, Long.toString(fileSize)));
                is = fis;
            }
            IO.readStreamToOutputStream(-1, is, response.getOutputStream(true), false);
        } finally {
            fis.close();
        }
    }

    protected HttpServer startHttpServer() throws IOException {
        final HttpServer server = new HttpServer(0);
        server.setLocalhostOnly(true);
        this.server = server;
        server.start();
        return server;
    }

    /**
     * extension checks in https://github.com/FFmpeg/FFmpeg/blob/master/libavformat/hls.c - test_segment method
     *
     * @param ffmpeg
     * @param segment
     * @return
     * @throws Exception
     */
    protected String getSegmentExtension(final AbstractFFmpegBinary ffmpeg, final M3U8Segment segment) throws Exception {
        final String format = getFFmpegFormat(ffmpeg);
        if (format != null) {
            if (format.matches("(?i)^flac$")) {
                return "flac";
            } else if (format.matches("(?i)^mp3$")) {
                return "mp3";
            } else if (format.matches("(?i)^(mp4|aac|mpegts)$")) {
                return "ts";
            }
        }
        return "ts";
    }

    protected boolean isSegmentDownload(GetRequest request) {
        final String requestedPath = request.getRequestedPath();
        if (requestedPath == null) {
            return false;
        }
        return requestedPath.matches("^/download(\\.(ts|mp3|flac))?$");
    }

    private void initPipe(final AbstractFFmpegBinary ffmpeg) throws IOException {
        final LinkedList<MeteredThrottledInputStream> connectedMeteredThrottledInputStream = new LinkedList<MeteredThrottledInputStream>();
        final DelayedRunnable delayedCleanup = new DelayedRunnable(5000) {
            @Override
            public void delayedrun() {
                synchronized (connectedMeteredThrottledInputStream) {
                    while (true) {
                        final MeteredThrottledInputStream is = connectedMeteredThrottledInputStream.pollLast();
                        if (is != null) {
                            if (connectionHandler != null) {
                                connectionHandler.removeThrottledConnection(is);
                            }
                        } else {
                            break;
                        }
                    }
                }
            }
        };
        final HttpServer finalServer = startHttpServer();
        finalServer.registerRequestHandler(new HttpRequestHandler() {
            final byte[] readBuf = new byte[512];

            @Override
            public boolean onPostRequest(PostRequest request, HttpResponse response) {
                requestsInProcess.incrementAndGet();
                try {
                    logger.info(request.toString());
                    if (!validateID(request)) {
                        return false;
                    }
                    if ("/progress".equals(request.getRequestedPath())) {
                        while (request.getInputStream().read(readBuf) != -1) {
                        }
                        response.setResponseCode(ResponseCode.SUCCESS_OK);
                        return true;
                    }
                } catch (Exception e) {
                    logger.log(e);
                } finally {
                    synchronized (requestsInProcess) {
                        requestsInProcess.decrementAndGet();
                        requestsInProcess.notifyAll();
                    }
                }
                return false;
            }

            private final boolean validateID(HttpRequest request) throws IOException {
                final String id = request.getParameterbyKey("id");
                if (id == null) {
                    return false;
                } else if (processID != Long.parseLong(request.getParameterbyKey("id"))) {
                    return false;
                } else {
                    return true;
                }
            }

            private final Map<String, Object> parseRetryMap(HttpRequest request) throws Exception {
                final String value = request.getParameterbyKey("retryMap");
                if (value == null) {
                    return null;
                }
                return ((DownloadLinkDownloadable) getDownloadable()).getPlugin().restoreFromString(value, TypeRef.MAP);
            }

            private final String retry(HttpRequest request, int responseCode, int maxRetry) throws Exception {
                Map<String, Object> retryMap = parseRetryMap(request);
                if (retryMap == null) {
                    retryMap = new HashMap<String, Object>();
                }
                Number retryCount = (Number) retryMap.get(String.valueOf(responseCode));
                if (retryCount == null) {
                    retryCount = 1;
                } else {
                    retryCount = retryCount.intValue() + 1;
                }
                if (retryCount.intValue() > maxRetry) {
                    throw new IOException("retry(" + retryCount + ") limit(" + maxRetry + ") reached for responseCode=" + responseCode);
                }
                retryMap.put(String.valueOf(responseCode), retryCount);
                String ret = request.getRequestedURL();
                final String retryMapString = URLEncode.encodeURIComponent(new SimpleMapper().setPrettyPrintEnabled(false).objectToString(retryMap));
                if (!ret.contains("retryMap=")) {
                    ret = ret + "&retryMap=" + retryMapString;
                } else {
                    ret = ret.replaceFirst("(retryMap=.*?)(&|$)", "retryMap=" + Matcher.quoteReplacement(retryMapString));
                }
                return ret;
            }

            private final boolean handleRetry(HttpRequest request, HttpResponse response, URLConnectionAdapter connection) throws Exception {
                if (connection.getResponseCode() == 429) {
                    final String location = retry(request, 429, 2);
                    Thread.sleep(1000);
                    response.setResponseCode(ResponseCode.get(302));
                    response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_LOCATION, location));
                    return true;
                }
                return false;
            }

            @Override
            public boolean onGetRequest(GetRequest request, HttpResponse response) {
                boolean requestOkay = false;
                final LogSource requestLogger = new LogSource(request.getRequestedURL());
                requestsInProcess.incrementAndGet();
                try {
                    requestLogger.info("START " + request.getRequestedURL());
                    requestLogger.info(request.toString());
                    if (!validateID(request)) {
                        requestLogger.info("invalid ID");
                        return false;
                    } else if ("/file".equals(request.getRequestedPath())) {
                        ffmpeg.updateLastUpdateTimestamp();
                        final String fileID = request.getParameterbyKey("fileID");
                        final File file = fileMap.get(fileID);
                        requestLogger.info("handle file request:fileID:" + fileID + "|file:" + file);
                        if (file == null || !file.isFile()) {
                            response.setResponseCode(HTTPConstants.ResponseCode.get(404));
                        } else {
                            try {
                                handleFileRequest(file, request, response);
                            } catch (final FileNotFoundException e) {
                                requestLogger.log(e);
                                response.setResponseCode(HTTPConstants.ResponseCode.get(404));
                            }
                        }
                        requestOkay = true;
                        return true;
                    } else if ("/concat".equals(request.getRequestedPath())) {
                        ffmpeg.updateLastUpdateTimestamp();
                        final StringBuilder sb = new StringBuilder();
                        for (final PartFile partFile : outputPartFiles) {
                            if (sb.length() > 0) {
                                sb.append("\r\n");
                            }
                            sb.append("file '");
                            switch (getConcatSource()) {
                            case FILE:
                                if (CrossSystem.isWindows()) {
                                    // https://trac.ffmpeg.org/ticket/2702
                                    // NOTE: this does not work for long path lengths! see different way in buildConcatCommandLine
                                    sb.append("file:" + partFile.file.getAbsolutePath().replaceAll("\\\\", "/"));
                                } else {
                                    sb.append("file://" + partFile.file.getAbsolutePath());
                                }
                                break;
                            case HTTP:
                                String fileID = null;
                                for (final Entry<String, File> fileMapEntry : fileMap.entrySet()) {
                                    if (fileMapEntry.getValue().equals(partFile.file)) {
                                        fileID = fileMapEntry.getKey();
                                        break;
                                    }
                                }
                                if (fileID == null) {
                                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                                } else {
                                    sb.append("http://" + finalServer.getServerAddress() + "/file?fileID=" + fileID + "&id=" + processID);
                                }
                                break;
                            }
                            sb.append("'");
                        }
                        final byte[] bytes = sb.toString().getBytes("UTF-8");
                        response.setResponseCode(ResponseCode.get(200));
                        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain"));
                        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(bytes.length)));
                        final OutputStream out = response.getOutputStream(true);
                        out.write(bytes);
                        out.flush();
                        requestOkay = true;
                        return true;
                    } else if ("/meta".equals(request.getRequestedPath())) {
                        ffmpeg.updateLastUpdateTimestamp();
                        final FFmpegMetaData ffMpegMetaData = getFFmpegMetaData();
                        if (ffMpegMetaData != null && !ffMpegMetaData.isEmpty()) {
                            final String content = ffMpegMetaData.getFFmpegMetaData();
                            final byte[] bytes = content.getBytes("UTF-8");
                            response.setResponseCode(HTTPConstants.ResponseCode.get(200));
                            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/plain; charset=utf-8"));
                            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(bytes.length)));
                            final OutputStream out = response.getOutputStream(true);
                            if (bytes.length > 0) {
                                out.write(bytes);
                                out.flush();
                            }
                        } else {
                            response.setResponseCode(HTTPConstants.ResponseCode.get(404));
                        }
                        requestOkay = true;
                        return true;
                    } else if ("/m3u8.m3u8".equals(request.getRequestedPath())) {
                        ffmpeg.updateLastUpdateTimestamp();
                        final M3U8Playlist m3u8 = getCurrentPlayList();
                        if (isSupported(m3u8)) {
                            final StringBuilder sb = new StringBuilder();
                            sb.append("#EXTM3U\r\n");
                            sb.append("#EXT-X-VERSION:3\r\n");
                            sb.append("#EXT-X-MEDIA-SEQUENCE:0\r\n");
                            if (m3u8.getTargetDuration() > 0) {
                                sb.append("#EXT-X-TARGETDURATION:");
                                sb.append(m3u8.getTargetDuration());
                                sb.append("\r\n");
                            }
                            final M3U8Segment extXMap = m3u8.getExtXMap();
                            if (extXMap != null) {
                                sb.append("#EXT-X-MAP:URI=" + extXMap.getUrl() + "\r\n");
                            }

                            for (int index = 0; index < m3u8.size(); index++) {
                                final M3U8Segment segment = m3u8.getSegment(index);
                                sb.append("#EXTINF:" + M3U8Segment.toExtInfDuration(segment.getDuration()));
                                // prefer relative URLs
                                sb.append("\r\ndownload." + getSegmentExtension(ffmpeg, segment) + "?id=" + processID + "&ts_index=" + index + "\r\n");
                            }
                            sb.append("#EXT-X-ENDLIST\r\n");
                            response.setResponseCode(ResponseCode.get(200));
                            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "application/vnd.apple.mpegurl"));
                            final byte[] bytes = sb.toString().getBytes("UTF-8");
                            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(bytes.length)));
                            final OutputStream out = response.getOutputStream(true);
                            out.write(bytes);
                            out.flush();
                        } else {
                            response.setResponseCode(ResponseCode.get(404));
                        }
                        requestOkay = true;
                        return true;
                    } else if (isSegmentDownload(request)) {
                        final String url = request.getParameterbyKey("url");
                        final String segmentIndex = request.getParameterbyKey("ts_index");
                        if (segmentIndex == null && url == null) {
                            return false;
                        }
                        final String downloadURL;
                        final M3U8Segment segment;
                        final M3U8Playlist playList;
                        if (url != null) {
                            // disabled in HLSDownloader! do not allow access to other urls than hls segments
                            segment = null;
                            downloadURL = url;
                            playList = null;
                            return false;
                        }
                        final int playListIndex = getCurrentPlayListIndex();
                        Map<String, Object> currentRetryMap = null;
                        synchronized (HLSDownloader.this.retryMap) {
                            currentRetryMap = HLSDownloader.this.retryMap.get(playListIndex);
                            if (currentRetryMap == null) {
                                currentRetryMap = new HashMap<String, Object>();
                                HLSDownloader.this.retryMap.put(playListIndex, currentRetryMap);
                            }
                        }
                        synchronized (currentRetryMap) {
                            if (currentRetryMap.containsKey("block_" + segmentIndex)) {
                                response.setResponseCode(ResponseCode.get(404));
                                return true;
                            }
                        }
                        playList = getCurrentPlayList();
                        try {
                            final int index = Integer.parseInt(segmentIndex);
                            segment = playList.getSegment(index);
                            if (segment == null) {
                                throw new IndexOutOfBoundsException("Unknown segment:" + index);
                            } else {
                                requestLogger.info("Forward segment:" + (index + 1) + "/" + playList.size());
                                downloadURL = segment.getUrl();
                            }
                            if (checkAbortDownloadCondition(playList, segment)) {
                                requestLogger.info("Abort segment:" + (index + 1) + "/" + playList.size());
                                response.setResponseCode(ResponseCode.get(404));
                                return true;
                            }
                            synchronized (m3u8SegmentsStates) {
                                if (!m3u8SegmentsStates.containsKey(segment)) {
                                    m3u8SegmentsStates.put(segment, M3U8SEGMENTSTATE.UNKNOWN);
                                }
                            }
                        } catch (final NumberFormatException e) {
                            requestLogger.log(e);
                            return false;
                        } catch (final IndexOutOfBoundsException e) {
                            requestLogger.log(e);
                            return false;
                        }
                        final FileBytesMap fileBytesMap = new FileBytesMap();
                        final Browser br = getRequestBrowser();
                        br.setLogger(requestLogger);
                        OutputStream requestOutputStream = null;
                        MeteredThrottledInputStream meteredThrottledInputStream = null;
                        final long timeoutBuffer = 10 * 1000l;
                        try {
                            retryLoop: for (int retry = 0; retry < 5; retry++) {
                                try {
                                    final jd.http.requests.GetRequest getRequest = new jd.http.requests.GetRequest(buildDownloadUrl(downloadURL)) {
                                        protected boolean isBrotliAcceptEncodingEnabled() {
                                            return false;
                                        };
                                    };
                                    final long byteRange[] = segment.getByteRange();
                                    if (fileBytesMap.getFinalSize() > 0) {
                                        requestLogger.info("Resume(" + retry + "): " + fileBytesMap.toString());
                                        final List<Long[]> unMarkedAreas = fileBytesMap.getUnMarkedAreas();
                                        final long startByteRange = byteRange != null ? byteRange[1] : 0;
                                        getRequest.getHeaders().put(HTTPConstants.HEADER_REQUEST_RANGE, "bytes=" + startByteRange + unMarkedAreas.get(0)[0] + "-" + startByteRange + unMarkedAreas.get(0)[1]);
                                    } else if (byteRange != null) {
                                        getRequest.getHeaders().put(HTTPConstants.HEADER_REQUEST_RANGE, "bytes=" + byteRange[1] + "-" + (byteRange[1] + byteRange[0] - 1));
                                    } else if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                                        getRequest.getHeaders().put(HTTPConstants.HEADER_REQUEST_RANGE, "bytes=0-");
                                    }
                                    URLConnectionAdapter connection = null;
                                    boolean closeConnection = true;
                                    boolean updateTimestamp = true;
                                    int skippedBytes = 0;
                                    try {
                                        ffmpeg.updateLastUpdateTimestamp(getRequest.getConnectTimeout() + getRequest.getReadTimeout() + timeoutBuffer);
                                        connection = br.openRequestConnection(getRequest);
                                        if (handleRetry(request, response, connection)) {
                                            return true;
                                        } else if (connection.getResponseCode() != 200 && connection.getResponseCode() != 206) {
                                            throw new IOException("ResponseCode(" + connection.getResponseCode() + ") must be 200 or 206!");
                                        } else {
                                            if (StringUtils.contains(connection.getContentType(), "image/png") || StringUtils.contains(connection.getContentType(), "image/jpeg") || StringUtils.contains(connection.getContentType(), "image/gif")) {
                                                // try to skip fake png/jpg image in front of video segment
                                                if (false) {
                                                    final CountingInputStream cis = new CountingInputStream(new NoClosingInputStream(connection.getInputStream()));
                                                    final Image image = ImageIO.read(cis);
                                                    skippedBytes = (int) cis.transferedBytes();
                                                    requestLogger.info("Skip fake image(read image method:" + skippedBytes + " bytes)");
                                                } else {
                                                    final byte[] peekBytes = connection.peek(100);
                                                    for (int index = 0; index < peekBytes.length - 1; index++) {
                                                        if (peekBytes[index] == 0x47 && peekBytes[index + 1] == 0x40) {
                                                            requestLogger.info("Skip fake image(seek method:" + index + " bytes)");
                                                            final DataInputStream dis = new DataInputStream(connection.getInputStream());
                                                            dis.readFully(new byte[index]);
                                                            skippedBytes = index;
                                                            break;
                                                        }
                                                    }
                                                    if (skippedBytes == 0) {
                                                        if (StringUtils.contains(connection.getContentType(), "image/png")) {
                                                            // skip png header
                                                            skippedBytes = 8;
                                                        } else if (StringUtils.contains(connection.getContentType(), "image/gif")) {
                                                            // skip gif header
                                                            skippedBytes = 6;
                                                        }
                                                        if (skippedBytes > 0) {
                                                            requestLogger.info("Skip fake image(skip header method:" + skippedBytes + " bytes)");
                                                            final DataInputStream dis = new DataInputStream(connection.getInputStream());
                                                            dis.readFully(new byte[skippedBytes]);
                                                        }
                                                    }
                                                }
                                            }
                                            closeConnection = false;
                                        }
                                    } catch (IOException e) {
                                        if (Exceptions.containsInstanceOf(e, SocketTimeoutException.class)) {
                                            updateTimestamp = false;
                                        }
                                        requestLogger.log(e);
                                        if (onSegmentConnectException(connection, e, fileBytesMap, retry, currentRetryMap, requestLogger)) {
                                            continue retryLoop;
                                        } else {
                                            synchronized (currentRetryMap) {
                                                currentRetryMap.put("block_" + segmentIndex, Boolean.TRUE);
                                            }
                                            response.setResponseCode(ResponseCode.get(404));
                                            return true;
                                        }
                                    } finally {
                                        if (closeConnection && connection != null) {
                                            connection.disconnect();
                                        }
                                        if (updateTimestamp) {
                                            ffmpeg.updateLastUpdateTimestamp();
                                        }
                                    }
                                    final byte[] readWriteBuffer = new byte[32 * 1024];
                                    long length = -1;
                                    try {
                                        if (fileBytesMap.getFinalSize() > 0) {
                                            length = fileBytesMap.getFinalSize();
                                        } else if (byteRange != null) {
                                            length = connection.getContentLength();
                                        } else {
                                            length = connection.getCompleteContentLength();
                                        }
                                        length = length - skippedBytes;
                                        if (requestOutputStream == null) {
                                            // final String acceptRanges =
                                            // connection.getHeaderField(HTTPConstants.HEADER_RESPONSE_ACCEPT_RANGES);
                                            response.setResponseCode(ResponseCode.SUCCESS_OK);
                                            if (length > 0) {
                                                fileBytesMap.setFinalSize(length);
                                                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, Long.toString(length)));
                                            }
                                            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, connection.getContentType()));
                                            // if (StringUtils.equalsIgnoreCase(acceptRanges, "bytes")) {
                                            // response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_ACCEPT_RANGES,
                                            // acceptRanges));
                                            // }
                                            requestOutputStream = response.getOutputStream(true);
                                        }
                                        final InputStream inputStream;
                                        if (segment != null && segment.isEncrypted()) {
                                            // FIXME: resume(range request) not supported yet
                                            final String keyURI = segment.getxKeyURI();
                                            if (M3U8Segment.X_KEY_METHOD.AES_128.equals(segment.getxKeyMethod()) && keyURI != null) {
                                                SecretKeySpec key = aes128Keys.get(keyURI);
                                                if (key == null) {
                                                    final Browser br2 = getRequestBrowser();
                                                    br2.setLogger(requestLogger);
                                                    br2.setFollowRedirects(true);
                                                    final URLConnectionAdapter con = br2.openGetConnection(buildDownloadUrl(keyURI));
                                                    try {
                                                        if (con.getResponseCode() == 200) {
                                                            final byte[] buf = IO.readStream(20, con.getInputStream());
                                                            if (buf.length == 16) {
                                                                key = new SecretKeySpec(buf, "AES");
                                                                aes128Keys.put(keyURI, key);
                                                            }
                                                        }
                                                    } finally {
                                                        con.disconnect();
                                                    }
                                                    if (key == null) {
                                                        throw new IOException("Failed to fetch #EXT-X-KEY:URI=" + keyURI);
                                                    }
                                                }
                                                /*
                                                 * https://tools.ietf.org/html/draft-pantos-http-live-streaming-20#section-5.2
                                                 */
                                                final IvParameterSpec ivSpec;
                                                if (segment.getxKeyIV() != null) {
                                                    ivSpec = new IvParameterSpec(HexFormatter.hexToByteArray(segment.getxKeyIV()));
                                                } else {
                                                    final int sequenceNumber = playList.getMediaSequenceOffset() + playList.indexOf(segment);
                                                    final byte[] iv = new byte[16];
                                                    iv[15] = (byte) (sequenceNumber >>> 0 & 0xFF);
                                                    iv[14] = (byte) (sequenceNumber >>> 8 & 0xFF);
                                                    iv[13] = (byte) (sequenceNumber >>> 16 & 0xFF);
                                                    ivSpec = new IvParameterSpec(iv);
                                                }
                                                try {
                                                    final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
                                                    cipher.init(Cipher.DECRYPT_MODE, key, ivSpec);
                                                    inputStream = new javax.crypto.CipherInputStream(connection.getInputStream(), cipher);
                                                } catch (Exception e) {
                                                    throw new IOException(e);
                                                }
                                            } else {
                                                throw new IOException("Unsupported Method #EXT-X-KEY:METHOD=" + segment.getxKeyMethod());
                                            }
                                        } else {
                                            inputStream = connection.getInputStream();
                                        }
                                        if (meteredThrottledInputStream == null) {
                                            for (int waitLoop = 1; waitLoop < 10; waitLoop++) {
                                                synchronized (connectedMeteredThrottledInputStream) {
                                                    meteredThrottledInputStream = connectedMeteredThrottledInputStream.poll();
                                                }
                                                if (meteredThrottledInputStream != null) {
                                                    break;
                                                } else {
                                                    Thread.sleep(50);
                                                }
                                            }
                                            if (meteredThrottledInputStream == null) {
                                                meteredThrottledInputStream = new MeteredThrottledInputStream(inputStream, new AverageSpeedMeter(10));
                                                if (connectionHandler != null) {
                                                    connectionHandler.addThrottledConnection(meteredThrottledInputStream);
                                                }
                                            } else {
                                                meteredThrottledInputStream.setInputStream(inputStream);
                                            }
                                        } else {
                                            meteredThrottledInputStream.setInputStream(inputStream);
                                        }
                                        long position = fileBytesMap.getMarkedBytes();
                                        final OutputStream outputStream;
                                        if (position > 0) {
                                            if (connection.getResponseCode() == 206) {
                                                outputStream = requestOutputStream;
                                            } else {
                                                outputStream = new SkippingLimitedOutputStream(requestOutputStream, position);
                                            }
                                        } else {
                                            outputStream = requestOutputStream;
                                        }
                                        final PushbackInputStream pushBackInputStream = new PushbackInputStream(meteredThrottledInputStream, readWriteBuffer.length + 1024);
                                        while (meteredThrottledInputStream != null) {
                                            final int len;
                                            final boolean eof;
                                            try {
                                                len = pushBackInputStream.read(readWriteBuffer);
                                                final int eofRead = pushBackInputStream.read();
                                                if (eofRead == -1) {
                                                    eof = true;
                                                    synchronized (connectedMeteredThrottledInputStream) {
                                                        meteredThrottledInputStream.setInputStream(new EmptyInputStream());
                                                        connectedMeteredThrottledInputStream.add(meteredThrottledInputStream);
                                                        meteredThrottledInputStream = null;
                                                    }
                                                    if (!delayedCleanup.isDelayerActive()) {
                                                        delayedCleanup.resetAndStart();
                                                    }
                                                } else {
                                                    eof = false;
                                                    pushBackInputStream.unread(eofRead);
                                                }
                                            } catch (IOException e) {
                                                synchronized (connectedMeteredThrottledInputStream) {
                                                    meteredThrottledInputStream.setInputStream(new EmptyInputStream());
                                                    connectedMeteredThrottledInputStream.add(meteredThrottledInputStream);
                                                    meteredThrottledInputStream = null;
                                                }
                                                requestLogger.log(e);
                                                if (onSegmentReadException(connection, e, fileBytesMap, retry, requestLogger)) {
                                                    continue retryLoop;
                                                } else {
                                                    throw e;
                                                }
                                            }
                                            if (len > 0) {
                                                if (eof) {
                                                    System.out.println("wat");
                                                }
                                                IOException ioe = null;
                                                int written = 0;
                                                ffmpeg.updateLastUpdateTimestamp(getRequest.getReadTimeout() + timeoutBuffer);
                                                try {
                                                    while (written != len) {
                                                        final int toWrite = Math.min(len - written, 1024);
                                                        outputStream.write(readWriteBuffer, written, toWrite);
                                                        written += toWrite;
                                                    }
                                                } catch (IOException e) {
                                                    requestLogger.exception("ReadWriteBuffer:" + written + "/" + len + "|Position:" + position, e);
                                                    ioe = e;
                                                }
                                                if (written > 0) {
                                                    try {
                                                        fileBytesMap.mark(position, written);
                                                        if (eof && fileBytesMap.getUnMarkedBytes() < 512) {
                                                            System.out.println("wat");
                                                            fileBytesMap.setFinalSize(fileBytesMap.getMarkedBytes());
                                                        }
                                                        if (segment != null) {
                                                            segment.setLoaded(fileBytesMap.getMarkedBytes());
                                                        }
                                                    } catch (IllegalArgumentException e) {
                                                        requestLogger.log(e);
                                                        if (fileBytesMap.getFinalSize() != -1) {
                                                            requestLogger.info("apply 'Ignore Content-Length' workaround!");
                                                            fileBytesMap.setFinalSize(-1);
                                                            fileBytesMap.mark(position, written);
                                                            if (segment != null) {
                                                                segment.setLoaded(fileBytesMap.getMarkedBytes());
                                                            }
                                                        } else {
                                                            throw e;
                                                        }
                                                    }
                                                }
                                                if (ioe != null) {
                                                    throw ioe;
                                                }
                                                position += len;
                                            } else if (len == -1) {
                                                break;
                                            }
                                        }
                                        outputStream.flush();
                                        outputStream.close();
                                        if (fileBytesMap.getSize() > 0) {
                                            requestOkay = fileBytesMap.getUnMarkedBytes() == 0;
                                        } else {
                                            requestOkay = true;
                                        }
                                        return true;
                                    } finally {
                                        try {
                                            if (segment != null) {
                                                final boolean validResponseCode = connection.getResponseCode() == 200 || connection.getResponseCode() == 206;
                                                synchronized (m3u8SegmentsStates) {
                                                    m3u8SegmentsStates.put(segment, validResponseCode ? M3U8SEGMENTSTATE.ONLINE : M3U8SEGMENTSTATE.FAILED);
                                                }
                                                if (validResponseCode) {
                                                    segment.setSize(Math.max(length, fileBytesMap.getSize()));
                                                }
                                                logger.severe("Segment(" + segmentIndex + "/" + (playList != null ? playList.size() : "~") + "):" + segment.getUrl() + "|Loaded:" + segment.getLoaded() + "|Size:" + segment.getSize());
                                            }
                                            requestLogger.info(fileBytesMap.toString());
                                        } finally {
                                            connection.disconnect();
                                        }
                                    }
                                } finally {
                                    br.disconnect();
                                }
                            }
                        } finally {
                            if (meteredThrottledInputStream != null) {
                                synchronized (connectedMeteredThrottledInputStream) {
                                    meteredThrottledInputStream.setInputStream(new EmptyInputStream());
                                    connectedMeteredThrottledInputStream.add(meteredThrottledInputStream);
                                    meteredThrottledInputStream = null;
                                }
                                delayedCleanup.resetAndStart();
                            }
                        }
                    } else {
                        requestLogger.info("unhandled request:" + request.getRequestedURL());
                    }
                } catch (Throwable e) {
                    requestLogger.log(e);
                } finally {
                    requestLogger.info("END:" + requestOkay + ">" + request.getRequestedURL());
                    logger.info(requestLogger.toString());
                    synchronized (requestsInProcess) {
                        requestsInProcess.decrementAndGet();
                        requestsInProcess.notifyAll();
                    }
                }
                return true;
            }
        });
    }

    protected boolean onSegmentReadException(final URLConnectionAdapter connection, final IOException e, final FileBytesMap fileBytesMap, final int retry, final LogSource logger) throws Exception {
        try {
            Thread.sleep(250 + (retry * 250));
            return true;
        } catch (InterruptedException ie) {
            if (logger != null) {
                logger.log(ie);
            }
            return false;
        }
    }

    protected boolean onSegmentConnectException(final URLConnectionAdapter connection, final IOException e, final FileBytesMap fileBytesMap, final int retry, Map<String, Object> retryMap, final LogSource logger) throws Exception {
        try {
            if (connection != null) {
                switch (connection.getResponseCode()) {
                case 999:
                case 504:
                case 503:
                case 502:
                    Thread.sleep(250 + (retry * 100));
                    return true;
                default:
                    return false;
                }
            } else {
                Thread.sleep(250 + (retry * 100));
                return true;
            }
        } catch (InterruptedException ie) {
            if (logger != null) {
                logger.log(ie);
            }
            return false;
        }
    }

    public long getBytesLoaded() {
        return bytesWritten.get();
    }

    @Override
    public ManagedThrottledConnectionHandler getManagedConnetionHandler() {
        return connectionHandler;
    }

    @Override
    public URLConnectionAdapter connect(Browser br) throws Exception {
        throw new WTFException("Not needed");
    }

    @Override
    public long getTotalLinkBytesLoadedLive() {
        return getBytesLoaded();
    }

    @Override
    public boolean startDownload() throws Exception {
        if (isEncrypted()) {
            throw new PluginException(LinkStatus.ERROR_FATAL, "Encrypted HLS(" + getEncryptionMethod() + ") is not supported!");
        }
        final List<File> requiredFiles = new ArrayList<File>();
        try {
            downloadable.setDownloadInterface(this);
            DownloadPluginProgress downloadPluginProgress = null;
            downloadable.setConnectionHandler(this.getManagedConnetionHandler());
            final DiskSpaceReservation reservation = downloadable.createDiskSpaceReservation();
            // TODO: update to handle 2x disk space usage (download + concat)
            try {
                if (!downloadable.checkIfWeCanWrite(new ExceptionRunnable() {
                    @Override
                    public void run() throws Exception {
                        downloadable.checkAndReserve(reservation);
                        requiredFiles.addAll(createOutputChannel());
                        try {
                            downloadable.lockFiles(requiredFiles.toArray(new File[0]));
                        } catch (FileIsLockedException e) {
                            downloadable.unlockFiles(requiredFiles.toArray(new File[0]));
                            throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS, null, e);
                        }
                    }
                }, null)) {
                    throw new SkipReasonException(SkipReason.INVALID_DESTINATION);
                }
                startTimeStamp = System.currentTimeMillis();
                downloadPluginProgress = new DownloadPluginProgress(downloadable, this, Color.GREEN.darker());
                downloadable.addPluginProgress(downloadPluginProgress);
                downloadable.setAvailable(AvailableStatus.TRUE);
                /* TODO: add resume to continue with unfinished playlist */
                runDownload();
                if (outputPartFiles.size() > 1) {
                    for (PartFile partFile : outputPartFiles) {
                        if (!isPartFileComplete(partFile)) {
                            logger.severe("PartFile:" + partFile.file + " not complete");
                            throw new PluginException(LinkStatus.ERROR_DOWNLOAD_INCOMPLETE, "PartFile:" + partFile.file + " not complete");
                        }
                    }
                    runConcat();
                }
            } finally {
                try {
                    downloadable.free(reservation);
                } catch (final Throwable e) {
                    LogSource.exception(logger, e);
                }
                try {
                    final long startTimeStamp = getStartTimeStamp();
                    if (startTimeStamp > 0) {
                        downloadable.addDownloadTime(System.currentTimeMillis() - getStartTimeStamp());
                    }
                } catch (final Throwable e) {
                }
                downloadable.removePluginProgress(downloadPluginProgress);
            }
            return onDownloadReady();
        } finally {
            downloadable.unlockFiles(requiredFiles.toArray(new File[0]));
            cleanupDownladInterface();
        }
    }

    protected void error(PluginException pluginException) {
        synchronized (this) {
            /* if we recieved external stop, then we dont have to handle errors */
            if (externalDownloadStop()) {
                return;
            }
            LogSource.exception(logger, pluginException);
            if (caughtPluginException == null) {
                caughtPluginException = pluginException;
            }
        }
        terminate();
    }

    protected boolean onDownloadReady() throws Exception {
        cleanupDownladInterface();
        if (!handleErrors()) {
            return false;
        } else {
            if (outputPartFiles.size() == 1) {
                final boolean renameOkay = downloadable.rename(outputPartFiles.get(0).file, outputCompleteFile);
                if (!renameOkay) {
                    error(new PluginException(LinkStatus.ERROR_DOWNLOAD_FAILED, _JDT.T.system_download_errors_couldnotrename(), LinkStatus.VALUE_LOCAL_IO_ERROR));
                }
            }
            return true;
        }
    }

    protected void cleanupDownladInterface() {
        try {
            downloadable.removeConnectionHandler(this.getManagedConnetionHandler());
        } catch (final Throwable e) {
        }
        try {
            final URLConnectionAdapter currentConnection = getConnection();
            if (currentConnection != null) {
                currentConnection.disconnect();
            }
        } catch (Throwable e) {
        }
    }

    protected boolean isM3U8SegmentLoaded(final M3U8Segment segment) {
        if (segment != null && segment.getLoaded() >= 0) {
            final long loaded = segment.getLoaded();
            final long size = Math.max(loaded, segment.getSize());
            final double done = (loaded * 100 / size) / 100d;
            return done > 0.9d;
        } else {
            return false;
        }
    }

    public static enum M3U8SEGMENTSTATE {
        NOT_LOADED,
        UNKNOWN,
        ONLINE,
        FAILED
    }

    protected M3U8SEGMENTSTATE getM3U8SegmentState(final M3U8Segment segment) {
        synchronized (m3u8SegmentsStates) {
            final M3U8SEGMENTSTATE ret = m3u8SegmentsStates.get(segment);
            return ret;
        }
    }

    private boolean handleErrors() throws PluginException {
        if (externalDownloadStop() && !isAcceptDownloadStopAsValidEnd()) {
            return false;
        } else if (caughtPluginException != null) {
            throw caughtPluginException;
        } else if (!isAcceptDownloadStopAsValidEnd()) {
            for (final M3U8Playlist m3u8Playlist : m3u8Playlists) {
                for (int index = 0; index < m3u8Playlist.size(); index++) {
                    final M3U8Segment segment = m3u8Playlist.getSegment(index);
                    if (!isM3U8SegmentLoaded(segment)) {
                        logger.severe("Segment(" + index + "/" + m3u8Playlist.size() + "):" + segment.getUrl() + "|Loaded:" + segment.getLoaded() + "|Size:" + segment.getSize());
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Segment:" + index + " not loaded");
                    }
                }
            }
        }
        for (final PartFile partFile : outputPartFiles) {
            if (!isPartFileComplete(partFile)) {
                logger.severe("PartFile:" + partFile.file + " not complete: exists:" + partFile.file.isFile() + "|size:" + partFile.file.length());
                throw new PluginException(LinkStatus.ERROR_DOWNLOAD_INCOMPLETE, "PartFile:" + partFile.file + " not complete");
            }
        }
        if (outputPartFiles.size() == 1 && isPartFileComplete(outputPartFiles.get(0))) {
            finalizeDownload(outputPartFiles.get(0).file);
            return true;
        } else if (isOutputFileComplete(0, outputCompleteFile)) {
            finalizeDownload(outputCompleteFile);
            return true;
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
    }

    protected void finalizeDownload(final File file) {
        final long fileSize = file.length();
        downloadable.setLinkStatus(LinkStatus.FINISHED);
        downloadable.setDownloadBytesLoaded(fileSize);
        downloadable.setVerifiedFileSize(fileSize);
        if (JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified()) {
            Long lastModifiedDate = null;
            if (downloadable instanceof DownloadLinkDownloadable) {
                final long lastModifiedTimestampDownloadLink = downloadable.getDownloadLink().getLastModifiedTimestamp();
                if (lastModifiedTimestampDownloadLink != -1) {
                    lastModifiedDate = lastModifiedTimestampDownloadLink;
                }
            }
            if (lastModifiedDate != null) {
                /* set desired/original lastModified timestamp */
                file.setLastModified(lastModifiedDate.longValue());
            }
        }
    }

    protected boolean isPartFileComplete(PartFile partFile) {
        return partFile.concatFlag.get() || (partFile.downloadFlag.get() && isOutputFileComplete(partFile.index, partFile.file));
    }

    protected boolean isOutputFileComplete(int index, File file) {
        return file.isFile() && file.length() > 1024;
    }

    private List<File> createOutputChannel() throws SkipReasonException {
        try {
            final List<File> requiredFiles = new ArrayList<File>();
            final String fileOutput = downloadable.getFileOutput();
            outputCompleteFile = new File(fileOutput);
            requiredFiles.add(outputCompleteFile);
            outputPartFiles.clear();
            if (m3u8Playlists.size() > 1) {
                for (int index = 0; index < m3u8Playlists.size(); index++) {
                    outputPartFiles.add(new PartFile(index, new File(downloadable.getFileOutputPart() + index + ".part")));
                }
            } else {
                outputPartFiles.add(new PartFile(0, new File(downloadable.getFileOutputPart())));
            }
            for (final PartFile partFile : outputPartFiles) {
                requiredFiles.add(partFile.file);
            }
            return requiredFiles;
        } catch (Exception e) {
            LogSource.exception(logger, e);
            throw new SkipReasonException(SkipReason.INVALID_DESTINATION, e);
        }
    }

    @Override
    public URLConnectionAdapter getConnection() {
        return currentConnection;
    }

    protected boolean checkAbortDownloadCondition(M3U8Playlist playList, M3U8Segment segment) {
        if (externalDownloadStop()) {
            return true;
        } else {
            final int index = playList.indexOf(segment);
            if (index != -1) {
                final Set<M3U8Segment> failed = new HashSet<M3U8Segment>();
                for (int checkPreviousSegmentIndex = index - 1; checkPreviousSegmentIndex >= 0; checkPreviousSegmentIndex--) {
                    final M3U8Segment check = playList.getSegment(checkPreviousSegmentIndex);
                    if (getM3U8SegmentState(check) != null && !isM3U8SegmentLoaded(check)) {
                        failed.add(check);
                        if (failed.size() > 5) {
                            logger.info("Too many failed segments:" + failed);
                            terminate();
                            return true;
                        }
                    }
                }
            }
            return false;
        }
    }

    @Override
    public void stopDownload() {
        if (abort.getAndSet(true) == false) {
            logger.info("externalStop recieved");
            terminate();
        }
    }

    private final AtomicBoolean abort                        = new AtomicBoolean(false);
    private final AtomicBoolean terminated                   = new AtomicBoolean(false);
    /**
     * if set to true, external Stops will finish and rename the file, else the file will be handled as unfinished. This is usefull for live
     * streams since
     */
    private boolean             acceptDownloadStopAsValidEnd = false;

    @Override
    public boolean externalDownloadStop() {
        return abort.get();
    }

    @Override
    public long getStartTimeStamp() {
        return startTimeStamp;
    }

    @Override
    public void close() {
        final URLConnectionAdapter currentConnection = getConnection();
        if (currentConnection != null) {
            currentConnection.disconnect();
        }
    }

    @Override
    public Downloadable getDownloadable() {
        return downloadable;
    }

    @Override
    public boolean isResumedDownload() {
        return false;
    }

    public void setAcceptDownloadStopAsValidEnd(boolean b) {
        this.acceptDownloadStopAsValidEnd = b;
    }

    public boolean isAcceptDownloadStopAsValidEnd() {
        return acceptDownloadStopAsValidEnd;
    }
}
