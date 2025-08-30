package org.jdownloader.controlling.ffmpeg;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.plugins.PluginProgress;
import jd.plugins.download.raf.FileBytesMap;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.resources.AWUTheme;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.Application;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.locale._AWU;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.NullOutputStream;
import org.appwork.utils.net.httpserver.HttpServer;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.ARCHFamily;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.controlling.ffmpeg.FFMpegException.ERROR;
import org.jdownloader.downloader.hls.M3U8Playlist;
import org.jdownloader.downloader.hls.M3U8Playlist.M3U8Segment;

public abstract class AbstractFFmpegBinary {
    public static enum FLAGTYPE {
        LIB,
        FORMAT,
        CODEC
    };

    public static enum FLAG {
        OPUS(FLAGTYPE.CODEC, "D.A...\\s*opus"), // encode/decode
        VORBIS(FLAGTYPE.CODEC, "D.A...\\s*vorbis"), // encode/decode
        AV1(FLAGTYPE.CODEC, "D.V...\\s*av1"), // encode/decode
        WEBM(FLAGTYPE.FORMAT, "E\\s*(webm|matroska,webm)"), // mux
        DASH(FLAGTYPE.FORMAT, "E\\s*dash"), // mux
        HLS(FLAGTYPE.FORMAT, "D\\s*(hls|applehttp)");// demux

        private final Pattern  pattern;
        private final FLAGTYPE type;

        private FLAG(FLAGTYPE type, final String pattern) {
            this.pattern = Pattern.compile(pattern);
            this.type = type;
        }

        public FLAGTYPE getType() {
            return type;
        }

        public boolean isSupported(final String string) {
            return pattern.matcher(string).find();
        }
    }

    public abstract LogInterface getLogger();

    public Set<FLAG> getSupportedFlags() {
        final LogInterface logger = getLogger();
        try {
            HashSet<FLAG> ret = null;
            for (final FLAGTYPE flagType : FLAGTYPE.values()) {
                final Set<FLAG> supported = getSupported(flagType);
                if (supported != null) {
                    if (ret == null) {
                        ret = new HashSet<AbstractFFmpegBinary.FLAG>();
                    }
                    ret.addAll(supported);
                }
            }
            if (ret != null) {
                logger.info("ffmpeg: " + ret);
            }
            return ret;
        } catch (Throwable e) {
            logger.log(e);
        }
        return null;
    }

    protected final FFmpegSetup config = JsonConfig.create(FFmpegSetup.class);
    protected final Browser     sourceBrowser;

    public AbstractFFmpegBinary(Browser br) {
        this.sourceBrowser = br;
    }

    protected Set<FLAG> getSupported(FLAGTYPE flagType) throws InterruptedException, IOException {
        final String fullPath = getFullPath();
        if (fullPath != null) {
            final int timeout = 10 * 1000;
            final File root = Application.getApplicationRoot();
            final String ret[];
            switch (flagType) {
            case CODEC:
                ret = execute(timeout, null, root, fullPath, "-codecs");
                break;
            case FORMAT:
                ret = execute(timeout, null, root, fullPath, "-formats");
                break;
            default:
            case LIB:
                ret = execute(timeout, null, root, fullPath);
                break;
            }
            if (ret != null) {
                final Set<FLAG> supported = new HashSet<FLAG>();
                for (final FLAG flag : FLAG.values()) {
                    if (flag.getType() == flagType) {
                        for (final String output : ret) {
                            if (flag.isSupported(output)) {
                                supported.add(flag);
                                break;
                            }
                        }
                    }
                }
                return supported;
            }
        }
        return null;
    }

    protected Boolean isSupported(FLAG flag) throws InterruptedException, IOException {
        final String fullPath = getFullPath();
        if (fullPath != null) {
            final int timeout = 10 * 1000;
            final File root = Application.getApplicationRoot();
            final String ret[];
            switch (flag.getType()) {
            case CODEC:
                ret = execute(timeout, null, root, fullPath, "-codecs");
                break;
            case FORMAT:
                ret = execute(timeout, null, root, fullPath, "-formats");
                break;
            default:
            case LIB:
                ret = execute(timeout, null, root, fullPath);
                break;
            }
            if (ret != null) {
                for (final String output : ret) {
                    if (flag.isSupported(output)) {
                        return true;
                    }
                }
                return false;
            }
        }
        return null;
    }

    protected Browser getRequestBrowser() {
        final Browser ret = sourceBrowser.cloneBrowser();
        ret.setConnectTimeout(30 * 1000);
        ret.setReadTimeout(30 * 1000);
        ret.setFollowRedirects(true);
        return ret;
    }

    protected String[] execute(final int timeout, PluginProgress progess, File runin, String... cmds) throws InterruptedException, IOException {
        final ProcessBuilder pb = ProcessBuilderFactory.create(cmds);
        if (runin != null) {
            pb.directory(runin);
        }
        final Process process = pb.start();
        final AtomicBoolean processExitedFlag = new AtomicBoolean(false);
        final AccessibleByteArrayOutputStream stdout = new AccessibleByteArrayOutputStream();
        final AccessibleByteArrayOutputStream stderr = new AccessibleByteArrayOutputStream();
        final Thread stdoutThread = new Thread("ffmpegReader:stdout") {
            public void run() {
                try {
                    readInputStreamToString(stdout, processExitedFlag, process.getInputStream(), true);
                } catch (Throwable e) {
                    getLogger().log(e);
                }
            }
        };
        final Thread stderrThread = new Thread("ffmpegReader:stderr") {
            public void run() {
                try {
                    readInputStreamToString(stderr, processExitedFlag, process.getErrorStream(), false);
                } catch (Throwable e) {
                    getLogger().log(e);
                }
            }
        };
        try {
            stdoutThread.start();
            stderrThread.start();
            if (timeout > 0) {
                final AtomicBoolean timeoutReached = new AtomicBoolean(false);
                final AtomicBoolean processAlive = new AtomicBoolean(true);
                final Thread timouter = new Thread("ffmpegReaderTimeout") {
                    public void run() {
                        try {
                            Thread.sleep(timeout);
                        } catch (InterruptedException e) {
                            return;
                        }
                        if (processAlive.compareAndSet(true, false)) {
                            timeoutReached.set(true);
                            process.destroy();
                        }
                    }
                };
                timouter.start();
                getLogger().info("ExitCode1: " + process.waitFor());
                processAlive.set(false);
                processExitedFlag.set(true);
                synchronized (processExitedFlag) {
                    processExitedFlag.notifyAll();
                }
                timouter.interrupt();
                if (timeoutReached.get()) {
                    throw new InterruptedException("Timeout!");
                }
                waitForReader(getLogger(), stdoutThread, 1000);
                waitForReader(getLogger(), stderrThread, 1000);
                final String lastStdout;
                synchronized (stdout) {
                    lastStdout = stdout.toString("UTF-8");
                }
                final String lastStderr;
                synchronized (stderr) {
                    lastStderr = stderr.toString("UTF-8");
                }
                return new String[] { lastStdout, lastStderr };
            } else {
                getLogger().info("ExitCode2: " + process.waitFor());
                processExitedFlag.set(true);
                synchronized (processExitedFlag) {
                    processExitedFlag.notifyAll();
                }
                waitForReader(getLogger(), stdoutThread, 1000);
                waitForReader(getLogger(), stderrThread, 1000);
                final String lastStdout;
                synchronized (stdout) {
                    lastStdout = stdout.toString("UTF-8");
                }
                final String lastStderr;
                synchronized (stderr) {
                    lastStderr = stderr.toString("UTF-8");
                }
                return new String[] { lastStdout, lastStderr };
            }
        } finally {
            processExitedFlag.set(true);
            synchronized (processExitedFlag) {
                processExitedFlag.notifyAll();
            }
        }
    }

    protected final class AccessibleByteArrayOutputStream extends ByteArrayOutputStream {
        public AccessibleByteArrayOutputStream(final int size) {
            super(size);
        }

        public AccessibleByteArrayOutputStream() {
            super();
        }

        public final synchronized byte[] getBuf() {
            return this.buf;
        }
    }

    // TODO: rewrite to use ProcessStreamReader or AbstractLineHandler
    private void readInputStreamToString(final AccessibleByteArrayOutputStream bos, final AtomicBoolean processExitedFlag, final InputStream fis, final boolean isStdout) throws IOException {
        final LogInterface logger = getLogger();
        long size = 0;
        try {
            final byte[] buf = new byte[8192];
            final boolean isInstantFlush;
            if (logger != null && logger instanceof LogSource) {
                isInstantFlush = ((LogSource) logger).isInstantFlush();
            } else {
                isInstantFlush = false;
            }
            int lastReadPosition = 0;
            int lastSize = 0;
            while (true) {
                if (fis.available() > 0 || processExitedFlag.get()) {
                    final int read = fis.read(buf);
                    if (read == -1) {
                        return;
                    } else if (read > 0) {
                        size += read;
                        synchronized (bos) {
                            if (bos.size() < lastSize) {
                                // external AccessibleByteArrayOutputStream manipulation
                                lastReadPosition = 0;
                            }
                            bos.write(buf, 0, read);
                            lastSize = bos.size();
                            final byte[] array = bos.getBuf();
                            for (int index = lastReadPosition; index < bos.size(); index++) {
                                if (array[index] == 10 || array[index] == 13) {
                                    final int length = index - 1 - lastReadPosition;
                                    if (length > 0) {
                                        final String line = new String(array, lastReadPosition, length, "UTF-8");
                                        if (isInstantFlush) {
                                            logger.info(isStdout + "|" + line);
                                        }
                                        parseLine(isStdout, line);
                                    }
                                    // index is \r or \n, so index +1
                                    lastReadPosition = index + 1;
                                }
                            }
                        }
                    } else {
                        synchronized (processExitedFlag) {
                            if (!processExitedFlag.get()) {
                                processExitedFlag.wait(100);
                            }
                        }
                    }
                } else {
                    synchronized (processExitedFlag) {
                        if (!processExitedFlag.get()) {
                            processExitedFlag.wait(100);
                        }
                    }
                }
            }
        } catch (IOException e) {
            if (!"Stream closed".equals(e.getMessage())) {
                throw e;
            }
        } catch (Throwable e) {
            throw new IOException(e);
        } finally {
            if (isStdout) {
                logger.info("Read(Stdout):" + size + "|Exited:" + processExitedFlag.get());
            } else {
                logger.info("Read(Stderr):" + size + "|Exited:" + processExitedFlag.get());
            }
        }
    }

    protected void parseLine(boolean isStdout, String line) {
    }

    private String       path;
    protected HttpServer server;

    protected String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public boolean isAvailable() {
        return getFullPath() != null;
    }

    public boolean isCompatible() {
        return true;
    }

    public String getFullPath() {
        try {
            final String path = getPath();
            if (StringUtils.isEmpty(path)) {
                throw new Exception("path is empty!");
            }
            File file = new File(path);
            if (!file.isAbsolute()) {
                file = Application.getResource(path);
            }
            if (!file.exists()) {
                throw new Exception("doesn't exist" + file.getAbsolutePath());
            } else if (!file.isFile()) {
                throw new Exception("not a file:" + file.getAbsolutePath());
            }
            if (Application.getJavaVersion() >= Application.JAVA16) {
                if (!file.canExecute()) {
                    file.setExecutable(true);
                }
            }
            return file.getCanonicalPath();
        } catch (Exception e) {
            getLogger().log(e);
            return null;
        }
    }

    /**
     * @param out
     * @param videoIn
     * @param audioIn
     * @param map
     *            TODO
     * @param mc
     * @return
     */
    public ArrayList<String> fillCommand(String out, String videoIn, String audioIn, HashMap<String, String[]> map, String... mc) {
        ArrayList<String> commandLine = new ArrayList<String>();
        commandLine.add(getFullPath());
        if (CrossSystem.isWindows()) {
            // https://msdn.microsoft.com/en-us/library/aa365247.aspx
            if (videoIn != null && videoIn.length() > 259) {
                videoIn = "\\\\?\\" + videoIn;
            }
            if (audioIn != null && audioIn.length() > 259) {
                audioIn = "\\\\?\\" + audioIn;
            }
            if (out != null && out.length() > 259) {
                out = "\\\\?\\" + out;
            }
        }
        main: for (int i = 0; i < mc.length; i++) {
            String param = mc[i];
            param = param.replace("%video", videoIn == null ? "" : videoIn);
            param = param.replace("%audio", audioIn == null ? "" : audioIn);
            param = param.replace("%out", out);
            if (map != null) {
                for (Entry<String, String[]> es : map.entrySet()) {
                    if (param.equals(es.getKey())) {
                        for (String s : es.getValue()) {
                            commandLine.add(s);
                        }
                        continue main;
                    }
                }
            }
            commandLine.add(param);
        }
        return commandLine;
    }

    protected long processID;

    protected void closePipe() {
        final HttpServer server = this.server;
        this.server = null;
        if (server != null) {
            server.stop();
        }
    }

    private final AtomicLong lastUpdateTimeStamp = new AtomicLong(0);

    public void updateLastUpdateTimestamp() {
        updateLastUpdateTimestamp(0);
    }

    public void updateLastUpdateTimestamp(long add) {
        lastUpdateTimeStamp.set(System.currentTimeMillis() + Math.max(0, add));
    }

    public long getLastUpdateTimestamp() {
        return lastUpdateTimeStamp.get();
    }

    protected void initPipe(final String m3u8URL) throws IOException {
        if (sourceBrowser == null) {
            return;
        }
        server = new HttpServer(0);
        server.setLocalhostOnly(true);
        final HttpServer finalServer = server;
        server.start();
        final AtomicReference<M3U8Playlist> m3u8 = new AtomicReference<M3U8Playlist>();
        final LogInterface logger = getLogger();
        finalServer.registerRequestHandler(new HttpRequestHandler() {
            final byte[] readBuf = new byte[512];

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

            @Override
            public boolean onPostRequest(PostRequest request, HttpResponse response) {
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
                }
                return false;
            }

            @Override
            public boolean onGetRequest(GetRequest request, HttpResponse response) {
                boolean requestOkay = false;
                try {
                    logger.info("START " + request.getRequestedURL());
                    logger.info(request.toString());
                    if (!validateID(request)) {
                        return false;
                    }
                    if ("/m3u8.m3u8".equals(request.getRequestedPath())) {
                        updateLastUpdateTimestamp();
                        final Browser br = getRequestBrowser();
                        // work around for longggggg m3u pages
                        final int was = br.getLoadLimit();
                        // lets set the connection limit to our required request
                        br.setLoadLimit(Integer.MAX_VALUE);
                        final String playlist;
                        try {
                            playlist = br.getPage(m3u8URL);
                        } finally {
                            // set it back!
                            br.setLoadLimit(was);
                        }
                        updateLastUpdateTimestamp();
                        response.setResponseCode(HTTPConstants.ResponseCode.get(br.getRequest().getHttpConnection().getResponseCode()));
                        final StringBuilder sb = new StringBuilder();
                        boolean containsEndList = false;
                        final M3U8Playlist m3u8Playlists = new M3U8Playlist();
                        long lastSegmentDuration = -1;
                        for (final String line : Regex.getLines(playlist)) {
                            if (StringUtils.isEmpty(line)) {
                                continue;
                            }
                            if (StringUtils.startsWithCaseInsensitive(line, "concat") || StringUtils.contains(line, "file:")) {
                                // http://habrahabr.ru/company/mailru/blog/274855/
                                logger.severe("possibly malicious: " + line);
                            } else if (line.matches("^https?://.+") || !line.trim().startsWith("#")) {
                                final String segmentURL = br.getURL(line).toString();
                                if (!m3u8Playlists.containsSegmentURL(segmentURL)) {
                                    final int index = m3u8Playlists.addSegment(segmentURL, lastSegmentDuration);
                                    if (sb.length() > 0) {
                                        sb.append("\n");
                                    }
                                    sb.append("http://" + finalServer.getServerAddress() + "/download.ts?id=" + processID + "&ts_index=" + index);
                                }
                                lastSegmentDuration = -1;
                            } else {
                                if (line.startsWith("#EXTINF:")) {
                                    final String duration = new Regex(line, "#EXTINF:(\\d+(\\.\\d+)?)").getMatch(0);
                                    if (duration != null) {
                                        if (duration.contains(".")) {
                                            lastSegmentDuration = Long.parseLong(duration.replace(".", ""));
                                        } else {
                                            lastSegmentDuration = Long.parseLong(duration) * 1000;
                                        }
                                    }
                                } else if ("#EXT-X-ENDLIST".equals(line)) {
                                    containsEndList = true;
                                }
                                if (sb.length() > 0) {
                                    sb.append("\n");
                                }
                                sb.append(line);
                            }
                        }
                        if (!containsEndList) {
                            if (sb.length() > 0) {
                                sb.append("\n");
                            }
                            sb.append("#EXT-X-ENDLIST");
                            sb.append("\n\n");
                        }
                        m3u8.set(m3u8Playlists);
                        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, br.getRequest().getHttpConnection().getContentType()));
                        byte[] bytes = sb.toString().getBytes("UTF-8");
                        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(bytes.length)));
                        OutputStream out = response.getOutputStream(true);
                        out.write(bytes);
                        out.flush();
                        requestOkay = true;
                        return true;
                    } else if ("/download".equals(request.getRequestedPath()) || "/download.ts".equals(request.getRequestedPath())) {
                        final String url = request.getParameterbyKey("url");
                        final String indexString = request.getParameterbyKey("ts_index");
                        if (indexString == null && url == null) {
                            return false;
                        }
                        final String downloadURL;
                        final M3U8Segment segment;
                        if (url != null) {
                            segment = null;
                            downloadURL = url;
                        } else {
                            final M3U8Playlist m3u8Playlists = m3u8.get();
                            if (m3u8Playlists == null) {
                                return false;
                            }
                            try {
                                final int index = Integer.parseInt(indexString);
                                segment = m3u8Playlists.getSegment(index);
                                if (segment == null) {
                                    throw new IndexOutOfBoundsException("Unknown segment:" + index);
                                } else {
                                    logger.info("Forward segment:" + (index + 1) + "/" + m3u8Playlists.size());
                                    downloadURL = segment.getUrl();
                                }
                            } catch (final NumberFormatException e) {
                                logger.log(e);
                                return false;
                            } catch (final IndexOutOfBoundsException e) {
                                logger.log(e);
                                return false;
                            }
                        }
                        OutputStream outputStream = null;
                        final FileBytesMap fileBytesMap = new FileBytesMap();
                        final Browser br = getRequestBrowser();
                        retryLoop: for (int retry = 0; retry < 10; retry++) {
                            try {
                                br.disconnect();
                            } catch (final Throwable e) {
                            }
                            final jd.http.requests.GetRequest getRequest = new jd.http.requests.GetRequest(downloadURL) {
                                protected boolean isBrotliAcceptEncodingEnabled() {
                                    return false;
                                };
                            };
                            if (fileBytesMap.getFinalSize() > 0) {
                                logger.info("Resume(" + retry + "): " + fileBytesMap.toString());
                                final List<Long[]> unMarkedAreas = fileBytesMap.getUnMarkedAreas();
                                getRequest.getHeaders().put(HTTPConstants.HEADER_REQUEST_RANGE, "bytes=" + unMarkedAreas.get(0)[0] + "-" + unMarkedAreas.get(0)[1]);
                            }
                            URLConnectionAdapter connection = null;
                            try {
                                updateLastUpdateTimestamp();
                                connection = br.openRequestConnection(getRequest);
                                if (connection.getResponseCode() != 200 && connection.getResponseCode() != 206) {
                                    throw new IOException("ResponseCode must be 200 or 206!");
                                }
                            } catch (IOException e) {
                                logger.log(e);
                                if (connection == null || connection.getResponseCode() == 504) {
                                    Thread.sleep(250 + (retry * 50));
                                    continue retryLoop;
                                } else {
                                    return false;
                                }
                            }
                            updateLastUpdateTimestamp();
                            final byte[] readWriteBuffer = new byte[32 * 1024];
                            final long length = connection.getCompleteContentLength();
                            try {
                                if (outputStream == null) {
                                    response.setResponseCode(HTTPConstants.ResponseCode.get(br.getRequest().getHttpConnection().getResponseCode()));
                                    if (length > 0) {
                                        fileBytesMap.setFinalSize(length);
                                        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, Long.toString(length)));
                                    }
                                    response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, connection.getContentType()));
                                    outputStream = response.getOutputStream(true);
                                }
                                long position = fileBytesMap.getMarkedBytes();
                                final InputStream is = connection.getInputStream();
                                while (true) {
                                    int len = -1;
                                    try {
                                        len = is.read(readWriteBuffer);
                                    } catch (IOException e) {
                                        if (fileBytesMap.getFinalSize() > 0) {
                                            Thread.sleep(250 + (retry * 50));
                                            continue retryLoop;
                                        } else {
                                            throw e;
                                        }
                                    }
                                    if (len > 0) {
                                        updateLastUpdateTimestamp();
                                        outputStream.write(readWriteBuffer, 0, len);
                                        fileBytesMap.mark(position, len);
                                        if (segment != null) {
                                            segment.setLoaded(fileBytesMap.getMarkedBytes());
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
                                if (segment != null && (connection.getResponseCode() == 200 || connection.getResponseCode() == 206)) {
                                    segment.setSize(Math.max(connection.getCompleteContentLength(), fileBytesMap.getSize()));
                                }
                                connection.disconnect();
                            }
                        }
                    }
                } catch (InterruptedException e) {
                    e.printStackTrace();
                } catch (IOException e) {
                    e.printStackTrace();
                } finally {
                    logger.info("END:" + requestOkay + ">" + request.getRequestedURL());
                }
                return true;
            }
        });
    }

    private final static HashMap<String, String> DEFAULT_FORMAT_BY_EXTENSION;
    static {
        DEFAULT_FORMAT_BY_EXTENSION = new HashMap<String, String>();
        DEFAULT_FORMAT_BY_EXTENSION.put(".mp3", "mp3");
        DEFAULT_FORMAT_BY_EXTENSION.put(".flac", "flac");
        DEFAULT_FORMAT_BY_EXTENSION.put(".mp4", "mp4");
        DEFAULT_FORMAT_BY_EXTENSION.put(".mpg", "mpeg");
        DEFAULT_FORMAT_BY_EXTENSION.put(".mov", "mov");
        DEFAULT_FORMAT_BY_EXTENSION.put(".avi", "avi");
        DEFAULT_FORMAT_BY_EXTENSION.put(".flv", "flv");
        DEFAULT_FORMAT_BY_EXTENSION.put(".ogg", "ogg");
        DEFAULT_FORMAT_BY_EXTENSION.put(".opus", "opus");
        DEFAULT_FORMAT_BY_EXTENSION.put(".mkv", "matroska");
        DEFAULT_FORMAT_BY_EXTENSION.put(".webm", "webm");
        DEFAULT_FORMAT_BY_EXTENSION.put(".mpeg", "mpeg");
        DEFAULT_FORMAT_BY_EXTENSION.put(".aac", "adts");
        DEFAULT_FORMAT_BY_EXTENSION.put(".wav", "wav");
        DEFAULT_FORMAT_BY_EXTENSION.put(".m4a", "ipod");
        DEFAULT_FORMAT_BY_EXTENSION.put(".m4v", "ipod");
    }

    public boolean requiresAdtstoAsc(final String format) {
        return ("ipod".equalsIgnoreCase(format) || "mp4".equalsIgnoreCase(format) || "m4v".equalsIgnoreCase(format) || "m4a".equalsIgnoreCase(format) || "mov".equalsIgnoreCase(format) || "flv".equalsIgnoreCase(format));
    }

    public String getDefaultFormatByFileName(final String fileName) {
        String checkForExtension = fileName;
        int dotIndex = checkForExtension.lastIndexOf(".");
        while (dotIndex > 0) {
            final String ext = checkForExtension.substring(dotIndex, checkForExtension.length()).toLowerCase(Locale.ENGLISH);
            final String format = DEFAULT_FORMAT_BY_EXTENSION.get(ext);
            if (format != null) {
                return format;
            }
            checkForExtension = checkForExtension.substring(0, dotIndex);
            dotIndex = checkForExtension.lastIndexOf(".");
        }
        return null;
    }

    private boolean isLocalhost(List<String> commandLine) {
        if (commandLine != null) {
            for (final String cmd : commandLine) {
                if (StringUtils.containsIgnoreCase(cmd, "127.0.0.1") || StringUtils.contains(cmd, "[::1]") || StringUtils.contains(cmd, "[0:0:0:0:0:0:0:1]")) {
                    return true;
                } else if (StringUtils.containsIgnoreCase(cmd, "localhost")) {
                    return true;
                }
            }
        }
        return false;
    }

    private static final AtomicBoolean ROSETTA_2_DIALOG_SHOWN = new AtomicBoolean(false);

    public static void showRosetta2Dialog(final AbstractFFmpegBinary ffmpeg) throws InterruptedException {
        if (ROSETTA_2_DIALOG_SHOWN.get() == false) {
            final Thread thread = new Thread("RosettaDialog") {
                {
                    setDaemon(true);
                }

                public void run() {
                    if (ROSETTA_2_DIALOG_SHOWN.compareAndSet(false, true)) {
                        // check if rosetta is installed: "/usr/bin/pgrep oahd" returns process id if installed
                        try {
                            try {
                                Dialog.getInstance().showConfirmDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, _AWU.T.DIALOG_MESSAGE_TITLE(), "Intel ffmpeg binary requires Rosetta 2 to be installed! Do you want to install Rosetta 2 now?", AWUTheme.I().getIcon(Dialog.ICON_WARNING, 32), null, null, "test3");
                                final ProcessBuilder pb = ProcessBuilderFactory.create(Arrays.asList(new String[] { "/usr/sbin/softwareupdate", "--install-rosetta", "--agree-to-license" }));
                                ProcessBuilderFactory.runCommand(pb, new NullOutputStream(), new NullOutputStream());
                            } catch (Exception e) {
                                if (ffmpeg == null) {
                                    e.printStackTrace();
                                } else {
                                    ffmpeg.getLogger().log(e);
                                }
                            }
                        } finally {
                            ROSETTA_2_DIALOG_SHOWN.compareAndSet(true, false);
                        }
                    }
                };
            };
            thread.start();
            thread.join();
        }
    }

    private void waitForReader(LogInterface logger, Thread thread, int waitTimeout) throws InterruptedException {
        if (thread.isAlive()) {
            logger.info("Wait for Reader:" + thread);
            thread.join(waitTimeout);
            logger.info("Reader:" + thread + " still alive?" + thread.isAlive());
        }
    }

    public String runCommand(FFMpegProgress progress, List<String> commandLine) throws IOException, InterruptedException, FFMpegException {
        final LogInterface logger = getLogger();
        logger.info("runCommand(ProcessBuilderFactory):" + commandLine);
        final ProcessBuilder pb = ProcessBuilderFactory.create(commandLine);
        final Map<String, String> env = pb.environment();
        if (isLocalhost(commandLine) && env != null) {
            logger.info("unset SysEnv:https_proxy=" + env.remove("https_proxy"));
            logger.info("unset SysEnv:http_proxy=" + env.remove("http_proxy"));
        }
        logger.info("runCommand(ProcessBuilder):" + pb.command());
        final Process process;
        try {
            process = pb.start();
        } catch (IOException e) {
            if (CrossSystem.isMac() && ARCHFamily.ARM.equals(CrossSystem.getARCHFamily()) && StringUtils.contains(e.getMessage(), "Bad CPU type in executable")) {
                showRosetta2Dialog(this);
                throw new FFMpegException("Rosetta required to use intel ffmpeg binary", e);
            } else {
                throw e;
            }
        }
        final AccessibleByteArrayOutputStream stdout = new AccessibleByteArrayOutputStream();
        final AccessibleByteArrayOutputStream stderr = new AccessibleByteArrayOutputStream();
        final AtomicBoolean processExitedFlag = new AtomicBoolean(false);
        try {
            final Thread stdoutThread = new Thread("ffmpegReader:stdout") {
                public void run() {
                    try {
                        readInputStreamToString(stdout, processExitedFlag, process.getInputStream(), true);
                    } catch (Throwable e) {
                        logger.log(e);
                    }
                }
            };
            final Thread stderrThread = new Thread("ffmpegReader:stderr") {
                public void run() {
                    try {
                        readInputStreamToString(stderr, processExitedFlag, process.getErrorStream(), false);
                    } catch (Throwable e) {
                        logger.log(e);
                    }
                }
            };
            stdoutThread.start();
            stderrThread.start();
            updateLastUpdateTimestamp();
            long lastDuration = -1;
            long lastRead = -1;
            String lastNonEmptyStderr = null;
            while (true) {
                long read = 0;
                synchronized (stdout) {
                    read = stdout.size();
                }
                final String currentStderr;
                synchronized (stderr) {
                    read += stderr.size();
                    int lastRN = 0;
                    final byte[] array = stderr.getBuf();
                    for (int index = 0; index < stderr.size(); index++) {
                        if (array[index] == 10 || array[index] == 13) {
                            lastRN = index;
                        }
                    }
                    if (lastRN > 0) {
                        updateLastUpdateTimestamp();
                        currentStderr = new String(array, 0, lastRN, "UTF-8");
                        final int length = stderr.size() - lastRN - 1;
                        if (length == 0) {
                            stderr.reset();
                        } else {
                            final byte[] tmpBuf = stderr.toByteArray();
                            stderr.reset();
                            stderr.write(tmpBuf, lastRN, length);
                        }
                    } else {
                        currentStderr = null;
                    }
                }
                if (StringUtils.isNotEmpty(currentStderr)) {
                    lastNonEmptyStderr = currentStderr;
                    updateLastUpdateTimestamp();
                }
                if (read != lastRead) {
                    updateLastUpdateTimestamp();
                    lastRead = read;
                }
                final String duration = new Regex(currentStderr, "Duration\\: (.*?).?\\d*?\\, start").getMatch(0);
                if (duration != null) {
                    lastDuration = formatStringToMilliseconds(duration);
                }
                if (lastDuration > 0) {
                    final String[] times = new Regex(currentStderr, "time=(.*?).?\\d*? ").getColumn(0);
                    if (times != null && times.length > 0) {
                        final long msDone = formatStringToMilliseconds(times[times.length - 1]);
                        if (progress != null) {
                            progress.updateValues(msDone, lastDuration);
                        }
                    }
                }
                try {
                    String lastStderr;
                    String lastStdout;
                    int stderrSize;
                    int stdoutSize;
                    synchronized (stderr) {
                        stderrSize = stderr.size();
                        if (lastNonEmptyStderr != null) {
                            lastStderr = lastNonEmptyStderr + stderr.toString("UTF-8");
                        } else {
                            lastStderr = stderr.toString("UTF-8");
                        }
                    }
                    synchronized (stdout) {
                        lastStdout = stdout.toString("UTF-8");
                        stdoutSize = stdout.size();
                    }
                    final int exitCode = exitProcess(process, lastStdout, lastStderr);
                    processExitedFlag.set(true);
                    synchronized (processExitedFlag) {
                        processExitedFlag.notifyAll();
                    }
                    waitForReader(logger, stdoutThread, 1000);
                    // update lastStderr and lastStdout
                    synchronized (stderr) {
                        stderrSize = stderr.size();
                        if (lastNonEmptyStderr != null) {
                            lastStderr = lastNonEmptyStderr + stderr.toString("UTF-8");
                        } else {
                            lastStderr = stderr.toString("UTF-8");
                        }
                    }
                    synchronized (stdout) {
                        lastStdout = stdout.toString("UTF-8");
                        stdoutSize = stdout.size();
                    }
                    logger.info("LastStdout(Exited):(" + stdoutSize + ")" + lastStdout);
                    logger.info("LastStderr(Exited):(" + stderrSize + ")" + lastStderr);
                    logger.info("ExitCode:" + exitCode);
                    final boolean okay = exitCode == 0;
                    if (!okay) {
                        if (StringUtils.containsIgnoreCase(lastStderr, "No such file or directory") || StringUtils.containsIgnoreCase(lastStderr, "Invalid argument")) {
                            throw new FFMpegException("FFmpeg Failed: path too long?", lastStdout, lastStderr, ERROR.PATH_LENGTH);
                        } else if (StringUtils.containsIgnoreCase(lastStderr, "Unrecognized option 'c:v'") || StringUtils.containsIgnoreCase(lastStderr, "Unrecognized option '-c:v'")) {
                            throw new FFMpegException("FFmpeg Failed: version too old", lastStdout, lastStderr, ERROR.TOO_OLD);
                        } else if (StringUtils.containsIgnoreCase(lastStderr, "No space left on device") && StringUtils.containsIgnoreCase(lastStderr, "Error writing")) {
                            throw new FFMpegException("FFmpeg Failed: disk full", lastStdout, lastStderr, ERROR.DISK_FULL);
                        } else if (StringUtils.containsIgnoreCase(lastStderr, ": Protocol not found'") || StringUtils.containsIgnoreCase(lastStderr, "Did you mean file:http")) {
                            throw new FFMpegException("FFmpeg Failed: version does not support http protocol", lastStdout, lastStderr, ERROR.INCOMPATIBLE);
                        } else {
                            throw new FFMpegException("FFmpeg Failed", lastStdout, lastStderr);
                        }
                    } else {
                        return lastStdout;
                    }
                } catch (IllegalThreadStateException e) {
                    // still running;
                }
                if (System.currentTimeMillis() - getLastUpdateTimestamp() > getLastUpdateTimestampTimeout()) {
                    // 60 seconds without any ffmpeg update. interrupt
                    final String lastStderr;
                    final int stderrSize;
                    synchronized (stderr) {
                        stderrSize = stderr.size();
                        if (lastNonEmptyStderr != null) {
                            lastStderr = lastNonEmptyStderr + stderr.toString("UTF-8");
                        } else {
                            lastStderr = stderr.toString("UTF-8");
                        }
                    }
                    final String lastStdout;
                    final int stdoutSize;
                    synchronized (stdout) {
                        lastStdout = stdout.toString("UTF-8");
                        stdoutSize = stdout.size();
                    }
                    logger.info("LastStdout(Timeout):(" + stdoutSize + ")" + lastStdout);
                    logger.info("LastStderr(Timeout):(" + stderrSize + ")" + lastStderr);
                    throw new InterruptedException("FFmpeg does not answer");
                }
                Thread.sleep(100);
            }
        } catch (InterruptedException e) {
            logger.log(e);
            throw e;
        } finally {
            processExitedFlag.set(true);
            synchronized (processExitedFlag) {
                processExitedFlag.notifyAll();
            }
            if (process != null) {
                process.destroy();
            }
            stdout.close();
            stderr.close();
        }
    }

    protected int exitProcess(Process process, final String stdout, final String stderr) throws IllegalThreadStateException {
        return process.exitValue();
    }

    protected long getLastUpdateTimestampTimeout() {
        return 60 * 1000l;
    }

    public static long formatStringToMilliseconds(final String text) {
        final String[] found = new Regex(text, "(\\d+):(\\d+):(\\d+)").getRow(0);
        if (found == null) {
            return 0;
        } else {
            final int hours = Integer.parseInt(found[0]);
            final int minutes = Integer.parseInt(found[1]);
            final int seconds = Integer.parseInt(found[2]);
            return hours * 60 * 60 * 1000 + minutes * 60 * 1000 + seconds * 1000;
        }
    }
}
