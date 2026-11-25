package org.jdownloader.downloader.segment;

import java.awt.Color;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.appwork.exceptions.WTFException;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.NullInputStream;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.throttledconnection.MeteredThrottledInputStream;
import org.appwork.utils.speedmeter.AverageSpeedMeter;
import org.jdownloader.plugins.DownloadPluginProgress;
import org.jdownloader.plugins.SkipReason;
import org.jdownloader.plugins.SkipReasonException;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.translate._JDT;

import jd.controlling.downloadcontroller.DiskSpaceReservation;
import jd.controlling.downloadcontroller.ExceptionRunnable;
import jd.controlling.downloadcontroller.FileIsLockedException;
import jd.controlling.downloadcontroller.ManagedThrottledConnectionHandler;
import jd.http.Browser;
import jd.http.Request;
import jd.http.URLConnectionAdapter;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.download.DownloadInterface;
import jd.plugins.download.DownloadLinkDownloadable;
import jd.plugins.download.Downloadable;
import jd.plugins.download.raf.ChunkRange;
import jd.plugins.download.raf.HTTPDownloader;

//http://tools.ietf.org/html/draft-pantos-http-live-streaming-13
public class SegmentDownloader extends DownloadInterface {
    protected volatile long                         bytesWritten                   = 0l;
    private final Downloadable                      downloadable;
    private final DownloadLink                      link;
    private long                                    startTimeStamp                 = -1;
    private final LogInterface                      logger;
    private volatile URLConnectionAdapter           currentConnection;
    private final ManagedThrottledConnectionHandler connectionHandler              = new ManagedThrottledConnectionHandler();
    private File                                    outputCompleteFile;
    private File                                    outputFinalCompleteFile;
    private File                                    outputPartFile;
    private PluginException                         caughtPluginException;
    protected final Browser                         obr;
    protected final List<Segment>                   segments                       = new ArrayList<Segment>();
    protected Long                                  lastModified                   = null;
    /**
     * Property of the link which stores the resume point: "SEGMENTCOUNT_SEGMENTINDEX_POSITION_SEGMENTBYTES" (the fourth part is optional,
     * missing = 0).
     *
     * <pre>
     * SEGMENTINDEX  = index of the segment the download continues with
     * POSITION      = position in the part file where this segment starts
     * SEGMENTBYTES  = bytes of this segment which are already in the part file (0 = the segment has to be loaded from its beginning)
     * </pre>
     *
     * It is saved every time a segment was loaded completely and (see isSingleSegmentResumeEnabled) in short intervals while a segment is
     * loaded.
     */
    private static final String                     PROPERTY_RESUME                = "SegmentDownloader_resume";
    /* Minimum time between two saves of the progress inside of a segment */
    private static final long                       SEGMENT_PROGRESS_SAVE_INTERVAL = TimeUnit.SECONDS.toMillis(2);
    /*
     * Resume point of this download: Index of the first segment which has to be loaded and the position in the part file where it starts.
     */
    private volatile int                            resumeSegmentIndex             = 0;
    /* Number of segments which are completely loaded (including the ones which were loaded before a resume), see isAllSegmentsLoaded */
    private volatile int                            completedSegments              = 0;
    private volatile long                           resumePosition                 = 0;
    /* Bytes of the first segment which are already in the part file */
    private volatile long                           resumeSegmentBytes             = 0;
    /* The segment which is continued with a range request, null if there is none or the attempt is over (successful or failed) */
    private volatile Segment                        singleResumeSegment            = null;
    /*
     * Set if the attempt to continue a segment failed: The data of the segment has to be thrown away before it is loaded from its beginning
     */
    private volatile boolean                        singleResumeRewind             = false;

    @Deprecated
    public static List<Segment> buildSegments(URL baseURL, String[] segments) {
        final List<Segment> ret = new ArrayList<Segment>();
        for (final String segment : segments) {
            ret.add(new Segment(URLHelper.parseLocation(baseURL, segment)));
        }
        return ret;
    }

    @Deprecated
    public SegmentDownloader(final PluginForHost plugin, final DownloadLink link, Downloadable dashDownloadable, Browser br, URL baseURL, String[] segments) {
        this(plugin, link, dashDownloadable, br, buildSegments(baseURL, segments));
    }

    public SegmentDownloader(final PluginForHost plugin, final DownloadLink link, Downloadable dashDownloadable, Browser br2, List<Segment> segments) {
        this.obr = br2.cloneBrowser();
        this.link = link;
        logger = plugin.getLogger();
        this.segments.addAll(segments);
        if (dashDownloadable == null) {
            /* Resumable if the plugin says so (the link is resumable): Resume works on segment level, see loadResumePoint */
            this.downloadable = new DownloadLinkDownloadable(link);
        } else {
            this.downloadable = dashDownloadable;
        }
        downloadable.setDownloadInterface(this);
    }

    protected void terminate() {
        if (terminated.getAndSet(true)) {
            /* Already terminated (e.g. by an error and the stop of the user afterwards): Everything below has to happen only once. */
            return;
        }
        if (!externalDownloadStop()) {
            logger.severe("A critical Downloaderror occured. Terminate...");
        }
        /* make sure the download loop stops and blocking reads are aborted */
        abort.set(true);
        final URLConnectionAdapter lCurrentConnection = currentConnection;
        if (lCurrentConnection != null) {
            lCurrentConnection.disconnect();
        }
    }

    private void close(Closeable closable) {
        try {
            if (closable != null) {
                closable.close();
            }
        } catch (Throwable e) {
        }
    }

    protected boolean retrySegmentConnection(Browser br, Segment segment, int segmentRetryCounter) throws InterruptedException, PluginException {
        final Boolean retry = segment.retrySegmentConnection(br, segment, segmentRetryCounter);
        if (retry != null) {
            return retry.booleanValue();
        }
        if (externalDownloadStop() || segmentRetryCounter != 0) {
            return false;
        }
        final int responseCode = br.getHttpConnection().getResponseCode();
        return responseCode == 404 || responseCode == 502;
    }

    protected boolean isSegmentConnectionValid(URLConnectionAdapter con, Segment segment) throws IOException, PluginException {
        /* A segment which is continued by a resume is requested behind the bytes which are already in the file. */
        if (!segment.isConnectionValid(con, segment == singleResumeSegment ? resumeSegmentBytes : 0)) {
            return false;
        }
        final long expectedLength = segment.getExpectedLength();
        if (expectedLength == Segment.UNKNOWN_LENGTH) {
            return true;
        }
        /* A segment which is continued by a resume is requested without the bytes which are already in the file. */
        final long expectedBodyLength = segment == singleResumeSegment ? expectedLength - resumeSegmentBytes : expectedLength;
        /* The length is checked only if the server sends it (not the case e.g. for chunked transfer encoding). */
        final long contentLength = con.getLongContentLength();
        return contentLength == -1 || contentLength == expectedBodyLength;
    }

    protected URLConnectionAdapter openSegmentConnection(Segment segment) throws IOException, PluginException, InterruptedException {
        return openSegmentConnection(segment, segments.indexOf(segment));
    }

    /**
     * Returns true if all segments are completely loaded, i.e. all data is in the part file. This does not mean that the download is
     * finished: The check of the file and the rename follow afterwards (see onDownloadReady).
     */
    protected boolean isAllSegmentsLoaded() {
        return completedSegments >= segments.size();
    }

    /** Share of the given segment which is loaded as text, e.g. "30.1%", or "unknown" if the length of the segment is not known. */
    private String getSegmentPercent(final Segment segment, final long loadedBytes) {
        final long expectedLength = segment.getExpectedLength();
        if (expectedLength <= 0) {
            return "unknown";
        }
        return (Math.round(loadedBytes * 1000d / expectedLength) / 10d) + "%";
    }

    /**
     * Share of the segments which are completely loaded as text, e.g. "45.0%". The size of the segments can be unknown, so the number of
     * segments is used.
     */
    private String getProgressPercent(final int completedSegments) {
        final int total = segments.size();
        if (total <= 0) {
            return "unknown";
        }
        return (Math.round(completedSegments * 1000d / total) / 10d) + "%";
    }

    /**
     * Opens the connection of the segment (including retries).
     *
     * <pre>
     * Logging: Every log line of a segment contains the number of the segment ("X/Y", starting with 1) and the range which is requested
     * ("-" = no range header, the complete segment). The range is the same kind of value for nearly all segments (none, or the fixed range
     * of the segment), it only differs for a segment which is continued by a resume. It is part of the line anyway: As soon as several
     * segments are loaded at the same time (threads), the lines of different segments are mixed up in the log and every line has to
     * show on its own which part of which segment it is about. To keep the log small only every 10th segment and every failed one is
     * logged.
     * </pre>
     *
     * @param segmentIndex
     *            Index of the segment in the list of segments
     */
    private URLConnectionAdapter openSegmentConnection(final Segment segment, final int segmentIndex) throws IOException, PluginException, InterruptedException {
        final String segmentName = (segmentIndex + 1) + "/" + segments.size() + "(" + getProgressPercent(segmentIndex) + ")";
        int segmentRetryCounter = 0;
        while (!externalDownloadStop()) {
            /* true: The segment is continued with a range request. If that does not work it is loaded again without range. */
            final boolean resumeAttempt = singleResumeSegment == segment;
            final Browser br = obr.cloneBrowser();
            final Request request = createSegmentRequest(segment);
            final String range = getRequestedRange(request);
            if (resumeAttempt) {
                logger.info("Try to resume segment " + segmentName + " behind " + resumeSegmentBytes + " bytes: range=" + range);
            } else if (segmentRetryCounter == 0 && (segmentIndex + 1) % 10 == 0) {
                logger.info("Downloading segment " + segmentName + " range=" + range);
            }
            URLConnectionAdapter ret = null;
            boolean valid = false;
            try {
                try {
                    ret = segment.open(br, request);
                } catch (final IOException e) {
                    if (!resumeAttempt || externalDownloadStop()) {
                        throw e;
                    }
                    logger.log(e);
                    cancelSingleResume("Connect failed: " + e);
                    continue;
                }
                valid = isSegmentConnectionValid(ret, segment);
                if (valid && resumeAttempt) {
                    valid = isSingleResumeConnectionValid(segment, ret);
                }
                if (valid) {
                    return ret;
                }
                if (resumeAttempt) {
                    /* No retry counter: This is not an error of the segment, it only cannot be continued. */
                    cancelSingleResume("Invalid response: responseCode=" + ret.getResponseCode() + " contentRange=" + ret.getHeaderField("Content-Range"));
                    continue;
                }
                logger.info("Failed to load segment " + segmentName + " range=" + range + ": responseCode=" + ret.getResponseCode() + " length=" + ret.getCompleteContentLength() + " retryCounter=" + segmentRetryCounter);
                br.followConnection(true);
                if (!retrySegmentConnection(br, segment, segmentRetryCounter)) {
                    throw new IOException("Invalid responseCode:" + ret.getResponseCode() + "|RetryCounter:" + segmentRetryCounter + "|Segment:" + segment.getUrl());
                }
                segmentRetryCounter++;
            } finally {
                if (!valid && ret != null) {
                    ret.disconnect();
                }
            }
        }
        throw new PluginException(LinkStatus.ERROR_RETRY, "externalDownloadStop");
    }

    /** Returns the range which is requested by the given request for the log: The value of the Range header or "-" if there is none. */
    private String getRequestedRange(final Request request) {
        final String range = request.getHeaders().get(HTTPConstants.HEADER_REQUEST_RANGE);
        return range != null ? range : "-";
    }

    /**
     * The range request to continue a segment is only valid if the server answers with exactly the part which was requested: Status 206 and
     * a Content-Range which starts at the requested position. A server that ignores the range sends the whole segment (200) which would
     * corrupt the file.
     */
    private boolean isSingleResumeConnectionValid(final Segment segment, final URLConnectionAdapter con) throws IOException {
        if (con.getResponseCode() != 206) {
            return false;
        }
        /* the Content-Range of the response, not the Range of the request */
        final long[] range = HTTPConnectionUtils.parseContentRange(con);
        return range != null && range[0] == segment.getChunkRange().getFrom() + resumeSegmentBytes;
    }

    /** The continuation of the segment did not work: The next attempt loads the segment from its beginning without range request. */
    private void cancelSingleResume(final String reason) {
        final Segment segment = singleResumeSegment;
        final int segmentIndex = segment != null ? segments.indexOf(segment) : -1;
        final String segmentName = segmentIndex >= 0 ? (segmentIndex + 1) + "/" + segments.size() + "(" + getProgressPercent(segmentIndex) + ")" : "?";
        logger.info("Resume of segment " + segmentName + " failed (" + reason + "): Load it again from the beginning");
        singleResumeSegment = null;
        singleResumeRewind = true;
    }

    protected InputStream getInputStream(Segment segment, URLConnectionAdapter connection) throws IOException, PluginException {
        return connection.getInputStream();
    }

    protected int writeSegment(Segment segment, URLConnectionAdapter con, RandomAccessFile outputStream, byte[] buf, int index, int len) throws IOException {
        outputStream.write(buf, index, len);
        segment.getChunkRange().incLoaded(len);
        return len;
    }

    protected int readSegment(Segment segment, URLConnectionAdapter con, InputStream is, byte[] buf) throws IOException {
        return is.read(buf);
    }

    protected void onSegmentClose(RandomAccessFile outputStream, Segment segment, URLConnectionAdapter con) throws IOException {
    }

    protected long onSegmentStart(RandomAccessFile outputStream, Segment segment, URLConnectionAdapter con) throws IOException {
        final long[] range = HTTPConnectionUtils.parseRequestRange(con);
        if (range != null && range[0] != -1) {
            outputStream.seek(range[0]);
            bytesWritten = range[0];
        }
        return bytesWritten;
    }

    /**
     * Resume on segment level: Already loaded segments are skipped, the file is continued behind the last completely loaded segment.
     * Enabled if the Downloadable is resumable. Override and return false if the subclass handles resume on its own. This disables every
     * kind of resume of this class, including isSingleSegmentResumeEnabled.
     */
    protected boolean isResumeEnabled() {
        return downloadable.isResumable();
    }

    /**
     * Resume of a single segment: If the download was interrupted inside of a segment, only the rest of this segment is requested (range
     * request) instead of loading the segment again from its beginning. The server has to support range requests for the segments, if it
     * does not the segment is loaded again without range. Override and return false if that is known to be impossible or avoid the request.
     * It has to be false if the data written to the file is not exactly the data received (e.g. if readSegment, writeSegment or
     * getInputStream change the data): The saved offset is the number of written bytes.
     *
     * @param segment
     *            The segment which is about to be loaded
     */
    protected boolean isSingleSegmentResumeEnabled(final Segment segment) {
        return true;
    }

    /** Resume of a single segment is effectively allowed only if resume is enabled at all (isResumeEnabled) and for this segment. */
    protected final boolean isSingleSegmentResumeEffectivelyAllowed(final Segment segment) {
        return isResumeEnabled() && isSingleSegmentResumeEnabled(segment);
    }

    /** Returns the saved resume point (see PROPERTY_RESUME) or null. Default: stored in the link. */
    protected String loadResumeInfo() {
        return link.getStringProperty(PROPERTY_RESUME, null);
    }

    /** Saves the resume point, null removes it. Default: stored in the link (override if the given link is not persistent). */
    protected void storeResumeInfo(final String info) {
        if (info == null) {
            link.removeProperty(PROPERTY_RESUME);
        } else {
            link.setProperty(PROPERTY_RESUME, info);
        }
    }

    /** Additional check of a loaded resume point, e.g. whether the segment at the given index is still the expected one. */
    protected boolean isResumePointValid(final int nextSegmentIndex, final long position) {
        return true;
    }

    /** Logs why a saved resume point cannot be used. The properties of a link can be changed from outside at any time. */
    private void logIgnoredResumeInfo(final String info, final String reason) {
        logger.info("Ignore resume info '" + info + "': " + reason);
    }

    /**
     * Reads the resume point of the link and checks it: It must belong to the same number of segments, the values must be plausible and the
     * part file must be long enough. Otherwise the download starts from the beginning (the reason is logged).
     */
    private void loadResumePoint() {
        resumeSegmentIndex = 0;
        resumePosition = 0;
        resumeSegmentBytes = 0;
        singleResumeSegment = null;
        singleResumeRewind = false;
        if (!isResumeEnabled()) {
            storeResumeInfo(null);
            return;
        }
        final String info = loadResumeInfo();
        if (info == null) {
            return;
        }
        final String[] parts = info.split("_");
        if (parts.length != 3 && parts.length != 4) {
            logIgnoredResumeInfo(info, "invalid format");
            return;
        }
        final int count;
        final int index;
        final long position;
        long segmentBytes;
        try {
            count = Integer.parseInt(parts[0]);
            index = Integer.parseInt(parts[1]);
            position = Long.parseLong(parts[2]);
            segmentBytes = parts.length == 4 ? Long.parseLong(parts[3]) : 0;
        } catch (final NumberFormatException e) {
            /* Bad format of resume info property */
            logger.log(e);
            logIgnoredResumeInfo(info, "not a number");
            return;
        }
        if (count != segments.size()) {
            logIgnoredResumeInfo(info, "segment count " + count + " != " + segments.size());
            return;
        } else if (index < 0 || index > count) {
            logIgnoredResumeInfo(info, "segment index out of range 0-" + count);
            return;
        } else if (position < 0 || segmentBytes < 0) {
            logIgnoredResumeInfo(info, "negative position or segment bytes");
            return;
        } else if (index == 0 && position != 0) {
            logIgnoredResumeInfo(info, "the first segment cannot start at position " + position);
            return;
        } else if (index > 0 && position == 0) {
            logIgnoredResumeInfo(info, "segment " + index + " cannot start at position 0");
            return;
        } else if (index == count && segmentBytes > 0) {
            logIgnoredResumeInfo(info, "there are no bytes of a segment behind the last segment");
            return;
        }
        final long verifiedFileSize = downloadable.getVerifiedFileSize();
        if (verifiedFileSize > 0 && position + segmentBytes > verifiedFileSize) {
            logIgnoredResumeInfo(info, "position " + (position + segmentBytes) + " is behind the known file size " + verifiedFileSize);
            return;
        }
        if (!outputPartFile.isFile()) {
            logIgnoredResumeInfo(info, "part file does not exist");
            return;
        }
        final long fileLength = outputPartFile.length();
        if (fileLength < position) {
            logIgnoredResumeInfo(info, "part file is too short: " + fileLength + " < " + position);
            return;
        } else if (!isResumePointValid(index, position)) {
            logIgnoredResumeInfo(info, "rejected by isResumePointValid");
            return;
        }
        if (segmentBytes > 0 && fileLength < position + segmentBytes) {
            logger.info("Resume info '" + info + "': Part file is too short for the bytes of the segment (" + fileLength + " < " + (position + segmentBytes) + "), the segment is loaded from its beginning");
            segmentBytes = 0;
        } else if (segmentBytes > 0 && !isSingleSegmentResumeEnabled(segments.get(index))) {
            logger.info("Resume info '" + info + "': Resume of a single segment is not enabled, the segment is loaded from its beginning");
            segmentBytes = 0;
        }
        if (index == 0 && segmentBytes == 0) {
            /* nothing to resume */
            return;
        }
        resumeSegmentIndex = index;
        resumePosition = position;
        resumeSegmentBytes = segmentBytes;
        if (segmentBytes > 0) {
            singleResumeSegment = segments.get(index);
            logger.info("Resume at segment " + (index + 1) + "/" + count + "(" + getProgressPercent(index) + ") position " + position + " + " + segmentBytes + " bytes of the segment (" + getSegmentPercent(segments.get(index), segmentBytes) + ")");
        } else {
            logger.info("Resume at segment " + (index + 1) + "/" + count + "(" + getProgressPercent(index) + ") position " + position);
        }
    }

    /** Saves the point behind the segment with the given index: The next segment starts at the current end of the written data. */
    private void saveResumePoint(final int nextSegmentIndex) {
        if (!isResumeEnabled()) {
            return;
        }
        storeResumeInfo(segments.size() + "_" + nextSegmentIndex + "_" + bytesWritten + "_0");
    }

    /**
     * Saves the progress inside of the segment with the given index.
     *
     * @param segmentStartPosition
     *            Position in the part file where the segment starts
     */
    private void saveSegmentProgress(final RandomAccessFile outputStream, final int segmentIndex, final long segmentStartPosition) throws IOException {
        /* the data must be on disk before the resume point is saved */
        outputStream.getChannel().force(true);
        storeResumeInfo(segments.size() + "_" + segmentIndex + "_" + segmentStartPosition + "_" + (bytesWritten - segmentStartPosition));
    }

    public void run() throws Exception {
        link.setDownloadSize(-1);
        loadResumePoint();
        final boolean isResumedDownload = isResumedDownload();
        final RandomAccessFile outputStream;
        try {
            outputStream = new RandomAccessFile(outputPartFile, "rw");
        } catch (IOException e) {
            throw new SkipReasonException(SkipReason.INVALID_DESTINATION, e);
        }
        final MeteredThrottledInputStream meteredThrottledInputStream = new MeteredThrottledInputStream(new NullInputStream(), new AverageSpeedMeter(10));
        boolean localIO = false;
        /* index of the segment which is loaded at the moment, used for the log message if the download is aborted */
        int activeSegmentIndex = resumeSegmentIndex;
        completedSegments = resumeSegmentIndex;
        /* file position where the segment which is loaded at the moment starts, -1: no segment is in progress */
        long activeSegmentStartPosition = -1;
        try {
            if (resumeSegmentIndex > 0 || resumeSegmentBytes > 0) {
                /*
                 * Throw away the data behind the resume point (a segment which was not loaded completely) and continue behind it. If the
                 * bytes of the interrupted segment can be used it is continued with a range request, see openSegmentConnection.
                 */
                final long position = resumePosition + resumeSegmentBytes;
                outputStream.getChannel().truncate(position);
                outputStream.seek(position);
                bytesWritten = position;
                downloadable.setDownloadBytesLoaded(bytesWritten);
                for (int i = 0; i < resumeSegmentIndex; i++) {
                    segments.get(i).getChunkRange().setValidLoaded(true);
                }
            } else if (!isResumedDownload) {
                outputStream.getChannel().truncate(0);
            }
            final String cust = link.getCustomExtension();
            link.setCustomExtension(null);
            link.setCustomExtension(cust);
            final byte[] readWriteBuffer = new byte[512 * 1024];
            if (connectionHandler != null) {
                connectionHandler.addThrottledConnection(meteredThrottledInputStream);
            }
            segmentLoop: for (int segmentIndex = resumeSegmentIndex; segmentIndex < segments.size(); segmentIndex++) {
                final Segment segment = segments.get(segmentIndex);
                activeSegmentIndex = segmentIndex;
                activeSegmentStartPosition = -1;
                if (externalDownloadStop()) {
                    break segmentLoop;
                }
                try {
                    currentConnection = openSegmentConnection(segment, segmentIndex);
                    /* true: the segment is continued, the range request of the resume was accepted */
                    final boolean singleResumed = singleResumeSegment == segment;
                    singleResumeSegment = null;
                    if (singleResumeRewind) {
                        /* the range request did not work: throw away the part of the segment and load it again completely */
                        outputStream.getChannel().truncate(resumePosition);
                        outputStream.seek(resumePosition);
                        bytesWritten = resumePosition;
                        downloadable.setDownloadBytesLoaded(bytesWritten);
                        singleResumeRewind = false;
                    }
                    if (lastModified == null) {
                        final Date last = TimeFormatter.parseDateString(currentConnection.getHeaderField("Last-Modified"));
                        if (last != null) {
                            lastModified = last.getTime();
                        }
                    }
                    meteredThrottledInputStream.setInputStream(getInputStream(segment, currentConnection));
                    bytesWritten = onSegmentStart(outputStream, segment, currentConnection);
                    final long segmentStartPosition;
                    if (singleResumed) {
                        /*
                         * The range of the response is relative to the segment, not to the file: Continue behind the bytes of the segment
                         * which are already in the file.
                         */
                        outputStream.seek(resumePosition + resumeSegmentBytes);
                        bytesWritten = resumePosition + resumeSegmentBytes;
                        segmentStartPosition = resumePosition;
                        logger.info("Resumed segment " + (segmentIndex + 1) + "/" + segments.size() + "(" + getProgressPercent(segmentIndex) + ") behind " + resumeSegmentBytes + " bytes");
                    } else {
                        segmentStartPosition = bytesWritten;
                    }
                    activeSegmentStartPosition = segmentStartPosition;
                    final boolean singleResumeAllowed = isSingleSegmentResumeEffectivelyAllowed(segment);
                    long lastProgressSave = Time.systemIndependentCurrentJVMTimeMillis();
                    readLoop: while (!externalDownloadStop()) {
                        final int read = readSegment(segment, currentConnection, meteredThrottledInputStream, readWriteBuffer);
                        if (read == -1) {
                            segment.getChunkRange().setValidLoaded(true);
                            completedSegments = segmentIndex + 1;
                            /* no segment is in progress anymore */
                            activeSegmentStartPosition = -1;
                            /* the data must be on disk before the resume point is saved */
                            outputStream.getChannel().force(true);
                            saveResumePoint(segmentIndex + 1);
                            /* segment complete: continue with the next segment in segmentLoop */
                            break readLoop;
                        } else if (read == 0) {
                            continue readLoop;
                        }
                        localIO = true;
                        bytesWritten += writeSegment(segment, currentConnection, outputStream, readWriteBuffer, 0, read);
                        localIO = false;
                        downloadable.setDownloadBytesLoaded(bytesWritten);
                        if (singleResumeAllowed) {
                            final long now = Time.systemIndependentCurrentJVMTimeMillis();
                            if (now - lastProgressSave >= SEGMENT_PROGRESS_SAVE_INTERVAL) {
                                lastProgressSave = now;
                                saveSegmentProgress(outputStream, segmentIndex, segmentStartPosition);
                            }
                        }
                    }
                } finally {
                    final URLConnectionAdapter lCurrentConnection = currentConnection;
                    currentConnection = null;
                    if (lCurrentConnection != null) {
                        try {
                            onSegmentClose(outputStream, segment, lCurrentConnection);
                        } finally {
                            lCurrentConnection.disconnect();
                        }
                    }
                }
            }
        } catch (IOException e) {
            if (externalDownloadStop() && !localIO) {
                /* connection was closed by stopDownload/terminate */
                logger.log(e);
                return;
            }
            if (localIO) {
                throw new SkipReasonException(SkipReason.DISK_FULL);
            } else {
                throw e;
            }
        } catch (InterruptedException e) {
            throw e;
        } catch (PluginException e) {
            throw e;
        } catch (Throwable e) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, e.getMessage(), -1, e);
        } finally {
            /*
             * The progress of the interrupted segment is saved only if all of these conditions are met:
             *
             * - activeSegmentStartPosition >= 0: A segment is in progress, i.e. its connection was opened and it was not read completely
             * (the value is reset to -1 at the start of every loop iteration and when the segment is complete).
             *
             * - !localIO: The interruption was not caused by writing to the disk (e.g. disk full). Then it is unclear what is in the file.
             *
             * - activeSegmentIndex < segments.size(): The index is valid (it is not if the resume point was behind the last segment).
             *
             * - bytesWritten > activeSegmentStartPosition: Data of the segment was written. Otherwise there is nothing to continue.
             *
             * - the segment is not marked as completely loaded (isValidLoaded).
             *
             * - isSingleSegmentResumeEffectivelyAllowed: Resume is enabled at all and for this segment.
             */
            if (activeSegmentStartPosition >= 0 && !localIO && activeSegmentIndex < segments.size() && bytesWritten > activeSegmentStartPosition) {
                final Segment activeSegment = segments.get(activeSegmentIndex);
                if (!activeSegment.getChunkRange().isValidLoaded() && isSingleSegmentResumeEffectivelyAllowed(activeSegment)) {
                    /*
                     * The segment was interrupted (stop or error): Save the exact progress now, otherwise only the state of the last
                     * periodic save would be available, which is none at all if segments are loaded faster than the save interval.
                     */
                    try {
                        saveSegmentProgress(outputStream, activeSegmentIndex, activeSegmentStartPosition);
                    } catch (final IOException e) {
                        logger.log(e);
                    }
                }
            }
            if (externalDownloadStop()) {
                /* Logged once and always when the stop flag is set, even if all segments happened to be loaded already. */
                final int loadedSegments = completedSegments;
                final boolean allSegmentsLoaded = isAllSegmentsLoaded();
                final String stopPosition;
                final String segmentProgress;
                if (allSegmentsLoaded) {
                    /* Edge case: Download was stopped just after all segments were loaded */
                    stopPosition = "AFTER the last segment";
                    segmentProgress = "none (all segments were loaded)";
                } else if (activeSegmentStartPosition >= 0 && bytesWritten > activeSegmentStartPosition) {
                    stopPosition = "IN THE MIDDLE of a segment";
                    final long segmentBytes = bytesWritten - activeSegmentStartPosition;
                    final long expectedLength = segments.get(activeSegmentIndex).getExpectedLength();
                    if (expectedLength > 0) {
                        segmentProgress = segmentBytes + "/" + expectedLength + " bytes (" + getSegmentPercent(segments.get(activeSegmentIndex), segmentBytes) + ")";
                    } else {
                        /* the size of the segment is unknown: no percentage possible */
                        segmentProgress = segmentBytes + " bytes (percentage unknown, size of the segment is unknown)";
                    }
                } else {
                    stopPosition = "CLEAN between segments";
                    segmentProgress = "none (no partial segment data)";
                }
                /* The start of the line is always the same, the progress of the individual segment is always at the end. */
                logger.info("Download stopped: segment " + Math.min(activeSegmentIndex + 1, segments.size()) + "/" + segments.size() + "(" + getProgressPercent(loadedSegments) + "), completedSegments=" + loadedSegments + ", bytesWritten=" + bytesWritten + ", position=" + stopPosition + ", segmentProgress=" + segmentProgress);
            }
            close(outputStream);
            if (connectionHandler != null && meteredThrottledInputStream != null) {
                connectionHandler.removeThrottledConnection(meteredThrottledInputStream);
            }
        }
    }

    protected Request createSegmentRequest(final Segment seg) throws IOException {
        final Request ret = seg.createRequest();
        if (singleResumeSegment == seg) {
            /* continue the segment behind the bytes which are already in the file */
            final ChunkRange chunkRange = seg.getChunkRange();
            final Long to = chunkRange.getTo();
            final String range = "bytes=" + (chunkRange.getFrom() + resumeSegmentBytes) + "-" + (to != null && to >= 0 ? to.toString() : "");
            ret.getHeaders().put(new HTTPHeader(HTTPConstants.HEADER_REQUEST_RANGE, range, false));
        }
        return ret;
    }

    public long getBytesLoaded() {
        return bytesWritten;
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
        try {
            downloadable.validateLastChallengeResponse();
            String fileOutput = downloadable.getFileOutput();
            String finalFileOutput = downloadable.getFinalFileOutput();
            outputCompleteFile = new File(fileOutput);
            outputFinalCompleteFile = outputCompleteFile;
            if (!fileOutput.equals(finalFileOutput)) {
                outputFinalCompleteFile = new File(finalFileOutput);
            }
            outputPartFile = new File(downloadable.getFileOutputPart());
            DownloadPluginProgress downloadPluginProgress = null;
            downloadable.setConnectionHandler(this.getManagedConnetionHandler());
            final DiskSpaceReservation reservation = downloadable.createDiskSpaceReservation();
            try {
                if (!downloadable.checkIfWeCanWrite(new ExceptionRunnable() {
                    @Override
                    public void run() throws Exception {
                        downloadable.checkAndReserve(reservation);
                        createOutputChannel();
                        try {
                            downloadable.lockFiles(outputCompleteFile, outputFinalCompleteFile, outputPartFile);
                        } catch (FileIsLockedException e) {
                            downloadable.unlockFiles(outputCompleteFile, outputFinalCompleteFile, outputPartFile);
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
                run();
                return onDownloadReady();
            } finally {
                try {
                    downloadable.free(reservation);
                } catch (final Throwable e) {
                    logger.log(e);
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
        } finally {
            downloadable.unlockFiles(outputCompleteFile, outputFinalCompleteFile, outputPartFile);
            cleanupDownladInterface();
        }
    }

    protected void error(PluginException pluginException) {
        synchronized (this) {
            /* if we recieved external stop, then we dont have to handle errors */
            if (externalDownloadStop()) {
                return;
            }
            logger.log(pluginException);
            if (caughtPluginException == null) {
                caughtPluginException = pluginException;
            }
        }
        terminate();
    }

    protected boolean onDownloadReady() throws Exception {
        cleanupDownladInterface();
        if (handleErrors(outputPartFile) == false) {
            return false;
        }
        final boolean renameOkay = finalizeDownload(outputPartFile, outputCompleteFile, lastModified);
        if (!renameOkay) {
            error(new PluginException(LinkStatus.ERROR_DOWNLOAD_FAILED, _JDT.T.system_download_errors_couldnotrename(), LinkStatus.VALUE_LOCAL_IO_ERROR));
            return false;
        }
        /*
         * The download is complete: The resume point is not needed anymore. Every property of a DownloadLink uses RAM (for as long as the
         * link exists in the download list), so it is removed instead of being left behind on all finished links.
         */
        storeResumeInfo(null);
        return true;
    }

    protected boolean finalizeDownload(File outputPartFile, File outputCompleteFile, Long lastModified) throws Exception {
        if (!downloadable.rename(outputPartFile, outputCompleteFile)) {
            return false;
        }
        try {
            boolean useOriginalLastModified = JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified();
            if (useOriginalLastModified) {
                final Date lastModifiedDate = HTTPDownloader.getLastModifiedDate(getDownloadable(), null);
                if (lastModifiedDate != null) {
                    outputCompleteFile.setLastModified(lastModifiedDate.getTime());
                    return true;
                }
                if (lastModified != null && lastModified != -1) {
                    outputCompleteFile.setLastModified(lastModified);
                    return true;
                }
            }
            outputCompleteFile.setLastModified(System.currentTimeMillis());
        } catch (final Throwable ignore) {
            LogSource.exception(logger, ignore);
        }
        return true;
    }

    protected void cleanupDownladInterface() {
    }

    protected void checkComplete(final List<Segment> segments, final long fileSize) throws PluginException {
        for (final Segment segment : segments) {
            if (!segment.isLoaded()) {
                // ignore index>0 as it is not supported yet
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Segment:" + segment.getUrl() + " not loaded");
            }
        }
        if (downloadable.getVerifiedFileSize() > 0 && downloadable.getVerifiedFileSize() != fileSize) {
            throw new PluginException(LinkStatus.ERROR_DOWNLOAD_INCOMPLETE, "Verified:" + downloadable.getVerifiedFileSize() + " != Filesize:" + fileSize);
        }
    }

    protected boolean handleErrors(final File file) throws PluginException {
        final PluginException caught;
        synchronized (this) {
            caught = caughtPluginException;
        }
        if (caught != null) {
            throw caught;
        }
        if (externalDownloadStop()) {
            return false;
        }
        if (!file.isFile()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final long fileSize = file.length();
        checkComplete(segments, fileSize);
        downloadable.setDownloadBytesLoaded(fileSize);
        downloadable.setVerifiedFileSize(fileSize);
        downloadable.setLinkStatus(LinkStatus.FINISHED);
        return true;
    }

    private void createOutputChannel() throws SkipReasonException {
        try {
            final String fileOutput = downloadable.getFileOutput();
            logger.info("createOutputChannel for " + fileOutput);
            final String finalFileOutput = downloadable.getFinalFileOutput();
            outputCompleteFile = new File(fileOutput);
            outputFinalCompleteFile = outputCompleteFile;
            if (!fileOutput.equals(finalFileOutput)) {
                outputFinalCompleteFile = new File(finalFileOutput);
            }
        } catch (Exception e) {
            logger.log(e);
            throw new SkipReasonException(SkipReason.INVALID_DESTINATION, e);
        }
    }

    @Override
    public URLConnectionAdapter getConnection() {
        return currentConnection;
    }

    @Override
    public void stopDownload() {
        if (abort.getAndSet(true) == false) {
            logger.info("externalStop recieved");
            terminate();
        }
        final URLConnectionAdapter lCurrentConnection = currentConnection;
        if (lCurrentConnection != null) {
            lCurrentConnection.disconnect();
        }
    }

    private final AtomicBoolean abort      = new AtomicBoolean(false);
    private final AtomicBoolean terminated = new AtomicBoolean(false);

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
        final URLConnectionAdapter lCurrentConnection = currentConnection;
        if (lCurrentConnection != null) {
            lCurrentConnection.disconnect();
        }
    }

    @Override
    public Downloadable getDownloadable() {
        return downloadable;
    }

    @Override
    public boolean isResumedDownload() {
        return resumeSegmentIndex > 0 || resumeSegmentBytes > 0;
    }
}
