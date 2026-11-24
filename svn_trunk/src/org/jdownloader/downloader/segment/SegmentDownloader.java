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
import java.util.concurrent.atomic.AtomicBoolean;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
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
import jd.plugins.download.raf.HTTPDownloader;

//http://tools.ietf.org/html/draft-pantos-http-live-streaming-13
public class SegmentDownloader extends DownloadInterface {
    protected volatile long                         bytesWritten      = 0l;
    private final Downloadable                      downloadable;
    private final DownloadLink                      link;
    private long                                    startTimeStamp    = -1;
    private final LogInterface                      logger;
    private volatile URLConnectionAdapter           currentConnection;
    private final ManagedThrottledConnectionHandler connectionHandler = new ManagedThrottledConnectionHandler();
    private File                                    outputCompleteFile;
    private File                                    outputFinalCompleteFile;
    private File                                    outputPartFile;
    private PluginException                         caughtPluginException;
    protected final Browser                         obr;
    protected final List<Segment>                   segments          = new ArrayList<Segment>();
    protected Long                                  lastModified      = null;
    /**
     * Property of the link which stores the resume point: "SEGMENTCOUNT_NEXTSEGMENTINDEX_FILEPOSITION". The resume point is saved every
     * time a segment was loaded completely.
     */
    private static final String                     PROPERTY_RESUME   = "SegmentDownloader_resume";
    /* Resume point of this download: Index of the first segment which has to be loaded and the position in the part file where it starts. */
    private volatile int                            resumeSegmentIndex = 0;
    private volatile long                           resumePosition     = 0;

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
        if (terminated.getAndSet(true) == false) {
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

    protected boolean isSegmentConnectionValid(Segment segment, URLConnectionAdapter con) throws IOException, PluginException {
        return segment.isConnectionValid(con);
    }

    protected URLConnectionAdapter openSegmentConnection(Segment segment) throws IOException, PluginException, InterruptedException {
        int segmentRetryCounter = 0;
        while (!externalDownloadStop()) {
            final Browser br = obr.cloneBrowser();
            final Request request = createSegmentRequest(segment);
            final URLConnectionAdapter ret = segment.open(br, request);
            boolean valid = false;
            try {
                valid = isSegmentConnectionValid(segment, ret);
                if (valid) {
                    return ret;
                }
                br.followConnection(true);
                if (!retrySegmentConnection(br, segment, segmentRetryCounter)) {
                    throw new IOException("Invalid responseCode:" + ret.getResponseCode() + "|RetryCounter:" + segmentRetryCounter + "|Segment:" + segment.getUrl());
                }
                segmentRetryCounter++;
            } finally {
                if (!valid) {
                    ret.disconnect();
                }
            }
        }
        throw new PluginException(LinkStatus.ERROR_RETRY, "externalDownloadStop");
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
     * Enabled if the Downloadable is resumable. Override and return false if the subclass handles resume on its own.
     */
    protected boolean isSegmentResumeEnabled() {
        return downloadable.isResumable();
    }

    /** Returns the saved resume point ("SEGMENTCOUNT_NEXTSEGMENTINDEX_FILEPOSITION") or null. Default: stored in the link. */
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

    /**
     * Reads the resume point of the link and checks it: It must belong to the same number of segments and the part file must be at least
     * as long as the saved position. Otherwise the download starts from the beginning.
     */
    private void loadResumePoint() {
        resumeSegmentIndex = 0;
        resumePosition = 0;
        if (!isSegmentResumeEnabled()) {
            storeResumeInfo(null);
            return;
        }
        final String info = loadResumeInfo();
        if (info == null) {
            return;
        }
        final String[] parts = info.split("_");
        if (parts.length != 3) {
            return;
        }
        try {
            final int count = Integer.parseInt(parts[0]);
            final int index = Integer.parseInt(parts[1]);
            final long position = Long.parseLong(parts[2]);
            if (count != segments.size() || index <= 0 || index > count || position <= 0) {
                return;
            }
            if (!outputPartFile.isFile() || outputPartFile.length() < position || !isResumePointValid(index, position)) {
                return;
            }
            resumeSegmentIndex = index;
            resumePosition = position;
            logger.info("Resume at segment " + (index + 1) + "/" + count + " position " + position);
        } catch (final NumberFormatException e) {
            logger.log(e);
        }
    }

    /** Saves the point behind the segment with the given index: The next segment starts at the current end of the written data. */
    private void saveResumePoint(final int nextSegmentIndex) {
        if (isSegmentResumeEnabled()) {
            storeResumeInfo(segments.size() + "_" + nextSegmentIndex + "_" + bytesWritten);
        }
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
        try {
            if (resumeSegmentIndex > 0) {
                /* Throw away the data of the segment which was not loaded completely and continue behind the last complete segment. */
                outputStream.getChannel().truncate(resumePosition);
                outputStream.seek(resumePosition);
                bytesWritten = resumePosition;
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
            for (int segmentIndex = resumeSegmentIndex; segmentIndex < segments.size(); segmentIndex++) {
                final Segment segment = segments.get(segmentIndex);
                if (externalDownloadStop()) {
                    break;
                }
                try {
                    currentConnection = openSegmentConnection(segment);
                    if (lastModified == null) {
                        final Date last = TimeFormatter.parseDateString(currentConnection.getHeaderField("Last-Modified"));
                        if (last != null) {
                            lastModified = last.getTime();
                        }
                    }
                    meteredThrottledInputStream.setInputStream(getInputStream(segment, currentConnection));
                    bytesWritten = onSegmentStart(outputStream, segment, currentConnection);
                    while (!externalDownloadStop()) {
                        final int read = readSegment(segment, currentConnection, meteredThrottledInputStream, readWriteBuffer);
                        if (read == -1) {
                            segment.getChunkRange().setValidLoaded(true);
                            /* the data must be on disk before the resume point is saved */
                            outputStream.getChannel().force(true);
                            saveResumePoint(segmentIndex + 1);
                            break;
                        } else if (read == 0) {
                            continue;
                        }
                        localIO = true;
                        bytesWritten += writeSegment(segment, currentConnection, outputStream, readWriteBuffer, 0, read);
                        localIO = false;
                        downloadable.setDownloadBytesLoaded(bytesWritten);
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
            close(outputStream);
            if (connectionHandler != null && meteredThrottledInputStream != null) {
                connectionHandler.removeThrottledConnection(meteredThrottledInputStream);
            }
        }
    }

    protected Request createSegmentRequest(final Segment seg) throws IOException {
        final Request ret = seg.createRequest();
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
        storeResumeInfo(null);
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
        return resumeSegmentIndex > 0;
    }
}
