package jd.plugins.hoster;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Date;
import java.util.concurrent.atomic.AtomicBoolean;

import jd.PluginWrapper;
import jd.controlling.downloadcontroller.DiskSpaceReservation;
import jd.controlling.downloadcontroller.ExceptionRunnable;
import jd.controlling.downloadcontroller.FileIsLockedException;
import jd.controlling.downloadcontroller.ManagedThrottledConnectionHandler;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.download.DownloadInterface;
import jd.plugins.download.DownloadLinkDownloadable;
import jd.plugins.download.Downloadable;
import jd.plugins.download.raf.HTTPDownloader;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.IO;
import org.appwork.utils.logging2.LogSource;
import org.jdownloader.plugins.DownloadPluginProgress;
import org.jdownloader.plugins.SkipReason;
import org.jdownloader.plugins.SkipReasonException;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.translate._JDT;

@HostPlugin(revision = "$Revision: 51496 $", interfaceVersion = 2, names = { "filesystem" }, urls = { "filesystem:/.+" })
public class FileSystem extends PluginForHost {
    public FileSystem(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getAGBLink() {
        return null;
    }

    private File toFile(DownloadLink link) throws Exception {
        // TODO: new way !
        return new File(new URI(link.getPluginPatternMatcher().replaceFirst("^filesystem:/", "file:/")));
    }

    private String toFilesystemURI(File file) {
        // TODO: new way !
        return file.toURI().toString().replaceFirst("^file:/", "filesystem:/");
    }

    @Override
    public Object getFavIcon(String host) throws IOException {
        return super.getFavIcon(host);
    }

    @Override
    public String getPluginContentURL(DownloadLink link) {
        try {
            return toFile(link).toURI().toString();
        } catch (Exception e) {
            e.printStackTrace();
            return super.getPluginContentURL(link);
        }
    }

    @Override
    public AvailableStatus requestFileInformation(DownloadLink link) throws Exception {
        final File file = toFile(link);
        // TODO: use RandomAccessFile to open the file, also checks permissions
        if (file.isFile()) {
            link.setForcedFileName(file.getName());
            link.setVerifiedFileSize(file.length());
            return AvailableStatus.TRUE;
        } else if (file.isDirectory()) {
            link.setName(file.getName());
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else {
            link.setName(file.getName());
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception {
        if (true) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unfinished work in progress");
        }
        dl = new FileSystemDownloader(this, link);
        dl.startDownload();
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return -1;
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }

    protected class FileSystemDownloader extends DownloadInterface {
        protected volatile long                   bytesWritten      = 0l;
        private final Downloadable                downloadable;
        private File                              outputCompleteFile;
        private File                              outputFinalCompleteFile;
        private ManagedThrottledConnectionHandler connectionHandler = new ManagedThrottledConnectionHandler();
        private long                              startTimeStamp    = -1;

        public boolean isResumable() {
            return true;
        }

        protected FileSystemDownloader(final FileSystem plugin, final DownloadLink link) {
            if (plugin == null || link == null) {
                throw new IllegalArgumentException();
            } else {
                this.downloadable = new DownloadLinkDownloadable(link);
                downloadable.setDownloadInterface(this);
            }
        }

        public long getBytesLoaded() {
            return bytesWritten;
        }

        @Override
        public ManagedThrottledConnectionHandler getManagedConnetionHandler() {
            return this.connectionHandler;
        }

        @Override
        public URLConnectionAdapter connect(Browser br) throws Exception {
            throw new WTFException("Not needed");
        }

        @Override
        public long getTotalLinkBytesLoadedLive() {
            return getBytesLoaded();
        }

        public boolean startDownload() throws Exception {
            try {
                try {
                    /* We are about to start downloading so we know that if there was a captcha, it has been solved successfully. */
                    downloadable.validateLastChallengeResponse();
                } catch (final Throwable e) {
                    LogSource.exception(downloadable.getLogger(), e);
                }
                downloadable.getLogger().finer("Start Download");
                try {
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
                                    downloadable.lockFiles(outputCompleteFile, outputFinalCompleteFile);
                                } catch (FileIsLockedException e) {
                                    downloadable.unlockFiles(outputCompleteFile, outputFinalCompleteFile);
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
                        if (true) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unfinished work in progress");
                        } else {
                            IO.writeToFile(this.outputFinalCompleteFile, "".getBytes("UTF-8"), IO.SYNC.META_AND_DATA);
                        }
                        /* Set filesize so user can see it in UI. */
                        this.bytesWritten = this.outputFinalCompleteFile.length();
                        downloadable.setVerifiedFileSize(this.bytesWritten);
                        downloadable.setDownloadBytesLoaded(bytesWritten);
                        /* Set progress to finished - the "download" is complete. */
                        finalizeDownload(outputCompleteFile, outputFinalCompleteFile);
                        downloadable.setLinkStatus(LinkStatus.FINISHED);
                    } finally {
                        try {
                            downloadable.free(reservation);
                        } catch (final Throwable e) {
                            LogSource.exception(downloadable.getLogger(), e);
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
                    return true;
                } finally {
                    try {
                        cleanupDownladInterface();
                    } finally {
                        downloadable.unlockFiles(outputCompleteFile, outputFinalCompleteFile);
                    }
                }
            } finally {
                cleanupDownladInterface();
            }
        }

        protected void finalizeDownload(File outputPartFile, File outputCompleteFile) throws Exception {
            if (downloadable.rename(outputPartFile, outputCompleteFile)) {
                try {
                    boolean UseOriginalLastModified = JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified();
                    final Date lastModifiedDate;
                    if (UseOriginalLastModified && (lastModifiedDate = HTTPDownloader.getLastModifiedDate(getDownloadable(), null)) != null) {
                        outputCompleteFile.setLastModified(lastModifiedDate.getTime());
                    } else {
                        /* set current timestamp as lastModified timestamp */
                        outputCompleteFile.setLastModified(System.currentTimeMillis());
                    }
                } catch (final Throwable ignore) {
                    LogSource.exception(downloadable.getLogger(), ignore);
                }
            } else {
                throw new PluginException(LinkStatus.ERROR_DOWNLOAD_FAILED, _JDT.T.system_download_errors_couldnotrename(), LinkStatus.VALUE_LOCAL_IO_ERROR);
            }
        }

        protected void cleanupDownladInterface() {
        }

        private void createOutputChannel() throws SkipReasonException {
            try {
                final String fileOutput = downloadable.getFileOutput();
                downloadable.getLogger().info("createOutputChannel for " + fileOutput);
                final String finalFileOutput = downloadable.getFinalFileOutput();
                outputCompleteFile = new File(fileOutput);
                outputFinalCompleteFile = outputCompleteFile;
                if (!fileOutput.equals(finalFileOutput)) {
                    outputFinalCompleteFile = new File(finalFileOutput);
                }
            } catch (Exception e) {
                downloadable.getLogger().log(e);
                throw new SkipReasonException(SkipReason.INVALID_DESTINATION, e);
            }
        }

        @Override
        public URLConnectionAdapter getConnection() {
            return null;
        }

        /** signal that we stopped download external */
        public void stopDownload() {
            if (abort.getAndSet(true) == false) {
                downloadable.getLogger().info("externalStop recieved");
            }
        }

        private final AtomicBoolean abort = new AtomicBoolean(false);

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
            /* Do nothing */
        }

        @Override
        public Downloadable getDownloadable() {
            return downloadable;
        }

        @Override
        public boolean isResumedDownload() {
            return false;
        }
    }
}
