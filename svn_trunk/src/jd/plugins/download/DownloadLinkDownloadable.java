package jd.plugins.download;

import java.awt.Color;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.zip.CRC32;
import java.util.zip.CheckedInputStream;
import java.util.zip.Checksum;

import jd.controlling.downloadcontroller.DiskSpaceManager.DISKSPACERESERVATIONRESULT;
import jd.controlling.downloadcontroller.DiskSpaceReservation;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.ExceptionRunnable;
import jd.controlling.downloadcontroller.FileIsLockedException;
import jd.controlling.downloadcontroller.ManagedThrottledConnectionHandler;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.DownloadLinkDatabindingInterface;
import jd.plugins.FilePackage;
import jd.plugins.Plugin;
import jd.plugins.PluginForHost;
import jd.plugins.PluginForHost.FILENAME_SOURCE;
import jd.plugins.PluginProgress;
import jd.plugins.download.HashInfo.TYPE;
import jd.plugins.hoster.DirectHTTP;

import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.DispositionHeader;
import org.bouncycastle.crypto.digests.WhirlpoolDigest;
import org.jdownloader.controlling.FileCreationManager;
import org.jdownloader.plugins.FinalLinkState;
import org.jdownloader.plugins.HashCheckPluginProgress;
import org.jdownloader.plugins.SkipReason;
import org.jdownloader.plugins.SkipReasonException;

public class DownloadLinkDownloadable implements Downloadable {
    private static volatile boolean crcHashingInProgress = false;
    /**
     *
     */
    private final DownloadLink      downloadLink;
    private final PluginForHost     plugin;
    private final Browser           contextBrowser;

    public DownloadLinkDownloadable(DownloadLink downloadLink) {
        this(downloadLink, null);
    }

    public DownloadLinkDownloadable(DownloadLink downloadLink, final Browser br) {
        this.downloadLink = downloadLink;
        plugin = downloadLink.getLivePlugin();
        this.contextBrowser = br;
    }

    public DownloadLink getDownloadLink() {
        return downloadLink;
    }

    @Override
    public void setResumeable(boolean value) {
        getDownloadLink().setResumeable(value);
    }

    @Override
    public Browser getContextBrowser() {
        if (contextBrowser != null) {
            return contextBrowser.cloneBrowser();
        } else {
            return getPlugin().getBrowser().cloneBrowser();
        }
    }

    @Override
    public LogInterface getLogger() {
        return getPlugin().getLogger();
    }

    @Override
    public void setDownloadInterface(DownloadInterface di) {
        getPlugin().setDownloadInterface(di);
    }

    @Override
    public long getVerifiedFileSize() {
        return getDownloadLink().getView().getBytesTotalVerified();
    }

    @Override
    public boolean isServerComaptibleForByteRangeRequest() {
        return getDownloadLink().getBooleanProperty(DirectHTTP.PROPERTY_ServerComaptibleForByteRangeRequest, false);
    }

    @Override
    public String getHost() {
        final DownloadInterface dli = getDownloadInterface();
        if (dli != null) {
            final URLConnectionAdapter connection = dli.getConnection();
            if (connection != null) {
                return connection.getURL().getHost();
            }
        }
        return getDownloadLink().getHost();
    }

    @Override
    public boolean isDebug() {
        return getPlugin().getBrowser().isDebug();
    }

    @Override
    public void setDownloadTotalBytes(long l) {
        getDownloadLink().setDownloadSize(l);
    }

    public SingleDownloadController getDownloadLinkController() {
        return getDownloadLink().getDownloadLinkController();
    }

    @Override
    public void setLinkStatus(int finished) {
        getDownloadLinkController().getLinkStatus().setStatus(finished);
    }

    @Override
    public void setVerifiedFileSize(long length) {
        getDownloadLink().setVerifiedFileSize(length);
    }

    @Override
    public void validateLastChallengeResponse() {
        getPlugin().validateLastChallengeResponse();
    }

    @Override
    public void setConnectionHandler(ManagedThrottledConnectionHandler managedConnetionHandler) {
        getDownloadLinkController().getConnectionHandler().addConnectionHandler(managedConnetionHandler);
    }

    @Override
    public void removeConnectionHandler(ManagedThrottledConnectionHandler managedConnetionHandler) {
        getDownloadLinkController().getConnectionHandler().removeConnectionHandler(managedConnetionHandler);
    }

    @Override
    public void setAvailable(AvailableStatus status) {
        getDownloadLink().setAvailableStatus(status);
    }

    @Override
    public String getFinalFileName() {
        return getDownloadLink().getFinalFileName();
    }

    @Override
    public void setFinalFileName(String newfinalFileName) {
        getDownloadLink().setFinalFileName(newfinalFileName);
    }

    public void setForcedFileName(String newforcedFileName) {
        getDownloadLink().setForcedFileName(newforcedFileName);
    }

    @Override
    public boolean checkIfWeCanWrite(final ExceptionRunnable runOkay, final ExceptionRunnable runFailed) throws Exception {
        final SingleDownloadController dlc = getDownloadLinkController();
        final AtomicBoolean atomicBoolean = new AtomicBoolean(false);
        DownloadWatchDog.getInstance().localFileCheck(dlc, new ExceptionRunnable() {
            @Override
            public void run() throws Exception {
                runOkay.run();
                atomicBoolean.set(true);
            }
        }, runFailed);
        return atomicBoolean.get();
    }

    @Override
    public void lockFiles(File... files) throws FileIsLockedException {
        final SingleDownloadController dlc = getDownloadLinkController();
        for (File f : files) {
            dlc.lockFile(f);
        }
    }

    @Override
    public void unlockFiles(File... files) {
        final SingleDownloadController dlc = getDownloadLinkController();
        for (File f : files) {
            dlc.unlockFile(f);
        }
    }

    @Override
    public void addDownloadTime(long ms) {
        getDownloadLink().addDownloadTime(ms);
    }

    @Override
    public void setLinkStatusText(String label) {
        getDownloadLinkController().getLinkStatus().setStatusText(label);
    }

    @Override
    public long getDownloadTotalBytes() {
        return getDownloadLink().getView().getBytesTotalEstimated();
    }

    @Override
    public void setDownloadBytesLoaded(long bytes) {
        getDownloadLink().setDownloadCurrent(bytes);
    }

    @Override
    public boolean isHashCheckEnabled() {
        if (!getDownloadLink().getBooleanProperty("ALLOW_HASHCHECK", true)) {
            return false;
        }
        if (getClass() == DownloadLinkDownloadable.class) {
            // default implementation checks for isValidHashInfo
            final HashInfo hashInfo = getHashInfo();
            if (!isValidHashInfo(hashInfo)) {
                return false;
            }
        }
        return true;
    }

    protected boolean isValidHashInfo(HashInfo hashInfo) {
        return hashInfo != null && !TYPE.NONE.equals(hashInfo.getType());
    }

    @Override
    public String getName() {
        return getDownloadLink().getName();
    }

    @Override
    public long getKnownDownloadSize() {
        return getDownloadLink().getView().getBytesTotal();
    }

    @Override
    public void addPluginProgress(PluginProgress progress) {
        getDownloadLink().addPluginProgress(progress);
    }

    public HashResult getHashResult(HashInfo hashInfo, File outputPartFile) {
        if (!isValidHashInfo(hashInfo)) {
            return null;
        }
        final TYPE type = hashInfo.getType();
        final PluginProgress hashProgress = new HashCheckPluginProgress(outputPartFile, Color.YELLOW.darker(), type);
        hashProgress.setProgressSource(this);
        try {
            addPluginProgress(hashProgress);
            final byte[] b = new byte[512 * 1024];
            String hashFile = null;
            FileInputStream fis = null;
            try {
                int read = 0;
                long currentPosition = 0;
                switch (type) {
                case WHIRLPOOL:
                    try {
                        fis = new FileInputStream(outputPartFile);
                        crcHashingInProgress = true;
                        final org.bouncycastle.crypto.io.DigestInputStream is = new org.bouncycastle.crypto.io.DigestInputStream(fis, new WhirlpoolDigest());
                        while ((read = is.read(b)) >= 0) {
                            currentPosition += read;
                            hashProgress.setCurrent(currentPosition);
                        }
                        final byte[] hashBytes = new byte[is.getDigest().getDigestSize()];
                        is.getDigest().doFinal(hashBytes, 0);
                        hashFile = HexFormatter.byteArrayToHex(hashBytes);
                        is.close();
                    } catch (final Throwable e) {
                        getLogger().log(e);
                    } finally {
                        crcHashingInProgress = false;
                    }
                    break;
                case MD5:
                case SHA1:
                case SHA224:
                case SHA256:
                case SHA384:
                case SHA512:
                    try {
                        fis = new FileInputStream(outputPartFile);
                        crcHashingInProgress = true;
                        final DigestInputStream is = new DigestInputStream(fis, MessageDigest.getInstance(type.getDigest()));
                        while ((read = is.read(b)) >= 0) {
                            currentPosition += read;
                            hashProgress.setCurrent(currentPosition);
                        }
                        hashFile = HexFormatter.byteArrayToHex(is.getMessageDigest().digest());
                        is.close();
                    } catch (final Throwable e) {
                        getLogger().log(e);
                    } finally {
                        crcHashingInProgress = false;
                    }
                    break;
                case CRC32:
                case CRC32C:
                    try {
                        fis = new FileInputStream(outputPartFile);
                        crcHashingInProgress = true;
                        final Checksum checksum;
                        switch (type) {
                        case CRC32:
                            checksum = new CRC32();
                            break;
                        case CRC32C:
                            checksum = PureJavaCrc32C.newCRC32CChecksumInstance();
                            break;
                        default:
                            return null;
                        }
                        final CheckedInputStream cis = new CheckedInputStream(fis, checksum);
                        while ((read = cis.read(b)) >= 0) {
                            currentPosition += read;
                            hashProgress.setCurrent(currentPosition);
                        }
                        final long value = cis.getChecksum().getValue();
                        cis.close();
                        final byte[] longBytes = new byte[] { (byte) (value >>> 24), (byte) (value >>> 16), (byte) (value >>> 8), (byte) value };
                        hashFile = HexFormatter.byteArrayToHex(longBytes);
                    } catch (final Throwable e) {
                        getLogger().log(e);
                    } finally {
                        crcHashingInProgress = false;
                    }
                    break;
                case NONE:
                    return null;
                default:
                    break;
                }
            } finally {
                if (fis != null) {
                    try {
                        fis.close();
                    } catch (final Throwable e) {
                    }
                }
            }
            return new HashResult(hashInfo, hashFile);
        } finally {
            removePluginProgress(hashProgress);
        }
    }

    @Override
    public HashInfo getHashInfo() {
        final HashInfo hashInfo = getDownloadLink().getHashInfo();
        if (hashInfo != null) {
            return hashInfo;
        }
        final String name = getName();
        final List<HashInfo> hashInfos = new ArrayList<HashInfo>();
        for (final HashInfo.TYPE type : HashInfo.TYPE.values()) {
            if (!type.isAutoMode()) {
                continue;
            }
            final String hash = new Regex(name, ".*?\\[([A-Fa-f0-9]{" + type.getSize() + "})\\]").getMatch(0);
            if (hash != null) {
                hashInfos.add(new HashInfo(hash, type, false));
            }
        }
        final FilePackage filePackage = getDownloadLink().getFilePackage();
        if (!FilePackage.isDefaultFilePackage(filePackage)) {
            final ArrayList<File> checkSumFiles = new ArrayList<File>();
            final boolean readL = filePackage.getModifyLock().readLock();
            try {
                for (final DownloadLink dl : filePackage.getChildren()) {
                    if (dl != getDownloadLink() && FinalLinkState.CheckFinished(dl.getFinalLinkState())) {
                        final File checkSumFile = getFileOutput(dl, false);
                        final String fileName = checkSumFile.getName();
                        if (fileName.matches(".*\\.(sfv|md5|sha1|sha256|sha512)$") && checkSumFile.isFile() && !checkSumFiles.contains(checkSumFile)) {
                            checkSumFiles.add(checkSumFile);
                        }
                    }
                }
            } finally {
                filePackage.getModifyLock().readUnlock(readL);
            }
            final File[] files = new File(filePackage.getDownloadDirectory()).listFiles();
            if (files != null) {
                for (final File file : files) {
                    final String fileName = file.getName();
                    if (fileName.matches(".*\\.(sfv|md5|sha1|sha256|sha512)$") && file.isFile() && !checkSumFiles.contains(file)) {
                        checkSumFiles.add(file);
                    }
                }
            }
            for (final File checkSumFile : checkSumFiles) {
                try {
                    if (checkSumFile.length() < 1024 * 512) {
                        // Avoid OOM
                        // TODO: instead of checking file size, better use line reader with limited inputstream and check line by line
                        final String content = IO.readFileToString(checkSumFile);
                        if (StringUtils.isNotEmpty(content)) {
                            final String lines[] = Regex.getLines(content);
                            for (final String line : lines) {
                                if (line.startsWith(";") || !line.contains(name)) {
                                    continue;
                                }
                                for (final HashInfo.TYPE type : HashInfo.TYPE.values()) {
                                    if (!type.isAutoMode()) {
                                        continue;
                                    }
                                    final String hash = new Regex(line, "(?:^|\\s+)([A-Fa-f0-9]{" + type.getSize() + "})(\\s+|$)").getMatch(0);
                                    if (hash != null) {
                                        hashInfos.add(new HashInfo(hash, type));
                                    }
                                }
                            }
                        }
                    }
                } catch (IOException e) {
                    getLogger().log(e);
                }
            }
        }
        if (hashInfos.size() == 1) {
            return hashInfos.get(0);
        } else if (hashInfos.size() > 1) {
            HashInfo best = null;
            for (final HashInfo info : hashInfos) {
                if (best == null) {
                    best = info;
                } else if (info.isStrongerThan(best) && (info.isTrustworthy() || !best.isTrustworthy())) {
                    best = info;
                } else if (info.isTrustworthy() && !best.isTrustworthy()) {
                    best = info;
                }
            }
            return best;
        } else {
            return null;
        }
    }

    private File getFileOutput(DownloadLink link, boolean ignoreCustom) {
        final SingleDownloadController controller = link.getDownloadLinkController();
        if (controller == null) {
            return new File(link.getFileOutput(false, ignoreCustom));
        } else {
            return controller.getFileOutput(false, ignoreCustom);
        }
    }

    @Override
    @Deprecated
    public long[] getChunksProgress() {
        return getDownloadLink().getView().getChunksProgress();
    }

    @Override
    @Deprecated
    public void setChunksProgress(long[] ls) {
        getDownloadLink().setChunksProgress(ls);
    }

    public PluginForHost getPlugin() {
        return plugin;
    }

    @Override
    public void setHashResult(HashResult result) {
        getDownloadLinkController().setHashResult(result);
    }

    @Override
    public boolean rename(final File outputPartFile, final File outputCompleteFile) throws InterruptedException {
        boolean renameOkay = false;
        int retry = 5;
        /* rename part file to final filename */
        while (retry > 0) {
            /* first we try normal rename method */
            if ((renameOkay = outputPartFile.renameTo(outputCompleteFile)) == true) {
                break;
            }
            /* this may fail because something might lock the file */
            Thread.sleep(1000);
            retry--;
        }
        /* Fallback */
        if (renameOkay == false) {
            /* rename failed, lets try fallback */
            getLogger().severe("Could not rename file " + outputPartFile + " to " + outputCompleteFile);
            getLogger().severe("Try copy workaround!");
            DiskSpaceReservation reservation = new DiskSpaceReservation() {
                @Override
                public long getSize() {
                    return outputPartFile.length() - outputCompleteFile.length();
                }

                @Override
                public File getDestination() {
                    return outputCompleteFile;
                }

                @Override
                public Object getOwner() {
                    return DownloadLinkDownloadable.this;
                }

                @Override
                public LogInterface getLogger() {
                    return DownloadLinkDownloadable.this.getLogger();
                }
            };
            try {
                try {
                    DISKSPACERESERVATIONRESULT result = DownloadWatchDog.getInstance().getSession().getDiskSpaceManager().checkAndReserve(reservation, this);
                    switch (result) {
                    case OK:
                    case UNSUPPORTED:
                        IO.copyFile(outputPartFile, outputCompleteFile);
                        renameOkay = true;
                        outputPartFile.delete();
                        break;
                    }
                } finally {
                    DownloadWatchDog.getInstance().getSession().getDiskSpaceManager().free(reservation, this);
                }
            } catch (Throwable e) {
                getLogger().log(e);
                /* error happened, lets delete complete file */
                if (outputCompleteFile.exists() && outputCompleteFile.length() != outputPartFile.length()) {
                    FileCreationManager.getInstance().delete(outputCompleteFile, null);
                }
            }
            if (!renameOkay) {
                getLogger().severe("Copy workaround: :(");
            } else {
                getLogger().severe("Copy workaround: :)");
            }
        }
        return renameOkay;
    }

    @Override
    public void waitForNextConnectionAllowed() throws InterruptedException {
        getPlugin().waitForNextConnectionAllowed(getDownloadLink());
    }

    @Override
    public boolean isInterrupted() {
        final SingleDownloadController sdc = getDownloadLinkController();
        return (sdc != null && sdc.isAborting());
    }

    @Override
    public String getFileOutput() {
        return getFileOutput(getDownloadLink(), false).getAbsolutePath();
    }

    @Override
    public int getLinkStatus() {
        return getDownloadLinkController().getLinkStatus().getStatus();
    }

    @Override
    public String getFileOutputPart() {
        return getFileOutput() + ".part";
    }

    @Override
    public String getFinalFileOutput() {
        return getFileOutput(getDownloadLink(), true).getAbsolutePath();
    }

    @Override
    public boolean isResumable() {
        return getDownloadLink().isResumeable();
    }

    @Override
    public DiskSpaceReservation createDiskSpaceReservation() {
        return new DiskSpaceReservation() {
            @Override
            public long getSize() {
                final File partFile = new File(getFileOutputPart());
                final long doneSize = Math.max((partFile.exists() ? partFile.length() : 0l), getDownloadBytesLoaded());
                return getKnownDownloadSize() - Math.max(0, doneSize);
            }

            @Override
            public File getDestination() {
                return new File(getFileOutput());
            }

            @Override
            public Object getOwner() {
                return DownloadLinkDownloadable.this;
            }

            @Override
            public LogInterface getLogger() {
                return DownloadLinkDownloadable.this.getLogger();
            }
        };
    }

    @Override
    public void checkAndReserve(DiskSpaceReservation reservation) throws Exception {
        DISKSPACERESERVATIONRESULT result = DownloadWatchDog.getInstance().getSession().getDiskSpaceManager().checkAndReserve(reservation, getDownloadLinkController());
        switch (result) {
        case FAILED:
            throw new SkipReasonException(SkipReason.DISK_FULL);
        case INVALIDDESTINATION:
            throw new SkipReasonException(SkipReason.INVALID_DESTINATION);
        }
    }

    @Override
    public void free(DiskSpaceReservation reservation) {
        DownloadWatchDog.getInstance().getSession().getDiskSpaceManager().free(reservation, getDownloadLinkController());
    }

    @Override
    public long getDownloadBytesLoaded() {
        return getDownloadLink().getView().getBytesLoaded();
    }

    @Override
    public boolean removePluginProgress(PluginProgress remove) {
        return getDownloadLink().removePluginProgress(remove);
    }

    @Override
    public <T> T getDataBindingInterface(Class<? extends DownloadLinkDatabindingInterface> T) {
        return (T) getDownloadLink().bindData(T);
    }

    protected boolean isAllowFilenameFromURL(URLConnectionAdapter connection) {
        final DownloadInterface dl = getDownloadInterface();
        return dl != null && dl.allowFilenameFromURL;
    }

    protected boolean isFixWrongEncoding(URLConnectionAdapter connection, final String fileName) {
        final DownloadInterface dl = getDownloadInterface();
        return dl != null && dl.fixWrongContentDispositionHeader;
    }

    protected String fixWrongEncoding(URLConnectionAdapter connection, final String fileName) {
        final List<String[]> results = Plugin.decodeURIComponentFindBestEncoding(fileName);
        if (results != null && results.size() > 0) {
            return results.get(0)[1];
        } else {
            return fileName;
        }
    }

    @Override
    public void updateFinalFileName() {
        final LogInterface logger = getLogger();
        final DownloadInterface dl = getDownloadInterface();
        final URLConnectionAdapter connection = dl.getConnection();
        final SingleDownloadController controller = getDownloadLinkController();
        final PluginForHost plugin = getPlugin();
        final DownloadLink downloadLink = getDownloadLink();
        if (controller.getSessionDownloadFilename() != null) {
            // SessionDownloadFilename -> either ForcedFileName or due to rename of the file
            final String forcedFileName = FILENAME_SOURCE.FORCED.getFilename(plugin, downloadLink, connection);
            if (controller.getDownloadLink() == downloadLink) {
                // required for the auto rename handling as we already have
                controller.setSessionDownloadFilename(forcedFileName);
            }
            if (setFilename(FILENAME_SOURCE.FORCED, forcedFileName)) {
                logger.info("updateForcedName: update to '" + forcedFileName + "' Content-Type:" + connection.getContentType());
            }
            return;
        }
        final String existingFinalFilename = FILENAME_SOURCE.FINAL.getFilename(plugin, downloadLink, connection);
        if (existingFinalFilename != null) {
            if (setFilename(FILENAME_SOURCE.FINAL, existingFinalFilename)) {
                logger.info("updateFinalName: update to '" + existingFinalFilename + "' Content-Type:" + connection.getContentType());
            }
            return;
        }
        final String fileNameFromDispositionHeader = FILENAME_SOURCE.HEADER.getFilename(plugin, downloadLink, connection);
        if (StringUtils.isNotEmpty(fileNameFromDispositionHeader)) {
            /* Get filename from content-disposition header */
            final DispositionHeader dispositonHeader = getPlugin().getDispositionHeader(connection);
            final String newFinalFilename;
            if ((dispositonHeader == null || dispositonHeader.getEncoding() == null) && isFixWrongEncoding(connection, fileNameFromDispositionHeader)) {
                newFinalFilename = fixWrongEncoding(connection, fileNameFromDispositionHeader);
            } else {
                newFinalFilename = fileNameFromDispositionHeader;
            }
            if (setFilename(FILENAME_SOURCE.HEADER, newFinalFilename)) {
                logger.info("updateFinalFileName: set to '" + newFinalFilename + "' from connection:" + dispositonHeader + "|Content-Type:" + connection.getContentType() + "|fixEncoding:" + !StringUtils.equals(newFinalFilename, fileNameFromDispositionHeader));
            }
            return;
        }
        final String fileNameFromURL = FILENAME_SOURCE.URL.getFilename(plugin, downloadLink, connection);
        if (StringUtils.isNotEmpty(fileNameFromURL) && isAllowFilenameFromURL(connection)) {
            /* Get filename from URL */
            final String newFinalFilename;
            if (isFixWrongEncoding(connection, fileNameFromURL)) {
                newFinalFilename = fixWrongEncoding(connection, fileNameFromURL);
            } else {
                newFinalFilename = fileNameFromURL;
            }
            if (setFilename(FILENAME_SOURCE.URL, newFinalFilename)) {
                logger.info("updateFinalFileName: set to '" + newFinalFilename + "' from url:" + connection.getURL().getPath() + "|Content-Type:" + connection.getContentType() + "|fixEncoding:" + !StringUtils.equals(newFinalFilename, fileNameFromURL));
            }
            return;
        }
    }

    protected boolean setFilename(FILENAME_SOURCE source, final String fileName) {
        return source.setFilename(this, fileName);
    }

    protected String decodeURIComponent(final String name, String charSet) {
        try {
            if (StringUtils.isEmpty(charSet)) {
                charSet = "UTF-8";
            }
            return URLEncode.decodeURIComponent(name, charSet, true);
        } catch (final IllegalArgumentException ignore) {
            getLogger().log(ignore);
        } catch (final UnsupportedEncodingException ignore) {
            getLogger().log(ignore);
        }
        return name;
    }

    @Override
    public DownloadInterface getDownloadInterface() {
        return getPlugin().getDownloadInterface();
    }

    public void setHashInfo(final HashInfo hashInfo) {
        if (hashInfo == null) {
            return;
        } else if (!hashInfo.isTrustworthy()) {
            return;
        }
        final HashInfo existingHashInfo = getHashInfo();
        if (existingHashInfo == null || hashInfo.equals(existingHashInfo) || hashInfo.isStrongerThan(existingHashInfo)) {
            getDownloadLink().setHashInfo(hashInfo);
        }
    }

    @Override
    public int getChunks() {
        return getDownloadLink().getChunks();
    }

    public static boolean isCrcHashingInProgress() {
        return crcHashingInProgress;
    }
}