package org.jdownloader.api.logs;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.exceptions.WTFException;
import org.appwork.remoteapi.exceptions.BadParameterException;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.logging2.LogSink.FLUSH;
import org.appwork.utils.logging2.LogSourceProvider;
import org.appwork.utils.logging2.sendlogs.AbstractLogAction;
import org.appwork.utils.logging2.sendlogs.LogFolder;
import org.appwork.utils.zip.ZipIOException;
import org.appwork.utils.zip.ZipIOWriter;
import org.jdownloader.jdserv.JDServUtils;
import org.jdownloader.logging.LogController;
import org.jdownloader.startup.commands.ThreadDump;

public class LogAPIImpl implements LogAPI {
    public static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("dd.MM.yy HH.mm.ss", Locale.GERMANY);

    @Override
    public List<LogFolderStorable> getAvailableLogs() {
        new ThreadDump().run(null, new String[0]);
        final ArrayList<LogFolder> folders = AbstractLogAction.getLogFolders();
        final ArrayList<LogFolderStorable> result = new ArrayList<LogFolderStorable>();
        for (final LogFolder folder : folders) {
            if (isCurrentLogFolder(folder.getCreated())) {
                folder.setCurrent(true);
                folder.setNeedsFlush(true);
                result.add(0, new LogFolderStorable(folder));
            } else {
                result.add(new LogFolderStorable(folder));
            }
        }
        return result;
    }

    private boolean isCurrentLogFolder(long timestamp) {
        final long startup = LogController.getInstance().getInitTime();
        return startup == timestamp;
    }

    @Override
    public String sendLogFile(final LogFolderStorable[] selectedFolders) throws BadParameterException {
        if (selectedFolders == null || selectedFolders.length == 0) {
            throw new BadParameterException("selection empty or null");
        }
        final ArrayList<LogFolder> availableLogFolders = AbstractLogAction.getLogFolders();
        final ArrayList<LogFolder> logFoldersToSend = new ArrayList<LogFolder>();
        for (final LogFolderStorable selectedFolder : selectedFolders) {
            for (final LogFolder availableLogFolder : availableLogFolders) {
                if (isCurrentLogFolder(availableLogFolder.getCreated()) || (availableLogFolder.getCreated() == selectedFolder.getCreated() && availableLogFolder.getLastModified() >= selectedFolder.getLastModified())) {
                    // always select current session too
                    logFoldersToSend.add(availableLogFolder);
                    break;
                }
            }
        }
        if (!logFoldersToSend.isEmpty()) {
            LogSourceProvider.flushAllSinks(FLUSH.FORCE);
            final AtomicReference<String> logIDRef = new AtomicReference<String>(null);
            final Thread uploadThread = new Thread(new Runnable() {
                @Override
                public void run() {
                    String logID = null;
                    File zip = null;
                    try {
                        for (final LogFolder logFolder : logFoldersToSend) {
                            if (zip != null) {
                                zip.delete();
                            }
                            zip = createPackage(logFolder);
                            logID = JDServUtils.uploadLog(zip, logID);
                            if (logID != null && logIDRef.get() == null) {
                                synchronized (logIDRef) {
                                    logIDRef.set(logID);
                                    logIDRef.notify();
                                }
                            }
                        }
                    } catch (Throwable th) {
                        LogController.CL().log(th);
                    } finally {
                        synchronized (logIDRef) {
                            logIDRef.notifyAll();
                        }
                        if (zip != null) {
                            zip.delete();
                        }
                    }
                }
            }, "LogAPIImpl:sendLogFile");
            uploadThread.setDaemon(true);
            uploadThread.start();
            synchronized (logIDRef) {
                if (uploadThread.isAlive() && logIDRef.get() == null) {
                    try {
                        logIDRef.wait();
                    } catch (InterruptedException e) {
                    }
                }
                return logIDRef.get();
            }
        }
        return null;
    }

    private File createPackage(final LogFolder lf) throws Exception {
        final File zip = Application.getTempResource("logs/logPackage" + System.currentTimeMillis() + ".zip");
        zip.delete();
        zip.getParentFile().mkdirs();
        final ZipIOWriter writer = new ZipIOWriter(zip) {
            @Override
            protected void addFile(File addFile, boolean compress, String fullPath) throws ZipIOException {
                if (addFile.getName().endsWith(".lck") || addFile.isFile() && addFile.length() == 0) {
                    return;
                } else if (Thread.currentThread().isInterrupted()) {
                    throw new WTFException("INterrupted");
                } else {
                    super.addFile(addFile, compress, fullPath);
                }
            }
        };
        try {
            final String name = lf.getFolder().getName() + "-" + DATE_FORMAT.format(lf.getCreated()) + " to " + DATE_FORMAT.format(lf.getLastModified());
            final File folder = Application.getTempResource("logs/" + name);
            if (lf.isNeedsFlush()) {
                LogController.getInstance().flushSinks(FLUSH.FORCE);
            }
            if (folder.exists()) {
                Files.deleteRecursive(folder);
            }
            IO.copyFolderRecursive(lf.getFolder(), folder, true);
            writer.add(folder, true);
        } finally {
            writer.close();
        }
        return zip;
    }
}
