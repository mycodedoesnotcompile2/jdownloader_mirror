/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.loggingv3.simple.sink;

import java.io.BufferedInputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.StandardCopyOption;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.appwork.loggingv3.LogV3;
import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Files;
import org.appwork.utils.IO;

/**
 * @author Thomas
 * @date 19.09.2018
 *
 */
public class LogToFileSink extends AbstractSink {
    /**
     *
     */
    private static final String           REGEX_DATE_LOGS    = "^logs_(.*)_\\d+(\\.zip)?$";
    /**
     *
     */
    private static final SimpleDateFormat SIMPLE_DATE_FORMAT = new SimpleDateFormat("yyyy.MM.dd_HH.mm.ss.SSS");
    private final String                  filepattern;
    private final File                    logRoot;
    protected volatile File               logFolder;
    private final String                  timeTag;
    private volatile boolean              shutdown;
    private volatile boolean              enabled            = true;
    private volatile FileLock             logFolderLock      = null;

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * @param string
     */
    public LogToFileSink(File root, String filepattern, int zipLevel) {
        logRoot = root;
        logRoot.mkdirs();
        timeTag = createTimeTag();
        if (!filepattern.contains("\\d")) {
            filepattern += ".\\d";
        }
        this.filepattern = filepattern;
        this.zipLevel = zipLevel;
        Runtime.getRuntime().addShutdownHook(new Thread("ShutdownHook: Logger") {
            /*
             * (non-Javadoc)
             *
             * @see java.lang.Thread#run()
             */
            @Override
            public void run() {
                onShutdown();
            }
        });
        initPostCleanupAndCompressionThread();
    }

    protected void initPostCleanupAndCompressionThread() {
        new Thread("Cleanup & compress Logs:" + logRoot + "|" + filepattern) {
            {
                setDaemon(true);
            }

            public void run() {
                // delay to not slow down the startup
                try {
                    Thread.sleep(2 * 60 * 1000l);
                    runPostCleanupAndCompressionThread();
                } catch (InterruptedException e1) {
                }

            };
        }.start();
    }

    public static class LogFolder {
        public final File path;
        public final long time;

        /**
         * @param f
         * @param time
         */
        public LogFolder(File f, long time) {
            path = f;
            this.time = time;
        }

        /**
         * @return
         */
        public long getSize() {
            if (path.isFile()) {
                return path.length();
            } else {
                return Files.getDirectorySize(path);
            }
        }
    }

    public void keepOnlyLatestByBytes(long bytes) {
        final ArrayList<LogFolder> logFolders = getLogFilesOrFolders(true);
        Collections.sort(logFolders, new Comparator<LogFolder>() {
            @Override
            public int compare(LogFolder o1, LogFolder o2) {
                return CompareUtils.compare(o2.time, o1.time);
            }
        });
        long sizeSoFar = 0l;
        for (final LogFolder f : logFolders) {
            sizeSoFar += f.getSize();
            if (sizeSoFar > bytes) {
                LogV3.info("Cleanup Logs: " + f.path);
                try {
                    Files.deleteRecursive(f.path, false);
                } catch (IOException e) {
                    LogV3.defaultLogger().exception("Could not delete Logfolder: " + f.path, e);
                }
            }
        }
    }

    /**
     * @return
     */
    public ArrayList<LogFolder> getLogFilesOrFolders(final boolean excludeFoldersInUse) {
        final File[] folders = logRoot.listFiles(createFileFilter());
        final ArrayList<LogFolder> files = new ArrayList<LogFolder>();
        if (folders != null) {
            final File currentLogFolder = this.logFolder;
            for (final File f : folders) {
                if (excludeFoldersInUse) {
                    if (currentLogFolder != null && currentLogFolder.equals(f)) {
                        continue;
                    } else if (f.isDirectory()) {
                        final FileLock lock = tryLock(f);
                        if (lock == null) {
                            continue;
                        } else {
                            releaseLock(lock);
                        }
                    }
                }
                final Matcher matcher = Pattern.compile(REGEX_DATE_LOGS).matcher(f.getName());
                if (matcher.find()) {
                    try {
                        final Date date = SIMPLE_DATE_FORMAT.parse(matcher.group(1));
                        if (date != null) {
                            files.add(new LogFolder(f, date.getTime()));
                        }
                    } catch (ParseException e) {
                    }
                }
            }
        }
        return files;
    }

    /**
     * @return
     */
    protected FilenameFilter createFileFilter() {
        return new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                // if (logFolder != null && name.equals(logFolder.getName())) {
                //
                // }
                Matcher matcher = Pattern.compile(REGEX_DATE_LOGS).matcher(name);
                if (matcher.find()) {
                    Date date = null;
                    try {
                        date = SIMPLE_DATE_FORMAT.parse(matcher.group(1));
                    } catch (ParseException e) {
                    }
                    if (date != null) {
                        return true;
                    }
                }
                return false;
            }
        };
    }

    /**
     * cleanup and compress log folders from previous sessions. The cleanup process runs in a shutdown hook, and may have been interrupted,
     * or the process itself may have been killed. In this case, this code cleans up old folders and ensures that all are compressed
     * properly
     */
    protected void runPostCleanupAndCompressionThread() {
        synchronized (WORK_ON_FOLDERS_AND_FILES_LOCK) {
            for (LogFolder f : getLogFilesOrFolders(true)) {
                if (f.path.isFile()) {
                    continue;
                }
                try {
                    File tmp = null;
                    while (tmp == null || tmp.exists()) {
                        tmp = new File(f.path.getAbsoluteFile() + "." + System.currentTimeMillis());
                    }
                    if (f.path.renameTo(tmp)) {
                        tmp.renameTo(f.path);
                        packToSingleZip(f.path);
                    }
                } catch (Throwable e) {
                    LogV3.logger(LogToFileSink.class).exception("Failed to compress old logfolder " + f, e);
                }
            }
        }
    }

    public File getLogRoot() {
        return logRoot;
    }

    public File getLogFolder() {
        return logFolder;
    }

    private boolean releaseLock(final FileLock lock) {
        boolean ret = false;
        try {
            if (lock != null) {
                lock.release();
                ret = true;
                lock.channel().close();
                ret = true;
                final File lockFile;
                synchronized (lockFiles) {
                    lockFile = lockFiles.remove(lock);
                }
                if (lockFile != null) {
                    lockFile.delete();
                }
            }
        } catch (Exception ignore) {
        }
        return ret;
    }

    /**
     *
     */
    protected void onShutdown() {
        synchronized (this) {
            this.shutdown = true;
            closeOldFile();
            releaseLock(logFolderLock);
            while (compressionThreadsRunning.get() > 0) {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    return;
                }
            }
            runPackToSingleFileOnShutdown();
        }
    }

    protected void runPackToSingleFileOnShutdown() {
        final File logFolder = this.logFolder;
        if (logFolder != null) {
            packToSingleZip(logFolder, new File(logFolder.getAbsolutePath() + ".zip"), true);
        }
    }

    /**
     * @param f
     */
    private void packToSingleZip(File logFolder) {
        packToSingleZip(logFolder, new File(logFolder.getAbsolutePath() + ".zip"), true);
    }

    /**
     * @param logFolder2
     */
    private void packToSingleZip(File logFolder, File zip, boolean delete) {
        try {
            if (!logFolder.exists() || !logFolder.isDirectory()) {
                return;
            }
            if (zip.isFile()) {
                // zip only exists if it is complete
                // if the process got killed during cleanup, we have to delete remaining .deleteMe files
                for (File f : logFolder.listFiles()) {
                    if (f.getName().endsWith(".deleteMe")) {
                        f.delete();
                    }
                }
                if (logFolder.listFiles().length == 0) {
                    logFolder.delete();
                }
                return;
            }
            File tmp = new File(zip.getAbsolutePath() + ".tmp");
            java.nio.file.Files.deleteIfExists(tmp.toPath());
            ZipOutputStream zipout = new ZipOutputStream(new FileOutputStream(tmp));
            try {
                zipout.setLevel(zipLevel);
                writeFolderToZip(logFolder, delete, zipout, false);
                zipout.close();
                java.nio.file.Files.move(tmp.toPath(), zip.toPath(), StandardCopyOption.ATOMIC_MOVE);
                for (File f : logFolder.listFiles()) {
                    if (f.getName().endsWith(".deleteMe")) {
                        f.delete();
                    }
                }
                if (logFolder.listFiles().length == 0) {
                    logFolder.delete();
                }
            } finally {
                zipout.close();
            }
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    protected void writeFolderToZip(File logFolder, boolean delete, ZipOutputStream zipout, boolean addSubfolder) throws FileNotFoundException, IOException, Error, InterruptedException {
        try {
            for (File f : logFolder.listFiles()) {
                if (Thread.interrupted()) {
                    throw new InterruptedException();
                }
                if (f.isFile()) {
                    if (f.getName().endsWith(".zip")) {
                        ZipInputStream zis = new ZipInputStream(new BufferedInputStream(new FileInputStream(f)));
                        try {
                            ZipEntry entry;
                            while ((entry = zis.getNextEntry()) != null) {
                                if (Thread.interrupted()) {
                                    throw new InterruptedException();
                                }
                                if (addSubfolder) {
                                    zipout.putNextEntry(new ZipEntry(logFolder.getName() + "/" + entry.getName()));
                                } else {
                                    zipout.putNextEntry(new ZipEntry(entry.getName()));
                                }
                                try {
                                    IO.readStreamToOutputStream(-1, zis, zipout, false);
                                } finally {
                                    zipout.closeEntry();
                                    zipout.flush();
                                }
                            }
                        } finally {
                            zis.close();
                        }
                        if (delete) {
                            java.nio.file.Files.move(f.toPath(), new File(f.getAbsolutePath() + ".deleteMe").toPath(), StandardCopyOption.ATOMIC_MOVE);
                        }
                    } else if (f.getName().endsWith(".txt")) {
                        if (addSubfolder) {
                            zipout.putNextEntry(new ZipEntry(logFolder.getName() + "/" + f.getName()));
                        } else {
                            zipout.putNextEntry(new ZipEntry(f.getName()));
                        }
                        try {
                            IO.readStreamToOutputStream(-1, new BufferedInputStream(new FileInputStream(f)), zipout, true);
                        } finally {
                            zipout.closeEntry();
                            zipout.flush();
                        }
                        if (delete) {
                            java.nio.file.Files.move(f.toPath(), new File(f.getAbsolutePath() + ".deleteMe").toPath(), StandardCopyOption.ATOMIC_MOVE);
                        }
                    }
                }
            }
        } catch (IOException e) {
            if (Thread.interrupted()) {
                throw Exceptions.addSuppressed(new InterruptedException(), e);
            }
            throw e;
        }
    }

    protected String createTimeTag() {
        return SIMPLE_DATE_FORMAT.format(new Date());
    }

    private File currentFile = null;

    public File getCurrentFile() {
        return currentFile;
    }

    private volatile BufferedWriter fos      = null;
    private int                     zipLevel = 3;

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.loggingv3.simple.sink.Sink#publish(java.lang.String)
     */
    @Override
    public void publish(LogRecord2 record) {
        synchronized (this) {
            if (!isEnabled()) {
                return;
            } else if (shutdown) {
                return;
            } else if (counter == null || counter.written >= getMaxFileSize()) {
                nextFile();
            }
            try {
                final BufferedWriter fos = this.fos;
                if (fos != null) {
                    fos.write(format(record) + "\r\n");
                    fos.flush();
                }
            } catch (IOException e) {
                // TODO: WAS JETZT?
                e.printStackTrace();
            }
        }
    }

    int                          files = 0;
    private CountingOutputStream counter;

    /**
     *
     */
    private void nextFile() {
        try {
            closeOldFile();
            if (logFolder == null) {
                initLogFolder();
            }
            files++;
            File file = null;
            while (file == null || file.exists()) {
                file = new File(logFolder, filepattern.replace("\\d", createFileIndexTag(files) + "-" + createTimeTag()));
                if (file.exists()) {
                    files++;
                }
            }
            file.getParentFile().mkdirs();
            fos = new BufferedWriter(new OutputStreamWriter(counter = new CountingOutputStream(new FileOutputStream(file)), "UTF-8"));
            this.currentFile = file;
        } catch (IOException e) {
            // TODO: WAS JETZT?
            e.printStackTrace();
        }
    }

    private final Map<FileLock, File> lockFiles = new HashMap<FileLock, File>();

    @SuppressWarnings("resource")
    protected FileLock tryLock(final File folder) {
        FileChannel channel = null;
        try {
            if (folder.isFile()) {
                return null;
            } else if (!folder.exists()) {
                folder.mkdirs();
            }
            final File lockFile = new File(folder, "lock.lck");
            channel = new FileOutputStream(lockFile).getChannel();
            final FileLock ret = channel.tryLock();
            synchronized (lockFiles) {
                lockFiles.put(ret, lockFile);
            }
            return ret;
        } catch (Exception ignore) {
            if (channel != null) {
                try {
                    channel.close();
                } catch (Exception ignore2) {
                }
            }
            return null;
        }
    }

    protected void initLogFolder() {
        int i = 1;
        while (logFolder == null || logFolder.exists()) {
            logFolder = new File(logRoot, "logs_" + timeTag + "_" + (i++));
            logFolderLock = tryLock(logFolder);
            if (logFolderLock == null) {
                logFolder = null;
                continue;
            } else {
                break;
            }
        }
    }

    private AtomicInteger compressionThreadsRunning = new AtomicInteger(0);

    /**
     *
     */
    protected void closeOldFile() {
        synchronized (this) {
            try {
                final BufferedWriter fos = this.fos;
                this.fos = null;
                if (fos != null) {
                    fos.close();
                    startAsyncCompressionAfterLogRotation();
                }
            } catch (IOException e1) { // TODO: WAS JETZT?
                e1.printStackTrace();
            }
        }
    }

    protected void startAsyncCompressionAfterLogRotation() {
        final File cf = currentFile;
        Thread th = new Thread("compress log") {
            public void run() {
                synchronized (WORK_ON_FOLDERS_AND_FILES_LOCK) {
                    try {
                        compress(cf, new File(cf.getAbsolutePath() + ".zip"));
                    } finally {
                        compressionThreadsRunning.decrementAndGet();
                    }
                }
            };
        };
        compressionThreadsRunning.incrementAndGet();
        th.start();
    }

    /**
     * @param currentFile2
     * @param file
     */
    protected void compress(File file, File zip) {
        try {
            ZipOutputStream zipout;
            zip.delete();
            File tmp = new File(zip.getAbsolutePath() + ".tmp");
            tmp.delete();
            zipout = new ZipOutputStream(new FileOutputStream(tmp));
            zipout.setLevel(zipLevel);
            zipout.putNextEntry(new ZipEntry(file.getName()));
            IO.readStreamToOutputStream(-1, new BufferedInputStream(new FileInputStream(file)), zipout, true);
            zipout.closeEntry();
            zipout.close();
            java.nio.file.Files.move(tmp.toPath(), zip.toPath(), StandardCopyOption.ATOMIC_MOVE);
            file.delete();
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    class CountingOutputStream extends OutputStream {
        private final OutputStream os;
        private volatile long      written = 0;

        public CountingOutputStream(final OutputStream os) {
            this.os = os;
        }

        @Override
        public void close() throws IOException {
            this.os.close();
        }

        @Override
        public void flush() throws IOException {
            this.os.flush();
        }

        @Override
        public void write(final byte b[]) throws IOException {
            this.os.write(b);
            this.written += b.length;
        }

        public void setTransferedBytes(long written) {
            if (written >= 0) {
                this.written = written;
            }
        }

        @Override
        public void write(final byte b[], final int off, final int len) throws IOException {
            this.os.write(b, off, len);
            this.written += len;
        }

        @Override
        public void write(final int b) throws IOException {
            this.os.write(b);
            this.written++;
        }
    }

    /**
     * @param files2
     * @return
     */
    protected String createFileIndexTag(int files2) {
        String ret = String.valueOf(files2);
        while (ret.length() < 3) {
            ret = "0" + ret;
        }
        return ret;
    }

    private int maxFileSize = 50 * 1024 * 1024;

    public int getMaxFileSize() {
        return maxFileSize;
    }

    public void setMaxFileSize(int maxFileSize) {
        this.maxFileSize = maxFileSize;
    }

    private final Object WORK_ON_FOLDERS_AND_FILES_LOCK = new Object();

    /**
     * @param days
     * @return
     * @throws Error
     * @throws IOException
     * @throws InterruptedException
     */
    public File exportFull(long days) throws IOException, Error, InterruptedException {
        synchronized (WORK_ON_FOLDERS_AND_FILES_LOCK) {
            ArrayList<LogFolder> logFolders = getLogFilesOrFolders(false);
            Collections.sort(logFolders, new Comparator<LogFolder>() {
                @Override
                public int compare(LogFolder o1, LogFolder o2) {
                    return CompareUtils.compare(o1.time, o2.time);
                }
            });
            String name;
            if (logFolders.size() > 1) {
                name = SIMPLE_DATE_FORMAT.format(new Date(logFolders.get(0).time)) + " - " + SIMPLE_DATE_FORMAT.format(new Date(logFolders.get(logFolders.size() - 1).time));
            } else {
                name = SIMPLE_DATE_FORMAT.format(new Date(logFolders.get(0).time));
            }
            File zip = new File(logRoot, "packages/" + name + ".zip");
            zip.getParentFile().mkdirs();
            File tmp = new File(zip.getAbsolutePath() + ".tmp");
            ZipOutputStream zipout = new ZipOutputStream(new FileOutputStream(tmp));
            IOException ioException = null;
            try {
                zipout.setLevel(4);
                extendExport(zipout);
                for (LogFolder f : logFolders) {
                    if (Thread.interrupted()) {
                        throw new InterruptedException();
                    }
                    if (f.time < days) {
                        continue;
                    }
                    if (f.path.isFile()) {
                        ZipInputStream zis = new ZipInputStream(new BufferedInputStream(new FileInputStream(f.path)));
                        try {
                            ZipEntry entry;
                            while ((entry = zis.getNextEntry()) != null) {
                                if (Thread.interrupted()) {
                                    throw new InterruptedException();
                                }
                                zipout.putNextEntry(new ZipEntry(Files.getFileNameWithoutExtension(f.path.getName()) + "/" + entry.getName()));
                                IO.readStreamToOutputStream(-1, zis, zipout, false);
                                zipout.closeEntry();
                            }
                        } finally {
                            zis.close();
                        }
                    } else {
                        if (logFolder != null && f.path.equals(logFolder)) {
                            synchronized (this) {
                                boolean restoreFos = false;
                                if (fos != null) {
                                    restoreFos = true;
                                    fos.close();
                                }
                                try {
                                    writeFolderToZip(f.path, false, zipout, true);
                                } finally {
                                    if (restoreFos) {
                                        CountingOutputStream oldCounter = counter;
                                        fos = new BufferedWriter(new OutputStreamWriter(counter = new CountingOutputStream(new FileOutputStream(currentFile, true)), "UTF-8"));
                                        counter.written = oldCounter.written;
                                    }
                                }
                            }
                        } else {
                            writeFolderToZip(f.path, false, zipout, true);
                        }
                    }
                }
            } catch (IOException e) {
                ioException = e;
            } finally {
                zipout.close();
            }
            if (ioException != null) {
                tmp.delete();
                if (Thread.interrupted()) {
                    throw Exceptions.addSuppressed(new InterruptedException(), ioException);
                }
                throw ioException;
            }
            java.nio.file.Files.move(tmp.toPath(), zip.toPath(), StandardCopyOption.ATOMIC_MOVE);
            return zip;
        }
    }

    /**
     * @param zipout
     * @throws IOException
     */
    protected void extendExport(ZipOutputStream zipout) throws IOException {
        // TODO Auto-generated method stub
    }
}
