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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.logging2;

/**
 * @author daniel
 *
 */
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.sql.Date;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicBoolean;

import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogSink.FLUSH;
import org.appwork.utils.os.CrossSystem;

public abstract class LogSourceProvider {
    /**
     *
     */
    /**
     *
     */
    protected static final String            LOG_INIT_DONE = "LOG_INIT_DONE";
    protected final HashMap<String, LogSink> logSinks      = new HashMap<String, LogSink>();
    private final int                        maxSize;
    private final int                        maxLogs;
    // Do not set final!
    protected long                           logTimeout;

    public long getLogTimeout() {
        return logTimeout;
    }

    public void setLogTimeout(long logTimeout) {
        this.logTimeout = logTimeout;
    }

    protected Thread            flushThread = null;
    protected final File        logFolder;
    protected LogConsoleHandler consoleHandler;
    protected boolean           instantFlushDefault;
    private final boolean       debugMode;

    public boolean isDebugMode() {
        return debugMode;
    }

    private long                   initTime;
    private final boolean          writeLogs;
    private File                   logBaseFolder;
    static List<LogSourceProvider> INSTANCES = new ArrayList<LogSourceProvider>();

    protected static List<LogSourceProvider> getInstances() {
        synchronized (INIT_LOCK) {
            return new ArrayList<LogSourceProvider>(INSTANCES);
        }
    }

    public boolean isWriteLogs() {
        return writeLogs;
    }

    private final static AtomicBoolean TRASHLOCK              = new AtomicBoolean(false);
    static final Object                INIT_LOCK              = new Object();
    public static final String         LOG_NO_CONSOLE         = "LOG_NO_CONSOLE";
    public static final String         LOG_NO_FILE            = "LOG_NO_FILE";
    public static final String         LOG_SINGLE_LOGGER_NAME = "LOG_SINGLE_LOGGER_NAME";
    private static boolean             FIRST                  = true;
    public static boolean              AUTO_CLEANUP           = true;

    public LogSourceProvider(final long timeStamp) {
        synchronized (INIT_LOCK) {
            if (FIRST) {
                FIRST = false;
                ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
                    @Override
                    public void onShutdown(final ShutdownRequest shutdownRequest) {
                        LogSourceProvider.flushAllSinks(FLUSH.CLOSE);
                    }

                    @Override
                    public String toString() {
                        return "flushing logs to disk";
                    }
                });
            }
            this.initTime = timeStamp;
            if (!"true".equalsIgnoreCase(System.getProperty(LOG_NO_CONSOLE))) {
                this.consoleHandler = new LogConsoleHandler();
            }
            final LogConfig config = JsonConfig.create(LogConfig.class);
            this.maxSize = config.getMaxLogFileSize();
            this.writeLogs = maxSize > 100 * 1024 && !"true".equalsIgnoreCase(System.getProperty(LOG_NO_FILE));
            this.maxLogs = config.getMaxLogFiles();
            this.logTimeout = config.getLogFlushTimeout() * 1000l;
            debugMode = config.isDebugModeEnabled();
            instantFlushDefault = debugMode;
            logBaseFolder = Application.getResource("logs");
            if (System.getProperty("LOG_BASE_DIRECTORY") != null) {
                logBaseFolder = new File(System.getProperty("LOG_BASE_DIRECTORY"));
            }
            if (INSTANCES.size() > 0) {
                for (LogSourceProvider p : INSTANCES) {
                    System.out.println("Multiple LogControllers Detected: " + p.getClass().getName() + "|" + p);
                }
                System.out.println("Multiple LogControllers Detected: " + getClass().getName() + "|" + this);
                logFolder = INSTANCES.get(0).getLogFolder();
                final int currentLength = String.valueOf(System.currentTimeMillis()).length();
                final String regex = "^(\\d{" + (currentLength - 1) + "," + (currentLength + 1) + "})_.+";
                final String timeStampString = new Regex(logFolder.getName(), regex).getMatch(0);
                initTime = Long.parseLong(timeStampString);
            } else {
                // it is important that folders start with " + timeStamp + "_" !. the rest does not matter.
                File llogFolder = new File(logBaseFolder, timeStamp + "_" + new SimpleDateFormat("EEE, MMM d, yyyy HH.mm Z", Locale.ENGLISH).format(new Date(timeStamp)) + "/");
                int i = 2;
                while (llogFolder.exists()) {
                    llogFolder = new File(logBaseFolder, timeStamp + "_" + new SimpleDateFormat("EEE, MMM d, yyyy HH.mm Z", Locale.ENGLISH).format(new Date(timeStamp)) + "_" + (i++) + "/");
                }
                this.logFolder = llogFolder;
            }
            if (!logFolder.exists() && isWriteLogs()) {
                logFolder.mkdirs();
            }
            if (LogSourceProvider.TRASHLOCK.compareAndSet(false, true)) {
                if (AUTO_CLEANUP) {
                    new Thread("LogsCleanup") {
                        long newestTimeStamp = -1;

                        @Override
                        public void run() {
                            final File oldLogs[] = logBaseFolder.listFiles(new FilenameFilter() {
                                final long   removeTimeStamp = timeStamp - config.getCleanupLogsOlderThanXDays() * 24 * 60 * 60 * 1000l;
                                final int    currentLength   = String.valueOf(System.currentTimeMillis()).length();
                                final String regex           = "^(\\d{" + (currentLength - 1) + "," + (currentLength + 1) + "})_.+";

                                @Override
                                public boolean accept(final File dir, final String name) {
                                    if (dir.exists() && dir.isDirectory() && name.matches(regex)) {
                                        final String timeStamp = new Regex(name, regex).getMatch(0);
                                        long times = 0;
                                        if (timeStamp != null && (times = Long.parseLong(timeStamp)) < this.removeTimeStamp) {
                                            if (newestTimeStamp == -1 || times > newestTimeStamp) {
                                                /*
                                                 * find the latest logfolder, so we can keep it
                                                 */
                                                newestTimeStamp = times;
                                            }
                                            return true;
                                        }
                                    }
                                    return false;
                                }
                            });
                            if (oldLogs != null) {
                                for (final File oldLog : oldLogs) {
                                    try {
                                        if (this.newestTimeStamp > 0 && oldLog.getName().contains(this.newestTimeStamp + "")) {
                                            /* always keep at least the last logfolder! */
                                            continue;
                                        }
                                        Files.deleteRecursiv(oldLog);
                                    } catch (final IOException e) {
                                        e.printStackTrace();
                                    }
                                }
                            }
                        }
                    }.start();
                }
            }
            INSTANCES.add(this);
            System.setProperty(LOG_INIT_DONE, INSTANCES.size() + " instances");
        }
    }

    public File getLogFolder() {
        return logFolder;
    }

    /**
     * @param name
     * @param i
     * @return
     */
    protected LogSource createLogSource(final String name, final int i) {
        return new LogSource(name, i);
    }

    public void flushSinks(final FLUSH flush) {
        java.util.List<LogSink> logSinks2Flush = null;
        java.util.List<LogSink> logSinks2Close = null;
        synchronized (this.logSinks) {
            logSinks2Flush = new ArrayList<LogSink>(this.logSinks.size());
            logSinks2Close = new ArrayList<LogSink>(this.logSinks.size());
            final Iterator<LogSink> it = this.logSinks.values().iterator();
            while (it.hasNext()) {
                final LogSink next = it.next();
                if (next.hasLogSources()) {
                    logSinks2Flush.add(next);
                } else {
                    if (FLUSH.CLOSE.equals(flush)) {
                        it.remove();
                        logSinks2Close.add(next);
                    } else {
                        logSinks2Flush.add(next);
                    }
                }
            }
        }
        for (final LogSink sink : logSinks2Close) {
            try {
                sink.close();
            } catch (final Throwable e) {
            }
        }
        for (final LogSink sink : logSinks2Flush) {
            try {
                sink.flush(flush);
            } catch (final Throwable e) {
            }
        }
    }

    public LogSource getClassLogger(final Class<?> clazz) {
        String name = clazz.getSimpleName();
        if (StringUtils.isEmpty(name)) {
            name = clazz.getName();
        }
        return this.getLogger(name);
    }

    public LogConsoleHandler getConsoleHandler() {
        return this.consoleHandler;
    }

    /**
     * CL = Class Logger, returns a logger for calling Class
     *
     * @return
     */
    public LogSource getCurrentClassLogger() {
        Throwable e = null;
        final Throwable stackTrace = new Throwable();
        try {
            for (final StackTraceElement element : stackTrace.getStackTrace()) {
                final String currentClassName = element.getClassName();
                final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
                final Class<?> currentClass;
                if (classLoader != null) {
                    currentClass = Class.forName(currentClassName, true, classLoader);
                } else {
                    currentClass = Class.forName(currentClassName);
                }
                if (Modifier.isAbstract(currentClass.getModifiers())) {
                    /* we dont want the abstract class to be used */
                    continue;
                }
                if (Modifier.isInterface(currentClass.getModifiers())) {
                    /* we dont want the interface class to be used */
                    continue;
                }
                if (LogSourceProvider.class.isAssignableFrom(currentClass)) {
                    /* we dont want the logging class itself to be used */
                    continue;
                }
                return this.getLogger(currentClassName);
            }
        } catch (final Throwable e2) {
            e = e2;
        }
        final LogSource logger = this.getLogger("LogSourceProvider");
        if (e != null) {
            /* an exception occured during stacktrace walking */
            logger.log(e);
        }
        /*
         * as we could not determine current class, lets put the strackTrace into this generated logger
         */
        logger.log(stackTrace);
        return logger;
    }

    public long getInitTime() {
        return this.initTime;
    }

    public LogSource getLogger(String name) {
        LogSink sink = null;
        name = System.getProperty(LOG_SINGLE_LOGGER_NAME, name);
        name = CrossSystem.alleviatePathParts(name);
        if (StringUtils.isEmpty(name)) {
            return null;
        }
        if (!name.endsWith(".log")) {
            name = name + ".log";
        }
        synchronized (INIT_LOCK) {
            sink = getGlobalLockSink(name.toLowerCase(Locale.ENGLISH));
            if (sink == null) {
                sink = new LogSink(name);
                if (this.consoleHandler != null) {
                    /*
                     * add ConsoleHandler to sink, it will add it to it's sources
                     */
                    sink.addHandler(this.consoleHandler);
                }
                try {
                    if (isWriteLogs()) {
                        final LogSinkFileHandler fileHandler = new LogSinkFileHandler(new File(this.logFolder, name), this.maxSize, this.maxLogs);
                        fileHandler.setFormatter(new LogSourceFormatter());
                        sink.addHandler(fileHandler);
                    }
                } catch (final Throwable e) {
                    e.printStackTrace();
                }
                this.logSinks.put(name.toLowerCase(Locale.ENGLISH), sink);
                this.startFlushThread();
            }
            final LogSource source = this.createLogSource(name, -1);
            source.setInstantFlush(isInstantFlushDefault());
            sink.addLogSource(source);
            return source;
        }
    }

    /**
     * @param name
     * @return
     */
    private LogSink getGlobalLockSink(String name) {
        LogSink ret = this.logSinks.get(name);
        if (ret == null) {
            for (LogSourceProvider p : INSTANCES) {
                if (p == this) {
                    continue;
                }
                ret = p.logSinks.get(name);
                if (ret != null) {
                    System.out.println("Use Log Sink from " + p.getClass() + "." + name + " (instead of " + this.getClass().getName() + ")");
                    break;
                }
            }
        }
        return ret;
    }

    public LogSource getPreviousThreadLogSource() {
        return LogSource.getPreviousThreadLogSource();
    }

    public boolean isInstantFlushDefault() {
        return instantFlushDefault || isWriteLogs() == false;
    }

    public void removeConsoleHandler() {
        synchronized (this.logSinks) {
            if (this.consoleHandler == null) {
                return;
            }
            final Iterator<LogSink> it = this.logSinks.values().iterator();
            while (it.hasNext()) {
                final LogSink next = it.next();
                if (next.hasLogSources()) {
                    next.removeHandler(this.consoleHandler);
                } else {
                    next.close();
                    it.remove();
                }
            }
            this.consoleHandler = null;
        }
    }

    protected void startFlushThread() {
        if (this.flushThread != null && this.flushThread.isAlive()) {
            return;
        }
        this.flushThread = new Thread("LogFlushThread") {
            @Override
            public void run() {
                while (true) {
                    synchronized (LogSourceProvider.this.logSinks) {
                        if (LogSourceProvider.this.logSinks.size() == 0) {
                            LogSourceProvider.this.flushThread = null;
                            return;
                        }
                    }
                    try {
                        try {
                            Thread.sleep(LogSourceProvider.this.logTimeout);
                        } catch (final InterruptedException e) {
                        }
                        LogSourceProvider.this.flushSinks(FLUSH.TIMEOUT);
                    } catch (final Throwable e) {
                    }
                }
            }
        };
        this.flushThread.setDaemon(true);
        this.flushThread.start();
    }

    /**
     * @param b
     * @param c
     */
    public static void flushAllSinks(final FLUSH flush) {
        for (final LogSourceProvider p : getInstances()) {
            if (p != null) {
                p.flushSinks(flush);
            }
        }
    }
}
