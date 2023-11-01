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

import java.io.InputStream;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.WeakHashMap;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import org.appwork.utils.Exceptions;
import org.appwork.utils.logging.ExceptionDefaultLogLevel;

public class LogSource extends Logger implements LogInterface, ClearableLogInterface, ClosableLogInterface {
    private static WeakHashMap<Thread, WeakReference<LogSource>> LASTTHREADLOGSOURCE = new WeakHashMap<Thread, WeakReference<LogSource>>();

    public static void exception(final Logger logger, final Throwable e) {
        if (logger == null || e == null) {
            return;
        }
        if (logger instanceof LogSource) {
            ((LogSource) logger).log(e);
        } else {
            logger.severe(Exceptions.getStackTrace(e));
        }
    }

    /**
     * @param logger
     * @param e
     */
    public static void exception(LogInterface logger, Throwable e) {
        if (logger == null || e == null) {
            return;
        }
        logger.log(e);
    }

    public static LogSource getPreviousThreadLogSource() {
        synchronized (LogSource.LASTTHREADLOGSOURCE) {
            final Thread thread = Thread.currentThread();
            final WeakReference<LogSource> prevLogSource = LogSource.LASTTHREADLOGSOURCE.get(thread);
            if (prevLogSource != null) {
                final LogSource previousLogger = prevLogSource.get();
                if (previousLogger != null && previousLogger.isClosed() == false) {
                    return previousLogger;
                }
                LogSource.LASTTHREADLOGSOURCE.remove(thread);
            }
        }
        return null;
    }

    private java.util.List<LogRecord> records               = new ArrayList<LogRecord>();
    private int                       maxLogRecordsInMemory = -1;
    private int                       maxSizeInMemory       = -1;
    private int                       currentSizeInMemory   = 0;
    private int                       flushCounter          = 0;
    private int                       recordsCounter        = 0;
    private boolean                   closed                = false;
    private boolean                   allowTimeoutFlush     = true;
    private boolean                   autoFlushOnThrowable  = false;

    public boolean isAutoFlushOnThrowable() {
        return this.autoFlushOnThrowable;
    }

    public void setAutoFlushOnThrowable(boolean autoFlushOnThrowable) {
        this.autoFlushOnThrowable = autoFlushOnThrowable;
    }

    private boolean instantFlush    = false;
    private boolean flushOnFinalize = false;
    private boolean flushOnClose    = true;

    public boolean isFlushOnClose() {
        return flushOnClose;
    }

    public void setFlushOnClose(boolean flushOnClose) {
        this.flushOnClose = flushOnClose;
    }

    private Logger parent = null;

    public LogSource(final String name) {
        this(name, -1);
    }

    /*
     * creates a LogCollector with given name
     * 
     * maxLogRecordsInMemory defines how many log records this logger will buffer in memory before logging to parent's handlers
     * 
     * <0 = unlimited in memory, manual flush needed
     * 
     * 0 = forward directly to parent's handlers
     * 
     * >0 = limited
     */
    public LogSource(final String name, final int maxLogRecordsInMemory) {
        this(name, (String) null);
        this.maxLogRecordsInMemory = maxLogRecordsInMemory;
        super.setUseParentHandlers(false);
        this.setLevel(Level.ALL);
    }

    protected LogSource(final String name, final String resourceBundleName) {
        super(name, resourceBundleName);
        this.setCurrentThreadLogSource();
    }

    public synchronized void clear() {
        this.records = null;
    }

    public synchronized void close() {
        this.flush();
        this.closed = true;
        this.records = null;
    }

    @Override
    protected void finalize() throws Throwable {
        try {
            if (this.allowTimeoutFlush || this.flushOnFinalize) {
                this.close();
            }
        } finally {
            super.finalize();
        }
    }

    public synchronized void flush() {
        if (this.closed) {
            return;
        }
        if (this.records == null || this.records.size() == 0) {
            this.currentSizeInMemory = 0;
            this.records = null;
            return;
        }
        try {
            final Logger parent = this.getParent();
            if (parent != null) {
                this.flushCounter++;
                for (final Handler handler : parent.getHandlers()) {
                    if (handler != null) {
                        if (handler instanceof ConsoleHandler) {
                            /*
                             * we dont want logRecords to appear twice on console
                             */
                            continue;
                        }
                        synchronized (handler) {
                            for (final LogRecord record : this.records) {
                                handler.publish(record);
                            }
                        }
                    }
                }
            }
        } finally {
            this.currentSizeInMemory = 0;
            this.records = null;
        }
    }

    public int getMaxLogRecordsInMemory() {
        return this.maxLogRecordsInMemory;
    }

    /**
     * @return the maxSizeInMemory
     */
    public int getMaxSizeInMemory() {
        return this.maxSizeInMemory;
    }

    @Override
    public Logger getParent() {
        return this.parent;
    }

    public boolean isAllowTimeoutFlush() {
        return this.allowTimeoutFlush;
    }

    protected boolean isClosed() {
        return this.closed;
    }

    /**
     * @return the flushOnFinalize
     */
    public boolean isFlushOnFinalize() {
        return this.flushOnFinalize;
    }

    /**
     * @return the instantFlush
     */
    public boolean isInstantFlush() {
        return this.instantFlush;
    }

    @Override
    public synchronized void log(final LogRecord record) {
        if (this.closed || record == null) {
            return;
        }
        this.recordsCounter++;
        this.setCurrentThreadLogSource();
        record.setLoggerName(this.getName());
        /* make sure we have gathered all information about current class/method */
        /* this will collect current class/method if net set yet */
        record.getSourceClassName();
        /* Thread.getName creates fresh copy of name each time -> memleak! */
        // record.setLoggerName(Thread.currentThread().getName());
        int recordSize = 0;
        if (record.getMessage() != null) {
            recordSize = record.getMessage().length();
        }
        if (this.maxLogRecordsInMemory == 0 || this.maxSizeInMemory == 0 || this.instantFlush) {
            /* maxLogRecordsInMemory == 0, we want to use parent's handlers */
            final Logger parent = this.getParent();
            if (parent != null) {
                for (final Handler handler : parent.getHandlers()) {
                    if (handler != null) {
                        synchronized (handler) {
                            handler.publish(record);
                        }
                    }
                }
            }
            return;
        } else if (this.maxLogRecordsInMemory > 0 && this.records != null && this.records.size() == this.maxLogRecordsInMemory || this.maxSizeInMemory > 0 && this.currentSizeInMemory + recordSize > this.maxSizeInMemory) {
            /* maxLogRecordsInMemory >0 we have limited max records in memory */
            /* we flush in case we reached maxLogRecordsInMemory */
            this.flush();
        }
        if (this.records == null) {
            /* records will be null at first use or after a flush */
            this.records = new ArrayList<LogRecord>();
        }
        this.currentSizeInMemory += recordSize;
        this.records.add(record);
        super.log(record);
    }

    public void log(Throwable e) {
        String thrownAt = null;
        for (StackTraceElement es : new Exception().getStackTrace()) {
            if (LogSource.class.getName().equals(es.getClassName())) {
                continue;
            }
            thrownAt = es.toString();
            break;
        }
        if (e == null) {
            e = new NullPointerException("e is null");
        }
        Level lvl = null;
        if (e instanceof ExceptionDefaultLogLevel) {
            lvl = ((ExceptionDefaultLogLevel) e).getDefaultLogLevel();
        }
        if (lvl == null) {
            lvl = Level.SEVERE;
        }
        final LogRecord lr = new LogRecord(lvl, "Exception thrown at " + thrownAt + ":\r\n" + Exceptions.getStackTrace(e));
        lr.setLoggerName(this.getName());
        // lr.setThrown(e);
        this.log(lr);
        if (this.isAutoFlushOnThrowable()) {
            this.flush();
        }
    }

    /**
     * @param errorStream
     */
    public void logAsynch(final InputStream is) {
        new InputStreamLogger(is, this).start();
    }

    public void setAllowTimeoutFlush(final boolean allowTimeoutFlush) {
        this.allowTimeoutFlush = allowTimeoutFlush;
    }

    private void setCurrentThreadLogSource() {
        synchronized (LogSource.LASTTHREADLOGSOURCE) {
            final Thread thread = Thread.currentThread();
            final WeakReference<LogSource> prevLogSource = LogSource.LASTTHREADLOGSOURCE.get(thread);
            if (prevLogSource == null || prevLogSource.get() != this) {
                LogSource.LASTTHREADLOGSOURCE.put(Thread.currentThread(), new WeakReference<LogSource>(this));
            }
        }
    }

    /**
     * @param flushOnFinalize
     *            the flushOnFinalize to set
     */
    public void setFlushOnFinalize(final boolean flushOnFinalize) {
        this.flushOnFinalize = flushOnFinalize;
    }

    /**
     * @param instantFlush
     *            the instantFlush to set
     */
    public void setInstantFlush(final boolean instantFlush) {
        if (this.instantFlush == instantFlush) {
            return;
        }
        this.instantFlush = instantFlush;
        if (instantFlush) {
            this.flush();
        }
    }

    public synchronized void setMaxLogRecordsInMemory(int newMax) {
        newMax = Math.max(0, newMax);
        if (this.maxLogRecordsInMemory == newMax) {
            return;
        }
        this.maxLogRecordsInMemory = newMax;
        if (newMax == 0 || newMax <= this.records.size()) {
            this.flush();
        }
    }

    /**
     * @param maxSizeInMemory
     *            the maxSizeInMemory to set
     */
    public synchronized void setMaxSizeInMemory(int maxSizeInMemory) {
        maxSizeInMemory = Math.max(0, maxSizeInMemory);
        if (this.maxSizeInMemory == maxSizeInMemory) {
            return;
        }
        this.maxSizeInMemory = maxSizeInMemory;
        if (maxSizeInMemory == 0 || maxSizeInMemory <= this.currentSizeInMemory) {
            this.flush();
        }
    }

    public LogSink getLogSink() {
        final Logger parent = this.parent;
        if (parent instanceof LogSink) {
            return (LogSink) parent;
        } else {
            return null;
        }
    }

    @Override
    public void setParent(final Logger parent) {
        this.parent = parent;
    }

    @Override
    public void setUseParentHandlers(final boolean useParentHandlers) {
        /* do not allow to change this */
    }

    @Override
    public String toString() {
        return this.toString(0);
    }

    public String toString(final int lastXEntries) {
        final StringBuilder sb = new StringBuilder();
        synchronized (this) {
            sb.append("Log:" + this.getName() + "|Records:" + this.recordsCounter + "|Cached:" + (records != null ? (currentSizeInMemory + "/" + records.size()) : "0") + "|Flushed:" + this.flushCounter);
            if (this.records != null && this.records.size() > 0) {
                sb.append("\r\n");
                final LogSourceFormatter formatter = new LogSourceFormatter();
                formatter.setFormatterStringBuilder(sb);
                int index = 0;
                if (lastXEntries > 0 && this.records.size() > lastXEntries) {
                    index = this.records.size() - lastXEntries;
                }
                for (; index < this.records.size(); index++) {
                    sb.append(formatter.format(this.records.get(index)));
                }
            }
        }
        return sb.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.logging2.LogInterface#exception(java.lang.String, java.lang.Throwable)
     */
    @Override
    public void exception(String msg, Throwable e) {
        severe(msg);
        log(e);
    }
}
