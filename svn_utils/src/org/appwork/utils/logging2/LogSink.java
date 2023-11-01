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

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

public class LogSink extends Logger {
    protected java.util.List<WeakReference<LogSource>> logSources     = new ArrayList<WeakReference<LogSource>>();
    protected LogSinkFileHandler                       fileHandler    = null;
    protected ConsoleHandler                           consoleHandler = null;
    protected Logger                                   parent         = null;

    protected LogSink(final String name) {
        this(name, (String) null);
        this.setLevel(Level.ALL);
    }

    protected LogSink(final String name, final String resourceBundleName) {
        super(name, resourceBundleName);
    }

    @Override
    public void addHandler(final Handler handler) throws SecurityException {
        if (handler == null) {
            return;
        }
        super.addHandler(handler);
        if (this.fileHandler == null && handler instanceof LogSinkFileHandler) {
            this.fileHandler = (LogSinkFileHandler) handler;
        } else if (this.consoleHandler == null && handler instanceof ConsoleHandler) {
            this.consoleHandler = (ConsoleHandler) handler;
            final java.util.List<LogSource> sources = this.getLogSources();
            for (final LogSource source : sources) {
                source.removeHandler(this.consoleHandler);
                source.addHandler(this.consoleHandler);
            }
        }
    }

    public LogSinkFileHandler getFileHandler() {
        return fileHandler;
    }

    protected void addLogSource(final LogSource source) {
        if (source == null) {
            return;
        }
        synchronized (this.logSources) {
            this.logSources.add(new WeakReference<LogSource>(source));
            source.setParent(this);
            if (this.consoleHandler != null) {
                source.removeHandler(this.consoleHandler);
                source.addHandler(this.consoleHandler);
            }
        }
    }

    public static enum FLUSH {
        FINALIZE,
        CLOSE,
        FORCE,
        TIMEOUT
    }

    protected synchronized void close() {
        try {
            flush(FLUSH.CLOSE);
        } finally {
            this.closeAndRemoveFileHandler();
        }
    }

    protected synchronized void closeAndRemoveFileHandler() {
        final LogSinkFileHandler fileHandler = this.fileHandler;
        try {
            if (fileHandler != null) {
                super.removeHandler(fileHandler);
            }
        } finally {
            try {
                if (fileHandler != null) {
                    fileHandler.close();
                }
            } catch (final Throwable e) {
            } finally {
                this.fileHandler = null;
            }
        }
    }

    @Override
    protected void finalize() throws Throwable {
        try {
            try {
                flush(FLUSH.FINALIZE);
            } finally {
                this.closeAndRemoveFileHandler();
            }
        } finally {
            super.finalize();
        }
    }

    protected synchronized void flush(final FLUSH flush) {
        try {
            for (final LogSource source : this.getLogSources()) {
                switch (flush) {
                case FORCE:
                    source.flush();
                    break;
                case TIMEOUT:
                    if (source.isAllowTimeoutFlush()) {
                        source.flush();
                    }
                    break;
                case CLOSE:
                    if (source.isFlushOnClose() || source.isFlushOnFinalize()) {
                        source.flush();
                    }
                    break;
                case FINALIZE:
                    if (source.isFlushOnFinalize()) {
                        source.flush();
                    }
                    break;
                }
            }
        } finally {
            final LogSinkFileHandler fileHandler = this.fileHandler;
            if (fileHandler != null) {
                fileHandler.flush();
            }
        }
    }

    protected java.util.List<LogSource> getLogSources() {
        final java.util.List<LogSource> sources = new ArrayList<LogSource>();
        synchronized (this.logSources) {
            final Iterator<WeakReference<LogSource>> it = this.logSources.iterator();
            while (it.hasNext()) {
                final WeakReference<LogSource> next = it.next();
                final LogSource item = next.get();
                if (item == null || item.isClosed()) {
                    it.remove();
                    continue;
                } else {
                    sources.add(item);
                }
            }
        }
        return sources;
    }

    @Override
    public Logger getParent() {
        return this.parent;
    }

    protected boolean hasLogSources() {
        synchronized (this.logSources) {
            if (logSources.size() > 0) {
                final Iterator<WeakReference<LogSource>> it = this.logSources.iterator();
                while (it.hasNext()) {
                    final WeakReference<LogSource> next = it.next();
                    final LogSource item = next.get();
                    if (item != null && !item.isClosed()) {
                        return true;
                    }
                }
                logSources.clear();
            }
        }
        return false;
    }

    @Override
    public void removeHandler(final Handler handler) throws SecurityException {
        if (handler == null) {
            return;
        }
        super.removeHandler(handler);
        if (this.fileHandler != null && this.fileHandler == handler) {
            this.close();
        } else if (this.consoleHandler != null && handler == this.consoleHandler) {
            final java.util.List<LogSource> sources = this.getLogSources();
            for (final LogSource source : sources) {
                source.removeHandler(this.consoleHandler);
            }
            this.consoleHandler = null;
        }
    }

    @Override
    public void setParent(final Logger parent) {
        this.parent = parent;
    }
}
