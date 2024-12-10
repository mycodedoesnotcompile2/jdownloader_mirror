/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2024, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.loggingv3.simple;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

import org.appwork.loggingv3.LogV3;
import org.appwork.loggingv3.LogV3Factory;
import org.appwork.loggingv3.LoggerProvider;
import org.appwork.loggingv3.simple.sink.LogToFileSink;
import org.appwork.loggingv3.simple.sink.LogToStdOutSink;
import org.appwork.loggingv3.simple.sink.Sink;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.reflection.Clazz;

/**
 * @author Thomas
 * @date 19.09.2018
 *
 */
public class SimpleLoggerFactory implements LogV3Factory, SinkProvider {
    protected final CopyOnWriteArrayList<Sink> sinks = new CopyOnWriteArrayList<Sink>();

    public List<Sink> getSinks() {
        return Collections.unmodifiableList(this.sinks);
    }

    protected final HashMap<String, LoggerToSink> logger = new HashMap<String, LoggerToSink>();
    protected LogToFileSink                       sinkToFile;

    public LogToFileSink getSinkToFile() {
        return this.sinkToFile;
    }

    public void setSinkToFile(final LogToFileSink sinkToFile) {
        this.removeSink(this.sinkToFile);
        this.sinkToFile = sinkToFile;
        this.addSink(sinkToFile);
    }

    public LogToStdOutSink getSinkToConsole() {
        return this.sinkToConsole;
    }

    public void setSinkToConsole(final LogToStdOutSink sinkToConsole) {
        this.removeSink(this.sinkToConsole);
        this.sinkToConsole = sinkToConsole;
        this.addSink(sinkToConsole);
    }

    protected LogToStdOutSink sinkToConsole;

    public SimpleLoggerFactory() {
    }

    public SimpleLoggerFactory initDefaults() {
        this.sinkToConsole = new LogToStdOutSink();
        // do not add a file sink by default. Else every application that uses the lib, will create log files. this must be enabled in the
        // application itself, usually by overwriting the facrory and put its path in the properties
        // addSink(sinkToFile);
        this.addSink(this.sinkToConsole);
        return this;
    }

    @Override
    public LogInterface getLogger(final Object context) {
        if (context == null) {
            return this.getDefaultLogger();
        } else {
            if (context instanceof LogInterface) {
                return (LogInterface) context;
            } else if (context instanceof LoggerProvider) {
                return ((LoggerProvider) context).getLogger();
            } else {
                synchronized (this.logger) {
                    final String id = context.toString();
                    LoggerToSink ret = this.logger.get(id);
                    if (ret == null) {
                        this.logger.put(id, ret = this.createLogger(context));
                        ret.info("Created Logger " + this.logger.size() + ": " + context);
                    }
                    return ret;
                }
            }
        }
    }

    protected LoggerToSink createLogger(final Object name) {
        return new LoggerToSink(this);
    }

    @Override
    public LogInterface getDefaultLogger() {
        return this.getLogger(LogV3.class.getSimpleName());
    }

    /**
     * @param logToFileSink
     */
    public boolean addSink(final Sink sink) {
        return sink != null && this.sinks.addIfAbsent(sink);
    }

    /**
     * @param logToFileSink
     */
    public boolean removeSink(final Sink sink) {
        return sink != null && this.sinks.remove(sink);
    }

    private volatile HashSet<LogVetoListener> vetoListener;

    public boolean addVetoListener(final LogVetoListener logVetoListener) {
        synchronized (this) {
            if (logVetoListener != null) {
                final HashSet<LogVetoListener> newList = new HashSet<LogVetoListener>();
                if (this.vetoListener != null) {
                    newList.addAll(this.vetoListener);
                }
                final boolean ret = newList.add(logVetoListener);
                this.vetoListener = newList;
                return ret;
            } else {
                return false;
            }
        }
    }

    public boolean removeVetoListener(final LogVetoListener logVetoListener) {
        synchronized (this) {
            if (this.vetoListener != null && logVetoListener != null) {
                final HashSet<LogVetoListener> newList = new HashSet<LogVetoListener>();
                newList.addAll(this.vetoListener);
                final boolean ret = newList.remove(logVetoListener);
                if (newList.size() == 0) {
                    this.vetoListener = null;
                } else {
                    this.vetoListener = newList;
                }
                return ret;
            } else {
                return false;
            }
        }
    }

    @Override
    public void publish(final LogRecord2 record) {
        final Set<LogVetoListener> vetoListener = this.vetoListener;
        sinks: for (final Sink sink : this.sinks) {
            if (vetoListener != null) {
                for (final LogVetoListener veto : vetoListener) {
                    if (veto.blockLogPublishing(this, sink, record)) {
                        continue sinks;
                    }
                }
            }
            sink.publish(record);
        }
    }

    @Override
    public void setPredecessor(final LogV3Factory previousFactory) {
    }

    /**
     * @param sinkToConsole2
     * @return
     */
    public boolean hasSink(final Sink sink) {
        return sink != null && this.sinks.contains(sink);
    }

    /**
     * @param <T>
     * @param class1
     * @return
     */
    public <T extends Sink> T getSinkByClass(final Class<T> class1) {
        for (final Sink sink : this.sinks) {
            if (Clazz.isInstanceof(sink.getClass(), class1)) {
                return (T) sink;
            }
        }
        return null;
    }

    /**
     *
     */
    public void set() {
        LogV3.setFactory(this);
    }

    /**
     * @see org.appwork.loggingv3.LogV3Factory#setSuccessor(org.appwork.loggingv3.LogV3Factory)
     */
    @Override
    public void setSuccessor(LogV3Factory newFactory) {
    }
}
