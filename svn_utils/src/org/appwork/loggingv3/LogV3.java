/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.loggingv3;

import java.io.PrintStream;
import java.lang.Thread.UncaughtExceptionHandler;
import java.lang.reflect.Method;
import java.util.IllegalFormatException;

import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.loggingv3.simple.LogVetoListener;
import org.appwork.loggingv3.simple.SimpleLoggerFactory;
import org.appwork.loggingv3.simple.sink.Sink;
import org.appwork.utils.DebugMode;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.NullOutputStream;

public class LogV3 {
    static {
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler() {
            @Override
            public void uncaughtException(final Thread t, final Throwable e) {
                LogV3.logger(t).exception("Uncaught Exception in: " + t.getId() + "=" + t.getName(), e);
                DebugMode.debugger();
            }
        });
    }

    public static LogV3Factory I() {
        if (INSTANCE == null) {
            DebugMode.debugger();
            // this should not happen. We added a debugger call in setFactory to see if we set INSTANCE to null anywhere.
            // we should be able to remove this soon.
            setFactory(new LogV3FactoryImpl());
        }
        return INSTANCE;
    }

    private volatile static LogV3Factory INSTANCE = new PreInitLoggerFactory().initDefaults();
    static {
        setFactory(createInstance());
    }

    /**
     * @return
     */
    private static LogV3Factory createInstance() {
        try {
            final String clazz = System.getProperty("org.appwork.LoggerFactory");
            if (clazz == null) {
                return INSTANCE;
            }
            final LogV3Factory ret = (LogV3Factory) Class.forName(clazz).newInstance();
            try {
                final Method initDefaults = ret.getClass().getMethod("initDefaults", new Class[] {});
                initDefaults.invoke(ret, new Object[] {});
            } catch (final NoSuchMethodException e) {
                // Thats ok, this method is optional
            }
            return ret;
        } catch (final Throwable e) {
            throw new RuntimeException("Error Initialising Logging facade", e);
        }
    }

    /**
     * @param t
     */
    public static void log(final Throwable t) {
        I().getDefaultLogger().log(t);
    }

    /**
     * @param string
     */
    public static void info(final String string) {
        I().getDefaultLogger().info(string);
    }

    /**
     * @param string
     */
    public static void severe(final String string) {
        I().getDefaultLogger().severe(string);
    }

    /**
     * @param string
     */
    public static void warning(final String string) {
        I().getDefaultLogger().warning(string);
    }

    /**
     * @param string
     */
    public static void finest(final String string) {
        I().getDefaultLogger().finest(string);
    }

    /**
     * @param string
     */
    public static void finer(final String string) {
        I().getDefaultLogger().finer(string);
    }

    /**
     * @param string
     */
    public static void fine(final String string) {
        I().getDefaultLogger().fine(string);
    }

    /**
     * @return
     */
    public static LogInterface defaultLogger() {
        return I().getDefaultLogger();
    }

    /**
     * returns the logger for the given context. if the context is a logger itself, the context is returned
     *
     * @param name
     * @return
     */
    public static LogInterface logger(final Object context) {
        return I().getLogger(context);
    }

    /**
     * @param string
     */
    public static void setFactory(final LogV3Factory instance) {
        if (instance == INSTANCE) {
            return;
        }
        if (instance == null) {
            DebugMode.debugger();
            INSTANCE = null;
        } else {
            LogV3Factory prev = INSTANCE;
            instance.setPredecessor(prev);
            if (prev != null) {
                prev.setSuccessor(instance);
            }
            INSTANCE = instance;
        }
    }

    /**
     *
     */
    public static LogV3Factory getFactory() {
        return INSTANCE;
    }

    /**
     *
     */
    public static void disableSysout() {
        final LogV3Factory logFactory = getFactory();
        if (logFactory != null && logFactory instanceof SimpleLoggerFactory) {
            // disable all logs. LogSinks have a cached Sysout and syserr field, and thus will be not affected by the stream redirection
            // below
            ((SimpleLoggerFactory) logFactory).addVetoListener(new LogVetoListener() {
                @Override
                public boolean blockLogPublishing(final SimpleLoggerFactory simpleLoggerFactory, final Sink sink, final LogRecord2 record) {
                    return true;
                }
            });
        }
        final PrintStream nullStream = new java.io.PrintStream(new NullOutputStream()) {
            @Override
            public void flush() {
            }

            @Override
            public void close() {
            }

            @Override
            public void write(final int b) {
            }

            @Override
            public void write(final byte[] b) {
            }

            @Override
            public void write(final byte[] buf, final int off, final int len) {
            }

            @Override
            public void print(final boolean b) {
            }

            @Override
            public void print(final char c) {
            }

            @Override
            public void print(final int i) {
            }

            @Override
            public void print(final long l) {
            }

            @Override
            public void print(final float f) {
            }

            @Override
            public void print(final double d) {
            }

            @Override
            public void print(final char[] s) {
            }

            @Override
            public void print(final String s) {
            }

            @Override
            public void print(final Object obj) {
            }

            @Override
            public void println() {
            }

            @Override
            public void println(final boolean x) {
            }

            @Override
            public void println(final char x) {
            }

            @Override
            public void println(final int x) {
            }

            @Override
            public void println(final long x) {
            }

            @Override
            public void println(final float x) {
            }

            @Override
            public void println(final double x) {
            }

            @Override
            public void println(final char[] x) {
            }

            @Override
            public void println(final String x) {
            }

            @Override
            public void println(final Object x) {
            }

            @Override
            public java.io.PrintStream printf(final String format, final Object... args) {
                return this;
            }

            @Override
            public java.io.PrintStream printf(final java.util.Locale l, final String format, final Object... args) {
                return this;
            }

            @Override
            public java.io.PrintStream format(final String format, final Object... args) {
                return this;
            }

            @Override
            public java.io.PrintStream format(final java.util.Locale l, final String format, final Object... args) {
                return this;
            }

            @Override
            public java.io.PrintStream append(final CharSequence csq) {
                return this;
            }

            @Override
            public java.io.PrintStream append(final CharSequence csq, final int start, final int end) {
                return this;
            }

            @Override
            public java.io.PrintStream append(final char c) {
                return this;
            }
        };
        System.setOut(nullStream);
        System.setErr(nullStream);
    }

    /**
     * @param abstractMultiRepoUpdateClient
     * @param string
     * @param link
     * @param target
     * @param file
     */
    public static void info(final Object context, final String formatString, final Object... formatParams) {
        final LogInterface logger = logger(context);
        if (logger != null) {
            String logMessage = formatString;
            if (formatParams != null && formatParams.length > 0) {
                try {
                    logMessage = String.format(logMessage, formatParams);
                } catch (final IllegalFormatException e) {
                    e.printStackTrace();
                }
            }
            logger.info(logMessage);
        }
    }

    /**
     * @param logToFileSink
     * @param e
     */
    public static void exception(final Object context, final Throwable e, final String formatString, final Object... formatParams) {
        final LogInterface logger = logger(context);
        if (logger != null) {
            String logMessage = formatString;
            if (formatParams != null && formatParams.length > 0) {
                try {
                    logMessage = String.format(logMessage, formatParams);
                } catch (final IllegalFormatException e1) {
                    e1.printStackTrace();
                }
            }
            logger.exception(logMessage, e);
        }
    }

    public static void exception(final Object context, final Throwable e) {
        exception(context, e, null);
    }
}
