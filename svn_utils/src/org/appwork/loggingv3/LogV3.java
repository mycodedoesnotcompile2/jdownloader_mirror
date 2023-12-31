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
package org.appwork.loggingv3;

import java.io.PrintStream;
import java.lang.Thread.UncaughtExceptionHandler;
import java.lang.reflect.Method;

import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.loggingv3.simple.LogVetoListener;
import org.appwork.loggingv3.simple.SimpleLoggerFactory;
import org.appwork.loggingv3.simple.sink.Sink;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.NullOutputStream;

/**
 * @author Thomas
 * @date 18.09.2018
 *
 */
public class LogV3 {
    static {
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler() {
            @Override
            public void uncaughtException(final Thread t, final Throwable e) {
                LogV3.logger(t).exception("Uncaught Exception in: " + t.getId() + "=" + t.getName(), e);
            }
        });
    }

    public static LogV3Factory I() {
        if (INSTANCE == null) {
            // this can only happen, if there is a LogV3 call in the LoggerFactory init itself
            setFactory(new LogV3FactoryImpl());
        }
        return INSTANCE;
    }

    private static LogV3Factory INSTANCE = createInstance();

    /**
     * @return
     */
    private static LogV3Factory createInstance() {
        String load = LogV3FactoryImpl.class.getName();
        try {
            Class.forName(load = LogV3.class.getPackage().getName() + ".simple.SimpleLoggerFactory");
        } catch (Throwable e) {
            load = LogV3FactoryImpl.class.getName();
        }
        try {
            String clazz = System.getProperty("org.appwork.LoggerFactory", load);
            LogV3Factory ret = (LogV3Factory) Class.forName(clazz).newInstance();
            try {
                Method initDefaults = ret.getClass().getMethod("initDefaults", new Class[] {});
                initDefaults.invoke(ret, new Object[] {});
            } catch (NoSuchMethodException e) {
                // Thats ok, this method is optional
            }
            return ret;
        } catch (Throwable e) {
            throw new RuntimeException("Error Initialising Logging facade", e);
        }
    }

    /**
     * @param t
     */
    public static void log(Throwable t) {
        I().getDefaultLogger().log(t);
    }

    /**
     * @param string
     */
    public static void info(String string) {
        I().getDefaultLogger().info(string);
    }

    /**
     * @param string
     */
    public static void severe(String string) {
        I().getDefaultLogger().severe(string);
    }

    /**
     * @param string
     */
    public static void warning(String string) {
        I().getDefaultLogger().warning(string);
    }

    /**
     * @param string
     */
    public static void finest(String string) {
        I().getDefaultLogger().finest(string);
    }

    /**
     * @param string
     */
    public static void finer(String string) {
        I().getDefaultLogger().finer(string);
    }

    /**
     * @param string
     */
    public static void fine(String string) {
        I().getDefaultLogger().fine(string);
    }

    /**
     * @return
     */
    public static LogInterface defaultLogger() {
        return I().getDefaultLogger();
    }

    /**
     * @param name
     * @return
     */
    public static LogInterface logger(Object name) {
        return I().getLogger(name);
    }

    /**
     * @param string
     */
    public static void setFactory(LogV3Factory instance) {
        if (instance == null) {
            INSTANCE = null;
        } else {
            INSTANCE = instance.setFactory(INSTANCE);
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
        LogV3Factory logFactory = getFactory();
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
        PrintStream nullStream = new java.io.PrintStream(new NullOutputStream()) {
            @Override
            public void flush() {
            }

            @Override
            public void close() {
            }

            @Override
            public void write(int b) {
            }

            @Override
            public void write(byte[] b) {
            }

            @Override
            public void write(byte[] buf, int off, int len) {
            }

            @Override
            public void print(boolean b) {
            }

            @Override
            public void print(char c) {
            }

            @Override
            public void print(int i) {
            }

            @Override
            public void print(long l) {
            }

            @Override
            public void print(float f) {
            }

            @Override
            public void print(double d) {
            }

            @Override
            public void print(char[] s) {
            }

            @Override
            public void print(String s) {
            }

            @Override
            public void print(Object obj) {
            }

            @Override
            public void println() {
            }

            @Override
            public void println(boolean x) {
            }

            @Override
            public void println(char x) {
            }

            @Override
            public void println(int x) {
            }

            @Override
            public void println(long x) {
            }

            @Override
            public void println(float x) {
            }

            @Override
            public void println(double x) {
            }

            @Override
            public void println(char[] x) {
            }

            @Override
            public void println(String x) {
            }

            @Override
            public void println(Object x) {
            }

            @Override
            public java.io.PrintStream printf(String format, Object... args) {
                return this;
            }

            @Override
            public java.io.PrintStream printf(java.util.Locale l, String format, Object... args) {
                return this;
            }

            @Override
            public java.io.PrintStream format(String format, Object... args) {
                return this;
            }

            @Override
            public java.io.PrintStream format(java.util.Locale l, String format, Object... args) {
                return this;
            }

            @Override
            public java.io.PrintStream append(CharSequence csq) {
                return this;
            }

            @Override
            public java.io.PrintStream append(CharSequence csq, int start, int end) {
                return this;
            }

            @Override
            public java.io.PrintStream append(char c) {
                return this;
            }
        };
        System.setOut(nullStream);
        System.setErr(nullStream);
    }
}
