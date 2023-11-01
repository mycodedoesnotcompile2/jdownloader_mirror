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
package org.appwork.utils.logging2.extmanager;

import java.lang.Thread.UncaughtExceptionHandler;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Enumeration;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.logging2.ConsoleLogImpl;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.logging2.LogSourceProvider;

/**
 * @author Thomas
 *
 */
public class LoggerFactory extends LogSourceProvider {
    private static final LoggerFactory INSTANCE = initialize();

    private static LoggerFactory initialize() {
        try {
            // the LogManager must not be initialized here. so setting the property will tell the LogManager
            // to load/init a ExtLogManager instance on initialization
            // INFO: LogManager uses SystemClassLoader to load/init default 'java.util.logging.manager' LogManager
            // WARNING: do not initialize LogManager or ExtLogManager before these lines!
            System.setProperty("extlogmanager.avoidsystemclassloader.hack", ExtLogManager.class.getClassLoader().toString());
            System.setProperty("java.util.logging.manager", ExtLogManager.class.getName());
            // will try to initialize 'java.util.logging.manager' if not initialized yet
            final LogManager logManager = LogManager.getLogManager();
            // throws a ClassCastException if not instanceof ExtLogManager of same Classloader!
            ((ExtLogManager) logManager).getClass();
            // The init order is important
            final LoggerFactory instance = new LoggerFactory(MODE.NORMAL);
            ((ExtLogManager) logManager).setLoggerFactory(instance);
            return instance;
        } catch (final Throwable e) {
            final LogManager logManager = LogManager.getLogManager();
            // if possible, no sysouts or syserrs here. they would appear in every application startup without any change to block them
            // e.printStackTrace();
            // if (logManager != null) {
            // System.err.println("Logmanager:" + logManager + "|ClassLoader:" + logManager.getClass().getClassLoader());
            // } else {
            // System.err.println("Logmanager: null");
            // }
            try {
                if (logManager != null) {
                    // seems like the logmanager has already been set, and is
                    // not of type ExtLogManager. try to fix this here
                    // we experiences this bug once on a mac system. may be
                    // caused by mac jvm, or the mac install4j launcher
                    // 12.11:
                    // a winxp user had this problem with install4j (exe4j) as
                    // well.
                    // seems like 4xeej sets a logger before our main is
                    // reached.
                    final Field managerField = ReflectionUtils.getField(LogManager.class, "manager", logManager, LogManager.class);

                    final ExtLogManager manager = new ExtLogManager();
                    final LoggerFactory instance = new LoggerFactory(MODE.REPLACED);
                    manager.setLoggerFactory(instance);

                    final Field modifiersField = ReflectionUtils.getField(Field.class, "modifiers", managerField, int.class);
                    modifiersField.setInt(managerField, managerField.getModifiers() & ~Modifier.FINAL);
                    managerField.set(null, manager);

                    final Field rootLoggerField = ReflectionUtils.getField(LogManager.class, "rootLogger", logManager, Logger.class);
                    final Logger rootLoggerInstance = (Logger) rootLoggerField.get(logManager);
                    modifiersField.setInt(rootLoggerField, rootLoggerField.getModifiers() & ~Modifier.FINAL);
                    rootLoggerField.set(manager, rootLoggerInstance);
                    manager.addLogger(rootLoggerInstance);
                    // Adding the global Logger. Doing so in the Logger.<clinit>
                    // would deadlock with the LogManager.<clinit>.
                    ReflectionUtils.invoke(Logger.class, "setLogManager", Logger.global, void.class, manager);
                    final Enumeration<String> names = logManager.getLoggerNames();
                    while (names.hasMoreElements()) {
                        manager.addLogger(logManager.getLogger(names.nextElement()));
                    }
                    return instance;
                }
            } catch (final Throwable e1) {
            }
        }
        return new LoggerFactory(MODE.FALLBACK);
    }

    public static enum MODE {
        NORMAL,
        REPLACED,
        FALLBACK
    }

    public static LoggerFactory I() {
        return INSTANCE;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "Mode:" + getMode();
    }

    private LogInterface defaultLogInterface = null;

    public LogInterface getDefaultLogInterface() {
        return defaultLogInterface;
    }

    public void setDefaultLogInterface(LogInterface defaultLogInterface) {
        this.defaultLogInterface = defaultLogInterface;
    }

    private LogSourceProvider delegate;
    private final MODE        mode;

    public MODE getMode() {
        return mode;
    }

    protected LoggerFactory(MODE mode) {
        super(System.currentTimeMillis());
        Thread.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler() {
            @Override
            public void uncaughtException(final Thread t, final Throwable e) {
                final LogSource logger = getLogger("UncaughtExceptionHandler:" + t.getName());
                logger.severe("Uncaught Exception in: " + t.getId() + "=" + t.getName());
                logger.log(e);
                logger.close();
            }
        });
        this.mode = mode;
    }

    /**
     * @return
     */
    public static LogInterface getDefaultLogger() {
        if (INSTANCE == null) {
            if ("true".equalsIgnoreCase(System.getProperty(LOG_NO_CONSOLE))) {
                return new DevNullLogger();
            } else {
                return new ConsoleLogImpl();
            }
        } else {
            LogInterface defaultLogInterface = null;
            synchronized (INSTANCE) {
                defaultLogInterface = INSTANCE.defaultLogInterface;
                if (defaultLogInterface == null) {
                    INSTANCE.defaultLogInterface = defaultLogInterface = INSTANCE.getLogger("Log.L");
                }
            }
            return defaultLogInterface;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.logging2.LogSourceProvider#getLogger(java.lang.String)
     */
    @Override
    public LogSource getLogger(String name) {
        final LogSourceProvider delegate = this.delegate;
        if (delegate != null) {
            return delegate.getLogger(name);
        } else {
            return super.getLogger(name);
        }
    }

    /**
     * @param logController
     */
    public void setDelegate(LogSourceProvider newLogController) {
        this.delegate = newLogController;
    }

    /**
     * @return
     */
    public static LoggerFactory getInstance() {
        return INSTANCE;
    }

    /**
     * @param name
     * @return
     */
    public static LogInterface get(String name) {
        if (INSTANCE == null) {
            return getDefaultLogger();
        } else {
            return INSTANCE.getLogger(name);
        }
    }

    /**
     * @param logger
     * @param e
     */
    public static void log(LogSource logger, Throwable e) {
        if (logger != null) {
            logger.log(e);
        }
    }
}
