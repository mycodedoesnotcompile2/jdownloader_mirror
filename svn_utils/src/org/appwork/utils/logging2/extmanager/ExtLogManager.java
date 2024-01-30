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

import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import org.appwork.utils.logging2.LogSource;

public class ExtLogManager extends LogManager {

    public static String[] WHITELIST     = new String[] { "de.btobastian", "org.fourthline", "org.mongo", "com.mongo", "javax.mail", "com.sun.xml.internal.messaging.saaj" };
    public static String[] BLACKLIST     = new String[] { "org.mongodb.driver", "org.bson.ObjectId", "org.mongodb.diagnostics", "com.mongodb.diagnostics", "com.mongodb.driver", "org.fourthline", "org.fourthline.cling.registry.Registry", "org.fourthline.cling.model.message.header", "org.fourthline.cling.model.message.UpnpHeaders", "org.fourthline.cling.transport" };
    private LoggerFactory  loggerFactory = null;

    public LoggerFactory getLoggerFactory() {
        return loggerFactory;
    }

    public void setLoggerFactory(final LoggerFactory LoggerFactory) {
        this.loggerFactory = LoggerFactory;
    }

    public ExtLogManager() throws ClassNotFoundException {
        super();
        final String classLoaderID = getClass().getClassLoader().toString();
        final String boundClassloader = System.getProperty("extlogmanager.avoidsystemclassloader.hack", classLoaderID);
        if (!classLoaderID.equals(boundClassloader)) {
            // INFO: LogManager initializes 'java.util.logging.manager' class via ClassLoader.getSystemClassLoader() first!
            // works because Class.newInstance uses Unsafe.getUnsafe().throwException(e.getTargetException())
            throw new ClassNotFoundException("bound classloader:" + boundClassloader + "|current classloader:" + classLoaderID);
        }
    }

    public boolean addLogger(final Logger logger) {
        if (logger != null) {
            String name = logger.getName();
            if ("sun.util.logging.resources.logging".equals(logger.getResourceBundleName())) {
                if (loggerFactory != null) {
                    for (final String w : WHITELIST) {
                        if (name.startsWith(w)) {
                            System.out.println("Redirect Logger (WL): " + name);
                            return false;
                        }
                    }
                }
            }
            if (!(logger instanceof LogSource)) {
                // adds a handler to system loggers.
                // this handler delegates the output to our logging system
                logger.setLevel(Level.INFO);
                logger.addHandler(new Handler() {
                    {
                        setLevel(Level.INFO);
                    }
                    private LogSource del;

                    @Override
                    public void publish(LogRecord record) {
                        ensureLogger(logger);
                        if (del != null) {
                            del.log(record);
                        } else {
                            System.out.println(record.getMessage());
                        }
                    }

                    /**
                     * @param logger
                     */
                    protected void ensureLogger(final Logger logger) {
                        if (del == null && loggerFactory != null) {
                            String name = logger.getName();
                            if (name == null || name.trim().length() == 0) {
                                name = logger.toString();
                            }
                            del = loggerFactory.getLogger(name);
                        }
                    }

                    @Override
                    public void flush() {                        
                    }

                    @Override
                    public void close() throws SecurityException {
                    }
                });
            }
        }
        boolean ret = super.addLogger(logger);
        return ret;
    }

    @Override
    public synchronized Logger getLogger(final String name) {
        if (loggerFactory != null) {
            for (final String b : BLACKLIST) {
                if (name.startsWith(b)) {
                    LoggerFactory.getDefaultLogger().finer("Ignored (BL): " + name);
                    Logger ret = super.getLogger(name);
                    if (ret != null) {
                        ret.setLevel(Level.OFF);
                    }
                    return ret;
                }
            }
            for (final String w : WHITELIST) {
                if (name.startsWith(w)) {
                    LoggerFactory.getDefaultLogger().finer("Redirect Logger (WL): " + name);
                    return loggerFactory.getLogger(name);
                }
            }
        }
        LoggerFactory.getDefaultLogger().finer("Ignored: " + name);
        Logger ret = super.getLogger(name);
        if (ret != null) {
            ret.setLevel(Level.OFF);
        }
        return ret;
    }
}
