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
package org.appwork.loggingv3.bridges;

import java.util.HashMap;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.appwork.loggingv3.LogV3;
import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.loggingv3.simple.LogV3Level;
import org.appwork.loggingv3.simple.LoggerToSink;
import org.appwork.utils.DebugMode;
import org.appwork.utils.logging2.LogInterface;

/**
 *
 * Pipe all logging to the java intern logger to LogV3
 *
 * @author thomas
 * @date 02.08.2024
 *
 */
public class JavaLoggerBridge extends Handler {
    private final HashMap<String, LogV3Level> levelMap = new HashMap<String, LogV3Level>();
    {
        this.levelMap.put("SEVERE", LogV3Level.EXCEPTION);
        this.levelMap.put("INFO", LogV3Level.UNDEFINED);
        this.levelMap.put("WARNING", LogV3Level.UNDEFINED);
        // Ignore all levels set to null
        this.levelMap.put("FINE", null);
        this.levelMap.put("FINER", null);
        this.levelMap.put("FINEST", null);
    }

    @Override
    public void publish(final LogRecord record) {
        if (!this.isLoggable(record)) {
            return;
        }
        String prepend = "Logger:" + record.getLoggerName() + ": ";
        final LogInterface logger = LogV3.I().getLogger("Legacy:" + record.getLoggerName());
        if (logger instanceof LoggerToSink) {
            try {
                LogV3Level level = this.levelMap.get(record.getLevel().getName());
                if (level == null && !this.levelMap.containsKey(record.getLevel().getName())) {
                    level = LogV3Level.UNDEFINED;
                    prepend = "Unknown Level:" + record.getLevel().getName() + "-";
                }
                if (level != null) {
                    final StackTraceElement se = new StackTraceElement(record.getSourceClassName(), record.getSourceMethodName(), (String) null, (int) record.getSequenceNumber());
                    final LogRecord2 newRecord = new LogRecord2(logger, prepend + this.getFormatter().formatMessage(record), se);
                    newRecord.level(level);
                    ((LoggerToSink) logger).publish(newRecord);
                }
            } catch (final Exception e) {
                try {
                    // LogSource.log(exception) does not will source
                    LogV3Level level = this.levelMap.get(record.getLevel().getName());
                    if (level == null && !this.levelMap.containsKey(record.getLevel().getName())) {
                        level = LogV3Level.UNDEFINED;
                        prepend = "Unknown Level:" + record.getLevel().getName() + "-";
                    }
                    if (level != null) {
                        final LogRecord2 newRecord = new LogRecord2(logger, prepend + this.getFormatter().formatMessage(record), ((LoggerToSink) logger).filterThrownAt(new Exception()));
                        newRecord.level(level);
                        ((LoggerToSink) logger).publish(newRecord);
                    }
                } catch (final Exception e1) {
                    DebugMode.debugger();
                    e1.printStackTrace();
                }
            }
        }
    }

    @Override
    public void flush() {
        // Implement if needed
    }

    @Override
    public void close() throws SecurityException {
        // Implement if needed
    }

    /**
     *
     */
    public static void init(Level level) {
        final Logger rootLogger = Logger.getLogger("");
        // Remove all existing handlers
        final Handler[] handlers = rootLogger.getHandlers();
        for (final Handler handler : handlers) {
            rootLogger.removeHandler(handler);
        }
        // Add our custom handler
        final Handler customHandler = new JavaLoggerBridge();
        customHandler.setFormatter(new SimpleFormatter()); // You can set your preferred formatter
        rootLogger.addHandler(customHandler);
        // Set the global log level
        rootLogger.setLevel(level);
    }
}