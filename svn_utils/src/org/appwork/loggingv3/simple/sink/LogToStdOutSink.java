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

import java.io.PrintStream;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.loggingv3.simple.sink.SimpleFormatter.LocalTimeFormat;
import org.appwork.utils.Application;
import org.appwork.utils.StringUtils;

/**
 * @author Thomas
 * @date 19.09.2018
 *
 */
public class LogToStdOutSink extends AbstractSink {
    protected final PrintStream     errOut;
    protected final PrintStream     stdOut;
    protected LogRecord2            lastErr;
    protected LogRecord2            lastOut;
    protected final LocalTimeFormat dateOnly = new LocalTimeFormat("dd.MM.yy");
    protected final LocalTimeFormat timeOnly = new LocalTimeFormat("HH:mm:ss.SSS");

    public LogToStdOutSink() {
        this(getStdOut(), getStdErr());
        formatter = new SimpleFormatter() {
            {
                offsetForThrownAt = new IntByReference(40);
            }

            protected String createPre(LogRecord2 record, String sourceString) {
                StackTraceElement source = record.getThrownAt();
                sourceString = "";
                if (StringUtils.isNotEmpty(source.getFileName()) && source.getLineNumber() >= 0) {
                    sourceString = " (" + source.getFileName() + ":" + source.getLineNumber() + ")";
                }
                sourceString += "." + source.getMethodName();
                return fillPre(timeOnly.get().format(new Date(record.timestamp)), " ", offsetForTimestamp) + " - " + fillPost("" + abbr(String.valueOf(sourceString) + "", maxSourceStringLength), " ", offsetForThrownAt) + " > ";
            }
        };
    }

    /**
     * @return
     */
    private static PrintStream getStdErr() {
        PrintStream ret = Application.getWrappedStdErr();
        if (ret == null) {
            ret = System.err;
        }
        return ret;
    }

    /**
     * @return
     */
    private static PrintStream getStdOut() {
        PrintStream ret = Application.getWrappedStdOut();
        if (ret == null) {
            ret = System.out;
        }
        return ret;
    }

    private LogToStdOutSink(PrintStream out, PrintStream err) {
        errOut = err;
        stdOut = out;
    }

    @Override
    public synchronized void publish(LogRecord2 record) {
        switch (record.getLevel()) {
        case EXCEPTION:
            if (switchIt(lastErr, record)) {
                errOut.println("Err> Date: " + dateOnly.get().format(record.timestamp) + "  Thread " + record.threadID + "/" + record.threadName + " - " + record.getThrownAt().getClassName());
            }
            errOut.println(format(record));
            lastErr = record;
            break;
        default:
            if (switchIt(lastOut, record)) {
                stdOut.println("Std> Date: " + dateOnly.get().format(record.timestamp) + "  Thread " + record.threadID + "/" + record.threadName + " - " + record.getThrownAt().getClassName());
            }
            stdOut.println(format(record));
            lastOut = record;
        }
    }

    /**
     * @param lastOut2
     * @param record
     * @return
     */
    protected boolean switchIt(LogRecord2 last, LogRecord2 record) {
        if (last == null) {
            return true;
        }
        if (last.threadID != record.threadID) {
            return true;
        }
        SimpleDateFormat format = dateOnly.get();
        if (!format.format(record.timestamp).equals(format.format(last.timestamp))) {
            return true;
        }
        if (!record.getThrownAt().getClassName().equals(last.getThrownAt().getClassName())) {
            return true;
        }
        return false;
    }
}
