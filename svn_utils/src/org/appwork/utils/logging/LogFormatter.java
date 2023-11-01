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
package org.appwork.utils.logging;

import java.text.DateFormat;
import java.util.Date;
import java.util.logging.LogRecord;
import java.util.logging.SimpleFormatter;

import org.appwork.utils.Exceptions;
import org.appwork.utils.StringBuilderQueue;
import org.appwork.utils.os.CrossSystem;

public class LogFormatter extends SimpleFormatter {
    /**
     * Date to convert timestamp to a readable format
     */
    private final Date       date          = new Date();
    /**
     * For thread controlled logs
     */
    private int              lastThreadID;
    /**
     * Dateformat to convert timestamp to a readable format
     */
    private final DateFormat longTimestamp = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.MEDIUM);
    private final String     NEWLINE       = CrossSystem.getNewLine();
    private StringBuilder    sb            = new StringBuilder();

    @Override
    public synchronized String format(final LogRecord record) {
        /* clear StringBuilder buffer */
        sb.setLength(0);
        final StringBuilderQueue sbo = new StringBuilderQueue();
        // Minimize memory allocations here.
        this.date.setTime(record.getMillis());
        final CharSequence message = getMessage(record);
        final int th = record.getThreadID();
        if (th != this.lastThreadID) {
            sbo.append(NEWLINE);
            sbo.append("THREAD: ");
            sbo.append(th);
            sbo.append(NEWLINE);
        }
        this.lastThreadID = th;
        sbo.append(record.getThreadID());
        sbo.append('|');
        sbo.append(record.getLoggerName());
        sbo.append(' ');
        sbo.append(this.longTimestamp.format(this.date));
        sbo.append(" - ");
        sbo.append(record.getLevel().getName());
        sbo.append(" [ ");
        if (record.getSourceClassName() != null) {
            sbo.append(record.getSourceClassName());
        } else {
            sbo.append(record.getLoggerName());
        }
        if (record.getSourceMethodName() != null) {
            sbo.append('(');
            sbo.append(record.getSourceMethodName());
            sbo.append(')');
        }
        sbo.append(" ] -> ");
        sbo.append(message);
        sbo.append(NEWLINE);
        if (record.getThrown() != null) {
            sbo.append(Exceptions.getStackTrace(record.getThrown()));
            sbo.append(NEWLINE);
        }
        sbo.flushToStringBuilder(sb);
        final String ret = sb.toString();
        if (sb.capacity() > 32767) {
            sb = new StringBuilder();
        }
        return ret;
    }

    protected CharSequence getMessage(LogRecord record) {
        return this.formatMessage(record);
    }
}
