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

import java.text.DateFormat;
import java.util.Date;
import java.util.logging.LogRecord;
import java.util.logging.SimpleFormatter;

import org.appwork.utils.Exceptions;
import org.appwork.utils.StringBuilderQueue;

public class LogSourceFormatter extends SimpleFormatter {

    private final Date       dat                    = new Date();
    private final DateFormat longTimestamp          = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.MEDIUM);

    private int              lastThreadID;

    protected StringBuilder  formatterStringBuilder = null;

    @Override
    public synchronized String format(final LogRecord record) {
        StringBuilder sb = getFormatterStringBuilder();
        final boolean sharedStringBuilder;
        if (sb == null) {
            sharedStringBuilder = false;
            /*
             * create new local StringBuilder in case we don't have once set externally
             */
            sb = new StringBuilder();
        } else {
            sharedStringBuilder = true;
        }
        final StringBuilderQueue sbo = new StringBuilderQueue();
        // Minimize memory allocations here.
        this.dat.setTime(record.getMillis());
        final String message = this.formatMessage(record);
        final int th = record.getThreadID();
        if (th != this.lastThreadID) {
            sbo.append("------------------------Thread: ");
            sbo.append(th);
            sbo.append(":" + record.getLoggerName());
            sbo.append("-----------------------\r\n");
        }
        this.lastThreadID = th;
        /* we have this line for easier logfile purifier :) */
        sbo.append("--ID:" + th + "TS:" + record.getMillis() + "-");
        sbo.append(this.longTimestamp.format(this.dat));
        sbo.append(" - ");
        sbo.append(" [");
        String tmp = null;
        if ((tmp = record.getSourceClassName()) != null) {
            sbo.append(tmp);
        }
        if ((tmp = record.getSourceMethodName()) != null) {
            sbo.append('(');
            sbo.append(tmp);
            sbo.append(')');
        }
        sbo.append("] ");
        sbo.append("-> ");
        sbo.append(message);
        sbo.append("\r\n");
        if (record.getThrown() != null) {
            final StringBuilder esb = new StringBuilder();
            Exceptions.getStackTrace(esb, record.getThrown());
            sbo.append(esb);
            sbo.append("\r\n");
        }
        sbo.flushToStringBuilder(sb);
        if (sharedStringBuilder) {
            return "";
        } else {
            return sb.toString();
        }
    }

    public StringBuilder getFormatterStringBuilder() {
        return this.formatterStringBuilder;
    }

    public void setFormatterStringBuilder(final StringBuilder formatterStringBuilder) {
        this.formatterStringBuilder = formatterStringBuilder;
    }
}