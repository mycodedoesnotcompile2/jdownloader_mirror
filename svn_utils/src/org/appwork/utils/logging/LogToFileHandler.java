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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Calendar;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import org.appwork.utils.Application;

/**
 * @author thomas
 * 
 */
public class LogToFileHandler extends java.util.logging.Handler {

    private volatile File      file;
    private BufferedWriter     writer;
    private OutputStreamWriter osw = null;
    private FileOutputStream   fos = null;

    public LogToFileHandler() throws IOException {
        super();
        final Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis(new Date().getTime());
        this.file = Application.getResource("logs/" + cal.get(Calendar.YEAR) + "-" + (1 + cal.get(Calendar.MONTH)) + "-" + cal.get(Calendar.DATE) + ".log");
        this.file.getParentFile().mkdirs();
        this.file.deleteOnExit();
        if (!this.file.isFile()) {
            this.file.createNewFile();
        }
        try {
            this.writer = new BufferedWriter(this.osw = new OutputStreamWriter(this.fos = new FileOutputStream(this.file, true), "UTF8"));
        } catch (final IOException e) {
            e.printStackTrace();
            this.close();
            throw e;
        }
    }

    @Override
    public void close() {
        try {
            if (this.writer != null) {
                this.writer.close();
            }
        } catch (final Throwable e) {
        } finally {
            this.writer = null;
        }
        try {
            if (this.osw != null) {
                this.osw.close();
            }
        } catch (final Throwable e) {
        } finally {
            this.osw = null;
        }
        try {
            if (this.fos != null) {
                this.fos.close();
            }
        } catch (final Throwable e) {
        } finally {
            this.fos = null;
        }
        final File lfile = this.file;
        this.file = null;
        if (lfile != null && lfile.exists() && lfile.length() == 0) {
            lfile.delete();
        }
    }

    @Override
    public void flush() {
        try {
            final BufferedWriter lwriter = this.writer;
            if (lwriter != null) {
                lwriter.flush();
            }
        } catch (final IOException e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
    }

    @Override
    public void publish(final LogRecord logRecord) {
        if (logRecord.getLevel() == Level.INFO) {
            try {
                final BufferedWriter lwriter = this.writer;
                if (lwriter != null) {
                    lwriter.write(this.getFormatter().format(logRecord));
                }
            } catch (final IOException e) {
                if (e.getMessage().contains("not enough")) {
                    org.appwork.loggingv3.LogV3.severe("Cannot write log, Disk is full!");
                } else {
                    org.appwork.loggingv3.LogV3.log(e);
                }
            }

        }

    }
}
