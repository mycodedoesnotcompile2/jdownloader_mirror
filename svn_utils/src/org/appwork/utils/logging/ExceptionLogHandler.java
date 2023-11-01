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
import java.util.logging.LogRecord;

import org.appwork.utils.Application;

/**
 * @author thomas
 * 
 */
public class ExceptionLogHandler extends java.util.logging.Handler {

    private File               file;
    private BufferedWriter     writer = null;
    private OutputStreamWriter osw    = null;
    private FileOutputStream   fos    = null;

    public ExceptionLogHandler() {
        super();
        try {
            final Calendar cal = Calendar.getInstance();
            cal.setTimeInMillis(new Date().getTime());
            this.file = Application.getResource("logs/error_" + cal.get(Calendar.YEAR) + "-" + (cal.get(Calendar.MONTH) + 1) + "-" + cal.get(Calendar.DATE) + "-" + System.currentTimeMillis() + ".log");
            this.file.getParentFile().mkdirs();
            this.file.deleteOnExit();
            if (!this.file.isFile()) {
                this.file.createNewFile();
            }
            this.writer = new BufferedWriter(osw = new OutputStreamWriter(fos = new FileOutputStream(this.file, true), "UTF8"));
        } catch (final Exception e) {
            e.printStackTrace();
            close();
        }
    }

    @Override
    public void close() {
        try {
            writer.close();
        } catch (Throwable e) {
        } finally {
            this.writer = null;
        }
        try {
            osw.close();
        } catch (Throwable e) {
        }
        try {
            fos.close();
        } catch (Throwable e) {
        }
    }

    @Override
    public void flush() {
        try {
            if (this.writer == null) return;
            this.writer.flush();
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * @return
     */
    public File getFile() {
        return this.file;
    }

    @Override
    public void publish(final LogRecord logRecord) {
        try {
            if (this.writer == null) return;
            this.writer.write(this.getFormatter().format(logRecord));
        } catch (final IOException e) {
            /*
             * in case write does not work, we close the file and no further
             * logging to file
             */
            e.printStackTrace();
            try {
                close();
            } catch (Throwable e2) {
            }
        }
    }
}
