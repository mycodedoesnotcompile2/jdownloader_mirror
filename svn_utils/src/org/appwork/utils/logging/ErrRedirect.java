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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Calendar;
import java.util.Date;

import org.appwork.utils.Application;

public class ErrRedirect extends Thread {
    private final File       file;
    private FileOutputStream outStr      = null;
    private PrintStream      printStream = null;

    public ErrRedirect() {
        this.setDaemon(true);
        final Calendar cal = Calendar.getInstance();

        cal.setTimeInMillis(new Date().getTime());

        this.file = Application.getResource("logs/error_cerr_" + cal.get(Calendar.YEAR) + "-" + (cal.get(Calendar.MONTH) + 1) + "-" + cal.get(Calendar.DATE) + "-" + System.currentTimeMillis() + ".log");

        try {
            this.file.getParentFile().mkdirs();
            this.file.deleteOnExit();
            if (!this.file.isFile()) {
                this.file.createNewFile();
            }
            this.outStr = new FileOutputStream(this.file, true);
            this.printStream = new PrintStream(this.outStr);
            System.setErr(this.printStream);
            this.start();
        } catch (final IOException e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
    }

    public void close() throws IOException {
        try {
            this.printStream.close();
        } catch (final Throwable e) {
        }
        try {
            this.outStr.close();
        } catch (final Throwable e) {
        }
        this.printStream = null;
        this.outStr = null;
    }

    public void flush() throws IOException {
        if (this.printStream == null) { return; }
        this.printStream.flush();
        this.outStr.flush();
    }

    /**
     * @return the {@link ErrRedirect#file}
     * @see ErrRedirect#file
     */
    public File getFile() {
        return this.file;
    }

    @Override
    public void run() {
        // flushes the log every 60 seconds and writes it to file
        while (true) {
            if (this.printStream == null) {
                break;
            }
            try {
                Thread.sleep(60000);
            } catch (final InterruptedException e) {
                // e.printStackTrace();
            }
            try {
                if (this.printStream == null) {
                    break;
                }
                this.printStream.flush();
            } catch (final Throwable e) {
                org.appwork.loggingv3.LogV3.log(e);
                break;
            }
        }
        try {
            this.close();
        } catch (final IOException e) {
        }
    }
}
