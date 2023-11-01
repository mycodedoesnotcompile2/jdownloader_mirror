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
package org.appwork.utils.logging2;

import java.io.File;
import java.io.IOException;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

/**
 * @author daniel
 * @date 14.03.2016
 *
 */
public class LogSinkFileHandler extends Handler {

    private final File file;

    public File getFile() {
        return file;
    }

    public int getMaxSize() {
        return maxSize;
    }

    public int getMaxLogs() {
        return maxLogs;
    }

    public boolean isClosed() {
        return closed;
    }

    public boolean isOpen() {
        return fileHandler != null && !closed;
    }

    private final int            maxSize;
    private final int            maxLogs;
    private volatile FileHandler fileHandler = null;
    private volatile boolean     closed      = false;

    public LogSinkFileHandler(File file, int maxSize, int maxLogs) {
        this.file = file;
        this.maxSize = maxSize;
        this.maxLogs = maxLogs;
    }

    @Override
    public synchronized void publish(LogRecord record) {
        if (!closed) {
            if (this.fileHandler == null) {
                try {
                    fileHandler = new FileHandler(file.getAbsolutePath(), this.maxSize, this.maxLogs, true) {
                        @Override
                        public Formatter getFormatter() {
                            return LogSinkFileHandler.this.getFormatter();
                        }

                    };
                    fileHandler.setEncoding("UTF-8");
                    fileHandler.setLevel(Level.ALL);
                } catch (IOException e) {
                    e.printStackTrace();
                    close();
                }
            }
            if (this.fileHandler != null) {
                this.fileHandler.publish(record);
            }
        }
    };

    @Override
    public synchronized void close() throws SecurityException {
        final FileHandler fileHandler = this.fileHandler;
        this.fileHandler = null;
        this.closed = true;
        if (fileHandler != null) {
            fileHandler.close();
        }
    }

    @Override
    public synchronized void flush() {
        final FileHandler fileHandler = this.fileHandler;
        if (!closed && fileHandler != null) {
            fileHandler.flush();
        }
    }
}
