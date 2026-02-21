/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.testframework;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * Runner class that executes serialized {@link JRETestTask} instances in a child JRE process. This class is used internally by
 * {@link TestJREProvider#runInJRE(org.appwork.utils.JavaVersion, JRETestTask)}.
 * <p>
 * The task is read from a file path passed as command line argument (to avoid stdin issues).
 * <p>
 * Exit codes:
 * <ul>
 * <li>0 = Success</li>
 * <li>1 = Task threw an exception</li>
 * <li>2 = Failed to deserialize task</li>
 * <li>3 = Invalid arguments</li>
 * </ul>
 *
 * @author Thomas
 * @date 20.02.2026
 */
public class JRETaskRunner {
    public static final int    EXIT_SUCCESS              = 0;
    public static final int    EXIT_TASK_EXCEPTION       = 1;
    public static final int    EXIT_DESERIALIZATION_FAIL = 2;
    public static final int    EXIT_INVALID_ARGS         = 3;
    public static final String EXCEPTION_FILE_SUFFIX     = ".exception";

    public static void main(final String[] args) {
        if (args.length < 1) {
            System.err.println("Missing task file path argument");
            System.exit(EXIT_INVALID_ARGS);
            return;
        }
        final File taskFile = new File(args[0]);
        if (!taskFile.exists()) {
            System.err.println("Task file does not exist: " + taskFile.getAbsolutePath());
            System.exit(EXIT_INVALID_ARGS);
            return;
        }
        JRETestTask task = null;
        try {
            final FileInputStream fis = new FileInputStream(taskFile);
            try {
                final ObjectInputStream ois = new ObjectInputStream(fis);
                try {
                    task = (JRETestTask) ois.readObject();
                } finally {
                    ois.close();
                }
            } finally {
                fis.close();
            }
        } catch (final Exception e) {
            System.err.println("Failed to deserialize task");
            e.printStackTrace(System.err);
            writeExceptionToFile(taskFile, e);
            System.exit(EXIT_DESERIALIZATION_FAIL);
            return;
        }
        try {
            task.run();
            System.exit(EXIT_SUCCESS);
        } catch (final Throwable e) {
            e.printStackTrace(System.err);
            writeExceptionToFile(taskFile, e);
            System.exit(EXIT_TASK_EXCEPTION);
        }
    }

    /**
     * Writes exception information to a file for retrieval by the parent process.
     */
    private static void writeExceptionToFile(final File taskFile, final Throwable e) {
        final File exceptionFile = new File(taskFile.getAbsolutePath() + EXCEPTION_FILE_SUFFIX);
        try {
            final FileOutputStream fos = new FileOutputStream(exceptionFile);
            try {
                final ObjectOutputStream oos = new ObjectOutputStream(fos);
                try {
                    oos.writeObject(new SerializedException(e));
                } finally {
                    oos.close();
                }
            } finally {
                fos.close();
            }
        } catch (final Exception writeError) {
            System.err.println("JRETaskRunner: Failed to write exception file: " + writeError.getMessage());
        }
    }

    /**
     * Wrapper class to hold exception information in a serializable form.
     */
    public static class SerializedException implements java.io.Serializable {
        private static final long serialVersionUID = 1L;
        private final String      exceptionClass;
        private final String      message;
        private final String      stackTrace;
        private Throwable         originalException;

        public SerializedException(final Throwable e) {
            this.exceptionClass = e.getClass().getName();
            this.message = e.getMessage();
            final StringWriter sw = new StringWriter();
            e.printStackTrace(new PrintWriter(sw));
            this.stackTrace = sw.toString();
            if (e instanceof java.io.Serializable) {
                try {
                    this.originalException = e;
                } catch (final Exception ignore) {
                    this.originalException = null;
                }
            }
        }

        public String getExceptionClass() {
            return this.exceptionClass;
        }

        public String getMessage() {
            return this.message;
        }

        public String getStackTrace() {
            return this.stackTrace;
        }

        /**
         * Returns the original exception if it was serializable, null otherwise.
         */
        public Throwable getOriginalException() {
            return this.originalException;
        }

        @Override
        public String toString() {
            return this.exceptionClass + ": " + this.message + "\n" + this.stackTrace;
        }
    }
}
