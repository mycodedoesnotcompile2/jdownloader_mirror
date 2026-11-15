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
package org.appwork.testframework.tests;

import java.io.File;

import org.appwork.testframework.AWTest;
import org.appwork.testframework.JREExecuter;
import org.appwork.testframework.JREExecuter.JRETaskExecutionException;
import org.appwork.testframework.JREExecuter.JreOptions;
import org.appwork.testframework.JRETestTask;
import org.appwork.utils.IO;
import org.appwork.utils.JavaVersion;

/**
 * Tests for {@link JREExecuter#runInJRE(JreOptions, JRETestTask)}. Verifies that a serialized {@link JRETestTask} is executed in a child
 * JRE and that success/exception handling works.
 */
/**
 * Serializable task that throws a RuntimeException with the given message. Used by {@link TestRunInJRETask#testRunInJRETaskThrows}.
 */
final class ThrowingJRETaskTarget implements JRETestTask {
    private static final long serialVersionUID = 1L;
    private final String      message;

    ThrowingJRETaskTarget(final String message) {
        this.message = message;
    }

    @Override
    public void run() throws Exception {
        throw new RuntimeException(message);
    }
}

public class TestRunInJRETask extends AWTest {
    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        testRunInJRESuccess();
        testRunInJRETaskThrows();
    }

    /**
     * Returns a JRE version suitable for runInJRE tests: current runtime version, or at least 11 so that JRETaskRunner (compiled with
     * current project compliance) can load in the child JRE.
     */
    private static JavaVersion getRunInJRETestVersion() {
        final JavaVersion current = JavaVersion.getVersion().getBase();
        if (current != null && current != JavaVersion.UNKNOWN && !current.isLowerThan(JavaVersion.JVM_1_8)) {
            return current;
        }
        return JavaVersion.JVM_11_0;
    }

    /**
     * Runs a task that writes "OK" + java.version to a temp file; verifies the file content after runInJRE returns.
     */
    private void testRunInJRESuccess() throws Exception {
        final File outFile = File.createTempFile("runInJRE_", ".txt");
        try {
            final String path = outFile.getAbsolutePath();
            final JRETestTask task = new RunInJRETaskTarget(path);
            final JavaVersion version = getRunInJRETestVersion();
            JREExecuter.runInJRE(JreOptions.version(version), task);
            assertTrue(outFile.exists(), "Task should have written output file");
            final String content = IO.readFileToString(outFile).trim();
            assertTrue(content.startsWith("OK "), "Output should start with OK and java.version, got: " + content);
        } finally {
            outFile.delete();
        }
    }

    /**
     * Runs a task that throws an exception; verifies that the exception is propagated to the caller.
     */
    private void testRunInJRETaskThrows() throws Exception {
        final String expectedMessage = "expected failure from JRETestTask";
        final JRETestTask task = new ThrowingJRETaskTarget(expectedMessage);
        final JavaVersion version = getRunInJRETestVersion();
        try {
            JREExecuter.runInJRE(JreOptions.version(version), task);
            assertTrue(false, "runInJRE should throw when task throws");
        } catch (final JRETaskExecutionException e) {
            final Throwable cause = e.getCause();
            assertTrue(cause != null || e.getMessage().contains(expectedMessage), "Exception should contain task's message: " + e.getMessage());
            if (cause != null) {
                assertTrue(cause.getMessage() != null && cause.getMessage().contains(expectedMessage), "Cause message should contain: " + expectedMessage);
            }
        } catch (final RuntimeException e) {
            assertTrue(e.getMessage() != null && e.getMessage().contains(expectedMessage), "RuntimeException should be task's exception: " + e.getMessage());
        }
    }
}
