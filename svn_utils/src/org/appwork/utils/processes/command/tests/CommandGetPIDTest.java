/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.utils.processes.command.tests;

import java.io.Serializable;

import org.appwork.exceptions.NotSupportedException;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestJREProvider;
import org.appwork.testframework.TestJREProvider.JreOptions;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.command.Command;

/**
 * Test for {@link Command#getPID()} method.
 *
 * @author Thomas
 * @date 20.02.2026
 */
public class CommandGetPIDTest extends AWTest implements Serializable {
    /**
     * @author thomas
     * @date 20.02.2026
     *
     */
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        testGetPIDBeforeStart();
        for (JavaVersion java : new JavaVersion[] { JavaVersion.JVM_1_6, JavaVersion.JVM_1_8, JavaVersion.JVM_9_0, JavaVersion.JVM_11_0 }) {
            TestJREProvider.executeInJRE(JreOptions.version(java), CommandGetPIDTest.class, "testGetPIDAfterStart");
        }
        testGetPIDMatchesExpected();
    }

    /**
     * Tests that getPID() throws IllegalStateException when called before process is started.
     */
    private void testGetPIDBeforeStart() throws Exception {
        logInfoAnyway("Test: getPID() before start should throw IllegalStateException");
        Command cmd;
        if (CrossSystem.isWindows()) {
            cmd = new Command("cmd.exe", "/C", "echo test");
        } else {
            cmd = new Command("echo", "test");
        }
        try {
            cmd.getPID();
            throw new AssertionError("Expected IllegalStateException when calling getPID() before start()");
        } catch (IllegalStateException e) {
            logInfoAnyway("  OK: Got expected IllegalStateException: " + e.getMessage());
        }
    }

    /**
     * Tests that getPID() returns a valid PID after the process has started.
     */
    private void testGetPIDAfterStart() throws Exception {
        logInfoAnyway("Test: getPID() after start should return valid PID > 0");
        Command cmd;
        if (CrossSystem.isWindows()) {
            cmd = new Command("cmd.exe", "/C", "ping -n 2 127.0.0.1");
        } else {
            cmd = new Command("sleep", "2");
        }
        try {
            cmd.start(true);
            try {
                long pid = cmd.getPID();
                logInfoAnyway("  Got PID: " + pid);
                assertTrue(pid > 0, "PID should be > 0");
            } catch (NotSupportedException e) {
                logInfoAnyway("  SKIPPED: getPID() not supported on this platform/JVM: " + e.getMessage());
            }
        } finally {
            cmd.destroy();
        }
    }

    /**
     * Tests getPID() with a longer running process and verifies the PID remains consistent.
     */
    private void testGetPIDMatchesExpected() throws Exception {
        logInfoAnyway("Test: getPID() should return consistent PID for same process");
        logInfoAnyway("  Java Version: " + JavaVersion.getVersion());
        logInfoAnyway("  OS Family: " + CrossSystem.getOSFamily());
        Command cmd;
        if (CrossSystem.isWindows()) {
            cmd = new Command("cmd.exe", "/C", "ping -n 3 127.0.0.1");
        } else {
            cmd = new Command("sleep", "3");
        }
        try {
            cmd.start(true);
            try {
                long pid1 = cmd.getPID();
                logInfoAnyway("  First getPID() call: " + pid1);
                assertTrue(pid1 > 0, "PID should be > 0");
                Thread.sleep(100);
                long pid2 = cmd.getPID();
                logInfoAnyway("  Second getPID() call: " + pid2);
                assertThat(pid1).is(pid2);
                logInfoAnyway("  OK: PID is consistent across multiple calls");
            } catch (NotSupportedException e) {
                logInfoAnyway("  SKIPPED: getPID() not supported on this platform/JVM: " + e.getMessage());
            }
        } finally {
            cmd.destroy();
        }
    }
}
