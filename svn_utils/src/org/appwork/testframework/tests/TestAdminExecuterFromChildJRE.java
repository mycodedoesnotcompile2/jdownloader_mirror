/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.tests;

import org.appwork.testframework.AWTest;
import org.appwork.testframework.JREExecuter;
import org.appwork.testframework.JREExecuter.JreOptions;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.ProcessOutput;

/**
 * Verifies that a process started via JREExecuter (another JRE) can use AdminExecuter to connect to the same helper that was started by
 * this test. Flow: (1) This test runs a task as admin, which starts the helper. (2) This test starts a child JRE via
 * JREExecuter.executeInJRE (JREExecuter adds AdminExecuter helper env automatically). (3) The child runs AdminExecuter.runAsAdmin. The
 * helper must accept the connection because the child is a descendant of this process, and this process is the helper's direct parent (or
 * has it as ancestor). Windows-only.
 */
@TestDependency({ "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class TestAdminExecuterFromChildJRE extends AWTest {
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("TestAdminExecuterFromChildJRE: Windows only, skipped.");
            return;
        }
        testAdminExecuterFromChildJRE();
    }

    /**
     * Starts the helper with a first runAsAdmin, then runs a different JRE via JREExecuter; that child calls runAsAdmin again and must
     * connect to the same helper (accepted as descendant of helper's direct parent). Verifies that both the parent and the child actually
     * run commands elevated (via net session check).
     */
    private void testAdminExecuterFromChildJRE() throws Exception {
        logInfoAnyway("Step 1: runAsAdmin(cmd /c echo OK) to ensure helper is running");
        ProcessOutput first = AdminExecuter.runAsAdmin(null, new String[] { "cmd.exe", "/c", "echo", "OK" }, ProcessOptions.DEFAULT);
        assertTrue(first != null && first.getExitCode() == 0, "First runAsAdmin must succeed");
        logInfoAnyway("Step 1b: runAsAdmin(net session) to verify parent runs elevated");
        ProcessOutput elevatedCheck = AdminExecuter.runAsAdmin(null, new String[] { "cmd.exe", "/c", "net session >nul 2>&1" }, ProcessOptions.DEFAULT);
        assertTrue(elevatedCheck != null && elevatedCheck.getExitCode() == 0, "runAsAdmin in parent must run elevated (net session exit 0), got: " + (elevatedCheck != null ? elevatedCheck.getExitCode() : "null"));
        logInfoAnyway("Step 2: executeInJRE with AdminExecuterInChildJRETarget.runRunAsAdminInChild() (JREExecuter adds helper env from AdminExecuter)");
        JavaVersion runJRE = JavaVersion.getVersion().getBase();
        if (runJRE == null || runJRE == JavaVersion.UNKNOWN) {
            runJRE = JavaVersion.JVM_11_0;
        }
        JreOptions options = JreOptions.version(runJRE);
        JREExecuter.runInJRE(options, AdminExecuterInChildJRETarget.class, "runRunAsAdminInChild");
        logInfoAnyway("OK: Child JRE connected to helper and runAsAdmin succeeded");
    }
}
