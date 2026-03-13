/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.tests;

import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.ProcessOutput;

/**
 * Target class for {@link TestAdminExecuterFromChildJRE}. Invoked in a child JRE via
 * {@link org.appwork.testframework.JREExecuter#executeInJRE}. Calls AdminExecuter.runAsAdmin to verify that a process that is a
 * descendant of the helper's direct parent (e.g. a PostBuild test or a JRE started by it) can connect to the same helper.
 */
public class AdminExecuterInChildJRETarget {

    /**
     * Runs a simple command via AdminExecuter.runAsAdmin in this (child) JRE. Expects the helper to already be running and
     * connection params to be provided via env (AWTEST_HELPER_LOCK_DIR, AWTEST_HELPER_PRIVATE_KEY_BASE64). The helper must accept
     * this process because it is a descendant of the helper's direct parent. Verifies that the command actually runs elevated (net session)
     * and that stdout is returned. Throws if the command fails or connection is rejected.
     */
    public static void runRunAsAdminInChild() throws Exception {
        if (!CrossSystem.isWindows()) {
            return;
        }
        ProcessOutput elevatedCheck = AdminExecuter.runAsAdmin(null, new String[] { "cmd.exe", "/c", "net session >nul 2>&1" }, ProcessOptions.DEFAULT);
        if (elevatedCheck == null || elevatedCheck.getExitCode() != 0) {
            throw new AssertionError("runAsAdmin from child JRE must run elevated (net session exit 0), got: " + (elevatedCheck != null ? elevatedCheck.getExitCode() : "null"));
        }
        ProcessOutput out = AdminExecuter.runAsAdmin(null, new String[] { "cmd.exe", "/c", "echo", "ADMIN_FROM_CHILD_JRE" }, ProcessOptions.DEFAULT);
        if (out == null || out.getExitCode() != 0) {
            throw new AssertionError("runAsAdmin from child JRE failed: exitCode=" + (out != null ? out.getExitCode() : "null"));
        }
        String stdout = out.getStdOutString();
        if (stdout == null || !stdout.contains("ADMIN_FROM_CHILD_JRE")) {
            throw new AssertionError("runAsAdmin from child JRE: stdout should contain ADMIN_FROM_CHILD_JRE, got: " + stdout);
        }
    }
}
