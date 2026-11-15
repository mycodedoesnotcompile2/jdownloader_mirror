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
