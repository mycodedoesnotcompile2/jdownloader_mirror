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

import java.io.Serializable;
import java.util.EnumSet;
import java.util.Set;

import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.TestTag;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ElevatedTestTask;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.windows.execute.RunAsLaunchOptions;
import org.appwork.utils.processes.ProcessOutput;

/**
 * AWTest for {@link AdminExecuter#runAsNonElevatedUser}. Verifies that when running inside a runAsAdmin or runAsLocalSystem task, a call to
 * runAsNonElevatedUser runs the command as non-elevated (e.g. whoami does not show SYSTEM, net session fails). Also verifies that when
 * already non-elevated, the command runs in-process. Windows-only.
 */
@TestDependency({ "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class TestRunAsNonElevatedUserFromAdmin extends AWTest implements Serializable {
    private static final long   serialVersionUID = 1L;
    /** SID for Local System; whoami /user output must NOT contain this when run via runAsNonElevatedUser. */
    private static final String SID_LOCAL_SYSTEM = "S-1-5-18";

    public static void main(String[] args) {
        run();
    }

    @Override
    public Set<TestTag> getTags() {
        return EnumSet.of(TestTag.UAC);
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("TestRunAsNonElevatedUserFromAdmin: Windows only, skipped.");
            return;
        }
        testRunAsNonElevatedUserFromLocalSystemTask();
        testRunAsNonElevatedUserFromAdminTask();
        testRunAsNonElevatedUserAlreadyNonElevated();
    }

    /**
     * Inside a runAsLocalSystem task, runAsNonElevatedUser runs the command as non-SYSTEM. whoami /user must not show S-1-5-18. The WTS
     * session id of the test process is captured before the helper runs as LocalSystem and passed so the token is resolved for the same
     * interactive session (e.g. RDP), not only the physical console. Also asserts that omitting {@code sessionId} from LocalSystem causes
     * {@link IllegalStateException} (automatic console session selection is disabled).
     */
    private void testRunAsNonElevatedUserFromLocalSystemTask() throws Exception {
        final int callerSessionId = WindowsUtils.getCurrentProcessSessionId();
        assertTrue(callerSessionId >= 0, "Current process must have a WTS session id (ProcessIdToSessionId), got: " + callerSessionId);
        Integer result = AdminExecuter.runAsLocalSystem(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                assertTrue(WindowsUtils.isRunningAsLocalSystem(), "This task must run as LocalSystem");
                String[] whoamiCmd = new String[] { "cmd", "/c", "whoami", "/user" };
                ProcessOutput out = AdminExecuter.runAsNonElevatedUser(whoamiCmd, RunAsLaunchOptions.DEFAULT, Integer.valueOf(callerSessionId));
                assertTrue(out != null, "ProcessOutput must not be null");
                String stdout = out.getStdOutString();
                assertTrue(stdout != null, "whoami /user must produce stdout");
                assertTrue(!stdout.contains(SID_LOCAL_SYSTEM), "runAsNonElevatedUser must not run as SYSTEM (S-1-5-18), got: " + stdout);
                try {
                    AdminExecuter.runAsNonElevatedUser(new String[] { "cmd", "/c", "echo", "x" }, RunAsLaunchOptions.DEFAULT, null);
                    assertTrue(false, "runAsNonElevatedUser from LocalSystem without sessionId must throw IllegalStateException");
                } catch (IllegalStateException e) {
                    assertTrue(e.getMessage() != null && e.getMessage().length() > 0, "IllegalStateException must have a message");
                }
                logInfoAnyway("  OK: runAsNonElevatedUser from LocalSystem without sessionId fails as required");
                return Integer.valueOf(0);
            }
        }, TypeRef.INT, ProcessOptions.DEFAULT);
        assertTrue(result != null && result.intValue() == 0, "outer task should return 0");
        logInfoAnyway("  OK: runAsNonElevatedUser from LocalSystem task runs as non-SYSTEM");
    }

    /**
     * Inside a runAsAdmin task, runAsNonElevatedUser runs the command non-elevated. net session fails (exit code != 0) when not elevated.
     */
    private void testRunAsNonElevatedUserFromAdminTask() throws Exception {
        Integer result = AdminExecuter.runAsAdmin(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                assertTrue(WindowsUtils.isElevated(), "This task must run elevated");
                ProcessOutput out = AdminExecuter.runAsNonElevatedUser(new String[] { "cmd", "/c", "net", "session" }, RunAsLaunchOptions.DEFAULT);
                assertTrue(out != null, "ProcessOutput must not be null");
                assertTrue(out.getExitCode() != 0, "net session must fail when run non-elevated (exitCode should be != 0), got: " + out.getExitCode());
                return Integer.valueOf(0);
            }
        }, TypeRef.INT, ProcessOptions.DEFAULT);
        assertTrue(result != null && result.intValue() == 0, "outer task should return 0");
        logInfoAnyway("  OK: runAsNonElevatedUser from Admin task runs non-elevated (net session fails)");
    }

    /**
     * When already non-elevated (normal test process), runAsNonElevatedUser runs the command in-process. whoami /user returns current user
     * (no SYSTEM).
     */
    private void testRunAsNonElevatedUserAlreadyNonElevated() throws Exception {
        if (WindowsUtils.isElevated() || WindowsUtils.isRunningAsLocalSystem()) {
            logInfoAnyway("  SKIP: testRunAsNonElevatedUserAlreadyNonElevated requires non-elevated process");
            return;
        }
        ProcessOutput out = AdminExecuter.runAsNonElevatedUser(null, new String[] { "cmd", "/c", "whoami", "/user" }, ProcessOptions.DEFAULT);
        assertTrue(out != null, "ProcessOutput must not be null");
        String stdout = out.getStdOutString();
        assertTrue(stdout != null && stdout.length() > 0, "whoami /user must produce stdout when run in-process");
        assertTrue(!stdout.contains(SID_LOCAL_SYSTEM), "Current user must not be SYSTEM when test runs non-elevated, got: " + stdout);
        logInfoAnyway("  OK: runAsNonElevatedUser when already non-elevated runs in-process");
    }
}
