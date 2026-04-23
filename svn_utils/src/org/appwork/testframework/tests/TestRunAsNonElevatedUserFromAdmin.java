/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.tests;

import java.io.Serializable;

import org.appwork.experimental.windowsexecuter.ExecuteOptions;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ElevatedTestTask;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
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
     * session id of the test process is captured before the helper runs as LocalSystem and passed as {@link ExecuteOptions#wtsSessionId} so
     * the token is resolved for the same interactive session (e.g. RDP), not only the physical console. Also asserts that omitting
     * {@code wtsSessionId} from LocalSystem causes {@link IllegalStateException} (automatic console session selection is disabled).
     */
    private void testRunAsNonElevatedUserFromLocalSystemTask() throws Exception {
        final int callerWtsSessionId = WindowsUtils.getCurrentProcessSessionId();
        assertTrue(callerWtsSessionId >= 0, "Current process must have a WTS session id (ProcessIdToSessionId), got: " + callerWtsSessionId);
        final String callerUserSid = WindowsUtils.getCurrentUserSID();
        Integer result = AdminExecuter.runAsLocalSystem(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                assertTrue(WindowsUtils.isRunningAsLocalSystem(), "This task must run as LocalSystem");
                ExecuteOptions opts = ExecuteOptions.builder().cmd("cmd", "/c", "whoami", "/user").waitFor(true).wtsSessionId(String.valueOf(callerWtsSessionId)).sid(callerUserSid).build();
                ProcessOutput out = AdminExecuter.runAsNonElevatedUser(opts);
                assertTrue(out != null, "ProcessOutput must not be null");
                String stdout = out.getStdOutString();
                assertTrue(stdout != null, "whoami /user must produce stdout");
                assertTrue(!stdout.contains(SID_LOCAL_SYSTEM), "runAsNonElevatedUser must not run as SYSTEM (S-1-5-18), got: " + stdout);
                try {
                    ExecuteOptions withoutSession = ExecuteOptions.builder().cmd("cmd", "/c", "echo", "x").waitFor(true).build();
                    AdminExecuter.runAsNonElevatedUser(withoutSession);
                    assertTrue(false, "runAsNonElevatedUser from LocalSystem without wtsSessionId must throw IllegalStateException");
                } catch (IllegalStateException e) {
                    assertTrue(e.getMessage() != null && e.getMessage().length() > 0, "IllegalStateException must have a message");
                }
                logInfoAnyway("  OK: runAsNonElevatedUser from LocalSystem without wtsSessionId fails as required");
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
                ExecuteOptions opts = ExecuteOptions.builder().cmd("cmd", "/c", "net", "session").waitFor(true).build();
                ProcessOutput out = AdminExecuter.runAsNonElevatedUser(opts);
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
