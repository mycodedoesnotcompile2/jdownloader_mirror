/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.experimental.windowsexecuter.tests;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.appwork.experimental.windowsexecuter.ExecuteOptions;
import org.appwork.experimental.windowsexecuter.WindowsExecuter;
import org.appwork.jna.windows.Kernel32Ext;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ElevatedTestTask;
import org.appwork.utils.LogCallback;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.processes.ProcessOutput;

import com.sun.jna.platform.win32.Advapi32Util;

/**
 * AWTest for {@link org.appwork.experimental.windowsexecuter.WindowsExecuter#runAsNonElevatedUser(ExecuteOptions)}. Starts a task as admin
 * via {@link AdminExecuter#runAsAdmin(ElevatedTestTask, TypeRef)}, then from inside that elevated task runs a command via
 * runAsNonElevatedUser and asserts the child is not elevated (e.g. net session fails).
 */
@TestDependency({ "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class TestRunAsNonElevatedUser extends AWTest implements Serializable {
    private static final long serialVersionUID = 1L;

    public static void main(String[] args) {
        run();
    }

    /**
     * @see org.appwork.testframework.AWTest#isMaintenance()
     */
    @Override
    public boolean isMaintenance() {
        return true;
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("TestRunAsNonElevatedUser: Windows only, skipped.");
            return;
        }
        if (!org.appwork.JNAHelper.isJNAAvailable()) {
            logInfoAnyway("TestRunAsNonElevatedUser: JNA not available, skipped.");
            return;
        }
        try {
            testRunAsAdminThenRunAsNonElevated();
        } catch (Exception e) {
            if (e.getMessage() != null && (e.getMessage().contains("elevated helper") || e.getMessage().contains("UAC") || e.getMessage().contains("Failed to start"))) {
                logInfoAnyway("TestRunAsNonElevatedUser: Could not start elevated helper (UAC skipped or failed). Running reduced test without elevation.");
                testRunAsNonElevatedUserStandalone();
            } else {
                throw e;
            }
        }
    }

    /**
     * Reduced test: run runAsNonElevatedUser without requiring elevated context. Verifies the API runs and echo returns stdout.
     */
    private void testRunAsNonElevatedUserStandalone() throws Exception {
        ProcessOutput echoOut = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                .cmd("cmd.exe", "/c", "echo", "NON_ELEVATED_MARKER") //
                .waitFor(true) //
                .build());
        assertTrue(echoOut != null, "ProcessOutput must not be null");
        assertTrue(echoOut.getStdOutString() != null && echoOut.getStdOutString().contains("NON_ELEVATED_MARKER"), "stdout must contain marker (got: " + echoOut.getStdOutString() + ")");
        testWorkingDirStandalone();
        testWaitForFalseStandalone();
        testWaitForFalseReturnsImmediatelyStandalone();
        testLogCallbackStandalone();
        testSidNullStandalone();
        testSidEmptyStandalone();
        testSidInvalidFallbackStandalone();
    }

    /** Test ExecuteOptions.workingDir: run cmd /c cd and assert stdout contains the working dir path. */
    private void testWorkingDirStandalone() throws Exception {
        File workDir = new File(System.getProperty("java.io.tmpdir"), "TestRunAsNonElevatedUser_workDir_" + System.currentTimeMillis());
        workDir.mkdirs();
        try {
            ProcessOutput out = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                    .cmd("cmd.exe", "/c", "cd") //
                    .workingDir(workDir) //
                    .waitFor(true) //
                    .build());
            assertTrue(out != null && out.getStdOutString() != null, "ProcessOutput and stdout must not be null");
            String normalizedOut = out.getStdOutString().trim().replace('/', '\\');
            String normalizedPath = workDir.getAbsolutePath().replace('/', '\\');
            assertTrue(normalizedOut.contains(normalizedPath), "stdout must contain workingDir path (expected containing: " + normalizedPath + ", got: " + normalizedOut + ")");
        } finally {
            if (workDir.exists()) {
                workDir.delete();
            }
        }
    }

    /** Test ExecuteOptions.waitFor(false): assert exitCode -1 and remotePid set. */
    private void testWaitForFalseStandalone() throws Exception {
        ProcessOutput out = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                .cmd("cmd.exe", "/c", "echo", "WAITFOR_FALSE_MARKER") //
                .waitFor(false) //
                .build());
        assertTrue(out != null, "ProcessOutput must not be null");
        assertTrue(out.getExitCode() == -1, "waitFor=false must return exitCode -1 (got: " + out.getExitCode() + ")");
        assertTrue(out.getRemotePid() != null && out.getRemotePid().intValue() > 0, "waitFor=false must set remotePid (got: " + out.getRemotePid() + ")");
    }

    /**
     * Test that waitFor(false) returns immediately without waiting for the process to exit. Starts a process that runs for several seconds
     * (ping) and asserts the call returns within 3 seconds.
     */
    private void testWaitForFalseReturnsImmediatelyStandalone() throws Exception {
        long start = System.currentTimeMillis();
        ProcessOutput out = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                .cmd("cmd.exe", "/c", "ping", "-n", "6", "127.0.0.1") //
                .waitFor(false) //
                .build());
        long elapsed = System.currentTimeMillis() - start;
        assertTrue(out != null, "ProcessOutput must not be null");
        assertTrue(out.getExitCode() == -1, "waitFor=false must return exitCode -1 (got: " + out.getExitCode() + ")");
        assertTrue(out.getRemotePid() != null && out.getRemotePid().intValue() > 0, "waitFor=false must set remotePid (got: " + out.getRemotePid() + ")");
        assertTrue(elapsed < 3000, "waitFor=false must return immediately (within 3s), not wait for process; elapsed=" + elapsed + " ms");
    }

    /** Test ExecuteOptions.logCallback: assert callback receives stdout line. */
    private void testLogCallbackStandalone() throws Exception {
        final List<String> stdoutLines = new ArrayList<String>();
        final List<String> stderrLines = new ArrayList<String>();
        LogCallback cb = new LogCallback() {
            @Override
            public void onStdOut(String line) {
                stdoutLines.add(line);
            }

            @Override
            public void onStdErr(String line) {
                stderrLines.add(line);
            }
        };
        ProcessOutput out = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                .cmd("cmd.exe", "/c", "echo", "LOG_CALLBACK_MARKER") //
                .waitFor(true) //
                .logCallback(cb) //
                .build());
        assertTrue(out != null, "ProcessOutput must not be null");
        assertTrue(out.getStdOutString() != null && out.getStdOutString().contains("LOG_CALLBACK_MARKER"), "stdout must contain marker");
        assertTrue(stdoutLines.size() >= 1 && stdoutLines.get(stdoutLines.size() - 1).contains("LOG_CALLBACK_MARKER"), "logCallback must have received stdout line containing LOG_CALLBACK_MARKER (got: " + stdoutLines + ")");
    }

    /** Test ExecuteOptions.sid(null): must run with lowest privilege like no sid; echo must succeed. */
    private void testSidNullStandalone() throws Exception {
        ProcessOutput out = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                .cmd("cmd.exe", "/c", "echo", "SID_NULL_MARKER") //
                .waitFor(true) //
                .sid(null) //
                .build());
        assertTrue(out != null, "ProcessOutput must not be null");
        assertTrue(out.getStdOutString() != null && out.getStdOutString().contains("SID_NULL_MARKER"), "sid(null) must run with lowest privilege and produce stdout (got: " + out.getStdOutString() + ")");
    }

    /** Test ExecuteOptions.sid(""): empty SID must fall back to lowest privilege; echo must succeed. */
    private void testSidEmptyStandalone() throws Exception {
        ProcessOutput out = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                .cmd("cmd.exe", "/c", "echo", "SID_EMPTY_MARKER") //
                .waitFor(true) //
                .sid("") //
                .build());
        assertTrue(out != null, "ProcessOutput must not be null");
        assertTrue(out.getStdOutString() != null && out.getStdOutString().contains("SID_EMPTY_MARKER"), "sid(empty) must run with lowest privilege (got: " + out.getStdOutString() + ")");
    }

    /** Test ExecuteOptions.sid(non-matching SID): must fall back to lowest privilege; echo must succeed. */
    private void testSidInvalidFallbackStandalone() throws Exception {
        ProcessOutput out = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                .cmd("cmd.exe", "/c", "echo", "SID_INVALID_FALLBACK_MARKER") //
                .waitFor(true) //
                .sid("S-1-5-99-99999999") //
                .build());
        assertTrue(out != null, "ProcessOutput must not be null");
        assertTrue(out.getStdOutString() != null && out.getStdOutString().contains("SID_INVALID_FALLBACK_MARKER"), "sid(non-matching) must fall back to lowest privilege and produce stdout (got: " + out.getStdOutString() + ")");
    }

    /**
     * Run as admin (elevated), then from inside that task call WindowsExecuter.runAsNonElevatedUser to run a command that only fails when
     * not elevated (net session). Asserts the child is non-elevated.
     */
    private void testRunAsAdminThenRunAsNonElevated() throws Exception {
        LogV3.info("RUN testRunAsAdminThenRunAsNonElevated");
        testSidActiveConsoleUserInTask();
        Integer result = AdminExecuter.runAsAdmin(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                LogV3.info("testRunAsAdminThenRunAsNonElevated");
                assertTrue(WindowsUtils.isElevated(), "This task must run elevated (in admin helper)");
                // From elevated context: run a child via runAsNonElevatedUser. The child must NOT be elevated.
                ProcessOutput netSessionOut = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                        .cmd("cmd.exe", "/c", "net", "session") //
                        .waitFor(true) //
                        .build());
                LogV3.info("netSessionOut " + netSessionOut + "");
                assertTrue(netSessionOut != null, "ProcessOutput must not be null");
                // net session succeeds only when elevated; non-elevated gets error exit code
                assertTrue(netSessionOut.getExitCode() != 0, "net session must fail when run non-elevated (exitCode=" + netSessionOut.getExitCode() + ")");
                // Sanity: run echo and check stdout
                ProcessOutput echoOut = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                        .cmd("cmd.exe", "/c", "echo", "NON_ELEVATED_MARKER") //
                        .waitFor(true) //
                        .build());
                LogV3.info("ECHOOUT: " + echoOut + "");
                assertTrue(echoOut != null && echoOut.getStdOutString() != null && echoOut.getStdOutString().contains("NON_ELEVATED_MARKER"), "stdout must contain marker");
                // Test other ExecuteOptions: workingDir, waitFor(false), logCallback
                testWorkingDirInTask();
                testWaitForFalseInTask();
                testLogCallbackInTask();
                testSidActiveConsoleUserInTask();
                return Integer.valueOf(0);
            }
        }, TypeRef.INT);
        assertTrue(result != null && result.intValue() == 0, "task should return 0");
    }

    /** Test workingDir from inside elevated task. */
    private void testWorkingDirInTask() throws Exception {
        File workDir = new File(System.getProperty("java.io.tmpdir"), "TestRunAsNonElevatedUser_workDir_" + System.currentTimeMillis());
        workDir.mkdirs();
        try {
            ProcessOutput out = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                    .cmd("cmd.exe", "/c", "cd") //
                    .workingDir(workDir) //
                    .waitFor(true) //
                    .build());
            assertTrue(out != null && out.getStdOutString() != null, "ProcessOutput and stdout must not be null");
            String normalizedOut = out.getStdOutString().trim().replace('/', '\\');
            String normalizedPath = workDir.getAbsolutePath().replace('/', '\\');
            assertTrue(normalizedOut.contains(normalizedPath), "stdout must contain workingDir (expected: " + normalizedPath + ", got: " + normalizedOut + ")");
        } finally {
            if (workDir.exists()) {
                workDir.delete();
            }
        }
    }

    /** Test waitFor(false) from inside elevated task. */
    private void testWaitForFalseInTask() throws Exception {
        ProcessOutput out = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                .cmd("cmd.exe", "/c", "echo", "WAITFOR_FALSE_MARKER") //
                .waitFor(false) //
                .build());
        assertTrue(out != null, "ProcessOutput must not be null");
        assertTrue(out.getExitCode() == -1, "waitFor=false must return exitCode -1 (got: " + out.getExitCode() + ")");
        assertTrue(out.getRemotePid() != null && out.getRemotePid().intValue() > 0, "waitFor=false must set remotePid (got: " + out.getRemotePid() + ")");
    }

    /** Test logCallback from inside elevated task. */
    private void testLogCallbackInTask() throws Exception {
        final List<String> stdoutLines = new ArrayList<String>();
        LogCallback cb = new LogCallback() {
            @Override
            public void onStdOut(String line) {
                stdoutLines.add(line);
            }

            @Override
            public void onStdErr(String line) {
            }
        };
        ProcessOutput out = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                .cmd("cmd.exe", "/c", "echo", "LOG_CALLBACK_MARKER") //
                .waitFor(true) //
                .logCallback(cb) //
                .build());
        assertTrue(out != null && out.getStdOutString() != null && out.getStdOutString().contains("LOG_CALLBACK_MARKER"), "stdout must contain marker");
        assertTrue(stdoutLines.size() >= 1 && stdoutLines.get(stdoutLines.size() - 1).contains("LOG_CALLBACK_MARKER"), "logCallback must have received line (got: " + stdoutLines + ")");
    }

    /**
     * From LocalSystem, {@link WindowsExecuter#runAsNonElevatedUser} requires an explicit {@link ExecuteOptions#getWtsSessionId()} (no
     * automatic active-console pick). Pass the physical active-console session id ({@code WTSGetActiveConsoleSessionId}), then from a
     * LocalSystem task run {@code whoami /user} and assert the token matches {@link WindowsUtils#getActiveConsoleAccount()}.
     */
    private void testSidActiveConsoleUserInTask() throws Exception {
        LogV3.info("testSidActiveConsoleUserInTask");
        final int activeConsoleSessionId = Kernel32Ext.INSTANCE.WTSGetActiveConsoleSessionId();
        if (activeConsoleSessionId == (int) 0xFFFFFFFFL) {
            logInfoAnyway("testSidActiveConsoleUserInTask: no active console session (WTSGetActiveConsoleSessionId), skipped.");
            return;
        }
        final String wtsSessionIdForChild = String.valueOf(activeConsoleSessionId);
        Integer result = AdminExecuter.runAsLocalSystem(new ElevatedTestTask() {
            @Override
            public Serializable run() throws Exception {
                Advapi32Util.Account activeAccount = WindowsUtils.getActiveConsoleAccount();
                assertNotNull(activeAccount);
                assertNotNull(activeAccount.sidString);
                ProcessOutput out = WindowsExecuter.runAsNonElevatedUser(ExecuteOptions.builder() //
                        .cmd("cmd.exe", "/c", "whoami", "/user") //
                        .waitFor(true) //
                        .wtsSessionId(wtsSessionIdForChild) //
                        .build());
                LogV3.info("Output: " + out);
                assertTrue(out != null, "ProcessOutput must not be null");
                assertTrue(out.getStdOutString() != null, "stdout must not be null");
                assertTrue(out.getStdOutString().contains(activeAccount.sidString), "Process must run under active console SID " + activeAccount.sidString + "; whoami /user output must contain that SID (got: " + out + ")");
                return Integer.valueOf(0);
            }
        }, TypeRef.INT, null);
        assertTrue(result != null && result.intValue() == 0, "task should return 0");
    }
}
