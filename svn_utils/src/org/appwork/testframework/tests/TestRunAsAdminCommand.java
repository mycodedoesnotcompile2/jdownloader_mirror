/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.tests;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.LogCallback;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.ProcessOutput;

/**
 * AWTest test class for {@link AdminExecuter#runAsAdmin(File, String[], ProcessOptions)}. Verifies that a single command is executed with elevated
 * privileges (one UAC prompt if helper not already running). Windows-only.
 */
@TestDependency({ "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess" })
public class TestRunAsAdminCommand extends AWTest {

    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("TestRunAsAdminCommand: Windows only, skipped.");
            return;
        }
        testRunAsAdminCommandRunsAsAdmin();
        testRunAsAdminCommandExitCode();
        testRunAsAdminCommandStdout();
        testRunAsAdminCommandWithWorkDir();
        testRunAsAdminCommandLogCallback();
    }

    /**
     * runAsAdmin(File, String[]) runs the command in an elevated process. Verify by running a command that only succeeds as admin:
     * "net session" exits 0 only when run with admin rights. Optionally check whoami /groups for "High Mandatory Level" (elevated token).
     */
    private void testRunAsAdminCommandRunsAsAdmin() throws Exception {
        ProcessOutput netSessionOut = AdminExecuter.runAsAdmin(null, new String[] { "cmd.exe", "/c", "net session >nul 2>&1" }, ProcessOptions.DEFAULT);
        assertTrue(netSessionOut != null, "ProcessOutput must not be null");
        assertEquals(0, netSessionOut.getExitCode(), "net session must succeed when run as admin (exit 0); got exitCode=" + netSessionOut.getExitCode());
        ProcessOutput whoamiOut = AdminExecuter.runAsAdmin(null, new String[] { "whoami", "/groups" }, ProcessOptions.DEFAULT);
        assertTrue(whoamiOut != null && whoamiOut.getStdOutString() != null, "whoami /groups must return stdout");
        String stdout = whoamiOut.getStdOutString();
        assertTrue(stdout.contains("High Mandatory Level") || stdout.contains("S-1-16-12288"), "whoami /groups should show elevated token (High Mandatory Level or S-1-16-12288); got: " + stdout);
    }

    /**
     * runAsAdmin(File, String[]) runs cmd and returns exit code 0 for "cmd /c exit 0".
     */
    private void testRunAsAdminCommandExitCode() throws Exception {
        ProcessOutput out = AdminExecuter.runAsAdmin(null, new String[] { "cmd.exe", "/c", "exit", "0" }, ProcessOptions.DEFAULT);
        assertTrue(out != null, "ProcessOutput must not be null");
        assertEquals(0, out.getExitCode(), "exit 0 should return 0");
        out = AdminExecuter.runAsAdmin(null, new String[] { "cmd.exe", "/c", "exit", "7" }, ProcessOptions.DEFAULT);
        assertTrue(out != null, "ProcessOutput must not be null");
        assertEquals(7, out.getExitCode(), "exit 7 should return 7");
    }

    /**
     * runAsAdmin(File, String[]) captures stdout (e.g. echo).
     */
    private void testRunAsAdminCommandStdout() throws Exception {
        ProcessOutput out = AdminExecuter.runAsAdmin(null, new String[] { "cmd.exe", "/c", "echo", "RUN_AS_ADMIN_STDOUT_MARKER" }, ProcessOptions.DEFAULT);
        assertTrue(out != null, "ProcessOutput must not be null");
        String stdout = out.getStdOutString();
        assertTrue(stdout != null && stdout.contains("RUN_AS_ADMIN_STDOUT_MARKER"), "stdout should contain marker, got: " + stdout);
    }

    /**
     * runAsAdmin(File, String[]) with workDir uses that directory (e.g. cd prints it).
     */
    private void testRunAsAdminCommandWithWorkDir() throws Exception {
        File workDir = new File(System.getProperty("java.io.tmpdir"));
        ProcessOutput out = AdminExecuter.runAsAdmin(workDir, new String[] { "cmd.exe", "/c", "cd" }, ProcessOptions.DEFAULT);
        assertTrue(out != null, "ProcessOutput must not be null");
        String stdout = out.getStdOutString();
        assertTrue(stdout != null && stdout.trim().toLowerCase().contains(workDir.getAbsolutePath().toLowerCase().replace("/", "\\")), "stdout should contain workDir, got: " + stdout);
    }

    /**
     * runAsAdmin(File, String[], ProcessOptions) with LogCallback invokes onStdOut/onStdErr for each line of remote stdout/stderr.
     */
    private void testRunAsAdminCommandLogCallback() throws Exception {
        final List<String> stdoutLines = Collections.synchronizedList(new ArrayList<String>());
        final List<String> stderrLines = Collections.synchronizedList(new ArrayList<String>());
        LogCallback callback = new LogCallback() {
            @Override
            public void onStdOut(String line) {
                stdoutLines.add(line);
            }

            @Override
            public void onStdErr(String line) {
                stderrLines.add(line);
            }
        };
        ProcessOptions options = ProcessOptions.builder().logCallback(callback).build();

        stdoutLines.clear();
        stderrLines.clear();
        ProcessOutput out = AdminExecuter.runAsAdmin(null, new String[] { "cmd.exe", "/c", "echo", "LOG_CALLBACK_STDOUT_MARKER" }, options);
        assertTrue(out != null, "ProcessOutput must not be null");
        assertTrue(stdoutLines.size() >= 1, "LogCallback.onStdOut should be called at least once, got: " + stdoutLines.size());
        boolean hasMarker = false;
        for (String line : stdoutLines) {
            if (line != null && line.contains("LOG_CALLBACK_STDOUT_MARKER")) {
                hasMarker = true;
                break;
            }
        }
        assertTrue(hasMarker, "LogCallback should receive line containing LOG_CALLBACK_STDOUT_MARKER, got: " + stdoutLines);

        stdoutLines.clear();
        stderrLines.clear();
        out = AdminExecuter.runAsAdmin(null, new String[] { "cmd.exe", "/c", "echo LOG_CB_OUT & echo LOG_CB_ERR >&2" }, options);
        assertTrue(out != null, "ProcessOutput must not be null");
        boolean hasOut = false;
        for (String line : stdoutLines) {
            if (line != null && line.contains("LOG_CB_OUT")) {
                hasOut = true;
                break;
            }
        }
        assertTrue(hasOut, "LogCallback.onStdOut should receive line containing LOG_CB_OUT, got: " + stdoutLines);
        boolean hasErr = false;
        for (String line : stderrLines) {
            if (line != null && line.contains("LOG_CB_ERR")) {
                hasErr = true;
                break;
            }
        }
        assertTrue(hasErr, "LogCallback.onStdErr should receive line containing LOG_CB_ERR, got: " + stderrLines);
        logInfoAnyway("  OK: LogCallback received stdout and stderr lines");
    }
}
