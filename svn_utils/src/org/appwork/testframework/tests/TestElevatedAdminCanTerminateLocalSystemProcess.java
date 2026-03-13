/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.tests;

import java.io.IOException;
import java.io.Serializable;

import org.appwork.exceptions.NotSupportedException;
import org.appwork.loggingv3.LogV3;
import org.appwork.processes.ProcessHandlerFactory;
import org.appwork.processes.ProcessInfo;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.testframework.executer.AdminExecuter;
import org.appwork.testframework.executer.ElevatedTestTask;
import org.appwork.testframework.executer.ProcessOptions;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.command.Command;

/**
 * Verifies that an elevated admin process (runAsAdmin) can terminate a process running as LocalSystem (NT AUTHORITY\SYSTEM).
 * Uses AdminExecuter.runAsLocalSystem to start a long-running process as SYSTEM and get its PID, then runAsAdmin to
 * call ProcessHandler.terminateForced on that PID (requires SeDebugPrivilege in JNAWindowsProcessHandler).
 */
@TestDependency({ "org.appwork.testframework.executer.AdminExecuter", "org.appwork.testframework.executer.AdminHelperProcess", "org.appwork.processes.jna.JNAWindowsProcessHandler" })
public class TestElevatedAdminCanTerminateLocalSystemProcess extends AWTest implements Serializable {
    private static final long serialVersionUID = 1L;

    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("TestElevatedAdminCanTerminateLocalSystemProcess: Windows only, skipped.");
            return;
        }
        testElevatedAdminTerminatesLocalSystemProcess();
    }

    /**
     * Starts a long-running process as LocalSystem via AdminExecuter.runAsLocalSystem (returns its PID), then runs as
     * elevated admin and calls terminateForced on that PID. Asserts that the elevated admin can terminate the
     * LocalSystem process (SeDebugPrivilege must be enabled by JNAWindowsProcessHandler).
     */
    private void testElevatedAdminTerminatesLocalSystemProcess() throws Exception {
        // 1) Run as LocalSystem: start a process that runs for a long time and return its PID
        logInfoAnyway("TestElevatedAdminCanTerminateLocalSystemProcess: running runAsLocalSystem (ping as SYSTEM to get PID)...");
        Integer localSystemPid = AdminExecuter.runAsLocalSystem(new ElevatedTestTask() {
            private static final long serialVersionUID = 1L;

            @Override
            public Serializable run() throws Exception {
                // ping -n 300 runs ~300 seconds; process runs as LocalSystem
                Command cmd = new Command("ping", "127.0.0.1", "-n", "300");
                cmd.start(true);
                long pid;
                try {
                    pid = cmd.getPID();
                } catch (NotSupportedException e) {
                    LogV3.log(e);
                    throw new Exception("Could not get PID of started process: " + e.getMessage(), e);
                }
                if (pid <= 0) {
                    throw new Exception("Invalid PID from started process: " + pid);
                }
                return Integer.valueOf((int) pid);
            }
        }, TypeRef.INT, ProcessOptions.DEFAULT);

        assertTrue(localSystemPid != null && localSystemPid.intValue() > 0, "runAsLocalSystem must return a valid PID, got: " + localSystemPid);

        logInfoAnyway("TestElevatedAdminCanTerminateLocalSystemProcess: runAsLocalSystem returned PID=" + localSystemPid + ", now running runAsAdmin(terminateForced)...");
        final int pidToTerminate = localSystemPid.intValue();
        try {
            // 2) Run as elevated admin: terminate that PID (must succeed with SeDebugPrivilege)
            Boolean terminated = AdminExecuter.runAsAdmin(new ElevatedTestTask() {
                private static final long serialVersionUID = 1L;
                private final int pid = pidToTerminate;

                @Override
                public Serializable run() throws Exception {
                    ProcessInfo pi = new ProcessInfo(pid);
                    try {
                        boolean ok = ProcessHandlerFactory.getProcessHandler().terminateForced(pi, 0);
                        return Boolean.valueOf(ok);
                    } catch (IOException e) {
                        throw new Exception("terminateForced failed for PID " + pid + ": " + e.getMessage(), e);
                    }
                }
            }, TypeRef.BOOLEAN);

            assertTrue(terminated != null && terminated.booleanValue(), "Elevated admin must be able to terminate LocalSystem process PID " + pidToTerminate + "; terminateForced returned: " + terminated);
        } finally {
            // Best-effort: if runAsAdmin failed or did not kill, try again from current process (e.g. if we are elevated)
            try {
                ProcessInfo pi = new ProcessInfo(pidToTerminate);
                ProcessHandlerFactory.getProcessHandler().terminateForced(pi, 0);
            } catch (Throwable t) {
                LogV3.log(t);
            }
        }
    }
}
