/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.processes.jna.tests;

import java.io.File;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.app.gui.BasicGui;
import org.appwork.processes.ProcessHandler;
import org.appwork.processes.ProcessInfo;
import org.appwork.processes.windows.jna.JNAWindowsProcessHandler;
import org.appwork.serializer.Deser;
import org.appwork.storage.TypeRef;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.testhelper.AWTestValidateClassReference;
import org.appwork.utils.Time;
import org.appwork.utils.UniqueAlltimeID;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.JNAProcessInfo;
import org.appwork.utils.os.WindowsUtilsKernel32;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.parser.ShellParser.Style;
import org.appwork.utils.processes.command.AbstractLineHandler;
import org.appwork.utils.processes.command.Command;

import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinDef.HWND;

/**
 * @author thomas
 * @date 19.11.2024
 *
 */
@TestDependency({ TestJNAWindowsProcessHandler.ORG_APPWORK_PROCESSES_JNA_JNA_WINDOWS_PROCESS_HANDLER, TestJNAWindowsProcessHandler.ORG_APPWORK_APP_GUI_BASIC_GUI, "org.appwork.utils.os.WindowsUtils" })
public class TestJNAWindowsProcessHandler extends AWTest {
    @AWTestValidateClassReference
    public static final String ORG_APPWORK_APP_GUI_BASIC_GUI                         = "org.appwork.app.gui.BasicGui";
    @AWTestValidateClassReference
    public static final String ORG_APPWORK_PROCESSES_JNA_JNA_WINDOWS_PROCESS_HANDLER = "org.appwork.processes.windows.jna.JNAWindowsProcessHandler";

    /**
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            return;
        }
        // required for post build tests to auto-include requirements
        // this will throw a Class not found error, if BasicGui is not part of the build - and this will get ignored.
        // without this import, we will get an undefined error in the extra process spawned later - which cannot be evaluated by the Post
        // Script Runner correctly.
        BasicGui.class.getName();
        String unique = "awtest" + UniqueAlltimeID.create();
        JNAWindowsProcessHandler handler = new JNAWindowsProcessHandler();
        testSendCloseToWindow(unique, handler);
        testSendCTRL(unique, handler);
        testKill(unique, handler);
        testListByProcessInfoWithNormalOnly(unique, handler);
        testListByProcessInfoWithJNAOnly(handler);
        testListByProcessInfoMixed(unique, handler);
        testGetLockingProcesses(unique, handler);
        testGetLockingProcessesNoLock(unique, handler);
        testListByPathVsListByPidsTiming(handler);
    }

    /**
     * Tests listByProcessInfo with only normal ProcessInfo instances (from listByPath / Command).
     */
    protected void testListByProcessInfoWithNormalOnly(String unique, ProcessHandler handler) throws Exception {
        Command c = new Command("cmd", "/c", "ping localhost -n 30 -" + unique);
        c.setOutputHandler(new AbstractLineHandler() {
            @Override
            public void handleLine(String line, Object caller) {
            }
        });
        c.start(true);
        try {
            ProcessInfo found = null;
            Timeout timeout = new Timeout(5000);
            while (timeout.isAlive() && found == null) {
                List<ProcessInfo> list = handler.listByPath("C:\\Windows\\System32\\PING.EXE");
                for (ProcessInfo p : list) {
                    if (p.getCommandLine() != null && p.getCommandLine().contains("-" + unique)) {
                        found = p;
                        break;
                    }
                }
                if (found == null) {
                    Thread.sleep(200);
                }
            }
            assertTrue(found != null, "Should find ping process");
            List<ProcessInfo> running = handler.listByProcessInfo(found);
            assertTrue(running.size() == 1, "listByProcessInfo should return one running process");
            assertEquals(found.getPid(), running.get(0).getPid());
            assertTrue(running.get(0) != found, "listByProcessInfo returns new instances");
            handler.terminateForced(running.get(0), 0);
            List<ProcessInfo> after = handler.listByProcessInfo(found);
            assertTrue(after.isEmpty(), "Process should be gone");
        } finally {
            if (c.getProcess() != null && c.getProcess().isAlive()) {
                c.getProcess().destroyForcibly();
            }
        }
    }

    /**
     * Tests listByProcessInfo and terminate/wait with only JNAProcessInfo (startProcessWithHandle, no UAC).
     */
    protected void testListByProcessInfoWithJNAOnly(ProcessHandler handler) throws Exception {
        String marker = "awtest-jnaonly-" + UniqueAlltimeID.create();
        JNAProcessInfo jna = null;
        try {
            jna = startProcessWithHandle(new String[] { "cmd", "/c", "ping localhost -n 30 -" + marker }, null);
            assertTrue(jna.getPid() > 0);
            assertTrue(jna.getCommandLine() != null && jna.getCommandLine().contains(marker));
            List<ProcessInfo> running = handler.listByProcessInfo(jna);
            assertTrue(running.size() == 1, "listByProcessInfo(JNAProcessInfo) should return one when still running");
            assertEquals(jna.getPid(), running.get(0).getPid());
            assertTrue(running.get(0) != jna, "listByProcessInfo returns new instances");
            boolean killed = handler.terminateForced(jna, 7);
            assertTrue(killed);
            assertTrue(jna.isClosed(), "Handler should close handle after terminateForced");
            List<ProcessInfo> after = handler.listByProcessInfo(jna);
            assertTrue(after.isEmpty(), "Process should be gone");
        } finally {
            if (jna != null && !jna.isClosed()) {
                jna.close();
            }
        }
    }

    /**
     * Starts a process without elevation (no UAC) via CreateProcess. The returned {@link JNAProcessInfo} holds the process handle for use
     * with {@link org.appwork.processes.ProcessHandler} or {@link JNAProcessInfo#close()}.
     * <p>
     * The command line is passed in a mutable buffer (CreateProcessW may modify it). Use for long-running commands (e.g.
     * {@code cmd.exe /c ping localhost -n 200}) so the process stays alive until you terminate or wait for it.
     *
     * @param command
     *            command and arguments (e.g. {@code "cmd.exe", "/c", "ping", "localhost", "-n", "200"})
     * @param workingDir
     *            working directory (can be null)
     * @return JNAProcessInfo with PID, handle and commandLine; use ProcessHandler or {@link JNAProcessInfo#close()}
     * @throws Win32Exception
     *             if the process cannot be started
     * @throws UnsupportedOperationException
     *             if not running on Windows
     */
    public static JNAProcessInfo startProcessWithHandle(String[] command, String workingDir) throws Win32Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("This operation is only supported on Windows");
        }
        if (command == null || command.length == 0) {
            throw new IllegalArgumentException("Command cannot be null or empty");
        }
        String cmdLine = ShellParser.createCommandLine(Style.WINDOWS, command);
        // CreateProcessW modifies lpCommandLine; must pass a mutable buffer (see MSDN / Old New Thing).
        char[] cmdLineBuf = (cmdLine + "\0").toCharArray();
        WinBase.STARTUPINFO si = new WinBase.STARTUPINFO();
        si.clear();
        WinBase.PROCESS_INFORMATION pi = new WinBase.PROCESS_INFORMATION();
        if (!WindowsUtilsKernel32.INSTANCE.CreateProcess(null, cmdLineBuf, null, null, false, new DWORD(Kernel32.CREATE_UNICODE_ENVIRONMENT | WinBase.CREATE_NO_WINDOW), null, workingDir, si, pi)) {
            throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
        }
        if (pi.hThread != null && Pointer.nativeValue(pi.hThread.getPointer()) != 0) {
            Kernel32.INSTANCE.CloseHandle(pi.hThread);
        }
        JNAProcessInfo info = new JNAProcessInfo(pi.hProcess);
        info.setCommandLine(cmdLine);
        info.setWorkingDirectory(workingDir);
        return info;
    }

    /**
     * Tests listByProcessInfo with mixed JNAProcessInfo and normal ProcessInfo.
     */
    protected void testListByProcessInfoMixed(String unique, ProcessHandler handler) throws Exception {
        Command c = new Command("cmd", "/c", "ping localhost -n 40 -" + unique);
        c.setOutputHandler(new AbstractLineHandler() {
            @Override
            public void handleLine(String line, Object caller) {
            }
        });
        c.start(true);
        JNAProcessInfo jna = null;
        try {
            ProcessInfo normalFound = null;
            Timeout timeout = new Timeout(5000);
            while (timeout.isAlive() && normalFound == null) {
                List<ProcessInfo> list = handler.listByPath("C:\\Windows\\System32\\PING.EXE");
                for (ProcessInfo p : list) {
                    if (p.getCommandLine() != null && p.getCommandLine().contains("-" + unique)) {
                        normalFound = p;
                        break;
                    }
                }
                if (normalFound == null) {
                    Thread.sleep(200);
                }
            }
            assertTrue(normalFound != null);
            String marker = "awtest-mixed-" + UniqueAlltimeID.create();
            jna = startProcessWithHandle(new String[] { "cmd", "/c", "ping localhost -n 30 -" + marker }, null);
            List<ProcessInfo> running = handler.listByProcessInfo(normalFound, jna);
            assertTrue(running.size() == 2, "listByProcessInfo(mixed) should return two: " + running.size());
            Set<Integer> pids = new HashSet<Integer>();
            for (ProcessInfo p : running) {
                pids.add(Integer.valueOf(p.getPid()));
            }
            assertTrue(pids.contains(Integer.valueOf(normalFound.getPid())));
            assertTrue(pids.contains(Integer.valueOf(jna.getPid())));
            handler.terminateForced(jna, 0);
            List<ProcessInfo> afterJna = handler.listByProcessInfo(normalFound, jna);
            assertTrue(afterJna.size() == 1, "After killing JNA process one should remain");
            handler.terminateForced(afterJna.get(0), 0);
            List<ProcessInfo> afterBoth = handler.listByProcessInfo(normalFound, jna);
            assertTrue(afterBoth.isEmpty());
        } finally {
            if (jna != null && !jna.isClosed()) {
                jna.close();
            }
            if (c.getProcess() != null && c.getProcess().isAlive()) {
                c.getProcess().destroyForcibly();
            }
        }
    }

    protected void testSendCTRL(String unique, ProcessHandler handler) throws IOException, InterruptedException, Exception {
        Command c = new Command("cmd", "/c", "ping localhost -n 1000 -" + unique);
        c.setOutputHandler(new AbstractLineHandler() {
            @Override
            public void handleLine(String line, Object caller) {
                // System.out.println(line);
            }
        });
        c.start(true);
        try {
            ProcessInfo found = null;
            ArrayList<Integer> pids = new ArrayList<Integer>();
            Timeout timeout = new Timeout(5000);
            List<ProcessInfo> list = null;
            while (timeout.isAlive() && found == null) {
                pids.clear();
                list = handler.listByPath("C:\\Windows\\System32\\PING.EXE");
                for (ProcessInfo p : list) {
                    pids.add(p.getPid());
                    if (p.getCommandLine().contains("-" + unique)) {
                        found = p;
                    }
                }
            }
            assertTrue(found != null);
            List<ProcessInfo> list2 = handler.listByPids(Deser.get().convert(pids, TypeRef.INT_ARRAY));
            assertEqualsDeep(list, list2);
            handler.terminateRequest(found);
            if (handler.waitForExit(TimeSpan.parse("10s"), found).size() > 0) {
                throw new Exception("Could not kill");
            }
            assertFalse(handler.terminateRequest(found));
            list.remove(found);
            List<ProcessInfo> list3 = handler.listByPids(Deser.get().convert(pids, TypeRef.INT_ARRAY));
            assertEqualsDeep(list, list3);
        } finally {
            c.getProcess().destroyForcibly();
        }
    }

    protected void testKill(String unique, ProcessHandler handler) throws IOException, Exception {
        Timeout timeout = new Timeout(5000);
        logInfoAnyway(java.lang.management.ManagementFactory.getRuntimeMXBean().getClassPath());
        final Command command = new Command(CrossSystem.getJavaBinary(), "-cp", java.lang.management.ManagementFactory.getRuntimeMXBean().getClassPath(), TestApp.class.getName(), "-" + unique);
        command.setOutputHandler(new AbstractLineHandler() {
            @Override
            public void handleLine(String line, Object caller) {
                System.out.println(caller + ":" + line);
            }
        });
        command.start(true);
        try {
            List<ProcessInfo> list = handler.listByPath(CrossSystem.getJavaBinary());
            ProcessInfo instance = null;
            ArrayList<Integer> pids = new ArrayList<Integer>();
            while (timeout.isAlive() && instance == null) {
                pids.clear();
                list = handler.listByPath(CrossSystem.getJavaBinary());
                for (ProcessInfo p : list) {
                    pids.add(p.getPid());
                    if (p.getCommandLine().contains("-" + unique)) {
                        instance = p;
                    }
                }
            }
            assertTrue(instance != null);
            assertTrue(handler.terminateForced(instance, -1));
            if (handler.waitForExit(TimeSpan.parse("10s"), instance).size() > 0) {
                throw new Exception("Could not kill");
            }
            assertFalse(handler.terminateRequest(instance));
            List<ProcessInfo> list2 = handler.listByPath(CrossSystem.getJavaBinary());
            list.remove(instance);
            assertEqualsDeep(list2, list);
        } finally {
            command.destroy();
        }
    }

    protected void testSendCloseToWindow(String unique, JNAWindowsProcessHandler handler) throws IOException, Exception {
        final Command command = new Command(CrossSystem.getJavaBinary(), "-cp", java.lang.management.ManagementFactory.getRuntimeMXBean().getClassPath(), TestApp.class.getName(), "-" + unique);
        final AtomicReference<Boolean> received = new AtomicReference<Boolean>(false);
        command.setOutputHandler(new AbstractLineHandler() {
            @Override
            public void handleLine(String line, Object caller) {
                AWTest.logInfoAnyway(caller + " :" + line);
                if (line.contains("Received Request Exit")) {
                    received.set(true);
                }
            }
        });
        command.start(true);
        List<ProcessInfo> list = null;
        Timeout timeout = new Timeout(5000);
        ProcessInfo instance = null;
        while (timeout.isAlive() && instance == null) {
            list = handler.listByPath(CrossSystem.getJavaBinary());
            for (ProcessInfo p : list) {
                if (p.getCommandLine().contains("-" + unique)) {
                    instance = p;
                }
            }
        }
        assertTrue(instance != null);
        long started = Time.systemIndependentCurrentJVMTimeMillis();
        while (Time.systemIndependentCurrentJVMTimeMillis() - started < 10000) {
            HWND window = handler.findMainWindow(instance.getPid());
            if (window != null) {
                break;
            }
            Thread.sleep(1000);
        }
        handler.terminateRequest(instance);
        Thread.sleep(2000);
        assertTrue(received.get());
        List<ProcessInfo> remaining;
        if ((remaining = handler.waitForExit(TimeSpan.parse("10s"), instance)).size() > 0) {
            throw new Exception("Could not kill: " + remaining);
        }
        List<ProcessInfo> list2 = handler.listByPath(CrossSystem.getJavaBinary());
        list.remove(instance);
        assertFalse(handler.terminateRequest(instance));
        // s assertTrue(list2.size() == list.size());
        assertEqualsDeep(list2, list);
    }

    protected void testGetLockingProcesses(String unique, JNAWindowsProcessHandler handler) throws IOException, InterruptedException, Exception {
        // Create a temporary file
        File tempFile = File.createTempFile("testlock-" + unique, ".tmp");
        tempFile.deleteOnExit();
        try {
            // Start a process that locks the file
            final Command command = new Command(CrossSystem.getJavaBinary(), "-cp", java.lang.management.ManagementFactory.getRuntimeMXBean().getClassPath(), TestFileLock.class.getName(), tempFile.getAbsolutePath(), unique);
            command.setOutputHandler(new AbstractLineHandler() {
                @Override
                public void handleLine(String line, Object caller) {
                    System.out.println(caller + ":" + line);
                }
            });
            command.start(true);
            ProcessInfo lockingProcess = null;
            try {
                // Wait a bit for the process to start and lock the file
                Thread.sleep(2000);
                // Find the process that should be locking the file
                List<ProcessInfo> javaProcesses = handler.listByPath(CrossSystem.getJavaBinary());
                Timeout timeout = new Timeout(5000);
                while (timeout.isAlive() && lockingProcess == null) {
                    javaProcesses = handler.listByPath(CrossSystem.getJavaBinary());
                    for (ProcessInfo p : javaProcesses) {
                        if (p.getCommandLine().contains(TestFileLock.class.getName()) && p.getCommandLine().contains(unique)) {
                            lockingProcess = p;
                            break;
                        }
                    }
                    if (lockingProcess == null) {
                        Thread.sleep(500);
                    }
                }
                assertTrue(lockingProcess != null, "Could not find locking process");
                // Test getLockingProcesses
                List<ProcessInfo> lockingProcesses = handler.getLockingProcesses(tempFile);
                assertTrue(lockingProcesses.size() == 1, "getLockingProcesses should return one process");
                // Verify that our process is in the list
                Set<Integer> lockingPids = new HashSet<Integer>();
                for (ProcessInfo pi : lockingProcesses) {
                    lockingPids.add(pi.getPid());
                }
                assertTrue(lockingPids.contains(lockingProcess.getPid()), "Locking process PID " + lockingProcess.getPid() + " should be in the list of locking processes");
                logInfoAnyway("Found " + lockingProcesses.size() + " locking process(es) for file: " + tempFile.getAbsolutePath());
                for (ProcessInfo pi : lockingProcesses) {
                    logInfoAnyway("  - PID: " + pi.getPid() + ", Name: " + pi.getReadableName() + ", Path: " + pi.getExecutablePath());
                }
                // Now unlock the file by terminating the process
                command.destroy();
                // Wait for process to exit
                if (lockingProcess != null) {
                    handler.waitForExit(TimeSpan.parse("5s"), lockingProcess);
                }
                // Wait a bit more to ensure file handles are released
                Thread.sleep(500);
                // Verify that getLockingProcesses now returns empty list
                List<ProcessInfo> lockingProcessesAfterUnlock = handler.getLockingProcesses(tempFile);
                assertTrue(lockingProcessesAfterUnlock.isEmpty(), "getLockingProcesses should return empty list after unlocking file, but returned " + lockingProcessesAfterUnlock.size() + " process(es)");
                logInfoAnyway("Correctly returned empty list after unlocking file: " + tempFile.getAbsolutePath());
            } finally {
                // Ensure process is terminated even if something went wrong
                if (command.getProcess() != null && command.getProcess().isAlive()) {
                    command.destroy();
                }
            }
        } finally {
            // Clean up temp file
            if (tempFile.exists()) {
                tempFile.delete();
            }
        }
    }

    protected void testGetLockingProcessesNoLock(String unique, JNAWindowsProcessHandler handler) throws IOException, InterruptedException, Exception {
        // Create a temporary file that is NOT locked
        File tempFile = File.createTempFile("testnolock-" + unique, ".tmp");
        tempFile.deleteOnExit();
        try {
            // Write some content to the file
            java.nio.file.Files.write(tempFile.toPath(), ("Test content for " + unique).getBytes());
            // Ensure file is closed (not locked by any process)
            // Wait a moment to ensure file handles are released
            Thread.sleep(500);
            // Test getLockingProcesses - should return empty list
            List<ProcessInfo> lockingProcesses = handler.getLockingProcesses(tempFile);
            assertTrue(lockingProcesses.isEmpty(), "getLockingProcesses should return empty list for unlocked file, but returned " + lockingProcesses.size() + " process(es)");
            logInfoAnyway("Correctly returned empty list for unlocked file: " + tempFile.getAbsolutePath());
        } finally {
            // Clean up temp file
            if (tempFile.exists()) {
                tempFile.delete();
            }
        }
    }

    /**
     * AWTest: On Windows, compares timing of listByPath(executablePath) vs listByPids(pid) for the same process. Both use one WMI query and
     * should run roughly equally fast. Asserts that neither is more than 5x slower than the other (ratio between 0.2 and 5). A high ratio
     * indicates listByPath (WHERE ExecutablePath) is disproportionally slow due to Win32_Process provider behaviour.
     *
     * @throws Exception
     */
    protected void testListByPathVsListByPidsTiming(JNAWindowsProcessHandler handler) throws Exception {
        int currentPid = getCurrentPid();
        String javaPath = CrossSystem.getJavaBinary();
        if (javaPath == null || currentPid <= 0) {
            logInfoAnyway("testListByPathVsListByPidsTiming: skip (pid=" + currentPid + ", path=" + javaPath + ")");
            return;
        }
        // Warm-up: one call each so first-call WMI init does not skew only one side
        handler.listByPath(javaPath);
        handler.listByPids(currentPid);
        final int iterations = 5;
        long tPath = System.currentTimeMillis();
        for (int i = 0; i < iterations; i++) {
            handler.listByPath(javaPath);
        }
        tPath = System.currentTimeMillis() - tPath;
        long tPids = System.currentTimeMillis();
        for (int i = 0; i < iterations; i++) {
            handler.listByPids(currentPid);
        }
        tPids = System.currentTimeMillis() - tPids;
        long avgPath = tPath / iterations;
        long avgPids = tPids / iterations;
        logInfoAnyway("listByPath(" + iterations + "x): total " + tPath + " ms, avg " + avgPath + " ms");
        logInfoAnyway("listByPids(" + iterations + "x): total " + tPids + " ms, avg " + avgPids + " ms");
        assertTrue(avgPath >= 0 && avgPids >= 0, "Timing completed");
        if (avgPids > 0 && avgPath > 0) {
            double ratio = (double) avgPath / (double) avgPids;
            logInfoAnyway("listByPath / listByPids ratio: " + String.format("%.2f", ratio));
            assertTrue(ratio >= 0.2 && ratio <= 5.0, "listByPath and listByPids should run roughly equally fast (ratio " + String.format("%.2f", ratio) + " outside 0.2..5)");
        }
    }

    /**
     * @return current JVM process PID from ManagementFactory, or -1 if not available
     */
    private static int getCurrentPid() {
        try {
            String name = ManagementFactory.getRuntimeMXBean().getName();
            int at = name.indexOf('@');
            if (at > 0) {
                return Integer.parseInt(name.substring(0, at));
            }
        } catch (Exception e) {
            // ignore
        }
        return -1;
    }

    public static void main(String[] args) {
        run();
    }
}
