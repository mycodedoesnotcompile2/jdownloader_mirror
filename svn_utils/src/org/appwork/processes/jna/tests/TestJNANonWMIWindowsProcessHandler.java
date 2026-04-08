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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.appwork.app.gui.BasicGui;
import org.appwork.processes.ProcessHandler;
import org.appwork.processes.ProcessInfo;
import org.appwork.processes.windows.jna.JNANonWMIWindowsProcessHandler;
import org.appwork.processes.windows.jna.JNAWindowsProcessHandler;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.AWTestValidateClassReference;
import org.appwork.testframework.TestDependency;
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

/**
 * AWTest for {@link JNANonWMIWindowsProcessHandler}. Uses PID from {@link Command#getPID()} or current process instead of finding by
 * command line (non-WMI handler does not provide CommandLine).
 *
 * @author thomas
 * @date 08.03.2026
 */
@TestDependency({ TestJNANonWMIWindowsProcessHandler.ORG_APPWORK_PROCESSES_JNA_JNANON_WMI_WINDOWS_PROCESS_HANDLER, TestJNANonWMIWindowsProcessHandler.ORG_APPWORK_PROCESSES_JNA_JNA_WINDOWS_PROCESS_HANDLER, TestJNANonWMIWindowsProcessHandler.ORG_APPWORK_APP_GUI_BASIC_GUI, "org.appwork.utils.os.WindowsUtils" })
public class TestJNANonWMIWindowsProcessHandler extends AWTest {
    @AWTestValidateClassReference
    public static final String ORG_APPWORK_APP_GUI_BASIC_GUI                                = "org.appwork.app.gui.BasicGui";
    @AWTestValidateClassReference
    public static final String ORG_APPWORK_PROCESSES_JNA_JNANON_WMI_WINDOWS_PROCESS_HANDLER = "org.appwork.processes.windows.jna.JNANonWMIWindowsProcessHandler";
    @AWTestValidateClassReference
    public static final String ORG_APPWORK_PROCESSES_JNA_JNA_WINDOWS_PROCESS_HANDLER        = "org.appwork.processes.windows.jna.JNAWindowsProcessHandler";

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            return;
        }
        BasicGui.class.getName();
        String unique = "awtest-nonwmi" + UniqueAlltimeID.create();
        JNANonWMIWindowsProcessHandler handler = new JNANonWMIWindowsProcessHandler();
        testListByPidsCurrentProcess(handler);
        testListByPath(handler);
        testListByProcessInfoWithJNAOnly(handler);
        testListByProcessInfoWithNormalOnlyByPid(unique, handler);
        testGetLockingProcesses(unique, handler);
        testGetLockingProcessesNoLock(unique, handler);
        testListByPathVsListByPidsTiming(handler);
        testWmiVsNonWmiSameInfo();
        testKillByPid(unique, handler);
        testSendCloseToWindowByPid(unique, handler);
    }

    protected void testListByPidsCurrentProcess(ProcessHandler handler) throws Exception {
        int currentPid = getCurrentPid();
        if (currentPid <= 0) {
            logInfoAnyway("testListByPidsCurrentProcess: skip (no current pid)");
            return;
        }
        List<ProcessInfo> list = handler.listByPids(currentPid);
        assertTrue(list.size() == 1, "listByPids(currentPid) should return one process");
        ProcessInfo pi = list.get(0);
        assertEquals(currentPid, pi.getPid());
        assertTrue(pi.getExecutablePath() != null && pi.getExecutablePath().length() > 0, "ExecutablePath should be set");
    }

    protected void testListByPath(ProcessHandler handler) throws Exception {
        String javaPath = CrossSystem.getJavaBinary();
        if (javaPath == null) {
            logInfoAnyway("testListByPath: skip (no java path)");
            return;
        }
        List<ProcessInfo> list = handler.listByPath(javaPath);
        assertTrue(list != null && list.size() >= 1, "listByPath(java) should return at least one process");
    }

    protected void testListByProcessInfoWithJNAOnly(ProcessHandler handler) throws Exception {
        String marker = "awtest-jnaonly-nonwmi-" + UniqueAlltimeID.create();
        JNAProcessInfo jna = null;
        try {
            jna = startProcessWithHandle(new String[] { "cmd", "/c", "ping localhost -n 30 -" + marker }, null);
            assertTrue(jna.getPid() > 0);
            List<ProcessInfo> running = handler.listByProcessInfo(jna);
            assertTrue(running.size() == 1, "listByProcessInfo(JNAProcessInfo) should return one when still running");
            assertEquals(jna.getPid(), running.get(0).getPid());
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
     * Test listByProcessInfo with a normal ProcessInfo identified by PID (from Command.getPID()). Non-WMI does not provide command line so
     * we cannot find by command line; we start a process and get PID from Command.
     */
    protected void testListByProcessInfoWithNormalOnlyByPid(String unique, ProcessHandler handler) throws Exception {
        Command c = new Command("cmd", "/c", "ping localhost -n 30 -" + unique);
        c.setOutputHandler(new AbstractLineHandler() {
            @Override
            public void handleLine(String line, Object caller) {
            }
        });
        c.start(true);
        try {
            long pidLong = -1;
            try {
                pidLong = c.getPID();
            } catch (Exception e) {
                if (e instanceof org.appwork.utils.os.NotSupportedException) {
                    logInfoAnyway("testListByProcessInfoWithNormalOnlyByPid: skip getPID not supported");
                    return;
                }
                throw e;
            }
            int pid = (int) pidLong;
            ProcessInfo found = new ProcessInfo(pid);
            List<ProcessInfo> running = handler.listByProcessInfo(found);
            assertTrue(running.size() == 1, "listByProcessInfo should return one running process");
            assertEquals(pid, running.get(0).getPid());
            handler.terminateForced(running.get(0), 0);
            List<ProcessInfo> after = handler.listByProcessInfo(found);
            assertTrue(after.isEmpty(), "Process should be gone");
        } finally {
            if (c.getProcess() != null && c.getProcess().isAlive()) {
                c.getProcess().destroyForcibly();
            }
        }
    }

    protected void testGetLockingProcesses(String unique, JNANonWMIWindowsProcessHandler handler) throws IOException, InterruptedException, Exception {
        File tempFile = File.createTempFile("testlock-" + unique, ".tmp");
        tempFile.deleteOnExit();
        try {
            final Command command = new Command(CrossSystem.getJavaBinary(), "-cp", java.lang.management.ManagementFactory.getRuntimeMXBean().getClassPath(), TestFileLock.class.getName(), tempFile.getAbsolutePath(), unique);
            command.setOutputHandler(new AbstractLineHandler() {
                @Override
                public void handleLine(String line, Object caller) {
                    System.out.println(caller + ":" + line);
                }
            });
            command.start(true);
            try {
                long pidLong;
                try {
                    pidLong = command.getPID();
                } catch (Exception e) {
                    if (e instanceof org.appwork.utils.os.NotSupportedException) {
                        logInfoAnyway("testGetLockingProcesses: skip getPID not supported");
                        return;
                    }
                    throw e;
                }
                int lockingPid = (int) pidLong;
                Thread.sleep(2000);
                List<ProcessInfo> lockingProcesses = handler.getLockingProcesses(tempFile);
                assertTrue(lockingProcesses.size() == 1, "getLockingProcesses should return one process");
                Set<Integer> lockingPids = new HashSet<Integer>();
                for (ProcessInfo pi : lockingProcesses) {
                    lockingPids.add(Integer.valueOf(pi.getPid()));
                }
                assertTrue(lockingPids.contains(Integer.valueOf(lockingPid)), "Locking process PID " + lockingPid + " should be in the list");
                command.destroy();
                if (lockingPid > 0) {
                    ProcessInfo p = new ProcessInfo(lockingPid);
                    handler.waitForExit(TimeSpan.parse("5s"), p);
                }
                Thread.sleep(500);
                List<ProcessInfo> afterUnlock = handler.getLockingProcesses(tempFile);
                assertTrue(afterUnlock.isEmpty(), "getLockingProcesses should return empty after unlock");
            } finally {
                if (command.getProcess() != null && command.getProcess().isAlive()) {
                    command.destroy();
                }
            }
        } finally {
            if (tempFile.exists()) {
                tempFile.delete();
            }
        }
    }

    protected void testGetLockingProcessesNoLock(String unique, JNANonWMIWindowsProcessHandler handler) throws IOException, InterruptedException, Exception {
        File tempFile = File.createTempFile("testnolock-" + unique, ".tmp");
        tempFile.deleteOnExit();
        try {
            java.nio.file.Files.write(tempFile.toPath(), ("Test content for " + unique).getBytes());
            Thread.sleep(500);
            List<ProcessInfo> lockingProcesses = handler.getLockingProcesses(tempFile);
            assertTrue(lockingProcesses.isEmpty(), "getLockingProcesses should return empty for unlocked file");
        } finally {
            if (tempFile.exists()) {
                tempFile.delete();
            }
        }
    }

    protected void testListByPathVsListByPidsTiming(JNANonWMIWindowsProcessHandler handler) throws Exception {
        int currentPid = getCurrentPid();
        String javaPath = CrossSystem.getJavaBinary();
        if (javaPath == null || currentPid <= 0) {
            logInfoAnyway("testListByPathVsListByPidsTiming: skip (pid=" + currentPid + ", path=" + javaPath + ")");
            return;
        }
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
        logInfoAnyway("non-WMI listByPath(" + iterations + "x): total " + tPath + " ms, avg " + avgPath + " ms");
        logInfoAnyway("non-WMI listByPids(" + iterations + "x): total " + tPids + " ms, avg " + avgPids + " ms");
        assertTrue(avgPath >= 0 && avgPids >= 0, "Timing completed");
    }

    /**
     * Compares WMI and non-WMI handler for the same process: both must return the same pid, executablePath, executableName, parentPid,
     * creationTime, and command line (after getCommandLine() on non-WMI). Ensures both variants provide identical information for the same
     * process.
     */
    protected void testWmiVsNonWmiSameInfo() throws Exception {
        int currentPid = getCurrentPid();
        if (currentPid <= 0) {
            logInfoAnyway("testWmiVsNonWmiSameInfo: skip (no current pid)");
            return;
        }
        JNAWindowsProcessHandler wmiHandler = new JNAWindowsProcessHandler();
        JNANonWMIWindowsProcessHandler nonWmiHandler = new JNANonWMIWindowsProcessHandler();
        List<ProcessInfo> wmiList = wmiHandler.listByPids(currentPid);
        List<ProcessInfo> nonWmiList = nonWmiHandler.listByPids(currentPid);
        assertTrue(wmiList.size() == 1, "WMI listByPids(currentPid) should return one process");
        assertTrue(nonWmiList.size() == 1, "non-WMI listByPids(currentPid) should return one process");
        ProcessInfo wmi = wmiList.get(0);
        ProcessInfo nonWmi = nonWmiList.get(0);
        assertEquals(wmi.getPid(), nonWmi.getPid(), "pid");
        String normWmiPath = wmi.getExecutablePath() != null ? JNANonWMIWindowsProcessHandler.normalizeExecutablePathForCompare(wmi.getExecutablePath()) : null;
        String normNonWmiPath = nonWmi.getExecutablePath() != null ? JNANonWMIWindowsProcessHandler.normalizeExecutablePathForCompare(nonWmi.getExecutablePath()) : null;
        assertEquals(normWmiPath, normNonWmiPath, "executablePath (normalized)");
        assertEquals(wmi.getExecutableName(), nonWmi.getExecutableName(), "executableName");
        assertEquals(wmi.getParentPid(), nonWmi.getParentPid(), "parentPid");
        assertEquals(wmi.getCreationTime(), nonWmi.getCreationTime(), "creationTime");
        assertTrue(wmi.getId() != null && wmi.getId().length() > 0, "WMI id should be set");
        assertTrue(nonWmi.getId() != null && nonWmi.getId().length() > 0, "non-WMI id should be set");
        String wmiCmd = wmi.getCommandLine();
        String nonWmiCmd = nonWmi.getCommandLine();
        if (wmiCmd == null && nonWmiCmd == null) {
            // both null is ok
        } else if (wmiCmd != null && nonWmiCmd != null) {
            assertEquals(wmiCmd.trim(), nonWmiCmd.trim(), "commandLine (after getCommandLine())");
        } else {
            assertTrue(false, "commandLine: WMI=" + (wmiCmd != null) + " nonWMI=" + (nonWmiCmd != null) + " should both be null or both non-null");
        }
    }

    protected void testKillByPid(String unique, ProcessHandler handler) throws IOException, Exception {
        final Command command = new Command(CrossSystem.getJavaBinary(), "-cp", java.lang.management.ManagementFactory.getRuntimeMXBean().getClassPath(), TestApp.class.getName(), "-" + unique);
        command.setOutputHandler(new AbstractLineHandler() {
            @Override
            public void handleLine(String line, Object caller) {
                System.out.println(caller + ":" + line);
            }
        });
        command.start(true);
        try {
            long pidLong = command.getPID();
            int pid = (int) pidLong;
            ProcessInfo instance = new ProcessInfo(pid);
            List<ProcessInfo> list = handler.listByPids(pid);
            assertTrue(list.size() == 1, "listByPids(pid) should return one");
            instance = list.get(0);
            assertTrue(handler.terminateForced(instance, -1));
            if (handler.waitForExit(TimeSpan.parse("10s"), instance).size() > 0) {
                throw new Exception("Could not kill");
            }
        } finally {
            command.destroy();
        }
    }

    protected void testSendCloseToWindowByPid(String unique, ProcessHandler handler) throws IOException, Exception {
        JNANonWMIWindowsProcessHandler nonWmiHandler = (JNANonWMIWindowsProcessHandler) handler;
        final Command command = new Command(CrossSystem.getJavaBinary(), "-cp", java.lang.management.ManagementFactory.getRuntimeMXBean().getClassPath(), TestApp.class.getName(), "-" + unique);
        final boolean[] received = new boolean[] { false };
        command.setOutputHandler(new AbstractLineHandler() {
            @Override
            public void handleLine(String line, Object caller) {
                AWTest.logInfoAnyway(caller + " :" + line);
                if (line.contains("Received Request Exit")) {
                    received[0] = true;
                }
            }
        });
        command.start(true);
        long pidLong = command.getPID();
        int pid = (int) pidLong;
        ProcessInfo instance = new ProcessInfo(pid);
        long started = Time.systemIndependentCurrentJVMTimeMillis();
        while (Time.systemIndependentCurrentJVMTimeMillis() - started < 10000) {
            com.sun.jna.platform.win32.WinDef.HWND window = nonWmiHandler.findMainWindow(pid);
            if (window != null) {
                break;
            }
            Thread.sleep(1000);
        }
        handler.terminateRequest(instance);
        Thread.sleep(2000);
        assertTrue(received[0], "Should have received WM_CLOSE");
        List<ProcessInfo> remaining = handler.waitForExit(TimeSpan.parse("10s"), instance);
        if (remaining.size() > 0) {
            throw new Exception("Could not kill: " + remaining);
        }
    }

    public static JNAProcessInfo startProcessWithHandle(String[] command, String workingDir) throws Win32Exception {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("This operation is only supported on Windows");
        }
        if (command == null || command.length == 0) {
            throw new IllegalArgumentException("Command cannot be null or empty");
        }
        String cmdLine = ShellParser.createCommandLine(Style.WINDOWS, command);
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
