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
package org.appwork.processes.windows.jna;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.appwork.jna.windows.wmi.JNAWMIUtils;
import org.appwork.jna.windows.wmi.WMIException;
import org.appwork.loggingv3.LogV3;
import org.appwork.processes.CommandLineLoader;
import org.appwork.processes.LazyProcessInfo;
import org.appwork.processes.ProcessInfo;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.os.JNAProcessInfo;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.WindowsUtilsKernel32;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Kernel32Util;
import com.sun.jna.platform.win32.Tlhelp32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.W32Errors;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinDef.LPARAM;
import com.sun.jna.platform.win32.WinDef.WPARAM;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinUser;
import com.sun.jna.ptr.IntByReference;

/**
 * Windows ProcessHandler that implements the full {@link ProcessHandler} interface without using WMI for listing.
 * Extends {@link JNAWindowsProcessHandler} and overrides only the WMI-based methods: {@link #listByPath(String)},
 * {@link #listByPids(int...)}, {@link #waitForExit(TimeSpan, ProcessInfo...)}, {@link #terminateForced(ProcessInfo, int)},
 * and {@link #toFront(ProcessInfo...)} (Vista+ APIs). Uses Toolhelp32 + QueryFullProcessImageNameW + GetProcessTimes for
 * listing; Restart Manager for file locks is inherited; terminate/wait/toFront use Vista+ Kernel32/User32 where needed.
 * <p>
 * <b>Windows version:</b> Requires Windows Vista or later. The following APIs used here are not available on Windows XP:
 * {@code QueryFullProcessImageNameW}, {@code PROCESS_QUERY_LIMITED_INFORMATION}, Restart Manager (RmStartSession etc.).
 * {@link ProcessHandlerFactory#getProcessHandler()} returns {@link JNAWindowsProcessHandler} (WMI) on Windows XP and
 * earlier, and this handler on Vista+.
 * </p>
 * Use this when WMI is slow or undesired; use {@link JNAWindowsProcessHandler} when CommandLine or WMI-based listing is required.
 * <p>
 * <b>Limitations compared to {@link JNAWindowsProcessHandler} (WMI):</b>
 * <ul>
 * <li><b>CommandLine</b> – By default {@code null}; no WMI at list time. When using {@link LazyProcessInfo}
 * (default), {@link ProcessInfo#getCommandLine()} loads the command line on first access: on Windows 8.1+ via
 * {@link NtProcessCommandLine} (NtQueryInformationProcess, no WMI); otherwise via WMI (Win32_Process).</li>
 * <li><b>Process id</b> – Set to {@code pid + "-" + creationTime + "-" + normalizedExecutablePath} (from GetProcessTimes and
 * QueryFullProcessImageNameW). This is more unique than {@code pid + creationTime} alone when multiple processes share the same
 * executable; for identity checks (e.g. "same process still running") this matches the same process instance.</li>
 * </ul>
 * All other behaviour is inherited from {@link JNAWindowsProcessHandler}.
 *
 * @author thomas
 * @date 08.03.2026
 */
public class JNANonWMIWindowsProcessHandler extends JNAWindowsProcessHandler {

    /** Loader for command line on demand via WMI (used by {@link LazyProcessInfo}). */
    private final CommandLineLoader commandLineLoader = new CommandLineLoader() {
        @Override
        public String getCommandLine(int pid) throws IOException, InterruptedException {
            String cmd = NtProcessCommandLine.getCommandLine(pid);
            if (cmd != null) {
                return cmd;
            }
            try {
                List<Map<String, Object>> rows = JNAWMIUtils.query(null, "SELECT CommandLine FROM Win32_Process WHERE ProcessId = " + pid, "CommandLine");
                if (rows != null && rows.size() > 0) {
                    Object v = rows.get(0).get("CommandLine");
                    return v != null ? v.toString() : null;
                }
                return null;
            } catch (WMIException e) {
                throw new IOException(e);
            }
        }
    };

    // ---------- WMI overrides: listByPath, listByPids (Toolhelp32 + QueryFullProcessImageNameW) ----------

    @Override
    public List<ProcessInfo> listByPath(String path) throws IOException, InterruptedException {
        final long tStart = System.currentTimeMillis();
        LogV3.info("[listByPath non-WMI] START path=" + (path == null ? "null" : path));
        List<ProcessInfo> ret = enumerateProcesses(path);
        LogV3.info("[listByPath non-WMI] END total " + (System.currentTimeMillis() - tStart) + " ms, count=" + ret.size());
        return ret;
    }

    @Override
    public List<ProcessInfo> listByPids(int... pids) throws IOException, InterruptedException {
        if (pids == null || pids.length == 0) {
            return new ArrayList<ProcessInfo>();
        }
        Map<Integer, Integer> pidToParent = buildPidToParentMap();
        ArrayList<ProcessInfo> ret = new ArrayList<ProcessInfo>();
        for (int pid : pids) {
            ProcessInfo pi = getProcessInfoForPid(pid, pidToParent);
            if (pi != null) {
                ret.add(pi);
            }
        }
        return ret;
    }

    /**
     * Normalize path for set comparison. Delegates to {@link WindowsUtils#normalizeExecutablePathForCompare(String)}.
     */
    public static String normalizeExecutablePathForCompare(String path) {
        return WindowsUtils.normalizeExecutablePathForCompare(path);
    }

    private List<ProcessInfo> enumerateProcesses(String pathFilter) throws IOException, InterruptedException {
        String normalizedFilter = pathFilter != null ? WindowsUtils.normalizeExecutablePathForCompare(pathFilter) : null;
        boolean filterAll = (normalizedFilter != null && normalizedFilter.length() == 0);
        HANDLE snapshot = Kernel32.INSTANCE.CreateToolhelp32Snapshot(Tlhelp32.TH32CS_SNAPPROCESS, new DWORD(0));
        if (snapshot == null || WinBase.INVALID_HANDLE_VALUE.equals(snapshot)) {
            return new ArrayList<ProcessInfo>();
        }
        ArrayList<ProcessInfo> result = new ArrayList<ProcessInfo>();
        try {
            Tlhelp32.PROCESSENTRY32.ByReference pe = new Tlhelp32.PROCESSENTRY32.ByReference();
            pe.dwSize = new DWORD(pe.size());
            if (!Kernel32.INSTANCE.Process32First(snapshot, pe)) {
                return result;
            }
            do {
                int pid = pe.th32ProcessID.intValue();
                String fullPath = getProcessExecutableFullPath(pid);
                if (fullPath == null || fullPath.length() == 0) {
                    continue;
                }
                if (pathFilter != null && !filterAll) {
                    String normalized = WindowsUtils.normalizeExecutablePathForCompare(fullPath);
                    if (!normalized.equals(normalizedFilter)) {
                        continue;
                    }
                }
                ProcessInfo pi = new LazyProcessInfo(pid, commandLineLoader);
                pi.setExecutablePath(fullPath);
                pi.setExecutableName(Native.toString(pe.szExeFile));
                pi.setParentPid(pe.th32ParentProcessID.intValue());
                long creationTime = getProcessCreationTime(pid);
                if (creationTime >= 0) {
                    pi.setCreationTime(creationTime);
                }
                String normalizedPath = WindowsUtils.normalizeExecutablePathForCompare(fullPath);
                pi.setId(creationTime >= 0 ? (pid + "-" + creationTime + "-" + normalizedPath) : (pid + "-" + normalizedPath));
                result.add(pi);
            } while (Kernel32.INSTANCE.Process32Next(snapshot, pe));
        } finally {
            Kernel32.INSTANCE.CloseHandle(snapshot);
        }
        return result;
    }

    private Map<Integer, Integer> buildPidToParentMap() {
        Map<Integer, Integer> map = new HashMap<Integer, Integer>();
        HANDLE snapshot = Kernel32.INSTANCE.CreateToolhelp32Snapshot(Tlhelp32.TH32CS_SNAPPROCESS, new DWORD(0));
        if (snapshot == null || WinBase.INVALID_HANDLE_VALUE.equals(snapshot)) {
            return map;
        }
        try {
            Tlhelp32.PROCESSENTRY32.ByReference pe = new Tlhelp32.PROCESSENTRY32.ByReference();
            pe.dwSize = new DWORD(pe.size());
            if (!Kernel32.INSTANCE.Process32First(snapshot, pe)) {
                return map;
            }
            do {
                map.put(Integer.valueOf(pe.th32ProcessID.intValue()), Integer.valueOf(pe.th32ParentProcessID.intValue()));
            } while (Kernel32.INSTANCE.Process32Next(snapshot, pe));
        } finally {
            Kernel32.INSTANCE.CloseHandle(snapshot);
        }
        return map;
    }

    private ProcessInfo getProcessInfoForPid(int pid, Map<Integer, Integer> pidToParent) {
        String fullPath = getProcessExecutableFullPath(pid);
        if (fullPath == null || fullPath.length() == 0) {
            return null;
        }
        ProcessInfo pi = new LazyProcessInfo(pid, commandLineLoader);
        pi.setExecutablePath(fullPath);
        int last = fullPath.replace('/', '\\').lastIndexOf('\\');
        pi.setExecutableName(last >= 0 ? fullPath.substring(last + 1) : fullPath);
        if (pidToParent != null) {
            Integer parent = pidToParent.get(Integer.valueOf(pid));
            if (parent != null) {
                pi.setParentPid(parent.intValue());
            }
        }
        long creationTime = getProcessCreationTime(pid);
        if (creationTime >= 0) {
            pi.setCreationTime(creationTime);
        }
        String normalizedPath = WindowsUtils.normalizeExecutablePathForCompare(fullPath);
        pi.setId(creationTime >= 0 ? (pid + "-" + creationTime + "-" + normalizedPath) : (pid + "-" + normalizedPath));
        return pi;
    }

    private static String getProcessExecutableFullPath(int pid) {
        HANDLE h = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_LIMITED_INFORMATION, false, pid);
        if (h == null || WinBase.INVALID_HANDLE_VALUE.equals(h)) {
            return null;
        }
        try {
            char[] buf = new char[WinBase.MAX_PATH];
            IntByReference size = new IntByReference(buf.length);
            if (!WindowsUtilsKernel32.INSTANCE.QueryFullProcessImageNameW(h, 0, buf, size)) {
                return null;
            }
            return new String(buf, 0, size.getValue()).trim();
        } finally {
            Kernel32.INSTANCE.CloseHandle(h);
        }
    }

    // ---------- waitForExit override (uses temp handles for non-JNA processes on Vista+) ----------

    @Override
    public List<ProcessInfo> waitForExit(final TimeSpan timeSpan, ProcessInfo... processes) throws IOException, InterruptedException {
        if (processes.length == 0) {
            return new ArrayList<ProcessInfo>();
        }
        final long started = Time.systemIndependentCurrentJVMTimeMillis();
        List<ProcessInfo> running = new ArrayList<ProcessInfo>();
        for (ProcessInfo p : processes) {
            running.add(p);
        }
        final Map<ProcessInfo, HANDLE> tempHandles = new HashMap<ProcessInfo, HANDLE>();
        final int waitAccess = WinNT.PROCESS_QUERY_LIMITED_INFORMATION | WinNT.SYNCHRONIZE;
        for (ProcessInfo p : running) {
            if (p instanceof JNAProcessInfo) {
                continue;
            }
            HANDLE h = Kernel32.INSTANCE.OpenProcess(waitAccess, false, p.getPid());
            if (h != null && !WinBase.INVALID_HANDLE_VALUE.equals(h)) {
                IntByReference exitRef = new IntByReference();
                if (Kernel32.INSTANCE.GetExitCodeProcess(h, exitRef)) {
                    if (exitRef.getValue() != WinNT.STILL_ACTIVE) {
                        p.setExitCode(Integer.valueOf(exitRef.getValue()));
                        Kernel32.INSTANCE.CloseHandle(h);
                        continue;
                    }
                }
                tempHandles.put(p, h);
            }
        }
        try {
            while (timeSpan == null || !timeSpan.isExpired(started)) {
                long remainingMs = timeSpan == null ? Long.MAX_VALUE : Math.max(0, timeSpan.toMillis() - (Time.systemIndependentCurrentJVMTimeMillis() - started));
                int pollMs = remainingMs >= Integer.MAX_VALUE ? 1000 : (int) Math.min(1000, remainingMs);

                List<ProcessInfo> stillWithHandle = new ArrayList<ProcessInfo>();
                List<ProcessInfo> withTempHandle = new ArrayList<ProcessInfo>();
                List<ProcessInfo> noHandle = new ArrayList<ProcessInfo>();
                for (ProcessInfo p : running) {
                    if (p instanceof JNAProcessInfo) {
                        try {
                            Integer exit = waitForHandle(((JNAProcessInfo) p).getHandle(), pollMs);
                            if (exit != null) {
                                p.setExitCode(exit);
                                ((JNAProcessInfo) p).close();
                            } else {
                                stillWithHandle.add(p);
                            }
                        } catch (InterruptedException e) {
                            throw e;
                        }
                    } else if (tempHandles.containsKey(p)) {
                        HANDLE h = tempHandles.get(p);
                        try {
                            Integer exit = waitForHandle(h, pollMs);
                            if (exit != null) {
                                p.setExitCode(exit);
                                Kernel32.INSTANCE.CloseHandle(h);
                                tempHandles.remove(p);
                            } else {
                                withTempHandle.add(p);
                            }
                        } catch (InterruptedException e) {
                            throw e;
                        }
                    } else {
                        noHandle.add(p);
                    }
                }
                if (stillWithHandle.isEmpty() && withTempHandle.isEmpty() && noHandle.isEmpty()) {
                    return new ArrayList<ProcessInfo>();
                }
                if (noHandle.isEmpty()) {
                    running = new ArrayList<ProcessInfo>(stillWithHandle);
                    running.addAll(withTempHandle);
                } else {
                    List<ProcessInfo> stillRunningByPid = listByProcessInfo(noHandle.toArray(new ProcessInfo[0]));
                    running = new ArrayList<ProcessInfo>(stillWithHandle);
                    running.addAll(withTempHandle);
                    running.addAll(stillRunningByPid);
                }
                if (remainingMs <= 0) {
                    break;
                }
                long sleepMs = Math.min(1000, remainingMs);
                if (sleepMs > 0) {
                    Thread.sleep(sleepMs);
                }
            }
            return running;
        } finally {
            for (HANDLE h : tempHandles.values()) {
                if (h != null && !WinBase.INVALID_HANDLE_VALUE.equals(h)) {
                    Kernel32.INSTANCE.CloseHandle(h);
                }
            }
        }
    }

    private Integer waitForHandle(HANDLE h, long timeoutMillis) throws InterruptedException {
        final int pollMs = 200;
        final boolean infinite = (timeoutMillis < 0);
        final long start = System.nanoTime();
        final long deadlineNanos = infinite ? Long.MAX_VALUE : start + TimeUnit.MILLISECONDS.toNanos(timeoutMillis);
        while (true) {
            long now = System.nanoTime();
            long remainingNanos = deadlineNanos - now;
            if (!infinite && remainingNanos <= 0) {
                return null;
            }
            int thisWaitMs = infinite ? pollMs : (int) Math.max(1, Math.min(pollMs, TimeUnit.NANOSECONDS.toMillis(remainingNanos)));
            int res = Kernel32.INSTANCE.WaitForSingleObject(h, thisWaitMs);
            if (res == WinBase.WAIT_OBJECT_0) {
                IntByReference exitRef = new IntByReference();
                if (!Kernel32.INSTANCE.GetExitCodeProcess(h, exitRef)) {
                    return null;
                }
                return Integer.valueOf(exitRef.getValue());
            } else if (res == WinError.WAIT_TIMEOUT) {
                if (Thread.interrupted()) {
                    throw new InterruptedException("Interrupted while waiting for process");
                }
            } else {
                return null;
            }
        }
    }

    // ---------- terminateForced override (Vista+ Kernel32: OpenProcess/CloseHandle) ----------

    @Override
    public boolean terminateForced(ProcessInfo p, final int exitCode) throws IOException {
        if (p instanceof JNAProcessInfo) {
            JNAProcessInfo jna = (JNAProcessInfo) p;
            try {
                boolean ok = Kernel32.INSTANCE.TerminateProcess(jna.getHandle(), exitCode);
                jna.close();
                return ok;
            } catch (Exception e) {
                jna.close();
                if (e instanceof IOException) {
                    throw (IOException) e;
                }
                throw new IOException(e);
            }
        }
        if (!StringUtils.isEmpty(p.getId())) {
            if (!isRunning(p)) {
                return false;
            }
        }
        final int access = WinNT.PROCESS_QUERY_INFORMATION | Kernel32.PROCESS_TERMINATE;
        HANDLE hProcess = Kernel32ForJNANonWMIWindowsProcessHandler.INSTANCE.OpenProcess(access, false, p.getPid());
        HANDLE hToken = null;
        WinNT.TOKEN_PRIVILEGES previousPrivState = null;
        if (!isValid(hProcess)) {
            final WinNT.HANDLEByReference phToken = new WinNT.HANDLEByReference();
            if (Advapi32.INSTANCE.OpenProcessToken(Kernel32.INSTANCE.GetCurrentProcess(), WinNT.TOKEN_ADJUST_PRIVILEGES | WinNT.TOKEN_QUERY, phToken)) {
                hToken = phToken.getValue();
                previousPrivState = enableSeDebugPrivilege(hToken);
                if (previousPrivState != null) {
                    hProcess = Kernel32ForJNANonWMIWindowsProcessHandler.INSTANCE.OpenProcess(access, false, p.getPid());
                }
            }
        }
        if (!isValid(hProcess)) {
            if (hToken != null) {
                Kernel32.INSTANCE.CloseHandle(hToken);
            }
            LogV3.info("No Process for this PID:" + p.getPid());
            return false;
        }
        try {
            final String processName = getProcessName(p.getPid());
            LogV3.info("Terminate PID " + p.getPid() + "(" + processName + ")");
            final IntByReference processExitCode = new IntByReference();
            if (Kernel32.INSTANCE.GetExitCodeProcess(hProcess, processExitCode)) {
                if (processExitCode.getValue() != WinNT.STILL_ACTIVE) {
                    LogV3.info("Process already quit:" + p.getPid() + " exitcode: " + processExitCode.getValue());
                    return false;
                }
            } else {
                checkForErrors();
            }
            final boolean success = Kernel32.INSTANCE.TerminateProcess(hProcess, exitCode);
            LogV3.info("Process Exist Result: " + success);
            if (Kernel32.INSTANCE.GetExitCodeProcess(hProcess, processExitCode)) {
                if (processExitCode.getValue() == WinNT.STILL_ACTIVE) {
                    throw new IOException("Could not kill process " + p.getPid() + "(" + processName + ")");
                } else {
                    LogV3.info("Process gone. Fine");
                }
            } else {
                LogV3.info("Failed to call GetExitCodeProcess.");
                if (!success) {
                    throw new IOException("Failed kill process " + p.getPid() + "(" + processName + ")");
                }
            }
        } finally {
            if (hToken != null && previousPrivState != null) {
                Advapi32.INSTANCE.AdjustTokenPrivileges(hToken, false, previousPrivState, previousPrivState.size(), null, null);
            }
            if (hToken != null) {
                Kernel32.INSTANCE.CloseHandle(hToken);
            }
            Kernel32ForJNANonWMIWindowsProcessHandler.INSTANCE.CloseHandle(hProcess);
        }
        return true;
    }

    // ---------- toFront override (Vista+ User32) ----------

    @Override
    public int toFront(ProcessInfo... processes) throws IOException, InterruptedException {
        if (processes.length == 0) {
            return 0;
        }
        final java.util.HashSet<Integer> set = new java.util.HashSet<Integer>();
        for (ProcessInfo p : listByProcessInfo(processes)) {
            set.add(p.getPid());
        }
        final Thread th = Thread.currentThread();
        final int[] count = new int[] { 0 };
        User32.INSTANCE.EnumWindows(new WinUser.WNDENUMPROC() {
            @Override
            public boolean callback(HWND hWnd, Pointer data) {
                if (th.isInterrupted()) {
                    return false;
                }
                if (User32.INSTANCE.IsWindowVisible(hWnd)) {
                    IntByReference pid = new IntByReference();
                    User32.INSTANCE.GetWindowThreadProcessId(hWnd, pid);
                    if (set.remove(pid.getValue())) {
                        count[0]++;
                        LogV3.info("Bring Window to Front: PID: " + pid.getValue());
                        if (!User32ForJNANonWMIWindowsProcessHandler.INSTANCE.AllowSetForegroundWindow(pid.getValue())) {
                            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                                System.err.println("Failed to allow foreground for PID " + pid.getValue());
                            }
                        }
                        int currentThreadId = Kernel32.INSTANCE.GetCurrentThreadId();
                        int targetThreadId = User32ForJNANonWMIWindowsProcessHandler.INSTANCE.GetWindowThreadProcessId(hWnd, null);
                        boolean attached = false;
                        try {
                            attached = User32ForJNANonWMIWindowsProcessHandler.INSTANCE.AttachThreadInput(currentThreadId, targetThreadId, true);
                            if (!attached) {
                                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                                    System.err.println("Failed to attach thread input.");
                                }
                            }
                            User32ForJNANonWMIWindowsProcessHandler.INSTANCE.ShowWindow(hWnd, WinUser.SW_MINIMIZE);
                            User32ForJNANonWMIWindowsProcessHandler.INSTANCE.ShowWindow(hWnd, WinUser.SW_RESTORE);
                            User32ForJNANonWMIWindowsProcessHandler.INSTANCE.SendMessage(hWnd, WinUser.WM_SYSCOMMAND, new WPARAM(WinUser.SW_RESTORE), new LPARAM(0));
                            if (!User32ForJNANonWMIWindowsProcessHandler.INSTANCE.SetForegroundWindow(hWnd)) {
                                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                                    System.err.println("Failed to set foreground window.");
                                }
                            }
                            User32ForJNANonWMIWindowsProcessHandler.INSTANCE.ShowWindow(hWnd, WinUser.SW_SHOW);
                        } finally {
                            if (attached) {
                                if (!User32ForJNANonWMIWindowsProcessHandler.INSTANCE.AttachThreadInput(currentThreadId, targetThreadId, false)) {
                                    if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                                        System.err.println("Failed to detach thread input.");
                                    }
                                }
                            }
                        }
                    }
                }
                return set.size() > 0;
            }
        }, null);
        if (Thread.interrupted()) {
            throw new InterruptedException();
        }
        return count[0];
    }
}
