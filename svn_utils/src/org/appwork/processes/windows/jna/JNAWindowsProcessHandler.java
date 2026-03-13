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

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.appwork.jna.processes.CTRLSender;
import org.appwork.jna.windows.Kernel32Ext;
import org.appwork.jna.windows.User32Ext;
import org.appwork.jna.windows.interfaces.RM_PROCESS_INFO;
import org.appwork.jna.windows.interfaces.Rstrtmgr;
import org.appwork.jna.windows.wmi.JNAWMIUtils;
import org.appwork.jna.windows.wmi.WMIException;
import org.appwork.loggingv3.LogV3;
import org.appwork.processes.ProcessHandler;
import org.appwork.processes.ProcessInfo;
import org.appwork.processes.RestartManagerApplicationType;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Joiner;
import org.appwork.utils.NonInterruptibleRunnable;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.JNAProcessInfo;
import org.appwork.utils.os.WindowsUtils;
import org.appwork.utils.os.NotSupportedException;
import org.appwork.utils.processes.command.Command;
import org.appwork.utils.processes.command.ProcessOutputHandler;

import com.sun.jna.LastErrorException;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Kernel32Util;
import com.sun.jna.platform.win32.Psapi;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.W32Errors;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinBase.FILETIME;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinDef.LPARAM;
import com.sun.jna.platform.win32.WinDef.WPARAM;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;
import com.sun.jna.platform.win32.WinUser;
import com.sun.jna.ptr.IntByReference;

/**
 * Windows ProcessHandler using WMI (Win32_Process) for listing. When a {@link org.appwork.utils.os.JNAProcessInfo} is passed (e.g. from startElevatedProcess), the handler uses
 * its internal handle instead of PID for wait, terminate, and "is running" checks, so the correct process is targeted even if the PID was
 * reused.
 * <p>
 * WMI (Win32_Process) is available since Windows 2000. This handler is used by {@link org.appwork.processes.ProcessHandlerFactory} on
 * Windows XP and earlier (Vista+ uses {@link JNANonWMIWindowsProcessHandler}). {@code getLockingProcesses} requires Vista+ and throws
 * NotSupportedException on older Windows.
 * </p>
 *
 * @author thomas
 * @date 19.11.2024
 *
 */
public class JNAWindowsProcessHandler implements ProcessHandler {
    /**
     *
     */
    private static final String[] REQUIRED_PROPERTIES = new String[] { "ProcessId", "CommandLine", "CreationDate", "ExecutablePath", "Name", "ParentProcessId" };
    /**
     * Windows error code: ERROR_MORE_DATA - The buffer is too small for the requested data.
     */
    private static final int      ERROR_MORE_DATA     = 234;

    /** Enable verbose timing logs when system property logs.verbose.JNAWindowsProcessHandler=true */
    private static final boolean VERBOSE_LOGS        = Boolean.getBoolean("logs.verbose.JNAWindowsProcessHandler");

    /**
     * Returns processes whose ExecutablePath equals the given path (or all processes if path is null).
     * On Windows this uses WMI Win32_Process; the first enumeration step often takes ~1s per query even
     * for 0 results. Callers that need multiple paths should call listByPath(null) once and filter by path
     * in memory instead of calling this method per path.
     *
     * @throws InterruptedException
     * @see org.appwork.jna.processes.ProcessHandler#listByPath(java.lang.String)
     */
    @Override
    public List<ProcessInfo> listByPath(String path) throws IOException, InterruptedException {
        final long tStart = System.currentTimeMillis();
        if (VERBOSE_LOGS) {
            LogV3.info("[listByPath] START path=" + (path == null ? "null" : path));
        }
        try {
            String where = "";
            if (path != null) {
                where = " where ExecutablePath = '" + JNAWMIUtils.escape(path) + "'";
            }
            ArrayList<ProcessInfo> ret = new ArrayList<ProcessInfo>();
            final long tQuery = System.currentTimeMillis();
            final List<Map<String, Object>> wmiResult = JNAWMIUtils.query(null, "SELECT " + new Joiner(",").join(REQUIRED_PROPERTIES) + " from Win32_Process" + where);
            if (VERBOSE_LOGS) {
                LogV3.info("[listByPath] JNAWMIUtils.query took " + (System.currentTimeMillis() - tQuery) + " ms, rows=" + (wmiResult == null ? 0 : wmiResult.size()));
            }
            if (wmiResult != null && wmiResult.size() > 0) {
                final long tConvert = System.currentTimeMillis();
                for (final Map<String, Object> p : wmiResult) {
                    ret.add(wmiResultToProcessInfo(p));
                }
                if (VERBOSE_LOGS) {
                    LogV3.info("[listByPath] wmiResultToProcessInfo loop took " + (System.currentTimeMillis() - tConvert) + " ms for " + wmiResult.size() + " rows");
                }
            }
            if (VERBOSE_LOGS) {
                LogV3.info("[listByPath] END total " + (System.currentTimeMillis() - tStart) + " ms");
            }
            return ret;
        } catch (WMIException e) {
            throw new IOException(e);
        }
    }


    @Override
    public List<ProcessInfo> getLockingProcesses(File file) throws IOException, NotSupportedException, InterruptedException {
        // Restart Manager is only available on Windows Vista and later
        if (!CrossSystem.getOS().isMinimum(CrossSystem.OperatingSystem.WINDOWS_VISTA)) {
            throw new NotSupportedException("getLockingProcesses requires Windows Vista or later. Current OS: " + CrossSystem.getOS());
        }
        IntByReference session = new IntByReference();
        char[] key = UUID.randomUUID().toString().toCharArray();
        int res = Rstrtmgr.INSTANCE.RmStartSession(session, 0, key);
        if (res != 0) {
            String errorMessage = Kernel32Util.formatMessage(W32Errors.HRESULT_FROM_WIN32(res));
            throw new IOException("RmStartSession failed for file '" + file + "': " + errorMessage);
        }
        try {
            WString[] files = { new WString(file.getAbsolutePath()) };
            res = Rstrtmgr.INSTANCE.RmRegisterResources(session.getValue(), 1, files, 0, null, 0, null);
            if (res != 0) {
                String errorMessage = Kernel32Util.formatMessage(W32Errors.HRESULT_FROM_WIN32(res));
                throw new IOException("RmRegisterResources failed for file '" + file + "': " + errorMessage);
            }
            IntByReference needed = new IntByReference();
            IntByReference count = new IntByReference(0);
            RM_PROCESS_INFO[] list = new RM_PROCESS_INFO[1];
            // First call: get required buffer size
            res = Rstrtmgr.INSTANCE.RmGetList(session.getValue(), needed, count, list, new IntByReference());
            if (res == ERROR_MORE_DATA) {
                int size = needed.getValue();
                list = (RM_PROCESS_INFO[]) new RM_PROCESS_INFO().toArray(size);
                count.setValue(size);
                // Second call: actually retrieve list
                res = Rstrtmgr.INSTANCE.RmGetList(session.getValue(), needed, count, list, new IntByReference());
            }
            if (res != 0) {
                String errorMessage = Kernel32Util.formatMessage(W32Errors.HRESULT_FROM_WIN32(res));
                throw new IOException("RmGetList failed for file '" + file + "': " + errorMessage);
            }
            ArrayList<ProcessInfo> ret = new ArrayList<ProcessInfo>();
            for (int i = 0; i < count.getValue(); i++) {
                int pid = list[i].Process.dwProcessId;
                ProcessInfo pi = new ProcessInfo(pid);
                String appName = Native.toString(list[i].strAppName);
                int appType = list[i].ApplicationType;
                pi.setReadableName(appName);
                pi.setApplicationType(RestartManagerApplicationType.fromValue(appType));
                ret.add(pi);
            }
            return ret;
        } finally {
            try {
                Rstrtmgr.INSTANCE.RmEndSession(session.getValue());
            } catch (Exception e) {
                LogV3.log(e);
            }
        }
    }

    /**
     * Extracts the offset in minutes from the extra information (e.g., +060).
     */
    private static int extractOffsetInMinutes(String timestamp) {
        int extraIndex = timestamp.lastIndexOf('+'); // Find the position of '+'
        if (extraIndex == -1) {
            extraIndex = timestamp.lastIndexOf('-'); // Or find '-'
        }
        if (extraIndex != -1) {
            String offsetString = timestamp.substring(extraIndex);
            try {
                // Convert the offset (e.g., "+060") to minutes
                int sign = offsetString.startsWith("-") ? -1 : 1;
                int offsetMinutes = Integer.parseInt(offsetString.substring(1)); // Parse the numeric part
                return sign * offsetMinutes;
            } catch (NumberFormatException e) {
                System.err.println("Invalid offset: " + offsetString);
            }
        }
        return 0; // Default to 0 minutes if no offset is found
    }

    /**
     * Removes the extra information (e.g., +060) from the timestamp.
     */
    private static String removeExtraInfo(String timestamp) {
        int extraIndex = timestamp.lastIndexOf('+'); // Find the position of '+'
        if (extraIndex == -1) {
            extraIndex = timestamp.lastIndexOf('-'); // Or find '-'
        }
        return (extraIndex != -1) ? timestamp.substring(0, extraIndex) : timestamp;
    }

    /**
     * Extracts the microseconds and converts them to milliseconds.
     */
    private static int extractMilliseconds(String timestamp) {
        if (timestamp.contains(".")) {
            String fractionalPart = timestamp.split("\\.")[1];
            int tzIndex = Math.max(fractionalPart.indexOf('+'), fractionalPart.indexOf('-'));
            if (tzIndex != -1) {
                fractionalPart = fractionalPart.substring(0, tzIndex); // Isolate the microseconds
            }
            // Trim to the first 3 digits (milliseconds) or pad with 0s if fewer digits
            fractionalPart = fractionalPart.length() < 3 ? String.format(Locale.ROOT, "%-3s", fractionalPart).replace(' ', '0') // Pad to 3
                                                                                                                                // digits
                    : fractionalPart.substring(0, 3); // Trim to 3 digits
            return Integer.parseInt(fractionalPart); // Convert to an integer
        }
        return 0; // Default to 0 if no microseconds are present
    }

    /**
     * @param timestamp
     * @return
     */
    private long parseCreationTime(String timestamp) {
        try {
            // Extract the offset in minutes from the extra information
            int offsetInMinutes = extractOffsetInMinutes(timestamp);
            // Remove the extra information from the timestamp
            String correctedTimestamp = removeExtraInfo(timestamp);
            // Extract microseconds and convert them to milliseconds
            int milliseconds = extractMilliseconds(timestamp);
            // Parse the timestamp without microseconds or extra information
            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
            dateFormat.setTimeZone(TimeZone.getTimeZone("GMT+" + (offsetInMinutes / 60))); // Use UTC as the base timezone
            Date parsedDate = dateFormat.parse(correctedTimestamp);
            // Calculate the final Unix timestamp (milliseconds since 1970)
            long unixTimestamp = parsedDate.getTime() + milliseconds;
            return unixTimestamp;
        } catch (ParseException e) {
            return -1;
        }
    }

    /**
     * @throws InterruptedException
     * @see org.appwork.jna.processes.ProcessHandler#listByPids(int[])
     */
    @Override
    public List<ProcessInfo> listByPids(int... pids) throws IOException, InterruptedException {
        try {
            final ArrayList<ProcessInfo> ret = new ArrayList<ProcessInfo>();
            if (pids.length == 0) {
                return ret;
            }
            final String where = new Joiner(" OR ") {
                protected String elementToString(Object s) {
                    if (s == getSeparator()) {
                        return getSeparator();
                    }
                    return "ProcessId = " + s;
                };
            }.join(pids);
            // TODO: rewrite to use
            // final WinNT.HANDLE processHandle = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_INFORMATION, false, pid);
            // Psapi.INSTANCE.GetModuleFileNameExW(processHandle, null, processName, processName.length);
            final List<Map<String, Object>> wmiResult = JNAWMIUtils.query(null, "SELECT " + new Joiner(",").join(REQUIRED_PROPERTIES) + " from Win32_Process where " + where, REQUIRED_PROPERTIES);
            if (wmiResult != null && wmiResult.size() > 0) {
                for (final Map<String, Object> p : wmiResult) {
                    ProcessInfo pi = wmiResultToProcessInfo(p);
                    ret.add(pi);
                }
            }
            return ret;
        } catch (WMIException e) {
            throw new IOException(e);
        }
    }

    protected ProcessInfo wmiResultToProcessInfo(final Map<String, Object> p) {
        ProcessInfo pi = new ProcessInfo(((Number) p.get("ProcessId")).intValue(), (String) p.get("CommandLine"));
        String timestamp = (String) p.get("CreationDate");
        pi.setId(((Number) p.get("ProcessId")).intValue() + "-" + timestamp + (String) p.get("CommandLine"));
        pi.setExecutablePath((String) p.get("ExecutablePath"));
        pi.setExecutableName((String) p.get("Name"));
        if (p.get("ParentProcessId") != null) {
            pi.setParentPid(((Number) p.get("ParentProcessId")).intValue());
        }
        if (StringUtils.isNotEmpty(timestamp)) {
            pi.setCreationTime(parseCreationTime(timestamp));
        }
        return pi;
    }

    public WinDef.HWND findMainWindow(final int targetPID) {
        final WinDef.HWND[] foundWindow = { null };
        User32.INSTANCE.EnumWindows(new WinUser.WNDENUMPROC() {
            @Override
            public boolean callback(final WinDef.HWND hWnd, final Pointer arg1) {
                final IntByReference processId = new IntByReference();
                User32.INSTANCE.GetWindowThreadProcessId(hWnd, processId);
                if (processId.getValue() == targetPID && User32.INSTANCE.IsWindowVisible(hWnd)) {
                    final WinDef.HWND owner = User32.INSTANCE.GetWindow(hWnd, new DWORD(WinUser.GW_OWNER));
                    if (owner == null) {
                        foundWindow[0] = hWnd;
                        return false;
                    }
                }
                return true;
            }
        }, null);
        return foundWindow[0];
    }

    protected void sendCTRLToPID(final int pid) throws IOException {
        new NonInterruptibleRunnable<Void, IOException>() {
            @Override
            public Void run() throws IOException, InterruptedException {
                LogV3.info("Execute CTRL Sender for PID " + pid);
                final Command command = new Command(CrossSystem.getJavaBinary(), "-cp", java.lang.management.ManagementFactory.getRuntimeMXBean().getClassPath(), CTRLSender.class.getName(), pid + "");
                ProcessOutputHandler po;
                command.setOutputHandler(po = new ProcessOutputHandler());
                command.start(true);
                command.waitFor();
                LogV3.info(po.getResult().toString());
                if (po.getResult().getExitCode() == CTRLSender.PROCESS_NOT_FOUND) {
                    LogV3.info("Process Not Found");
                } else if (po.getResult().getExitCode() != 0) {
                    throw new IOException("Could not send CTRL: " + po.getResult().getExitCode());
                }
                return null;
            }
        }.startAndWait();
    }

    protected void sendWMCloseToWindow(final HWND window) {
        try {
            final int length = User32.INSTANCE.GetWindowTextLength(window);
            if (length > 0) {
                // terminator
                final char[] chars = new char[length + 1];
                User32.INSTANCE.GetWindowText(window, chars, length + 1);
                LogV3.info("Close Window with Title: " + Native.toString(chars));
            }
        } catch (final RuntimeException e) {
            LogV3.log(e);
        }
        LogV3.info("Send WM_CLOSE to Window");
        User32.INSTANCE.PostMessage(window, WinUser.WM_CLOSE, new WinDef.WPARAM(0), new WinDef.LPARAM(0));
        // final int error = Kernel32.INSTANCE.GetLastError();
        // System.out.println("PostMessage: " + error);
    }

    protected boolean isValid(WinNT.HANDLE handle) {
        return handle != null && !handle.equals(WinBase.INVALID_HANDLE_VALUE);
    }

    /**
     * Enables SeDebugPrivilege on the current process token so that OpenProcess can be used on processes running under
     * other security contexts (e.g. LocalSystem). Caller must restore the returned previous state and close the token.
     *
     * @param hToken
     *            process token with TOKEN_ADJUST_PRIVILEGES
     * @return previous TOKEN_PRIVILEGES state to pass to AdjustTokenPrivileges to restore, or null if enabling failed
     */
    protected WinNT.TOKEN_PRIVILEGES enableSeDebugPrivilege(HANDLE hToken) {
        final WinNT.LUID luid = new WinNT.LUID();
        if (!Advapi32.INSTANCE.LookupPrivilegeValue(null, "SeDebugPrivilege", luid)) {
            return null;
        }
        final WinNT.TOKEN_PRIVILEGES tp = new WinNT.TOKEN_PRIVILEGES(1);
        tp.Privileges[0] = new WinNT.LUID_AND_ATTRIBUTES(luid, new DWORD(WinNT.SE_PRIVILEGE_ENABLED));
        final WinNT.TOKEN_PRIVILEGES previousState = new WinNT.TOKEN_PRIVILEGES(1);
        final IntByReference returnLength = new IntByReference();
        if (!Advapi32.INSTANCE.AdjustTokenPrivileges(hToken, false, tp, tp.size(), previousState, returnLength)) {
            return null;
        }
        return previousState;
    }

    public long getProcessCreationTime(final int pid) {
        final WinNT.HANDLE processHandle = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_LIMITED_INFORMATION, false, pid);
        if (!isValid(processHandle)) {
            return -1;
        }
        try {
            FILETIME lpCreationTime = new FILETIME();
            FILETIME lpExitTime = new FILETIME();
            FILETIME lpKernelTime = new FILETIME();
            FILETIME lpUserTime = new FILETIME();
            if (!Kernel32.INSTANCE.GetProcessTimes(processHandle, lpCreationTime, lpExitTime, lpKernelTime, lpUserTime)) {
                checkForErrors();
                return -1;
            }
            return lpCreationTime.toTime();
        } finally {
            Kernel32.INSTANCE.CloseHandle(processHandle);
        }
    }

    public String getProcessName(final int pid) {
        final WinNT.HANDLE processHandle = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_INFORMATION | WinNT.PROCESS_VM_READ, false, pid);
        if (!isValid(processHandle)) {
            return "Cannot open process with PID " + pid;
        }
        try {
            final char[] processName = new char[1024]; // Buffer for the process name
            final int nameLength = Psapi.INSTANCE.GetModuleFileNameExW(processHandle, null, processName, processName.length);
            // Close the process handle
            if (nameLength > 0) {
                return Native.toString(processName);
            } else {
                return "Unable to get process name for PID " + pid;
            }
        } finally {
            Kernel32.INSTANCE.CloseHandle(processHandle);
        }
    }

    /**
     * Tries to terminate the process gracefully (WM_CLOSE / CTRL). Uses {@link #listByProcessInfo(ProcessInfo[])} for "is running" checks,
     * so for {@link JNAProcessInfo} the internal handle is used rather than PID.
     *
     * @param p
     *            process to terminate
     * @return true if a close was sent (window or CTRL)
     * @throws IOException
     */
    public boolean terminateRequest(ProcessInfo p) throws IOException {
        final HWND window = findMainWindow(p.getPid());
        if (isValid(window)) {
            LogV3.info("Close " + p + " WindowHandle: " + window);
            try {
                if (!StringUtils.isEmpty(p.getId())) {
                    // check the internal id
                    if (isRunning(p) == false) {
                        LogV3.info("Process with ID " + p.getId() + " is not running. Exit");
                        return false;
                    }
                } else {
                    // we found a window for this pid
                }
                this.sendWMCloseToWindow(window);
            } finally {
                Kernel32.INSTANCE.CloseHandle(window);
            }
            return true;
        } else {
            if (!StringUtils.isEmpty(p.getId())) {
                // check the internal id
                if (isRunning(p) == false) {
                    LogV3.info("Process with ID " + p.getId() + " is not running. Exit");
                    return false;
                }
            }
            LogV3.info("No Window Found");
            try {
                this.sendCTRLToPID(p.getPid());
            } catch (IOException e) {
                if (!StringUtils.isEmpty(p.getId())) {
                    if (isRunning(p) == false) {
                        return true;
                    }
                }
                throw e;
            }
            return true;
        }
    }

    /**
     * Returns a list of those given processes that are still running. The method returns <b>new</b> {@link ProcessInfo} instances with the
     * same fields filled for all elements: for {@link JNAProcessInfo}, "still running" is determined via the instance's handle
     * (GetExitCodeProcess), then full info is fetched via PID. At most one {@link #listByPids(int[])} call is made for the union of all
     * required PIDs. If {@code listByPids} returns nothing for a PID, that process is not included in the result (no partial instance).
     *
     * @throws InterruptedException
     * @see org.appwork.processes.ProcessHandler#listByProcessInfo(org.appwork.processes.ProcessInfo[])
     */
    @Override
    public List<ProcessInfo> listByProcessInfo(ProcessInfo... processes) throws IOException, InterruptedException {
        List<Integer> jnaStillRunningPids = new ArrayList<Integer>();
        List<ProcessInfo> others = new ArrayList<ProcessInfo>();
        for (ProcessInfo p : processes) {
            if (p instanceof JNAProcessInfo) {
                if (isProcessStillRunning(((JNAProcessInfo) p).getHandle())) {
                    jnaStillRunningPids.add(Integer.valueOf(p.getPid()));
                }
            } else {
                others.add(p);
            }
        }
        HashSet<Integer> allPidsSet = new HashSet<Integer>(jnaStillRunningPids);
        for (ProcessInfo o : others) {
            allPidsSet.add(Integer.valueOf(o.getPid()));
        }
        if (allPidsSet.isEmpty()) {
            return new ArrayList<ProcessInfo>();
        }
        int[] pids = new int[allPidsSet.size()];
        int i = 0;
        for (Integer pid : allPidsSet) {
            pids[i++] = pid.intValue();
        }
        final List<ProcessInfo> fromWmi = listByPids(pids);
        HashMap<Integer, ProcessInfo> pidToInfo = new HashMap<Integer, ProcessInfo>();
        for (ProcessInfo pi : fromWmi) {
            pidToInfo.put(Integer.valueOf(pi.getPid()), pi);
        }
        List<ProcessInfo> jnaStillRunning = new ArrayList<ProcessInfo>();
        for (Integer pid : jnaStillRunningPids) {
            ProcessInfo pi = pidToInfo.get(pid);
            if (pi != null) {
                jnaStillRunning.add(pi);
            }
        }
        HashMap<Integer, ProcessInfo> othersByPid = new HashMap<Integer, ProcessInfo>();
        for (ProcessInfo o : others) {
            othersByPid.put(Integer.valueOf(o.getPid()), o);
        }
        ArrayList<ProcessInfo> ret = new ArrayList<ProcessInfo>(jnaStillRunning);
        for (ProcessInfo pi : fromWmi) {
            ProcessInfo fromRequest = othersByPid.get(Integer.valueOf(pi.getPid()));
            if (fromRequest == null) {
                continue;
            }
            if (StringUtils.isNotEmpty(fromRequest.getId())) {
                if (!fromRequest.getId().equals(pi.getId())) {
                    continue;
                }
            }
            ret.add(pi);
        }
        return ret;
    }

    /**
     * Check if process is still running using its handle (GetExitCodeProcess == STILL_ACTIVE).
     *
     * @param h
     *            process handle
     * @return true if process has not exited yet
     */
    private boolean isProcessStillRunning(HANDLE h) {
        if (h == null) {
            return false;
        }
        IntByReference exitRef = new IntByReference();
        if (!Kernel32.INSTANCE.GetExitCodeProcess(h, exitRef)) {
            return false;
        }
        return exitRef.getValue() == WinNT.STILL_ACTIVE;
    }

    /**
     * Quick check via JNA whether a process with the given PID still exists and is running (GetExitCodeProcess == STILL_ACTIVE).
     * Does not verify process identity (e.g. id); use only when ProcessInfo has no id set.
     *
     * @param pid
     *            process id
     * @return true if the process exists and is still active
     */
    private boolean isPidAlive(int pid) {
        if (pid <= 0) {
            return false;
        }
        WinNT.HANDLE h = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_LIMITED_INFORMATION, false, pid);
        if (!isValid(h)) {
            return false;
        }
        try {
            IntByReference exitRef = new IntByReference();
            if (!Kernel32.INSTANCE.GetExitCodeProcess(h, exitRef)) {
                return false;
            }
            return exitRef.getValue() == WinNT.STILL_ACTIVE;
        } finally {
            Kernel32.INSTANCE.CloseHandle(h);
        }
    }

    protected int[] getPids(ProcessInfo... processes) {
        int[] res = new int[processes.length];
        int i = 0;
        for (ProcessInfo p : processes) {
            res[i++] = p.getPid();
        }
        return res;
    }

    protected void checkForErrors() throws LastErrorException {
        final int error = Kernel32.INSTANCE.GetLastError();
        if (error != 0) {
            LogV3.info("Error: " + error);
            throw new LastErrorException("Error: " + error + " - " + Kernel32Util.formatMessage(W32Errors.HRESULT_FROM_WIN32(error)));
        }
    }

    /**
     * Terminates the process with TerminateProcess. For {@link JNAProcessInfo}, uses the instance's handle directly (no OpenProcess by PID)
     * and closes the handle after termination. For other {@link ProcessInfo}, opens by PID and uses WMI/"is running" by PID.
     *
     * @param p
     *            process to kill
     * @param exitCode
     *            exit code to set
     * @return true if termination was successful
     * @throws IOException
     */
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
            // check the internal id
            if (!isRunning(p)) {
                return false;
            }
        }
        final int access = WinNT.PROCESS_QUERY_INFORMATION | Kernel32.PROCESS_TERMINATE;
        HANDLE hProcess = Kernel32Ext.INSTANCE.OpenProcess(access, false, p.getPid());
        HANDLE hToken = null;
        WinNT.TOKEN_PRIVILEGES previousPrivState = null;
        if (!isValid(hProcess)) {
            // Retry with SeDebugPrivilege (e.g. to terminate LocalSystem or other elevated processes)
            final HANDLEByReference phToken = new HANDLEByReference();
            if (Advapi32.INSTANCE.OpenProcessToken(Kernel32.INSTANCE.GetCurrentProcess(), WinNT.TOKEN_ADJUST_PRIVILEGES | WinNT.TOKEN_QUERY, phToken)) {
                hToken = phToken.getValue();
                previousPrivState = enableSeDebugPrivilege(hToken);
                if (previousPrivState != null) {
                    hProcess = Kernel32Ext.INSTANCE.OpenProcess(access, false, p.getPid());
                }
            }
        }
        if (!isValid(hProcess)) {
            if (hToken != null) {
                Kernel32.INSTANCE.CloseHandle(hToken);
            }
            // User may also not have PROCESS_TERMINATE rights
            LogV3.info("No Process for this PID:" + p.getPid());
            return false;
        }
        try {
            final String processName = getProcessName(p.getPid());
            LogV3.info("Terminate PID " + p.getPid() + "(" + processName + ")");
            final IntByReference processExitCode = new IntByReference();
            if (Kernel32.INSTANCE.GetExitCodeProcess(hProcess, processExitCode)) {
                if (processExitCode.getValue() != WinNT.STILL_ACTIVE) {
                    // process already gone
                    LogV3.info("Process already quit:" + p.getPid() + " exitcode: " + processExitCode.getValue());
                    return false;
                }
            } else {
                checkForErrors();
            }
            final boolean success = Kernel32.INSTANCE.TerminateProcess(hProcess, exitCode);
            LogV3.info("Process Exist Result: " + success);
            // process handler still available check exitcode
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
            Kernel32Ext.INSTANCE.CloseHandle(hProcess);
        }
        return true;
    }

    /**
     * Whether any of the given processes are still running. Delegates to {@link #listByProcessInfo(ProcessInfo[])}, so for
     * {@link JNAProcessInfo} the handle is used (GetExitCodeProcess), not PID.
     */
    protected boolean isRunning(ProcessInfo... proccesses) throws IOException {
        try {
            return listByProcessInfo(proccesses).size() > 0;
        } catch (IOException e) {
            LogV3.log(e);
        } catch (InterruptedException e) {
            LogV3.log(e);
        }
        return false;
    }

    /**
     * Requests graceful shutdown, then waits for processes to exit, then force-terminates any remaining. For {@link JNAProcessInfo},
     * {@link #fill(ProcessInfo)} is skipped and wait/terminate use the handle.
     *
     * @throws InterruptedException
     * @see org.appwork.jna.processes.ProcessHandler#terminateForcedAfterRequest(org.appwork.utils.duration.TimeSpan, int,
     *      org.appwork.processes.ProcessInfo)
     */
    @Override
    public boolean terminateForcedAfterRequest(TimeSpan timeout, int exitCode, ProcessInfo... proccesses) throws IOException, InterruptedException {
        boolean result = true;
        if (proccesses.length == 0) {
            return false;
        }
        for (ProcessInfo p : proccesses) {
            fill(p);
            try {
                if (terminateRequest(p)) {
                    result = true;
                }
            } catch (IOException e) {
                // swallow - we might be successfull with forced
                LogV3.log(e);
            }
        }
        LogV3.info("Close requested. Wait Timeout " + timeout);
        final List<ProcessInfo> running = waitForExit(timeout, proccesses);
        LogV3.info("Done. Remaining: " + running.size());
        if (running.size() > 0) {
            int failed = 0;
            LogV3.info("Timeout " + timeout + " expired. Terminate remaining processes");
            for (final ProcessInfo p : running) {
                try {
                    if (terminateForced(p, exitCode)) {
                        result = true;
                    }
                } catch (final IOException e) {
                    LogV3.log(e);
                    failed++;
                }
            }
            if (failed > 0) {
                throw new IOException("Failed to exit at least " + failed + " processes");
            }
        }
        return result;
    }

    /**
     * Fills process metadata (id, commandLine, etc.) from WMI when {@code id} is empty. Skips for {@link JNAProcessInfo} (data is already
     * set by the caller, e.g. startElevatedProcess).
     *
     * @param p
     *            process to fill
     * @throws IOException
     * @throws InterruptedException
     */
    private void fill(ProcessInfo p) throws IOException, InterruptedException {
        if (p instanceof JNAProcessInfo) {
            return;
        }
        if (StringUtils.isEmpty(p.getId())) {
            List<ProcessInfo> pi = listByPids(p.getPid());
            if (pi.size() > 0) {
                p.fill(pi.get(0));
            }
        }
    }

    /**
     * Waits until the given processes have exited or the timeout expires. For {@link JNAProcessInfo}, waits using the instance's handle
     * (WaitForSingleObject) and closes the handle when the process exits; for other {@link ProcessInfo}, uses
     * {@link #listByProcessInfo(ProcessInfo[])} by PID. Returns the list of processes still running after the wait (new instances for
     * non-JNA, same references for JNA that are still running).
     *
     * @param timeSpan
     *            maximum time to wait (null = wait until all exit)
     * @param processes
     *            processes to wait for
     * @return list of processes still running (may be new instances)
     * @throws IOException
     * @throws InterruptedException
     */
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
        while (timeSpan == null || !timeSpan.isExpired(started)) {
            List<ProcessInfo> stillWithHandle = new ArrayList<ProcessInfo>();
            List<ProcessInfo> noHandle = new ArrayList<ProcessInfo>();
            for (ProcessInfo p : running) {
                if (p instanceof JNAProcessInfo) {
                    try {
                        Integer exit = waitForHandle(((JNAProcessInfo) p).getHandle(), 1000);
                        if (exit != null) {
                            p.setExitCode(exit);
                            ((JNAProcessInfo) p).close();
                        } else {
                            stillWithHandle.add(p);
                        }
                    } catch (InterruptedException e) {
                        throw e;
                    }
                } else {
                    noHandle.add(p);
                }
            }
            if (stillWithHandle.isEmpty() && noHandle.isEmpty()) {
                return new ArrayList<ProcessInfo>();
            }
            if (noHandle.isEmpty()) {
                running = stillWithHandle;
            } else {
                List<ProcessInfo> stillRunningByPid = listByProcessInfo(noHandle.toArray(new ProcessInfo[0]));
                running = new ArrayList<ProcessInfo>(stillWithHandle);
                running.addAll(stillRunningByPid);
            }
            Thread.sleep(1000);
        }
        return running;
    }

    /**
     * Wait for process to exit using handle. Polls with WaitForSingleObject and returns exit code when process exited.
     *
     * @param h
     *            process handle
     * @param timeoutMillis
     *            0 = no wait, &lt;0 = infinite, &gt;0 = milliseconds
     * @return exit code if process exited; null if timeout
     */
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

    /**
     * @throws InterruptedException
     * @throws IOException
     * @see org.appwork.processes.ProcessHandler#toFront(java.util.ArrayList)
     */
    @Override
    public int toFront(ProcessInfo... processes) throws IOException, InterruptedException {
        if (processes.length == 0) {
            return 0;
        }
        final HashSet<Integer> set = new HashSet<Integer>();
        // update pids based on the extended process ids
        for (ProcessInfo p : listByProcessInfo(processes)) {
            set.add(p.getPid());
        }
        // note: Windows tries to block automated focus changed, thus all of the following is kind of a workaround
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
                        // tested 11.03.25 with windows 11 23h2
                        count[0]++;
                        LogV3.info("Bring Window to Front: PID: " + pid.getValue());
                        // allow the process to set the active window
                        if (!User32Ext.INSTANCE.AllowSetForegroundWindow(pid.getValue())) {
                            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                                System.err.println("Failed to allow foreground for PID " + pid.getValue());
                            }
                        }
                        int currentThreadId = Kernel32.INSTANCE.GetCurrentThreadId();
                        int targetThreadId = User32Ext.INSTANCE.GetWindowThreadProcessId(hWnd, null);
                        boolean attached = false;
                        try {
                            attached = User32Ext.INSTANCE.AttachThreadInput(currentThreadId, targetThreadId, true);
                            if (!attached) {
                                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                                    System.err.println("Failed to attach thread input.");
                                }
                            }
                            // tested 11.03.25 with windows 11 23h2 minimize -> restore is what actuallay worked today.
                            // minimize window
                            User32Ext.INSTANCE.ShowWindow(hWnd, WinUser.SW_MINIMIZE);
                            // restore window
                            User32Ext.INSTANCE.ShowWindow(hWnd, WinUser.SW_RESTORE);
                            // Send WM_SYSCOMMAND/SC_RESTORE to restore the window
                            User32Ext.INSTANCE.SendMessage(hWnd, WinUser.WM_SYSCOMMAND, new WPARAM(WinUser.SW_RESTORE), new LPARAM(0));
                            // try to bring the window to front
                            if (!User32Ext.INSTANCE.SetForegroundWindow(hWnd)) {
                                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                                    System.err.println("Failed to set foreground window.");
                                }
                            }
                            // try show window
                            User32Ext.INSTANCE.ShowWindow(hWnd, WinUser.SW_SHOW);
                        } finally {
                            if (attached) {
                                if (!User32Ext.INSTANCE.AttachThreadInput(currentThreadId, targetThreadId, false)) {
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

    /**
     * Returns true if all given processes are still running; false otherwise. Performs early exit: as soon as one process
     * is dead, returns false. Uses fast JNA checks where possible: {@link JNAProcessInfo} via handle
     * (GetExitCodeProcess); ProcessInfo with only PID (no id) via OpenProcess+GetExitCodeProcess. Only for ProcessInfo
     * with id set (need to verify same process) a single {@link #listByProcessInfo(ProcessInfo[])} call is used and
     * results are matched by id.
     *
     * @see org.appwork.processes.ProcessHandler#isProcessesAlive(org.appwork.processes.ProcessInfo[])
     */
    @Override
    public boolean isProcessesAlive(ProcessInfo... infos) {
        if (infos == null || infos.length == 0) {
            return true;
        }
        List<ProcessInfo> needIdCheck = new ArrayList<ProcessInfo>();
        for (ProcessInfo p : infos) {
            if (p == null) {
                continue;
            }
            if (p instanceof JNAProcessInfo) {
                if (!isProcessStillRunning(((JNAProcessInfo) p).getHandle())) {
                    return false;
                }
                continue;
            }
            if (StringUtils.isEmpty(p.getId())) {
                if (!isPidAlive(p.getPid())) {
                    return false;
                }
                continue;
            }
            needIdCheck.add(p);
        }
        if (needIdCheck.isEmpty()) {
            return true;
        }
        try {
            List<ProcessInfo> stillRunning = listByProcessInfo(needIdCheck.toArray(new ProcessInfo[needIdCheck.size()]));
            return stillRunning.size() == needIdCheck.size();
        } catch (IOException e) {
            return false;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return false;
        }
    }
}
