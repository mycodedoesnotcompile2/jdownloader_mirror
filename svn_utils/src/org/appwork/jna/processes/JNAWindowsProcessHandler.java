/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2024, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.jna.processes;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.TimeZone;
import java.util.concurrent.TimeoutException;

import org.appwork.jna.windows.Kernel32Ext;
import org.appwork.jna.wmi.WMIConnector;
import org.appwork.jna.wmi.WMIQueryFailedException;
import org.appwork.loggingv3.LogV3;
import org.appwork.processes.ProcessHandler;
import org.appwork.processes.ProcessInfo;
import org.appwork.utils.Joiner;
import org.appwork.utils.NonInterruptibleRunnable;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.processes.command.Command;
import org.appwork.utils.processes.command.ProcessOutputHandler;

import com.sun.jna.LastErrorException;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Kernel32Util;
import com.sun.jna.platform.win32.Psapi;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.W32Errors;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinUser;
import com.sun.jna.ptr.IntByReference;

/**
 * @author thomas
 * @date 19.11.2024
 *
 */
public class JNAWindowsProcessHandler implements ProcessHandler {
    /**
     * @throws
     * @see org.appwork.jna.processes.ProcessHandler#listByPath(java.lang.String)
     */
    @Override
    public List<ProcessInfo> listByPath(String path) throws IOException {
        ArrayList<ProcessInfo> ret = new ArrayList<ProcessInfo>();
        try {
            final WMIConnector wmi = new WMIConnector();
            try {
                final ArrayList<HashMap<String, Object>> wmiResult = wmi.executeQuery(null, "SELECT ProcessId,CommandLine,CreationDate,ProcessId from Win32_Process where ExecutablePath like '%" + path.replace("\\", "\\\\") + "%'");
                if (wmiResult != null && wmiResult.size() > 0) {
                    for (final HashMap<String, Object> p : wmiResult) {
                        ret.add(wmiResultToProcessInfo(p));
                    }
                }
            } finally {
                wmi.close();
            }
        } catch (TimeoutException e) {
            throw new IOException(e);
        } catch (WMIQueryFailedException e) {
            throw new IOException(e);
        }
        return ret;
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
            fractionalPart = fractionalPart.length() < 3 ? String.format("%-3s", fractionalPart).replace(' ', '0') // Pad to 3 digits
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
     * @see org.appwork.jna.processes.ProcessHandler#listByPids(int[])
     */
    @Override
    public List<ProcessInfo> listByPids(int... pids) throws IOException {
        ArrayList<ProcessInfo> ret = new ArrayList<ProcessInfo>();
        try {
            final WMIConnector wmi = new WMIConnector();
            try {
                String where = new Joiner(" OR ") {
                    protected String elementToString(Object s) {
                        if (s == getSeparator()) {
                            return getSeparator();
                        }
                        return "ProcessId = " + s;
                    };
                }.join(pids);
                final ArrayList<HashMap<String, Object>> wmiResult = wmi.executeQuery(null, "SELECT ProcessId,CommandLine,CreationDate,ProcessId from Win32_Process where " + where);
                if (wmiResult != null && wmiResult.size() > 0) {
                    for (final HashMap<String, Object> p : wmiResult) {
                        ProcessInfo pi = wmiResultToProcessInfo(p);
                        ret.add(pi);
                    }
                }
            } finally {
                wmi.close();
            }
        } catch (TimeoutException e) {
            throw new IOException(e);
        } catch (WMIQueryFailedException e) {
            throw new IOException(e);
        }
        return ret;
    }

    protected ProcessInfo wmiResultToProcessInfo(final HashMap<String, Object> p) {
        ProcessInfo pi = new ProcessInfo(((Number) p.get("ProcessId")).intValue(), (String) p.get("CommandLine"));
        String timestamp = (String) p.get("CreationDate");
        pi.setId(((Number) p.get("ProcessId")).intValue() + "-" + timestamp + (String) p.get("CommandLine"));
        if (StringUtils.isNotEmpty(timestamp)) {
            pi.setCreationTime(parseCreationTime(timestamp));
        }
        return pi;
    }

    public void requestExit(int pid) throws IOException {
        requestExit(new ProcessInfo(pid));
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
                final Command command = new Command(CrossSystem.getJavaBinary(), "-cp", java.lang.management.ManagementFactory.getRuntimeMXBean().getClassPath(), CTRLSender.class.getName(), pid + "");
                ProcessOutputHandler po;
                command.setOutputHandler(po = new ProcessOutputHandler());
                command.start(true);
                command.waitFor();
                LogV3.info(po.getResult().toString());
                if (po.getResult().getExitCode() != 0) {
                    throw new IOException("Could not send CTRL");
                }
                return null;
            }
        }.startAndWait();
    }

    protected void sendWMCloseToWindow(final HWND window) {
        try {
            final int length = User32.INSTANCE.GetWindowTextLength(window);
            final char[] chars = new char[length];
            User32.INSTANCE.GetWindowText(window, chars, length);
            LogV3.info("Close Window Title: " + Native.toString(chars));
        } catch (final RuntimeException e) {
            LogV3.log(e);
        }
        User32.INSTANCE.PostMessage(window, WinUser.WM_CLOSE, new WinDef.WPARAM(0), new WinDef.LPARAM(0));
        final int error = Kernel32.INSTANCE.GetLastError();
        // System.out.println("PostMessage: " + error);
    }

    private String getProcessName(final int pid) {
        // Access rights to query information and read memory
        final int PROCESS_QUERY_INFORMATION = 0x0400;
        final int PROCESS_VM_READ = 0x0010;
        // Open the process with the given PID
        final WinNT.HANDLE processHandle = Kernel32.INSTANCE.OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, false, pid);
        if (processHandle == null) {
            return "Cannot open process with PID " + pid;
        }
        final char[] processName = new char[1024]; // Buffer for the process name
        final int nameLength = Psapi.INSTANCE.GetModuleFileNameExW(processHandle, null, processName, processName.length);
        // Close the process handle
        Kernel32.INSTANCE.CloseHandle(processHandle);
        if (nameLength > 0) {
            return Native.toString(processName);
        } else {
            return "Unable to get process name for PID " + pid;
        }
    }

    /**
     * @param p
     * @throws IOException
     */
    public boolean requestExit(ProcessInfo p) throws IOException {
        final HWND window = findMainWindow(p.getPid());
        LogV3.info("Close " + p + "/" + window);
        if (window != null) {
            if (!StringUtils.isEmpty(p.getId())) {
                // check the internal id
                if (isRunning(p) == 0) {
                    return false;
                }
            } else {
                // we found a window for this pid
            }
            this.sendWMCloseToWindow(window);
        } else {
            if (isRunning(p) == 0) {
                return false;
            }
            this.sendCTRLToPID(p.getPid());
        }
        return true;
    }

    /**
     * @param p
     * @return
     * @throws IOException
     */
    public int isRunning(ProcessInfo... processes) throws IOException {
        List<ProcessInfo> running = listByPids(getPids(processes));
        if (running.size() == 0) {
            return 0;
        }
        HashSet<String> ids = new HashSet<String>();
        HashMap<Integer, ProcessInfo> map = new HashMap<Integer, ProcessInfo>();
        for (ProcessInfo o : processes) {
            if (StringUtils.isNotEmpty(o.getId())) {
                ids.add(o.getId());
            }
            map.put(o.getPid(), o);
        }
        int count = 0;
        for (ProcessInfo pi : running) {
            ProcessInfo fromRequest = map.get(pi.getPid());
            if (StringUtils.isNotEmpty(fromRequest.getId())) {
                if (!fromRequest.getId().equals(pi.getId())) {
                    // do not count. has the same pid, but different id - probably a different process with same pid
                    continue;
                }
            }
            count++;
        }
        return count;
    }

    public List<ProcessInfo> listRunning(ProcessInfo... processes) throws IOException {
        List<ProcessInfo> running = listByPids(getPids(processes));
        if (running.size() == 0) {
            return running;
        }
        HashMap<Integer, ProcessInfo> map = new HashMap<Integer, ProcessInfo>();
        for (ProcessInfo o : processes) {
            map.put(o.getPid(), o);
        }
        ArrayList<ProcessInfo> ret = new ArrayList<ProcessInfo>();
        for (ProcessInfo pi : running) {
            ProcessInfo fromRequest = map.get(pi.getPid());
            if (StringUtils.isNotEmpty(fromRequest.getId())) {
                if (!fromRequest.getId().equals(pi.getId())) {
                    // do not count. has the same pid, but different id - probably a different process with same pid
                    continue;
                }
            }
            ret.add(pi);
        }
        return ret;
    }

    protected int[] getPids(ProcessInfo... processes) {
        int[] res = new int[processes.length];
        int i = 0;
        for (ProcessInfo p : processes) {
            res[i++] = p.getPid();
        }
        return res;
    }

    private void checkForErrors() throws LastErrorException {
        final int error = Kernel32.INSTANCE.GetLastError();
        if (error != 0) {
            LogV3.info("Error: " + error);
            throw new LastErrorException("Error: " + error + " - " + Kernel32Util.formatMessage(W32Errors.HRESULT_FROM_WIN32(error)));
        }
    }

    /**
     * @param instance
     * @throws IOException
     */
    public boolean destroy(ProcessInfo p) throws IOException {
        if (!StringUtils.isEmpty(p.getId())) {
            // check the internal id
            if (isRunning(p) == 0) {
                return false;
            }
        }
        final HANDLE hProcess = Kernel32Ext.INSTANCE.OpenProcess(Kernel32.PROCESS_TERMINATE | WinNT.PROCESS_QUERY_INFORMATION, false, p.getPid());
        try {
            if (hProcess == null) {
                LogV3.info("No Process for this PID:" + p.getPid());
                return false;
            }
            this.checkForErrors();
            LogV3.info("Terminate PID " + p.getPid() + "(" + getProcessName(p.getPid()) + ")");
            IntByReference exitCode = new IntByReference();
            if (Kernel32.INSTANCE.GetExitCodeProcess(hProcess, exitCode)) {
                if (exitCode.getValue() != WinNT.STILL_ACTIVE) {
                    // process already gone
                    LogV3.info("Process already quit:" + p.getPid() + " exitcode: " + exitCode.getValue());
                    return false;
                }
            }
            final boolean success = Kernel32.INSTANCE.TerminateProcess(hProcess, p.getPid());
            LogV3.info("Process Exist Result: " + success);
            final WinNT.HANDLE processHandle = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_INFORMATION, false, p.getPid());
            try {
                if (processHandle == null) {
                    LogV3.info("Process handle gone. Fine");
                    return true;
                } else {
                    // process handler still available check exitcode
                    exitCode = new IntByReference();
                    if (Kernel32.INSTANCE.GetExitCodeProcess(processHandle, exitCode)) {
                        if (exitCode.getValue() == WinNT.STILL_ACTIVE) {
                            throw new IOException("Could not kill process " + p.getPid() + "(" + getProcessName(p.getPid()) + ")");
                        } else {
                            LogV3.info("Process gone. Fine");
                        }
                    } else {
                        LogV3.info("Failed to call GetExitCodeProcess.");
                        if (!success) {
                            throw new IOException("Failed kill process " + p.getPid() + "(" + getProcessName(p.getPid()) + ")");
                        }
                    }
                }
            } finally {
                if (processHandle != null) {
                    Kernel32Ext.INSTANCE.CloseHandle(processHandle);
                }
            }
            // get last error returned 5 for me even the process was terminated fine..
            this.checkForErrors();
        } finally {
            if (hProcess != null) {
                Kernel32Ext.INSTANCE.CloseHandle(hProcess);
            }
        }
        return true;
    }

    /**
     * @throws InterruptedException
     * @see org.appwork.jna.processes.ProcessHandler#requestExit(org.appwork.utils.duration.TimeSpan, org.appwork.processes.ProcessInfo)
     */
    @Override
    public boolean requestExit(TimeSpan timeout, ProcessInfo... proccesses) throws IOException, InterruptedException {
        boolean result = true;
        for (ProcessInfo p : proccesses) {
            fill(p);
            if (!requestExit(p)) {
                result = false;
            }
        }
        LogV3.info("Close requested. Wait Timeout " + timeout);
        List<ProcessInfo> running = waitForExit(timeout, proccesses);
        if (running.size() > 0) {
            int failed = 0;
            LogV3.info("Timeout " + timeout + " expired. Terminate remaining processes");
            for (final ProcessInfo p : running) {
                try {
                    destroy(p);
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
     * @param p
     * @throws IOException
     */
    private void fill(ProcessInfo p) throws IOException {
        if (StringUtils.isEmpty(p.getId())) {
            List<ProcessInfo> pi = listByPids(p.getPid());
            if (pi.size() > 0) {
                p.fill(pi.get(0));
            }
        }
    }

    /**
     * @param instance
     * @param timeSpan
     * @return
     * @throws IOException
     * @throws InterruptedException
     */
    @Override
    public List<ProcessInfo> waitForExit(TimeSpan timeSpan, ProcessInfo... processes) throws IOException, InterruptedException {
        long started = Time.systemIndependentCurrentJVMTimeMillis();
        List<ProcessInfo> running = listRunning(processes);
        while (!timeSpan.isExpired(started)) {
            if (running.size() == 0) {
                return running;
            }
            Thread.sleep(1000);
            running = listRunning(running.toArray(new ProcessInfo[0]));
        }
        return running;
    }
}
