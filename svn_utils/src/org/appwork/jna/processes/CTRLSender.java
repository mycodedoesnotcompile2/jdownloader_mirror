/**
 *
 */
package org.appwork.jna.processes;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.appwork.jna.windows.JNAOptions;
import org.appwork.jna.windows.Kernel32Ext;
import org.appwork.loggingv3.LogV3;
import org.appwork.loggingv3.simple.SimpleLoggerFactory;
import org.appwork.loggingv3.simple.sink.LogToStdOutSink;
import org.appwork.utils.Application;
import org.appwork.utils.os.CrossSystem;

import com.sun.jna.Native;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Tlhelp32;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinBase.FILETIME;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.StdCallLibrary;

/**
 * Helper main class that sends CTRL+C (soft kill: the process gets the chance to shut down regularly) to the console of another process.
 * It runs in its own JVM, because a process can only be attached to one console and has to give up its own console for that.
 * <p>
 * GenerateConsoleCtrlEvent reaches EVERY process attached to the console. To never terminate foreign applications, CTRL+C is only sent if
 * all processes on the console may receive it (see {@link #getAllowedProcesses(int, long)}): the target, its descendants and, if the caller
 * started the target, the processes between caller and target (e.g. {@code cmd /c ping}: cmd and ping). The caller itself never receives
 * it. Otherwise nothing is sent and {@link #FOREIGN_PROCESS_ON_CONSOLE} is returned.
 * <p>
 * Known residual risk: a foreign process can still join the console in the microseconds between listing the console's processes and
 * GenerateConsoleCtrlEvent. Windows offers no atomic way to close this gap.
 * <p>
 * Arguments: {@code <targetPid> [<callerPid>]}. The caller PID is optional. Without it, only the target and its descendants are allowed.
 * <p>
 * Exit codes: 0 = CTRL+C sent, {@link #PROCESS_NOT_FOUND}, {@link #EXCEPTION}, {@link #FOREIGN_PROCESS_ON_CONSOLE}. Any other code (e.g. 1
 * from a JVM that could not start the class) means that nothing was sent.
 */
public class CTRLSender {
    /**
     * Not in the bundled jna-platform. Deliberately a tiny private binding instead of another function in the global Kernel32Ext.
     */
    private interface ConsoleProcessListLib extends StdCallLibrary {
        /**
         * @return number of processes attached to the current console; if larger than {@code dwProcessCount}, the list was not filled and
         *         the call must be repeated with a buffer of at least that size. 0 on error.
         */
        int GetConsoleProcessList(int[] lpdwProcessList, int dwProcessCount);
    }

    /**
     * Sending failed or could not be done safely (invalid arguments, OpenProcess, AttachConsole, GetConsoleProcessList, process snapshot or
     * GenerateConsoleCtrlEvent failed).
     */
    public static final int  EXCEPTION                  = 2;
    /**
     * Target process does not exist or has already exited. Nothing was sent. Deliberately not 1: that is what a JVM returns if it could
     * not even start this class, which must not look like "process is gone".
     */
    public static final int  PROCESS_NOT_FOUND          = 10;
    /**
     * At least one process on the target's console may not receive CTRL (not the target, not one of its descendants, not the chain to
     * the caller that started it, or the caller itself). Nothing was sent, because CTRL+C would also hit these foreign processes. The
     * caller has to terminate the target in another way (e.g. forced).
     */
    public static final int  FOREIGN_PROCESS_ON_CONSOLE = 3;
    private static final int ERROR_INVALID_PARAMETER    = 87;

    public static void main(final String[] args) {
        try {
            Application.setApplication(".AppWorkUtils");
            final SimpleLoggerFactory fac = new SimpleLoggerFactory();
            fac.addSink(new LogToStdOutSink());
            LogV3.setFactory(fac);
            // inside try: invalid arguments end as EXCEPTION, not as an uncaught exception with JVM exit code 1
            final int pid = Integer.parseInt(args[0]);
            final long callerPid = args.length > 1 ? Long.parseLong(args[1]) : -1;
            if (!CrossSystem.isWindows()) {
                // nothing to fall back to: console CTRL events are a Windows concept. Fail loudly, the caller reports it.
                throw new IllegalStateException("CTRLSender only works on Windows. OS: " + CrossSystem.getOSString());
            }
            // free console, because we cannot attach to multiple consoles
            // https://blog.codetitans.pl/post/sending-ctrl-c-signal-to-another-application-on-windows/
            // this
            LogV3.info("Send CTRL " + pid + "(" + pid + ")" + (callerPid > 0 ? " caller " + callerPid : ""));
            {
                final WinNT.HANDLE processHandle = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_LIMITED_INFORMATION, false, pid);
                try {
                    if (processHandle == null) {
                        final int error = Kernel32.INSTANCE.GetLastError();
                        if (error == ERROR_INVALID_PARAMETER) {
                            LogV3.info("Process handle gone. Fine");
                            System.exit(PROCESS_NOT_FOUND);
                            return;
                        }
                        // e.g. access denied: we do not know whether the process is gone. This must not look like "gone".
                        throw new com.sun.jna.platform.win32.Win32Exception(error);
                    } else {
                        // process handler still available check exitcode
                        final IntByReference exitCode = new IntByReference();
                        if (Kernel32.INSTANCE.GetExitCodeProcess(processHandle, exitCode)) {
                            if (exitCode.getValue() != WinNT.STILL_ACTIVE) {
                                LogV3.info("Process gone. Fine");
                                System.exit(PROCESS_NOT_FOUND);
                                return;
                            }
                        }
                    }
                } finally {
                    if (processHandle != null) {
                        Kernel32Ext.INSTANCE.CloseHandle(processHandle);
                    }
                }
            }
            final ConsoleProcessListLib consoleLib;
            try {
                consoleLib = Native.load("kernel32", ConsoleProcessListLib.class, JNAOptions.SYSTEM_DLLS_ONLY);
            } catch (final Throwable e) {
                // Without the list we cannot exclude foreign processes, and we must not hit them. Do not send blindly.
                throw new IllegalStateException("GetConsoleProcessList is not available on this system (" + CrossSystem.getOSString() + "). Cannot verify which processes share the console of " + pid + ", so CTRL is not sent", e);
            }
            // No FreeConsole()/handler restore on any path below: every path ends in System.exit, which detaches this process
            boolean foreignOnConsole = false;
            Kernel32Ext.INSTANCE.FreeConsole();
            Kernel32.INSTANCE.SetLastError(0);
            if (Kernel32Ext.INSTANCE.AttachConsole(pid)) {
                // GenerateConsoleCtrlEvent(CTRL_C, 0) below hits EVERY process attached to this console, not only the
                // target. List them first, so we never terminate foreign applications.
                int[] attached = new int[64];
                int count;
                while ((count = consoleLib.GetConsoleProcessList(attached, attached.length)) > attached.length) {
                    // buffer too small: nothing was written, count is the required size
                    attached = new int[count];
                }
                if (count == 0) {
                    throw new com.sun.jna.platform.win32.Win32Exception(Kernel32.INSTANCE.GetLastError());
                }
                // Snapshot AFTER the console list: the shorter the time between the two, the smaller the window in which a PID can be
                // reused by a foreign process that joined the console.
                final Set<Integer> allowed = getAllowedProcesses(pid, callerPid);
                allowed.add(Kernel32.INSTANCE.GetCurrentProcessId());
                LogV3.info("Processes that may receive CTRL: " + allowed);
                final StringBuilder foreign = new StringBuilder();
                for (int i = 0; i < count; i++) {
                    if (!allowed.contains(attached[i])) {
                        foreignOnConsole = true;
                        if (foreign.length() > 0) {
                            foreign.append(", ");
                        }
                        foreign.append(attached[i]);
                        if (attached[i] == callerPid) {
                            foreign.append(" (caller)");
                        }
                    }
                }
                if (foreignOnConsole) {
                    // E.g. the target was started without its own console from a JVM that runs in a terminal and shares
                    // it with the caller. (Java's ProcessBuilder starts children with CREATE_NO_WINDOW, so they get their own
                    // console and this does not happen for them.) Orphaned grandchildren whose parent already exited also end
                    // up here, because the tree is built from parent PIDs. That is the safe direction: no CTRL, forced fallback.
                    LogV3.info("Processes on the console of " + pid + " that may not receive CTRL: " + foreign + ". Do not send CTRL");
                } else {
                    // disable ctrl handling - else GenerateConsoleCtrlEvent will kill ourself.
                    // Not enabled again: this process exits right after. GenerateConsoleCtrlEvent delivers asynchronously in a
                    // new thread, re-enabling (or freeing the console) too early let the late CTRL+C kill this helper
                    // (exit 130) although it was sent.
                    Kernel32Ext.INSTANCE.SetConsoleCtrlHandler(null, true);
                    Kernel32.INSTANCE.SetLastError(0);
                    // After AttachConsole(pid), process group 0 = all processes that share this console.
                    // Passing the target PID only works when that PID is also the process-group leader
                    // (CREATE_NEW_PROCESS_GROUP). For cmd /c ping the group leader is usually cmd (or
                    // Java), so GenerateConsoleCtrlEvent(CTRL_C, pingPid) can return true while ping
                    // keeps running — TestJNAWindowsProcessHandler.testSendCTRL then fails with
                    // "Could not kill".
                    final boolean success = Kernel32.INSTANCE.GenerateConsoleCtrlEvent(0, 0);
                    LogV3.info("CTRL Success Result: " + success);
                    if (!success) {
                        throw new Exception("Could not send CTRL");
                    }
                }
            } else {
                // Also a failed attach with last error 0 is an error: nothing was sent, this must not look like success
                final int error = Kernel32.INSTANCE.GetLastError();
                LogV3.info("AttachConsole failed. Last Error: " + error);
                if (error != 0) {
                    throw new com.sun.jna.platform.win32.Win32Exception(error);
                }
                throw new IllegalStateException("AttachConsole(" + pid + ") failed without error code");
            }
            if (foreignOnConsole) {
                System.exit(FOREIGN_PROCESS_ON_CONSOLE);
            }
            System.exit(0);
        } catch (final Throwable e) {
            LogV3.log(e);
            System.exit(EXCEPTION);
        }
    }

    /**
     * @return The processes that may receive CTRL together with the target:
     *         <ul>
     *         <li>the target and all its descendants</li>
     *         <li>if the target was started by the caller (the caller is an ancestor of the target): the chain of processes between
     *         caller and target, e.g. {@code cmd} for {@code cmd /c ping}. We started them, so we may stop them. Other children of these
     *         processes are NOT included.</li>
     *         </ul>
     *         The caller itself is never included, not even as a descendant of the target: CTRL would shut down the caller. If the caller
     *         is unknown or not an ancestor, the target was not started by us and only its own tree is allowed. Parents of such a target
     *         may be a user's shell and must never receive CTRL.
     *         <p>
     *         A parent PID is not updated by Windows when the parent exits, and the PID can be reused. A parent/child edge therefore
     *         only counts if the child was created after its parent. If a creation time cannot be read, the edge is rejected.
     * @throws Exception
     *             if the snapshot could not be created. We must not guess which processes belong to the target.
     */
    private static Set<Integer> getAllowedProcesses(final int targetPid, final long callerPid) throws Exception {
        final WinNT.HANDLE snapshot = Kernel32.INSTANCE.CreateToolhelp32Snapshot(Tlhelp32.TH32CS_SNAPPROCESS, new DWORD(0));
        if (snapshot == null || WinBase.INVALID_HANDLE_VALUE.equals(snapshot)) {
            throw new IllegalStateException("Could not create process snapshot. Last Error: " + Kernel32.INSTANCE.GetLastError());
        }
        final Map<Integer, Integer> parentOf = new HashMap<Integer, Integer>();
        try {
            final Tlhelp32.PROCESSENTRY32.ByReference pe = new Tlhelp32.PROCESSENTRY32.ByReference();
            pe.dwSize = new DWORD(pe.size());
            if (Kernel32.INSTANCE.Process32First(snapshot, pe)) {
                do {
                    parentOf.put(pe.th32ProcessID.intValue(), pe.th32ParentProcessID.intValue());
                } while (Kernel32.INSTANCE.Process32Next(snapshot, pe));
            }
        } finally {
            Kernel32.INSTANCE.CloseHandle(snapshot);
        }
        final Map<Integer, Long> creationTimes = new HashMap<Integer, Long>();
        final Set<Integer> allowed = new HashSet<Integer>();
        allowed.add(targetPid);
        // repeat until no new descendant is found: the snapshot is not ordered by hierarchy
        boolean added = true;
        while (added) {
            added = false;
            for (final Map.Entry<Integer, Integer> e : parentOf.entrySet()) {
                if (allowed.contains(e.getValue()) && !allowed.contains(e.getKey()) && isChildOf(e.getKey(), e.getValue(), creationTimes)) {
                    allowed.add(e.getKey());
                    added = true;
                }
            }
        }
        if (callerPid > 0) {
            // Walk up from the target. Only if we reach the caller, the processes on the way are ours. The walk is bounded and ends at
            // unknown processes and at edges that are not plausible parent/child relations.
            final Set<Integer> chain = new HashSet<Integer>();
            int child = targetPid;
            Integer parent = parentOf.get(child);
            int depth = 0;
            while (parent != null && depth++ < 64 && !allowed.contains(parent) && isChildOf(child, parent, creationTimes)) {
                if (parent.longValue() == callerPid) {
                    allowed.addAll(chain);
                    break;
                }
                chain.add(parent);
                child = parent;
                parent = parentOf.get(child);
            }
            if (callerPid <= Integer.MAX_VALUE) {
                allowed.remove(Integer.valueOf((int) callerPid));
            }
        }
        return allowed;
    }

    /**
     * @return true if {@code child} was created after {@code parent}, i.e. the parent PID of the child is not a reused PID. False if a
     *         creation time cannot be read (e.g. access denied): unknown means not allowed.
     */
    private static boolean isChildOf(final int child, final int parent, final Map<Integer, Long> creationTimes) {
        final Long childTime = getCreationTime(child, creationTimes);
        final Long parentTime = getCreationTime(parent, creationTimes);
        return childTime != null && parentTime != null && childTime.longValue() >= parentTime.longValue();
    }

    private static Long getCreationTime(final int pid, final Map<Integer, Long> cache) {
        if (cache.containsKey(pid)) {
            return cache.get(pid);
        }
        Long result = null;
        final WinNT.HANDLE handle = Kernel32.INSTANCE.OpenProcess(WinNT.PROCESS_QUERY_LIMITED_INFORMATION, false, pid);
        if (handle != null) {
            try {
                final FILETIME creation = new FILETIME();
                final FILETIME exit = new FILETIME();
                final FILETIME kernel = new FILETIME();
                final FILETIME user = new FILETIME();
                if (Kernel32.INSTANCE.GetProcessTimes(handle, creation, exit, kernel, user)) {
                    result = Long.valueOf(creation.toDWordLong().longValue());
                }
            } finally {
                Kernel32.INSTANCE.CloseHandle(handle);
            }
        }
        if (result == null) {
            LogV3.info("Creation time of process " + pid + " is not readable. It is not treated as part of the process tree");
        }
        cache.put(pid, result);
        return result;
    }
}
