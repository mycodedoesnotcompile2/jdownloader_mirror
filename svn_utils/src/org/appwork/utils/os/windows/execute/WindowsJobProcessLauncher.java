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
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.os.windows.execute;

import java.io.File;

import org.appwork.JNAHelper;
import org.appwork.jna.windows.Kernel32Ext;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.parser.ShellParser.Style;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinNT.HANDLE;

/**
 * Starts a process via {@code CreateProcessW} into a {@link WindowsJobObject}, fire-and-forget: no wait, no stdout/stderr pipes.
 * <p>
 * Sequence: {@code CREATE_SUSPENDED} → {@link WindowsJobObject#assignProcess(HANDLE)} → {@code ResumeThread}. Process and thread handles are
 * closed after resume; the job handle stays with the caller for later PID enumeration.
 */
public class WindowsJobProcessLauncher {
    private WindowsJobProcessLauncher() {
    }

    public static final class LaunchResult {
        public final int             pid;
        public final WindowsJobObject job;

        public LaunchResult(final int pid, final WindowsJobObject job) {
            this.pid = pid;
            this.job = job;
        }
    }

    /**
     * Creates a new job, starts {@code command} in that job, returns immediately.
     *
     * @param command
     *            argv-style command (first element is executable path or name)
     * @param workingDirectory
     *            optional working directory; null = inherit
     */
    public static LaunchResult startDetachedInNewJob(final String[] command, final File workingDirectory) throws Exception {
        final WindowsJobObject job = WindowsJobObject.create();
        try {
            final int pid = startDetachedInJob(command, workingDirectory, job);
            return new LaunchResult(pid, job);
        } catch (final Exception e) {
            job.close();
            throw e;
        }
    }

    /**
     * Starts {@code command} assigned to an existing {@code job}. Does not wait and does not capture output.
     *
     * @return process id of the created process
     */
    public static int startDetachedInJob(final String[] command, final File workingDirectory, final WindowsJobObject job) throws Exception {
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            throw new IllegalStateException("WindowsJobProcessLauncher requires Windows + JNA");
        }
        if (command == null || command.length == 0) {
            throw new IllegalArgumentException("command is empty");
        }
        if (job == null || !job.isOpen()) {
            throw new IllegalArgumentException("job is null or closed");
        }
        final String commandLine = ShellParser.createCommandLine(Style.WINDOWS, command);
        final String workDir = workingDirectory != null ? workingDirectory.getAbsolutePath() : null;
        if (StringUtils.isNotEmpty(workDir) && !new File(workDir).isDirectory()) {
            throw new IllegalArgumentException("workingDirectory is not a directory: " + workDir);
        }
        // CreateProcessW may rewrite lpCommandLine in-place — must be a mutable buffer.
        final char[] cmdLineBuf = Native.toCharArray(commandLine);
        final WinBase.STARTUPINFO si = new WinBase.STARTUPINFO();
        si.clear();
        si.cb = new DWORD(si.size());
        final WinBase.PROCESS_INFORMATION pi = new WinBase.PROCESS_INFORMATION();
        pi.clear();
        final int flags = WinBase.CREATE_SUSPENDED | WinBase.CREATE_UNICODE_ENVIRONMENT;
        LogV3.info("WindowsJobProcessLauncher: CreateProcessW suspended cmd=" + commandLine + " wd=" + workDir);
        final boolean ok = Kernel32.INSTANCE.CreateProcessW(null, cmdLineBuf, null, null, false, new DWORD(flags), Pointer.NULL, workDir, si, pi);
        if (!ok) {
            final int err = Kernel32.INSTANCE.GetLastError();
            LogV3.info("WindowsJobProcessLauncher: CreateProcessW FAILED err=" + err);
            throw new Win32Exception(err);
        }
        final HANDLE hProcess = pi.hProcess;
        final HANDLE hThread = pi.hThread;
        final int pid = pi.dwProcessId != null ? pi.dwProcessId.intValue() : 0;
        LogV3.info("WindowsJobProcessLauncher: CreateProcessW OK pid=" + pid + " tid=" + (pi.dwThreadId != null ? pi.dwThreadId.intValue() : -1));
        try {
            job.assignProcess(hProcess);
            final DWORD prev = Kernel32Ext.INSTANCE.ResumeThread(hThread);
            if (prev == null || prev.intValue() == -1) {
                final int err = Kernel32.INSTANCE.GetLastError();
                LogV3.info("WindowsJobProcessLauncher: ResumeThread FAILED err=" + err);
                throw new Win32Exception(err);
            }
            LogV3.info("WindowsJobProcessLauncher: ResumeThread OK prevSuspend=" + prev.intValue() + " pid=" + pid + " (fire-and-forget, closing process/thread handles)");
            return pid;
        } catch (final Exception e) {
            try {
                Kernel32.INSTANCE.TerminateProcess(hProcess, 1);
            } catch (final Throwable ignored) {
            }
            throw e;
        } finally {
            closeHandleSafe(hThread);
            closeHandleSafe(hProcess);
        }
    }

    private static void closeHandleSafe(final HANDLE h) {
        if (h != null && !WinBase.INVALID_HANDLE_VALUE.equals(h)) {
            try {
                Kernel32.INSTANCE.CloseHandle(h);
            } catch (final Throwable ignored) {
            }
        }
    }
}
