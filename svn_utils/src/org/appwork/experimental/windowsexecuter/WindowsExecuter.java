/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.experimental.windowsexecuter;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.UnsupportedEncodingException;

import org.appwork.JNAHelper;
import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.executer.LogCallback;
import org.appwork.utils.parser.ShellParser;
import org.appwork.utils.parser.ShellParser.Style;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Tlhelp32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinBase;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;
import com.sun.jna.platform.win32.WinNT.PSIDByReference;
import com.sun.jna.ptr.IntByReference;

import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsUtils;

/**
 * Experimental: Runs a process as non-elevated on Windows via JNA (WinSafer API + CreateProcessAsUser). The child process is guaranteed to run with
 * medium integrity / normal user rights even when the current process is elevated. For UAC elevation use
 * {@link WindowsUtils#startElevatedProcess}. For running as LocalSystem use
 * {@link org.appwork.testframework.executer.AdminExecuter#runAsLocalSystem runAsLocalSystem} (scheduler/service approach).
 */
public final class WindowsExecuter {
    private static final int STARTF_USESTDHANDLES  = 0x00000100;
    private static final int TOKEN_INTEGRITY_LEVEL = 25;        // WinNT.TOKEN_INFORMATION_CLASS.TokenIntegrityLevel

    private WindowsExecuter() {
    }

    /**
     * Throws if the current OS is not Windows. Call at the start of each public method so callers get a clear error without repeating the
     * check in every method.
     */
    private static void ensureWindows() {
        if (!CrossSystem.isWindows()) {
            throw new UnsupportedOperationException("WindowsExecuter is only supported on Windows");
        }
    }

    /**
     * (SAFER_LEVELID_NORMALUSER), SaferComputeTokenFromLevel, SetTokenInformation(TokenIntegrityLevel), then CreateProcessAsUser. The
     * process runs with medium integrity even if the caller is elevated.
     *
     * @param options
     *            command, working dir, waitFor, env
     * @return ProcessOutput with exit code and stdout/stderr; when waitFor is true, waits for process exit
     * @throws UnsupportedOperationException
     *             if not Windows or JNA not available
     * @throws Exception
     *             on API or I/O errors
     */
    public static ProcessOutput runAsNonElevatedUser(ExecuteOptions options) throws Exception {
        ensureWindows();
        if (!JNAHelper.isJNAAvailable()) {
            throw new UnsupportedOperationException("runAsNonElevatedUser requires JNA");
        }
        if (options == null) {
            throw new IllegalArgumentException("options cannot be null");
        }
        String[] cmd = options.getCmd();
        String commandLine = ShellParser.createCommandLine(Style.WINDOWS, cmd);
        LogV3.info("WindowsExecuter.runAsNonElevatedUser: " + commandLine);

        // When runInActiveSession is set (e.g. by runViaWindowsScheduler fallback), run under active console token without SID check.
        if (options.isRunInActiveSession()) {
            HANDLE consoleToken = null;
            HANDLE primaryToken = null;
            try {
                consoleToken = WindowsUtils.getActiveConsoleUserToken();
                if (consoleToken != null) {
                    HANDLEByReference pPrimary = new HANDLEByReference();
                    if (Advapi32DuplicateTokenEx.INSTANCE.DuplicateTokenEx(consoleToken, 0, null, 2, Advapi32Safer.TOKEN_TYPE_PRIMARY, pPrimary)) {
                        primaryToken = pPrimary.getValue();
                        return runWithToken(primaryToken, options);
                    }
                }
            } catch (Throwable t) {
                LogV3.log(t);
            } finally {
                if (consoleToken != null) {
                    Kernel32.INSTANCE.CloseHandle(consoleToken);
                }
                if (primaryToken != null) {
                    Kernel32.INSTANCE.CloseHandle(primaryToken);
                }
            }
            LogV3.info("WindowsExecuter: runInActiveSession failed (no token), falling back to lowest privilege");
        }

        // When explicit SID is set (e.g. by direct callers/tests), try to run under that user if it matches active console; otherwise run with lowest privilege.
        String sid = options.getSid();
        if (sid != null && sid.trim().length() > 0) {
            HANDLE consoleToken = null;
            HANDLE primaryToken = null;
            try {
                consoleToken = WindowsUtils.getActiveConsoleUserToken();
                if (consoleToken != null) {
                    Advapi32Util.Account account = Advapi32Util.getTokenAccount(consoleToken);
                    if (account != null && sid.equals(account.sidString)) {
                        HANDLEByReference pPrimary = new HANDLEByReference();
                        // SecurityImpersonation = 2; TokenPrimary = 1
                        if (Advapi32DuplicateTokenEx.INSTANCE.DuplicateTokenEx(consoleToken, 0, null, 2, Advapi32Safer.TOKEN_TYPE_PRIMARY, pPrimary)) {
                            primaryToken = pPrimary.getValue();
                            return runWithToken(primaryToken, options);
                        }
                    }
                }
            } catch (Throwable t) {
                LogV3.log(t);
            } finally {
                if (consoleToken != null) {
                    Kernel32.INSTANCE.CloseHandle(consoleToken);
                }
                if (primaryToken != null) {
                    Kernel32.INSTANCE.CloseHandle(primaryToken);
                }
            }
            // SID path failed or no match; fall through to lowest-privilege path
            LogV3.info("WindowsExecuter: running with lowest privilege (SID " + sid + " not available or no match)");
        }

        HANDLE levelHandle = null;
        HANDLE hToken = null;
        Pointer sidToFree = null;
        try {
            HANDLEByReference pLevel = new HANDLEByReference();
            if (!Advapi32Safer.INSTANCE.SaferCreateLevel(Advapi32Safer.SAFER_SCOPEID_USER, Advapi32Safer.SAFER_LEVELID_NORMALUSER, Advapi32Safer.SAFER_LEVEL_OPEN, pLevel, null)) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            levelHandle = pLevel.getValue();
            HANDLEByReference pToken = new HANDLEByReference();
            if (!Advapi32Safer.INSTANCE.SaferComputeTokenFromLevel(levelHandle, null, pToken, 0, null)) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            hToken = pToken.getValue();
            PSIDByReference pSid = new PSIDByReference();
            if (!Advapi32.INSTANCE.ConvertStringSidToSid(Advapi32Safer.INTEGRITY_SID_MEDIUM, pSid)) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            sidToFree = pSid.getValue().getPointer();
            int tmlStructSize = new Advapi32Safer.TOKEN_MANDATORY_LABEL().size();
            Memory tmlBuf = new Memory(tmlStructSize);
            tmlBuf.setPointer(0, pSid.getValue().getPointer());
            tmlBuf.setInt(Native.POINTER_SIZE, Advapi32Safer.SE_GROUP_INTEGRITY);
            int tmlSize = tmlStructSize + Advapi32.INSTANCE.GetLengthSid(pSid.getValue());
            if (!Advapi32Safer.INSTANCE.SetTokenInformation(hToken, WinNT.TOKEN_INFORMATION_CLASS.TokenIntegrityLevel, tmlBuf, tmlSize)) {
                throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
            }
            return runWithToken(hToken, options);
        } finally {
            if (levelHandle != null) {
                Advapi32Safer.INSTANCE.SaferCloseLevel(levelHandle);
            }
            if (hToken != null) {
                Kernel32.INSTANCE.CloseHandle(hToken);
            }
            if (sidToFree != null) {
                Kernel32.INSTANCE.LocalFree(sidToFree);
            }
        }
    }

    private static void streamLinesToCallback(ByteArrayOutputStream data, LogCallback callback, boolean isStdOut) {
        String charset = "UTF-8";
        String text;
        try {
            text = new String(data.toByteArray(), charset);
        } catch (UnsupportedEncodingException e) {
            text = new String(data.toByteArray());
        }
        String[] lines = text.split("[\r\n]+");
        for (String line : lines) {
            line = line.trim();
            if (line.length() > 0) {
                if (isStdOut) {
                    callback.onStdOut(line);
                } else {
                    callback.onStdErr(line);
                }
            }
        }
    }

    private static int findPidOfProcess(String exeName) {
        HANDLE snapshot = Kernel32.INSTANCE.CreateToolhelp32Snapshot(Tlhelp32.TH32CS_SNAPPROCESS, new DWORD(0));
        if (snapshot == null || WinBase.INVALID_HANDLE_VALUE.equals(snapshot)) {
            return -1;
        }
        try {
            Tlhelp32.PROCESSENTRY32.ByReference pe = new Tlhelp32.PROCESSENTRY32.ByReference();
            pe.dwSize = new DWORD(pe.size());
            if (!Kernel32.INSTANCE.Process32First(snapshot, pe)) {
                return -1;
            }
            do {
                String name = Native.toString(pe.szExeFile);
                if (exeName.equalsIgnoreCase(name)) {
                    return pe.th32ProcessID.intValue();
                }
            } while (Kernel32.INSTANCE.Process32Next(snapshot, pe));
            return -1;
        } finally {
            Kernel32.INSTANCE.CloseHandle(snapshot);
        }
    }

    private static ProcessOutput runWithToken(HANDLE hToken, ExecuteOptions options) throws Exception {
        String[] cmd = options.getCmd();
        File workingDir = options.getWorkingDir();
        boolean waitFor = options.isWaitFor();
        String workDirPath = workingDir != null ? workingDir.getAbsolutePath() : null;
        String commandLine = ShellParser.createCommandLine(Style.WINDOWS, cmd);
        LogV3.info("runWithToken: commandLine.length=" + commandLine.length() + " cmdLine=\"" + (commandLine.length() > 200 ? commandLine.substring(0, 200) + "..." : commandLine) + "\" workDirPath=" + workDirPath + " waitFor=" + waitFor);
        HANDLE stdInHandle = Kernel32.INSTANCE.GetStdHandle(Kernel32.STD_INPUT_HANDLE);
        long stdInVal = stdInHandle != null ? Pointer.nativeValue(stdInHandle.getPointer()) : -1;
        boolean hasConsole = (stdInVal != 0 && stdInVal != -1L);
        LogV3.info("runWithToken: GetStdHandle(STD_INPUT)=" + stdInVal + " hasConsole=" + hasConsole);
        // CreateProcessAsUserW requires writable lpCommandLine; use Memory buffer with UTF-16LE (setWideString).
        int cmdLineBufSize = Native.WCHAR_SIZE * (commandLine.length() + 1);
        Memory cmdLineMem = new Memory(cmdLineBufSize);
        cmdLineMem.setWideString(0, commandLine);
        LogV3.info("runWithToken: cmdLineMem.size=" + cmdLineBufSize + " WCHAR_SIZE=" + Native.WCHAR_SIZE + " nullTerminator=" + (cmdLineBufSize >= 2 ? "0x" + (cmdLineMem.getByte(cmdLineBufSize - 2) & 0xFF) + "," + (cmdLineMem.getByte(cmdLineBufSize - 1) & 0xFF) : "n/a"));
        HANDLE hStdOutRd = null;
        HANDLE hStdErrRd = null;
        HANDLE hStdInRd = null;
        try {
            HANDLEByReference hStdOutRdRef = new HANDLEByReference();
            HANDLEByReference hStdOutWrRef = new HANDLEByReference();
            HANDLEByReference hStdErrRdRef = new HANDLEByReference();
            HANDLEByReference hStdErrWrRef = new HANDLEByReference();
            HANDLEByReference hStdInRdRef = new HANDLEByReference();
            HANDLEByReference hStdInWrRef = new HANDLEByReference();
            WinBase.SECURITY_ATTRIBUTES sa = new WinBase.SECURITY_ATTRIBUTES();
            sa.dwLength = new DWORD(sa.size());
            sa.bInheritHandle = true;
            if (waitFor) {
                if (!Kernel32.INSTANCE.CreatePipe(hStdOutRdRef, hStdOutWrRef, sa, 0)) {
                    throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
                }
                if (!Kernel32.INSTANCE.CreatePipe(hStdErrRdRef, hStdErrWrRef, sa, 0)) {
                    closeHandleSafe(hStdOutRdRef.getValue());
                    closeHandleSafe(hStdOutWrRef.getValue());
                    throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
                }
                if (!Kernel32.INSTANCE.CreatePipe(hStdInRdRef, hStdInWrRef, sa, 0)) {
                    closeHandleSafe(hStdOutRdRef.getValue());
                    closeHandleSafe(hStdOutWrRef.getValue());
                    closeHandleSafe(hStdErrRdRef.getValue());
                    closeHandleSafe(hStdErrWrRef.getValue());
                    throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
                }
                hStdOutRd = hStdOutRdRef.getValue();
                hStdErrRd = hStdErrRdRef.getValue();
                hStdInRd = hStdInRdRef.getValue();
                LogV3.info("runWithToken: pipes created (stdin/out/err)");
            }
            WinBase.STARTUPINFO si = new WinBase.STARTUPINFO();
            si.clear();
            si.cb = new DWORD(si.size());
            if (waitFor) {
                si.dwFlags = STARTF_USESTDHANDLES;
                si.hStdInput = hStdInRdRef.getValue();
                si.hStdOutput = hStdOutWrRef.getValue();
                si.hStdError = hStdErrWrRef.getValue();
            }
            int siSize = si.size();
            long hIn = waitFor && si.hStdInput != null ? Pointer.nativeValue(si.hStdInput.getPointer()) : 0;
            long hOut = waitFor && si.hStdOutput != null ? Pointer.nativeValue(si.hStdOutput.getPointer()) : 0;
            long hErr = waitFor && si.hStdError != null ? Pointer.nativeValue(si.hStdError.getPointer()) : 0;
            LogV3.info("runWithToken: STARTUPINFO cb=" + si.cb + " size()=" + siSize + " dwFlags=0x" + Integer.toHexString(si.dwFlags) + " hStdInput=0x" + Long.toHexString(hIn) + " hStdOutput=0x" + Long.toHexString(hOut) + " hStdError=0x" + Long.toHexString(hErr));
            WinBase.PROCESS_INFORMATION pi = new WinBase.PROCESS_INFORMATION();
            pi.clear();
            int creationFlags = Kernel32.CREATE_UNICODE_ENVIRONMENT | WinBase.CREATE_NO_WINDOW;
            LogV3.info("runWithToken: calling CreateProcessAsUserW hToken=" + (hToken != null ? Pointer.nativeValue(hToken.getPointer()) : 0) + " creationFlags=0x" + Integer.toHexString(creationFlags));
            boolean ok = Advapi32CreateProcess.INSTANCE.CreateProcessAsUser(hToken, Pointer.NULL, cmdLineMem, null, null, true, creationFlags, null, workDirPath, si, pi);
            int createProcessLastError = Kernel32.INSTANCE.GetLastError();
            LogV3.info("runWithToken: CreateProcessAsUser returned " + ok + " GetLastError=" + createProcessLastError + " (0x" + Integer.toHexString(createProcessLastError) + ")");
            if (!ok) {
                LogV3.info("runWithToken: CreateProcessAsUser failed ERROR_BAD_LENGTH=0x18=" + (createProcessLastError == 0x18));
            }
            if (waitFor) {
                Kernel32.INSTANCE.CloseHandle(hStdInWrRef.getValue());
                Kernel32.INSTANCE.CloseHandle(hStdOutWrRef.getValue());
                Kernel32.INSTANCE.CloseHandle(hStdErrWrRef.getValue());
            }
            if (!ok) {
                if (waitFor) {
                    closeHandleSafe(hStdInRd);
                    closeHandleSafe(hStdOutRd);
                    closeHandleSafe(hStdErrRd);
                }
                int err = (createProcessLastError != 0) ? createProcessLastError : 0x1F; // 0x1F = ERROR_GEN_FAILURE if API did not set last
                                                                                         // error
                throw new Win32Exception(err);
            }
            if (pi.hThread != null && Pointer.nativeValue(pi.hThread.getPointer()) != 0) {
                Kernel32.INSTANCE.CloseHandle(pi.hThread);
            }
            if (waitFor) {
                ByteArrayOutputStream stdoutData = new ByteArrayOutputStream();
                ByteArrayOutputStream stderrData = new ByteArrayOutputStream();
                LogCallback logCb = options.getLogCallback();
                Thread outReader = readPipeInBackground(hStdOutRd, stdoutData, logCb, true);
                Thread errReader = readPipeInBackground(hStdErrRd, stderrData, logCb, false);
                Kernel32.INSTANCE.WaitForSingleObject(pi.hProcess, Kernel32.INFINITE);
                try {
                    outReader.join(5000);
                    errReader.join(5000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
                IntByReference exitCodeRef = new IntByReference();
                Kernel32.INSTANCE.GetExitCodeProcess(pi.hProcess, exitCodeRef);
                int exitCode = exitCodeRef.getValue();
                Kernel32.INSTANCE.CloseHandle(pi.hProcess);
                String codePage = getConsoleCodepageSafe();
                return new ProcessOutput(exitCode, stdoutData, stderrData, codePage);
            } else {
                int remotePid = Kernel32.INSTANCE.GetProcessId(pi.hProcess);
                Kernel32.INSTANCE.CloseHandle(pi.hProcess);
                String codePage = getConsoleCodepageSafe();
                return new ProcessOutput(-1, new ByteArrayOutputStream(), new ByteArrayOutputStream(), codePage, Integer.valueOf(remotePid), null);
            }
        } finally {
            if (hStdInRd != null) {
                closeHandleSafe(hStdInRd);
            }
            if (hStdOutRd != null) {
                closeHandleSafe(hStdOutRd);
            }
            if (hStdErrRd != null) {
                closeHandleSafe(hStdErrRd);
            }
        }
    }

    private static void closeHandleSafe(HANDLE h) {
        if (h != null && !WinBase.INVALID_HANDLE_VALUE.equals(h) && Pointer.nativeValue(h.getPointer()) != 0) {
            try {
                Kernel32.INSTANCE.CloseHandle(h);
            } catch (Throwable t) {
                LogV3.log(t);
            }
        }
    }

    private static Thread readPipeInBackground(final HANDLE hPipe, final ByteArrayOutputStream out, final LogCallback callback, final boolean isStdOut) {
        Thread t = new Thread(new Runnable() {
            @Override
            public void run() {
                byte[] buf = new byte[4096];
                IntByReference read = new IntByReference();
                ByteArrayOutputStream lineBuf = new ByteArrayOutputStream();
                String charset = "UTF-8";
                while (Kernel32.INSTANCE.ReadFile(hPipe, buf, buf.length, read, null)) {
                    int n = read.getValue();
                    if (n <= 0) {
                        break;
                    }
                    out.write(buf, 0, n);
                    if (callback != null) {
                        for (int i = 0; i < n; i++) {
                            byte b = buf[i];
                            if (b == '\n') {
                                flushLine(lineBuf, callback, isStdOut, charset);
                            } else if (b != '\r') {
                                lineBuf.write(b & 0xff);
                            }
                        }
                    }
                }
                if (callback != null && lineBuf.size() > 0) {
                    flushLine(lineBuf, callback, isStdOut, charset);
                }
                closeHandleSafe(hPipe);
            }
        }, "WindowsExecuter-PipeReader");
        t.setDaemon(true);
        t.start();
        return t;
    }

    private static void flushLine(ByteArrayOutputStream lineBuf, LogCallback callback, boolean isStdOut, String charset) {
        if (lineBuf.size() == 0) {
            return;
        }
        String line;
        try {
            line = new String(lineBuf.toByteArray(), charset).trim();
        } catch (UnsupportedEncodingException e) {
            line = new String(lineBuf.toByteArray()).trim();
        }
        lineBuf.reset();
        if (line.length() > 0) {
            if (isStdOut) {
                callback.onStdOut(line);
            } else {
                callback.onStdErr(line);
            }
        }
    }

    private static String getConsoleCodepageSafe() {
        try {
            return ProcessBuilderFactory.getConsoleCodepage();
        } catch (Throwable t) {
            return "UTF-8";
        }
    }
}
